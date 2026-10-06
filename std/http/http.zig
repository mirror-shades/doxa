//! The `std.http` engine.
//!
//! Networking runs on Zig tasks, one per connection, on a concurrent
//! `std.Io.Threaded`. A task reads and validates a whole request or WebSocket
//! message, hands it to the Doxa program through a bounded mailbox, and writes
//! whatever the program answers. The Doxa program stays single-threaded: it
//! takes finished results from the mailbox with `poll` and never touches a
//! socket.
//!
//! Rules every part of this file keeps (plan/http-robustness.md):
//!
//! - **Main-thread confinement.** Tasks never call into the Doxa runtime and
//!   never touch arena memory. Every `pub fn` here is called by the Doxa
//!   program's one thread; everything else may run on a task.
//! - **Bounded waits.** Each blocking read or write runs under a deadline the
//!   supervisor enforces by cancelling the task.
//! - **Bounded memory.** Buffers, queues and the connection count are capped by
//!   `Limits`; exceeding a cap closes that one connection with a reason.
//! - **One terminal report.** A connection ends with exactly one `closed`
//!   notice carrying a `CloseReason`, unless the program closed it itself.
//! - **No traps on network input.** Lengths, opcodes and casts derived from the
//!   peer go through error paths, never asserts.
//! - **Tasks never wait on each other.** A connection's reader and writer exit
//!   independently and the last one out tears the transport down, so the
//!   supervisor can always cancel a task and get it back promptly.

const std = @import("std");
const builtin = @import("builtin");

const Io = std.Io;
const net = Io.net;
const http = std.http;

const gpa = std.heap.smp_allocator;

// ---------------------------------------------------------------------------
// Error cell (main thread only)
// ---------------------------------------------------------------------------

const code_invalid_argument: i64 = 1;
const code_out_of_memory: i64 = 2;
const code_not_supported: i64 = 4;
const code_write_failed: i64 = 107;
const code_create_failed: i64 = 108;
const code_invalid_data: i64 = 113;
const code_connection_failed: i64 = 114;
const code_timeout: i64 = 115;
const code_http_status: i64 = 116;

const ErrorCell = struct {
    var last: i64 = 0;
};

pub fn clearLastError() void {
    ErrorCell.last = 0;
}

pub fn setLastError(code: i64) void {
    ErrorCell.last = code;
}

pub fn takeLastErrorCode() i64 {
    const code = ErrorCell.last;
    ErrorCell.last = 0;
    return code;
}

fn mapCommon(err_name: []const u8) i64 {
    if (std.mem.eql(u8, err_name, "InvalidArgument")) return 1;
    if (std.mem.eql(u8, err_name, "OutOfMemory")) return 2;
    if (std.mem.eql(u8, err_name, "AccessDenied")) return 3;
    if (std.mem.eql(u8, err_name, "PermissionDenied")) return 3;
    if (std.mem.eql(u8, err_name, "NotSupported")) return 4;
    if (std.mem.eql(u8, err_name, "OperationNotSupported")) return 4;
    return -1;
}

fn mapNetworkError(err_name: []const u8) i64 {
    const common = mapCommon(err_name);
    if (common >= 0) return common;
    if (std.mem.eql(u8, err_name, "Timeout")) return code_timeout;
    if (std.mem.eql(u8, err_name, "InvalidAddress") or
        std.mem.eql(u8, err_name, "InvalidPort") or
        std.mem.eql(u8, err_name, "AddressFamilyUnsupported") or
        std.mem.eql(u8, err_name, "HttpExpectationFailed")) return code_invalid_argument;
    if (std.mem.eql(u8, err_name, "WriteFailed")) return code_write_failed;
    return code_connection_failed;
}

// ---------------------------------------------------------------------------
// Limits
// ---------------------------------------------------------------------------

/// Caps and deadlines. A connection snapshots these when it is created, so a
/// change applies to connections opened afterwards.
const Limits = struct {
    head_bytes: usize = 64 * 1024,
    body_bytes: usize = 1024 * 1024,
    message_bytes: usize = 16 * 1024 * 1024,
    outbound_bytes: usize = 4 * 1024 * 1024,
    inbox_messages: usize = 16,
    pending_bytes: usize = 64 * 1024 * 1024,
    connections: usize = 256,

    handshake_ms: i64 = 10_000,
    head_ms: i64 = 10_000,
    read_ms: i64 = 30_000,
    write_ms: i64 = 30_000,
    idle_ms: i64 = 120_000,
    handler_ms: i64 = 60_000,
    linger_ms: i64 = 1_000,
};

/// Order is the wire contract with `Limit` in `http.doxa`.
const LimitKey = enum(i64) {
    head_bytes,
    body_bytes,
    message_bytes,
    outbound_bytes,
    inbox_messages,
    pending_bytes,
    connections,
    handshake_ms,
    head_ms,
    read_ms,
    write_ms,
    idle_ms,
    handler_ms,
    linger_ms,
};

/// Why a connection ended. Order is the wire contract with `CloseReason` in
/// `http.doxa`.
const CloseReason = enum(u8) {
    open,
    peer_closed,
    reset,
    local_close,
    idle_timeout,
    read_timeout,
    write_timeout,
    handler_timeout,
    slow_consumer,
    too_large,
    protocol_error,
    handshake_failed,
    connect_failed,
    tls_error,
};

// ---------------------------------------------------------------------------
// Engine
// ---------------------------------------------------------------------------

const NoticeKind = enum(u8) { accepted = 1, readable = 2, closed = 3 };

/// One line in the mailbox: "`handle`, which belongs to `owner`, has news".
/// The news itself stays on the connection; a notice only says where to look.
const Notice = struct {
    owner: i64,
    handle: i64,
    kind: NoticeKind,
};

const Entry = union(enum) {
    listener: *Listener,
    conn: *Conn,
};

const Engine = struct {
    var threaded: Io.Threaded = undefined;
    var started: bool = false;
    var concurrent: bool = false;

    /// Guards every field below it. Never held across a blocking call other
    /// than the condition waits on it.
    var mutex: Io.Mutex = .init;
    var limits: Limits = .{};
    var entries: std.AutoHashMapUnmanaged(i64, Entry) = .empty;
    var next_handle: i64 = 1;
    var mailbox: std.ArrayList(Notice) = .empty;
    /// Connections and listeners whose handle is dead but whose tasks may
    /// still be running; the supervisor frees them once the tasks are gone.
    var retired_conns: std.ArrayList(*Conn) = .empty;
    var retired_listeners: std.ArrayList(*Listener) = .empty;
    var live_conns: usize = 0;
    var slot_freed: Io.Condition = .init;
    var pending_bytes: usize = 0;
    var budget_freed: Io.Condition = .init;

    /// Set when the mailbox gains a notice; the Doxa thread is its only waiter.
    var mail: Io.Event = .unset;
    /// Set when the supervisor has something to do before its next tick.
    var supervisor_wake: Io.Event = .unset;
    var supervisor: ?Io.Future(void) = null;

    /// Handles whose `closed` notice the last `poll` returned. They die at the
    /// start of the next `poll`, so the program can still ask why. Main thread
    /// only.
    var delivered: std.ArrayList(i64) = .empty;
};

fn engineIo() Io {
    return Engine.threaded.io();
}

/// Starts the engine on first use. Main thread only.
fn ensureStarted() void {
    if (Engine.started) return;
    Engine.started = true;
    if (builtin.single_threaded) {
        Engine.threaded = Io.Threaded.init_single_threaded;
        Engine.threaded.allocator = gpa;
        return;
    }
    // The default stack size is kept on purpose. Under libc a thread's static
    // thread-local storage is carved from its stack, and every Zig module
    // linked into the program adds to it, so a smaller fixed size fails to
    // spawn in programs with more modules. The reservation is virtual.
    Engine.threaded = .init(gpa, .{});
    const io = engineIo();
    Engine.supervisor = io.concurrent(supervise, .{}) catch null;
    Engine.concurrent = Engine.supervisor != null;
}

fn nowNs(io: Io) i64 {
    return @intCast(Io.Clock.awake.now(io).nanoseconds);
}

fn afterMs(ms: i64) Io.Timeout {
    return .{ .duration = .{ .raw = .fromMilliseconds(ms), .clock = .awake } };
}

fn postNotice(io: Io, owner: i64, handle: i64, kind: NoticeKind) void {
    Engine.mutex.lockUncancelable(io);
    Engine.mailbox.append(gpa, .{ .owner = owner, .handle = handle, .kind = kind }) catch {};
    Engine.mutex.unlock(io);
    Engine.mail.set(io);
}

/// Reserves `bytes` of the global undelivered-data budget, waiting while the
/// budget is exhausted. One reservation is always admitted when nothing else
/// is pending, so a single item larger than the budget cannot deadlock.
fn reserveBudget(io: Io, bytes: usize) Io.Cancelable!void {
    try Engine.mutex.lock(io);
    defer Engine.mutex.unlock(io);
    while (Engine.pending_bytes != 0 and Engine.pending_bytes + bytes > Engine.limits.pending_bytes) {
        try Engine.budget_freed.wait(io, &Engine.mutex);
    }
    Engine.pending_bytes += bytes;
}

fn releaseBudget(io: Io, bytes: usize) void {
    if (bytes == 0) return;
    Engine.mutex.lockUncancelable(io);
    Engine.pending_bytes -= @min(bytes, Engine.pending_bytes);
    Engine.mutex.unlock(io);
    Engine.budget_freed.broadcast(io);
}

// ---------------------------------------------------------------------------
// Connections
// ---------------------------------------------------------------------------

const Role = enum { server, client };

/// What the supervisor cancelled a task for. A task that sees
/// `error.Canceled` reads its own slot to learn why.
const Stop = enum(u8) { none, kill, read_deadline, write_deadline };

/// The two tasks a connection can have. An HTTP connection only has a reader,
/// which also writes; a WebSocket connection adds a writer for its outbox.
const Task = enum { reader, writer };

/// Which wait a read deadline is guarding, so expiry maps to the right reason.
const ReadWait = enum(u8) { idle, head, progress, handler, handshake, linger };

const Phase = enum { reading, awaiting_reply, writing };

const Opcode = enum(u4) {
    continuation = 0,
    text = 1,
    binary = 2,
    close = 8,
    ping = 9,
    pong = 10,
    _,
};

/// The request a connection has handed to the program and not yet had
/// answered. Built by the reader task and published under `Conn.mutex`; from
/// then on only the main thread reads it, and frees it when it answers.
const Exchange = struct {
    state: enum { none, ready } = .none,
    method: []const u8 = "",
    target: []u8 = &.{},
    head: []u8 = &.{},
    body: []u8 = &.{},
    keep_alive: bool = false,
    wants_websocket: bool = false,
    reserved: usize = 0,

    fn free(x: *Exchange, io: Io) void {
        gpa.free(x.target);
        gpa.free(x.head);
        gpa.free(x.body);
        releaseBudget(io, x.reserved);
        x.* = .{};
    }
};

/// The program's answer to an `Exchange`. Written by the main thread under
/// `Conn.mutex`; taken by the reader task.
const Reply = struct {
    kind: enum { none, response, upgrade, close } = .none,
    status: u16 = 0,
    /// Validated `Name: value\r\n` lines, owned.
    headers: []u8 = &.{},
    body: []u8 = &.{},

    fn free(r: *Reply) void {
        gpa.free(r.headers);
        gpa.free(r.body);
        r.* = .{};
    }
};

/// One received WebSocket message, or the in-band marker that the connection
/// failed. Owned by the inbox, then by `Conn.current`.
const Message = struct {
    op: i64 = 0,
    data: []u8 = &.{},
    failed: bool = false,

    fn free(m: *Message) void {
        gpa.free(m.data);
        m.* = .{};
    }
};

const Frame = struct {
    opcode: u4,
    data: []u8,
};

const Transport = union(enum) {
    stream: net.Stream,
    client: *http.Client.Connection,
};

const Conn = struct {
    handle: i64 = 0,
    owner: i64 = 0,
    role: Role,
    limits: Limits,
    transport: Transport,

    /// Released by whoever registered the connection, once its handle and
    /// futures are in place.
    start: Io.Event = .unset,
    reader_future: ?Io.Future(void) = null,
    writer_future: ?Io.Future(void) = null,
    tasks_active: std.atomic.Value(u32) = .init(0),
    finished: std.atomic.Value(bool) = .init(false),

    reader_stop: std.atomic.Value(Stop) = .init(.none),
    writer_stop: std.atomic.Value(Stop) = .init(.none),
    kill_requested: std.atomic.Value(bool) = .init(false),
    read_deadline: std.atomic.Value(i64) = .init(0),
    read_wait: std.atomic.Value(ReadWait) = .init(.idle),
    write_deadline: std.atomic.Value(i64) = .init(0),

    /// Guards the fields below it.
    mutex: Io.Mutex = .init,
    changed: Io.Condition = .init,
    phase: Phase = .reading,
    user_closed: bool = false,
    reason: CloseReason = .open,
    linger: bool = false,
    exchange: Exchange = .{},
    reply: Reply = .{},
    websocket: bool = false,
    inbox: std.ArrayList(Message) = .empty,
    inbox_head: usize = 0,
    /// `inboxLen()`, readable without the mutex so `poll` can re-report a
    /// connection whose messages the program has not taken yet.
    inbox_waiting: std.atomic.Value(usize) = .init(0),
    outbox: std.ArrayList(Frame) = .empty,
    outbox_head: usize = 0,
    outbox_bytes: usize = 0,
    outbox_closing: bool = false,

    /// The message the last `wsNext` surfaced. Main thread only.
    current: Message = .{},
    /// A close frame has been surfaced, or the connection failed. Main thread
    /// only.
    ws_ended: bool = false,

    read_buffer: []u8 = &.{},
    transfer_buffer: []u8 = &.{},
    write_buffer: [8 * 1024]u8 = undefined,
    stream_reader: net.Stream.Reader = undefined,
    stream_writer: net.Stream.Writer = undefined,
    in: *Io.Reader = undefined,
    out: *Io.Writer = undefined,

    fn create(role: Role, transport: Transport, limits: Limits) ?*Conn {
        const conn = gpa.create(Conn) catch return null;
        conn.* = .{ .role = role, .limits = limits, .transport = transport };
        conn.read_buffer = gpa.alloc(u8, @max(limits.head_bytes, 256)) catch {
            gpa.destroy(conn);
            return null;
        };
        conn.transfer_buffer = gpa.alloc(u8, 16 * 1024) catch {
            gpa.free(conn.read_buffer);
            gpa.destroy(conn);
            return null;
        };
        return conn;
    }

    /// Frees a connection whose tasks have all returned. Supervisor only.
    fn destroy(conn: *Conn, io: Io) void {
        conn.exchange.free(io);
        conn.reply.free();
        var reserved: usize = 0;
        for (conn.inbox.items[conn.inbox_head..]) |*message| {
            reserved += message.data.len;
            message.free();
        }
        releaseBudget(io, reserved);
        conn.inbox.deinit(gpa);
        for (conn.outbox.items[conn.outbox_head..]) |frame| gpa.free(frame.data);
        conn.outbox.deinit(gpa);
        conn.current.free();
        gpa.free(conn.read_buffer);
        gpa.free(conn.transfer_buffer);
        gpa.destroy(conn);
    }

    fn inboxLen(conn: *const Conn) usize {
        return conn.inbox.items.len - conn.inbox_head;
    }

    /// Records why the connection ended. The first reason wins: later ones
    /// are consequences of it.
    fn setReason(conn: *Conn, io: Io, reason: CloseReason) void {
        conn.mutex.lockUncancelable(io);
        if (conn.reason == .open) conn.reason = reason;
        conn.mutex.unlock(io);
    }

    fn armRead(conn: *Conn, io: Io, wait: ReadWait) void {
        const ms = switch (wait) {
            .idle => conn.limits.idle_ms,
            .head => conn.limits.head_ms,
            .progress => conn.limits.read_ms,
            .handler => conn.limits.handler_ms,
            .handshake => conn.limits.handshake_ms,
            .linger => conn.limits.linger_ms,
        };
        conn.read_wait.store(wait, .release);
        conn.read_deadline.store(if (ms <= 0) 0 else nowNs(io) +| ms *| std.time.ns_per_ms, .release);
    }

    fn disarmRead(conn: *Conn) void {
        conn.read_deadline.store(0, .release);
    }

    fn armWrite(conn: *Conn, io: Io) void {
        const ms = conn.limits.write_ms;
        conn.write_deadline.store(if (ms <= 0) 0 else nowNs(io) +| ms *| std.time.ns_per_ms, .release);
    }

    fn disarmWrite(conn: *Conn) void {
        conn.write_deadline.store(0, .release);
    }

    fn stopOf(conn: *Conn, task: Task) Stop {
        return switch (task) {
            .reader => conn.reader_stop.load(.acquire),
            .writer => conn.writer_stop.load(.acquire),
        };
    }

    /// The reason for a wait of `task` that ended in `error.Canceled`.
    fn canceledReason(conn: *Conn, task: Task) CloseReason {
        return switch (conn.stopOf(task)) {
            .none, .kill => .local_close,
            .write_deadline => .write_timeout,
            .read_deadline => switch (conn.read_wait.load(.acquire)) {
                .idle => .idle_timeout,
                .head, .progress, .linger => .read_timeout,
                .handler => .handler_timeout,
                .handshake => .handshake_failed,
            },
        };
    }

    fn readError(conn: *Conn) ?anyerror {
        return switch (conn.transport) {
            .stream => conn.stream_reader.err,
            .client => |c| c.stream_reader.err,
        };
    }

    fn writeError(conn: *Conn) ?anyerror {
        return switch (conn.transport) {
            .stream => conn.stream_writer.err,
            .client => |c| c.stream_writer.err,
        };
    }

    /// The reason for a failed read on the transport. Only the reader reads.
    fn readFailure(conn: *Conn) CloseReason {
        if (conn.readError()) |err| {
            if (err != error.Canceled) return .reset;
        }
        if (conn.stopOf(.reader) != .none) return conn.canceledReason(.reader);
        return .reset;
    }

    /// The reason for a failed write by `task` on the transport.
    fn writeFailure(conn: *Conn, task: Task) CloseReason {
        if (conn.writeError()) |err| {
            if (err != error.Canceled) return .reset;
        }
        if (conn.stopOf(task) != .none) return conn.canceledReason(task);
        return .reset;
    }

    fn flush(conn: *Conn) Io.Writer.Error!void {
        switch (conn.transport) {
            .stream => try conn.out.flush(),
            .client => |c| try c.flush(),
        }
    }

    /// Asks the supervisor to cancel this connection's tasks.
    fn requestKill(conn: *Conn, io: Io) void {
        conn.kill_requested.store(true, .release);
        Engine.supervisor_wake.set(io);
    }

    /// Stops the writer once it has drained what is already queued.
    fn closeOutbox(conn: *Conn, io: Io) void {
        conn.mutex.lockUncancelable(io);
        conn.outbox_closing = true;
        conn.mutex.unlock(io);
        conn.changed.broadcast(io);
    }
};

/// Called by each of a connection's tasks as it returns. The last one out
/// closes the transport and files the terminal report.
fn release(conn: *Conn, io: Io) void {
    if (conn.tasks_active.fetchSub(1, .acq_rel) != 1) return;

    conn.disarmWrite();
    conn.mutex.lockUncancelable(io);
    const linger = conn.linger;
    conn.mutex.unlock(io);
    // A cancelled task must return promptly: the supervisor is waiting on it.
    const cancelled = conn.stopOf(.reader) != .none or conn.stopOf(.writer) != .none;
    if (linger and !cancelled) lingerClose(conn, io);
    conn.disarmRead();
    switch (conn.transport) {
        .stream => |stream| stream.close(io),
        .client => |c| c.destroy(io),
    }

    conn.mutex.lockUncancelable(io);
    if (conn.reason == .open) conn.reason = .peer_closed;
    const report = !conn.user_closed;
    conn.mutex.unlock(io);
    if (report) postNotice(io, conn.owner, conn.handle, .closed);

    Engine.mutex.lockUncancelable(io);
    if (conn.role == .server) Engine.live_conns -= 1;
    Engine.mutex.unlock(io);
    Engine.slot_freed.signal(io);

    conn.finished.store(true, .release);
    Engine.supervisor_wake.set(io);
}

/// After the last write of a connection the library is closing on its own
/// initiative, stop sending and read until the peer closes or the linger
/// deadline passes. Closing with unread input makes the kernel reset the
/// connection, which can destroy the response or close frame just written.
fn lingerClose(conn: *Conn, io: Io) void {
    const stream = switch (conn.transport) {
        .stream => |stream| stream,
        .client => return,
    };
    stream.shutdown(io, .send) catch return;
    conn.armRead(io, .linger);
    while (true) {
        const chunk = conn.in.peekGreedy(1) catch return;
        conn.in.toss(chunk.len);
    }
}

// ---------------------------------------------------------------------------
// Supervisor
// ---------------------------------------------------------------------------

const supervisor_tick_ms = 50;

/// Owns every task's `Future`: it alone cancels and awaits them, so no other
/// thread ever blocks on a task. It enforces deadlines, carries out kill
/// requests, and frees connections and listeners whose tasks have returned.
fn supervise() void {
    const io = engineIo();
    while (true) {
        // With no connection to watch there is no deadline to enforce, so an
        // idle server costs no wakeups.
        const idle = blk: {
            Engine.mutex.lockUncancelable(io);
            defer Engine.mutex.unlock(io);
            if (Engine.retired_conns.items.len != 0 or Engine.retired_listeners.items.len != 0) break :blk false;
            var it = Engine.entries.valueIterator();
            while (it.next()) |entry| if (entry.* == .conn) break :blk false;
            break :blk true;
        };
        if (idle) {
            Engine.supervisor_wake.wait(io) catch return;
        } else {
            Engine.supervisor_wake.waitTimeout(io, afterMs(supervisor_tick_ms)) catch |err| switch (err) {
                error.Timeout => {},
                error.Canceled => return,
            };
        }
        Engine.supervisor_wake.reset();
        sweep(io);
    }
}

const Sweep = struct {
    cancel: std.ArrayList(*Conn) = .empty,
    free_conns: std.ArrayList(*Conn) = .empty,
    free_listeners: std.ArrayList(*Listener) = .empty,
};

fn sweep(io: Io) void {
    var work: Sweep = .{};
    defer work.cancel.deinit(gpa);
    defer work.free_conns.deinit(gpa);
    defer work.free_listeners.deinit(gpa);

    const now = nowNs(io);
    {
        Engine.mutex.lockUncancelable(io);
        defer Engine.mutex.unlock(io);

        var it = Engine.entries.valueIterator();
        while (it.next()) |entry| switch (entry.*) {
            .conn => |conn| if (dueForCancel(conn, now)) work.cancel.append(gpa, conn) catch {},
            .listener => {},
        };

        var kept: usize = 0;
        for (Engine.retired_conns.items) |conn| {
            if (conn.finished.load(.acquire)) {
                work.free_conns.append(gpa, conn) catch {
                    Engine.retired_conns.items[kept] = conn;
                    kept += 1;
                };
                continue;
            }
            if (dueForCancel(conn, now)) work.cancel.append(gpa, conn) catch {};
            Engine.retired_conns.items[kept] = conn;
            kept += 1;
        }
        Engine.retired_conns.items.len = kept;

        work.free_listeners.appendSlice(gpa, Engine.retired_listeners.items) catch {};
        if (work.free_listeners.items.len == Engine.retired_listeners.items.len) {
            Engine.retired_listeners.clearRetainingCapacity();
        } else {
            work.free_listeners.clearRetainingCapacity();
        }
    }

    for (work.cancel.items) |conn| cancelTasks(conn, io);
    for (work.free_conns.items) |conn| {
        if (conn.reader_future) |*future| future.await(io);
        if (conn.writer_future) |*future| future.await(io);
        conn.destroy(io);
    }
    for (work.free_listeners.items) |listener| {
        if (listener.future) |*future| future.cancel(io);
        listener.server.deinit(io);
        gpa.destroy(listener);
    }
}

/// Decides whether a connection has a task that must be cancelled now. A kill
/// stops the reader only, so a writer can still drain what is queued; a
/// missed deadline stops both.
fn dueForCancel(conn: *Conn, now: i64) bool {
    if (conn.finished.load(.acquire)) return false;
    const write = conn.write_deadline.load(.acquire);
    const read = conn.read_deadline.load(.acquire);
    const missed: Stop = if (write != 0 and now >= write)
        .write_deadline
    else if (read != 0 and now >= read)
        .read_deadline
    else
        .none;

    var due = false;
    if (missed != .none) {
        if (conn.reader_stop.cmpxchgStrong(.none, missed, .acq_rel, .acquire) == null) due = true;
        if (conn.writer_stop.cmpxchgStrong(.none, missed, .acq_rel, .acquire) == null) due = true;
    } else if (conn.kill_requested.load(.acquire)) {
        if (conn.reader_stop.cmpxchgStrong(.none, .kill, .acq_rel, .acquire) == null) due = true;
    }
    return due;
}

/// Cancels every task of `conn` whose stop slot is set. Cancelling a task
/// that has already been cancelled or has returned is a no-op.
fn cancelTasks(conn: *Conn, io: Io) void {
    if (conn.stopOf(.reader) != .none) {
        if (conn.reader_future) |*future| future.cancel(io);
    }
    if (conn.stopOf(.writer) != .none) {
        // The writer future is published under the connection mutex by the
        // reader task.
        conn.mutex.lockUncancelable(io);
        conn.outbox_closing = true;
        const has_writer = conn.writer_future != null;
        conn.mutex.unlock(io);
        conn.changed.broadcast(io);
        if (has_writer) {
            if (conn.writer_future) |*future| future.cancel(io);
        }
    }
}

// ---------------------------------------------------------------------------
// Listeners
// ---------------------------------------------------------------------------

const Listener = struct {
    handle: i64,
    server: net.Server,
    future: ?Io.Future(void) = null,
};

fn acceptLoop(listener: *Listener) void {
    const io = engineIo();
    while (true) {
        // Take a connection slot before accepting, so connections past the
        // limit wait in the kernel backlog rather than in memory.
        const limits = blk: {
            Engine.mutex.lock(io) catch return;
            defer Engine.mutex.unlock(io);
            while (Engine.live_conns >= Engine.limits.connections) {
                Engine.slot_freed.wait(io, &Engine.mutex) catch return;
            }
            Engine.live_conns += 1;
            break :blk Engine.limits;
        };

        const stream = listener.server.accept(io) catch |err| {
            returnSlot(io);
            if (err == error.Canceled) return;
            // A connection that died in the backlog, or a transient resource
            // shortage: this listener is still good.
            io.sleep(.fromMilliseconds(10), .awake) catch return;
            continue;
        };

        const conn = Conn.create(.server, .{ .stream = stream }, limits) orelse {
            stream.close(io);
            returnSlot(io);
            continue;
        };
        conn.owner = listener.handle;
        conn.tasks_active.store(1, .release);
        conn.reader_future = io.concurrent(serveConnection, .{conn}) catch {
            stream.close(io);
            conn.destroy(io);
            returnSlot(io);
            continue;
        };

        Engine.mutex.lockUncancelable(io);
        conn.handle = Engine.next_handle;
        Engine.next_handle += 1;
        const registered = if (Engine.entries.put(gpa, conn.handle, .{ .conn = conn })) |_| true else |_| false;
        if (registered) {
            Engine.mailbox.append(gpa, .{ .owner = conn.owner, .handle = conn.handle, .kind = .accepted }) catch {};
        } else {
            // Never visible to the program: retire it at once and let its
            // task observe the close.
            conn.user_closed = true;
            Engine.retired_conns.append(gpa, conn) catch {};
        }
        Engine.mutex.unlock(io);
        if (registered) Engine.mail.set(io) else conn.kill_requested.store(true, .release);
        // The supervisor sleeps while there is nothing to watch.
        Engine.supervisor_wake.set(io);
        conn.start.set(io);
    }
}

fn returnSlot(io: Io) void {
    Engine.mutex.lockUncancelable(io);
    Engine.live_conns -= 1;
    Engine.mutex.unlock(io);
    Engine.slot_freed.signal(io);
}

// ---------------------------------------------------------------------------
// HTTP server task
// ---------------------------------------------------------------------------

fn serveConnection(conn: *Conn) void {
    const io = engineIo();
    conn.start.waitUncancelable(io);
    defer release(conn, io);

    const stream = conn.transport.stream;
    conn.stream_reader = stream.reader(io, conn.read_buffer);
    conn.stream_writer = stream.writer(io, &conn.write_buffer);
    conn.in = &conn.stream_reader.interface;
    conn.out = &conn.stream_writer.interface;

    const reason = serveHttp(conn, io);
    conn.setReason(io, reason);
    conn.closeOutbox(io);
}

/// Serves requests until the connection ends, and returns why it ended.
fn serveHttp(conn: *Conn, io: Io) CloseReason {
    var server = http.Server.init(conn.in, conn.out);
    while (true) {
        // Between requests the connection is idle; once a request starts, the
        // rest of its head has a much shorter deadline.
        conn.armRead(io, .idle);
        _ = conn.in.peekGreedy(1) catch |err| switch (err) {
            error.EndOfStream => return .peer_closed,
            error.ReadFailed => return conn.readFailure(),
        };
        conn.armRead(io, .head);
        const request = server.receiveHead() catch |err| switch (err) {
            error.HttpHeadersOversize => return reject(conn, io, 431, .too_large),
            error.HttpHeadersInvalid => return reject(conn, io, 400, .protocol_error),
            error.HttpRequestTruncated, error.HttpConnectionClosing => return .peer_closed,
            error.ReadFailed => return conn.readFailure(),
        };
        const head = request.head;

        // Everything borrowed from the head is copied now: reading the body
        // invalidates it.
        const keep_alive = head.keep_alive;
        const is_head = head.method == .HEAD;
        var websocket_key: []u8 = &.{};
        defer gpa.free(websocket_key);
        switch (request.upgradeRequested()) {
            .websocket => |key| if (key) |k| {
                websocket_key = gpa.dupe(u8, k) catch return .reset;
            },
            .other, .none => {},
        }

        var exchange: Exchange = .{
            .method = @tagName(head.method),
            .keep_alive = keep_alive,
            .wants_websocket = websocket_key.len != 0,
        };
        var published = false;
        defer if (!published) exchange.free(io);
        exchange.target = gpa.dupe(u8, head.target) catch return .reset;
        exchange.head = gpa.dupe(u8, request.head_buffer) catch return .reset;

        if (head.expect) |expect| {
            if (!std.ascii.eqlIgnoreCase(expect, "100-continue")) return reject(conn, io, 417, .protocol_error);
        }
        const declared: u64 = head.content_length orelse 0;
        if (declared > conn.limits.body_bytes) return reject(conn, io, 413, .too_large);
        const has_body = head.transfer_encoding == .chunked or declared > 0;

        if (has_body) {
            if (head.expect != null) {
                conn.armWrite(io);
                conn.out.writeAll("HTTP/1.1 100 Continue\r\n\r\n") catch return conn.writeFailure(.reader);
                conn.flush() catch return conn.writeFailure(.reader);
                conn.disarmWrite();
            }
            var body: std.ArrayList(u8) = .empty;
            defer body.deinit(gpa);
            const body_reader = server.reader.bodyReader(conn.transfer_buffer, head.transfer_encoding, head.content_length);
            while (true) {
                conn.armRead(io, .progress);
                const chunk = body_reader.peekGreedy(1) catch |err| switch (err) {
                    error.EndOfStream => break,
                    error.ReadFailed => {
                        if (server.reader.body_err != null) return reject(conn, io, 400, .protocol_error);
                        return conn.readFailure();
                    },
                };
                if (chunk.len > conn.limits.body_bytes - body.items.len) return reject(conn, io, 413, .too_large);
                body.appendSlice(gpa, chunk) catch return .reset;
                body_reader.toss(chunk.len);
            }
            // A body cut short by the peer closing leaves the reader mid-body.
            if (server.reader.state != .ready) return .peer_closed;
            exchange.body = body.toOwnedSlice(gpa) catch return .reset;
        }
        conn.disarmRead();

        exchange.reserved = exchange.head.len + exchange.body.len;
        reserveBudget(io, exchange.reserved) catch {
            exchange.reserved = 0;
            return conn.canceledReason(.reader);
        };

        // Hand the request to the program and wait for its answer. From here
        // the exchange belongs to the main thread.
        exchange.state = .ready;
        conn.mutex.lockUncancelable(io);
        if (conn.user_closed) {
            conn.mutex.unlock(io);
            return .local_close;
        }
        conn.exchange = exchange;
        published = true;
        conn.phase = .awaiting_reply;
        conn.mutex.unlock(io);

        conn.armRead(io, .handler);
        postNotice(io, conn.owner, conn.handle, .readable);

        var reply: Reply = blk: {
            conn.mutex.lockUncancelable(io);
            defer conn.mutex.unlock(io);
            while (conn.reply.kind == .none) {
                conn.changed.wait(io, &conn.mutex) catch return conn.canceledReason(.reader);
            }
            const taken = conn.reply;
            conn.reply = .{};
            conn.phase = .writing;
            break :blk taken;
        };
        defer reply.free();
        conn.disarmRead();

        switch (reply.kind) {
            .none => unreachable,
            .close => return .local_close,
            .response => {
                conn.armWrite(io);
                writeResponse(conn, reply, keep_alive, is_head) catch return conn.writeFailure(.reader);
                conn.disarmWrite();
                if (!keep_alive) return .local_close;
            },
            .upgrade => {
                conn.armWrite(io);
                writeUpgrade(conn, websocket_key) catch return conn.writeFailure(.reader);
                conn.disarmWrite();
                return serveWebSocket(conn, io);
            },
        }

        conn.mutex.lockUncancelable(io);
        const closed = conn.user_closed;
        conn.phase = .reading;
        conn.mutex.unlock(io);
        if (closed) return .local_close;
    }
}

fn statusPhrase(status: u16) []const u8 {
    const known = std.enums.fromInt(http.Status, status) orelse return "";
    return known.phrase() orelse "";
}

fn writeResponse(conn: *Conn, reply: Reply, keep_alive: bool, is_head: bool) Io.Writer.Error!void {
    const out = conn.out;
    try out.print("HTTP/1.1 {d} {s}\r\n", .{ reply.status, statusPhrase(reply.status) });
    if (!keep_alive) try out.writeAll("connection: close\r\n");
    try out.print("content-length: {d}\r\n", .{reply.body.len});
    try out.writeAll(reply.headers);
    try out.writeAll("\r\n");
    if (!is_head) try out.writeAll(reply.body);
    try conn.flush();
}

/// Answers a request the program never sees, then lets the connection close.
fn reject(conn: *Conn, io: Io, status: u16, reason: CloseReason) CloseReason {
    conn.disarmRead();
    conn.armWrite(io);
    defer conn.disarmWrite();
    conn.out.print("HTTP/1.1 {d} {s}\r\nconnection: close\r\ncontent-length: 0\r\n\r\n", .{
        status, statusPhrase(status),
    }) catch return reason;
    conn.flush() catch return reason;
    conn.linger = true;
    return reason;
}

// ---------------------------------------------------------------------------
// WebSocket codec (both roles)
// ---------------------------------------------------------------------------

const websocket_guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

/// The `Sec-WebSocket-Accept` value for a `Sec-WebSocket-Key` (RFC 6455 §4.2.2).
fn acceptKey(key: []const u8, out: *[28]u8) []const u8 {
    var sha1 = std.crypto.hash.Sha1.init(.{});
    sha1.update(key);
    sha1.update(websocket_guid);
    var digest: [std.crypto.hash.Sha1.digest_length]u8 = undefined;
    sha1.final(&digest);
    return std.base64.standard.Encoder.encode(out, &digest);
}

fn writeUpgrade(conn: *Conn, key: []const u8) Io.Writer.Error!void {
    var accept_value: [28]u8 = undefined;
    try conn.out.print(
        "HTTP/1.1 101 Switching Protocols\r\nconnection: upgrade\r\nupgrade: websocket\r\nsec-websocket-accept: {s}\r\n\r\n",
        .{acceptKey(key, &accept_value)},
    );
    try conn.flush();
}

/// Writes one unfragmented frame. A client masks its payload; a server must
/// not (RFC 6455 §5.1).
fn writeFrame(conn: *Conn, io: Io, frame: Frame) Io.Writer.Error!void {
    const out = conn.out;
    const masked = conn.role == .client;
    const mask_bit: u8 = if (masked) 0x80 else 0;
    var header: [10]u8 = undefined;
    header[0] = 0x80 | @as(u8, frame.opcode);
    const len = frame.data.len;
    var header_len: usize = 2;
    if (len < 126) {
        header[1] = mask_bit | @as(u8, @intCast(len));
    } else if (len <= std.math.maxInt(u16)) {
        header[1] = mask_bit | 126;
        std.mem.writeInt(u16, header[2..4], @intCast(len), .big);
        header_len = 4;
    } else {
        header[1] = mask_bit | 127;
        std.mem.writeInt(u64, header[2..10], len, .big);
        header_len = 10;
    }
    try out.writeAll(header[0..header_len]);
    if (masked) {
        var mask: [4]u8 = undefined;
        io.random(&mask);
        try out.writeAll(&mask);
        // The queue owns the payload, so it is masked in place.
        for (frame.data, 0..) |*byte, i| byte.* ^= mask[i % 4];
    }
    try out.writeAll(frame.data);
    try conn.flush();
}

/// RFC 6455 §5.5.1/§7.4.1: a close payload is empty, or a 2-byte status code
/// followed by a UTF-8 reason.
fn validClosePayload(payload: []const u8) ?u16 {
    if (payload.len == 0) return null;
    if (payload.len == 1) return 1002;
    const code = std.mem.readInt(u16, payload[0..2], .big);
    const code_ok = switch (code) {
        1000...1003, 1007...1014, 3000...4999 => true,
        else => false,
    };
    if (!code_ok) return 1002;
    if (!std.unicode.utf8ValidateSlice(payload[2..])) return 1007;
    return null;
}

/// Reads exactly `dest.len` bytes, re-arming the progress deadline each time
/// the peer delivers some.
fn readExact(conn: *Conn, io: Io, dest: []u8) Io.Reader.Error!void {
    var filled: usize = 0;
    while (filled < dest.len) {
        conn.armRead(io, .progress);
        const chunk = try conn.in.peekGreedy(1);
        const n = @min(chunk.len, dest.len - filled);
        @memcpy(dest[filled..][0..n], chunk[0..n]);
        conn.in.toss(n);
        filled += n;
    }
}

/// Queues a frame for the writer task. Returns false when the queue is closed
/// or the frame would exceed the outbound cap.
fn enqueueFrame(conn: *Conn, io: Io, opcode: u4, payload: []const u8, final: bool) enum { queued, closed, full, no_memory } {
    const data = gpa.dupe(u8, payload) catch return .no_memory;
    conn.mutex.lockUncancelable(io);
    defer {
        conn.mutex.unlock(io);
        conn.changed.broadcast(io);
    }
    if (conn.outbox_closing) {
        gpa.free(data);
        return .closed;
    }
    if (data.len > conn.limits.outbound_bytes - @min(conn.outbox_bytes, conn.limits.outbound_bytes)) {
        gpa.free(data);
        return .full;
    }
    conn.outbox.append(gpa, .{ .opcode = opcode, .data = data }) catch {
        gpa.free(data);
        return .no_memory;
    };
    conn.outbox_bytes += data.len;
    if (final) conn.outbox_closing = true;
    return .queued;
}

/// Fails the connection (RFC 6455 §7.1.7): queue a Close frame with `code` as
/// the last thing sent, and tell the program in order with its messages.
fn failWebSocket(conn: *Conn, io: Io, code: u16, reason: CloseReason) CloseReason {
    conn.disarmRead();
    var payload: [2]u8 = undefined;
    std.mem.writeInt(u16, &payload, code, .big);
    _ = enqueueFrame(conn, io, @intFromEnum(Opcode.close), &payload, true);
    conn.linger = true;

    conn.mutex.lockUncancelable(io);
    const report = !conn.user_closed;
    if (report) conn.inbox.append(gpa, .{ .failed = true }) catch {};
    conn.inbox_waiting.store(conn.inboxLen(), .release);
    conn.mutex.unlock(io);
    if (report) postNotice(io, conn.owner, conn.handle, .readable);
    return reason;
}

/// Hands a complete message to the program, waiting while the connection's
/// undelivered-message credit or the global byte budget is spent. Takes
/// ownership of `data`. Returns a reason when the connection must end instead.
fn deliver(conn: *Conn, io: Io, op: Opcode, data: []u8) ?CloseReason {
    conn.disarmRead();
    reserveBudget(io, data.len) catch {
        gpa.free(data);
        return conn.canceledReason(.reader);
    };
    const refused: ?CloseReason = blk: {
        conn.mutex.lockUncancelable(io);
        defer conn.mutex.unlock(io);
        while (conn.inboxLen() >= conn.limits.inbox_messages and !conn.user_closed) {
            conn.changed.wait(io, &conn.mutex) catch break :blk conn.canceledReason(.reader);
        }
        if (conn.user_closed) break :blk .local_close;
        conn.inbox.append(gpa, .{ .op = @intFromEnum(op), .data = data }) catch break :blk .reset;
        conn.inbox_waiting.store(conn.inboxLen(), .release);
        break :blk null;
    };
    if (refused) |reason| {
        gpa.free(data);
        releaseBudget(io, data.len);
        return reason;
    }
    postNotice(io, conn.owner, conn.handle, .readable);
    return null;
}

/// Reads frames until the connection ends, and returns why. The program's
/// sends are written by `writeLoop`, started here.
fn serveWebSocket(conn: *Conn, io: Io) CloseReason {
    {
        conn.mutex.lockUncancelable(io);
        defer conn.mutex.unlock(io);
        if (conn.user_closed) return .local_close;
        // From here the reader only ever blocks on the socket.
        conn.phase = .reading;
        conn.tasks_active.store(2, .release);
        conn.writer_future = io.concurrent(writeLoop, .{conn}) catch {
            conn.tasks_active.store(1, .release);
            return .reset;
        };
    }

    var assembly: std.ArrayList(u8) = .empty;
    defer assembly.deinit(gpa);
    var assembling: ?Opcode = null;
    const expect_masked = conn.role == .server;

    while (true) {
        conn.disarmRead();
        const head = conn.in.takeArray(2) catch |err| switch (err) {
            error.EndOfStream => return .peer_closed,
            error.ReadFailed => return conn.readFailure(),
        };
        conn.armRead(io, .progress);
        const b0 = head[0];
        const b1 = head[1];
        const fin = b0 & 0x80 != 0;
        const opcode: Opcode = @enumFromInt(@as(u4, @truncate(b0)));
        const masked = b1 & 0x80 != 0;

        const is_control = switch (opcode) {
            .close, .ping, .pong => true,
            .continuation, .text, .binary => false,
            _ => return failWebSocket(conn, io, 1002, .protocol_error),
        };
        if (b0 & 0x70 != 0) return failWebSocket(conn, io, 1002, .protocol_error);
        if (masked != expect_masked) return failWebSocket(conn, io, 1002, .protocol_error);
        if (is_control and !fin) return failWebSocket(conn, io, 1002, .protocol_error);

        const declared: u64 = switch (b1 & 0x7f) {
            126 => std.mem.readInt(u16, conn.in.takeArray(2) catch |err| return frameReadFailure(conn, err), .big),
            127 => std.mem.readInt(u64, conn.in.takeArray(8) catch |err| return frameReadFailure(conn, err), .big),
            else => |short| short,
        };
        if (is_control and declared > 125) return failWebSocket(conn, io, 1002, .protocol_error);
        // The length is the peer's claim: compare before any arithmetic or
        // allocation that trusts it.
        const room = conn.limits.message_bytes - @min(assembly.items.len, conn.limits.message_bytes);
        if (!is_control and declared > room) return failWebSocket(conn, io, 1009, .too_large);
        const len: usize = @intCast(declared);

        var mask: [4]u8 = undefined;
        if (masked) {
            const key = conn.in.takeArray(4) catch |err| return frameReadFailure(conn, err);
            mask = key.*;
        }

        if (is_control) {
            var payload: [125]u8 = undefined;
            readExact(conn, io, payload[0..len]) catch |err| return frameReadFailure(conn, err);
            if (masked) for (payload[0..len], 0..) |*byte, i| {
                byte.* ^= mask[i % 4];
            };
            if (opcode == .close) {
                if (validClosePayload(payload[0..len])) |code| {
                    return failWebSocket(conn, io, code, .protocol_error);
                }
            }
            const copy = gpa.dupe(u8, payload[0..len]) catch return .reset;
            if (deliver(conn, io, opcode, copy)) |reason| return reason;
            continue;
        }

        switch (opcode) {
            .continuation => if (assembling == null) return failWebSocket(conn, io, 1002, .protocol_error),
            .text, .binary => {
                if (assembling != null) return failWebSocket(conn, io, 1002, .protocol_error);
                assembling = opcode;
                assembly.clearRetainingCapacity();
            },
            else => unreachable,
        }

        // Grow as the payload actually arrives rather than trusting the
        // declared length with one allocation.
        var remaining = len;
        var offset: usize = 0;
        while (remaining > 0) {
            const step = @min(remaining, 64 * 1024);
            const dest = assembly.addManyAsSlice(gpa, step) catch return .reset;
            readExact(conn, io, dest) catch |err| return frameReadFailure(conn, err);
            if (masked) for (dest, 0..) |*byte, i| {
                byte.* ^= mask[(offset + i) % 4];
            };
            offset += step;
            remaining -= step;
        }

        if (!fin) continue;
        const kind = assembling.?;
        assembling = null;
        if (kind == .text and !std.unicode.utf8ValidateSlice(assembly.items)) {
            return failWebSocket(conn, io, 1007, .protocol_error);
        }
        const message = assembly.toOwnedSlice(gpa) catch return .reset;
        if (deliver(conn, io, kind, message)) |reason| return reason;
    }
}

fn frameReadFailure(conn: *Conn, err: Io.Reader.Error) CloseReason {
    return switch (err) {
        error.EndOfStream => .peer_closed,
        error.ReadFailed => conn.readFailure(),
    };
}

/// Drains a WebSocket connection's outbound queue to the socket. Returns when
/// the queue is closed and empty, or a write fails.
fn writeLoop(conn: *Conn) void {
    const io = engineIo();
    defer release(conn, io);
    while (true) {
        const frame: Frame = blk: {
            conn.mutex.lockUncancelable(io);
            defer conn.mutex.unlock(io);
            while (conn.outbox.items.len == conn.outbox_head) {
                if (conn.outbox_closing) return;
                conn.changed.wait(io, &conn.mutex) catch return;
            }
            const next = conn.outbox.items[conn.outbox_head];
            conn.outbox_head += 1;
            if (conn.outbox_head == conn.outbox.items.len) {
                conn.outbox.clearRetainingCapacity();
                conn.outbox_head = 0;
            }
            conn.outbox_bytes -= next.data.len;
            break :blk next;
        };
        defer gpa.free(frame.data);

        conn.armWrite(io);
        writeFrame(conn, io, frame) catch {
            // The reader is still blocked on the socket; it has to go too.
            conn.setReason(io, conn.writeFailure(.writer));
            conn.closeOutbox(io);
            conn.requestKill(io);
            return;
        };
        conn.disarmWrite();
    }
}

// ---------------------------------------------------------------------------
// Main-thread surface: lookup helpers
// ---------------------------------------------------------------------------

fn connOf(handle: i64) ?*Conn {
    if (!Engine.started) return null;
    const io = engineIo();
    Engine.mutex.lockUncancelable(io);
    defer Engine.mutex.unlock(io);
    return switch (Engine.entries.get(handle) orelse return null) {
        .conn => |conn| conn,
        .listener => null,
    };
}

/// Removes a connection's handle and leaves the rest to its tasks and the
/// supervisor. Caller holds `Engine.mutex`.
fn retireConnLocked(conn: *Conn) void {
    _ = Engine.entries.remove(conn.handle);
    Engine.retired_conns.append(gpa, conn) catch {};
}

/// Tells a retired connection's tasks to finish: flush what is queued, then
/// stop. A reader blocked on the socket has to be cancelled.
fn stopConn(conn: *Conn, io: Io) void {
    conn.mutex.lockUncancelable(io);
    conn.user_closed = true;
    if (conn.reason == .open) conn.reason = .local_close;
    conn.outbox_closing = true;
    const blocked_reading = conn.phase == .reading;
    if (conn.phase == .awaiting_reply and conn.reply.kind == .none) conn.reply.kind = .close;
    conn.mutex.unlock(io);
    conn.changed.broadcast(io);
    if (blocked_reading) conn.requestKill(io) else Engine.supervisor_wake.set(io);
}

// ---------------------------------------------------------------------------
// Main-thread surface: configuration
// ---------------------------------------------------------------------------

/// Sets one cap or deadline for connections opened from now on. Deadlines are
/// in milliseconds; zero disables one.
pub fn setLimit(key: i64, value: i64) void {
    clearLastError();
    const which = std.enums.fromInt(LimitKey, key) orelse return setLastError(code_invalid_argument);
    if (value < 0) return setLastError(code_invalid_argument);
    ensureStarted();
    const io = engineIo();
    Engine.mutex.lockUncancelable(io);
    defer Engine.mutex.unlock(io);
    const limits = &Engine.limits;
    const size: usize = std.math.cast(usize, value) orelse std.math.maxInt(usize);
    switch (which) {
        .head_bytes => limits.head_bytes = size,
        .body_bytes => limits.body_bytes = size,
        .message_bytes => limits.message_bytes = size,
        .outbound_bytes => limits.outbound_bytes = size,
        .inbox_messages => limits.inbox_messages = @max(size, 1),
        .pending_bytes => limits.pending_bytes = size,
        .connections => limits.connections = @max(size, 1),
        .handshake_ms => limits.handshake_ms = value,
        .head_ms => limits.head_ms = value,
        .read_ms => limits.read_ms = value,
        .write_ms => limits.write_ms = value,
        .idle_ms => limits.idle_ms = value,
        .handler_ms => limits.handler_ms = value,
        .linger_ms => limits.linger_ms = value,
    }
    Engine.slot_freed.broadcast(io);
}

/// Why `handle`'s connection ended, as a `CloseReason` ordinal; `open` while
/// it has not. An unknown handle reads as closed by the program.
pub fn closeReason(handle: i64) i64 {
    const conn = connOf(handle) orelse return @intFromEnum(CloseReason.local_close);
    const io = engineIo();
    conn.mutex.lockUncancelable(io);
    defer conn.mutex.unlock(io);
    return @intFromEnum(conn.reason);
}

// ---------------------------------------------------------------------------
// Main-thread surface: listeners and the mailbox
// ---------------------------------------------------------------------------

pub fn listen(port: i64) i64 {
    clearLastError();
    if (port < 0 or port > 65535) {
        setLastError(code_invalid_argument);
        return 0;
    }
    ensureStarted();
    if (!Engine.concurrent) {
        setLastError(code_not_supported);
        return 0;
    }
    const io = engineIo();
    const address: net.IpAddress = .{ .ip4 = .loopback(@intCast(port)) };
    var server = address.listen(io, .{ .reuse_address = true }) catch |err| {
        setLastError(mapNetworkError(@errorName(err)));
        return 0;
    };
    const listener = gpa.create(Listener) catch {
        server.deinit(io);
        setLastError(code_out_of_memory);
        return 0;
    };

    Engine.mutex.lockUncancelable(io);
    listener.* = .{ .handle = Engine.next_handle, .server = server };
    Engine.next_handle += 1;
    const registered = if (Engine.entries.put(gpa, listener.handle, .{ .listener = listener })) |_| true else |_| false;
    Engine.mutex.unlock(io);
    if (!registered) {
        server.deinit(io);
        gpa.destroy(listener);
        setLastError(code_out_of_memory);
        return 0;
    }

    listener.future = io.concurrent(acceptLoop, .{listener}) catch {
        Engine.mutex.lockUncancelable(io);
        _ = Engine.entries.remove(listener.handle);
        Engine.mutex.unlock(io);
        server.deinit(io);
        gpa.destroy(listener);
        setLastError(code_not_supported);
        return 0;
    };
    Engine.supervisor_wake.set(io);
    return listener.handle;
}

pub fn localAddr(handle: i64) i64 {
    clearLastError();
    if (Engine.started) {
        const io = engineIo();
        Engine.mutex.lockUncancelable(io);
        defer Engine.mutex.unlock(io);
        if (Engine.entries.get(handle)) |entry| switch (entry) {
            .listener => |listener| return listener.server.socket.address.getPort(),
            .conn => {},
        };
    }
    setLastError(code_invalid_argument);
    return 0;
}

const Events = struct {
    var handles: std.ArrayList(i64) = .empty;
    var kinds: std.ArrayList(i64) = .empty;
};

pub fn eventCount() i64 {
    return @intCast(Events.handles.items.len);
}

pub fn eventHandle(index: i64) i64 {
    if (index < 0 or index >= eventCount()) return 0;
    return Events.handles.items[@intCast(index)];
}

pub fn eventKind(index: i64) i64 {
    if (index < 0 or index >= eventCount()) return 0;
    return Events.kinds.items[@intCast(index)];
}

/// Connections whose `closed` notice an earlier `poll` returned are dead from
/// here on.
fn retireDelivered(io: Io) void {
    if (Engine.delivered.items.len == 0) return;
    Engine.mutex.lockUncancelable(io);
    for (Engine.delivered.items) |handle| {
        if (Engine.entries.get(handle)) |entry| switch (entry) {
            .conn => |conn| retireConnLocked(conn),
            .listener => {},
        };
    }
    Engine.mutex.unlock(io);
    Engine.delivered.clearRetainingCapacity();
    Engine.supervisor_wake.set(io);
}

/// Moves every notice for `owner` out of the mailbox, optionally only the
/// first of one kind. Notices for dead handles are dropped. Caller holds
/// `Engine.mutex`.
fn takeNoticesLocked(owner: i64, only: ?NoticeKind) usize {
    var taken: usize = 0;
    var kept: usize = 0;
    for (Engine.mailbox.items) |notice| {
        const alive = Engine.entries.contains(notice.handle);
        const wanted = notice.owner == owner and
            (only == null or (only.? == notice.kind and taken == 0));
        if (alive and !wanted) {
            Engine.mailbox.items[kept] = notice;
            kept += 1;
            continue;
        }
        if (!alive) continue;
        Events.handles.append(gpa, notice.handle) catch continue;
        Events.kinds.append(gpa, @intFromEnum(notice.kind)) catch {
            _ = Events.handles.pop();
            continue;
        };
        taken += 1;
    }
    Engine.mailbox.items.len = kept;
    return taken;
}

/// Reports every connection of `owner` that still has messages waiting. A
/// notice says a message arrived; this says one is still there, so a program
/// that puts off reading is told again instead of waiting forever on a
/// connection the library has stopped reading. Caller holds `Engine.mutex`.
fn remindLocked(owner: i64) usize {
    var reminded: usize = 0;
    var it = Engine.entries.valueIterator();
    while (it.next()) |entry| switch (entry.*) {
        .listener => {},
        .conn => |conn| {
            if (conn.owner != owner or conn.inbox_waiting.load(.acquire) == 0) continue;
            Events.handles.append(gpa, conn.handle) catch continue;
            Events.kinds.append(gpa, @intFromEnum(NoticeKind.readable)) catch {
                _ = Events.handles.pop();
                continue;
            };
            reminded += 1;
        },
    };
    return reminded;
}

/// Waits up to `timeout_ms` for notices belonging to `owner` and moves them
/// into the event cells. Negative waits indefinitely; zero does not wait.
fn collect(owner: i64, timeout_ms: i64, only: ?NoticeKind) i64 {
    const io = engineIo();
    Events.handles.clearRetainingCapacity();
    Events.kinds.clearRetainingCapacity();
    retireDelivered(io);

    const deadline: ?i64 = if (timeout_ms < 0) null else nowNs(io) +| timeout_ms *| std.time.ns_per_ms;
    while (true) {
        Engine.mutex.lockUncancelable(io);
        if (!Engine.entries.contains(owner)) {
            Engine.mutex.unlock(io);
            setLastError(code_invalid_argument);
            return -1;
        }
        var taken = takeNoticesLocked(owner, only);
        if (taken == 0 and only == null) taken = remindLocked(owner);
        if (taken == 0) Engine.mail.reset();
        Engine.mutex.unlock(io);
        if (taken != 0) break;

        if (deadline) |at| {
            const remaining = at - nowNs(io);
            if (remaining <= 0) break;
            Engine.mail.waitTimeout(io, .{ .duration = .{ .raw = .fromNanoseconds(remaining), .clock = .awake } }) catch {};
        } else {
            Engine.mail.wait(io) catch {};
        }
    }

    for (Events.handles.items, Events.kinds.items) |handle, kind| {
        if (kind == @intFromEnum(NoticeKind.closed)) Engine.delivered.append(gpa, handle) catch {};
    }
    return @intCast(Events.handles.items.len);
}

pub fn poll(owner: i64, timeout_ms: i64) i64 {
    clearLastError();
    if (!Engine.started) {
        setLastError(code_invalid_argument);
        return -1;
    }
    return collect(owner, timeout_ms, null);
}

/// Blocks until `listener` has accepted a connection and returns its handle.
pub fn accept(listener: i64) i64 {
    clearLastError();
    if (!Engine.started) {
        setLastError(code_invalid_argument);
        return 0;
    }
    if (collect(listener, -1, .accepted) != 1) return 0;
    const handle = Events.handles.items[0];
    Events.handles.clearRetainingCapacity();
    Events.kinds.clearRetainingCapacity();
    return handle;
}

pub fn close(handle: i64) void {
    clearLastError();
    if (!Engine.started) return setLastError(code_invalid_argument);
    const io = engineIo();

    var owned: std.ArrayList(*Conn) = .empty;
    defer owned.deinit(gpa);
    var found = false;
    {
        Engine.mutex.lockUncancelable(io);
        defer Engine.mutex.unlock(io);
        if (Engine.entries.get(handle)) |entry| {
            found = true;
            switch (entry) {
                .conn => |conn| {
                    retireConnLocked(conn);
                    owned.append(gpa, conn) catch {};
                },
                .listener => |listener| {
                    // A listener's connections report through it, so they go
                    // with it.
                    var it = Engine.entries.valueIterator();
                    while (it.next()) |other| switch (other.*) {
                        .conn => |conn| if (conn.owner == handle) owned.append(gpa, conn) catch {},
                        .listener => {},
                    };
                    for (owned.items) |conn| retireConnLocked(conn);
                    _ = Engine.entries.remove(handle);
                    Engine.retired_listeners.append(gpa, listener) catch {};
                },
            }
        }
    }
    if (!found) return setLastError(code_invalid_argument);
    for (owned.items) |conn| stopConn(conn, io);
    Engine.supervisor_wake.set(io);
}

// ---------------------------------------------------------------------------
// Main-thread surface: HTTP requests and responses
// ---------------------------------------------------------------------------

/// 1 = a request is waiting (`takeRequest`), 0 = none yet, -1 = the
/// connection has ended or the handle is unknown.
pub fn readRequest(handle: i64) i64 {
    clearLastError();
    const conn = connOf(handle) orelse {
        setLastError(code_invalid_argument);
        return -1;
    };
    const io = engineIo();
    conn.mutex.lockUncancelable(io);
    defer conn.mutex.unlock(io);
    if (conn.exchange.state == .ready) return 1;
    if (conn.reason != .open) return -1;
    return 0;
}

fn exchangeOf(handle: i64) ?*Exchange {
    const conn = connOf(handle) orelse return null;
    const io = engineIo();
    conn.mutex.lockUncancelable(io);
    defer conn.mutex.unlock(io);
    if (conn.exchange.state != .ready) return null;
    return &conn.exchange;
}

pub fn isWebSocket(handle: i64) bool {
    const conn = connOf(handle) orelse return false;
    const io = engineIo();
    conn.mutex.lockUncancelable(io);
    defer conn.mutex.unlock(io);
    return conn.websocket;
}

pub fn requestMethod(handle: i64) []const u8 {
    const exchange = exchangeOf(handle) orelse return "";
    return exchange.method;
}

pub fn requestTarget(handle: i64) []const u8 {
    const exchange = exchangeOf(handle) orelse return "";
    return exchange.target;
}

pub fn requestHead(handle: i64) []const u8 {
    const exchange = exchangeOf(handle) orelse return "";
    return exchange.head;
}

pub fn requestBody(handle: i64) []const u8 {
    const exchange = exchangeOf(handle) orelse return "";
    return exchange.body;
}

pub fn keepAlive(handle: i64) bool {
    const exchange = exchangeOf(handle) orelse return false;
    return exchange.keep_alive;
}

fn validHeaderName(name: []const u8) bool {
    if (name.len == 0) return false;
    for (name) |c| {
        if (std.ascii.isAlphanumeric(c)) continue;
        if (std.mem.indexOfScalar(u8, "!#$%&'*+-.^_`|~", c) == null) return false;
    }
    return true;
}

fn validHeaderValue(value: []const u8) bool {
    for (value) |c| {
        if ((c < 32 and c != '\t') or c == 127) return false;
    }
    return true;
}

fn isFramingHeader(name: []const u8) bool {
    return std.ascii.eqlIgnoreCase(name, "content-length") or
        std.ascii.eqlIgnoreCase(name, "transfer-encoding") or
        std.ascii.eqlIgnoreCase(name, "connection");
}

/// Normalizes caller-supplied header lines into `Name: value\r\n` form,
/// rejecting anything that could break framing. Lines are separated by CRLF;
/// a trailing separator is optional. `reserved` names the headers the library
/// writes itself.
fn normalizeHeaders(blob: []const u8, comptime reserved: fn ([]const u8) bool) error{ InvalidArgument, OutOfMemory }![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(gpa);
    var lines = std.mem.splitSequence(u8, blob, "\r\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const colon = std.mem.indexOfScalar(u8, line, ':') orelse return error.InvalidArgument;
        const name = line[0..colon];
        const value = std.mem.trim(u8, line[colon + 1 ..], " \t");
        if (!validHeaderName(name) or !validHeaderValue(value) or reserved(name)) return error.InvalidArgument;
        try out.appendSlice(gpa, name);
        try out.appendSlice(gpa, ": ");
        try out.appendSlice(gpa, value);
        try out.appendSlice(gpa, "\r\n");
    }
    return out.toOwnedSlice(gpa);
}

/// Publishes the program's answer to the pending request and frees the
/// request: the program is done with it. Takes ownership of `reply`.
fn answer(handle: i64, reply: Reply) void {
    var owned = reply;
    const conn = connOf(handle) orelse {
        owned.free();
        return setLastError(code_invalid_argument);
    };
    const io = engineIo();
    var answered: Exchange = .{};
    {
        conn.mutex.lockUncancelable(io);
        defer conn.mutex.unlock(io);
        const exchange = &conn.exchange;
        const refused = exchange.state != .ready or conn.reply.kind != .none or
            (owned.kind == .upgrade and !exchange.wants_websocket);
        if (refused) {
            owned.free();
            return setLastError(code_invalid_argument);
        }
        conn.reply = owned;
        if (owned.kind == .upgrade) conn.websocket = true;
        answered = exchange.*;
        exchange.* = .{};
    }
    conn.changed.broadcast(io);
    answered.free(io);
}

pub fn respond(handle: i64, status: i64, headers: []const u8, body: []const u8) void {
    clearLastError();
    // 1xx responses are interim; the library sends the only ones it supports.
    if (status < 200 or status > 999) return setLastError(code_invalid_argument);
    const header_lines = normalizeHeaders(headers, isFramingHeader) catch |err| {
        return setLastError(if (err == error.OutOfMemory) code_out_of_memory else code_invalid_argument);
    };
    const body_copy = gpa.dupe(u8, body) catch {
        gpa.free(header_lines);
        return setLastError(code_out_of_memory);
    };
    answer(handle, .{
        .kind = .response,
        .status = @intCast(status),
        .headers = header_lines,
        .body = body_copy,
    });
}

/// Accepts the pending request's upgrade to a WebSocket. The handshake is
/// written by the connection's task; sends may be queued at once.
pub fn upgradeWebSocket(handle: i64) i64 {
    clearLastError();
    answer(handle, .{ .kind = .upgrade });
    return if (ErrorCell.last == 0) 0 else 1;
}

// ---------------------------------------------------------------------------
// Main-thread surface: WebSocket messages
// ---------------------------------------------------------------------------

pub fn wsSend(handle: i64, op: i64, data: []const u8) void {
    clearLastError();
    const conn = connOf(handle) orelse return setLastError(code_invalid_argument);
    const io = engineIo();
    const opcode: Opcode = switch (op) {
        1 => .text,
        2 => .binary,
        8 => .close,
        9 => .ping,
        10 => .pong,
        else => return setLastError(code_invalid_argument),
    };
    const is_control = opcode == .close or opcode == .ping or opcode == .pong;
    if (is_control and data.len > 125) return setLastError(code_invalid_argument);
    {
        conn.mutex.lockUncancelable(io);
        defer conn.mutex.unlock(io);
        if (!conn.websocket) return setLastError(code_invalid_argument);
    }
    switch (enqueueFrame(conn, io, @intFromEnum(opcode), data, false)) {
        .queued => {},
        .closed => setLastError(code_invalid_argument),
        .no_memory => setLastError(code_out_of_memory),
        .full => {
            // The peer is not reading as fast as the program sends.
            conn.setReason(io, .slow_consumer);
            conn.closeOutbox(io);
            conn.requestKill(io);
            setLastError(code_write_failed);
        },
    }
}

/// 1 = a message is ready (`wsMessageOp`/`wsMessageData`), 0 = none yet,
/// 2 = the peer closed, -1 = the connection failed (`takeLastErrorCode`).
pub fn wsNext(handle: i64) i64 {
    clearLastError();
    const conn = connOf(handle) orelse {
        setLastError(code_invalid_argument);
        return -1;
    };
    const io = engineIo();
    var freed: usize = 0;
    defer releaseBudget(io, freed);
    conn.mutex.lockUncancelable(io);
    defer {
        conn.mutex.unlock(io);
        conn.changed.broadcast(io);
    }
    if (!conn.websocket) {
        setLastError(code_invalid_argument);
        return -1;
    }
    if (conn.ws_ended) return 2;
    if (conn.inboxLen() == 0) {
        if (conn.reason == .open) return 0;
        conn.ws_ended = true;
        return 2;
    }
    conn.current.free();
    conn.current = conn.inbox.items[conn.inbox_head];
    conn.inbox_head += 1;
    if (conn.inbox_head == conn.inbox.items.len) {
        conn.inbox.clearRetainingCapacity();
        conn.inbox_head = 0;
    }
    conn.inbox_waiting.store(conn.inboxLen(), .release);
    if (conn.current.failed) {
        conn.ws_ended = true;
        setLastError(code_invalid_data);
        return -1;
    }
    freed = conn.current.data.len;
    if (conn.current.op == @intFromEnum(Opcode.close)) conn.ws_ended = true;
    return 1;
}

pub fn wsMessageOp(handle: i64) i64 {
    const conn = connOf(handle) orelse return 0;
    return conn.current.op;
}

pub fn wsMessageData(handle: i64) []const u8 {
    const conn = connOf(handle) orelse return "";
    return conn.current.data;
}

/// How many messages are waiting for `wsNext`.
pub fn wsPending(handle: i64) i64 {
    const conn = connOf(handle) orelse return 0;
    return @intCast(conn.inbox_waiting.load(.acquire));
}

// ---------------------------------------------------------------------------
// Route patterns
// ---------------------------------------------------------------------------

// The Router itself is Doxa data (verbs, patterns, ids); only the segment
// matching lives here. Captures are owned copies drained by the Doxa matcher
// before the next call: process-lifetime cells must never alias arena strings.
const Captures = struct {
    var names: std.ArrayList([]u8) = .empty;
    var values: std.ArrayList([]u8) = .empty;

    fn clear() void {
        for (names.items) |name| gpa.free(name);
        for (values.items) |value| gpa.free(value);
        names.clearRetainingCapacity();
        values.clearRetainingCapacity();
    }
};

fn segmentEnd(text: []const u8, start: usize) usize {
    return std.mem.indexOfScalarPos(u8, text, start, '/') orelse text.len;
}

/// A `:name` segment captures the corresponding path segment as `name`.
/// Returns 1 on match, 0 on mismatch, and -1 on OOM.
pub fn matchPattern(pattern: []const u8, path: []const u8) i64 {
    Captures.clear();
    var p: usize = 0;
    var q: usize = 0;
    while (true) {
        const p_end = segmentEnd(pattern, p);
        const q_end = segmentEnd(path, q);
        const p_segment = pattern[p..p_end];
        const q_segment = path[q..q_end];
        if (p_segment.len >= 2 and p_segment[0] == ':') {
            const name = gpa.dupe(u8, p_segment[1..]) catch return -1;
            Captures.names.append(gpa, name) catch {
                gpa.free(name);
                return -1;
            };
            const value = gpa.dupe(u8, q_segment) catch return -1;
            Captures.values.append(gpa, value) catch {
                gpa.free(value);
                return -1;
            };
        } else if (!std.mem.eql(u8, p_segment, q_segment)) {
            return 0;
        }
        const p_done = p_end >= pattern.len;
        const q_done = q_end >= path.len;
        if (p_done or q_done) return if (p_done and q_done) 1 else 0;
        p = p_end + 1;
        q = q_end + 1;
    }
}

pub fn paramCount() i64 {
    return @intCast(@min(Captures.names.items.len, Captures.values.items.len));
}

pub fn paramNameAt(index: i64) []const u8 {
    if (index < 0 or index >= paramCount()) return "";
    return Captures.names.items[@intCast(index)];
}

pub fn paramValueAt(index: i64) []const u8 {
    if (index < 0 or index >= paramCount()) return "";
    return Captures.values.items[@intCast(index)];
}

// ---------------------------------------------------------------------------
// HTTP client
// ---------------------------------------------------------------------------

const Client = struct {
    var shared: ?http.Client = null;

    /// The response cells of the last request, drained by the Doxa wrapper
    /// and then released. Main thread only.
    var status_code: i64 = 0;
    var raw_headers: []u8 = &.{};
    var body: []u8 = &.{};
};

fn httpClient() *http.Client {
    if (Client.shared == null) {
        Client.shared = .{ .allocator = gpa, .io = engineIo() };
    }
    return &Client.shared.?;
}

pub fn takeStatusCode() i64 {
    const status = Client.status_code;
    Client.status_code = 0;
    return status;
}

pub fn takeResponseHeaders() []const u8 {
    return Client.raw_headers;
}

/// Frees the response cells once the Doxa wrapper has cloned what it needs.
/// Idempotent.
pub fn releaseResponse() void {
    gpa.free(Client.raw_headers);
    gpa.free(Client.body);
    Client.raw_headers = &.{};
    Client.body = &.{};
    Client.status_code = 0;
}

fn isHopHeader(name: []const u8) bool {
    return std.ascii.eqlIgnoreCase(name, "host") or isFramingHeader(name);
}

fn isCredentialHeader(name: []const u8) bool {
    return std.ascii.eqlIgnoreCase(name, "authorization") or
        std.ascii.eqlIgnoreCase(name, "proxy-authorization") or
        std.ascii.eqlIgnoreCase(name, "cookie") or
        std.ascii.eqlIgnoreCase(name, "cookie2");
}

fn sameOrigin(a: std.Uri, b: std.Uri) bool {
    if (!std.ascii.eqlIgnoreCase(a.scheme, b.scheme)) return false;
    const protocol_a = http.Client.Protocol.fromUri(a) orelse return false;
    const protocol_b = http.Client.Protocol.fromUri(b) orelse return false;
    if (protocol_a != protocol_b) return false;
    var host_a_buffer: [net.HostName.max_len]u8 = undefined;
    var host_b_buffer: [net.HostName.max_len]u8 = undefined;
    const host_a = a.getHost(&host_a_buffer) catch return false;
    const host_b = b.getHost(&host_b_buffer) catch return false;
    const default_port: u16 = if (protocol_a == .tls) 443 else 80;
    return net.HostName.eql(host_a, host_b) and
        (a.port orelse default_port) == (b.port orelse default_port);
}

/// One outgoing request, run to completion on a task while the main thread
/// waits under the caller's deadline. Inputs are borrowed from the caller,
/// which outlives the task; outputs are owned.
const Job = struct {
    method: []const u8,
    url: []const u8,
    headers: []const u8,
    body: []const u8,
    max_redirects: u16,
    /// When set, the response body is streamed here instead of buffered.
    destination: ?[]const u8 = null,

    code: i64 = 0,
    status: i64 = 0,
    raw_headers: []u8 = &.{},
    response_body: []u8 = &.{},
    done: Io.Event = .unset,

    fn fail(job: *Job, code: i64) void {
        job.code = code;
        gpa.free(job.raw_headers);
        gpa.free(job.response_body);
        job.raw_headers = &.{};
        job.response_body = &.{};
        job.status = 0;
    }
};

fn runJob(job: *Job) void {
    const io = engineIo();
    defer job.done.set(io);
    executeJob(job, io) catch |err| job.fail(switch (err) {
        error.InvalidArgument => code_invalid_argument,
        error.OutOfMemory => code_out_of_memory,
        error.ConnectionFailed => code_connection_failed,
        error.WriteFailed => code_write_failed,
        error.CreateFailed => code_create_failed,
    });
}

const JobError = error{ InvalidArgument, OutOfMemory, ConnectionFailed, WriteFailed, CreateFailed };

fn executeJob(job: *Job, io: Io) JobError!void {
    var method = std.meta.stringToEnum(http.Method, job.method) orelse return error.InvalidArgument;
    if (job.body.len != 0 and !method.requestHasBody()) return error.InvalidArgument;

    const header_lines = try normalizeHeaders(job.headers, isHopHeader);
    defer gpa.free(header_lines);
    var headers: std.ArrayList(http.Header) = .empty;
    defer headers.deinit(gpa);
    var lines = std.mem.splitSequence(u8, header_lines, "\r\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const colon = std.mem.indexOfScalar(u8, line, ':').?;
        try headers.append(gpa, .{ .name = line[0..colon], .value = line[colon + 2 ..] });
    }

    var uri = std.Uri.parse(job.url) catch return error.InvalidArgument;
    var redirects_left = job.max_redirects;
    var redirect_buffers: [2][8 * 1024]u8 = undefined;
    var redirect_index: usize = 0;

    while (true) {
        var request = httpClient().request(method, uri, .{
            .redirect_behavior = .unhandled,
            .headers = .{ .accept_encoding = .omit },
            .extra_headers = headers.items,
        }) catch |err| return if (err == error.OutOfMemory) error.OutOfMemory else error.ConnectionFailed;
        defer request.deinit();

        if (method.requestHasBody()) {
            const mutable_body = try gpa.dupe(u8, job.body);
            defer gpa.free(mutable_body);
            request.sendBodyComplete(mutable_body) catch return error.ConnectionFailed;
        } else {
            request.sendBodiless() catch return error.ConnectionFailed;
        }

        var response = request.receiveHead(&.{}) catch return error.ConnectionFailed;

        if (response.head.status.class() == .redirect and response.head.location != null) {
            if (redirects_left == 0) return error.ConnectionFailed;
            const location = response.head.location.?;
            redirect_index = 1 - redirect_index;
            const buffer = &redirect_buffers[redirect_index];
            if (location.len > buffer.len) return error.ConnectionFailed;
            @memcpy(buffer[0..location.len], location);
            const previous = uri;
            var aux: []u8 = buffer[0..];
            uri = uri.resolveInPlace(location.len, &aux) catch return error.ConnectionFailed;
            var discard: [1024]u8 = undefined;
            _ = response.reader(&discard).discardRemaining() catch return error.ConnectionFailed;
            if (!sameOrigin(previous, uri)) {
                var kept: usize = 0;
                for (headers.items) |header| {
                    if (isCredentialHeader(header.name)) continue;
                    headers.items[kept] = header;
                    kept += 1;
                }
                headers.items.len = kept;
            }
            redirects_left -= 1;
            const status = response.head.status;
            if (status == .see_other or
                ((status == .moved_permanently or status == .found) and method == .POST))
            {
                method = .GET;
            }
            continue;
        }

        job.status = @intFromEnum(response.head.status);
        job.raw_headers = try gpa.dupe(u8, response.head.bytes);
        var transfer_buffer: [16 * 1024]u8 = undefined;
        const reader = response.reader(&transfer_buffer);

        if (job.destination) |path| {
            var file = Io.Dir.cwd().createFile(io, path, .{}) catch return error.CreateFailed;
            defer file.close(io);
            var file_buffer: [16 * 1024]u8 = undefined;
            var file_writer = file.writer(io, &file_buffer);
            _ = reader.streamRemaining(&file_writer.interface) catch |err| switch (err) {
                error.ReadFailed => return error.ConnectionFailed,
                error.WriteFailed => return error.WriteFailed,
            };
            file_writer.interface.flush() catch return error.WriteFailed;
            return;
        }

        var collected: std.ArrayList(u8) = .empty;
        defer collected.deinit(gpa);
        reader.appendRemaining(gpa, &collected, .unlimited) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ReadFailed, error.StreamTooLong => return error.ConnectionFailed,
        };
        job.response_body = try collected.toOwnedSlice(gpa);
        return;
    }
}

/// Runs `job` under one deadline that covers name resolution, connecting,
/// the TLS handshake, sending and receiving. Zero means no deadline.
fn performJob(job: *Job, timeout_ms: i64) void {
    clearLastError();
    releaseResponse();
    if (job.url.len == 0 or timeout_ms < 0) return setLastError(code_invalid_argument);
    ensureStarted();
    const io = engineIo();

    if (!Engine.concurrent) {
        runJob(job);
    } else {
        var future = io.concurrent(runJob, .{job}) catch return setLastError(code_connection_failed);
        if (timeout_ms == 0) {
            job.done.wait(io) catch {};
            future.await(io);
        } else if (job.done.waitTimeout(io, afterMs(timeout_ms))) |_| {
            future.await(io);
        } else |_| {
            future.cancel(io);
            // Cancellation surfaces inside the request as a transport error;
            // report what actually happened.
            if (job.code != 0) job.fail(code_timeout);
        }
    }

    if (job.code != 0) return setLastError(job.code);
    Client.status_code = job.status;
    Client.raw_headers = job.raw_headers;
    Client.body = job.response_body;
}

pub fn performRequest(method: []const u8, url: []const u8, headers: []const u8, body: []const u8, timeout_ms: i64, max_redirects: i64) []const u8 {
    if (max_redirects < 0 or max_redirects >= std.math.maxInt(u16)) {
        clearLastError();
        setLastError(code_invalid_argument);
        return "";
    }
    var job: Job = .{
        .method = method,
        .url = url,
        .headers = headers,
        .body = body,
        .max_redirects = @intCast(max_redirects),
    };
    performJob(&job, timeout_ms);
    return Client.body;
}

pub fn getText(url: []const u8, timeout_ms: i64) []const u8 {
    return performRequest("GET", url, "", "", timeout_ms, 3);
}

fn filenameFromUrl(url: []const u8) []const u8 {
    const without_query = url[0 .. std.mem.indexOfScalar(u8, url, '?') orelse url.len];
    const without_fragment = without_query[0 .. std.mem.indexOfScalar(u8, without_query, '#') orelse without_query.len];
    const trimmed = std.mem.trimEnd(u8, without_fragment, "/");
    const name = trimmed[if (std.mem.lastIndexOfScalar(u8, trimmed, '/')) |slash| slash + 1 else 0..];
    return if (name.len == 0) "downloaded.bin" else name;
}

pub fn downloadUrl(url: []const u8, destination: []const u8) void {
    if (url.len == 0 or destination.len == 0) {
        clearLastError();
        return setLastError(code_invalid_argument);
    }
    var job: Job = .{
        .method = "GET",
        .url = url,
        .headers = "",
        .body = "",
        .max_redirects = 3,
        .destination = destination,
    };
    performJob(&job, 30_000);
    const status = Client.status_code;
    const failed = ErrorCell.last != 0;
    releaseResponse();
    if (!failed and (status < 200 or status >= 300)) setLastError(code_http_status);
}

pub fn download(url: []const u8) void {
    downloadUrl(url, filenameFromUrl(url));
}

// ---------------------------------------------------------------------------
// Response and request header access
// ---------------------------------------------------------------------------

const HeaderLine = struct { name: []const u8, value: []const u8 };

fn headerAt(raw_headers: []const u8, wanted: usize) ?HeaderLine {
    var lines = std.mem.splitSequence(u8, raw_headers, "\r\n");
    _ = lines.next(); // status or request line
    var index: usize = 0;
    while (lines.next()) |line| {
        if (line.len == 0) break;
        const colon = std.mem.indexOfScalar(u8, line, ':') orelse continue;
        if (index == wanted) return .{
            .name = line[0..colon],
            .value = std.mem.trim(u8, line[colon + 1 ..], " \t"),
        };
        index += 1;
    }
    return null;
}

pub fn headerCount(raw_headers: []const u8) i64 {
    var count: usize = 0;
    while (headerAt(raw_headers, count) != null) : (count += 1) {}
    return @intCast(count);
}

pub fn headerNameAt(raw_headers: []const u8, index: i64) []const u8 {
    if (index < 0) return "";
    return if (headerAt(raw_headers, @intCast(index))) |header| header.name else "";
}

pub fn headerValueAt(raw_headers: []const u8, index: i64) []const u8 {
    if (index < 0) return "";
    return if (headerAt(raw_headers, @intCast(index))) |header| header.value else "";
}

pub fn headerNameMatches(raw_headers: []const u8, index: i64, name: []const u8) bool {
    if (index < 0) return false;
    const header = headerAt(raw_headers, @intCast(index)) orelse return false;
    return std.ascii.eqlIgnoreCase(header.name, name);
}
