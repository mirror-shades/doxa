//! The one fault path: every way a program fails at run time ends here, in
//! `report`. A leaf below the rest of the runtime, so an arena or a map that
//! runs out of memory reaches it as a program fault does (`doxa_fault`).

const std = @import("std");
const builtin = @import("builtin");
const instrument = @import("instrument.zig");
const win32 = @import("win32.zig");

/// Write a slice to a raw WASI file descriptor. `std.Io.Threaded`'s syscall
/// path is not usable under wasi preview1 (writes fail with ENOTCAPABLE), so —
/// like the Windows branch — the runtime uses the platform's direct write call.
pub fn writeWasiFd(fd: i32, slice: []const u8) void {
    if (slice.len == 0) return;
    var iov = [1]std.os.wasi.ciovec_t{.{ .base = slice.ptr, .len = slice.len }};
    var written: usize = 0;
    _ = std.os.wasi.fd_write(fd, &iov, 1, &written);
}

pub fn writeStderr(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const handle = win32.GetStdHandle(win32.STD_ERROR_HANDLE);
        if (handle == std.os.windows.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win32.WriteFile(handle, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    if (builtin.os.tag == .wasi) {
        writeWasiFd(2, slice);
        return;
    }
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writerStreaming(std.Io.Threaded.global_single_threaded.io(), &stderr_buffer);
    const stderr = &stderr_writer.interface;
    // `write` may take only part of a slice longer than the buffer.
    stderr.writeAll(slice) catch return;
    stderr.flush() catch return;
}

// ── Faults ──
//
// Every way a program fails at run time ends in `report`, one line on
// stderr: what happened, where, then its details —
// `Index out of bounds at main.doxa:4:12: index 7, length 3`.

pub const FaultKind = enum(u32) {
    panic,
    assert_fail,
    unreachable_code,
    div_by_zero,
    overflow,
    index_out_of_bounds,
    pop_empty,
    parse_int,
    parse_float,
    parse_byte,
    /// `--instrument=memcheck`: a read of memory its arena reclaimed. The
    /// verifier rules this out, so it is a compiler or runtime bug.
    use_after_reclaim,
    /// An arena or map could not get memory from the system. Raised by the
    /// runtime, which has no site for it: under `-g` the backtrace names the
    /// program's frames.
    out_of_memory,
    /// The program's stack ran out, raised by the platform's trap on the
    /// stack's guard page (`installHandlers`). It has no site: under `-g` the
    /// backtrace names the frames, starting from the one that overflowed.
    stack_overflow,

    fn headline(self: FaultKind) []const u8 {
        return switch (self) {
            .panic => "Panic",
            .assert_fail => "Assertion failed",
            .unreachable_code => "Reached unreachable code",
            .div_by_zero => "Division by zero",
            .overflow => "Integer overflow",
            .index_out_of_bounds => "Index out of bounds",
            .pop_empty => "Pop from an empty array",
            .parse_int => "Not a valid int",
            .parse_float => "Not a valid float",
            .parse_byte => "Not a valid byte",
            .use_after_reclaim => "Read of memory its arena reclaimed",
            .out_of_memory => "Out of memory",
            .stack_overflow => "Stack overflow",
        };
    }

    /// 2 is reserved for a compiler-internal trap; every program fault is 1.
    fn exitCode(self: FaultKind) u8 {
        return if (self == .unreachable_code or self == .use_after_reclaim) 2 else 1;
    }
};

/// Where a fault can happen: one constant per site in the program's IR, so
/// the runtime stays program-independent. `file` is the record's display path.
pub const DoxaFaultSite = extern struct {
    kind: u32,
    line: u32,
    column: u32,
    file: [*:0]const u8,
};

/// Set by a program built with debug information: only then do its frames
/// carry lines. The runtime's own frames always do, so their presence cannot
/// decide it.
pub var backtrace_enabled = false;

/// The return address of the program's `__doxa_enter` (`emit.zig`): the
/// return into `__doxa_init` from the call into the entry. A backtrace
/// stops there, as every frame past it is the compiler's. Null until the
/// entry runs, and in a program built without debug information.
export var doxa_entry_return: ?*const anyopaque = null;

/// Frames a backtrace holds at most.
const backtrace_depth = 64;

/// Where a fault's backtrace comes from.
pub const Trace = union(enum) {
    /// Unwind the calling thread, from the faulting call's return address.
    unwind_from: usize,
    /// Return addresses captured already: a trap's, unwound on the thread
    /// that trapped, which is the only thread that can unwind its stack.
    captured: std.debug.StackTrace,
};

/// Up to `addresses.len` return addresses, innermost first. Only safe
/// unwinding: a fault handler must never crash or hang. An unsafe
/// frame-pointer walk read garbage on macOS (a crash after the fault's line,
/// or a hang).
fn captureTrace(options: std.debug.StackUnwindOptions, addresses: []usize) std.debug.StackTrace {
    var safe = options;
    safe.allow_unsafe_unwind = false;
    return std.debug.captureCurrentStackTrace(safe, addresses);
}

/// The program's frames, innermost first, from the faulting one:
/// `  in <name> at <file>:<line>:<column>`. A frame whose source is not a
/// `.doxa` file is the runtime's or the entry glue's, and is skipped. The
/// names are the display names the `!DISubprogram`s carry.
///
/// The trace is bounded: at most `backtrace_depth` frames, a run of one frame
/// repeated (deep recursion) prints once with its count, and a trace cut
/// short says so.
///
/// When no frame can be shown, one line says why: a backtrace that silently
/// vanishes is a bug no one sees.
fn writeBacktrace(source: Trace) void {
    if (!std.options.allow_stack_tracing) return noBacktrace("stack tracing is disabled in this runtime", null);
    var addresses: [backtrace_depth]usize = undefined;
    const trace = switch (source) {
        .unwind_from => |first| captureTrace(.{ .first_address = first }, &addresses),
        .captured => |captured| captured,
    };
    const info = std.debug.getSelfDebugInfo() catch |err| return noBacktrace("cannot read the program's debug information", err);
    if (trace.return_addresses.len == 0) return noBacktrace("the stack could not be unwound to the faulting frame", null);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const io = std.Options.debug_io;
    var frames: FrameRuns = .{};
    var reached_entry = false;
    var failure: ?struct { address: usize, err: anyerror } = null;
    for (trace.return_addresses) |return_address| {
        if (doxa_entry_return) |end| if (return_address == @intFromPtr(end)) {
            reached_entry = true;
            break;
        };
        var symbols: std.ArrayList(std.debug.Symbol) = .empty;
        // A return address follows its call; one before it is in the call.
        info.getSymbols(io, arena.allocator(), arena.allocator(), return_address -| 1, true, &symbols) catch |err| {
            if (failure == null) failure = .{ .address = return_address -| 1, .err = err };
            continue;
        };
        for (symbols.items) |symbol| {
            const location = symbol.source_location orelse continue;
            // Line 0 is code the compiler made, which no source line stands for.
            if (location.line == 0 or !std.mem.endsWith(u8, location.file_name, ".doxa")) continue;
            frames.add(.{
                .name = symbol.name orelse "?",
                .file = framePath(location.file_name),
                .line = location.line,
                .column = location.column,
            });
        }
    }
    frames.flush();
    if (frames.shown != 0) {
        // Cut at the bound. `skipped` cannot say: it is also `.unknown` where
        // the walk stops because only an unsafe unwind is left.
        if (!reached_entry and trace.return_addresses.len == backtrace_depth) writeStderr("  ... more frames not shown\n");
        return;
    }
    if (failure) |f| return unresolved(info, io, arena.allocator(), f.address, f.err);
    noBacktrace("no frame has a .doxa source line", null);
}

/// The backtrace's lines, with a run of one frame folded into one line and
/// its count, so a deep recursion reads as the frame it repeats.
const FrameRuns = struct {
    const Frame = struct {
        name: []const u8,
        file: []const u8,
        line: u64,
        column: u64,

        fn eql(a: Frame, b: Frame) bool {
            return a.line == b.line and a.column == b.column and
                std.mem.eql(u8, a.name, b.name) and std.mem.eql(u8, a.file, b.file);
        }
    };

    last: ?Frame = null,
    repeats: usize = 0,
    shown: usize = 0,

    fn add(self: *FrameRuns, frame: Frame) void {
        if (self.last) |last| if (last.eql(frame)) {
            self.repeats += 1;
            return;
        };
        self.flush();
        writeStderr("  in ");
        writeStderr(frame.name);
        writeStderr(" at ");
        writeStderr(frame.file);
        var buffer: [48]u8 = undefined;
        writeStderr(std.fmt.bufPrint(&buffer, ":{d}:{d}\n", .{ frame.line, frame.column }) catch unreachable);
        self.last = frame;
        self.shown += 1;
    }

    fn flush(self: *FrameRuns) void {
        if (self.repeats == 0) return;
        var buffer: [64]u8 = undefined;
        writeStderr(std.fmt.bufPrint(&buffer, "  ... the frame above {d} more times\n", .{self.repeats}) catch unreachable);
        self.repeats = 0;
    }
};

/// Why the frame at `address` has no source: no loaded module holds it, or
/// the module holding it has debug information the runtime cannot read.
fn unresolved(info: *std.debug.SelfInfo, io: std.Io, arena: std.mem.Allocator, address: usize, err: anyerror) void {
    var buffer: [512]u8 = undefined;
    const module = info.getModuleName(io, address) catch {
        return noBacktrace(std.fmt.bufPrint(&buffer, "no loaded module holds the frame at 0x{x}", .{address}) catch unreachable, err);
    };
    // The executable itself has no module name of its own.
    const name = if (module.len != 0) module else std.process.executablePathAlloc(io, arena) catch "the executable";
    noBacktrace(std.fmt.bufPrint(&buffer, "cannot read the debug information of {s} for the frame at 0x{x}", .{ name, address }) catch "cannot read a frame's debug information", err);
}

/// `  (no backtrace: <why>[: error.X])`.
fn noBacktrace(why: []const u8, err: ?anyerror) void {
    writeStderr("  (no backtrace: ");
    writeStderr(why);
    if (err) |e| {
        writeStderr(": error.");
        writeStderr(@errorName(e));
    }
    writeStderr(")\n");
}

/// A frame's file as the program names it. The emitted path is relative,
/// but each format adds a root of its own: lld-link roots a CodeView path
/// at a drive (`C:/t.doxa`), and a DWARF reader joins the compile unit's
/// `.` directory on (`./t.doxa`). Neither root is the program's.
fn framePath(path: []const u8) []const u8 {
    if (builtin.os.tag == .windows and path.len > 3 and path[1] == ':' and (path[2] == '/' or path[2] == '\\')) return path[3..];
    if (path.len > 2 and path[0] == '.' and (path[1] == '/' or path[1] == '\\')) return path[2..];
    return path;
}

/// Report a `kind` fault and terminate. `site` is where in the program it
/// happened, or null for a fault the runtime raises with no site of its own
/// (out of memory). `text` is the site's message (a panic's or an
/// assertion's) or the text that failed to parse; `aux0` and `aux1` are an
/// out-of-bounds index and length. The backtrace comes from `trace`.
pub fn report(kind: FaultKind, site: ?*const DoxaFaultSite, text: []const u8, aux0: i64, aux1: i64, trace: Trace) noreturn {
    var buffer: [128]u8 = undefined;
    writeStderr(kind.headline());
    if (site) |at| writeStderr(std.fmt.bufPrint(&buffer, " at {s}:{d}:{d}", .{ std.mem.span(at.file), at.line, at.column }) catch unreachable);
    switch (kind) {
        .panic, .assert_fail => if (text.len != 0) {
            writeStderr(": ");
            writeStderr(text);
        },
        .parse_int, .parse_float, .parse_byte => {
            writeStderr(": \"");
            writeStderr(text);
            writeStderr("\"");
        },
        .index_out_of_bounds => writeStderr(std.fmt.bufPrint(&buffer, ": index {d}, length {d}", .{ aux0, aux1 }) catch unreachable),
        .unreachable_code, .div_by_zero, .overflow, .pop_empty, .use_after_reclaim, .out_of_memory, .stack_overflow => {},
    }
    writeStderr("\n");
    if (backtrace_enabled) writeBacktrace(trace);
    instrument.flush();
    std.process.exit(kind.exitCode());
}

/// An allocation failed: the out-of-memory fault, from the caller's frame.
pub fn outOfMemory() noreturn {
    report(.out_of_memory, null, "", 0, 0, .{ .unwind_from = @returnAddress() });
}

/// Report the fault at `site` and terminate (`fault.report`).
pub export fn doxa_fault(site: *const DoxaFaultSite, text_ptr: ?[*]const u8, text_len: u64, aux0: i64, aux1: i64) callconv(.c) noreturn {
    const text: []const u8 = if (text_ptr) |p| p[0..@intCast(text_len)] else "";
    report(@enumFromInt(site.kind), site, text, aux0, aux1, .{ .unwind_from = @returnAddress() });
}

// ── Traps ──
//
// A fault the CPU raises, not the program's code: a stack overflow is the
// trap past the end of the stack.
//
// Windows runs the handler on the stack that ran out, where a backtrace's
// first read of the PDB does not fit: it needs megabytes of stack (between 1
// and 4 MiB, measured). So the work is split. The trapped thread unwinds its
// own stack, which needs no debug information and is the only thread that can
// (Zig's Windows unwinder stops at a frame outside the calling thread's
// stack). A thread with a stack of its own then reports, symbols and all,
// while the trapped thread waits for it.
//
// Linux and macOS run the handler on the thread's alternate signal stack
// (`std.options.signal_stack_size`), which holds a symbolized backtrace. The
// trap is a SIGSEGV or SIGBUS like any other bad access, told apart by its
// address: inside the main thread's stack or just past its end.
//
// Every other trap is a crash, not a fault: a bad access or an illegal
// instruction in an inline `zig` block, say. Its line and the Doxa frames
// that led to it print, and then the trap goes on to the system, so the
// process ends with the platform's crash status (`reportCrash`).

/// Stack kept on the program's thread for the handler: enough to unwind and
/// start the reporting thread (measured: 64 KiB, cold debug information,
/// Debug and ReleaseFast).
const windows_handler_stack: u32 = 64 * 1024;

/// The trapped thread's return addresses. Static: the trapped stack has no
/// room to spare, and a stack overflow is reported once.
var trap_addresses: [backtrace_depth]usize = undefined;

/// The reporting thread's stack.
const report_thread_stack: usize = 16 * 1024 * 1024;

/// Where the runtime owns the traps (`installHandlers`); elsewhere Zig's
/// handler reports them (`std_options.enable_segfault_handler`).
pub const owns_traps = switch (builtin.os.tag) {
    .windows, .linux, .macos => true,
    else => false,
};

/// Called once by the entry root, before the program runs, on the thread
/// that runs it.
///
/// Windows: a first-in-line vectored handler reports `EXCEPTION_STACK_OVERFLOW`
/// as the `stack_overflow` fault, and the unhandled-exception filter reports
/// every other trap no handler took as a crash. The filter, not a vectored
/// handler, so an exception some code handles itself is never reported.
///
/// Linux and macOS: records the main thread's stack (`main_stack`) and
/// handles SIGSEGV, SIGBUS, SIGILL and SIGFPE on the alternate signal stack.
///
/// Only the main thread can overflow as the `stack_overflow` fault, and that
/// is enough: Doxa code runs on the main thread alone. The only other threads
/// are `std.http`'s tasks, which never call into the program
/// (`std/http/http.zig`, "Main-thread confinement"), so an overflow there is
/// std's own bug, and ends as a crash: a POSIX thread Zig starts has its own
/// signal stack, while a Windows thread has no guarantee, so its process ends
/// with the system's `STATUS_STACK_OVERFLOW`. Running Doxa code on another
/// thread breaks this: each thread would then need its guarantee and its stack
/// recorded as it starts.
pub fn installHandlers() void {
    switch (builtin.os.tag) {
        .windows => {
            var reserve: u32 = windows_handler_stack;
            _ = win32.SetThreadStackGuarantee(&reserve);
            _ = std.os.windows.ntdll.RtlAddVectoredExceptionHandler(1, handleWindowsOverflow);
            _ = win32.SetUnhandledExceptionFilter(handleWindowsCrash);
        },
        .linux, .macos => {
            main_stack = StackRange.ofCurrentThread();
            const action: std.posix.Sigaction = .{
                .handler = .{ .sigaction = handlePosixTrap },
                .mask = std.posix.sigemptyset(),
                // Reset to the default action, so the trap that ends the
                // process is the system's.
                .flags = std.posix.SA.SIGINFO | std.posix.SA.ONSTACK | std.posix.SA.RESETHAND,
            };
            for ([_]std.posix.SIG{ .SEGV, .BUS, .ILL, .FPE }) |sig| std.posix.sigaction(sig, &action, null);
        },
        else => {},
    }
}

/// Windows' traps a crash names; anything else is left to the system alone.
fn windowsTrapName(code: u32) ?[]const u8 {
    return switch (code) {
        std.os.windows.EXCEPTION_ACCESS_VIOLATION => "Invalid memory access",
        std.os.windows.EXCEPTION_DATATYPE_MISALIGNMENT => "Misaligned memory access",
        std.os.windows.EXCEPTION_ILLEGAL_INSTRUCTION => "Illegal instruction",
        0xC0000096 => "Illegal instruction", // STATUS_PRIVILEGED_INSTRUCTION
        0xC0000094, 0xC0000095 => "Arithmetic trap", // STATUS_INTEGER_DIVIDE_BY_ZERO, _OVERFLOW
        0xC0000006 => "Invalid memory access", // STATUS_IN_PAGE_ERROR
        else => null,
    };
}

fn handleWindowsCrash(info: *std.os.windows.EXCEPTION_POINTERS) callconv(.winapi) c_long {
    const record = info.ExceptionRecord;
    if (windowsTrapName(record.ExceptionCode)) |name| {
        const address: ?usize = if (record.ExceptionCode == std.os.windows.EXCEPTION_ACCESS_VIOLATION) record.ExceptionInformation[1] else null;
        const context = std.debug.cpu_context.fromWindowsContext(info.ContextRecord);
        reportCrash(name, address, &context);
    }
    return std.os.windows.EXCEPTION_CONTINUE_SEARCH;
}

fn handlePosixTrap(sig: std.posix.SIG, info: *const std.posix.siginfo_t, raw_context: ?*anyopaque) callconv(.c) void {
    const address: usize = switch (builtin.os.tag) {
        .linux => @intFromPtr(info.fields.sigfault.addr),
        .macos => @intFromPtr(info.addr),
        else => comptime unreachable,
    };
    const native = std.debug.cpu_context.fromPosixSignalContext(raw_context);
    const context: ?std.debug.CpuContextPtr = if (native) |*c| c else null;
    if (sig == .SEGV or sig == .BUS) {
        if (main_stack) |range| if (range.overflowedAt(address)) reportStackOverflow(captureTrap(context));
    }
    const name = switch (sig) {
        .SEGV, .BUS => "Invalid memory access",
        .ILL => "Illegal instruction",
        .FPE => "Arithmetic trap",
        else => unreachable,
    };
    reportCrash(name, if (sig == .SEGV or sig == .BUS) address else null, context);
    // The handler is reset, and the signal is blocked until it returns: it
    // then ends the process, whether the trap re-runs or the signal was sent.
    std.posix.raise(sig) catch {};
}

/// The trapped thread's frames, unwound from its context.
fn captureTrap(context: ?std.debug.CpuContextPtr) std.debug.StackTrace {
    return if (backtrace_enabled and std.options.allow_stack_tracing)
        captureTrace(.{ .context = context }, &trap_addresses)
    else
        .{ .return_addresses = &.{}, .skipped = .none };
}

/// Set once a crash is being reported: a trap inside the report leaves the
/// rest to the system.
var crashing = std.atomic.Value(bool).init(false);

/// A crash's line, `<name>` and `at address 0x…` where the trap names one,
/// then the Doxa frames that led to it. The caller hands the trap on.
fn reportCrash(name: []const u8, address: ?usize, context: ?std.debug.CpuContextPtr) void {
    if (crashing.swap(true, .acq_rel)) return;
    var buffer: [48]u8 = undefined;
    writeStderr(name);
    if (address) |at| writeStderr(std.fmt.bufPrint(&buffer, " at address 0x{x}", .{at}) catch unreachable);
    writeStderr("\n");
    if (backtrace_enabled) writeBacktrace(.{ .captured = captureTrap(context) });
    instrument.flush();
}

fn handleWindowsOverflow(info: *std.os.windows.EXCEPTION_POINTERS) callconv(.winapi) c_long {
    if (info.ExceptionRecord.ExceptionCode != std.os.windows.EXCEPTION_STACK_OVERFLOW) return std.os.windows.EXCEPTION_CONTINUE_SEARCH;
    const context = std.debug.cpu_context.fromWindowsContext(info.ContextRecord);
    const trace = captureTrap(&context);
    const reporter = std.Thread.spawn(.{ .stack_size = report_thread_stack }, reportStackOverflow, .{trace}) catch {
        // No thread, so no stack to read debug information on: the fault's
        // line alone, which fits in the guarantee.
        backtrace_enabled = false;
        reportStackOverflow(trace);
    };
    reporter.join();
    unreachable; // `report` ends the process.
}

fn reportStackOverflow(trace: std.debug.StackTrace) noreturn {
    report(.stack_overflow, null, "", 0, 0, .{ .captured = trace });
}

/// How far below its lowest address a stack's overflow can land: a frame
/// larger than a page may reach past the guard before it touches memory.
/// Linux keeps this gap below a growing stack (`stack_guard_gap`).
const overflow_reach: usize = 1024 * 1024;

/// A thread's stack, `[low, high)`.
const StackRange = struct {
    low: usize,
    high: usize,

    /// The calling thread's stack as libc records it, or null where it does
    /// not say. Linux's main thread reaches as far as `RLIMIT_STACK` allows.
    fn ofCurrentThread() ?StackRange {
        const self = std.c.pthread_self();
        switch (builtin.os.tag) {
            .linux => {
                var attr: std.c.pthread_attr_t = undefined;
                if (pthread.pthread_getattr_np(self, &attr) != 0) return null;
                defer _ = std.c.pthread_attr_destroy(&attr);
                var low: usize = 0;
                var size: usize = 0;
                if (pthread.pthread_attr_getstack(&attr, &low, &size) != 0 or size == 0) return null;
                return .{ .low = low, .high = low +| size };
            },
            .macos => {
                // The address is the stack's top: it grows down from there.
                const high = pthread.pthread_get_stackaddr_np(self);
                const size = pthread.pthread_get_stacksize_np(self);
                if (size == 0 or size > high) return null;
                return .{ .low = high - size, .high = high };
            },
            else => comptime unreachable,
        }
    }

    /// Whether a bad access at `address` is this stack running out: inside
    /// it, where only growth past its limit faults, or within
    /// `overflow_reach` below it.
    fn overflowedAt(range: StackRange, address: usize) bool {
        return address < range.high and address +| overflow_reach >= range.low;
    }
};

/// libc's stack queries Zig's `std.c` does not declare.
const pthread = struct {
    extern "c" fn pthread_getattr_np(thread: std.c.pthread_t, attr: *std.c.pthread_attr_t) c_int;
    extern "c" fn pthread_attr_getstack(attr: *const std.c.pthread_attr_t, addr: *usize, size: *usize) c_int;
    extern "c" fn pthread_get_stackaddr_np(thread: std.c.pthread_t) usize;
    extern "c" fn pthread_get_stacksize_np(thread: std.c.pthread_t) usize;
};

/// The main thread's stack, recorded by `installHandlers`.
var main_stack: ?StackRange = null;

/// Set by a program built with `-g`: only then do its frames carry lines.
pub export fn doxa_enable_backtrace() callconv(.c) void {
    backtrace_enabled = true;
}
