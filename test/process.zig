//! Every subprocess the black-box suites start goes through `run`, which owns
//! the guarantees concurrent suites depend on:
//!
//! - **Spawns are serialized; runs are not.** On Windows, Zig creates a
//!   child's pipe ends inheritable and spawns with `bInheritHandles` and no
//!   handle list, so a child spawned while another spawn is in flight inherits
//!   that sibling's pipe ends and holds its output open. Only the spawn call
//!   itself is locked: a child's inheritable ends exist only inside it.
//! - **The suite's own standard handles are sealed** before its first spawn, so
//!   no descendant inherits, and holds open, the pipe the suite's caller reads.
//! - **A child's process tree is one unit.** On Windows the child starts
//!   suspended inside a kill-on-close job object, so every descendant belongs
//!   to the job from its first instruction and none outlives the run. On POSIX
//!   the child leads its own process group.
//! - **Every run has a deadline.** A tree still running at the deadline is
//!   killed, and the run returns what it had written so far as `.timed_out`.
//!
//! Only test executables run outside the build runner's protocol (the program
//! suites) may spawn; `build.zig`'s wiring check keeps this file out of the
//! protocol roots, whose results are cached on their own executable.

const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const Child = std.process.Child;

const native_os = builtin.os.tag;

pub const Options = struct {
    argv: []const []const u8,
    /// Working directory; inherited when null.
    cwd: ?[]const u8 = null,
    /// Written whole, then closed, before any output is read, so it must fit
    /// the pipe buffer. Stdin is the null device when absent.
    stdin: ?[]const u8 = null,
    /// Hands the child this file as its stdout instead of a pipe, for probes of
    /// how a program writes to a redirected, seekable stdout. `Capture.stdout`
    /// is then empty; the output is in the file.
    stdout_file: ?Io.File = null,
    /// How long the whole tree may run before it is killed.
    timeout: Io.Duration,
};

/// How a run ended. Every variant but `.exited` is a failure in its own right.
pub const End = union(enum) {
    exited: u8,
    /// Ended without an exit status: a signal, or a termination the OS could
    /// not describe.
    abnormal: Child.Term,
    /// Still running at the deadline; its whole process tree was killed.
    timed_out,
};

pub const Capture = struct {
    stdout: []u8,
    stderr: []u8,
    end: End,
    /// Empty unless `.timed_out`: the processes still running at the deadline,
    /// one per line as `<pid> <command line>`, taken just before the kill. A
    /// timeout is a hang, and this names where it is.
    stuck: []u8,

    pub fn deinit(capture: Capture, gpa: Allocator) void {
        gpa.free(capture.stdout);
        gpa.free(capture.stderr);
        gpa.free(capture.stuck);
    }

    /// Exited with status 0.
    pub fn succeeded(capture: Capture) bool {
        return switch (capture.end) {
            .exited => |status| status == 0,
            .abnormal, .timed_out => false,
        };
    }
};

/// Runs `options.argv` to completion or to its deadline and captures both
/// output streams. Output and stderr are owned by `gpa`.
pub fn run(gpa: Allocator, io: Io, options: Options) !Capture {
    var child = try spawn(io, options);
    defer child.kill(io);
    const tree = try Tree.adopt(&child);
    defer tree.close();

    if (options.stdin) |data| {
        var buffer: [1024]u8 = undefined;
        var writer = child.stdin.?.writer(io, &buffer);
        try writer.interface.writeAll(data);
        try writer.interface.flush();
        child.stdin.?.close(io);
        child.stdin = null;
    }

    // Stream 0 is stdout and stream 1 stderr; a stdout handed over as a file
    // leaves only stderr to read, at index 0.
    const piped_stdout = child.stdout != null;
    const files: [2]Io.File = if (piped_stdout) .{ child.stdout.?, child.stderr.? } else .{ child.stderr.?, undefined };
    const stream_count: u32 = if (piped_stdout) 2 else 1;
    var streams: Io.File.MultiReader.Buffer(2) = undefined;
    const stream_set = streams.toStreams();
    stream_set.len = stream_count;
    var reader: Io.File.MultiReader = undefined;
    reader.init(gpa, io, stream_set, files[0..stream_count]);
    defer reader.deinit();

    const deadline = (Io.Timeout{ .duration = .{ .raw = options.timeout, .clock = .awake } }).toDeadline(io);
    var timed_out = false;
    var stuck: []u8 = &.{};
    errdefer gpa.free(stuck);
    reader.fillRemaining(deadline) catch |err| switch (err) {
        error.Timeout => {
            timed_out = true;
            stuck = tree.list(gpa, io) catch |list_err|
                try std.fmt.allocPrint(gpa, "(could not list the tree: {s})\n", .{@errorName(list_err)});
            tree.kill();
            // Reads still pending belong to a killed tree; keep what arrived.
            reader.batch.cancel(io);
        },
        else => |e| return e,
    };
    try reader.checkAnyError();

    const term = try child.wait(io);
    const stdout = if (piped_stdout) try reader.toOwnedSlice(0) else try gpa.alloc(u8, 0);
    errdefer gpa.free(stdout);
    const stderr = try reader.toOwnedSlice(if (piped_stdout) 1 else 0);

    return .{
        .stdout = stdout,
        .stderr = stderr,
        .stuck = stuck,
        .end = if (timed_out) .timed_out else switch (term) {
            .exited => |code| .{ .exited = code },
            else => .{ .abnormal = term },
        },
    };
}

var spawn_lock: Io.Mutex = .init;
var sealed = false;

/// Spawns under the lock. The child comes back not yet running anything of its
/// own on Windows (suspended) and leading a fresh group on POSIX, ready for
/// `Tree.adopt`.
fn spawn(io: Io, options: Options) !Child {
    spawn_lock.lockUncancelable(io);
    defer spawn_lock.unlock(io);

    if (!sealed) {
        sealStdHandles();
        sealed = true;
    }

    return std.process.spawn(io, .{
        .argv = options.argv,
        .cwd = if (options.cwd) |dir| .{ .path = dir } else .inherit,
        .stdin = if (options.stdin != null) .pipe else .ignore,
        .stdout = if (options.stdout_file) |file| .{ .file = file } else .pipe,
        .stderr = .pipe,
        .start_suspended = native_os == .windows,
        .pgid = if (native_os == .windows) null else 0,
    });
}

const Tree = switch (native_os) {
    .windows => WindowsJob,
    else => ProcessGroup,
};

/// A kill-on-close job object. The child is assigned while still suspended,
/// so the job holds every process the child will ever start.
const WindowsJob = struct {
    job: windows.HANDLE,

    fn adopt(child: *Child) !WindowsJob {
        const job = CreateJobObjectW(null, null) orelse return error.CreateJobObjectFailed;
        errdefer windows.CloseHandle(job);
        var limits = std.mem.zeroes(JOBOBJECT_EXTENDED_LIMIT_INFORMATION);
        limits.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
        if (SetInformationJobObject(job, JobObjectExtendedLimitInformation, &limits, @sizeOf(JOBOBJECT_EXTENDED_LIMIT_INFORMATION)) == .FALSE)
            return error.SetInformationJobObjectFailed;
        if (AssignProcessToJobObject(job, child.id.?) == .FALSE) return error.AssignProcessToJobObjectFailed;
        if (ResumeThread(child.thread_handle) == std.math.maxInt(windows.DWORD)) return error.ResumeThreadFailed;
        return .{ .job = job };
    }

    fn kill(tree: WindowsJob) void {
        _ = TerminateJobObject(tree.job, 1);
    }

    /// Every process in the job with its command line.
    fn list(tree: WindowsJob, gpa: Allocator, io: Io) ![]u8 {
        _ = io;
        var ids: JOBOBJECT_BASIC_PROCESS_ID_LIST = undefined;
        if (QueryInformationJobObject(tree.job, JobObjectBasicProcessIdList, &ids, @sizeOf(JOBOBJECT_BASIC_PROCESS_ID_LIST), null) == .FALSE)
            return error.QueryInformationJobObjectFailed;

        var text: Io.Writer.Allocating = .init(gpa);
        errdefer text.deinit();
        for (ids.ProcessIdList[0..ids.NumberOfProcessIdsInList]) |pid| {
            try text.writer.print("{d} ", .{pid});
            try writeCommandLine(gpa, &text.writer, @intCast(pid));
            try text.writer.writeByte('\n');
        }
        if (ids.NumberOfAssignedProcesses > ids.NumberOfProcessIdsInList)
            try text.writer.print("({d} more not listed)\n", .{ids.NumberOfAssignedProcesses - ids.NumberOfProcessIdsInList});
        return text.toOwnedSlice();
    }

    fn writeCommandLine(gpa: Allocator, w: *Io.Writer, pid: windows.DWORD) !void {
        const process_handle = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, .FALSE, pid) orelse
            return w.writeAll("(exited)");
        defer windows.CloseHandle(process_handle);
        // A UNICODE_STRING header followed by the text it points at.
        var buffer: [32 * 1024]u8 align(@alignOf(windows.UNICODE_STRING)) = undefined;
        const status = windows.ntdll.NtQueryInformationProcess(process_handle, .CommandLineInformation, &buffer, buffer.len, null);
        if (status != .SUCCESS) return w.print("(command line unavailable: {t})", .{status});
        const line: *const windows.UNICODE_STRING = @ptrCast(&buffer);
        const units = line.Buffer.?[0 .. line.Length / 2];
        const utf8 = try std.unicode.utf16LeToUtf8Alloc(gpa, units);
        defer gpa.free(utf8);
        try w.writeAll(utf8);
    }

    /// Closing the last handle to a kill-on-close job ends every process still
    /// in it.
    fn close(tree: WindowsJob) void {
        windows.CloseHandle(tree.job);
    }
};

/// The child leads a new process group, which its descendants join.
const ProcessGroup = struct {
    leader: std.posix.pid_t,

    fn adopt(child: *Child) !ProcessGroup {
        return .{ .leader = child.id.? };
    }

    /// Only called before the leader is reaped, so the group id cannot have
    /// been reused.
    fn kill(tree: ProcessGroup) void {
        std.posix.kill(-tree.leader, .KILL) catch {};
    }

    /// Every process in the group with its command line, from `ps`: the one
    /// listing Linux and macOS share.
    fn list(tree: ProcessGroup, gpa: Allocator, io: Io) ![]u8 {
        const result = blk: {
            spawn_lock.lockUncancelable(io);
            defer spawn_lock.unlock(io);
            break :blk try std.process.run(gpa, io, .{ .argv = &.{ "ps", "-A", "-o", "pgid=,pid=,args=" } });
        };
        defer gpa.free(result.stdout);
        defer gpa.free(result.stderr);

        var text: Io.Writer.Allocating = .init(gpa);
        errdefer text.deinit();
        var lines = std.mem.splitScalar(u8, result.stdout, '\n');
        while (lines.next()) |line| {
            var fields = std.mem.tokenizeScalar(u8, line, ' ');
            const pgid = fields.next() orelse continue;
            if ((std.fmt.parseInt(std.posix.pid_t, pgid, 10) catch continue) != tree.leader) continue;
            try text.writer.print("{s}\n", .{std.mem.trim(u8, fields.rest(), " ")});
        }
        return text.toOwnedSlice();
    }

    fn close(tree: ProcessGroup) void {
        _ = tree;
    }
};

/// Clear `HANDLE_FLAG_INHERIT` on this process's standard handles, which are
/// whatever the suite's caller handed it: a console, or the pipes a build
/// runner or CI log is reading to EOF. A no-op off Windows, where a child's
/// standard descriptors are replaced rather than inherited alongside its own.
fn sealStdHandles() void {
    if (native_os != .windows) return;
    for ([_]windows.DWORD{ STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE }) |which| {
        const handle = GetStdHandle(which) orelse continue;
        if (handle == windows.INVALID_HANDLE_VALUE) continue;
        _ = SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
    }
}

const windows = std.os.windows;

const STD_INPUT_HANDLE: windows.DWORD = @bitCast(@as(i32, -10));
const STD_OUTPUT_HANDLE: windows.DWORD = @bitCast(@as(i32, -11));
const STD_ERROR_HANDLE: windows.DWORD = @bitCast(@as(i32, -12));
const HANDLE_FLAG_INHERIT: windows.DWORD = 0x00000001;
const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE: windows.DWORD = 0x00002000;
const JobObjectExtendedLimitInformation: c_int = 9;
const JobObjectBasicProcessIdList: c_int = 3;
const PROCESS_QUERY_LIMITED_INFORMATION: windows.DWORD = 0x1000;

/// Room for more processes than a doxa compile ever starts at once.
const JOBOBJECT_BASIC_PROCESS_ID_LIST = extern struct {
    NumberOfAssignedProcesses: windows.DWORD,
    NumberOfProcessIdsInList: windows.DWORD,
    ProcessIdList: [64]usize,
};

const JOBOBJECT_BASIC_LIMIT_INFORMATION = extern struct {
    PerProcessUserTimeLimit: windows.LARGE_INTEGER,
    PerJobUserTimeLimit: windows.LARGE_INTEGER,
    LimitFlags: windows.DWORD,
    MinimumWorkingSetSize: usize,
    MaximumWorkingSetSize: usize,
    ActiveProcessLimit: windows.DWORD,
    Affinity: usize,
    PriorityClass: windows.DWORD,
    SchedulingClass: windows.DWORD,
};

const IO_COUNTERS = extern struct {
    ReadOperationCount: u64,
    WriteOperationCount: u64,
    OtherOperationCount: u64,
    ReadTransferCount: u64,
    WriteTransferCount: u64,
    OtherTransferCount: u64,
};

const JOBOBJECT_EXTENDED_LIMIT_INFORMATION = extern struct {
    BasicLimitInformation: JOBOBJECT_BASIC_LIMIT_INFORMATION,
    IoInfo: IO_COUNTERS,
    ProcessMemoryLimit: usize,
    JobMemoryLimit: usize,
    PeakProcessMemoryUsed: usize,
    PeakJobMemoryUsed: usize,
};

extern "kernel32" fn GetStdHandle(n_std_handle: windows.DWORD) callconv(.winapi) ?windows.HANDLE;
extern "kernel32" fn SetHandleInformation(h_object: windows.HANDLE, mask: windows.DWORD, flags: windows.DWORD) callconv(.winapi) windows.BOOL;
extern "kernel32" fn CreateJobObjectW(attributes: ?*anyopaque, name: ?[*:0]const u16) callconv(.winapi) ?windows.HANDLE;
extern "kernel32" fn SetInformationJobObject(job: windows.HANDLE, class: c_int, info: *const anyopaque, length: windows.DWORD) callconv(.winapi) windows.BOOL;
extern "kernel32" fn AssignProcessToJobObject(job: windows.HANDLE, process: windows.HANDLE) callconv(.winapi) windows.BOOL;
extern "kernel32" fn QueryInformationJobObject(job: windows.HANDLE, class: c_int, info: *anyopaque, length: windows.DWORD, return_length: ?*windows.DWORD) callconv(.winapi) windows.BOOL;
extern "kernel32" fn OpenProcess(access: windows.DWORD, inherit: windows.BOOL, pid: windows.DWORD) callconv(.winapi) ?windows.HANDLE;
extern "kernel32" fn TerminateJobObject(job: windows.HANDLE, exit_code: windows.UINT) callconv(.winapi) windows.BOOL;
extern "kernel32" fn ResumeThread(thread: windows.HANDLE) callconv(.winapi) windows.DWORD;
