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

    pub fn deinit(capture: Capture, gpa: Allocator) void {
        gpa.free(capture.stdout);
        gpa.free(capture.stderr);
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

    var streams: Io.File.MultiReader.Buffer(2) = undefined;
    var reader: Io.File.MultiReader = undefined;
    reader.init(gpa, io, streams.toStreams(), &.{ child.stdout.?, child.stderr.? });
    defer reader.deinit();

    const deadline = (Io.Timeout{ .duration = .{ .raw = options.timeout, .clock = .awake } }).toDeadline(io);
    var timed_out = false;
    reader.fillRemaining(deadline) catch |err| switch (err) {
        error.Timeout => {
            timed_out = true;
            tree.kill();
            // Reads still pending belong to a killed tree; keep what arrived.
            reader.batch.cancel(io);
        },
        else => |e| return e,
    };
    try reader.checkAnyError();

    const term = try child.wait(io);
    const stdout = try reader.toOwnedSlice(0);
    errdefer gpa.free(stdout);
    const stderr = try reader.toOwnedSlice(1);

    return .{
        .stdout = stdout,
        .stderr = stderr,
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
        .stdout = .pipe,
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
extern "kernel32" fn TerminateJobObject(job: windows.HANDLE, exit_code: windows.UINT) callconv(.winapi) windows.BOOL;
extern "kernel32" fn ResumeThread(thread: windows.HANDLE) callconv(.winapi) windows.DWORD;
