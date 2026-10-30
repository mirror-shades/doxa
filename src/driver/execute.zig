//! Run the native binary produced for `doxa run`, and emit the final profile.

const std = @import("std");
const builtin = @import("builtin");
const Profiler = @import("../utils/profiler.zig").Profiler;

/// `doxa run`'s own status when the program crashed: a trap no handler of the
/// program's reported, or a signal. The diagnostic names the crash; the status
/// stays a conventional failure, never the platform's crash code.
const EXIT_CODE_PROGRAM_CRASHED: u8 = 1;

// Run the program as `doxa compile`'s binary would run, and return the status
// `doxa run` exits with: the program's own status when it exited, or
// `EXIT_CODE_PROGRAM_CRASHED` after a line naming the crash when it did not.
// `program_args[0]` is the source path; skip it so the process argv[0] is the
// exe.
pub fn runExecutable(
    io: std.Io,
    allocator: std.mem.Allocator,
    environ_map: *const std.process.Environ.Map,
    exe_path: []const u8,
    program_args: []const []const u8,
) !u8 {
    var args = std.array_list.Managed([]const u8).init(allocator);
    defer args.deinit();
    try args.append(exe_path);
    if (program_args.len > 1) try args.appendSlice(program_args[1..]);

    // Point the compiled program at the real `doxa` executable so the std/build
    // library's `compileArtifact` re-invokes the compiler instead of recursing
    // into the compiled binary itself (`selfExePath` would resolve to it).
    var env = try environ_map.clone(allocator);
    defer env.deinit();
    if (std.process.executablePathAlloc(io, allocator)) |compiler_path| {
        defer allocator.free(compiler_path);
        env.put("DOXA_BIN", compiler_path) catch {};
    } else |_| {}

    var child = try std.process.spawn(io, .{
        .argv = args.items,
        .stdin = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
        .environ_map = &env,
    });
    // Zig's `wait` cuts a Windows status to its low byte and then closes the
    // handle, so the full status is read first.
    const windows_status: ?u32 = if (builtin.os.tag == .windows) windowsExitStatus(child.id.?) else null;
    const term = try child.wait(io);
    if (windows_status) |status| if (windowsCrash(status)) |name| {
        std.debug.print("doxa: the program crashed: {s} (0x{X:0>8})\n", .{ name, status });
        return EXIT_CODE_PROGRAM_CRASHED;
    };
    return switch (term) {
        .exited => |code| code,
        .signal, .stopped => |signal| {
            std.debug.print("doxa: the program was stopped by signal {s}\n", .{signalName(signal)});
            return EXIT_CODE_PROGRAM_CRASHED;
        },
        .unknown => |status| {
            std.debug.print("doxa: the program ended with an unknown status 0x{X}\n", .{status});
            return EXIT_CODE_PROGRAM_CRASHED;
        },
    };
}

/// The exited child's full 32-bit status, or null when it cannot be read.
fn windowsExitStatus(handle: std.os.windows.HANDLE) ?u32 {
    const windows = std.os.windows;
    if (windows.ntdll.NtWaitForSingleObject(handle, .FALSE, null) != windows.NTSTATUS.WAIT_0) return null;
    var info: windows.PROCESS.BASIC_INFORMATION = undefined;
    if (windows.ntdll.NtQueryInformationProcess(handle, .BasicInformation, &info, @sizeOf(windows.PROCESS.BASIC_INFORMATION), null) != .SUCCESS) return null;
    return @intFromEnum(info.ExitStatus);
}

/// What a Windows status names when the process ended on an exception it did
/// not handle, or null for a status the program chose (`@exit`). An unhandled
/// exception ends the process with its code, an NTSTATUS of error severity
/// (the top two bits set); no exit code a program passes looks like one.
fn windowsCrash(status: u32) ?[]const u8 {
    if (status >> 30 != 0b11) return null;
    const NTSTATUS = std.os.windows.NTSTATUS;
    return switch (@as(NTSTATUS, @enumFromInt(status))) {
        .ACCESS_VIOLATION => "access violation",
        .IN_PAGE_ERROR => "in-page error",
        .ILLEGAL_INSTRUCTION => "illegal instruction",
        .PRIVILEGED_INSTRUCTION => "privileged instruction",
        .INTEGER_DIVIDE_BY_ZERO => "integer division by zero",
        .INTEGER_OVERFLOW => "integer overflow",
        .STACK_OVERFLOW => "stack overflow",
        .STACK_BUFFER_OVERRUN => "fail-fast exit (stack buffer overrun or abort)",
        .HEAP_CORRUPTION => "heap corruption",
        .CONTROL_C_EXIT => "interrupted (Ctrl+C)",
        else => "unhandled exception",
    };
}

fn signalName(signal: std.posix.SIG) []const u8 {
    return std.enums.tagName(std.posix.SIG, signal) orelse "(unnamed)";
}

// Emit the human-readable profile to stderr and, when requested, the JSON
// report to disk. Called on every successful compile (and before a run's
// non-zero exit is propagated).
pub fn finishProfile(profiler: *Profiler, profile_out: ?[]const u8) !void {
    try profiler.dump();
    if (profile_out) |path| try profiler.writeJsonReport(path);
}
