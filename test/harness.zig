//! What the black-box suites share: the binary under test, the outcome of a
//! case, and the report that prints outcomes.

const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const Allocator = std.mem.Allocator;

const platform = @import("platform");
const process = @import("process.zig");

/// The deadline for one `doxa` invocation. A cold cache builds the runtime
/// object before anything else, which takes seconds on its own and longer with
/// every worker doing it at once, so this sits far above any case's honest
/// cost. It exists to turn a hang into a named failure, not to measure speed.
pub const timeout = Io.Duration.fromSeconds(120);

/// The installed compiler under test, run from the repository root.
pub const Doxa = struct {
    /// Absolute path of the binary: `DOXA_BIN`, else the test install.
    exe: [:0]const u8,
    /// Absolute repository root. Every child runs here, and every compile
    /// declares it as a root so cases may spell repo-relative module paths.
    root: [:0]const u8,
    /// `--include=<root>`.
    include: []const u8,

    /// Resolves the binary and the root, which is the working directory:
    /// `build.zig` runs every test executable from the build root.
    pub fn init(gpa: Allocator, io: Io) !Doxa {
        const cwd = Io.Dir.cwd();
        const root = try cwd.realPathFileAlloc(io, ".", gpa);
        errdefer gpa.free(root);

        const exe = if (std.testing.environ.getAlloc(gpa, "DOXA_BIN")) |configured| configured: {
            defer gpa.free(configured);
            break :configured try cwd.realPathFileAlloc(io, configured, gpa);
        } else |err| switch (err) {
            error.EnvironmentVariableMissing => try cwd.realPathFileAlloc(io, "doxa/test-bin/doxa" ++ comptime builtin.target.exeFileExt(), gpa),
            else => |e| return e,
        };
        errdefer gpa.free(exe);

        return .{
            .exe = exe,
            .root = root,
            .include = try std.fmt.allocPrint(gpa, "--include={s}", .{root}),
        };
    }

    pub fn deinit(doxa: Doxa, gpa: Allocator) void {
        gpa.free(doxa.exe);
        gpa.free(doxa.root);
        gpa.free(doxa.include);
    }

    pub const Invocation = struct {
        args: []const []const u8,
        /// The repository root when null.
        cwd: ?[]const u8 = null,
        stdin: ?[]const u8 = null,
    };

    /// `doxa <args>` under the standard deadline.
    pub fn run(doxa: Doxa, gpa: Allocator, io: Io, invocation: Invocation) !process.Capture {
        const argv = try std.mem.concat(gpa, []const u8, &.{ &.{doxa.exe}, invocation.args });
        defer gpa.free(argv);
        return process.run(gpa, io, .{
            .argv = argv,
            .cwd = invocation.cwd orelse doxa.root,
            .stdin = invocation.stdin,
            .timeout = timeout,
        });
    }
};

/// A persistent directory under `.zig-cache/doxa-tests/`, held by this process
/// alone until `release`.
///
/// Compiler caches are worth keeping between runs: a cold cache rebuilds the
/// runtime object and every inline-Zig shim, and persisting one is sound
/// because objects are named by their full key, so a stale one is never
/// served. But a cache is not safe to share between concurrent compiles, whose
/// staged runtime sources and `<stem>.*` outputs are written in place. So each
/// slot carries a lock file, and a slot another run holds (a second
/// `zig build test` in the same checkout) is passed over for the next.
pub const Slot = struct {
    /// Absolute path of the directory.
    path: [:0]const u8,
    lock: Io.File,

    /// Claims the lowest-numbered `<kind>-<n>` slot that no one holds,
    /// creating it when it does not exist yet.
    pub fn claim(gpa: Allocator, io: Io, kind: []const u8) !Slot {
        var index: usize = 0;
        while (true) : (index += 1) {
            var name_buffer: [128]u8 = undefined;
            const relative = try std.fmt.bufPrint(&name_buffer, ".zig-cache/doxa-tests/{s}-{d}", .{ kind, index });
            var dir = try Io.Dir.cwd().createDirPathOpen(io, relative, .{});
            defer dir.close(io);
            const lock = try dir.createFile(io, ".lock", .{ .truncate = false });
            errdefer lock.close(io);
            if (!try lock.tryLock(io, .exclusive)) {
                lock.close(io);
                continue;
            }
            return .{ .path = try Io.Dir.cwd().realPathFileAlloc(io, relative, gpa), .lock = lock };
        }
    }

    /// Closing the lock file releases the lock.
    pub fn release(slot: Slot, gpa: Allocator, io: Io) void {
        slot.lock.close(io);
        gpa.free(slot.path);
    }
};

/// What one case concluded. A failure carries everything needed to diagnose it
/// (the mismatch, how the child ended, and what it wrote) because the report
/// is all a reader of a failed run has.
pub const Outcome = union(enum) {
    pass,
    fail: []const u8,
};

/// Collects a case's mismatches. Nothing noted means the case passed.
pub const Failure = struct {
    text: Io.Writer.Allocating,

    /// `arena` owns the text for as long as the report needs it.
    pub fn init(arena: Allocator) Failure {
        return .{ .text = .init(arena) };
    }

    pub fn note(failure: *Failure, comptime fmt: []const u8, args: anytype) !void {
        try failure.text.writer.print(fmt ++ "\n", args);
    }

    /// `.pass` when nothing was noted. Otherwise the notes, then how `capture`
    /// ended and an excerpt of each stream it wrote.
    pub fn outcome(failure: *Failure, capture: process.Capture) !Outcome {
        if (failure.text.written().len == 0) return .pass;
        const w = &failure.text.writer;
        switch (capture.end) {
            .exited => |code| try w.print("exit status: {d}\n", .{code}),
            .abnormal => |term| try w.print("ended abnormally: {any}\n", .{term}),
            .timed_out => try w.print("timed out after {d}s; process tree killed\n", .{timeout.toSeconds()}),
        }
        try excerpt(w, "stdout", capture.stdout);
        try excerpt(w, "stderr", capture.stderr);
        return .{ .fail = failure.text.written() };
    }
};

/// Writes `bytes` under `label`, indented. Long streams keep their head, where
/// compile diagnostics land, and their tail, where a runtime failure lands.
fn excerpt(w: *Io.Writer, label: []const u8, bytes: []const u8) !void {
    const kept_each_end = 20;
    const text = std.mem.trimEnd(u8, bytes, "\r\n");
    if (text.len == 0) return;

    const total = std.mem.count(u8, text, "\n") + 1;
    try w.print("{s}:\n", .{label});
    var lines = std.mem.splitScalar(u8, text, '\n');
    var index: usize = 0;
    while (lines.next()) |line| : (index += 1) {
        const elided = total > 2 * kept_each_end and index >= kept_each_end and index < total - kept_each_end;
        if (!elided) {
            try w.print("  | {s}\n", .{std.mem.trimEnd(u8, line, "\r")});
        } else if (index == kept_each_end) {
            try w.print("  | ... {d} lines elided ...\n", .{total - 2 * kept_each_end});
        }
    }
}

/// One row of a suite's report.
pub const Result = struct {
    name: []const u8,
    /// What the case exercised, printed beside a failure.
    subject: []const u8,
    outcome: Outcome,
};

/// Prints every failure in table order, then the suite's tally, and fails when
/// any case did. `DOXA_TEST_VERBOSE=1` also lists the passes.
pub fn report(gpa: Allocator, suite: []const u8, results: []const Result) error{CasesFailed}!void {
    // Failures quote program output, which is UTF-8.
    platform.enableUtf8Console();
    const verbose = verboseFromEnv(gpa);
    var failed: usize = 0;
    for (results) |result| switch (result.outcome) {
        .pass => if (verbose) std.debug.print("ok   {s}: {s}\n", .{ suite, result.name }),
        .fail => |detail| {
            failed += 1;
            std.debug.print("FAIL {s}: {s} ({s})\n", .{ suite, result.name, result.subject });
            var lines = std.mem.splitScalar(u8, std.mem.trimEnd(u8, detail, "\n"), '\n');
            while (lines.next()) |line| std.debug.print("     {s}\n", .{line});
        },
    };
    std.debug.print("{s}: {d} passed, {d} failed\n", .{ suite, results.len - failed, failed });
    if (failed != 0) return error.CasesFailed;
}

/// `DOXA_TEST_VERBOSE` set to anything but empty or "0".
fn verboseFromEnv(gpa: Allocator) bool {
    const raw = std.testing.environ.getAlloc(gpa, "DOXA_TEST_VERBOSE") catch return false;
    defer gpa.free(raw);
    return raw.len > 0 and !std.mem.eql(u8, raw, "0");
}
