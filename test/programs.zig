//! The case table (`cases.zig`) through each pipeline:
//!
//! - **run**: one `doxa run` per case; the compile and the program are judged
//!   together.
//! - **compile**: one `doxa compile` per distinct program, then one run of the
//!   binary per case. A `.reject` case is judged on its compile.
//!
//! Cases run concurrently across a crew of workers and never abort the suite:
//! whatever happens to a case, a harness error included, becomes its outcome,
//! and the report lists every failure in table order.

const std = @import("std");
const builtin = @import("builtin");
const Io = std.Io;
const Allocator = std.mem.Allocator;

const constants = @import("constants");
const cases = @import("cases.zig");
const harness = @import("harness.zig");
const process = @import("process.zig");

const Case = cases.Case;
const Outcome = harness.Outcome;

/// Runs every case on `pipeline` and reports the results; fails when any case
/// did.
pub fn check(gpa: Allocator, pipeline: cases.Pipeline) !void {
    const io = std.testing.io;
    const doxa: harness.Doxa = try .init(gpa, io);
    defer doxa.deinit(gpa);

    // Some cases download into `test/out`, which only the compile pipeline
    // otherwise creates. A fresh checkout running the run pipeline first would
    // have no such directory, so the downloads fail. Make it exist here.
    std.Io.Dir.cwd().createDirPath(io, "test/out") catch {};

    var selected: std.ArrayList(Case) = .empty;
    defer selected.deinit(gpa);
    var programs: std.ArrayList(usize) = .empty;
    defer programs.deinit(gpa);
    for (cases.cases, cases.program_of) |case, program| {
        if (!case.runsOn(pipeline)) continue;
        try selected.append(gpa, case);
        try programs.append(gpa, program);
    }

    var crew: Crew = try .init(gpa, io);
    defer crew.deinit(gpa, io);

    const results = try gpa.alloc(harness.Result, selected.items.len);
    defer gpa.free(results);
    for (results, selected.items) |*result, case| {
        result.* = .{ .name = case.name, .subject = case.path, .outcome = undefined };
    }

    const suite: Suite = .{
        .gpa = gpa,
        .io = io,
        .doxa = &doxa,
        .cases = selected.items,
        .programs = programs.items,
        .results = results,
    };
    switch (pipeline) {
        .run => try crew.each(io, programs.items, &suite, Suite.viaRun),
        .compile => {
            const builds = try suite.planBuilds();
            defer {
                for (builds) |build| if (build.capture) |capture| capture.deinit(gpa);
                gpa.free(builds);
            }
            const build_programs = try gpa.alloc(usize, builds.len);
            defer gpa.free(build_programs);
            for (build_programs, builds) |*program, build| program.* = build.program;

            const compiled: Compiled = .{ .suite = &suite, .builds = builds };
            try crew.each(io, build_programs, &compiled, Compiled.build);
            try crew.each(io, programs.items, &compiled, Compiled.execute);
        },
    }
    try harness.report(gpa, @tagName(pipeline), results);
}

const Suite = struct {
    gpa: Allocator,
    io: Io,
    doxa: *const harness.Doxa,
    cases: []const Case,
    /// Parallel to `cases`: each case's program ordinal (`cases.program_of`).
    programs: []const usize,
    /// Parallel to `cases`; each slot is written by exactly one worker.
    results: []harness.Result,

    fn viaRun(suite: *const Suite, worker: *Worker, index: usize) void {
        suite.settle(worker, index, suite.runCase(worker, suite.cases[index]));
    }

    fn runCase(suite: *const Suite, worker: *Worker, case: Case) !Outcome {
        const arena = worker.arena.allocator();
        var args: std.ArrayList([]const u8) = .empty;
        try args.appendSlice(arena, &.{ "run", case.path, suite.doxa.include, worker.cache_flag });
        if (case.extra_args.len != 0) {
            try args.append(arena, "--");
            try args.appendSlice(arena, case.extra_args);
        }
        const capture = try suite.doxa.run(suite.gpa, suite.io, .{ .args = args.items, .stdin = case.input });
        defer capture.deinit(suite.gpa);
        return judge(arena, case, capture);
    }

    /// Records `outcome`, or the error that kept the case from producing one.
    fn settle(suite: *const Suite, worker: *Worker, index: usize, outcome: anyerror!Outcome) void {
        suite.results[index].outcome = outcome catch |err| .{
            .fail = std.fmt.allocPrint(worker.arena.allocator(), "the harness failed: {t}", .{err}) catch @errorName(err),
        };
    }

    /// One build per distinct program, in table order, so each build's owner
    /// is the first case that uses it.
    fn planBuilds(suite: *const Suite) ![]Build {
        var builds: std.ArrayList(Build) = .empty;
        errdefer builds.deinit(suite.gpa);
        for (suite.cases, suite.programs, 0..) |case, program, index| {
            for (builds.items) |build| {
                if (build.program == program) break;
            } else try builds.append(suite.gpa, .{ .path = case.path, .program = program, .owner = index });
        }
        return builds.toOwnedSlice(suite.gpa);
    }
};

/// A program's `doxa compile`, shared by every case that runs it.
const Build = struct {
    path: []const u8,
    /// The program's ordinal (`cases.program_of`).
    program: usize,
    /// Index of the first case using this program; a failed build is reported
    /// in full there and only referenced elsewhere.
    owner: usize,
    /// Set by the build phase. Null only when the harness itself failed.
    capture: ?process.Capture = null,
    harness_error: ?anyerror = null,

    fn succeeded(build: Build) bool {
        const capture = build.capture orelse return false;
        return capture.succeeded();
    }
};

const Compiled = struct {
    suite: *const Suite,
    /// Each slot is written by exactly one worker in the build phase and only
    /// read in the execute phase.
    builds: []Build,

    fn build(compiled: *const Compiled, worker: *Worker, index: usize) void {
        const suite = compiled.suite;
        const target = &compiled.builds[index];
        const out = cases.outputPathFor(worker.arena.allocator(), target.path) catch |err| {
            target.harness_error = err;
            return;
        };
        target.capture = suite.doxa.run(suite.gpa, suite.io, .{ .args = &.{
            "compile", target.path, "-o", out, suite.doxa.include, worker.cache_flag,
        } }) catch |err| {
            target.harness_error = err;
            return;
        };
    }

    fn execute(compiled: *const Compiled, worker: *Worker, index: usize) void {
        compiled.suite.settle(worker, index, compiled.executeCase(worker, index));
    }

    fn executeCase(compiled: *const Compiled, worker: *Worker, index: usize) !Outcome {
        const suite = compiled.suite;
        const arena = worker.arena.allocator();
        const case = suite.cases[index];
        const target = for (compiled.builds) |candidate| {
            if (candidate.program == suite.programs[index]) break candidate;
        } else unreachable;

        if (target.harness_error) |err| return err;
        if (case.mode == .reject) return judge(arena, case, target.capture.?);
        if (!target.succeeded()) {
            if (target.owner != index) {
                return .{ .fail = try std.fmt.allocPrint(arena, "did not compile; see '{s}'", .{suite.cases[target.owner].name}) };
            }
            var failure: harness.Failure = .init(arena);
            try failure.note("did not compile", .{});
            return failure.outcome(target.capture.?);
        }

        const out = try cases.outputPathFor(arena, case.path);
        const binary = try std.fmt.allocPrint(arena, "{s}{c}{s}{s}", .{
            suite.doxa.root, std.fs.path.sep, out, comptime builtin.target.exeFileExt(),
        });
        const argv = try std.mem.concat(arena, []const u8, &.{ &.{binary}, case.extra_args });
        const capture = try process.run(suite.gpa, suite.io, .{
            .argv = argv,
            .cwd = suite.doxa.root,
            .stdin = case.input,
            .timeout = harness.timeout,
        });
        defer capture.deinit(suite.gpa);
        return judge(arena, case, capture);
    }
};

/// The workers a pipeline runs on, one per CPU. Each owns an arena for its
/// outcome text and a compiler cache of its own (a `harness.Slot`), and takes
/// its programs by ordinal, so every run lands a program on the cache that
/// already holds its objects and inline-Zig shims.
const Crew = struct {
    workers: []Worker,

    fn init(gpa: Allocator, io: Io) !Crew {
        const workers = try gpa.alloc(Worker, try std.Thread.getCpuCount());
        var ready: usize = 0;
        errdefer {
            for (workers[0..ready]) |*worker| worker.deinit(gpa, io);
            gpa.free(workers);
        }
        for (workers) |*worker| {
            try worker.init(gpa, io);
            ready += 1;
        }
        return .{ .workers = workers };
    }

    fn deinit(crew: *Crew, gpa: Allocator, io: Io) void {
        for (crew.workers) |*worker| worker.deinit(gpa, io);
        gpa.free(crew.workers);
    }

    /// Calls `job(context, worker, index)` for every index of `programs`, on
    /// the worker that owns `programs[index]`, and returns when all are done.
    fn each(
        crew: *Crew,
        io: Io,
        programs: []const usize,
        context: anytype,
        comptime job: fn (@TypeOf(context), *Worker, usize) void,
    ) !void {
        const Share = struct {
            fn run(owned: []const usize, crew_size: usize, me: usize, ctx: @TypeOf(context), worker: *Worker) void {
                for (owned, 0..) |program, index| {
                    if (program % crew_size == me) job(ctx, worker, index);
                }
            }
        };
        var group: Io.Group = .init;
        defer group.cancel(io);
        for (crew.workers, 0..) |*worker, me| {
            try group.concurrent(io, Share.run, .{ programs, crew.workers.len, me, context, worker });
        }
        try group.await(io);
    }
};

const Worker = struct {
    arena: std.heap.ArenaAllocator,
    cache: harness.Slot,
    /// `--cache-dir=<cache.path>`, allocated from `arena`.
    cache_flag: []const u8,

    /// Initializes in place: an arena records its allocations in itself, so it
    /// must not be copied once it has made one.
    fn init(worker: *Worker, gpa: Allocator, io: Io) !void {
        worker.cache = try .claim(gpa, io, "worker");
        errdefer worker.cache.release(gpa, io);
        worker.arena = .init(gpa);
        errdefer worker.arena.deinit();
        worker.cache_flag = try std.fmt.allocPrint(worker.arena.allocator(), "--cache-dir={s}", .{worker.cache.path});
    }

    fn deinit(worker: *Worker, gpa: Allocator, io: Io) void {
        worker.arena.deinit();
        worker.cache.release(gpa, io);
    }
};

// ---------------------------------------------------------------------------
// Judging
// ---------------------------------------------------------------------------

fn judge(arena: Allocator, case: Case, capture: process.Capture) !Outcome {
    var failure: harness.Failure = .init(arena);
    switch (capture.end) {
        .exited => |status| switch (case.mode) {
            .print => try judgePrint(&failure, case, status, capture.stdout),
            .peek => try judgePeek(&failure, case, status, capture.stderr),
            .terminate => try judgeTerminate(&failure, case, status, capture.stderr),
            .reject => try judgeReject(&failure, case, status, capture.stderr),
        },
        .abnormal, .timed_out => try failure.note("did not run to completion", .{}),
    }
    return failure.outcome(capture);
}

/// The most mismatches a failure lists before summarizing the rest.
const listed_mismatches = 10;

fn judgePrint(failure: *harness.Failure, case: Case, status: u8, stdout: []const u8) !void {
    if (status != 0) return failure.note("expected exit status 0", .{});

    const expected = case.expected_print.?;
    var lines = outputLines(stdout);
    var mismatched: usize = 0;
    var index: usize = 0;
    while (lines.next()) |line| : (index += 1) {
        if (index >= expected.len) {
            mismatched += 1;
            if (mismatched <= listed_mismatches) try failure.note("line {d}: unexpected `{s}`", .{ index + 1, line });
        } else if (!std.mem.eql(u8, line, expected[index].value)) {
            mismatched += 1;
            if (mismatched <= listed_mismatches) try failure.note("line {d}: expected `{s}`, found `{s}`", .{ index + 1, expected[index].value, line });
        }
    }
    for (expected[@min(index, expected.len)..], @min(index, expected.len)..) |missing, at| {
        mismatched += 1;
        if (mismatched <= listed_mismatches) try failure.note("line {d}: missing `{s}`", .{ at + 1, missing.value });
    }
    if (mismatched > listed_mismatches) try failure.note("... {d} more mismatched lines", .{mismatched - listed_mismatches});
}

fn judgePeek(failure: *harness.Failure, case: Case, status: u8, stderr: []const u8) !void {
    if (status != 0) return failure.note("expected exit status 0", .{});

    const expected = case.expected_peek.?;
    var mismatched: usize = 0;
    var row_index: usize = 0;
    var in_diagnostic = false;
    var lines = outputLines(stderr);
    while (lines.next()) |line| {
        if (line.len != 0 and line[0] == ' ' and in_diagnostic) continue;
        in_diagnostic = false;
        if (Header.parse(line)) |header| {
            if (std.mem.eql(u8, header.severity, "Error")) {
                mismatched += 1;
                if (mismatched <= listed_mismatches) try failure.note("stderr: unexpected error `{s}`", .{line});
            }
            in_diagnostic = true;
            continue;
        }
        const row = PeekRow.parse(line) orelse {
            mismatched += 1;
            if (mismatched <= listed_mismatches) try failure.note("stderr: unexpected line `{s}`", .{line});
            continue;
        };
        defer row_index += 1;
        if (row_index >= expected.len) {
            mismatched += 1;
            if (mismatched <= listed_mismatches) try failure.note("peek {d}: unexpected `{s} is {s}`", .{ row_index + 1, row.type, row.value });
        } else if (!std.mem.eql(u8, row.type, expected[row_index].type) or !std.mem.eql(u8, row.value, expected[row_index].value)) {
            mismatched += 1;
            if (mismatched <= listed_mismatches) try failure.note("peek {d}: expected `{s} is {s}`, found `{s} is {s}`", .{
                row_index + 1, expected[row_index].type, expected[row_index].value, row.type, row.value,
            });
        }
    }
    for (expected[@min(row_index, expected.len)..], @min(row_index, expected.len)..) |missing, at| {
        mismatched += 1;
        if (mismatched <= listed_mismatches) try failure.note("peek {d}: missing `{s} is {s}`", .{ at + 1, missing.type, missing.value });
    }
    if (mismatched > listed_mismatches) try failure.note("... {d} more mismatches", .{mismatched - listed_mismatches});
}

fn judgeTerminate(failure: *harness.Failure, case: Case, status: u8, stderr: []const u8) !void {
    var diagnostics = Diagnostics.init(stderr);
    while (diagnostics.next()) |diagnostic| {
        if (diagnostic.header.isCompileError()) return failure.note("did not compile", .{});
    }
    if (status != case.expect_code.?) try failure.note("expected exit status {d}", .{case.expect_code.?});
    if (case.expect_stderr) |needle| {
        if (std.mem.indexOf(u8, stderr, needle) == null) try failure.note("stderr lacks `{s}`", .{needle});
    }
}

fn judgeReject(failure: *harness.Failure, case: Case, status: u8, stderr: []const u8) !void {
    if (status != constants.EXIT_CODE_USAGE) try failure.note("expected the compile-error exit status {d}", .{constants.EXIT_CODE_USAGE});
    var diagnostics = Diagnostics.init(stderr);
    while (diagnostics.next()) |diagnostic| {
        const header = diagnostic.header;
        if (!header.isCompileError()) continue;
        if (!std.mem.eql(u8, header.code orelse continue, case.expect_error.?)) continue;
        if (std.mem.indexOf(u8, diagnostic.text, case.expect_stderr.?) != null) return;
    }
    try failure.note("no compile error {s} reads `{s}`", .{ case.expect_error.?, case.expect_stderr.? });
}

/// The lines of a captured stream, without their terminators. A final newline
/// ends the last line rather than starting an empty one.
fn outputLines(bytes: []const u8) LineIterator {
    const body = if (std.mem.endsWith(u8, bytes, "\n")) bytes[0 .. bytes.len - 1] else bytes;
    return .{ .inner = std.mem.splitScalar(u8, body, '\n'), .empty = bytes.len == 0 };
}

const LineIterator = struct {
    inner: std.mem.SplitIterator(u8, .scalar),
    empty: bool,

    fn next(it: *LineIterator) ?[]const u8 {
        if (it.empty) return null;
        const line = it.inner.next() orelse return null;
        return std.mem.trimEnd(u8, line, "\r");
    }
};

/// A rendered diagnostic's first line (`src/utils/source_render.zig`):
/// `Doxa: [<Phase>][<Severity>]` and, when the diagnostic has one, `[<Code>]`.
const Header = struct {
    phase: []const u8,
    severity: []const u8,
    code: ?[]const u8,

    fn parse(line: []const u8) ?Header {
        const prefix = "Doxa: ";
        if (!std.mem.startsWith(u8, line, prefix)) return null;
        var rest = line[prefix.len..];
        const phase = bracketed(&rest) orelse return null;
        const severity = bracketed(&rest) orelse return null;
        return .{ .phase = phase, .severity = severity, .code = bracketed(&rest) };
    }

    fn isCompileError(header: Header) bool {
        return std.mem.eql(u8, header.phase, "CompileTime") and std.mem.eql(u8, header.severity, "Error");
    }

    fn bracketed(rest: *[]const u8) ?[]const u8 {
        if (rest.len == 0 or rest.*[0] != '[') return null;
        const close = std.mem.indexOfScalar(u8, rest.*, ']') orelse return null;
        defer rest.* = rest.*[close + 1 ..];
        return rest.*[1..close];
    }
};

/// The diagnostics in a stream: each header line together with the indented
/// lines that follow it (a multi-line message, the source snippet, notes).
const Diagnostics = struct {
    lines: LineIterator,
    bytes: []const u8,
    pending: ?Header = null,
    pending_start: usize = 0,

    const Diagnostic = struct {
        header: Header,
        /// The header and its indented continuation lines.
        text: []const u8,
    };

    fn init(bytes: []const u8) Diagnostics {
        return .{ .lines = outputLines(bytes), .bytes = bytes };
    }

    fn next(it: *Diagnostics) ?Diagnostic {
        while (it.lines.next()) |line| {
            const start = @intFromPtr(line.ptr) - @intFromPtr(it.bytes.ptr);
            if (line.len != 0 and line[0] == ' ') continue;
            const finished = it.take(start);
            if (Header.parse(line)) |header| {
                it.pending = header;
                it.pending_start = start;
            }
            if (finished) |diagnostic| return diagnostic;
        }
        return it.take(it.bytes.len);
    }

    /// Closes the pending diagnostic at byte `end`, if one is open.
    fn take(it: *Diagnostics, end: usize) ?Diagnostic {
        const header = it.pending orelse return null;
        it.pending = null;
        return .{ .header = header, .text = it.bytes[it.pending_start..end] };
    }
};

/// One `@peek` line: `[<location>] <name> :: <type> is <value>`, where the
/// name is absent for a peeked expression.
const PeekRow = struct {
    type: []const u8,
    value: []const u8,

    fn parse(line: []const u8) ?PeekRow {
        if (line.len == 0 or line[0] != '[') return null;
        const close = std.mem.indexOf(u8, line, "] ") orelse return null;
        const after_name = std.mem.indexOfPos(u8, line, close, ":: ") orelse return null;
        const typed = line[after_name + ":: ".len ..];
        const is = std.mem.indexOf(u8, typed, " is ") orelse return null;
        return .{ .type = typed[0..is], .value = typed[is + " is ".len ..] };
    }
};
