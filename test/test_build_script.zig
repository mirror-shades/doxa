const std = @import("std");
const testing = std.testing;

const harness = @import("harness.zig");
const process = @import("process.zig");

// Regression cover for the std build layer reporting success while skipping the
// compile. `artifactIsUpToDate` used to compare the output's mtime against the
// entry source alone, so editing an imported module left the output looking
// newer than the entry, the build was skipped, and `execute` printed
// "build exit code: 0" and exited 0, with a stale binary in place and not even a
// compile attempt to fail. The skip is gone; the driver decides. This pins the
// observable consequence: a build whose *imported module* no longer compiles
// must exit non-zero and surface the compiler's diagnostic, and restoring the
// module must build again.

/// The fixture project. A run copies it into a `build-script` `harness.Slot`,
/// so it never edits the checkout and concurrent runs never share a tree,
/// while the caches the builds leave there stay warm for the next run.
const fixture = "test/build_script";
const fixture_files = [_][]const u8{ "build.doxa", "src/app.doxa", "src/lib.doxa" };

const broken_lib =
    \\public function greet(who :: string) returns string {
    \\    return who.this_field_does_not_exist
    \\}
    \\
;

test "build script: an imported module change is not silently skipped" {
    const gpa = testing.allocator;
    const io = testing.io;

    const doxa: harness.Doxa = try .init(gpa, io);
    defer doxa.deinit(gpa);

    const slot: harness.Slot = try .claim(gpa, io, "build-script");
    defer slot.release(gpa, io);
    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();
    const project_path = try std.fs.path.join(arena.allocator(), &.{ slot.path, "project" });

    // A fresh copy of the sources and no output: the first build is clean.
    var project = try std.Io.Dir.cwd().createDirPathOpen(io, project_path, .{});
    defer project.close(io);
    try project.deleteTree(io, "bin");
    try project.createDirPath(io, "src");
    var source = try std.Io.Dir.cwd().openDir(io, fixture, .{});
    defer source.close(io);
    for (fixture_files) |file| try source.copyFile(file, project, file, io, .{});

    var script: Script = .{
        .gpa = gpa,
        .io = io,
        .doxa = &doxa,
        .slot = slot.path,
        .dir = project,
        .path = project_path,
        .failure = .init(arena.allocator()),
    };

    try harness.report(gpa, "build script", &.{.{
        .name = "an imported module change is not silently skipped",
        .subject = fixture,
        .outcome = try script.run(),
    }});
}

/// The fixture's three builds: clean, with `src/lib.doxa` broken, and with it
/// restored. Stops at the first build that misbehaves, whose capture explains
/// the failure.
const Script = struct {
    gpa: std.mem.Allocator,
    io: std.Io,
    doxa: *const harness.Doxa,
    /// Holds the project and one cache per build.
    slot: []const u8,
    dir: std.Io.Dir,
    path: []const u8,
    failure: harness.Failure,
    builds: usize = 0,

    fn run(script: *Script) !harness.Outcome {
        // After a clean build the output is newer than `src/app.doxa`, exactly
        // the state an entry-point-only freshness check treats as current, so
        // breaking `lib.doxa` next reproduces the silent skip.
        {
            const clean = try script.build();
            defer clean.deinit(script.gpa);
            if (!clean.succeeded()) {
                try script.failure.note("the clean build failed", .{});
                return script.failure.outcome(clean);
            }
        }

        const original = try script.dir.readFileAlloc(script.io, "src/lib.doxa", script.gpa, .unlimited);
        defer script.gpa.free(original);
        try script.dir.writeFile(script.io, .{ .sub_path = "src/lib.doxa", .data = broken_lib });
        {
            const broken = try script.build();
            defer broken.deinit(script.gpa);
            if (broken.succeeded()) try script.failure.note("the build with a broken src/lib.doxa succeeded", .{});
            const diagnostic = "Cannot access field on non-struct type String";
            if (std.mem.indexOf(u8, broken.stderr, diagnostic) == null) {
                try script.failure.note("its stderr lacks `{s}`", .{diagnostic});
            }
            const outcome = try script.failure.outcome(broken);
            if (outcome == .fail) return outcome;
        }

        try script.dir.writeFile(script.io, .{ .sub_path = "src/lib.doxa", .data = original });
        const restored = try script.build();
        defer restored.deinit(script.gpa);
        if (!restored.succeeded()) try script.failure.note("the rebuild after restoring src/lib.doxa failed", .{});
        return script.failure.outcome(restored);
    }

    /// Runs the fixture's build script. Each build gets its own cache directory:
    /// `doxa run` links the script itself into `<cache>/build.exe`, and
    /// relinking a binary a previous run just executed intermittently fails on
    /// Windows with "Permission denied" while the old image is still mapped.
    /// The artifact under test, `bin/app`, is shared on purpose: it is the file
    /// whose staleness the case is about.
    fn build(script: *Script) !process.Capture {
        script.builds += 1;
        var flag_buffer: [std.fs.max_path_bytes + 32]u8 = undefined;
        const cache_flag = try std.fmt.bufPrint(&flag_buffer, "--cache-dir={s}{c}cache-{d}", .{ script.slot, std.fs.path.sep, script.builds });
        return script.doxa.run(script.gpa, script.io, .{ .args = &.{ "run", "build.doxa", cache_flag }, .cwd = script.path });
    }
};
