const std = @import("std");
const platform = @import("platform");

const harness = @import("harness.zig");

const test_results = harness.Counts;

/// Regression cover for the std build layer reporting success while skipping the
/// compile. `artifactIsUpToDate` used to compare the output's mtime against the
/// entry source alone, so editing an imported module left the output looking
/// newer than the entry, the build was skipped, and `execute` printed
/// "build exit code: 0" and exited 0 — with a stale binary in place and not even
/// a compile attempt to fail. The skip is gone; the driver decides. These cases
/// pin the observable consequence: a build whose artifact no longer compiles
/// must exit non-zero and surface the compiler's diagnostic, and a build whose
/// *imported module* no longer compiles must do the same.
const ScriptCase = struct {
    name: []const u8,
    /// Run from the fixture project directory.
    cwd: []const u8,
    /// Source file mutated in place before the second build.
    module_to_break: ?[]const u8,
    expect_second_exit_nonzero: bool,
    expect_diagnostic: ?[]const u8,
};

/// Run the fixture's build script. Each invocation gets its own cache
/// directory: `doxa run` links the build script itself into `<cache>/build.exe`,
/// and relinking a binary a previous run just executed intermittently fails on
/// Windows with "Permission denied" while the old image is still mapped. Giving
/// each run its own cache keeps every link writing a path nothing else holds.
/// The artifact under test, `bin/app`, is shared on purpose — that is the file
/// whose staleness the case is about.
fn runBuildScript(
    allocator: std.mem.Allocator,
    exe_path: []const u8,
    cwd: []const u8,
    run_index: usize,
) !harness.CommandResult {
    const cache_dir = try std.fmt.allocPrint(allocator, "{s}/.doxa-cache-run{d}", .{ cwd, run_index });
    const cache_flag = try std.fmt.allocPrint(allocator, "--cache-dir={s}", .{cache_dir});
    var argv = [_][]const u8{ exe_path, "run", "build.doxa", cache_flag };
    return harness.runCommandCapture(allocator, &argv, cwd, null);
}

fn runCase(allocator: std.mem.Allocator, exe_path: []const u8, tc: ScriptCase) !test_results {
    var passed: usize = 0;
    var failed: usize = 0;

    const first = try runBuildScript(allocator, exe_path, tc.cwd, 1);
    defer allocator.free(first.stdout);
    defer allocator.free(first.stderr);

    if (first.exit_code == 0) {
        passed += 1;
    } else {
        std.debug.print("Build script test failed ({s}): clean build exited {d}\n", .{ tc.name, first.exit_code });
        std.debug.print("stderr: {s}\n", .{first.stderr});
        failed += 1;
        return .{ .passed = passed, .failed = failed, .untested = 0 };
    }

    const break_module = tc.module_to_break orelse {
        return .{ .passed = passed, .failed = failed, .untested = 0 };
    };

    // Back-date the entry point so that any up-to-date check keyed on it alone
    // considers the output current, then break the imported module. A check that
    // only looks at the entry source will skip this build and report success.
    // After the clean build the output is newer than `src/app.doxa`, which is
    // exactly the state an entry-point-only mtime check treats as current. So
    // breaking `lib.doxa` here reproduces the silent skip without needing to
    // manipulate timestamps.
    const broken_source = "public function greet(who :: string) returns string {\n    return who.this_field_does_not_exist\n}\n";

    const module_path = try std.fs.path.join(allocator, &.{ tc.cwd, "src", break_module });
    defer allocator.free(module_path);

    const original = try std.Io.Dir.cwd().readFileAlloc(std.testing.io, module_path, allocator, .unlimited);
    defer allocator.free(original);

    try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = module_path, .data = broken_source });

    const second = try runBuildScript(allocator, exe_path, tc.cwd, 2);
    defer allocator.free(second.stdout);
    defer allocator.free(second.stderr);

    // Restore before asserting so a failure cannot leave the tree broken.
    try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = module_path, .data = original });

    if (tc.expect_second_exit_nonzero) {
        if (second.exit_code != 0) {
            passed += 1;
        } else {
            std.debug.print("Build script test failed ({s}): build with a broken {s} exited 0\n", .{ tc.name, break_module });
            std.debug.print("stderr: {s}\n", .{second.stderr});
            failed += 1;
        }
    } else if (second.exit_code == 0) {
        passed += 1;
    } else {
        std.debug.print("Build script test failed ({s}): rebuild exited {d}\n", .{ tc.name, second.exit_code });
        std.debug.print("stderr: {s}\n", .{second.stderr});
        failed += 1;
    }

    if (tc.expect_diagnostic) |needle| {
        if (std.mem.indexOf(u8, second.stderr, needle) != null) {
            passed += 1;
        } else {
            std.debug.print("Build script test failed ({s}): expected \"{s}\" in stderr\n", .{ tc.name, needle });
            std.debug.print("stderr: {s}\n", .{second.stderr});
            failed += 1;
        }
    } else {
        passed += 1;
    }

    // A rebuild must converge: restoring the module and building again has to
    // succeed, which also proves the fixture is left reusable.
    const third = try runBuildScript(allocator, exe_path, tc.cwd, 3);
    defer allocator.free(third.stdout);
    defer allocator.free(third.stderr);
    if (third.exit_code == 0) {
        passed += 1;
    } else {
        std.debug.print("Build script test failed ({s}): rebuild after restore exited {d}\n", .{ tc.name, third.exit_code });
        std.debug.print("stderr: {s}\n", .{third.stderr});
        failed += 1;
    }

    return .{ .passed = passed, .failed = failed, .untested = 0 };
}

pub fn runAll(parent_allocator: std.mem.Allocator) !test_results {
    var arena = std.heap.ArenaAllocator.init(parent_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    platform.enableUtf8Console();
    platform.sealStdHandles();

    const verbose = harness.verboseFromEnv(allocator);
    const repo_root = try harness.repoRootFromEnv(allocator);
    defer if (repo_root) |rr| allocator.free(rr);

    const exe_path = try harness.doxaExePath(allocator);
    defer allocator.free(exe_path);

    const fixture = if (repo_root) |rr|
        try std.fs.path.join(allocator, &.{ rr, "test", "build_script" })
    else
        try allocator.dupe(u8, "test/build_script");
    defer allocator.free(fixture);

    const cases = [_]ScriptCase{
        .{
            .name = "imported module change is not silently skipped",
            .cwd = fixture,
            .module_to_break = "lib.doxa",
            .expect_second_exit_nonzero = true,
            .expect_diagnostic = "Cannot access field on non-struct type String",
        },
    };

    var total: harness.Counts = .{ .passed = 0, .failed = 0, .untested = 0 };
    for (cases) |tc| {
        const result = try runCase(allocator, exe_path, tc);
        total.passed += result.passed;
        total.failed += result.failed;
        total.untested += result.untested;
        harness.printCase(tc.name, result, verbose);
    }
    return total;
}
