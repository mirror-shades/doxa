const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Install the executable first
    b.installArtifact(exe);

    // Add run step that passes through command line arguments
    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // requires running `zig fetch --save git+https://github.com/raysan5/raylib.git` in the root directory`
    // Add raylib dependency
    const raylib_dep = b.dependency("raylib", .{
        .target = target,
        .optimize = optimize,
    });
    const raylib = raylib_dep.artifact("raylib");
    exe.linkLibrary(raylib);

    // Add test steps
    const test_run = b.addTest(.{
        .name = "test_run",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_run.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_test_run = b.addRunArtifact(test_run);

    // Make tests depend on successful compilation
    run_test_run.step.dependOn(&exe.step);

    // Make default step just build the executable
    const build_step = b.step("default", "Build the Doxa compiler");
    build_step.dependOn(&exe.step);

    // Add run step
    const run_step = b.step("run", "Run the Doxa compiler");
    run_step.dependOn(&run_cmd.step);

    // Keep the separate test step
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
}
