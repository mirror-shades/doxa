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

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const raylib_dep = b.dependency("raylib", .{
        .target = target,
        .optimize = optimize,
    });
    const raylib = raylib_dep.artifact("raylib");
    exe.linkLibrary(raylib);

    const test_run = b.addTest(.{
        .name = "test_run",
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_run.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_test_run = b.addRunArtifact(test_run);

    run_test_run.step.dependOn(&exe.step);

    const build_step = b.step("default", "Build the Doxa compiler");
    build_step.dependOn(&exe.step);

    const run_step = b.step("run", "Run the Doxa compiler");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
}
