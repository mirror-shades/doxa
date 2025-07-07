const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    // Add test steps
    const test_run = b.addTest(.{
        .name = "test_run",
        .root_source_file = b.path("test/test_run.zig"),
    });
    const run_test_run = b.addRunArtifact(test_run);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
    //test_step.dependOn(&compile_test_run.step);
}
