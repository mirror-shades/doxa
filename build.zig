const std = @import("std");

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // Step to generate the compiler hash
    const gen_hash_run = b.addRunArtifact(b.addExecutable(.{
        .name = "gen_hash",
        .root_source_file = b.path("build/gen_hash.zig"),
        .target = target,
        .optimize = .Debug,
    }));

    // The "hash" step
    const hash_step = b.step("hash", "Generate and print the compiler source hash");
    hash_step.dependOn(&gen_hash_run.step);

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    exe.step.dependOn(&gen_hash_run.step);

    b.installArtifact(exe);

    // Add test step
    const test_exe = b.addTest(.{
        .name = "test",
        .root_source_file = b.path("test/test.zig"),
    });
    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run the tests");
    test_step.dependOn(&run_test.step);
}
