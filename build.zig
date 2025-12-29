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

    const answers_module = b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_run = b.addTest(.{
        .name = "test_run",
        .root_module = blk: {
            var module = b.createModule(.{
                .root_source_file = b.path("test/test_run.zig"),
                .target = target,
                .optimize = optimize,
            });
            module.addImport("answers", answers_module);
            break :blk module;
        },
    });
    const run_test_run = b.addRunArtifact(test_run);
    run_test_run.step.dependOn(&exe.step);

    const reporting_module = b.createModule(.{
        .root_source_file = b.path("src/utils/reporting.zig"),
        .target = target,
        .optimize = optimize,
    });

    const test_lsp = b.addTest(.{
        .name = "test_lsp",
        .root_module = blk: {
            var module = b.createModule(.{
                .root_source_file = b.path("test/test_lsp.zig"),
                .target = target,
                .optimize = optimize,
            });
            module.addImport("reporting", reporting_module);
            break :blk module;
        },
    });
    const run_test_lsp = b.addRunArtifact(test_lsp);
    run_test_lsp.step.dependOn(&exe.step);

    const test_compile = b.addTest(.{
        .name = "test_compile",
        .root_module = blk: {
            var module = b.createModule(.{
                .root_source_file = b.path("test/test_compile.zig"),
                .target = target,
                .optimize = optimize,
            });
            module.addImport("answers", answers_module);
            break :blk module;
        },
    });
    const run_test_compile = b.addRunArtifact(test_compile);
    run_test_compile.step.dependOn(&exe.step);

    const run_step = b.step("run", "Run the Doxa compiler");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
    test_step.dependOn(&run_test_lsp.step);
    test_step.dependOn(&run_test_compile.step);
}
