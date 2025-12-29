const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
        }),
    });

    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

    // Main integration tests
    const test_run_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_run.zig"),
            .target = target,
        }),
    });
    test_run_exe.root_module.addImport("answers", b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    }));
    const run_test_run = b.addRunArtifact(test_run_exe);

    // Compilation tests
    const test_compile_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_compile.zig"),
            .target = target,
        }),
    });
    test_compile_exe.root_module.addImport("answers", b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    }));
    const run_test_compile = b.addRunArtifact(test_compile_exe);

    // LSP tests
    const reporting_module = b.createModule(.{
        .root_source_file = b.path("src/utils/reporting.zig"),
    });

    const test_lsp_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_lsp.zig"),
            .target = target,
        }),
    });
    test_lsp_exe.root_module.addImport("reporting", reporting_module);
    const run_test_lsp = b.addRunArtifact(test_lsp_exe);

    // Run all tests
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
    test_step.dependOn(&run_test_compile.step);
    test_step.dependOn(&run_test_lsp.step);
}
