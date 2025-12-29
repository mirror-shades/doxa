const std = @import("std");

const targets: []const std.Target.Query = &.{
    .{ .cpu_arch = .x86_64, .os_tag = .macos },
    .{ .cpu_arch = .aarch64, .os_tag = .macos },
    .{ .cpu_arch = .x86_64, .os_tag = .linux, .abi = .gnu },
    .{ .cpu_arch = .aarch64, .os_tag = .linux, .abi = .gnu },
    .{ .cpu_arch = .x86_64, .os_tag = .windows, .abi = .gnu },
};

const target_names = [_][]const u8{
    "macos-x64",
    "macos-arm64",
    "linux-x64",
    "linux-arm64",
    "windows-x64",
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    b.installArtifact(exe);

    const run_exe = b.addRunArtifact(exe);

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

    // Release step for cross-compilation
    const release_step = b.step("release", "Build release binaries for all platforms");

    for (targets, 0..) |t, i| {
        const release_exe = b.addExecutable(.{
            .name = "doxa",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = b.resolveTargetQuery(t),
                .optimize = .ReleaseFast,
                .strip = true,
            }),
        });

        const target_output = b.addInstallArtifact(release_exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = target_names[i],
                },
            },
        });

        release_step.dependOn(&target_output.step);
    }

    // Compress step for creating zip archives
    const compress_step = b.step("compress", "Create zip archives of release binaries");

    const compress_cmd = b.addSystemCommand(&[_][]const u8{
        "python3",
        "compress_releases.py",
        "--cwd",
        b.getInstallPath(.prefix, ""),
    });
    // Add all target names as arguments
    inline for (target_names) |name| {
        compress_cmd.addArg(name);
    }
    compress_step.dependOn(&compress_cmd.step);

    // Main integration tests
    const test_run_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_run.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_run_exe.root_module.addImport("answers", b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    }));
    const run_test_run = b.addRunArtifact(test_run_exe);
    run_test_run.skip_foreign_checks = true;

    // Compilation tests
    const test_compile_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_compile.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_compile_exe.root_module.addImport("answers", b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    }));
    const run_test_compile = b.addRunArtifact(test_compile_exe);
    run_test_compile.skip_foreign_checks = true;

    // LSP tests
    const reporting_module = b.createModule(.{
        .root_source_file = b.path("src/utils/reporting.zig"),
    });

    const test_lsp_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_lsp.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_lsp_exe.root_module.addImport("reporting", reporting_module);
    const run_test_lsp = b.addRunArtifact(test_lsp_exe);
    run_test_lsp.skip_foreign_checks = true;

    // Run all tests
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
    test_step.dependOn(&run_test_compile.step);
    test_step.dependOn(&run_test_lsp.step);
}
