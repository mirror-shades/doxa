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
    const host_target = b.graph.host;
    const can_run_target =
        target.result.os.tag == host_target.result.os.tag and
        target.result.cpu.arch == host_target.result.cpu.arch;

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

    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

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

    const compress_step = b.step("compress", "Create zip archives of release binaries");

    const compress_cmd = b.addSystemCommand(&[_][]const u8{
        "python3",
        "compress_releases.py",
        "--cwd",
        b.getInstallPath(.prefix, ""),
    });
    inline for (target_names) |name| {
        compress_cmd.addArg(name);
    }
    compress_step.dependOn(&compress_cmd.step);

    const answers_module = b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    });

    // Dedicated install location for tests so a long-running editor/LSP instance
    // doesn't lock `zig-out/bin/doxa(.exe)` and break `zig build test` on Windows.
    const test_install = b.addInstallArtifact(exe, .{
        .dest_dir = .{ .override = .{ .custom = "test-bin" } },
    });
    const exe_name = if (target.result.os.tag == .windows) "doxa.exe" else "doxa";
    const test_doxa_path = b.pathJoin(&.{ "zig-out", "test-bin", exe_name });

    const test_suite_exe = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    test_suite_exe.root_module.addImport("answers", answers_module);
    const run_test_suite = b.addRunArtifact(test_suite_exe);
    run_test_suite.skip_foreign_checks = true;
    run_test_suite.step.dependOn(&test_install.step);
    run_test_suite.setEnvironmentVariable("DOXA_BIN", test_doxa_path);

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
    run_test_lsp.step.dependOn(&test_install.step);
    run_test_lsp.setEnvironmentVariable("DOXA_BIN", test_doxa_path);

    const test_step = b.step("test", "Run all tests");
    if (can_run_target) {
        test_step.dependOn(&run_test_suite.step);
        test_step.dependOn(&run_test_lsp.step);
    } else {
        // Cross-target: compile tests + doxa, but don't execute anything.
        test_step.dependOn(&test_install.step);
        test_step.dependOn(&test_suite_exe.step);
        test_step.dependOn(&test_lsp_exe.step);
    }

    const test_lsp_step = b.step("test-lsp", "Run LSP tests");
    test_lsp_step.dependOn(&run_test_lsp.step);
}
