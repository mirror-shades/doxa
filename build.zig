const std = @import("std");

// Global storage for install_dir path (set during build(), read during copy step)
var install_dir_path: ?[]const u8 = null;

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

    // Add test steps
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

    // Custom install behaviour:
    // `zig build install -Dinstall_dir=./path/to/install/dir`
    // will build the executable, then copy everything from `zig-out/bin`
    // into the provided directory.
    // If -Dinstall_dir is not provided, normal install behavior applies.
    if (b.option([]const u8, "output", "Directory to copy zig-out/bin contents to")) |path| {
        install_dir_path = path;
        const install_step = b.getInstallStep();
        const copy_install_step = b.allocator.create(std.Build.Step) catch @panic("OOM");
        copy_install_step.* = std.Build.Step.init(.{
            .id = .custom,
            .name = "copy-bin-to-install-dir",
            .owner = b,
            .makeFn = copyBinToInstallDir,
        });
        // Ensure the binary is built before copying
        copy_install_step.dependOn(&exe.step);
        // Add copy step to the install step
        install_step.dependOn(copy_install_step);
    }

    // Add run step
    const run_step = b.step("run", "Run the Doxa compiler");
    run_step.dependOn(&run_cmd.step);

    // Keep the separate test step
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&run_test_run.step);
    test_step.dependOn(&run_test_lsp.step);
}

fn copyBinToInstallDir(step: *std.Build.Step, options: std.Build.Step.MakeOptions) !void {
    _ = options;
    const b = step.owner;

    // Read install_dir from threadlocal storage (set during build())
    const install_path = install_dir_path orelse {
        std.debug.print("Error: install_dir option not found (this should not happen)\n", .{});
        return error.InvalidInstallPath;
    };

    const allocator = b.allocator;

    // Resolve and create the target directory
    const install_dir = try std.fs.path.resolve(allocator, &.{install_path});
    defer allocator.free(install_dir);

    std.fs.cwd().makePath(install_dir) catch |err| {
        std.debug.print("Error creating install directory '{s}': {s}\n", .{
            install_dir,
            @errorName(err),
        });
        return err;
    };

    // `getInstallPath(.bin, "")` points at zig-out/bin for this build.
    const bin_dir_path = b.getInstallPath(.bin, "");

    var src_dir = try std.fs.cwd().openDir(bin_dir_path, .{ .iterate = true });
    defer src_dir.close();

    var dst_dir = try std.fs.cwd().openDir(install_dir, .{ .iterate = true });
    defer dst_dir.close();

    // Copy all regular files from zig-out/bin into the install directory.
    var it = src_dir.iterate();
    while (try it.next()) |entry| {
        switch (entry.kind) {
            .file => {
                try src_dir.copyFile(entry.name, dst_dir, entry.name, .{});
                std.debug.print("Installed {s} -> {s}\n", .{ entry.name, install_dir });
            },
            else => {},
        }
    }
}
