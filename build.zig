const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Standard C library is still needed
    exe.linkLibC();

    // LLVM configuration
    // To enable LLVM backend, set ENABLE_LLVM to true in main.zig after building with these settings
    const enable_llvm = b.option(bool, "enable-llvm", "Enable LLVM backend") orelse false;
    const llvm_path = b.option([]const u8, "llvm-path", "Path to LLVM installation") orelse "";

    if (enable_llvm) {
        // Detect OS for proper configuration
        const is_windows = target.result.os.tag == .windows;

        // Add LLVM path macro so we can load it dynamically at runtime
        if (llvm_path.len > 0) {
            exe.root_module.addCMacro("LLVM_PATH", b.fmt("\"{s}\"", .{llvm_path}));

            // Add LLVM include and lib paths
            if (is_windows) {
                // On Windows, we need to add include and lib directories explicitly
                const lib_dir = b.fmt("{s}/lib", .{llvm_path});
                const include_dir = b.fmt("{s}/include", .{llvm_path});

                // Add library search path
                exe.addLibraryPath(.{ .cwd_relative = lib_dir });
                exe.addIncludePath(.{ .cwd_relative = include_dir });
            }
        }

        // Add compile flag to signal LLVM is enabled
        exe.root_module.addCMacro("ENABLE_LLVM", "1");

        // Link against required LLVM libraries
        // List of core LLVM libraries required for our compiler
        const llvm_libs = [_][]const u8{
            "LLVMX86Info",
            "LLVMX86Desc",
            "LLVMCore",
            "LLVMSupport",
            "LLVMBinaryFormat",
            "LLVMBitReader",
            "LLVMBitWriter",
            "LLVMTargetParser",
            "LLVMDebugInfoCodeView",
            "LLVMObject",
            "LLVMMC",
            "LLVMIRReader",
            "LLVMPasses",
            "LLVMTransformUtils",
            "LLVMScalarOpts",
            "LLVMLinker",
            "LLVMDemangle",
        };

        // Link against the individual LLVM libraries
        for (llvm_libs) |lib| {
            exe.linkSystemLibrary(lib);
        }

        // Windows-specific dependencies
        if (is_windows) {
            exe.linkSystemLibrary("version");
            exe.linkSystemLibrary("ole32");
            exe.linkSystemLibrary("uuid");
        }

        std.debug.print("LLVM backend enabled\n", .{});
        if (llvm_path.len > 0) {
            std.debug.print("Using LLVM path: {s}\n", .{llvm_path});
        }
    }

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);
}
