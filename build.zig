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

const ZigDependency = struct {
    folder_name: []const u8,
    archive_ext: []const u8,
};

fn getZigDependencyForTarget(target: std.Target) ?ZigDependency {
    const archive_ext: []const u8 = if (target.os.tag == .windows) ".zip" else ".tar.xz";

    const folder_name = switch (target.os.tag) {
        .windows => switch (target.cpu.arch) {
            .x86_64 => "zig-x86_64-windows-0.16.0",
            else => return null,
        },
        .macos => switch (target.cpu.arch) {
            .x86_64 => "zig-x86_64-macos-0.16.0",
            .aarch64 => "zig-aarch64-macos-0.16.0",
            else => return null,
        },
        .linux => switch (target.cpu.arch) {
            .x86_64 => "zig-x86_64-linux-0.16.0",
            .aarch64 => "zig-aarch64-linux-0.16.0",
            else => return null,
        },
        else => return null,
    };

    return .{
        .folder_name = folder_name,
        .archive_ext = archive_ext,
    };
}

fn addUnpackZigDependencyStep(
    b: *std.Build,
    archive_tool: *std.Build.Step.Compile,
    archive_path: []const u8,
    destination_lib_dir: []const u8,
) *std.Build.Step.Run {
    const cmd = b.addRunArtifact(archive_tool);
    cmd.addArg("unpack-zig-dep");
    cmd.addArgs(&[_][]const u8{
        archive_path,
        destination_lib_dir,
    });
    return cmd;
}

pub fn build(b: *std.Build) void {
    const output_dir = b.option([]const u8, "output-dir", "Install output directory (default: doxa)") orelse "doxa";
    b.resolveInstallPrefix(output_dir, .{});

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const host_target = b.graph.host;
    const can_run_target =
        target.result.os.tag == host_target.result.os.tag and
        target.result.cpu.arch == host_target.result.cpu.arch;
    const archive_tool = b.addExecutable(.{
        .name = "archive_tool",
        .root_module = b.createModule(.{
            .root_source_file = b.path("scripts/archive_tool.zig"),
            .target = host_target,
            .optimize = .ReleaseSafe,
        }),
    });

    const exe = b.addExecutable(.{
        .name = "doxa",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            // Use system dlopen/dlsym on Linux for inline-zig .so modules.
            // Without libc, std.DynLib uses ElfDynLib, which is less robust.
            .link_libc = target.result.os.tag == .linux,
        }),
    });

    b.installArtifact(exe);
    const install_std = b.addInstallDirectory(.{
        .source_dir = b.path("std"),
        .install_dir = .prefix,
        .install_subdir = "lib/std",
    });
    b.getInstallStep().dependOn(&install_std.step);

    // Ship the Doxa runtime sources so `doxa compile` can stage and compile them
    // from any working directory (resolved relative to the executable).
    const install_runtime = b.addInstallDirectory(.{
        .source_dir = b.path("src/runtime"),
        .install_dir = .prefix,
        .install_subdir = "lib/runtime",
    });
    b.getInstallStep().dependOn(&install_runtime.step);

    if (getZigDependencyForTarget(target.result)) |zig_dep| {
        const archive_path = b.pathJoin(&.{ "external", b.fmt("{s}{s}", .{ zig_dep.folder_name, zig_dep.archive_ext }) });
        const destination_lib_dir = b.getInstallPath(.prefix, "lib");
        const unpack_zig_dep = addUnpackZigDependencyStep(
            b,
            archive_tool,
            archive_path,
            destination_lib_dir,
        );
        b.getInstallStep().dependOn(&unpack_zig_dep.step);
    }

    const run_exe_name = if (target.result.os.tag == .windows) "doxa.exe" else "doxa";
    const run_doxa_path = b.getInstallPath(.bin, run_exe_name);
    const run_exe = b.addSystemCommand(&[_][]const u8{run_doxa_path});
    run_exe.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_exe.addArgs(args);
    }

    const run_step = b.step("run", "Run the application");
    run_step.dependOn(&run_exe.step);

    const release_step = b.step("release", "Build release binaries for all platforms");

    for (targets, 0..) |t, i| {
        const release_target = b.resolveTargetQuery(t);
        const release_exe = b.addExecutable(.{
            .name = "doxa",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/main.zig"),
                .target = release_target,
                .optimize = .ReleaseFast,
                .strip = true,
                .link_libc = release_target.result.os.tag == .linux,
            }),
        });
        const target_output = b.addInstallArtifact(release_exe, .{
            .dest_dir = .{
                .override = .{
                    .custom = target_names[i],
                },
            },
        });
        const target_std = b.addInstallDirectory(.{
            .source_dir = b.path("std"),
            .install_dir = .prefix,
            .install_subdir = b.fmt("{s}/lib/std", .{target_names[i]}),
        });

        if (getZigDependencyForTarget(release_target.result)) |zig_dep| {
            const archive_path = b.pathJoin(&.{ "external", b.fmt("{s}{s}", .{ zig_dep.folder_name, zig_dep.archive_ext }) });
            const destination_lib_dir = b.getInstallPath(.prefix, b.fmt("{s}/lib", .{target_names[i]}));
            const unpack_zig_dep = addUnpackZigDependencyStep(
                b,
                archive_tool,
                archive_path,
                destination_lib_dir,
            );
            release_step.dependOn(&unpack_zig_dep.step);
        }

        release_step.dependOn(&target_output.step);
        release_step.dependOn(&target_std.step);
    }

    const compress_step = b.step("compress", "Create zip archives of release binaries");
    const compress_cmd = b.addRunArtifact(archive_tool);
    compress_cmd.addArgs(&[_][]const u8{
        "compress-releases",
        "--cwd",
        b.getInstallPath(.prefix, ""),
    });
    inline for (target_names) |name| {
        compress_cmd.addArg(name);
    }
    compress_step.dependOn(&compress_cmd.step);

    // Regenerate the standard-library API reference from `std/`. Kept as its
    // own step so it never runs implicitly; the generated page is committed.
    const catalog_module = b.createModule(.{
        .root_source_file = b.path("src/stdlib/catalog.zig"),
    });
    const docs_tool = b.addExecutable(.{
        .name = "gen_stdlib_docs",
        .root_module = b.createModule(.{
            .root_source_file = b.path("scripts/gen_stdlib_docs.zig"),
            .target = host_target,
            .optimize = .ReleaseSafe,
        }),
    });
    docs_tool.root_module.addImport("catalog", catalog_module);
    const gen_docs = b.addRunArtifact(docs_tool);
    gen_docs.setCwd(b.path("."));
    const docs_step = b.step("docs", "Regenerate docs/stdlib-api.md from std/");
    docs_step.dependOn(&gen_docs.step);

    const answers_module = b.createModule(.{
        .root_source_file = b.path("test/answers.zig"),
    });
    const platform_module = b.createModule(.{
        .root_source_file = b.path("src/utils/platform.zig"),
    });
    const reporting_module = b.createModule(.{
        .root_source_file = b.path("src/utils/reporting.zig"),
    });
    const constants_module = b.createModule(.{
        .root_source_file = b.path("src/common/constants.zig"),
    });

    // Dedicated install location for tests so a long-running editor/LSP instance
    // doesn't lock the default installed binary and break `zig build test` on Windows.
    const test_install = b.addInstallArtifact(exe, .{
        .dest_dir = .{ .override = .{ .custom = "test-bin" } },
    });
    const exe_name = if (target.result.os.tag == .windows) "doxa.exe" else "doxa";
    const test_doxa_path = b.getInstallPath(.{ .custom = "test-bin" }, exe_name);

    // Three test executables, each run from the build root. The unit and LSP
    // roots are in-process tests on the test runner's protocol. The program
    // suites drive the installed binary for about a minute, so they run as a
    // plain process (`test/suites.zig` says why). Every run is marked as having
    // side effects: the unit root reads `std/` and fixtures at runtime and the
    // suites drive a binary built elsewhere, so no result is a function of its
    // executable alone, and a cached pass would be a claim nothing re-checked.
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    const run_unit_tests = b.addRunArtifact(unit_tests);
    run_unit_tests.has_side_effects = true;
    run_unit_tests.skip_foreign_checks = true;
    run_unit_tests.setCwd(b.path("."));

    const lsp_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/test_lsp.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    lsp_tests.root_module.addImport("reporting", reporting_module);
    const run_lsp_tests = b.addRunArtifact(lsp_tests);
    run_lsp_tests.has_side_effects = true;
    run_lsp_tests.skip_foreign_checks = true;
    run_lsp_tests.setCwd(b.path("."));

    const program_suites = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("test/suites.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });
    program_suites.root_module.addImport("answers", answers_module);
    program_suites.root_module.addImport("platform", platform_module);
    program_suites.root_module.addImport("reporting", reporting_module);
    program_suites.root_module.addImport("constants", constants_module);
    const run_program_suites = std.Build.Step.Run.create(b, "run program suites");
    run_program_suites.has_side_effects = true;
    run_program_suites.addArtifactArg(program_suites);
    run_program_suites.skip_foreign_checks = true;
    run_program_suites.setCwd(b.path("."));
    run_program_suites.step.dependOn(&test_install.step);
    // test-bin/doxa resolves the bundled std and zig through ../lib.
    run_program_suites.step.dependOn(b.getInstallStep());
    run_program_suites.setEnvironmentVariable("DOXA_BIN", test_doxa_path);

    // Unit, then LSP, then the program suites: run steps never spawn
    // concurrently. On Windows, Zig creates each child's pipe ends inheritable
    // and spawns with `bInheritHandles` and no handle list, so a child spawned
    // while another spawn is in flight inherits that sibling's stdout, and a
    // short protocol root whose pipe a long-lived sibling captured sees no EOF
    // until the sibling exits ("test runner failed to respond"). Zig has no
    // order-only edge, so the chain is also a success gate: a failing root
    // skips the roots after it. The cheap, precise roots go first so the
    // minute-long suites never hide them.
    // TODO: drop this chain once Zig spawns with
    // `PROC_THREAD_ATTRIBUTE_HANDLE_LIST`.
    run_lsp_tests.step.dependOn(&run_unit_tests.step);
    run_program_suites.step.dependOn(&run_lsp_tests.step);

    const wiring = TestWiring.create(b, &.{
        .{ .path = "test.zig", .cached = true },
        .{ .path = "test/test_lsp.zig", .cached = true },
        .{ .path = "test/suites.zig", .cached = false },
    }, &.{
        .{ .name = "answers", .path = "test/answers.zig" },
        .{ .name = "platform", .path = "src/utils/platform.zig" },
        .{ .name = "reporting", .path = "src/utils/reporting.zig" },
        .{ .name = "constants", .path = "src/common/constants.zig" },
    });

    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&wiring.step);
    if (can_run_target) {
        test_step.dependOn(&run_unit_tests.step);
        test_step.dependOn(&run_lsp_tests.step);
        test_step.dependOn(&run_program_suites.step);
    } else {
        // Cross-target: compile tests + doxa, but don't execute anything.
        test_step.dependOn(&test_install.step);
        test_step.dependOn(&unit_tests.step);
        test_step.dependOn(&lsp_tests.step);
        test_step.dependOn(&program_suites.step);
    }

    const test_lsp_step = b.step("test-lsp", "Run LSP tests");
    test_lsp_step.dependOn(&run_lsp_tests.step);
}

/// Fails `zig build test` when a test would silently never run, or a cached
/// root could drive the installed binary. It reads the sources on every
/// invocation, so unlike a test it can never be served from a cache.
///
/// Zig compiles a file's tests only when a `test` block names the file, as in
/// `test { _ = @import("x.zig"); }`; a top-level import compiles the file but
/// never its tests. So this walks `@import`s from the test roots and requires:
///
/// - every `test/*.zig` is reached by some root, through any import;
/// - every file declaring a `test`, among `test/*.zig` and `src/**`, is a root
///   or is named in a `test` block of a file whose tests run, within the same
///   module (a named module's tests never run from another module's root);
/// - no cached root reaches `test/process.zig`, the suites' only spawner: a
///   cached root's result is a function of its own executable, which knows
///   nothing of the binary under test.
const TestWiring = struct {
    step: std.Build.Step,
    roots: []const Root,
    modules: []const Module,

    const Root = struct {
        path: []const u8,
        /// Run through the test runner's protocol.
        cached: bool,
    };

    /// A named module (`b.createModule`) one of the roots imports.
    const Module = struct {
        name: []const u8,
        path: []const u8,
    };

    const spawner = "test/process.zig";

    fn create(b: *std.Build, roots: []const Root, modules: []const Module) *TestWiring {
        const wiring = b.allocator.create(TestWiring) catch @panic("OOM");
        wiring.* = .{
            .step = .init(.{ .id = .custom, .name = "check test wiring", .owner = b, .makeFn = make }),
            .roots = b.allocator.dupe(Root, roots) catch @panic("OOM"),
            .modules = b.allocator.dupe(Module, modules) catch @panic("OOM"),
        };
        return wiring;
    }

    /// A source file's imports of other repository files, and whether it
    /// declares tests.
    const File = struct {
        imports: []const Import,
        declares_tests: bool,
    };

    const Import = struct {
        path: []const u8,
        /// Names the file inside a `test` block.
        in_test: bool,
        /// Crosses into a named module.
        named: bool,
    };

    const Files = std.StringArrayHashMapUnmanaged(File);
    const Paths = std.StringArrayHashMapUnmanaged(void);

    fn make(step: *std.Build.Step, options: std.Build.Step.MakeOptions) !void {
        const wiring: *TestWiring = @fieldParentPtr("step", step);
        const b = step.owner;
        const io = b.graph.io;
        var arena_state: std.heap.ArenaAllocator = .init(options.gpa);
        defer arena_state.deinit();
        const arena = arena_state.allocator();

        var files: Files = .empty;
        // Files reached through any import; through any import from a cached
        // root; and through test-block imports alone, whose tests therefore run.
        var reached: Paths = .empty;
        var cached_reach: Paths = .empty;
        var tested: Paths = .empty;
        for (wiring.roots) |root| {
            try wiring.walk(arena, io, &files, &reached, root.path, .any);
            if (root.cached) try wiring.walk(arena, io, &files, &cached_reach, root.path, .any);
            try wiring.walk(arena, io, &files, &tested, root.path, .tests);
        }

        var candidates: std.ArrayList([]const u8) = .empty;
        var test_dir = try b.build_root.handle.openDir(io, "test", .{ .iterate = true });
        defer test_dir.close(io);
        var test_entries = test_dir.iterate();
        while (try test_entries.next(io)) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.name, ".zig")) continue;
            const path = try std.fmt.allocPrint(arena, "test/{s}", .{entry.name});
            if (reached.contains(path)) {
                try candidates.append(arena, path);
            } else {
                try step.addError("{s} is not imported by any test root", .{path});
            }
        }
        var src_dir = try b.build_root.handle.openDir(io, "src", .{ .iterate = true });
        defer src_dir.close(io);
        var src_walker = try src_dir.walk(arena);
        defer src_walker.deinit();
        while (try src_walker.next(io)) |entry| {
            if (entry.kind != .file or !std.mem.endsWith(u8, entry.path, ".zig")) continue;
            const path = try std.fmt.allocPrint(arena, "src/{s}", .{entry.path});
            std.mem.replaceScalar(u8, path, '\\', '/');
            try candidates.append(arena, path);
        }
        for (candidates.items) |path| {
            if (tested.contains(path)) continue;
            if ((try wiring.load(arena, io, &files, path)).declares_tests) {
                try step.addError("{s} declares tests that never run; name it in a root's `test {{ _ = @import(...); }}` block", .{path});
            }
        }

        if (cached_reach.contains(spawner)) {
            try step.addError("a cached test root reaches {s}; suites that drive the installed binary belong under test/suites.zig", .{spawner});
        }
        if (step.result_error_msgs.items.len != 0) return error.MakeFailed;
    }

    /// Adds every file reachable from `start` to `seen`, following every import
    /// (`.any`) or only same-module imports inside `test` blocks (`.tests`).
    fn walk(
        wiring: *const TestWiring,
        arena: std.mem.Allocator,
        io: std.Io,
        files: *Files,
        seen: *Paths,
        start: []const u8,
        follow: enum { any, tests },
    ) !void {
        var pending: std.ArrayList([]const u8) = .empty;
        try pending.append(arena, start);
        while (pending.pop()) |path| {
            if ((try seen.getOrPut(arena, path)).found_existing) continue;
            for ((try wiring.load(arena, io, files, path)).imports) |import| {
                if (follow == .tests and (!import.in_test or import.named)) continue;
                try pending.append(arena, import.path);
            }
        }
    }

    fn load(wiring: *const TestWiring, arena: std.mem.Allocator, io: std.Io, files: *Files, path: []const u8) !File {
        if (files.get(path)) |file| return file;
        const source = try wiring.step.owner.build_root.handle.readFileAllocOptions(io, path, arena, .unlimited, .of(u8), 0);
        const file = try wiring.scan(arena, path, source);
        try files.put(arena, path, file);
        return file;
    }

    /// Tokenizes `source`, so comments and string contents never count.
    fn scan(wiring: *const TestWiring, arena: std.mem.Allocator, path: []const u8, source: [:0]const u8) !File {
        var imports: std.ArrayList(Import) = .empty;
        var declares_tests = false;
        var tokens: std.zig.Tokenizer = .init(source);
        var depth: usize = 0;
        // Brace depth of the open `test` body, if any.
        var test_depth: ?usize = null;
        var test_opening = false;
        while (true) {
            const token = tokens.next();
            switch (token.tag) {
                .eof => break,
                .keyword_test => {
                    declares_tests = true;
                    if (test_depth == null) test_opening = true;
                },
                .l_brace => {
                    depth += 1;
                    if (test_opening) {
                        test_depth = depth;
                        test_opening = false;
                    }
                },
                .r_brace => {
                    if (test_depth == depth) test_depth = null;
                    depth -= 1;
                },
                .builtin => {
                    if (!std.mem.eql(u8, source[token.loc.start..token.loc.end], "@import")) continue;
                    if (tokens.next().tag != .l_paren) continue;
                    const operand = tokens.next();
                    if (operand.tag != .string_literal) continue;
                    const spelled = source[operand.loc.start + 1 .. operand.loc.end - 1];
                    const in_test = test_depth != null;
                    if (std.mem.endsWith(u8, spelled, ".zig")) {
                        const dir = std.fs.path.dirnamePosix(path) orelse "";
                        const resolved = try std.fs.path.resolvePosix(arena, &.{ dir, spelled });
                        try imports.append(arena, .{ .path = resolved, .in_test = in_test, .named = false });
                    } else for (wiring.modules) |module| {
                        if (std.mem.eql(u8, module.name, spelled)) {
                            try imports.append(arena, .{ .path = module.path, .in_test = in_test, .named = true });
                        }
                    }
                },
                else => {},
            }
        }
        return .{ .imports = try imports.toOwnedSlice(arena), .declares_tests = declares_tests };
    }
};
