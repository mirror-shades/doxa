const std = @import("std");
const builtin = @import("builtin");
const LexicalAnalyzer = @import("./analysis/lexical.zig").LexicalAnalyzer;
const SemanticAnalyzer = @import("./analysis/semantic/semantic.zig").SemanticAnalyzer;
const Parser = @import("./parser/parser_types.zig").Parser;
const Reporting = @import("./utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const ReporterOptions = Reporting.ReporterOptions;
const MemoryImport = @import("./utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Token = @import("./types/token.zig").Token;
const TypesImport = @import("./types/types.zig");
const TokenLiteral = TypesImport.TokenLiteral;
const Environment = TypesImport.Environment;
const AST = @import("./ast/ast.zig");
const HIRGenerator = @import("./codegen/hir/soxa_generator.zig").HIRGenerator;
const HIRProgram = @import("./codegen/hir/soxa_types.zig").HIRProgram;
const HIRType = @import("./codegen/hir/soxa_types.zig").HIRType;

const ConstantFolder = @import("./analysis/constant_folder.zig").ConstantFolder;
const Errors = @import("./utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ProfilerImport = @import("./utils/profiler.zig");
const Phase = ProfilerImport.Phase;
const Profiler = ProfilerImport.Profiler;
const source_cache = @import("./utils/source_cache.zig");
const inline_zig_compiler = @import("./inline_zig/compiler.zig");
const StructMethodInfo = @import("./analysis/semantic/semantic.zig").StructMethodInfo;
const LspServer = @import("./lsp/server.zig");
const Resolver = @import("./resolver/resolver.zig").Resolver;

const constants = @import("common/constants.zig");
const MAX_FILE_SIZE = constants.MAX_SOURCE_FILE_BYTES;
const EXIT_CODE_USAGE = constants.EXIT_CODE_USAGE;
const DOXA_EXTENSION = ".doxa";
const DEFAULT_CACHE_DIR = ".doxa-cache";
const DEFAULT_BIN_DIR = "bin";

const Mode = enum {
    UNDEFINED,
    RUN,
    COMPILE,
};

const LspMode = enum {
    none,
    stdio,
    harness,
};

const CLI = struct {
    mode: Mode,
    reporter_options: ReporterOptions,
    script_path: ?[]const u8,
    profile: bool,
    output_path: ?[]const u8,
    cache_dir: []const u8,
    target_arch: ?[]const u8,
    target_os: ?[]const u8,
    target_abi: ?[]const u8,
    opt: Opt,
    // Native linking directives forwarded to the final `zig build-exe` link
    // step (and the object `cc` step for includes). Each `--link/--libdir/
    // --framework/--include` occurrence appends one entry.
    link_libs: std.array_list.Managed([]const u8),
    lib_dirs: std.array_list.Managed([]const u8),
    frameworks: std.array_list.Managed([]const u8),
    include_dirs: std.array_list.Managed([]const u8),
    emit_opt_ir: bool,
    emit_asm: bool,
    lsp_mode: LspMode,
    lsp_debug_file: ?[]const u8,
    lsp_io_trace: bool,
    program_args: []const []const u8,

    pub fn deinit(self: *const CLI, allocator: std.mem.Allocator) void {
        if (self.script_path) |p| allocator.free(p);
        if (self.output_path) |p| allocator.free(p);
        if (!std.mem.eql(u8, self.cache_dir, DEFAULT_CACHE_DIR)) allocator.free(self.cache_dir);
        if (self.target_arch) |p| allocator.free(p);
        if (self.target_os) |p| allocator.free(p);
        if (self.target_abi) |p| allocator.free(p);
        for (self.link_libs.items) |p| allocator.free(p);
        for (self.lib_dirs.items) |p| allocator.free(p);
        for (self.frameworks.items) |p| allocator.free(p);
        for (self.include_dirs.items) |p| allocator.free(p);
        @constCast(&self.link_libs).deinit();
        @constCast(&self.lib_dirs).deinit();
        @constCast(&self.frameworks).deinit();
        @constCast(&self.include_dirs).deinit();
        if (self.lsp_debug_file) |p| allocator.free(p);
        for (self.program_args) |arg| allocator.free(arg);
        if (self.program_args.len > 0) allocator.free(self.program_args);
    }
};

fn fileStem(path: []const u8) []const u8 {
    const filename = std.fs.path.basename(path);
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot| {
        if (dot > 0) return filename[0..dot];
    }
    return filename;
}

fn looksLikeOutputPath(path: []const u8) bool {
    if (std.fs.path.isAbsolute(path)) return true;
    if (std.mem.startsWith(u8, path, "./") or std.mem.startsWith(u8, path, "../")) return true;
    if (std.mem.startsWith(u8, path, ".\\") or std.mem.startsWith(u8, path, "..\\")) return true;
    return std.mem.indexOfAny(u8, path, "/\\") != null;
}

/// The resolved codegen target for the build: arch/os/abi components from the
/// CLI flags, defaulting to empty (host) when absent. This single resolution is
/// threaded through every toolchain step (`zig cc`, `build-obj`, `build-exe`)
/// and the output naming so the whole pipeline agrees on one triple.
const TargetTriple = struct {
    /// Owned "arch-os-abi" triple (components omitted when empty); empty when
    /// compiling for the host.
    triple: []const u8,
    /// Effective target OS name: the requested OS when cross-compiling,
    /// otherwise the host OS. Drives target-true naming decisions (`.exe`
    /// suffix, object extension, platform link libs).
    os: []const u8,

    /// Resolve the CLI's arch/os/abi flags. Explicitly naming any component
    /// while cross-compiling requires an arch and an OS: missing components
    /// would otherwise be silent host defaults, which are never correct for a
    /// cross build (an arch-less `-target linux` is not even a valid triple).
    fn resolve(cli: *const CLI, allocator: std.mem.Allocator) !TargetTriple {
        const arch = cli.target_arch orelse "";
        const os = cli.target_os orelse "";
        const abi = cli.target_abi orelse "";
        const is_cross = arch.len > 0 or os.len > 0 or abi.len > 0;
        if (is_cross and os.len == 0) return error.MissingTargetOs;
        if (is_cross and arch.len == 0) return error.MissingTargetArch;
        const effective_os = if (os.len > 0)
            os
        else
            switch (builtin.os.tag) {
                .windows => "windows",
                .linux => "linux",
                .macos => "macos",
                else => "",
            };
        const triple = try std.fmt.allocPrint(allocator, "{s}{s}{s}{s}{s}", .{
            arch,
            if (os.len > 0) "-" else "",
            os,
            if (abi.len > 0) "-" else "",
            abi,
        });
        errdefer allocator.free(triple);
        return .{ .triple = triple, .os = try allocator.dupe(u8, effective_os) };
    }

    fn deinit(self: *TargetTriple, allocator: std.mem.Allocator) void {
        allocator.free(self.triple);
        allocator.free(self.os);
    }

    fn isCross(self: TargetTriple) bool {
        return self.triple.len > 0;
    }

    fn isWindows(self: TargetTriple) bool {
        return std.mem.eql(u8, self.os, "windows");
    }

    /// Append `-target <triple>` to `args` when cross-compiling.
    fn appendTargetArg(self: TargetTriple, args: *std.array_list.Managed([]const u8)) !void {
        if (!self.isCross()) return;
        try args.append("-target");
        try args.append(self.triple);
    }
};

fn withExeSuffix(allocator: std.mem.Allocator, path: []const u8, is_windows: bool) ![]u8 {
    if (is_windows) {
        if (std.mem.endsWith(u8, path, ".exe")) return allocator.dupe(u8, path);
        return std.fmt.allocPrint(allocator, "{s}.exe", .{path});
    }
    return allocator.dupe(u8, path);
}

// `doxa compile` writes a user-facing binary (`-o`, or `bin/<stem>`).
// `doxa run` writes into the cache directory and executes from there.
fn nativeOutputPath(allocator: std.mem.Allocator, cli: *const CLI, script_path: []const u8, is_windows: bool) ![]u8 {
    const raw = switch (cli.mode) {
        .COMPILE => blk: {
            const stem = cli.output_path orelse fileStem(script_path);
            if (looksLikeOutputPath(stem)) break :blk try allocator.dupe(u8, stem);
            break :blk try std.fmt.allocPrint(allocator, "{s}/{s}", .{ DEFAULT_BIN_DIR, stem });
        },
        .RUN => try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cli.cache_dir, fileStem(script_path) }),
        .UNDEFINED => unreachable,
    };
    defer allocator.free(raw);
    return withExeSuffix(allocator, raw, is_windows);
}

fn registerMissingTypesFromModuleCache(parser: *Parser, semantic_analyzer: *SemanticAnalyzer) !void {
    const Registration = struct {
        fn enumDecl(parser_inner: *Parser, analyzer: *SemanticAnalyzer, ed: anytype) !void {
            const helpers = @import("./analysis/semantic/helpers.zig");
            const variants = try parser_inner.allocator.alloc([]const u8, ed.variants.len);
            for (ed.variants, variants) |v, *name| name.* = v.lexeme;
            try helpers.registerEnumType(analyzer, ed.name.lexeme, variants);
        }
        fn groupDecl(analyzer: *SemanticAnalyzer, gd: anytype) !void {
            const helpers = @import("./analysis/semantic/helpers.zig");
            try helpers.registerGroupType(analyzer, gd.name.lexeme, gd.members);
        }
        fn structDecl(analyzer: *SemanticAnalyzer, sd: anytype) !void {
            const ast = @import("./ast/ast.zig");
            const helpers = @import("./analysis/semantic/helpers.zig");
            const field_types = try analyzer.allocator.alloc(ast.StructFieldType, sd.fields.len);
            for (sd.fields, 0..) |field, i| {
                field_types[i] = ast.StructFieldType{
                    .name = field.name.lexeme,
                    .type_info = try analyzer.typeExprToTypeInfo(field.type_expr),
                    .is_public = field.is_public,
                };
            }
            try helpers.registerStructType(analyzer, sd.name.lexeme, field_types);
        }
    };

    // Ensure all lazy module namespaces are loaded so their enum/group/struct
    // declarations are available in the module cache.
    var ns_it = parser.module_namespaces.iterator();
    while (ns_it.next()) |entry| {
        const mi = entry.value_ptr.*;
        if (mi.ast == null) {
            _ = parser.ensureModuleNamespace(entry.key_ptr.*) catch continue;
        }
    }

    var cache_it = parser.module_cache.iterator();
    while (cache_it.next()) |entry| {
        const module_info = entry.value_ptr.*;
        const module_ast = module_info.ast orelse continue;
        if (module_ast.data != .Block) continue;
        for (module_ast.data.Block.statements) |stmt| {
            switch (stmt.data) {
                .EnumDecl => |ed| try Registration.enumDecl(parser, semantic_analyzer, ed),
                .GroupDecl => |gd| try Registration.groupDecl(semantic_analyzer, gd),
                .Expression => |maybe_expr| {
                    if (maybe_expr) |expr| {
                        if (expr.data == .EnumDecl) {
                            try Registration.enumDecl(parser, semantic_analyzer, expr.data.EnumDecl);
                        } else if (expr.data == .GroupDecl) {
                            try Registration.groupDecl(semantic_analyzer, expr.data.GroupDecl);
                        } else if (expr.data == .StructDecl) {
                            try Registration.structDecl(semantic_analyzer, expr.data.StructDecl);
                        }
                    }
                },
                else => {},
            }
        }
    }
}

fn generateHIRProgram(memoryManager: *MemoryManager, statements: []AST.Stmt, module_namespaces: std.StringHashMap(AST.ModuleInfo), parser: *Parser, semantic_analyzer: *SemanticAnalyzer, reporter: *Reporter) !HIRProgram {
    const root_scope = semantic_analyzer.memory.scope_manager.root_scope orelse return error.MissingRootScope;
    var constant_folder = ConstantFolder.init(memoryManager.getAnalysisAllocator(), root_scope);
    var folded_statements = std.array_list.Managed(AST.Stmt).init(memoryManager.getAnalysisAllocator());
    defer folded_statements.deinit();

    for (statements) |stmt| {
        var mutable_stmt = stmt;
        const folded_stmt = try constant_folder.foldStmt(&mutable_stmt);
        try folded_statements.append(folded_stmt);
    }

    // Enums, groups, and structs declared in dependency modules may not appear
    // on the root parser's `imported_symbols` map (private structs are never
    // direct imports, yet public structs reference them in fields). Register them
    // from the module cache so HIR lowering resolves types like `error.IO` in
    // return unions and `Node[]` in `LinkedList.nodes`.
    try registerMissingTypesFromModuleCache(parser, semantic_analyzer);

    // Recompute struct field HIR types now that enums/groups/structs from every
    // module are registered. The eager lowering in registerStructType ran before
    // cross-module types were known and left Struct(0)/Unknown placeholders.
    try semantic_analyzer.recomputeStructFieldHIRTypes();

    var hir_generator = HIRGenerator.init(memoryManager.getAnalysisAllocator(), reporter, module_namespaces, parser.imported_symbols, semantic_analyzer.getFunctionReturnTypes(), semantic_analyzer);
    defer hir_generator.deinit();

    hir_generator.type_system.function_signatures = &hir_generator.function_signatures;

    const custom_types = semantic_analyzer.getCustomTypes();
    var custom_types_iter = custom_types.iterator();
    while (custom_types_iter.next()) |entry| {
        const custom_type = entry.value_ptr.*;
        const converted_type = try SemanticAnalyzer.convertCustomTypeInfo(semantic_analyzer, custom_type, memoryManager.getAnalysisAllocator());
        try hir_generator.type_system.custom_types.put(custom_type.name, converted_type);
    }

    const struct_methods = semantic_analyzer.getStructMethods();
    var struct_methods_iter = struct_methods.iterator();
    while (struct_methods_iter.next()) |entry| {
        const struct_name = entry.key_ptr.*;
        const method_table_src = entry.value_ptr.*;

        var method_table_dst = std.StringHashMap(StructMethodInfo).init(memoryManager.getAnalysisAllocator());
        var mi_it = method_table_src.iterator();
        while (mi_it.next()) |mi_entry| {
            const mname = mi_entry.key_ptr.*;
            const mi = mi_entry.value_ptr.*;
            try method_table_dst.put(mname, mi);
        }

        try hir_generator.struct_methods.put(struct_name, method_table_dst);
    }

    const hir_program = try hir_generator.generateProgram(folded_statements.items);
    return hir_program;
}

fn compileInlineZigObjects(memoryManager: *MemoryManager, statements: []AST.Stmt, parser: *Parser, reporter: *Reporter, cache_dir: []const u8, zig_opt_flag: []const u8, target: TargetTriple, include_dirs: []const []const u8) ![]const []const u8 {
    const zig_exe_path = try resolveBundledZigExecutable(memoryManager.getAllocator());
    defer memoryManager.getAllocator().free(zig_exe_path);
    return inline_zig_compiler.compileInlineZigObjects(memoryManager, statements, parser, reporter, zig_exe_path, cache_dir, zig_opt_flag, target.triple, target.os, include_dirs);
}

fn openDirMaybeAbs(path: []const u8, opts: std.fs.Dir.OpenOptions) !std.fs.Dir {
    return if (std.fs.path.isAbsolute(path))
        std.fs.openDirAbsolute(path, opts)
    else
        std.fs.cwd().openDir(path, opts);
}

fn dirContainsFile(dir: []const u8, name: []const u8) bool {
    const joined = std.fs.path.join(std.heap.page_allocator, &.{ dir, name }) catch return false;
    defer std.heap.page_allocator.free(joined);
    const file = (if (std.fs.path.isAbsolute(joined))
        std.fs.openFileAbsolute(joined, .{})
    else
        std.fs.cwd().openFile(joined, .{})) catch return false;
    file.close();
    return true;
}

// Locate the directory holding the Doxa runtime source (`doxa_rt.zig` and its
// self-contained siblings). Tries the installed layout next to the executable
// first, then repo-relative layouts, then a CWD-relative fallback (running from
// the repo root). Returns an owned path to the directory.
fn resolveRuntimeSourceDir(allocator: std.mem.Allocator) ![]u8 {
    if (std.fs.selfExeDirPathAlloc(allocator)) |exe_dir| {
        defer allocator.free(exe_dir);
        const candidates = [_][]const []const u8{
            &.{ exe_dir, "..", "lib", "runtime" }, // installed / shipped
            &.{ exe_dir, "..", "..", "src", "runtime" }, // exe under <repo>/doxa/bin
            &.{ exe_dir, "..", "..", "..", "src", "runtime" },
        };
        for (candidates) |parts| {
            const dir = try std.fs.path.join(allocator, parts);
            if (dirContainsFile(dir, "doxa_rt.zig")) return dir;
            allocator.free(dir);
        }
    } else |_| {}

    const cwd_relative = try allocator.dupe(u8, "src/runtime");
    if (dirContainsFile(cwd_relative, "doxa_rt.zig")) return cwd_relative;
    allocator.free(cwd_relative);

    return error.RuntimeSourceNotFound;
}

// Copy the runtime `.zig` sources into `<cache_dir>/runtime/` so the generated
// root can `@import` them as a subpath (Zig forbids imports outside the root
// file's directory). Returns the owned destination directory path.
fn copyRuntimeToCache(allocator: std.mem.Allocator, src_dir: []const u8, cache_dir: []const u8) ![]u8 {
    const dst_dir = try std.fs.path.join(allocator, &.{ cache_dir, "runtime" });
    errdefer allocator.free(dst_dir);
    try std.fs.cwd().makePath(dst_dir);

    var src = try openDirMaybeAbs(src_dir, .{ .iterate = true });
    defer src.close();
    var dst = try openDirMaybeAbs(dst_dir, .{});
    defer dst.close();

    var it = src.iterate();
    while (try it.next()) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".zig")) continue;
        try src.copyFile(entry.name, dst, entry.name, .{});
    }

    return dst_dir;
}

// Stage the runtime under the cache dir and write the generated Zig root that owns
// the platform entry point. Returns the owned path to the root file.
fn prepareZigRoot(allocator: std.mem.Allocator, cache_dir: []const u8) ![]const u8 {
    const runtime_src = try resolveRuntimeSourceDir(allocator);
    defer allocator.free(runtime_src);
    const runtime_dst = try copyRuntimeToCache(allocator, runtime_src, cache_dir);
    defer allocator.free(runtime_dst);

    const root_path = try std.fmt.allocPrint(allocator, "{s}/__doxa_main.zig", .{cache_dir});
    errdefer allocator.free(root_path);

    var content = std.array_list.Managed(u8).init(allocator);
    defer content.deinit();
    try content.appendSlice("const std = @import(\"std\");\n");
    try content.appendSlice("const doxa_rt = @import(\"runtime/doxa_rt.zig\");\n");

    try content.appendSlice("\nextern fn doxa_program_main() callconv(.c) void;\n");
    try content.appendSlice("\npub fn main() void {\n");
    try content.appendSlice("    const argv = std.process.argsAlloc(std.heap.page_allocator) catch {\n");
    try content.appendSlice("        doxa_program_main();\n");
    try content.appendSlice("        return;\n");
    try content.appendSlice("    };\n");
    try content.appendSlice("    defer std.process.argsFree(std.heap.page_allocator, argv);\n");
    try content.appendSlice("    doxa_rt.doxa_set_args(@as(i32, @intCast(argv.len)), @ptrCast(argv.ptr));\n");
    try content.appendSlice("    doxa_program_main();\n");
    try content.appendSlice("}\n");

    try std.fs.cwd().writeFile(.{ .sub_path = root_path, .data = content.items });
    return root_path;
}

fn resolveBundledZigExecutable(allocator: std.mem.Allocator) ![]u8 {
    const exe_dir = try std.fs.selfExeDirPathAlloc(allocator);
    defer allocator.free(exe_dir);

    const zig_exe_name = if (builtin.os.tag == .windows) "zig.exe" else "zig";
    const zig_path = try std.fs.path.resolve(allocator, &.{ exe_dir, "..", "lib", "zig", zig_exe_name });
    errdefer allocator.free(zig_path);

    const file = std.fs.openFileAbsolute(zig_path, .{}) catch |err| {
        std.debug.print("Error: bundled Zig not found at '{s}' ({s})\n", .{ zig_path, @errorName(err) });
        std.debug.print("Run `zig build` from the repository root to unpack the toolchain into doxa/lib/zig.\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    };
    file.close();
    return zig_path;
}

// Optimization is described on two axes that govern different parts of the
// toolchain:
//
//   * `mode` — the zig backend release mode for the runtime and the final
//     `zig build-exe` link (`Debug`, `ReleaseSafe`, `ReleaseFast`,
//     `ReleaseSmall`). `safe` keeps runtime safety checks on; `fast` turns
//     them off.
//   * `level` — the clang `-O` level applied to the generated `.ll` when it is
//     compiled to an object. This governs the program's own code, so the
//     numeric `-O` / `--opt=` flags mirror clang's levels directly.
//
// The named `--opt-mode=` presets tie the axes together the way zig's C code
// does (`safe` == `-O2`, `fast` == `-O3`, `small` == `-Oz`); the numeric form
// picks a clang level and the zig mode that suits it.
const OptLevel = enum {
    o0,
    o1,
    o2,
    o3,
    oz,

    fn clangFlag(self: OptLevel) []const u8 {
        return switch (self) {
            .o0 => "-O0",
            .o1 => "-O1",
            .o2 => "-O2",
            .o3 => "-O3",
            .oz => "-Oz",
        };
    }
};

const OptMode = enum {
    debug,
    safe,
    fast,
    small,

    // `zig build-exe` / `zig build-obj` optimization mode flag. Debug is the
    // default and passed explicitly for clarity.
    fn zigFlag(self: OptMode) []const u8 {
        return switch (self) {
            .debug => "-ODebug",
            .safe => "-OReleaseSafe",
            .fast => "-OReleaseFast",
            .small => "-OReleaseSmall",
        };
    }
};

const Opt = struct {
    mode: OptMode,
    level: OptLevel,

    // clang `cc` optimization flag for the `.ll` -> `.o` object step.
    fn clangFlag(self: Opt) []const u8 {
        return self.level.clangFlag();
    }

    // zig optimization mode flag, forwarded to the runtime/link steps.
    fn zigFlag(self: Opt) []const u8 {
        return self.mode.zigFlag();
    }

    fn fromMode(text: []const u8) ?Opt {
        return if (std.mem.eql(u8, text, "debug"))
            .{ .mode = .debug, .level = .o0 }
        else if (std.mem.eql(u8, text, "safe"))
            .{ .mode = .safe, .level = .o2 }
        else if (std.mem.eql(u8, text, "fast"))
            .{ .mode = .fast, .level = .o3 }
        else if (std.mem.eql(u8, text, "small"))
            .{ .mode = .small, .level = .oz }
        else
            null;
    }

    // Numeric levels mirror clang: `-O2` is clang `-O2`, matching a C build
    // with `zig cc -O2`. The runtime and link step switch to the unchecked
    // `fast` mode at `-O2` and up so a release Doxa binary stays comparable to
    // its C counterpart; `-O1` is a checked, lightly-optimized release.
    fn fromLevel(level: i32) Opt {
        return if (level <= 0)
            .{ .mode = .debug, .level = .o0 }
        else switch (level) {
            1 => .{ .mode = .safe, .level = .o1 },
            2 => .{ .mode = .fast, .level = .o2 },
            else => .{ .mode = .fast, .level = .o3 }, // 3+
        };
    }
};

const EmitKind = enum { opt_ir, asm_ };

// Run the vendored `zig cc` over the already-emitted `<stem>.ll` to produce an
// inspection artifact (optimized LLVM IR or target assembly) at the same
// optimization level the object build uses. Non-fatal: a failure here warns but
// does not abort the compile.
fn emitInspectionArtifact(zig_exe_path: []const u8, cli_options: *const CLI, stem: []const u8, kind: EmitKind, target: TargetTriple) !void {
    var args = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
    defer args.deinit();

    const in_name = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.ll", .{stem});
    defer std.heap.page_allocator.free(in_name);
    const out_ext = switch (kind) {
        .opt_ir => "opt.ll",
        .asm_ => "s",
    };
    const out_name = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.{s}", .{ stem, out_ext });
    defer std.heap.page_allocator.free(out_name);

    try args.appendSlice(&[_][]const u8{ zig_exe_path, "cc", "-Wno-override-module", "-Wno-unused-command-line-argument", "-S" });
    if (kind == .opt_ir) try args.append("-emit-llvm");
    try args.appendSlice(&[_][]const u8{ in_name, "-o", out_name });

    try target.appendTargetArg(&args);
    try args.append(cli_options.opt.clangFlag());

    var child = std.process.Child.init(args.items, std.heap.page_allocator);
    child.cwd = cli_options.cache_dir;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    const term = try child.spawnAndWait();
    switch (term) {
        .Exited => |code| if (code != 0) {
            std.debug.print("Warning: could not emit {s} (zig cc exited {d})\n", .{ out_name, code });
            return;
        },
        else => {
            std.debug.print("Warning: could not emit {s}\n", .{out_name});
            return;
        },
    }
    std.debug.print("Wrote {s}/{s}\n", .{ cli_options.cache_dir, out_name });
}

fn compileToNative(
    allocator: std.mem.Allocator,
    memoryManager: *MemoryManager,
    cli_options: *const CLI,
    parsed_statements: []AST.Stmt,
    parser: *Parser,
    reporter: *Reporter,
    semantic_analyzer: *SemanticAnalyzer,
    hir_program: *const HIRProgram,
    exe_path: []const u8,
    target: TargetTriple,
) !void {
    const zig_exe_path = try resolveBundledZigExecutable(allocator);
    defer allocator.free(zig_exe_path);

    std.fs.cwd().makeDir(cli_options.cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    if (std.fs.path.dirname(exe_path)) |dir| {
        try std.fs.cwd().makePath(dir);
    }

    const stem_for_derivatives = blk: {
        const filename = std.fs.path.basename(exe_path);
        if (target.isWindows() and std.mem.endsWith(u8, filename, ".exe"))
            break :blk filename[0 .. filename.len - 4];
        break :blk filename;
    };

    var ir_path_buf: [512]u8 = undefined;
    const ir_path = try std.fmt.bufPrint(&ir_path_buf, "{s}/{s}.ll", .{ cli_options.cache_dir, stem_for_derivatives });
    {
        var zig_fn_param_types = std.StringHashMap([]HIRType).init(memoryManager.getExecutionAllocator());
        if (parser.imported_symbols) |imported_symbols| {
            var it = imported_symbols.iterator();
            while (it.next()) |entry| {
                const sym = entry.value_ptr.*;
                if (sym.kind != .Function) continue;
                if (sym.param_types) |pt| {
                    const hir_params = try memoryManager.getExecutionAllocator().alloc(HIRType, pt.len);
                    for (pt, 0..) |ti, i| {
                        hir_params[i] = switch (ti.base) {
                            .Int => HIRType.Int,
                            .Float => HIRType.Float,
                            .Byte => HIRType.Byte,
                            .Tetra => HIRType.Tetra,
                            .Nothing => HIRType.Nothing,
                            .String => HIRType.String,
                            else => HIRType.Nothing,
                        };
                    }
                    const key = try memoryManager.getExecutionAllocator().dupe(u8, entry.key_ptr.*);
                    try zig_fn_param_types.put(key, hir_params);
                }
            }
        }
        var printer = @import("./codegen/llvmir/ir_printer.zig").IRPrinter.init(memoryManager.getExecutionAllocator(), @ptrFromInt(@intFromPtr(semantic_analyzer.getGroupTable())), @ptrFromInt(@intFromPtr(semantic_analyzer.getEnumTable())), zig_fn_param_types);
        try printer.emitToFile(hir_program, ir_path);
    }

    var obj_path_buf: [512]u8 = undefined;
    const obj_path = try std.fmt.bufPrint(&obj_path_buf, "{s}/{s}.o", .{ cli_options.cache_dir, stem_for_derivatives });
    {
        var args = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer args.deinit();
        const ir_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.ll", .{stem_for_derivatives});
        defer std.heap.page_allocator.free(ir_filename);
        const obj_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.o", .{stem_for_derivatives});
        defer std.heap.page_allocator.free(obj_filename);
        try args.appendSlice(&[_][]const u8{ zig_exe_path, "cc", "-Wno-override-module", "-Wno-unused-command-line-argument", "-c", ir_filename, "-o", obj_filename });
        try target.appendTargetArg(&args);
        try args.append(cli_options.opt.clangFlag());
        var include_flags = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer {
            for (include_flags.items) |f| std.heap.page_allocator.free(f);
            include_flags.deinit();
        }
        for (cli_options.include_dirs.items) |dir| {
            const flag = try std.fmt.allocPrint(std.heap.page_allocator, "-I{s}", .{dir});
            try include_flags.append(flag);
            try args.append(flag);
        }
        var child = std.process.Child.init(args.items, std.heap.page_allocator);
        child.cwd = cli_options.cache_dir;
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;
        const term = try child.spawnAndWait();
        switch (term) {
            .Exited => |code| if (code != 0) return error.Unexpected,
            else => return error.Unexpected,
        }
    }

    if (cli_options.emit_opt_ir) try emitInspectionArtifact(zig_exe_path, cli_options, stem_for_derivatives, .opt_ir, target);
    if (cli_options.emit_asm) try emitInspectionArtifact(zig_exe_path, cli_options, stem_for_derivatives, .asm_, target);

    const inline_zig_wrapper_paths = try compileInlineZigObjects(memoryManager, parsed_statements, parser, reporter, cli_options.cache_dir, cli_options.opt.zigFlag(), target, cli_options.include_dirs.items);
    defer {
        for (inline_zig_wrapper_paths) |p| memoryManager.getAllocator().free(@constCast(p));
        memoryManager.getAllocator().free(inline_zig_wrapper_paths);
    }

    const root_path = try prepareZigRoot(
        memoryManager.getAllocator(),
        cli_options.cache_dir,
    );
    defer memoryManager.getAllocator().free(root_path);

    {
        var args_ln = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer args_ln.deinit();
        try args_ln.append(zig_exe_path);
        try args_ln.append("build-exe");
        try args_ln.append(root_path);
        try args_ln.append(obj_path);
        for (inline_zig_wrapper_paths) |p| {
            try args_ln.append(p);
        }
        const emit_flag = try std.fmt.allocPrint(std.heap.page_allocator, "-femit-bin={s}", .{exe_path});
        defer std.heap.page_allocator.free(emit_flag);
        try args_ln.append(emit_flag);
        try target.appendTargetArg(&args_ln);
        try args_ln.append(cli_options.opt.zigFlag());
        try args_ln.append("-lc");
        if (target.isWindows() and inline_zig_wrapper_paths.len > 0) {
            try args_ln.appendSlice(&.{ "-lws2_32", "-lcrypt32" });
        }

        var manifest_link_flags = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer {
            for (manifest_link_flags.items) |f| std.heap.page_allocator.free(f);
            manifest_link_flags.deinit();
        }
        for (cli_options.lib_dirs.items) |dir| {
            const flag = try std.fmt.allocPrint(std.heap.page_allocator, "-L{s}", .{dir});
            try manifest_link_flags.append(flag);
            try args_ln.append(flag);
        }
        for (cli_options.link_libs.items) |lib| {
            const flag = try std.fmt.allocPrint(std.heap.page_allocator, "-l{s}", .{lib});
            try manifest_link_flags.append(flag);
            try args_ln.append(flag);
        }
        for (cli_options.frameworks.items) |fw| {
            try args_ln.append("-framework");
            try args_ln.append(fw);
        }

        var child_ln = std.process.Child.init(args_ln.items, std.heap.page_allocator);
        child_ln.cwd = ".";
        child_ln.stdout_behavior = .Inherit;
        child_ln.stderr_behavior = .Inherit;
        const term2 = try child_ln.spawnAndWait();
        switch (term2) {
            .Exited => |code| if (code != 0) {
                std.debug.print("link failed\n", .{});
                return error.Unexpected;
            },
            else => {
                std.debug.print("link failed\n", .{});
                return error.Unexpected;
            },
        }
    }

    const pdb_filename = try std.fmt.allocPrint(std.heap.page_allocator, "{s}.pdb", .{stem_for_derivatives});
    defer std.heap.page_allocator.free(pdb_filename);
    const pdb_out_path = try std.fmt.allocPrint(std.heap.page_allocator, "{s}/{s}.pdb", .{ cli_options.cache_dir, stem_for_derivatives });
    defer std.heap.page_allocator.free(pdb_out_path);

    std.fs.cwd().rename(pdb_filename, pdb_out_path) catch |err| switch (err) {
        error.FileNotFound => {},
        else => {},
    };
}

// Spawn the native binary produced for `doxa run`. `program_args[0]` is the
// source path; skip it so the process argv[0] is the exe.
fn runNativeExecutable(exe_path: []const u8, program_args: []const []const u8) !u8 {
    var args = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
    defer args.deinit();
    try args.append(exe_path);
    if (program_args.len > 1) try args.appendSlice(program_args[1..]);

    // Point the compiled program at the real `doxa` executable so the std/build
    // library's `compileArtifact` re-invokes the compiler instead of recursing
    // into the compiled binary itself (`selfExePath` would resolve to it).
    var env = std.process.getEnvMap(std.heap.page_allocator) catch null;
    defer if (env) |*e| e.deinit();
    if (env) |*e| {
        if (std.fs.selfExePathAlloc(std.heap.page_allocator)) |compiler_path| {
            defer std.heap.page_allocator.free(compiler_path);
            e.put("DOXA_BIN", compiler_path) catch {};
        } else |_| {}
    }

    var child = std.process.Child.init(args.items, std.heap.page_allocator);
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;
    child.env_map = if (env) |*e| e else null;
    const term = try child.spawnAndWait();
    return switch (term) {
        .Exited => |code| code,
        else => error.Unexpected,
    };
}

fn parseArgs(allocator: std.mem.Allocator) !CLI {
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Error: No arguments provided\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    var options = CLI{
        .reporter_options = .{},
        .mode = .UNDEFINED,
        .script_path = null,
        .profile = false,
        .output_path = null,
        .cache_dir = DEFAULT_CACHE_DIR,
        .target_arch = null,
        .target_os = null,
        .target_abi = null,
        .opt = Opt.fromLevel(0),
        .link_libs = std.array_list.Managed([]const u8).init(allocator),
        .lib_dirs = std.array_list.Managed([]const u8).init(allocator),
        .frameworks = std.array_list.Managed([]const u8).init(allocator),
        .include_dirs = std.array_list.Managed([]const u8).init(allocator),
        .emit_opt_ir = false,
        .emit_asm = false,
        .lsp_mode = .none,
        .lsp_debug_file = null,
        .lsp_io_trace = false,
        .program_args = &[_][]const u8{},
    };

    if (stringEquals(args[1], "init")) {
        const project_name: ?[]const u8 = if (args.len > 2) blk: {
            if (args.len > 3) {
                std.debug.print("Error: `doxa init` takes at most one project name\n", .{});
                printUsage();
                std.process.exit(EXIT_CODE_USAGE);
            }
            break :blk args[2];
        } else null;
        runInit(project_name) catch |err| {
            std.debug.print("Error: could not initialize project: {s}\n", .{@errorName(err)});
            std.process.exit(EXIT_CODE_USAGE);
        };
        std.process.exit(0);
    }

    if (stringEquals(args[1], "--lsp")) {
        options.lsp_mode = .stdio;
        if (args.len > 2) {
            for (args[2..]) |arg| {
                if (stringEquals(arg, "--lsp-debug-io")) {
                    options.lsp_io_trace = true;
                } else {
                    std.debug.print("Error: Unknown flag for --lsp: '{s}'\n", .{arg});
                    printUsage();
                    std.process.exit(EXIT_CODE_USAGE);
                }
            }
        }
        return options;
    } else if (stringEquals(args[1], "--lsp-debug")) {
        if (args.len < 3) {
            std.debug.print("Error: Provide a .doxa file for --lsp-debug\n", .{});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        } else if (args.len > 3) {
            std.debug.print("Error: Unexpected extra arguments for --lsp-debug\n", .{});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
        options.lsp_mode = .harness;
        if (!stringEndsWith(args[2], DOXA_EXTENSION)) {
            std.debug.print("Error: '{s}' is not a .doxa file\n", .{args[2]});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
        options.lsp_debug_file = try allocator.dupe(u8, args[2]);
        return options;
    }

    if (stringEquals(args[1], "run")) {
        options.mode = .RUN;
    } else if (stringEquals(args[1], "compile")) {
        options.mode = .COMPILE;
    } else {
        std.debug.print("Error: specify `run` or `compile`\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    const raw_options = args[2..];
    var option_end = raw_options.len;
    var split_seen = false;
    for (raw_options, 0..) |arg, idx| {
        if (stringEquals(arg, "--")) {
            option_end = idx;
            split_seen = true;
            break;
        }
    }

    if (split_seen and options.mode == .COMPILE) {
        std.debug.print("Error: `--` program arguments are only valid with `doxa run`\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    const option_args = raw_options[0..option_end];
    const run_program_args = if (split_seen and options.mode == .RUN)
        raw_options[option_end + 1 ..]
    else
        raw_options[0..0];

    var options_list = std.array_list.Managed([]const u8).init(allocator);
    defer options_list.deinit();

    for (option_args) |arg| {
        try options_list.append(arg);
    }

    var expecting_output: bool = false;
    for (options_list.items) |arg| {
        if (expecting_output) {
            options.output_path = try allocator.dupe(u8, arg);
            expecting_output = false;
            continue;
        }
        if (stringEquals(arg, "--debug-verbose")) {
            options.reporter_options.debug_verbose = true;
            continue;
        } else if (stringEquals(arg, "--debug-lexer")) {
            options.reporter_options.debug_lexer = true;
            continue;
        } else if (stringEquals(arg, "--debug-parser")) {
            options.reporter_options.debug_parser = true;
            continue;
        } else if (stringEquals(arg, "--debug-semantic")) {
            options.reporter_options.debug_semantic = true;
            continue;
        } else if (stringEquals(arg, "--debug-hir")) {
            options.reporter_options.debug_hir = true;
            continue;
        } else if (stringEquals(arg, "--debug-memory")) {
            options.reporter_options.debug_memory = true;
            continue;
        } else if (stringEquals(arg, "--profile")) {
            options.profile = true;
            continue;
        } else if (stringEquals(arg, "-o") or stringEquals(arg, "--output")) {
            expecting_output = true;
            continue;
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            options.cache_dir = try allocator.dupe(u8, arg["--cache-dir=".len..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--arch=")) {
            options.target_arch = try allocator.dupe(u8, arg[7..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--os=")) {
            options.target_os = try allocator.dupe(u8, arg[5..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--abi=")) {
            options.target_abi = try allocator.dupe(u8, arg[6..]);
            continue;
        } else if (std.mem.startsWith(u8, arg, "--link=")) {
            try options.link_libs.append(try allocator.dupe(u8, arg["--link=".len..]));
            continue;
        } else if (std.mem.startsWith(u8, arg, "--libdir=")) {
            try options.lib_dirs.append(try allocator.dupe(u8, arg["--libdir=".len..]));
            continue;
        } else if (std.mem.startsWith(u8, arg, "--framework=")) {
            try options.frameworks.append(try allocator.dupe(u8, arg["--framework=".len..]));
            continue;
        } else if (std.mem.startsWith(u8, arg, "--include=")) {
            try options.include_dirs.append(try allocator.dupe(u8, arg["--include=".len..]));
            continue;
        } else if (std.mem.startsWith(u8, arg, "--opt-mode=")) {
            options.opt = Opt.fromMode(arg["--opt-mode=".len..]) orelse {
                std.debug.print("Error: invalid --opt-mode (expected debug|safe|fast|small): {s}\n", .{arg});
                std.process.exit(EXIT_CODE_USAGE);
            };
            continue;
        } else if (std.mem.startsWith(u8, arg, "--opt=")) {
            const level = std.fmt.parseInt(i32, arg[6..], 10) catch {
                std.debug.print("Error: invalid --opt level: {s}\n", .{arg});
                std.process.exit(EXIT_CODE_USAGE);
            };
            options.opt = Opt.fromLevel(level);
            continue;
        } else if (arg.len >= 3 and arg[0] == '-' and arg[1] == 'O') {
            const level = std.fmt.parseInt(i32, arg[2..], 10) catch {
                std.debug.print("Error: invalid optimization flag: {s}\n", .{arg});
                std.process.exit(EXIT_CODE_USAGE);
            };
            options.opt = Opt.fromLevel(level);
            continue;
        } else if (stringEquals(arg, "--emit-opt-ir")) {
            options.emit_opt_ir = true;
            continue;
        } else if (stringEquals(arg, "--emit-asm")) {
            options.emit_asm = true;
            continue;
        } else if (stringEquals(arg, "--help") or stringEquals(arg, "-h")) {
            printUsage();
            std.process.exit(0);
        } else if (stringEndsWith(arg, DOXA_EXTENSION)) {
            options.script_path = try allocator.dupe(u8, arg);
            continue;
        } else {
            std.debug.print("Error: Unknown command or invalid file: '{s}'\n", .{arg});
            printUsage();
            std.process.exit(EXIT_CODE_USAGE);
        }
    }

    if (options.script_path == null) {
        std.debug.print("Error: No file specified\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    if (options.mode == .COMPILE and options.output_path == null) {
        std.debug.print("Error: compile mode requires -o/--output <path>\n", .{});
        printUsage();
        std.process.exit(EXIT_CODE_USAGE);
    }

    if (options.mode == .RUN) {
        const script = options.script_path orelse unreachable;
        const total = 1 + run_program_args.len;
        const duplicated = try allocator.alloc([]const u8, total);
        errdefer allocator.free(duplicated);
        var copied: usize = 0;
        errdefer {
            var i: usize = 0;
            while (i < copied) : (i += 1) allocator.free(duplicated[i]);
        }
        duplicated[0] = try allocator.dupe(u8, script);
        copied += 1;
        for (run_program_args, 0..) |arg, idx| {
            duplicated[idx + 1] = try allocator.dupe(u8, arg);
            copied += 1;
        }
        options.program_args = duplicated;
    }

    return options;
}

fn runInit(project_name: ?[]const u8) !void {
    const cwd = std.fs.cwd();

    var dir = if (project_name) |name| dir: {
        cwd.makeDir(name) catch |err| switch (err) {
            error.PathAlreadyExists => {
                std.debug.print("Error: '{s}' already exists\n", .{name});
                std.process.exit(EXIT_CODE_USAGE);
            },
            else => return err,
        };
        break :dir try cwd.openDir(name, .{});
    } else dir: {
        if (!try isDirEmpty(cwd)) {
            if (!try promptYesNo("Current directory is not empty. Create project here?")) {
                std.debug.print("Aborted.\n", .{});
                std.process.exit(0);
            }
        }
        break :dir try cwd.openDir(".", .{});
    };
    defer if (project_name != null) dir.close();

    try dir.makePath("src");
    var src_dir = try dir.openDir("src", .{});
    defer src_dir.close();

    try src_dir.writeFile(.{ .sub_path = "main.doxa", .data = SCAFFOLD_MAIN });
    try dir.writeFile(.{ .sub_path = "build.doxa", .data = SCAFFOLD_BUILD });

    if (project_name) |name| {
        std.debug.print("Initialized Doxa project in {s}/\n", .{name});
    } else {
        std.debug.print("Initialized Doxa project in current directory\n", .{});
    }
}

// Hello-world entry the scaffold compiles with `doxa run build.doxa`.
const SCAFFOLD_MAIN =
    \\import io from @std()
    \\
    \\io.println("Hello, World!")
    \\
;

// Canonical build script shape (Context + Builder.executable + execute) with no
// external library dependencies, so the scaffold builds out of the box.
const SCAFFOLD_BUILD =
    \\import build from @std()
    \\
    \\var c is build.Context.host()
    \\c.debug is false
    \\c.optimization is build.Optimization.Speed
    \\
    \\var exe is build.Builder.executable("app", "src/main.doxa", "bin/app")
    \\
    \\c.addArtifact(exe)
    \\
    \\build.execute(c, false)
    \\
;

fn isDirEmpty(dir: std.fs.Dir) !bool {
    var iterable = try dir.openDir(".", .{ .iterate = true });
    defer iterable.close();
    var it = iterable.iterate();
    return (try it.next()) == null;
}

fn promptYesNo(question: []const u8) !bool {
    std.debug.print("{s} [Y/n] ", .{question});

    var stdin_buffer: [256]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buffer);
    const reader = &stdin_reader.interface;

    var line: [256]u8 = undefined;
    var len: usize = 0;
    while (len < line.len) {
        const byte = std.io.Reader.takeByte(reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (byte == '\n') break;
        line[len] = byte;
        len += 1;
    }

    const trimmed = std.mem.trim(u8, line[0..len], " \t\r");
    if (trimmed.len == 0) return true;
    return trimmed[0] == 'y' or trimmed[0] == 'Y';
}

fn printUsage() void {
    std.debug.print("Doxa Programming Language\n", .{});
    std.debug.print("\nUsage:\n", .{});
    std.debug.print("  doxa init [project-name]        # Scaffold a new Doxa project\n", .{});
    std.debug.print("  doxa run [general options] <file.doxa>\n", .{});
    std.debug.print("  doxa compile [general options] <file.doxa> -o <output> [compile options]\n", .{});
    std.debug.print("  doxa --lsp [--lsp-debug-io]     # Start the Language Server Protocol loop\n", .{});
    std.debug.print("  doxa --lsp-debug <file.doxa>    # Run the in-process LSP debug harness\n", .{});
    std.debug.print("\nGeneral options:\n", .{});
    std.debug.print("  --profile                         # Enable profiling\n", .{});
    std.debug.print("  --help, -h                        # Show this help message\n", .{});
    std.debug.print("  --debug-[stage]                   # Enable debug output for [stage]\n", .{});
    std.debug.print("                                    # lexer, parser, semantic, hir, memory\n", .{});
    std.debug.print("  --debug-verbose                   # Enable all debug output\n", .{});
    std.debug.print("  --cache-dir=<dir>                 # Build cache directory (default: .doxa-cache)\n", .{});
    std.debug.print("\nCompile options:\n", .{});
    std.debug.print("  -o, --output <path>               # Output executable path (required)\n", .{});
    std.debug.print("  --arch=<arch>                     # Target CPU architecture (default: host)\n", .{});
    std.debug.print("  --os=<os>                         # Target operating system (default: host)\n", .{});
    std.debug.print("  --abi=<abi>                       # Target ABI (optional)\n", .{});
    std.debug.print("  --link=<name>                     # Link a native library (-l<name>); repeatable\n", .{});
    std.debug.print("  --libdir=<dir>                    # Library search path (-L<dir>); repeatable\n", .{});
    std.debug.print("  --framework=<name>                # Link a macOS framework; repeatable\n", .{});
    std.debug.print("  --include=<dir>                   # Header search path (-I<dir>); repeatable\n", .{});
    std.debug.print("  --opt-mode=<debug|safe|fast|small># Zig release mode for the runtime and link step\n", .{});
    std.debug.print("  -O0..-O3 | --opt=0..3             # clang -O level for the program (-O2 == zig cc -O2)\n", .{});
    std.debug.print("  --emit-opt-ir                     # Also write optimized LLVM IR (<stem>.opt.ll) to cache\n", .{});
    std.debug.print("  --emit-asm                        # Also write target assembly (<stem>.s) to cache\n", .{});
    std.debug.print("  --lsp-debug-io                    # Trace raw LSP I/O when used with --lsp\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  doxa run file.doxa\n", .{});
    std.debug.print("  doxa compile file.doxa -o bin/myapp\n", .{});
    std.debug.print("  doxa compile file.doxa -o bin/myapp --arch=x86_64 --os=linux -O2\n", .{});
}

fn stringEquals(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, 0..) |char, i| {
        if (char != b[i]) return false;
    }
    return true;
}

fn stringEndsWith(str: []const u8, suffix: []const u8) bool {
    if (str.len < suffix.len) {
        return false;
    }
    const start = str.len - suffix.len;
    const result = stringEquals(str[start..], suffix);
    return result;
}

fn lexicAnalysis(memoryManager: *MemoryManager, source: []const u8, path: []const u8, reporter: *Reporter) !std.array_list.Managed(Token) {
    var lexer = try LexicalAnalyzer.init(memoryManager.getAnalysisAllocator(), source, path, reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    return tokens;
}

fn exitIfCompileErrors(reporter: *Reporter) void {
    if (reporter.hasCompileErrors()) {
        std.process.exit(EXIT_CODE_USAGE);
    }
}

fn isDoxaFile(path: []const u8, path_uri: []const u8, reporter: *Reporter) void {
    if (!stringEndsWith(path, DOXA_EXTENSION)) {
        const loc = Location{
            .file = path,
            .file_uri = path_uri,
            .range = .{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
            },
        };
        reporter.reportCompileError(loc, null, "Error: '{s}' is not a doxa file\n", .{path});
        std.process.exit(EXIT_CODE_USAGE);
    }
}

pub fn main() !void {
    if (builtin.os.tag == .windows) {
        // Set the console output code page to UTF-8 to enable Unicode support
        // I think this is only needed for Windows
        _ = std.os.windows.kernel32.SetConsoleOutputCP(65001);
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }

    var memoryManager = try MemoryManager.init(gpa.allocator());
    defer memoryManager.deinit();

    var sourceCache = source_cache.SourceCache.init(gpa.allocator());
    defer sourceCache.deinit();

    var reporter = Reporter.init(gpa.allocator(), .{}, &sourceCache);
    defer reporter.deinit();

    const cli_options = try parseArgs(gpa.allocator());
    defer cli_options.deinit(gpa.allocator());

    switch (cli_options.lsp_mode) {
        .none => {},
        .stdio => return LspServer.run(gpa.allocator(), .{
            .reporter_options = cli_options.reporter_options,
            .trace_io = cli_options.lsp_io_trace,
        }),
        .harness => return LspServer.runDebugHarness(gpa.allocator(), .{
            .reporter_options = cli_options.reporter_options,
            .script_path = cli_options.lsp_debug_file orelse unreachable,
        }),
    }

    const script_path = cli_options.script_path orelse unreachable;

    var profiler = Profiler.init(gpa.allocator(), cli_options.profile);
    defer profiler.deinit();

    const source = std.fs.cwd().readFileAlloc(memoryManager.getAnalysisAllocator(), script_path, MAX_FILE_SIZE) catch |err| {
        switch (err) {
            error.FileNotFound => std.debug.print("Error: could not find script '{s}' (looked relative to {s})\n", .{ script_path, std.fs.cwd().realpathAlloc(gpa.allocator(), ".") catch "the current directory" }),
            error.AccessDenied => std.debug.print("Error: permission denied reading script '{s}'\n", .{script_path}),
            error.IsDir => std.debug.print("Error: '{s}' is a directory, not a Doxa source file\n", .{script_path}),
            error.FileTooBig => std.debug.print("Error: script '{s}' exceeds the maximum size of {d} bytes\n", .{ script_path, MAX_FILE_SIZE }),
            else => std.debug.print("Error: could not read script '{s}': {s}\n", .{ script_path, @errorName(err) }),
        }
        std.process.exit(EXIT_CODE_USAGE);
    };
    defer memoryManager.getAnalysisAllocator().free(source);
    try sourceCache.load(script_path, source);
    isDoxaFile(script_path, try reporter.ensureFileUri(script_path), &reporter);

    try pipeline(gpa.allocator(), cli_options, script_path, &memoryManager, &reporter, &profiler, source);
}

fn pipeline(allocator: std.mem.Allocator, cli_options: CLI, script_path: []const u8, memoryManager: *MemoryManager, reporter: *Reporter, profiler: *Profiler, source: []const u8) !void {
    profiler.startPhase(Phase.LEXIC_A);
    const lexedTokens = try lexicAnalysis(memoryManager, source, script_path, reporter);
    defer lexedTokens.deinit();
    if (cli_options.reporter_options.debug_lexer) {
        for (lexedTokens.items) |token| {
            std.debug.print("{t} {s}\n", .{ token.type, token.lexeme });
        }
    }
    profiler.stopPhase();

    profiler.startPhase(Phase.PARSING);
    var parser = Parser.init(memoryManager.getAnalysisAllocator(), lexedTokens.items, script_path, try reporter.ensureFileUri(script_path), reporter);
    defer parser.deinit();
    const parsedStatements = try parser.execute();
    profiler.stopPhase();
    exitIfCompileErrors(reporter);

    if (cli_options.reporter_options.debug_parser) {
        var ast_dump = std.array_list.Managed(u8).init(memoryManager.getAnalysisAllocator());
        defer ast_dump.deinit();
        AST.dumpStatements(ast_dump.writer(), parsedStatements) catch {};
        reporter.report(.Debug, .Hint, null, "AST", "{s}", .{ast_dump.items});
    }

    profiler.startPhase(Phase.RESOLVING);
    var resolver = Resolver.init(memoryManager.getAnalysisAllocator(), &parser);
    defer resolver.deinit();
    try resolver.resolve(parsedStatements);
    profiler.stopPhase();
    exitIfCompileErrors(reporter);

    profiler.startPhase(Phase.SEMANTIC_A);
    var semantic_analyzer = SemanticAnalyzer.init(memoryManager.getAnalysisAllocator(), reporter, memoryManager, &parser);
    defer semantic_analyzer.deinit();
    try semantic_analyzer.analyze(parsedStatements);
    profiler.stopPhase();
    exitIfCompileErrors(reporter);

    if (cli_options.reporter_options.debug_memory) {
        memoryManager.dumpState(reporter);
    }

    profiler.startPhase(Phase.GENERATE_S);

    var reachable_modules = try parser.collectReachableModuleNamespaces(memoryManager.getAnalysisAllocator());
    defer reachable_modules.deinit();
    const hir_program = try generateHIRProgram(memoryManager, parsedStatements, reachable_modules, &parser, &semantic_analyzer, reporter);
    exitIfCompileErrors(reporter);
    profiler.stopPhase();

    profiler.startPhase(Phase.GENERATE_L);
    var target = TargetTriple.resolve(&cli_options, allocator) catch |err| switch (err) {
        error.MissingTargetOs => {
            std.debug.print("Error: cross-compiling requires --os=<os>; a target without an OS would silently fall back to the host\n", .{});
            std.process.exit(EXIT_CODE_USAGE);
        },
        error.MissingTargetArch => {
            std.debug.print("Error: cross-compiling requires --arch=<arch>; a target without an arch is not a valid triple\n", .{});
            std.process.exit(EXIT_CODE_USAGE);
        },
        else => return err,
    };
    defer target.deinit(allocator);
    const exe_path = try nativeOutputPath(allocator, &cli_options, script_path, target.isWindows());
    defer allocator.free(exe_path);
    try compileToNative(
        allocator,
        memoryManager,
        &cli_options,
        parsedStatements,
        &parser,
        reporter,
        &semantic_analyzer,
        &hir_program,
        exe_path,
        target,
    );
    profiler.stopPhase();

    if (cli_options.mode == .RUN) {
        profiler.startPhase(Phase.EXECUTION);
        const code = try runNativeExecutable(exe_path, cli_options.program_args);
        profiler.stopPhase();

        if (cli_options.reporter_options.debug_memory) {
            memoryManager.dumpState(reporter);
        }
        try profiler.dump();
        if (code != 0) std.process.exit(code);
        return;
    }

    try profiler.dump();
}
