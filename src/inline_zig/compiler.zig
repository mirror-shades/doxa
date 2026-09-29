const std = @import("std");

const ast = @import("../ast/ast.zig");
const Parser = @import("../parser/parser_types.zig").Parser;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const Profiler = @import("../utils/profiler.zig").Profiler;
const hashing = @import("../utils/hashing.zig");
const generator_source = @embedFile("compiler.zig");

fn cacheSeed() []const u8 {
    return generator_source;
}

const ZigDeclInfo = struct {
    module_name: []const u8,
    zig_source: []const u8,
    location: Reporting.Location,
    sigs: []ast.ZigFnSig,
};

const GeneratedModule = struct {
    zig_path: []const u8,

    pub fn deinit(self: *GeneratedModule, allocator: std.mem.Allocator) void {
        allocator.free(self.zig_path);
    }
};

fn appendZigSourceSanitized(buf: *std.array_list.Managed(u8), source: []const u8) !void {
    var i: usize = 0;
    while (i < source.len) {
        if (std.mem.startsWith(u8, source[i..], "public ")) {
            i += "public ".len;
            continue;
        }
        if (std.mem.startsWith(u8, source[i..], "export ") and
            (i + "export ".len < source.len) and
            std.mem.startsWith(u8, source[i + "export ".len ..], "fn "))
        {
            i += "export ".len;
            continue;
        }
        try buf.append(source[i]);
        i += 1;
    }
}

pub fn collectInlineZigDecls(
    allocator: std.mem.Allocator,
    statements: []ast.Stmt,
    parser: *Parser,
) ![]ZigDeclInfo {
    var seen = std.StringHashMap(void).init(allocator);
    defer seen.deinit();

    var out = std.array_list.Managed(ZigDeclInfo).init(allocator);
    errdefer out.deinit();

    for (statements) |s| {
        if (s.data != .ZigDecl) continue;
        const decl = s.data.ZigDecl;
        if (seen.contains(decl.name.lexeme)) continue;
        try seen.put(decl.name.lexeme, {});
        try out.append(.{
            .module_name = decl.name.lexeme,
            .zig_source = decl.source,
            .location = s.base.location(),
            .sigs = decl.sigs,
        });
    }

    var module_it = parser.module_namespaces.iterator();
    while (module_it.next()) |entry| {
        const module_info = entry.value_ptr.*;
        if (module_info.ast) |module_ast| {
            if (module_ast.data != .Block) continue;
            for (module_ast.data.Block.statements) |s| {
                if (s.data != .ZigDecl) continue;
                const decl = s.data.ZigDecl;
                if (seen.contains(decl.name.lexeme)) continue;
                try seen.put(decl.name.lexeme, {});
                try out.append(.{
                    .module_name = decl.name.lexeme,
                    .zig_source = decl.source,
                    .location = s.base.location(),
                    .sigs = decl.sigs,
                });
            }
        }
    }

    return try out.toOwnedSlice();
}

fn generateWrapperZigFile(
    io: std.Io,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    cache_dir: []const u8,
    decl: ZigDeclInfo,
) !GeneratedModule {
    const Sha256 = std.crypto.hash.sha2.Sha256;

    const sigs = decl.sigs;

    var h: [Sha256.digest_length]u8 = undefined;
    var hasher = Sha256.init(.{});
    hasher.update(cacheSeed());
    hasher.update("\n");
    hasher.update(decl.module_name);
    hasher.update("\n");
    hasher.update(decl.zig_source);
    hasher.final(&h);

    const hex_buf = std.fmt.bytesToHex(h, .lower);
    const short_hex = hex_buf[0..16];

    var zig_path_buf: [256]u8 = undefined;
    const zig_path = try std.fmt.bufPrint(&zig_path_buf, "{s}/{s}-{s}.zig", .{ cache_dir, decl.module_name, short_hex });

    var file_buf = std.array_list.Managed(u8).init(allocator);
    defer file_buf.deinit();
    try appendZigSourceSanitized(&file_buf, decl.zig_source);
    try file_buf.appendSlice("\n\n");

    const zigTypeName = struct {
        const TypeMeta = struct {
            native_param: ?[]const u8,
            native_ret: ?[]const u8,
        };

        fn metaFor(t: ast.TypeInfo) TypeMeta {
            return switch (t.base) {
                .Int => .{ .native_param = "i64", .native_ret = "i64" },
                .Float => .{ .native_param = "f64", .native_ret = "f64" },
                .Byte => .{ .native_param = "u8", .native_ret = "u8" },
                .Tetra => .{ .native_param = "bool", .native_ret = "bool" },
                .Nothing => .{ .native_param = "void", .native_ret = "void" },
                .String => .{ .native_param = "?[*]const u8", .native_ret = "void" },
                else => .{ .native_param = null, .native_ret = null },
            };
        }

        fn fromNativeParamTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return metaFor(t).native_param;
        }

        fn fromNativeReturnTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return metaFor(t).native_ret;
        }
    };

    // String returns cross the boundary already owned by the call site's scope
    // arena (the runtime clones on the wrapper's behalf), so the arena rules in
    // docs/memory.md apply and no separate free hook exists.
    try file_buf.appendSlice("extern fn doxa_str_clone_current(ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void;\n\n");

    for (sigs) |sig| {
        const native_ident = try std.fmt.allocPrint(allocator, "__doxa_native__{s}_{s}", .{ decl.module_name, sig.name });
        defer allocator.free(native_ident);
        const native_sym = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ decl.module_name, sig.name });
        defer allocator.free(native_sym);

        const native_ret_zig = zigTypeName.fromNativeReturnTypeInfo(sig.return_type) orelse {
            reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in native bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
            return error.NotImplemented;
        };

        var native_buf = std.array_list.Managed(u8).init(allocator);
        defer native_buf.deinit();
        try native_buf.appendSlice("pub fn ");
        try native_buf.appendSlice(native_ident);
        try native_buf.appendSlice("(");

        var native_prelude = std.array_list.Managed(u8).init(allocator);
        defer native_prelude.deinit();
        var native_call = std.array_list.Managed(u8).init(allocator);
        defer native_call.deinit();
        try native_call.appendSlice(sig.name);
        try native_call.appendSlice("(");

        for (sig.param_types, 0..) |pt, i| {
            const pt_zig = zigTypeName.fromNativeParamTypeInfo(pt) orelse {
                reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type in native bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
                return error.NotImplemented;
            };
            if (i > 0) {
                try native_buf.appendSlice(", ");
                try native_call.appendSlice(", ");
            }
            const arg_name = try std.fmt.allocPrint(allocator, "a{}", .{i});
            defer allocator.free(arg_name);
            try native_buf.appendSlice(arg_name);
            try native_buf.appendSlice(": ");
            try native_buf.appendSlice(pt_zig);
            if (pt.base == .String) {
                const s_name = try std.fmt.allocPrint(allocator, "__doxa_s{}", .{i});
                defer allocator.free(s_name);
                try native_prelude.appendSlice("    const ");
                try native_prelude.appendSlice(s_name);
                try native_prelude.appendSlice(": []const u8 = if (");
                try native_prelude.appendSlice(arg_name);
                try native_prelude.appendSlice(") |p| p[0..@intCast(");
                try native_prelude.appendSlice(arg_name);
                try native_prelude.appendSlice("_len)] else \"\";\n");
                try native_call.appendSlice(s_name);

                try native_buf.appendSlice(", ");
                try native_buf.appendSlice(arg_name);
                try native_buf.appendSlice("_len: u64");
            } else {
                try native_call.appendSlice(arg_name);
            }
        }

        if (sig.return_type.base == .String) {
            if (sig.param_types.len > 0) try native_buf.appendSlice(", ");
            try native_buf.appendSlice("out_ptr: *?[*]u8, out_len: *u64");
        }
        try native_buf.appendSlice(") callconv(.c) ");
        try native_buf.appendSlice(native_ret_zig);
        try native_buf.appendSlice(" {\n");
        if (native_prelude.items.len > 0) try native_buf.appendSlice(native_prelude.items);
        try native_call.appendSlice(")");

        if (sig.return_type.base == .String) {
            try native_buf.appendSlice("    const __doxa_out = ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
            try native_buf.appendSlice("    if (__doxa_out.len == 0) { out_ptr.* = null; out_len.* = 0; return; }\n");
            try native_buf.appendSlice("    doxa_str_clone_current(__doxa_out.ptr, __doxa_out.len, out_ptr, out_len);\n");
        } else if (std.mem.eql(u8, native_ret_zig, "void")) {
            try native_buf.appendSlice("    ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
        } else {
            try native_buf.appendSlice("    return ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
        }
        try native_buf.appendSlice("}\n");
        try native_buf.appendSlice("comptime { @export(&");
        try native_buf.appendSlice(native_ident);
        try native_buf.appendSlice(", .{ .name = \"");
        try native_buf.appendSlice(native_sym);
        try native_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(native_buf.items);
    }

    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = zig_path, .data = file_buf.items });

    return .{
        .zig_path = try allocator.dupe(u8, zig_path),
    };
}

pub fn compileInlineZigObjects(
    io: std.Io,
    memoryManager: *MemoryManager,
    statements: []ast.Stmt,
    parser: *Parser,
    reporter: *Reporter,
    zig_exe_path: []const u8,
    cache_dir: []const u8,
    zig_opt_flag: []const u8,
    target_triple: []const u8,
    target_os: []const u8,
    cpu_arg: []const u8,
    include_dirs: []const []const u8,
    toolchain: []const u8,
    profiler: *Profiler,
) ![]const []const u8 {
    const zig_decls = try collectInlineZigDecls(memoryManager.getAllocator(), statements, parser);
    defer memoryManager.getAllocator().free(zig_decls);

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{cache_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.Io.Dir.cwd().createDirPath(io, zig_cache_path);

    var out_paths = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
    errdefer {
        for (out_paths.items) |p| memoryManager.getAllocator().free(@constCast(p));
        out_paths.deinit();
    }

    for (zig_decls) |decl| {
        profiler.begin("wrapper-gen");
        var gen = try generateWrapperZigFile(io, memoryManager.getAllocator(), reporter, zig_cache_path, decl);
        profiler.end();
        defer gen.deinit(memoryManager.getAllocator());

        // The object extension follows the target, not the host.
        const obj_ext = if (std.mem.eql(u8, target_os, "windows")) "obj" else "o";
        const zig_dir = std.fs.path.dirname(gen.zig_path) orelse ".";
        const zig_stem = std.fs.path.stem(gen.zig_path);

        // Content-addressed cache key: wrapper source (already hashed into the
        // wrapper path) + toolchain + target triple + cpu + opt mode. Distinct
        // targets, cpus, opt modes, or compilers therefore produce distinct
        // objects, and unchanged source+target skips the `build-obj` spawn.
        var kb = hashing.KeyBuilder.init(cacheSeed());
        kb.addBytes(toolchain);
        kb.addBytes(gen.zig_path);
        kb.addBytes(target_triple);
        kb.addBytes(cpu_arg);
        kb.addBytes(zig_opt_flag);
        for (include_dirs) |dir| {
            kb.addBytes(dir);
        }
        const key_hex = hashing.shortHexOf(kb.finish());

        var obj_path_buf: [320]u8 = undefined;
        const obj_path = try std.fmt.bufPrint(&obj_path_buf, "{s}/{s}.{s}.{s}", .{ zig_dir, zig_stem, key_hex[0..], obj_ext });

        const obj_already_compiled = blk: {
            const f = std.Io.Dir.cwd().openFile(io, obj_path, .{}) catch break :blk false;
            defer f.close(io);
            const stat = f.stat(io) catch break :blk false;
            break :blk stat.kind == .file and stat.size > 0;
        };

        if (!obj_already_compiled) {
            profiler.begin("build-obj");
            defer profiler.end();
            const emit_flag = try std.fmt.allocPrint(std.heap.page_allocator, "-femit-bin={s}", .{obj_path});
            defer std.heap.page_allocator.free(emit_flag);
            var args_list = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
            defer args_list.deinit();
            try args_list.appendSlice(&[_][]const u8{
                zig_exe_path,
                "build-obj",
                gen.zig_path,
                emit_flag,
                zig_opt_flag,
            });
            if (target_triple.len > 0) {
                try args_list.append("-target");
                try args_list.append(target_triple);
            }
            if (cpu_arg.len > 0) {
                try args_list.append(cpu_arg);
            }

            var dir_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer dir_arena.deinit();
            // The build module's `--include` dirs apply to every module's
            // @cImport, matching how the artifact's `zig cc` step consumes them.
            for (include_dirs) |dir| {
                try args_list.append(try std.fmt.allocPrint(dir_arena.allocator(), "-I{s}", .{dir}));
            }
            // Every object linked into a Doxa program must be compiled with the
            // same libc-ness as the runtime root object (`src/main.zig`, which
            // passes `-lc`) and the final link (also `-lc`). Without this, an
            // inline shim's `builtin.link_libc` is false while the program still
            // links libc: `std.Thread` then selects `LinuxThreadImpl` and reads
            // the uninitialized `std.os.linux.tls.area_desc` under glibc's
            // startup, aborting on an invalid-alignment assertion. See
            // `plan/libc-dependence.md`.
            try args_list.append("-lc");

            var child = try std.process.spawn(io, .{
                .argv = args_list.items,
                .cwd = .{ .path = "." },
                .stdout = .inherit,
                .stderr = .inherit,
            });
            const term = try child.wait(io);
            switch (term) {
                .exited => |code| if (code != 0) return error.Unexpected,
                else => return error.Unexpected,
            }
        }

        try out_paths.append(try memoryManager.getAllocator().dupe(u8, obj_path));
    }

    return try out_paths.toOwnedSlice();
}
