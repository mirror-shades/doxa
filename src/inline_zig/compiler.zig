const std = @import("std");
const builtin = @import("builtin");

const ast = @import("../ast/ast.zig");
const Parser = @import("../parser/parser_types.zig").Parser;
const inline_zig = @import("../parser/inline_zig.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const VM = @import("../interpreter/vm.zig").VM;
const bc = @import("../codegen/bytecode/module.zig");

const ZigDeclInfo = struct {
    module_name: []const u8,
    zig_source: []const u8,
    location: Reporting.Location,
};

fn collectInlineZigDecls(
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
        });
    }

    var cache_it = parser.module_cache.iterator();
    while (cache_it.next()) |entry| {
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
                });
            }
        }
    }

    return try out.toOwnedSlice();
}

fn compileOneInlineModule(
    memoryManager: *MemoryManager,
    reporter: *Reporter,
    output_dir: []const u8,
    decl: ZigDeclInfo,
    modules: *std.StringHashMap(VM.ZigRuntimeModule),
) !void {
    const Sha256 = std.crypto.hash.sha2.Sha256;

    const sigs = try inline_zig.sanitizeAndExtract(memoryManager.getAllocator(), decl.zig_source);
    defer inline_zig.deinitSigs(memoryManager.getAllocator(), sigs);

    var h: [Sha256.digest_length]u8 = undefined;
    var hasher = Sha256.init(.{});
    hasher.update("__doxa_inlinezig_v2__\n");
    hasher.update(decl.module_name);
    hasher.update("\n");
    hasher.update(decl.zig_source);
    hasher.final(&h);

    const hex_buf = std.fmt.bytesToHex(h, .lower);
    const short_hex = hex_buf[0..16];

    var zig_path_buf: [512]u8 = undefined;
    const zig_path = try std.fmt.bufPrint(&zig_path_buf, "{s}/zig/cache/{s}-{s}.zig", .{ output_dir, decl.module_name, short_hex });

    var lib_path_buf: [512]u8 = undefined;
    const lib_ext = switch (builtin.os.tag) {
        .windows => "dll",
        .macos => "dylib",
        else => "so",
    };
    const lib_path = try std.fmt.bufPrint(&lib_path_buf, "{s}/zig/cache/{s}-{s}.{s}", .{ output_dir, decl.module_name, short_hex, lib_ext });

    var functions = std.StringHashMap(VM.ZigRuntimeFn).init(memoryManager.getAllocator());
    errdefer {
        var itf = functions.iterator();
        while (itf.next()) |entry| {
            const k = entry.key_ptr.*;
            const v = entry.value_ptr.*;
            memoryManager.getAllocator().free(@constCast(k));
            memoryManager.getAllocator().free(@constCast(v.symbol));
            memoryManager.getAllocator().free(v.param_types);
        }
        functions.deinit();
    }

    var file_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
    defer file_buf.deinit();
    try file_buf.appendSlice(decl.zig_source);
    try file_buf.appendSlice("\n\n");

    const zigTypeName = struct {
        fn fromParamTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return switch (t.base) {
                .Int => "i64",
                .Float => "f64",
                .Byte => "u8",
                .Tetra => "bool",
                .Nothing => "void",
                .String => "?[*:0]const u8",
                else => null,
            };
        }

        fn toBytecodeType(t: ast.TypeInfo) bc.BytecodeType {
            return switch (t.base) {
                .Int => .Int,
                .Float => .Float,
                .Byte => .Byte,
                .Tetra => .Tetra,
                .Nothing => .Nothing,
                .String => .String,
                else => .Nothing,
            };
        }

        fn fromReturnTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return switch (t.base) {
                .Int => "i64",
                .Float => "f64",
                .Byte => "u8",
                .Tetra => "bool",
                .Nothing => "void",
                .String => "?[*:0]u8",
                else => null,
            };
        }
    };

    try file_buf.appendSlice("const __doxa_std = @import(\"std\");\n\n");
    try file_buf.appendSlice("fn __doxa_to_cstr(s: []const u8) ?[*:0]u8 {\n");
    try file_buf.appendSlice("    const buf = __doxa_std.heap.page_allocator.allocSentinel(u8, s.len, 0) catch return null;\n");
    try file_buf.appendSlice("    @memcpy(buf[0..s.len], s);\n");
    try file_buf.appendSlice("    return buf.ptr;\n");
    try file_buf.appendSlice("}\n\n");
    const free_sym = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_inlinezig_free_cstr__{s}_{s}", .{ decl.module_name, short_hex });
    defer memoryManager.getAllocator().free(free_sym);

    const free_sym_owned = try memoryManager.getAllocator().dupe(u8, free_sym);

    try file_buf.appendSlice("pub export fn ");
    try file_buf.appendSlice(free_sym);
    try file_buf.appendSlice("(p: ?[*:0]u8) callconv(.c) void {\n");
    try file_buf.appendSlice("    if (p) |ptr| {\n");
    try file_buf.appendSlice("        const n: usize = __doxa_std.mem.len(ptr);\n");
    try file_buf.appendSlice("        const bytes: [*]u8 = @as([*]u8, @ptrCast(ptr));\n");
    try file_buf.appendSlice("        __doxa_std.heap.page_allocator.free(bytes[0 .. n + 1]);\n");
    try file_buf.appendSlice("    }\n");
    try file_buf.appendSlice("}\n\n");

    for (sigs) |sig| {
        const ret_zig = zigTypeName.fromReturnTypeInfo(sig.return_type) orelse {
            reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in VM bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
            return error.NotImplemented;
        };

        var param_types_bc = try memoryManager.getAllocator().alloc(bc.BytecodeType, sig.param_types.len);
        errdefer memoryManager.getAllocator().free(param_types_bc);

        var sig_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer sig_buf.deinit();
        const sym_ident = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_export__{s}_{s}", .{ decl.module_name, sig.name });
        defer memoryManager.getAllocator().free(sym_ident);
        const sym_export = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}.{s}", .{ decl.module_name, sig.name });
        errdefer memoryManager.getAllocator().free(sym_export);

        try sig_buf.appendSlice("pub fn ");
        try sig_buf.appendSlice(sym_ident);
        try sig_buf.appendSlice("(");

        var prelude_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer prelude_buf.deinit();

        var call_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer call_buf.deinit();
        try call_buf.appendSlice(sig.name);
        try call_buf.appendSlice("(");

        for (sig.param_types, 0..) |pt, i| {
            const pt_zig = zigTypeName.fromParamTypeInfo(pt) orelse {
                reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type in VM bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
                return error.NotImplemented;
            };
            param_types_bc[i] = zigTypeName.toBytecodeType(pt);

            if (i > 0) {
                try sig_buf.appendSlice(", ");
                try call_buf.appendSlice(", ");
            }
            const arg_name = try std.fmt.allocPrint(memoryManager.getAllocator(), "a{}", .{i});
            defer memoryManager.getAllocator().free(arg_name);
            try sig_buf.appendSlice(arg_name);
            try sig_buf.appendSlice(": ");
            try sig_buf.appendSlice(pt_zig);

            if (pt.base == .String) {
                const var_name = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_s{}", .{i});
                defer memoryManager.getAllocator().free(var_name);
                try prelude_buf.appendSlice("    const ");
                try prelude_buf.appendSlice(var_name);
                try prelude_buf.appendSlice(" = if (");
                try prelude_buf.appendSlice(arg_name);
                try prelude_buf.appendSlice(") |p| __doxa_std.mem.span(p) else \"\";\n");
                try call_buf.appendSlice(var_name);
            } else {
                try call_buf.appendSlice(arg_name);
            }
        }
        try sig_buf.appendSlice(") callconv(.c) ");
        try sig_buf.appendSlice(ret_zig);
        try sig_buf.appendSlice(" {\n    ");
        try call_buf.appendSlice(")");

        try sig_buf.appendSlice("\n");
        if (prelude_buf.items.len > 0) try sig_buf.appendSlice(prelude_buf.items);

        if (std.mem.eql(u8, ret_zig, "void")) {
            try sig_buf.appendSlice("    ");
            try sig_buf.appendSlice(call_buf.items);
            try sig_buf.appendSlice(";\n}\n\n");
        } else if (sig.return_type.base == .String) {
            try sig_buf.appendSlice("    const __doxa_out = ");
            try sig_buf.appendSlice(call_buf.items);
            try sig_buf.appendSlice(";\n");
            try sig_buf.appendSlice("    return __doxa_to_cstr(__doxa_out);\n}\n\n");
        } else {
            try sig_buf.appendSlice("    return ");
            try sig_buf.appendSlice(call_buf.items);
            try sig_buf.appendSlice(";\n}\n\n");
        }

        try file_buf.appendSlice(sig_buf.items);

        var export_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer export_buf.deinit();
        try export_buf.appendSlice("comptime { @export(");
        try export_buf.appendSlice("&");
        try export_buf.appendSlice(sym_ident);
        try export_buf.appendSlice(", .{ .name = \"");
        try export_buf.appendSlice(sym_export);
        try export_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(export_buf.items);

        const fn_key = try memoryManager.getAllocator().dupe(u8, sig.name);
        errdefer memoryManager.getAllocator().free(fn_key);
        errdefer memoryManager.getAllocator().free(sym_export);
        errdefer memoryManager.getAllocator().free(param_types_bc);
        try functions.put(fn_key, .{
            .symbol = sym_export,
            .param_types = param_types_bc,
            .return_type = zigTypeName.toBytecodeType(sig.return_type),
        });
    }

    const lib_exists = blk: {
        std.fs.cwd().access(lib_path, .{}) catch break :blk false;
        break :blk true;
    };
    if (!lib_exists) {
        try std.fs.cwd().writeFile(.{ .sub_path = zig_path, .data = file_buf.items });

        var args_list = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer args_list.deinit();
        const emit_flag = try std.fmt.allocPrint(std.heap.page_allocator, "-femit-bin={s}", .{lib_path});
        defer std.heap.page_allocator.free(emit_flag);
        try args_list.appendSlice(&[_][]const u8{
            "zig",
            "build-lib",
            "-dynamic",
            zig_path,
            emit_flag,
            "-OReleaseFast",
        });

        var child = std.process.Child.init(args_list.items, std.heap.page_allocator);
        child.cwd = ".";
        child.stdout_behavior = .Inherit;
        child.stderr_behavior = .Inherit;
        const term = try child.spawnAndWait();
        switch (term) {
            .Exited => |code| if (code != 0) return error.Unexpected,
            else => return error.Unexpected,
        }
    }

    const key_owned = try memoryManager.getAllocator().dupe(u8, decl.module_name);
    const lib_owned = try memoryManager.getAllocator().dupe(u8, lib_path);

    try modules.put(key_owned, .{
        .lib_path = lib_owned,
        .lib = null,
        .free_cstr_symbol = free_sym_owned,
        .functions = functions,
    });
}

pub fn compileInlineZigModules(
    memoryManager: *MemoryManager,
    statements: []ast.Stmt,
    parser: *Parser,
    reporter: *Reporter,
    output_dir: []const u8,
) !?std.StringHashMap(VM.ZigRuntimeModule) {
    const zig_decls = try collectInlineZigDecls(memoryManager.getAllocator(), statements, parser);
    defer memoryManager.getAllocator().free(zig_decls);
    if (zig_decls.len == 0) return null;

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{output_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.fs.cwd().makePath(zig_cache_path);

    var modules = std.StringHashMap(VM.ZigRuntimeModule).init(memoryManager.getAllocator());
    errdefer {
        var it = modules.iterator();
        while (it.next()) |entry| {
            memoryManager.getAllocator().free(@constCast(entry.key_ptr.*));
            var m = entry.value_ptr.*;
            m.deinit(memoryManager.getAllocator());
        }
        modules.deinit();
    }

    for (zig_decls) |decl| {
        try compileOneInlineModule(memoryManager, reporter, output_dir, decl, &modules);
    }
    return modules;
}

pub fn compileInlineZigObjects(
    memoryManager: *MemoryManager,
    statements: []ast.Stmt,
    parser: *Parser,
    reporter: *Reporter,
    output_dir: []const u8,
) ![]const []const u8 {
    const Sha256 = std.crypto.hash.sha2.Sha256;

    const zig_decls = try collectInlineZigDecls(memoryManager.getAllocator(), statements, parser);
    defer memoryManager.getAllocator().free(zig_decls);

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{output_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.fs.cwd().makePath(zig_cache_path);

    var out_paths = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
    errdefer {
        for (out_paths.items) |p| memoryManager.getAllocator().free(@constCast(p));
        out_paths.deinit();
    }

    for (zig_decls) |decl| {
        const sigs = try inline_zig.sanitizeAndExtract(memoryManager.getAllocator(), decl.zig_source);
        defer inline_zig.deinitSigs(memoryManager.getAllocator(), sigs);

        var h: [Sha256.digest_length]u8 = undefined;
        var hasher = Sha256.init(.{});
        hasher.update("__doxa_inlinezig_v2__\n");
        hasher.update(decl.module_name);
        hasher.update("\n");
        hasher.update(decl.zig_source);
        hasher.final(&h);
        const hex_buf = std.fmt.bytesToHex(h, .lower);
        const short_hex = hex_buf[0..16];

        var zig_path_buf: [512]u8 = undefined;
        const zig_path = try std.fmt.bufPrint(&zig_path_buf, "{s}/zig/cache/{s}-{s}.zig", .{ output_dir, decl.module_name, short_hex });

        var obj_path_buf: [512]u8 = undefined;
        const obj_ext = if (builtin.os.tag == .windows) "obj" else "o";
        const obj_path = try std.fmt.bufPrint(&obj_path_buf, "{s}/zig/cache/{s}-{s}.{s}", .{ output_dir, decl.module_name, short_hex, obj_ext });

        const obj_exists = blk: {
            std.fs.cwd().access(obj_path, .{}) catch break :blk false;
            break :blk true;
        };

        if (!obj_exists) {
            const zig_exists = blk2: {
                std.fs.cwd().access(zig_path, .{}) catch break :blk2 false;
                break :blk2 true;
            };
            if (!zig_exists) {
                _ = try compileInlineZigModules(memoryManager, statements, parser, reporter, output_dir);
            }

            const emit_flag = try std.fmt.allocPrint(std.heap.page_allocator, "-femit-bin={s}", .{obj_path});
            defer std.heap.page_allocator.free(emit_flag);

            var args_list = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
            defer args_list.deinit();
            try args_list.appendSlice(&[_][]const u8{
                "zig",
                "build-obj",
                zig_path,
                emit_flag,
                "-OReleaseFast",
            });

            var child = std.process.Child.init(args_list.items, std.heap.page_allocator);
            child.cwd = ".";
            child.stdout_behavior = .Inherit;
            child.stderr_behavior = .Inherit;
            const term = try child.spawnAndWait();
            switch (term) {
                .Exited => |code| if (code != 0) return error.Unexpected,
                else => return error.Unexpected,
            }
        }

        try out_paths.append(try memoryManager.getAllocator().dupe(u8, obj_path));
    }

    return try out_paths.toOwnedSlice();
}
