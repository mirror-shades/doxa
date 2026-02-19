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
    hasher.update("__doxa_inlinezig_v5__\n");
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
        fn fromNativeParamTypeInfo(t: ast.TypeInfo) ?[]const u8 {
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

        fn fromNativeReturnTypeInfo(t: ast.TypeInfo) ?[]const u8 {
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
    };

    try file_buf.appendSlice("const __doxa_std = @import(\"std\");\n\n");
    try file_buf.appendSlice("pub const DoxaAbiTag = enum(u32) {\n");
    try file_buf.appendSlice("    Int = 0,\n");
    try file_buf.appendSlice("    Float = 1,\n");
    try file_buf.appendSlice("    Byte = 2,\n");
    try file_buf.appendSlice("    String = 3,\n");
    try file_buf.appendSlice("    Tetra = 4,\n");
    try file_buf.appendSlice("    Nothing = 5,\n");
    try file_buf.appendSlice("};\n\n");
    try file_buf.appendSlice("pub const DoxaAbiValue = extern struct {\n");
    try file_buf.appendSlice("    tag: DoxaAbiTag,\n");
    try file_buf.appendSlice("    flags: u32,\n");
    try file_buf.appendSlice("    payload0: u64,\n");
    try file_buf.appendSlice("    payload1: u64,\n");
    try file_buf.appendSlice("};\n\n");
    try file_buf.appendSlice("pub const DoxaAbiStatus = enum(i32) {\n");
    try file_buf.appendSlice("    ok = 0,\n");
    try file_buf.appendSlice("    bad_arity = 1,\n");
    try file_buf.appendSlice("    bad_tag = 2,\n");
    try file_buf.appendSlice("    bad_value = 3,\n");
    try file_buf.appendSlice("    internal = 255,\n");
    try file_buf.appendSlice("};\n\n");

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
    try file_buf.appendSlice("(v: DoxaAbiValue) callconv(.c) void {\n");
    try file_buf.appendSlice("    if (v.tag != .String) return;\n");
    try file_buf.appendSlice("    if (v.payload0 == 0) return;\n");
    try file_buf.appendSlice("    const n: usize = @intCast(v.payload1);\n");
    try file_buf.appendSlice("    const p: [*]u8 = @ptrFromInt(v.payload0);\n");
    try file_buf.appendSlice("    __doxa_std.heap.page_allocator.free(p[0..n]);\n");
    try file_buf.appendSlice("}\n\n");

    try file_buf.appendSlice("fn __doxa_zero() DoxaAbiValue {\n");
    try file_buf.appendSlice("    return .{ .tag = .Nothing, .flags = 0, .payload0 = 0, .payload1 = 0 };\n");
    try file_buf.appendSlice("}\n\n");

    for (sigs) |sig| {
        var param_types_bc = try memoryManager.getAllocator().alloc(bc.BytecodeType, sig.param_types.len);
        errdefer memoryManager.getAllocator().free(param_types_bc);

        const sym_ident = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_export__{s}_{s}", .{ decl.module_name, sig.name });
        defer memoryManager.getAllocator().free(sym_ident);
        const sym_export_abi = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}.__doxa_abi__{s}", .{ decl.module_name, sig.name });
        errdefer memoryManager.getAllocator().free(sym_export_abi);
        const sym_export_native = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}.{s}", .{ decl.module_name, sig.name });
        defer memoryManager.getAllocator().free(sym_export_native);

        var sig_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer sig_buf.deinit();
        try sig_buf.appendSlice("pub fn ");
        try sig_buf.appendSlice(sym_ident);
        try sig_buf.appendSlice("(argv: [*]const DoxaAbiValue, argc: usize, out_ret: *DoxaAbiValue) callconv(.c) DoxaAbiStatus {\n");
        try sig_buf.appendSlice("    out_ret.* = __doxa_zero();\n");
        if (sig.param_types.len == 0) {
            try sig_buf.appendSlice("    _ = argv;\n");
        }
        const expected_argc = try std.fmt.allocPrint(memoryManager.getAllocator(), "{}", .{sig.param_types.len});
        defer memoryManager.getAllocator().free(expected_argc);
        try sig_buf.appendSlice("    if (argc != ");
        try sig_buf.appendSlice(expected_argc);
        try sig_buf.appendSlice(") return .bad_arity;\n");

        for (sig.param_types, 0..) |pt, i| {
            const bytecode_t = zigTypeName.toBytecodeType(pt);
            if (bytecode_t == .Array or bytecode_t == .Struct or bytecode_t == .Map or bytecode_t == .Enum or bytecode_t == .Function or bytecode_t == .Union) {
                reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type in VM bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
                return error.NotImplemented;
            }
            param_types_bc[i] = bytecode_t;

            const idx_str = try std.fmt.allocPrint(memoryManager.getAllocator(), "{}", .{i});
            defer memoryManager.getAllocator().free(idx_str);
            try sig_buf.appendSlice("    const __doxa_v");
            try sig_buf.appendSlice(idx_str);
            try sig_buf.appendSlice(" = argv[");
            try sig_buf.appendSlice(idx_str);
            try sig_buf.appendSlice("];\n");

            switch (pt.base) {
                .Int => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .Int) return .bad_tag;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": i64 = @bitCast(__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0);\n");
                },
                .Float => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .Float) return .bad_tag;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": f64 = @bitCast(__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0);\n");
                },
                .Byte => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .Byte) return .bad_tag;\n");
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0 > 255) return .bad_value;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": u8 = @intCast(__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0);\n");
                },
                .Tetra => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .Tetra) return .bad_tag;\n");
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0 > 3) return .bad_value;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": bool = switch (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0) { 1, 2 => true, else => false };\n");
                },
                .Nothing => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .Nothing) return .bad_tag;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": void = {};\n");
                },
                .String => {
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".tag != .String) return .bad_tag;\n");
                    try sig_buf.appendSlice("    if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0 == 0 and __doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload1 != 0) return .bad_value;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": []const u8 = if (__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload1 == 0) \"\" else blk: {\n");
                    try sig_buf.appendSlice("        const __doxa_p: [*]const u8 = @ptrFromInt(__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload0);\n");
                    try sig_buf.appendSlice("        const __doxa_n: usize = @intCast(__doxa_v");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(".payload1);\n");
                    try sig_buf.appendSlice("        break :blk __doxa_p[0..__doxa_n];\n");
                    try sig_buf.appendSlice("    };\n");
                },
                else => unreachable,
            }
        }

        var call_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer call_buf.deinit();
        try call_buf.appendSlice(sig.name);
        try call_buf.appendSlice("(");
        for (sig.param_types, 0..) |_, i| {
            if (i > 0) try call_buf.appendSlice(", ");
            const idx_str = try std.fmt.allocPrint(memoryManager.getAllocator(), "{}", .{i});
            defer memoryManager.getAllocator().free(idx_str);
            try call_buf.appendSlice("__doxa_a");
            try call_buf.appendSlice(idx_str);
        }
        try call_buf.appendSlice(")");

        const ret_type = zigTypeName.toBytecodeType(sig.return_type);
        if (ret_type == .Array or ret_type == .Struct or ret_type == .Map or ret_type == .Enum or ret_type == .Function or ret_type == .Union) {
            reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in VM bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
            return error.NotImplemented;
        }

        switch (sig.return_type.base) {
            .Nothing => {
                try sig_buf.appendSlice("    ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .Nothing, .flags = 0, .payload0 = 0, .payload1 = 0 };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            .Int => {
                try sig_buf.appendSlice("    const __doxa_out: i64 = ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .Int, .flags = 0, .payload0 = @bitCast(__doxa_out), .payload1 = 0 };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            .Float => {
                try sig_buf.appendSlice("    const __doxa_out: f64 = ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .Float, .flags = 0, .payload0 = @bitCast(__doxa_out), .payload1 = 0 };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            .Byte => {
                try sig_buf.appendSlice("    const __doxa_out: u8 = ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .Byte, .flags = 0, .payload0 = __doxa_out, .payload1 = 0 };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            .Tetra => {
                try sig_buf.appendSlice("    const __doxa_out: bool = ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .Tetra, .flags = 0, .payload0 = if (__doxa_out) 1 else 0, .payload1 = 0 };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            .String => {
                try sig_buf.appendSlice("    const __doxa_out: []const u8 = ");
                try sig_buf.appendSlice(call_buf.items);
                try sig_buf.appendSlice(";\n");
                try sig_buf.appendSlice("    if (__doxa_out.len == 0) {\n");
                try sig_buf.appendSlice("        out_ret.* = .{ .tag = .String, .flags = 0, .payload0 = 0, .payload1 = 0 };\n");
                try sig_buf.appendSlice("        return .ok;\n");
                try sig_buf.appendSlice("    }\n");
                try sig_buf.appendSlice("    const __doxa_buf = __doxa_std.heap.page_allocator.alloc(u8, __doxa_out.len) catch return .internal;\n");
                try sig_buf.appendSlice("    @memcpy(__doxa_buf, __doxa_out);\n");
                try sig_buf.appendSlice("    out_ret.* = .{ .tag = .String, .flags = 0, .payload0 = @intFromPtr(__doxa_buf.ptr), .payload1 = __doxa_buf.len };\n");
                try sig_buf.appendSlice("    return .ok;\n");
            },
            else => unreachable,
        }
        try sig_buf.appendSlice("}\n\n");

        try file_buf.appendSlice(sig_buf.items);

        var export_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer export_buf.deinit();
        try export_buf.appendSlice("comptime { @export(");
        try export_buf.appendSlice("&");
        try export_buf.appendSlice(sym_ident);
        try export_buf.appendSlice(", .{ .name = \"");
        try export_buf.appendSlice(sym_export_abi);
        try export_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(export_buf.items);

        const native_ident = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_native__{s}_{s}", .{ decl.module_name, sig.name });
        defer memoryManager.getAllocator().free(native_ident);

        const native_ret_zig = zigTypeName.fromNativeReturnTypeInfo(sig.return_type) orelse {
            reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in native bridge for '{s}.{s}'", .{ decl.module_name, sig.name });
            return error.NotImplemented;
        };

        var native_buf = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer native_buf.deinit();
        try native_buf.appendSlice("pub fn ");
        try native_buf.appendSlice(native_ident);
        try native_buf.appendSlice("(");

        var native_prelude = std.array_list.Managed(u8).init(memoryManager.getAllocator());
        defer native_prelude.deinit();
        var native_call = std.array_list.Managed(u8).init(memoryManager.getAllocator());
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
            const arg_name = try std.fmt.allocPrint(memoryManager.getAllocator(), "a{}", .{i});
            defer memoryManager.getAllocator().free(arg_name);
            try native_buf.appendSlice(arg_name);
            try native_buf.appendSlice(": ");
            try native_buf.appendSlice(pt_zig);
            if (pt.base == .String) {
                const s_name = try std.fmt.allocPrint(memoryManager.getAllocator(), "__doxa_s{}", .{i});
                defer memoryManager.getAllocator().free(s_name);
                try native_prelude.appendSlice("    const ");
                try native_prelude.appendSlice(s_name);
                try native_prelude.appendSlice(" = if (");
                try native_prelude.appendSlice(arg_name);
                try native_prelude.appendSlice(") |p| __doxa_std.mem.span(p) else \"\";\n");
                try native_call.appendSlice(s_name);
            } else {
                try native_call.appendSlice(arg_name);
            }
        }

        try native_buf.appendSlice(") callconv(.c) ");
        try native_buf.appendSlice(native_ret_zig);
        try native_buf.appendSlice(" {\n");
        if (native_prelude.items.len > 0) try native_buf.appendSlice(native_prelude.items);
        try native_call.appendSlice(")");

        if (std.mem.eql(u8, native_ret_zig, "void")) {
            try native_buf.appendSlice("    ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
        } else if (sig.return_type.base == .String) {
            try native_buf.appendSlice("    const __doxa_out = ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
            try native_buf.appendSlice("    return __doxa_to_cstr(__doxa_out);\n");
        } else {
            try native_buf.appendSlice("    return ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
        }
        try native_buf.appendSlice("}\n");
        try native_buf.appendSlice("comptime { @export(&");
        try native_buf.appendSlice(native_ident);
        try native_buf.appendSlice(", .{ .name = \"");
        try native_buf.appendSlice(sym_export_native);
        try native_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(native_buf.items);

        const fn_key = try memoryManager.getAllocator().dupe(u8, sig.name);
        errdefer memoryManager.getAllocator().free(fn_key);
        errdefer memoryManager.getAllocator().free(sym_export_abi);
        errdefer memoryManager.getAllocator().free(param_types_bc);
        try functions.put(fn_key, .{
            .symbol = sym_export_abi,
            .param_types = param_types_bc,
            .return_type = ret_type,
        });
    }

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

    var maybe_modules = try compileInlineZigModules(memoryManager, statements, parser, reporter, output_dir);
    defer {
        if (maybe_modules) |*mods| {
            var it = mods.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                var m = entry.value_ptr.*;
                m.deinit(memoryManager.getAllocator());
                memoryManager.getAllocator().free(@constCast(key));
            }
            mods.deinit();
        }
    }

    for (zig_decls) |decl| {
        const sigs = try inline_zig.sanitizeAndExtract(memoryManager.getAllocator(), decl.zig_source);
        defer inline_zig.deinitSigs(memoryManager.getAllocator(), sigs);

        var h: [Sha256.digest_length]u8 = undefined;
        var hasher = Sha256.init(.{});
        hasher.update("__doxa_inlinezig_v5__\n");
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

        try out_paths.append(try memoryManager.getAllocator().dupe(u8, obj_path));
    }

    return try out_paths.toOwnedSlice();
}
