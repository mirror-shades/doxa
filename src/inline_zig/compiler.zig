const std = @import("std");
const builtin = @import("builtin");

const ast = @import("../ast/ast.zig");
const Parser = @import("../parser/parser_types.zig").Parser;
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const hashing = @import("../utils/hashing.zig");
const abi_source = @embedFile("abi.zig");
const generator_source = @embedFile("compiler.zig");

fn cacheSeed() []const u8 {
    return abi_source ++ generator_source;
}

/// Scalar types an inline-Zig module may use at the ABI boundary. Collection,
/// enum, function, and union values are not supported as parameters/returns and
/// are rejected during codegen.
const AbiType = enum(u8) {
    Int,
    Byte,
    Float,
    String,
    Tetra,
    Nothing,
    Array,
    Struct,
    Map,
    Enum,
    Function,
    Union,
};

const ZigExportFn = struct {
    symbol: []const u8,
    param_types: []AbiType,
    return_type: AbiType,
};

const ZigDeclInfo = struct {
    module_name: []const u8,
    zig_source: []const u8,
    location: Reporting.Location,
    sigs: []ast.ZigFnSig,
};

const GeneratedModule = struct {
    zig_path: []const u8,
    lib_path: []const u8,
    functions: std.StringHashMap(ZigExportFn),
    free_sym_owned: []const u8,

    pub fn deinit(self: *GeneratedModule, allocator: std.mem.Allocator) void {
        allocator.free(self.zig_path);
        allocator.free(self.lib_path);
        allocator.free(self.free_sym_owned);
        var it = self.functions.iterator();
        while (it.next()) |entry| {
            allocator.free(@constCast(entry.key_ptr.*));
            allocator.free(@constCast(entry.value_ptr.*.symbol));
            allocator.free(entry.value_ptr.*.param_types);
        }
        self.functions.deinit();
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

    var lib_path_buf: [256]u8 = undefined;
    const lib_ext = switch (builtin.os.tag) {
        .windows => "dll",
        .macos => "dylib",
        else => "so",
    };
    const lib_path = try std.fmt.bufPrint(&lib_path_buf, "{s}/{s}-{s}.{s}", .{ cache_dir, decl.module_name, short_hex, lib_ext });

    var functions = std.StringHashMap(ZigExportFn).init(allocator);
    errdefer {
        var itf = functions.iterator();
        while (itf.next()) |entry| {
            const k = entry.key_ptr.*;
            const v = entry.value_ptr.*;
            allocator.free(@constCast(k));
            allocator.free(@constCast(v.symbol));
            allocator.free(v.param_types);
        }
        functions.deinit();
    }

    var file_buf = std.array_list.Managed(u8).init(allocator);
    defer file_buf.deinit();
    try appendZigSourceSanitized(&file_buf, decl.zig_source);
    try file_buf.appendSlice("\n\n");

    const zigTypeName = struct {
        const TypeMeta = struct {
            native_param: ?[]const u8,
            native_ret: ?[]const u8,
            abi: AbiType,
        };

        fn metaFor(t: ast.TypeInfo) TypeMeta {
            return switch (t.base) {
                .Int => .{ .native_param = "i64", .native_ret = "i64", .abi = .Int },
                .Float => .{ .native_param = "f64", .native_ret = "f64", .abi = .Float },
                .Byte => .{ .native_param = "u8", .native_ret = "u8", .abi = .Byte },
                .Tetra => .{ .native_param = "bool", .native_ret = "bool", .abi = .Tetra },
                .Nothing => .{ .native_param = "void", .native_ret = "void", .abi = .Nothing },
                .String => .{ .native_param = "?[*]const u8", .native_ret = "void", .abi = .String },
                else => .{ .native_param = null, .native_ret = null, .abi = .Nothing },
            };
        }

        fn fromNativeParamTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return metaFor(t).native_param;
        }

        fn fromNativeReturnTypeInfo(t: ast.TypeInfo) ?[]const u8 {
            return metaFor(t).native_ret;
        }

        fn toAbiType(t: ast.TypeInfo) AbiType {
            return metaFor(t).abi;
        }
    };

    try file_buf.appendSlice("const __doxa_std = @import(\"std\");\n\n");
    try file_buf.appendSlice(abi_source);
    try file_buf.appendSlice("\n");

    try file_buf.appendSlice("fn __doxa_to_cstr(s: []const u8) ?[*:0]u8 {\n");
    try file_buf.appendSlice("    const buf = __doxa_std.heap.page_allocator.allocSentinel(u8, s.len, 0) catch return null;\n");
    try file_buf.appendSlice("    @memcpy(buf[0..s.len], s);\n");
    try file_buf.appendSlice("    return buf.ptr;\n");
    try file_buf.appendSlice("}\n\n");

    const free_sym = try std.fmt.allocPrint(allocator, "__doxa_inlinezig_free_cstr__{s}_{s}", .{ decl.module_name, short_hex });
    defer allocator.free(free_sym);

    const free_sym_owned = try allocator.dupe(u8, free_sym);
    errdefer allocator.free(free_sym_owned);

    try file_buf.appendSlice("pub export fn ");
    try file_buf.appendSlice(free_sym);
    try file_buf.appendSlice("(v: *const DoxaAbiValue) callconv(.c) void {\n");
    try file_buf.appendSlice("    if (v.*.tag != .String) return;\n");
    try file_buf.appendSlice("    if (v.*.payload0 == 0) return;\n");
    try file_buf.appendSlice("    const n: usize = @intCast(v.*.payload1);\n");
    try file_buf.appendSlice("    const p: [*]u8 = @ptrFromInt(v.*.payload0);\n");
    try file_buf.appendSlice("    __doxa_std.heap.page_allocator.free(p[0..n]);\n");
    try file_buf.appendSlice("}\n\n");

    try file_buf.appendSlice("fn __doxa_zero() DoxaAbiValue {\n");
    try file_buf.appendSlice("    return .{ .tag = .Nothing, .flags = 0, .payload0 = 0, .payload1 = 0 };\n");
    try file_buf.appendSlice("}\n\n");

    for (sigs) |sig| {
        var param_types_abi = try allocator.alloc(AbiType, sig.param_types.len);
        errdefer allocator.free(param_types_abi);

        const sym_ident = try std.fmt.allocPrint(allocator, "__doxa_export__{s}_{s}", .{ decl.module_name, sig.name });
        defer allocator.free(sym_ident);
        const sym_export_abi = try std.fmt.allocPrint(allocator, "{s}.__doxa_abi__{s}", .{ decl.module_name, sig.name });
        errdefer allocator.free(sym_export_abi);
        const sym_export_native = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ decl.module_name, sig.name });
        defer allocator.free(sym_export_native);

        var sig_buf = std.array_list.Managed(u8).init(allocator);
        defer sig_buf.deinit();
        try sig_buf.appendSlice("pub fn ");
        try sig_buf.appendSlice(sym_ident);
        try sig_buf.appendSlice("(argv: [*]const DoxaAbiValue, argc: usize, out_ret: *DoxaAbiValue) callconv(.c) DoxaAbiStatus {\n");
        try sig_buf.appendSlice("    out_ret.* = __doxa_zero();\n");
        if (sig.param_types.len == 0) {
            try sig_buf.appendSlice("    _ = argv;\n");
        }
        const expected_argc = try std.fmt.allocPrint(allocator, "{}", .{sig.param_types.len});
        defer allocator.free(expected_argc);
        try sig_buf.appendSlice("    if (argc != ");
        try sig_buf.appendSlice(expected_argc);
        try sig_buf.appendSlice(") return .bad_arity;\n");

        var string_param_indices = std.ArrayListUnmanaged(usize){};
        defer string_param_indices.deinit(allocator);

        for (sig.param_types, 0..) |pt, i| {
            const abi_t = zigTypeName.toAbiType(pt);
            if (abi_t == .Array or abi_t == .Struct or abi_t == .Map or abi_t == .Enum or abi_t == .Function or abi_t == .Union) {
                reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type for '{s}.{s}'", .{ decl.module_name, sig.name });
                return error.NotImplemented;
            }
            param_types_abi[i] = abi_t;
            if (pt.base == .String) {
                try string_param_indices.append(allocator, i);
            }

            const idx_str = try std.fmt.allocPrint(allocator, "{}", .{i});
            defer allocator.free(idx_str);
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
                    try sig_buf.appendSlice("_raw: []const u8 = if (__doxa_v");
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
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice("_nul: ?[*:0]u8 = if (__doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice("_raw.len == 0) null else __doxa_to_cstr(__doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice("_raw) orelse return .internal;\n");
                    try sig_buf.appendSlice("    const __doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice(": []const u8 = if (__doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice("_nul) |p| p[0..__doxa_a");
                    try sig_buf.appendSlice(idx_str);
                    try sig_buf.appendSlice("_raw.len] else \"\";\n");
                },
                else => {
                    reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type for '{s}.{s}'", .{ decl.module_name, sig.name });
                    return error.NotImplemented;
                },
            }
        }

        if (string_param_indices.items.len > 0) {
            try sig_buf.appendSlice("    defer {\n");
            for (string_param_indices.items) |si| {
                const si_str = try std.fmt.allocPrint(allocator, "{}", .{si});
                defer allocator.free(si_str);
                try sig_buf.appendSlice("        if (__doxa_a");
                try sig_buf.appendSlice(si_str);
                try sig_buf.appendSlice("_raw.len > 0) {\n");
                try sig_buf.appendSlice("            if (__doxa_a");
                try sig_buf.appendSlice(si_str);
                try sig_buf.appendSlice("_nul) |__doxa_p| __doxa_std.heap.page_allocator.free(__doxa_p[0 .. __doxa_a");
                try sig_buf.appendSlice(si_str);
                try sig_buf.appendSlice("_raw.len + 1]);\n");
                try sig_buf.appendSlice("        }\n");
            }
            try sig_buf.appendSlice("    }\n");
        }

        var call_buf = std.array_list.Managed(u8).init(allocator);
        defer call_buf.deinit();
        try call_buf.appendSlice(sig.name);
        try call_buf.appendSlice("(");
        for (sig.param_types, 0..) |_, i| {
            if (i > 0) try call_buf.appendSlice(", ");
            const idx_str = try std.fmt.allocPrint(allocator, "{}", .{i});
            defer allocator.free(idx_str);
            try call_buf.appendSlice("__doxa_a");
            try call_buf.appendSlice(idx_str);
        }
        try call_buf.appendSlice(")");

        const ret_type = zigTypeName.toAbiType(sig.return_type);
        if (ret_type == .Array or ret_type == .Struct or ret_type == .Map or ret_type == .Enum or ret_type == .Function or ret_type == .Union) {
            reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type for '{s}.{s}'", .{ decl.module_name, sig.name });
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
            .Array, .Struct, .Map, .Enum, .Function, .Union, .Custom => {
                reporter.reportCompileError(decl.location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type for '{s}.{s}'", .{ decl.module_name, sig.name });
                return error.NotImplemented;
            },
        }
        try sig_buf.appendSlice("}\n\n");

        try file_buf.appendSlice(sig_buf.items);

        var export_buf = std.array_list.Managed(u8).init(allocator);
        defer export_buf.deinit();
        try export_buf.appendSlice("comptime { @export(");
        try export_buf.appendSlice("&");
        try export_buf.appendSlice(sym_ident);
        try export_buf.appendSlice(", .{ .name = \"");
        try export_buf.appendSlice(sym_export_abi);
        try export_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(export_buf.items);

        const native_ident = try std.fmt.allocPrint(allocator, "__doxa_native__{s}_{s}", .{ decl.module_name, sig.name });
        defer allocator.free(native_ident);

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
                try native_prelude.appendSlice(") |p| p[0..");
                try native_prelude.appendSlice(arg_name);
                try native_prelude.appendSlice("_len] else \"\";\n");
                try native_call.appendSlice(s_name);

                try native_buf.appendSlice(", ");
                try native_buf.appendSlice(arg_name);
                try native_buf.appendSlice("_len: usize");
            } else {
                try native_call.appendSlice(arg_name);
            }
        }

        if (sig.return_type.base == .String) {
            if (sig.param_types.len > 0) try native_buf.appendSlice(", ");
            try native_buf.appendSlice("out_ptr: *?[*]u8, out_len: *usize");
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
            try native_buf.appendSlice("    const __doxa_buf = __doxa_std.heap.page_allocator.alloc(u8, __doxa_out.len) catch { out_ptr.* = null; out_len.* = 0; return; };\n");
            try native_buf.appendSlice("    @memcpy(__doxa_buf, __doxa_out);\n");
            try native_buf.appendSlice("    out_ptr.* = __doxa_buf.ptr;\n");
            try native_buf.appendSlice("    out_len.* = __doxa_out.len;\n");
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
        try native_buf.appendSlice(sym_export_native);
        try native_buf.appendSlice("\" }); }\n\n");
        try file_buf.appendSlice(native_buf.items);

        const fn_key = try allocator.dupe(u8, sig.name);
        errdefer allocator.free(fn_key);
        errdefer allocator.free(sym_export_abi);
        errdefer allocator.free(param_types_abi);
        try functions.put(fn_key, .{
            .symbol = sym_export_abi,
            .param_types = param_types_abi,
            .return_type = ret_type,
        });
    }

    try std.fs.cwd().writeFile(.{ .sub_path = zig_path, .data = file_buf.items });

    return .{
        .zig_path = try allocator.dupe(u8, zig_path),
        .lib_path = try allocator.dupe(u8, lib_path),
        .functions = functions,
        .free_sym_owned = free_sym_owned,
    };
}

pub fn compileInlineZigObjects(
    memoryManager: *MemoryManager,
    statements: []ast.Stmt,
    parser: *Parser,
    reporter: *Reporter,
    zig_exe_path: []const u8,
    cache_dir: []const u8,
    zig_opt_flag: []const u8,
    target_triple: []const u8,
    target_os: []const u8,
    include_dirs: []const []const u8,
) ![]const []const u8 {
    const zig_decls = try collectInlineZigDecls(memoryManager.getAllocator(), statements, parser);
    defer memoryManager.getAllocator().free(zig_decls);

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{cache_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.fs.cwd().makePath(zig_cache_path);

    var out_paths = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
    errdefer {
        for (out_paths.items) |p| memoryManager.getAllocator().free(@constCast(p));
        out_paths.deinit();
    }

    for (zig_decls) |decl| {
        var gen = try generateWrapperZigFile(memoryManager.getAllocator(), reporter, zig_cache_path, decl);
        defer gen.deinit(memoryManager.getAllocator());

        // The object extension follows the target, not the host.
        const obj_ext = if (std.mem.eql(u8, target_os, "windows")) "obj" else "o";
        const zig_dir = std.fs.path.dirname(gen.zig_path) orelse ".";
        const zig_stem = std.fs.path.stem(gen.zig_path);

        // Content-addressed cache key: wrapper source (already hashed into the
        // wrapper path) + target triple + opt mode. Distinct targets or opt
        // modes therefore produce distinct objects, and unchanged source+target
        // skips the `build-obj` spawn.
        var kb = hashing.KeyBuilder.init(cacheSeed());
        kb.addBytes(gen.zig_path);
        kb.addBytes(target_triple);
        kb.addBytes(zig_opt_flag);
        for (include_dirs) |dir| {
            kb.addBytes(dir);
        }
        const key_hex = hashing.shortHexOf(kb.finish());

        var obj_path_buf: [320]u8 = undefined;
        const obj_path = try std.fmt.bufPrint(&obj_path_buf, "{s}/{s}.{s}.{s}", .{ zig_dir, zig_stem, key_hex[0..], obj_ext });

        const obj_already_compiled = blk: {
            const f = std.fs.cwd().openFile(obj_path, .{}) catch break :blk false;
            f.close();
            break :blk true;
        };

        if (!obj_already_compiled) {
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

            var dir_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer dir_arena.deinit();
            // The build module's `--include` dirs apply to every module's
            // @cImport, matching how the artifact's `zig cc` step consumes them.
            for (include_dirs) |dir| {
                try args_list.append(try std.fmt.allocPrint(dir_arena.allocator(), "-I{s}", .{dir}));
            }
            // A module that cImports C headers needs libc to resolve them.
            if (std.mem.indexOf(u8, decl.zig_source, "@cImport") != null) {
                try args_list.append("-lc");
            }

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
