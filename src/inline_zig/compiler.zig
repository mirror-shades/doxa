const std = @import("std");
const builtin = @import("builtin");

const ast = @import("../ast/ast.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const MemoryManager = @import("../utils/memory.zig").MemoryManager;
const Profiler = @import("../utils/profiler.zig").Profiler;
const hashing = @import("../utils/hashing.zig");
const module_graph = @import("../module/graph.zig");
const inline_zig = @import("../parser/inline_zig.zig");
const ModuleGraph = module_graph.ModuleGraph;
const ModuleRecord = module_graph.ModuleRecord;
const generator_source = @embedFile("compiler.zig");

fn cacheSeed() []const u8 {
    return generator_source;
}

const GeneratedModule = struct {
    zig_path: []const u8,
    /// How the wrapper's first `source_len` bytes, the unit's sanitized
    /// source, map back to the unit's own text.
    source_map: SourceMap,
    /// Every `.zig` file the unit's imports reach, read in place: an error in
    /// one is reported where it stands.
    imported: []const ImportedFile,
    /// Where `build-obj` runs, which the paths in its errors are relative to.
    /// Borrowed.
    build_dir: []const u8,

    pub fn deinit(self: *GeneratedModule, allocator: std.mem.Allocator) void {
        allocator.free(self.zig_path);
        allocator.free(self.source_map.cuts);
        for (self.imported) |file| {
            allocator.free(file.path);
            allocator.free(file.source);
        }
        allocator.free(self.imported);
    }
};

/// A `.zig` file a unit's imports reach.
const ImportedFile = struct {
    /// As its importer writes it.
    spelling: []const u8,
    /// Its real path.
    path: []const u8,
    source: []const u8,
};

/// Every file a unit's imports reach, the unit's own imports first, then what
/// each of those imports by a relative path, once each. Zig resolves those
/// itself inside each imported module; they are listed so their content keys
/// the shim and an error in one maps to it.
fn importClosure(io: std.Io, allocator: std.mem.Allocator, unit: module_graph.ZigUnit) ![]const ImportedFile {
    var files = std.array_list.Managed(ImportedFile).init(allocator);
    errdefer {
        for (files.items) |file| {
            allocator.free(file.path);
            allocator.free(file.source);
        }
        files.deinit();
    }
    for (unit.imports) |import| {
        const path = try allocator.dupe(u8, import.path);
        errdefer allocator.free(path);
        const source = try readSource(io, allocator, path);
        errdefer allocator.free(source);
        try files.append(.{ .spelling = import.spelling, .path = path, .source = source });
    }
    var next: usize = 0;
    while (next < files.items.len) : (next += 1) {
        const importer = files.items[next];
        const spellings = try inline_zig.fileImports(allocator, importer.source);
        defer allocator.free(spellings);
        for (spellings) |spelling| {
            const path = try std.fs.path.resolve(allocator, &.{ std.fs.path.dirname(importer.path) orelse ".", spelling });
            errdefer allocator.free(path);
            const seen = for (files.items) |file| {
                if (std.mem.eql(u8, file.path, path)) break true;
            } else false;
            if (seen) {
                allocator.free(path);
                continue;
            }
            const source = try readSource(io, allocator, path);
            errdefer allocator.free(source);
            try files.append(.{ .spelling = spelling, .path = path, .source = source });
        }
    }
    return files.toOwnedSlice();
}

fn readSource(io: std.Io, allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
    return std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .unlimited);
}

/// The wrapper opens with the unit's source copied verbatim except for the
/// keywords `appendZigSourceSanitized` strips. Each strip is a cut: `len`
/// source bytes removed just before wrapper offset `at`, in increasing order.
/// Lines are never cut, so this maps any wrapper offset in the copy exactly.
const SourceMap = struct {
    cuts: []const Cut,
    /// The copy's length in the wrapper; everything after it is generated.
    source_len: usize,

    const Cut = struct { at: usize, len: usize };

    /// The unit-source offset of a wrapper offset, or null for generated code.
    fn toSource(self: SourceMap, wrapper_offset: usize) ?usize {
        if (wrapper_offset > self.source_len) return null;
        var offset = wrapper_offset;
        for (self.cuts) |cut| {
            if (cut.at > wrapper_offset) break;
            offset += cut.len;
        }
        return offset;
    }
};

fn appendZigSourceSanitized(buf: *std.array_list.Managed(u8), cuts: *std.array_list.Managed(SourceMap.Cut), source: []const u8) !void {
    var i: usize = 0;
    while (i < source.len) {
        if (std.mem.startsWith(u8, source[i..], "public ")) {
            try cuts.append(.{ .at = buf.items.len, .len = "public ".len });
            i += "public ".len;
            continue;
        }
        if (std.mem.startsWith(u8, source[i..], "export ") and
            (i + "export ".len < source.len) and
            std.mem.startsWith(u8, source[i + "export ".len ..], "fn "))
        {
            try cuts.append(.{ .at = buf.items.len, .len = "export ".len });
            i += "export ".len;
            continue;
        }
        try buf.append(source[i]);
        i += 1;
    }
}

/// The Zig units the program reached: every inline `zig` block and imported
/// `.zig` file whose functions analysis registered.
pub fn programZigUnits(allocator: std.mem.Allocator, graph: *const ModuleGraph) ![]const *ModuleRecord {
    var out = std.array_list.Managed(*ModuleRecord).init(allocator);
    errdefer out.deinit();
    for (graph.records.items) |record| {
        if (record.zig == null or !record.status.atLeast(.Analyzed)) continue;
        try out.append(record);
    }
    return out.toOwnedSlice();
}

/// Compile-time description of an inline-Zig array: its innermost Zig type, how
/// many `[]const` levels wrap it, and the runtime tag/size of the innermost
/// element. Mirrors `arrayElementTag` / `arrayElementSize` in the LLVM backend
/// and the tags in `src/runtime/abi.zig` — they must stay in sync.
const ArrayInfo = struct {
    zig_type: []const u8,
    depth: usize,
    elem_size: u64,
    elem_tag: u64,
};

/// Resolve a Doxa type as it crosses the boundary. Scalars and `string` are
/// depth 0; every `Array` level adds one, so `int[]` is depth 1 and `int[][]`
/// depth 2. Returns null for anything unsupported, which the wrapper generator
/// reports as `E8002`.
fn arrayInfoFor(t: ast.TypeInfo) ?ArrayInfo {
    return switch (t.base) {
        .Int => .{ .zig_type = "i64", .depth = 0, .elem_size = 8, .elem_tag = 0 },
        .Float => .{ .zig_type = "f64", .depth = 0, .elem_size = 8, .elem_tag = 2 },
        .Byte => .{ .zig_type = "u8", .depth = 0, .elem_size = 1, .elem_tag = 1 },
        .String => .{ .zig_type = "[]const u8", .depth = 0, .elem_size = 16, .elem_tag = 3 },
        // A Doxa enum is a single `i64` discriminant; tag 8 matches the runtime
        // and LLVM `arrayElementTag`/`arrayElementSize` tables. The wrapper
        // validates the name is a registered enum before using this.
        .Enum => .{ .zig_type = "i64", .depth = 0, .elem_size = 8, .elem_tag = 8 },
        .Array => blk: {
            const inner = arrayInfoFor(t.array_type.?.*) orelse break :blk null;
            break :blk .{
                .zig_type = inner.zig_type,
                .depth = inner.depth + 1,
                .elem_size = inner.elem_size,
                .elem_tag = inner.elem_tag,
            };
        },
        else => null,
    };
}

fn nativeType(t: ast.TypeInfo, comptime string_type: []const u8) ?[]const u8 {
    return switch (t.base) {
        .Int => "i64",
        .Float => "f64",
        .Byte => "u8",
        .Tetra => "bool",
        .Nothing => "void",
        .String => string_type,
        .Array => "?*__DoxaArrayHeader",
        .Enum => "i64",
        else => null,
    };
}

fn nativeParamType(t: ast.TypeInfo) ?[]const u8 {
    return nativeType(t, "?[*]const u8");
}

fn nativeReturnType(t: ast.TypeInfo) ?[]const u8 {
    // String returns cross through the (out_ptr, out_len) pair instead.
    return nativeType(t, "void");
}

/// The enum a fallible return (`DoxaError_<path>!<payload>`) names, once analysis
/// has resolved it. `null` for every other return type, including a plain union
/// — an inline-Zig signature can only produce this one union shape.
fn errorUnionRef(t: ast.TypeInfo) ?ast.TypeRef {
    if (t.base != .Union) return null;
    for (t.union_type.?.types) |member| {
        if (member.base != .Enum) continue;
        if (member.custom_type) |custom| switch (custom) {
            .ref => |r| return r,
            .written => {},
        };
    }
    return null;
}

/// The success member of a fallible return. `!void` gives `nothing`, so the
/// union is `nothing | <enum>` and the payload carries no bytes.
/// Whether the wrapper for `sig` takes the caller's arena as a hidden first
/// argument: it does exactly when its result — or a fallible result's payload
/// — is a heap value, which the wrapper builds in that arena
/// (`docs/zig.md`, "ABI"). The emitter passes it by the same rule.
pub fn takesArena(sig: *const ast.ZigFnSig) bool {
    const result = resultType(sig) orelse return false;
    // A struct result is built in that arena by the caller, whatever it holds.
    return result.base == .String or result.base == .Array or result.base == .Custom;
}

/// What a signature's result carries: the return type, or a fallible
/// return's payload.
fn resultType(sig: *const ast.ZigFnSig) ?ast.TypeInfo {
    if (errorUnionRef(sig.return_type) != null) return errorUnionPayload(sig.return_type);
    return sig.return_type;
}

/// The index in `unit.structs` of the crossing struct `t` (a resolved
/// `.Custom`) names. Analysis recorded every struct a signature returns and
/// every struct one holds (`zig_interop.StructCheck`).
fn structIndex(unit: *const module_graph.ZigUnit, t: ast.TypeInfo) usize {
    const ref = t.custom_type.?.ref;
    for (unit.structs, 0..) |s, i| {
        if (s.ref.eql(ref)) return i;
    }
    unreachable;
}

/// Whether the struct holds a `string` or an array anywhere, which the
/// wrapper builds in the caller's arena.
fn structHoldsHeap(unit: *const module_graph.ZigUnit, index: usize) bool {
    for (unit.structs[index].fields) |field| switch (field.type_info.base) {
        .String, .Array => return true,
        .Custom => if (structHoldsHeap(unit, structIndex(unit, field.type_info))) return true,
        else => {},
    };
    return false;
}

/// The Zig type a crossing struct's field has in its mirror.
fn mirrorFieldType(allocator: std.mem.Allocator, unit: *const module_graph.ZigUnit, t: ast.TypeInfo) ![]u8 {
    return switch (t.base) {
        .Int, .Enum => allocator.dupe(u8, "i64"),
        .Float => allocator.dupe(u8, "f64"),
        .Byte => allocator.dupe(u8, "u8"),
        .Tetra => allocator.dupe(u8, "bool"),
        .String => allocator.dupe(u8, "[]const u8"),
        .Array => blk: {
            const info = arrayInfoFor(t).?;
            break :blk std.fmt.allocPrint(allocator, "__DoxaArrayType({s}, {d})", .{ info.zig_type, info.depth });
        },
        .Custom => std.fmt.allocPrint(allocator, "__DoxaStruct_{d}", .{structIndex(unit, t)}),
        else => unreachable,
    };
}

/// The bridge's handling of a struct result. A Doxa struct's layout belongs to
/// the emitter, so the bridge never writes one: each leaf field (a nested
/// struct's fields in turn, depth-first in declaration order) comes back
/// through out-parameters of its own, and the emitter builds the struct from
/// them. A `string` leaf is cloned into the caller's arena as a
/// `(ptr, len)` pair, an array leaf built there as an `ArrayHeader`.
fn appendStructLeaves(
    allocator: std.mem.Allocator,
    unit: *const module_graph.ZigUnit,
    index: usize,
    access: []const u8,
    leaf: *usize,
    params: *std.array_list.Managed(u8),
    writes: *std.array_list.Managed(u8),
) !void {
    for (unit.structs[index].fields) |field| {
        const path = try std.fmt.allocPrint(allocator, "{s}.@\"{s}\"", .{ access, field.name });
        defer allocator.free(path);
        if (field.type_info.base == .Custom) {
            try appendStructLeaves(allocator, unit, structIndex(unit, field.type_info), path, leaf, params, writes);
            continue;
        }
        const k = leaf.*;
        leaf.* += 1;
        const decl, const write = switch (field.type_info.base) {
            .String => .{
                try std.fmt.allocPrint(allocator, ", o{d}_ptr: *?[*]u8, o{d}_len: *u64", .{ k, k }),
                try std.fmt.allocPrint(allocator, "    if ({s}.len == 0) {{ o{d}_ptr.* = null; o{d}_len.* = 0; }} else doxa_str_clone(__doxa_scope, {s}.ptr, {s}.len, o{d}_ptr, o{d}_len);\n", .{ path, k, k, path, path, k, k }),
            },
            .Array => blk: {
                const info = arrayInfoFor(field.type_info).?;
                break :blk .{
                    try std.fmt.allocPrint(allocator, ", o{d}: *?*__DoxaArrayHeader", .{k}),
                    try std.fmt.allocPrint(allocator, "    o{d}.* = __doxa_build(__doxa_scope, {s}, {d}, {d}, {d}, {s});\n", .{ k, info.zig_type, info.depth, info.elem_tag, info.elem_size, path }),
                };
            },
            else => blk: {
                const zig_type = try mirrorFieldType(allocator, unit, field.type_info);
                defer allocator.free(zig_type);
                break :blk .{
                    try std.fmt.allocPrint(allocator, ", o{d}: *{s}", .{ k, zig_type }),
                    try std.fmt.allocPrint(allocator, "    o{d}.* = {s};\n", .{ k, path }),
                };
            },
        };
        defer allocator.free(decl);
        defer allocator.free(write);
        try params.appendSlice(decl);
        try writes.appendSlice(write);
    }
}

fn errorUnionPayload(t: ast.TypeInfo) ?ast.TypeInfo {
    if (t.base != .Union) return null;
    for (t.union_type.?.types) |member| {
        if (member.base != .Enum) return member.*;
    }
    return null;
}

/// The `DoxaError_` spelling whose set has this identity, so a signature's
/// bridge names the same generated error set the source spelled.
fn errorSetPathFor(error_sets: []const module_graph.ZigErrorSet, ref: ast.TypeRef) ?[]const u8 {
    for (error_sets) |error_set| {
        if (error_set.ref.eql(ref)) return error_set.path;
    }
    return null;
}

/// Append the identifier of the generated `name -> discriminant` switch for an
/// error-set path: every character that cannot appear in an identifier becomes
/// `_` (`zerr.ZigError` -> `__doxa_error_index_zerr_ZigError`).
fn appendErrorIndexFn(buf: *std.array_list.Managed(u8), path: []const u8) !void {
    try buf.appendSlice("__doxa_error_index_");
    for (path) |c| try buf.append(if (std.ascii.isAlphanumeric(c)) c else '_');
}

/// Generic adapters, emitted once per wrapper. `__DoxaArrayType` names the Zig
/// slice type for `depth` nested levels; `__doxa_view` materializes a borrowed
/// view of an incoming `ArrayHeader` (aliasing scalar buffers, copying string
/// elements into the call arena); `__doxa_build` copies a returned slice into a
/// fresh `ArrayHeader` in the call-site arena.
fn appendArrayAdapters(buf: *std.array_list.Managed(u8)) !void {
    try buf.appendSlice("fn __DoxaArrayType(comptime __T: type, comptime __depth: usize) type {\n" ++
        "    if (__depth == 0) return __T;\n" ++
        "    return []const __DoxaArrayType(__T, __depth - 1);\n" ++
        "}\n\n" ++
        "fn __doxa_view(comptime __T: type, comptime __depth: usize, comptime __tag: u64, __h: *__DoxaArrayHeader, __a: __doxa_std.mem.Allocator) __DoxaArrayType(__T, __depth) {\n" ++
        "    const __n: usize = @intCast(doxa_array_len(__h));\n" ++
        "    if (__depth == 1) {\n" ++
        "        if (__tag == 3) {\n" ++
        "            const __buf = __a.alloc([]const u8, __n) catch return &.{};\n" ++
        "            var __i: usize = 0;\n" ++
        "            while (__i < __n) : (__i += 1) {\n" ++
        "                var __sp: ?[*]u8 = undefined;\n" ++
        "                var __sl: u64 = undefined;\n" ++
        "                doxa_array_get_str(__h, @intCast(__i), &__sp, &__sl);\n" ++
        "                __buf[__i] = if (__sp) |__p| __p[0..@intCast(__sl)] else \"\";\n" ++
        "            }\n" ++
        "            return __buf;\n" ++
        "        }\n" ++
        "        if (__n == 0) return &.{};\n" ++
        "        const __raw: [*]const __T = @ptrCast(@alignCast(doxa_array_data(__h) orelse return &.{}));\n" ++
        "        return __raw[0..__n];\n" ++
        "    }\n" ++
        "    const __out = __a.alloc(__DoxaArrayType(__T, __depth - 1), __n) catch return &.{};\n" ++
        "    var __i: usize = 0;\n" ++
        "    while (__i < __n) : (__i += 1) {\n" ++
        "        const __addr: usize = @intCast(doxa_array_get_i64(__h, @intCast(__i)));\n" ++
        "        __out[__i] = if (__addr == 0) &.{} else __doxa_view(__T, __depth - 1, __tag, @ptrFromInt(__addr), __a);\n" ++
        "    }\n" ++
        "    return __out;\n" ++
        "}\n\n" ++
        "fn __doxa_scalar_bits(comptime __T: type, __v: __T) i64 {\n" ++
        "    return switch (__T) {\n" ++
        "        i64 => __v,\n" ++
        "        f64 => @bitCast(__v),\n" ++
        "        u8 => @intCast(__v),\n" ++
        "        else => @compileError(\"unsupported inline-zig array element\"),\n" ++
        "    };\n" ++
        "}\n\n" ++
        "fn __doxa_build(__scope: *__DoxaScope, comptime __T: type, comptime __depth: usize, comptime __tag: u64, comptime __esize: u64, __value: __DoxaArrayType(__T, __depth)) *__DoxaArrayHeader {\n" ++
        "    const __h = if (__depth == 1) doxa_array_new(__scope, __esize, __tag, __value.len) else doxa_array_new(__scope, 8, 6, __value.len);\n" ++
        "    var __i: usize = 0;\n" ++
        "    while (__i < __value.len) : (__i += 1) {\n" ++
        "        if (__depth == 1) {\n" ++
        "            if (__tag == 3) {\n" ++
        "                var __p: ?[*]u8 = null;\n" ++
        "                var __l: u64 = 0;\n" ++
        "                doxa_str_clone(__scope, __value[__i].ptr, __value[__i].len, &__p, &__l);\n" ++
        "                doxa_array_set_str(__h, @intCast(__i), __p, __l);\n" ++
        "            } else {\n" ++
        "                doxa_array_set_i64(__h, @intCast(__i), __doxa_scalar_bits(__T, __value[__i]));\n" ++
        "            }\n" ++
        "        } else {\n" ++
        "            const __inner = __doxa_build(__scope, __T, __depth - 1, __tag, __esize, __value[__i]);\n" ++
        "            doxa_array_set_i64(__h, @intCast(__i), @intCast(@intFromPtr(__inner)));\n" ++
        "        }\n" ++
        "    }\n" ++
        "    return __h;\n" ++
        "}\n\n");
}

/// Borrow an incoming `ArrayHeader` as a Zig slice of type
/// `__DoxaArrayType(element, depth)`. Fresh allocations come from a per-call
/// arena the wrapper deinitializes after the user function returns.
fn arrayParamPrelude(allocator: std.mem.Allocator, i: usize, arg_name: []const u8, info: ArrayInfo) ![]u8 {
    return std.fmt.allocPrint(allocator, "    var __doxa_arena{d} = __doxa_std.heap.ArenaAllocator.init(__doxa_std.heap.page_allocator);\n" ++
        "    defer __doxa_arena{d}.deinit();\n" ++
        "    const __doxa_s{d}: __DoxaArrayType({s}, {d}) = if ({s}) |__h| __doxa_view({s}, {d}, {d}, __h, __doxa_arena{d}.allocator()) else &.{{}};\n", .{ i, i, i, info.zig_type, info.depth, arg_name, info.zig_type, info.depth, info.elem_tag, i });
}

/// Copy the user function's returned slice into a fresh `ArrayHeader` in the
/// caller's arena; string elements are cloned into that arena
/// (`doxa_str_clone`) before `doxa_array_set_str` stores them.
fn arrayReturnPostlude(allocator: std.mem.Allocator, info: ArrayInfo) ![]u8 {
    return std.fmt.allocPrint(allocator, "    const __doxa_arr = __doxa_build(__doxa_scope, {s}, {d}, {d}, {d}, __doxa_out);\n" ++
        "    return __doxa_arr;\n", .{ info.zig_type, info.depth, info.elem_tag, info.elem_size });
}

/// Collect every enum name a type references, recursing through array element
/// types. Enums cross as `i64`, so the wrapper only needs the names to inject
/// one `const DoxaEnum_<name> = i64;` alias per name the user's Zig source
/// spells.
/// Write the wrapper for one Zig unit: the user's source, the ABI prologue, and
/// one exported C-ABI bridge per function, named by its mangled link name so
/// two units' same-named functions never meet at link time.
fn generateWrapperZigFile(
    io: std.Io,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    cache_dir: []const u8,
    build_dir: []const u8,
    graph: *const ModuleGraph,
    record: *const ModuleRecord,
) !GeneratedModule {
    const Sha256 = std.crypto.hash.sha2.Sha256;

    const unit = record.zig.?;
    const sigs = unit.sigs;
    const location = unit.location;

    var file_buf = std.array_list.Managed(u8).init(allocator);
    defer file_buf.deinit();
    var cuts = std.array_list.Managed(SourceMap.Cut).init(allocator);
    defer cuts.deinit();
    try appendZigSourceSanitized(&file_buf, &cuts, unit.source);
    const source_len = file_buf.items.len;
    try file_buf.appendSlice("\n\n");

    // Inline-Zig ABI prologue. A wrapper returning a heap value takes the
    // caller's arena as a hidden first argument (`takesArena`): a string
    // return is cloned into it by the runtime and an array return is
    // materialized there as a fresh `ArrayHeader`, so both follow the ordinary
    // arena ownership rules in docs/memory.md with no free hook.
    // `DoxaByte` is the marker a signature uses to spell `byte[]`: a bare
    // `[]const u8` is unambiguously a `string`, so bytes need their own name.
    // The wrapper is its object's root, so its `std_options` govern the Zig
    // std compiled into it. A status Zig std does not map (`unexpectedStatus`,
    // `unexpectedErrno`) still returns `error.Unexpected` to the caller; it
    // no longer also dumps a Zig stack trace into the program's stderr.
    // TODO: Zig 0.16's `netReadWindows` maps no STATUS_LOCAL_DISCONNECT (B058).
    try file_buf.appendSlice("const __doxa_std = @import(\"std\");\n\n" ++
        "pub const std_options: __doxa_std.Options = .{ .unexpected_error_tracing = false };\n\n" ++
        "const DoxaByte = u8;\n\n" ++
        "const __DoxaArrayHeader = opaque {};\n\n" ++
        "const __DoxaScope = opaque {};\n\n" ++
        "extern fn doxa_str_clone(scope: *__DoxaScope, ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void;\n" ++
        "extern fn doxa_array_new(scope: *__DoxaScope, elem_size: u64, elem_tag: u64, init_len: u64) callconv(.c) *__DoxaArrayHeader;\n" ++
        "extern fn doxa_array_len(hdr: *__DoxaArrayHeader) callconv(.c) u64;\n" ++
        "extern fn doxa_array_data(hdr: ?*__DoxaArrayHeader) callconv(.c) ?[*]u8;\n" ++
        "extern fn doxa_array_get_i64(hdr: *__DoxaArrayHeader, idx: u64) callconv(.c) i64;\n" ++
        "extern fn doxa_array_get_str(hdr: *__DoxaArrayHeader, idx: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void;\n" ++
        "extern fn doxa_array_set_i64(hdr: *__DoxaArrayHeader, idx: u64, value: i64) callconv(.c) void;\n" ++
        "extern fn doxa_array_set_str(hdr: *__DoxaArrayHeader, idx: u64, str_ptr: ?[*]const u8, str_len: u64) callconv(.c) void;\n\n");
    try appendArrayAdapters(&file_buf);

    // A Doxa enum crosses as its `i64` discriminant. Declare every
    // `DoxaEnum_<path>` the source spells, as the quoted identifier it is, so
    // the Zig author compares against integers; analysis resolved each path
    // that appears in a signature.
    {
        const spellings = try inline_zig.doxaEnumSpellings(allocator, unit.source);
        defer allocator.free(spellings);
        for (spellings) |spelling| {
            try file_buf.appendSlice("const @\"");
            try file_buf.appendSlice(spelling);
            try file_buf.appendSlice("\" = i64;\n");
        }
    }

    // A fallible return (`DoxaError_<path>!void`) crosses as an `i64`: -1 for
    // success (Doxa `nothing`), else the variant discriminant. The shim body
    // returns against a Zig error set the wrapper synthesizes here, named after
    // the enum and carrying its variant names, and an index switch carries the
    // caught error back across the ABI. Analysis resolved each set's variants
    // and identity, so this is the only place the names are needed.
    for (unit.error_sets) |error_set| {
        try file_buf.appendSlice("const @\"DoxaError_");
        try file_buf.appendSlice(error_set.path);
        try file_buf.appendSlice("\" = error{");
        for (error_set.variants, 0..) |variant, i| {
            if (i > 0) try file_buf.appendSlice(",");
            try file_buf.appendSlice(" ");
            try file_buf.appendSlice(variant);
        }
        try file_buf.appendSlice(" };\n");

        try file_buf.appendSlice("fn ");
        try appendErrorIndexFn(&file_buf, error_set.path);
        try file_buf.appendSlice("(e: @\"DoxaError_");
        try file_buf.appendSlice(error_set.path);
        try file_buf.appendSlice("\") i64 {\n    return switch (e) {\n");
        for (error_set.variants, 0..) |variant, i| {
            const arm = try std.fmt.allocPrint(allocator, "        error.{s} => {d},\n", .{ variant, i });
            defer allocator.free(arm);
            try file_buf.appendSlice(arm);
        }
        try file_buf.appendSlice("    };\n}\n");
    }

    // A struct result's Zig mirror, built from the Doxa declaration analysis
    // checked: the body returns it like any Zig struct, and the two cannot
    // drift. A struct precedes any that holds it, and each spelling a
    // signature names it by is an alias (`plan/inline-zig-structs.md`).
    for (unit.structs, 0..) |s, i| {
        const head = try std.fmt.allocPrint(allocator, "const __DoxaStruct_{d} = struct {{", .{i});
        defer allocator.free(head);
        try file_buf.appendSlice(head);
        for (s.fields) |field| {
            const zig_type = try mirrorFieldType(allocator, &unit, field.type_info);
            defer allocator.free(zig_type);
            const member = try std.fmt.allocPrint(allocator, " @\"{s}\": {s},", .{ field.name, zig_type });
            defer allocator.free(member);
            try file_buf.appendSlice(member);
        }
        try file_buf.appendSlice(" };\n");
        for (s.aliases) |alias| {
            const line = try std.fmt.allocPrint(allocator, "const @\"DoxaStruct_{s}\" = __DoxaStruct_{d};\n", .{ alias, i });
            defer allocator.free(line);
            try file_buf.appendSlice(line);
        }
    }

    for (sigs) |sig| {
        const native_ident = try std.fmt.allocPrint(allocator, "__doxa_native__{s}", .{sig.name});
        defer allocator.free(native_ident);
        const native_sym = try graph.mangle(allocator, record.id, .function, &.{sig.name});
        defer allocator.free(native_sym);

        const native_ret_zig = if (errorUnionRef(sig.return_type) != null)
            "i64"
        else if (sig.return_type.base == .Custom)
            "void"
        else
            nativeReturnType(sig.return_type) orelse {
                reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
                return error.NotImplemented;
            };
        const fallible_payload: ?ast.TypeInfo = if (errorUnionRef(sig.return_type) != null)
            errorUnionPayload(sig.return_type)
        else
            null;

        var native_buf = std.array_list.Managed(u8).init(allocator);
        defer native_buf.deinit();
        try native_buf.appendSlice("pub fn ");
        try native_buf.appendSlice(native_ident);
        try native_buf.appendSlice("(");
        const takes_arena = takesArena(&sig);
        if (takes_arena) {
            try native_buf.appendSlice("__doxa_scope: *__DoxaScope");
            if (sig.param_types.len > 0) try native_buf.appendSlice(", ");
        }

        var native_prelude = std.array_list.Managed(u8).init(allocator);
        defer native_prelude.deinit();
        var native_call = std.array_list.Managed(u8).init(allocator);
        defer native_call.deinit();
        try native_call.appendSlice(sig.name);
        try native_call.appendSlice("(");

        for (sig.param_types, 0..) |pt, i| {
            const pt_zig = nativeParamType(pt) orelse {
                reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported param type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
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
            } else if (pt.base == .Array) {
                const elem = arrayInfoFor(pt) orelse {
                    reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported array element type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
                    return error.NotImplemented;
                };
                const s_name = try std.fmt.allocPrint(allocator, "__doxa_s{}", .{i});
                defer allocator.free(s_name);
                const prelude = try arrayParamPrelude(allocator, i, arg_name, elem);
                defer allocator.free(prelude);
                try native_prelude.appendSlice(prelude);
                try native_call.appendSlice(s_name);
            } else {
                try native_call.appendSlice(arg_name);
            }
        }

        const has_params = sig.param_types.len > 0 or takes_arena;
        if (sig.return_type.base == .String) {
            if (has_params) try native_buf.appendSlice(", ");
            try native_buf.appendSlice("out_ptr: *?[*]u8, out_len: *u64");
        }
        // A fallible payload crosses through trailing out-parameters, so the
        // i64 return is free to carry the sentinel (-1 = success) or the error.
        if (fallible_payload) |payload| {
            const out_params: ?[]const u8 = switch (payload.base) {
                // A struct payload's out-parameters are its leaves, below.
                .Nothing, .Custom => null,
                .String => "out_ptr: *?[*]u8, out_len: *u64",
                .Array => "out_array: *?*__DoxaArrayHeader",
                .Int => "out_value: *i64",
                // TODO: a float/byte/tetra payload needs its own out-param type
                // and a matching load in `emitFallibleZigCall`.
                else => {
                    reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported fallible payload type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
                    return error.NotImplemented;
                },
            };
            if (out_params) |decl| {
                if (has_params) try native_buf.appendSlice(", ");
                try native_buf.appendSlice(decl);
            }
        }
        var struct_writes = std.array_list.Managed(u8).init(allocator);
        defer struct_writes.deinit();
        if (resultType(&sig)) |result| if (result.base == .Custom) {
            var struct_params = std.array_list.Managed(u8).init(allocator);
            defer struct_params.deinit();
            // The caller builds the struct in its arena; the bridge uses the
            // arena itself only for a string or array leaf.
            if (!structHoldsHeap(&unit, structIndex(&unit, result))) try struct_writes.appendSlice("    _ = __doxa_scope;\n");
            var leaf: usize = 0;
            try appendStructLeaves(allocator, &unit, structIndex(&unit, result), "__doxa_out", &leaf, &struct_params, &struct_writes);
            // Each leaf's declaration leads with its separator.
            try native_buf.appendSlice(if (has_params) struct_params.items else struct_params.items[@min(2, struct_params.items.len)..]);
        };
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
            try native_buf.appendSlice("    doxa_str_clone(__doxa_scope, __doxa_out.ptr, __doxa_out.len, out_ptr, out_len);\n");
        } else if (errorUnionRef(sig.return_type)) |ref| {
            // Success crosses as -1 (`nothing`), a caught error as its variant
            // discriminant; a payload, if any, is written to the out-params.
            const path = errorSetPathFor(unit.error_sets, ref) orelse {
                reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unresolved error set for '{s}.{s}'", .{ unit.name, sig.name });
                return error.NotImplemented;
            };
            const payload = fallible_payload orelse ast.TypeInfo{ .base = .Nothing };
            if (payload.base == .Nothing) {
                try native_buf.appendSlice("    ");
            } else {
                try native_buf.appendSlice("    const __doxa_out = ");
            }
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(" catch |__doxa_e| return ");
            try appendErrorIndexFn(&native_buf, path);
            try native_buf.appendSlice("(__doxa_e);\n");

            switch (payload.base) {
                .Nothing => {},
                .String => try native_buf.appendSlice("    if (__doxa_out.len == 0) { out_ptr.* = null; out_len.* = 0; return -1; }\n" ++
                    "    doxa_str_clone(__doxa_scope, __doxa_out.ptr, __doxa_out.len, out_ptr, out_len);\n"),
                .Array => {
                    const elem = arrayInfoFor(payload) orelse {
                        reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported fallible array payload for '{s}.{s}'", .{ unit.name, sig.name });
                        return error.NotImplemented;
                    };
                    const postlude = try std.fmt.allocPrint(allocator, "    out_array.* = __doxa_build(__doxa_scope, {s}, {d}, {d}, {d}, __doxa_out);\n", .{ elem.zig_type, elem.depth, elem.elem_tag, elem.elem_size });
                    defer allocator.free(postlude);
                    try native_buf.appendSlice(postlude);
                },
                .Int => try native_buf.appendSlice("    out_value.* = __doxa_out;\n"),
                .Custom => try native_buf.appendSlice(struct_writes.items),
                else => {},
            }
            try native_buf.appendSlice("    return -1;\n");
        } else if (sig.return_type.base == .Custom) {
            try native_buf.appendSlice("    const __doxa_out = ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
            try native_buf.appendSlice(struct_writes.items);
        } else if (sig.return_type.base == .Array) {
            const elem = arrayInfoFor(sig.return_type) orelse {
                reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported array element type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
                return error.NotImplemented;
            };
            try native_buf.appendSlice("    const __doxa_out = ");
            try native_buf.appendSlice(native_call.items);
            try native_buf.appendSlice(";\n");
            const postlude = try arrayReturnPostlude(allocator, elem);
            defer allocator.free(postlude);
            try native_buf.appendSlice(postlude);
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

    // The wrapper is named, and its object keyed (`shimObjectKey`), by the
    // text written here, not by the block's source alone: the prologue also
    // carries what the program's Doxa declarations fix, such as an error set's
    // variant order, so editing one of those must build a new object.
    // Every file the unit imports is built too, so each one's spelling and
    // content join the hash, length-framed and in closure order.
    const imported = try importClosure(io, allocator, unit);
    errdefer {
        for (imported) |file| {
            allocator.free(file.path);
            allocator.free(file.source);
        }
        allocator.free(imported);
    }
    var h: [Sha256.digest_length]u8 = undefined;
    var hasher = Sha256.init(.{});
    hasher.update(cacheSeed());
    hasher.update("\n");
    hasher.update(file_buf.items);
    for (imported) |file| {
        for ([_][]const u8{ file.spelling, file.source }) |part| {
            hasher.update(std.mem.asBytes(&@as(u64, part.len)));
            hasher.update(part);
        }
    }
    hasher.final(&h);
    const hex_buf = std.fmt.bytesToHex(h, .lower);
    const zig_path = try std.fmt.allocPrint(allocator, "{s}/{s}-{s}.zig", .{ cache_dir, unit.name, hex_buf[0..16] });
    errdefer allocator.free(zig_path);

    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = zig_path, .data = file_buf.items });

    return .{
        .zig_path = zig_path,
        .source_map = .{ .cuts = try cuts.toOwnedSlice(), .source_len = source_len },
        .imported = imported,
        .build_dir = build_dir,
    };
}

/// Compile every inline-Zig unit of the program to an object. `cache_dir` and
/// `include_dirs` are absolute: each `zig build-obj` runs inside the cache
/// directory, as every toolchain step does (`compileToNative`).
pub const ShimKeyInputs = struct {
    toolchain: []const u8,
    zig_path: []const u8,
    target_triple: []const u8,
    /// Identity of the CPU the shim is compiled for — never the constant
    /// `-mcpu=native` arg.
    cpu_key: []const u8,
    zig_opt_flag: []const u8,
    include_dirs: []const []const u8,
};

/// Content-addressed cache key for a shim object.
///
/// The CPU contributes as `cpu_key`, the *resolved* host identity, never the
/// `cpu_arg` the process is spawned with. Those differ for a native build:
/// `cpu_arg` is the literal `-mcpu=native` on every host, so keying on it
/// would let a shim built on one machine be linked on another that lacks the
/// instructions it contains — an illegal-instruction trap surfacing as a
/// crash in unrelated code, with nothing in the cache to explain it. A cross
/// build contributes `""`, because its target's baseline CPU is already fixed
/// by the triple and the toolchain.
///
/// Every field is a build input, so any of them changing produces a distinct
/// object; all of them holding means the `build-obj` spawn is skipped.
pub fn shimObjectKey(inputs: ShimKeyInputs) hashing.ShortHex {
    var kb = hashing.KeyBuilder.init(cacheSeed());
    kb.addBytes(inputs.toolchain);
    kb.addBytes(inputs.zig_path);
    kb.addBytes(inputs.target_triple);
    kb.addBytes(inputs.cpu_key);
    kb.addBytes(inputs.zig_opt_flag);
    for (inputs.include_dirs) |dir| {
        kb.addBytes(dir);
    }
    return hashing.shortHexOf(kb.finish());
}

pub fn compileInlineZigObjects(
    io: std.Io,
    memoryManager: *MemoryManager,
    graph: *const ModuleGraph,
    reporter: *Reporter,
    zig_exe_path: []const u8,
    cache_dir: []const u8,
    zig_opt_flag: []const u8,
    target_triple: []const u8,
    target_os: []const u8,
    cpu_arg: []const u8,
    cpu_key: []const u8,
    include_dirs: []const []const u8,
    toolchain: []const u8,
    profiler: *Profiler,
) ![]const []const u8 {
    const units = try programZigUnits(memoryManager.getAllocator(), graph);
    defer memoryManager.getAllocator().free(units);

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{cache_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.Io.Dir.cwd().createDirPath(io, zig_cache_path);
    // Zig's own local cache for the shim builds, where `build-obj --listen`
    // writes each object before it is copied to its published name.
    const zig_local_cache = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/build-obj", .{cache_dir});
    defer memoryManager.getAllocator().free(zig_local_cache);

    var out_paths = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
    errdefer {
        for (out_paths.items) |p| memoryManager.getAllocator().free(@constCast(p));
        out_paths.deinit();
    }

    for (units) |record| {
        profiler.begin("wrapper-gen");
        var gen = try generateWrapperZigFile(io, memoryManager.getAllocator(), reporter, zig_cache_path, cache_dir, graph, record);
        profiler.end();
        defer gen.deinit(memoryManager.getAllocator());

        // The object extension follows the target, not the host.
        const obj_ext = if (std.mem.eql(u8, target_os, "windows")) "obj" else "o";
        const zig_dir = std.fs.path.dirname(gen.zig_path) orelse ".";
        const zig_stem = std.fs.path.stem(gen.zig_path);

        const key_hex = shimObjectKey(.{
            .toolchain = toolchain,
            .zig_path = gen.zig_path,
            .target_triple = target_triple,
            .cpu_key = cpu_key,
            .zig_opt_flag = zig_opt_flag,
            .include_dirs = include_dirs,
        });

        try out_paths.ensureUnusedCapacity(1);
        const obj_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/{s}.{s}.{s}", .{ zig_dir, zig_stem, key_hex[0..], obj_ext });
        out_paths.appendAssumeCapacity(obj_path);

        const obj_already_compiled = blk: {
            const f = std.Io.Dir.cwd().openFile(io, obj_path, .{}) catch break :blk false;
            defer f.close(io);
            const stat = f.stat(io) catch break :blk false;
            break :blk stat.kind == .file and stat.size > 0;
        };

        if (!obj_already_compiled) {
            profiler.begin("build-obj");
            defer profiler.end();
            // Zig's compiler-server protocol (`--listen=-`) hands back errors
            // as a structured bundle that maps onto the unit's own source. It
            // forbids an explicit output path: Zig builds into the local cache
            // named here and reports the object's digest.
            var args_list = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
            defer args_list.deinit();
            var arg_arena = std.heap.ArenaAllocator.init(memoryManager.getAllocator());
            defer arg_arena.deinit();
            try args_list.appendSlice(&[_][]const u8{
                zig_exe_path,
                "build-obj",
                "--cache-dir",
                zig_local_cache,
                "--listen=-",
                // The full trace, so an error raised inside Zig's std still
                // reaches the unit's call into it. It costs only on failure.
                "-freference-trace",
            });

            // A module's options precede the `-M` that declares it. The
            // wrapper is the root module, named for its stem so the object is
            // too; each file the block imports is a module named as the block
            // spells it, read in place. Zig resolves what those import in turn.
            var module_flags = std.array_list.Managed([]const u8).init(arg_arena.allocator());
            try module_flags.append(zig_opt_flag);
            if (target_triple.len > 0) try module_flags.appendSlice(&.{ "-target", target_triple });
            if (cpu_arg.len > 0) try module_flags.append(cpu_arg);
            // The build module's `--include` dirs apply to every module's
            // @cImport, matching how the artifact's `zig cc` step consumes them.
            for (include_dirs) |dir| {
                try module_flags.append(try std.fmt.allocPrint(arg_arena.allocator(), "-I{s}", .{dir}));
            }

            const imports = record.zig.?.imports;
            try args_list.appendSlice(module_flags.items);
            for (imports) |import| try args_list.appendSlice(&.{ "--dep", import.spelling });
            try args_list.append(try std.fmt.allocPrint(arg_arena.allocator(), "-M{s}={s}", .{ zig_stem, gen.zig_path }));
            for (imports) |import| {
                try args_list.appendSlice(module_flags.items);
                try args_list.append(try std.fmt.allocPrint(arg_arena.allocator(), "-M{s}={s}", .{ import.spelling, import.path }));
            }
            // Every object linked into a Doxa program must be compiled with the
            // same libc-ness as the runtime root object (`src/build/native.zig`, which
            // passes `-lc`) and the final link (also `-lc`). Without this, an
            // inline shim's `builtin.link_libc` is false while the program still
            // links libc: `std.Thread` then selects `LinuxThreadImpl` and reads
            // the uninitialized `std.os.linux.tls.area_desc` under glibc's
            // startup, aborting on an invalid-alignment assertion. See
            // `plan/libc-dependence.md`.
            try args_list.append("-lc");

            var built = try buildObject(io, memoryManager.getAllocator(), args_list.items, cache_dir);
            defer built.errors.deinit(memoryManager.getAllocator());
            if (built.errors.errorMessageCount() > 0) {
                try reportZigErrors(io, memoryManager.getAnalysisAllocator(), reporter, built.errors, record.zig.?, gen);
                return error.InlineZigCompileFailed;
            }
            const digest = built.digest orelse return error.ZigCompilerCrashed;

            // Only the final rename may write the published name: the object
            // is copied to `obj_path.tmp` and moved into place once complete.
            // A shim truncated by an interrupted copy would otherwise be
            // trusted by the `size > 0` check above on every later run.
            const built_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/o/{s}/{s}.{s}", .{ zig_local_cache, digest[0..], zig_stem, obj_ext });
            defer memoryManager.getAllocator().free(built_path);
            const staging_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}.tmp", .{obj_path});
            defer memoryManager.getAllocator().free(staging_path);
            try std.Io.Dir.cwd().copyFile(built_path, std.Io.Dir.cwd(), staging_path, io, .{});
            try std.Io.Dir.cwd().rename(staging_path, std.Io.Dir.cwd(), obj_path, io);
        }
    }

    return try out_paths.toOwnedSlice();
}

const BuiltObject = struct {
    /// Empty when the object was built.
    errors: std.zig.ErrorBundle,
    /// The object's directory under the local cache, `o/<digest>`.
    digest: ?std.Build.Cache.HexDigest,
};

/// Run one `zig build-obj --listen=-` to completion over the compiler-server
/// protocol. The compiler is the bundled Zig this compiler was built with, so
/// the protocol on both ends is the same version. Its stderr is inherited:
/// under `--listen` only a crash writes there.
fn buildObject(io: std.Io, allocator: std.mem.Allocator, argv: []const []const u8, cwd: []const u8) !BuiltObject {
    var child = try std.process.spawn(io, .{
        .argv = argv,
        .cwd = .{ .path = cwd },
        .stdin = .pipe,
        .stdout = .pipe,
        .stderr = .inherit,
    });
    errdefer child.kill(io);

    {
        var stdin_writer = child.stdin.?.writer(io, &.{});
        const w = &stdin_writer.interface;
        inline for (.{ .update, .exit }) |tag| {
            const header: std.zig.Client.Message.Header = .{ .tag = tag, .bytes_len = 0 };
            w.writeStruct(header, .little) catch return stdin_writer.err.?;
        }
        child.stdin.?.close(io);
        child.stdin = null;
    }

    var bundle: std.zig.ErrorBundle = .empty;
    errdefer bundle.deinit(allocator);
    var digest: ?std.Build.Cache.HexDigest = null;

    var read_buf: [4096]u8 = undefined;
    var stdout_reader = child.stdout.?.reader(io, &read_buf);
    const r = &stdout_reader.interface;
    while (true) {
        const header = r.takeStruct(std.zig.Server.Message.Header, .little) catch |err| switch (err) {
            error.EndOfStream => break,
            error.ReadFailed => return stdout_reader.err.?,
        };
        const body = r.readAlloc(allocator, header.bytes_len) catch |err| switch (err) {
            error.ReadFailed => return stdout_reader.err.?,
            else => |e| return e,
        };
        defer allocator.free(body);
        switch (header.tag) {
            .error_bundle => {
                bundle.deinit(allocator);
                bundle = try std.zig.Server.allocErrorBundle(allocator, body);
            },
            .emit_digest => {
                const Cache = std.Build.Cache;
                const prefix = @sizeOf(std.zig.Server.Message.EmitDigest);
                if (body.len < prefix + Cache.bin_digest_len) return error.ZigProtocolViolation;
                digest = Cache.binToHex(body[prefix..][0..Cache.bin_digest_len].*);
            },
            else => {},
        }
    }

    const term = try child.wait(io);
    const exited_cleanly = switch (term) {
        .exited => |code| code == 0,
        else => false,
    };
    // A failure that sent no bundle is a crash; its stderr is already out.
    if (!exited_cleanly and bundle.errorMessageCount() == 0) return error.ZigCompilerCrashed;
    return .{ .errors = bundle, .digest = digest };
}

/// Report each of a unit's Zig compile errors where it stands in the unit's
/// own file. An error inside the copied source maps exactly through the
/// wrapper's `SourceMap`. One raised inside Zig's std (a bad call into it) is
/// reported at the unit's frame nearest it in Zig's reference trace, with the
/// std location attached. One in the generated bridge is a Doxa bug. Zig's
/// notes follow their error as related information, and an error Zig repeats
/// for each instantiation is reported once. `arena` holds the paths the
/// diagnostics borrow.
fn reportZigErrors(
    io: std.Io,
    arena: std.mem.Allocator,
    reporter: *Reporter,
    bundle: std.zig.ErrorBundle,
    unit: module_graph.ZigUnit,
    gen: GeneratedModule,
) !void {
    const unit_location = unit.location orelse unit.origin;
    const Reported = struct { range: Reporting.Range, text: []const u8 };
    var reported = std.array_list.Managed(Reported).init(arena);

    for (bundle.getMessages()) |message_index| {
        const message = bundle.getErrorMessage(message_index);
        const text = bundle.nullTerminatedString(message.msg);

        const notes = bundle.getNotes(message_index);
        var related = try std.array_list.Managed(Reporting.RelatedInformation).initCapacity(arena, notes.len + 1);

        const where = try zigLocation(io, arena, reporter, bundle, message.src_loc, unit, gen);
        const kind = if (where) |w| w.kind else .generated;
        const location = switch (kind) {
            .unit, .imported => where.?.location,
            .zig_std => blk: {
                related.appendAssumeCapacity(.{ .message = "raised here, in Zig's std", .location = where.?.location });
                break :blk try callSite(io, arena, reporter, bundle, message.src_loc, unit, gen) orelse unit_location;
            },
            .generated => unit_location,
        };

        for (notes) |note_index| {
            const note = bundle.getErrorMessage(note_index);
            const note_where = try zigLocation(io, arena, reporter, bundle, note.src_loc, unit, gen) orelse continue;
            related.appendAssumeCapacity(.{ .message = bundle.nullTerminatedString(note.msg), .location = note_where.location });
        }

        const seen = for (reported.items) |r| {
            if (std.meta.eql(r.range, location.range) and std.mem.eql(u8, r.text, text)) break true;
        } else false;
        if (seen) continue;
        try reported.append(.{ .range = location.range, .text = text });

        if (kind == .generated) {
            reporter.reportWithRelated(.CompileTime, .Error, location, ErrorCode.INTERNAL_ERROR, related.items, "inline zig: the generated bridge for '{s}' does not compile: {s}", .{ unit.name, text });
        } else {
            reporter.reportWithRelated(.CompileTime, .Error, location, ErrorCode.INLINE_ZIG_COMPILE_ERROR, related.items, "{s}", .{text});
        }
    }
}

/// The first frame of a source location's reference trace that lies in the
/// unit's own source: where the unit's code reached the failing std code.
/// `ErrorBundle` keeps its `extra` reader private; the layout it documents is
/// a `SourceLocation`'s fields followed by `reference_trace_len`
/// `ReferenceTrace`s, each a flat run of `u32`s.
fn callSite(
    io: std.Io,
    arena: std.mem.Allocator,
    reporter: *Reporter,
    bundle: std.zig.ErrorBundle,
    index: std.zig.ErrorBundle.SourceLocationIndex,
    unit: module_graph.ZigUnit,
    gen: GeneratedModule,
) !?Reporting.Location {
    const EB = std.zig.ErrorBundle;
    const loc_words = @typeInfo(EB.SourceLocation).@"struct".fields.len;
    const trace_words = @typeInfo(EB.ReferenceTrace).@"struct".fields.len;
    const loc = bundle.getSourceLocation(index);
    var at = @intFromEnum(index) + loc_words;
    for (0..loc.reference_trace_len) |_| {
        const frame_loc: EB.SourceLocationIndex = @enumFromInt(bundle.extra[at + 1]);
        at += trace_words;
        const frame = try zigLocation(io, arena, reporter, bundle, frame_loc, unit, gen) orelse continue;
        if (frame.kind == .unit or frame.kind == .imported) return frame.location;
    }
    return null;
}

const ZigLocation = struct {
    kind: enum { unit, imported, zig_std, generated },
    location: Reporting.Location,
};

/// Where a bundle source location stands: in the unit's own source, in a file
/// of Zig's std, or in the generated bridge. Null when Zig gave none.
fn zigLocation(
    io: std.Io,
    arena: std.mem.Allocator,
    reporter: *Reporter,
    bundle: std.zig.ErrorBundle,
    index: std.zig.ErrorBundle.SourceLocationIndex,
    unit: module_graph.ZigUnit,
    gen: GeneratedModule,
) !?ZigLocation {
    if (index == .none) return null;
    const loc = bundle.getSourceLocation(index);
    const path = bundle.nullTerminatedString(loc.src_path);

    if (!std.mem.eql(u8, std.fs.path.basename(path), std.fs.path.basename(gen.zig_path))) {
        const resolved = try std.fs.path.resolve(arena, &.{ gen.build_dir, path });
        const imported = for (gen.imported) |file| {
            if (!samePath(file.path, resolved)) continue;
            // So the diagnostic can show the line.
            if (reporter.source_cache) |cache| cache.load(resolved, file.source) catch {};
            break true;
        } else false;
        const file = if (imported) resolved else try arena.dupe(u8, path);
        const span_len = loc.span_end -| loc.span_main;
        return .{ .kind = if (imported) .imported else .zig_std, .location = .{
            .file = file,
            .file_uri = reporter.ensureFileUri(io, file) catch null,
            .range = .{
                .start_line = loc.line + 1,
                .start_col = loc.column + 1,
                .end_line = loc.line + 1,
                .end_col = loc.column + 1 + span_len,
            },
        } };
    }

    const start = gen.source_map.toSource(loc.span_main) orelse
        return .{ .kind = .generated, .location = unit.location orelse unit.origin };
    const end = gen.source_map.toSource(@max(loc.span_main, loc.span_end)) orelse start;
    return .{ .kind = .unit, .location = .{
        .file = unit.origin.file,
        .file_uri = unit.origin.file_uri,
        .range = rangeIn(unit, start, end),
    } };
}

/// Whether two resolved paths name the same file: Windows paths compare
/// without case.
fn samePath(a: []const u8, b: []const u8) bool {
    if (builtin.os.tag == .windows) return std.ascii.eqlIgnoreCase(a, b);
    return std.mem.eql(u8, a, b);
}

/// The file range of `source[start..end]`, placed through the unit's origin:
/// the origin's own line continues past its column; later lines start at
/// column 1.
fn rangeIn(unit: module_graph.ZigUnit, start: usize, end: usize) Reporting.Range {
    const a = positionIn(unit, start);
    const b = positionIn(unit, end);
    return .{ .start_line = a.line, .start_col = a.col, .end_line = b.line, .end_col = b.col };
}

fn positionIn(unit: module_graph.ZigUnit, offset: usize) struct { line: usize, col: usize } {
    const before = unit.source[0..@min(offset, unit.source.len)];
    const newlines = std.mem.count(u8, before, "\n");
    const line_start = if (std.mem.lastIndexOfScalar(u8, before, '\n')) |nl| nl + 1 else 0;
    const col = before.len - line_start;
    if (newlines == 0) return .{ .line = unit.origin.range.start_line, .col = unit.origin.range.start_col + col };
    return .{ .line = unit.origin.range.start_line + newlines, .col = col + 1 };
}
