const std = @import("std");

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
/// and the tags in `src/runtime/doxa_rt.zig` — they must stay in sync.
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

fn nativeParamType(t: ast.TypeInfo) ?[]const u8 {
    return switch (t.base) {
        .Int => "i64",
        .Float => "f64",
        .Byte => "u8",
        .Tetra => "bool",
        .Nothing => "void",
        .String => "?[*]const u8",
        .Array => "?*__DoxaArrayHeader",
        .Enum => "i64",
        else => null,
    };
}

fn nativeReturnType(t: ast.TypeInfo) ?[]const u8 {
    return switch (t.base) {
        .Int => "i64",
        .Float => "f64",
        .Byte => "u8",
        .Tetra => "bool",
        .Nothing => "void",
        // String returns cross through the (out_ptr, out_len) pair instead.
        .String => "void",
        .Array => "?*__DoxaArrayHeader",
        .Enum => "i64",
        else => null,
    };
}

/// Generic adapters, emitted once per wrapper. `__DoxaArrayType` names the Zig
/// slice type for `depth` nested levels; `__doxa_view` materializes a borrowed
/// view of an incoming `ArrayHeader` (aliasing scalar buffers, copying string
/// elements into the call arena); `__doxa_build` copies a returned slice into a
/// fresh `ArrayHeader` in the call-site arena.
fn appendArrayAdapters(buf: *std.array_list.Managed(u8)) !void {
    try buf.appendSlice(
        "fn __DoxaArrayType(comptime __T: type, comptime __depth: usize) type {\n" ++
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
        "fn __doxa_build(comptime __T: type, comptime __depth: usize, comptime __tag: u64, comptime __esize: u64, __value: __DoxaArrayType(__T, __depth)) *__DoxaArrayHeader {\n" ++
        "    const __h = if (__depth == 1) doxa_array_new(__esize, __tag, __value.len) else doxa_array_new(8, 6, __value.len);\n" ++
        "    var __i: usize = 0;\n" ++
        "    while (__i < __value.len) : (__i += 1) {\n" ++
        "        if (__depth == 1) {\n" ++
        "            if (__tag == 3) {\n" ++
        "                doxa_array_set_str(__h, @intCast(__i), __value[__i].ptr, __value[__i].len);\n" ++
        "            } else {\n" ++
        "                doxa_array_set_i64(__h, @intCast(__i), __doxa_scalar_bits(__T, __value[__i]));\n" ++
        "            }\n" ++
        "        } else {\n" ++
        "            const __inner = __doxa_build(__T, __depth - 1, __tag, __esize, __value[__i]);\n" ++
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
/// call-site arena; string elements are cloned into that arena by
/// `doxa_array_set_str`.
fn arrayReturnPostlude(allocator: std.mem.Allocator, info: ArrayInfo) ![]u8 {
    return std.fmt.allocPrint(allocator, "    const __doxa_arr = __doxa_build({s}, {d}, {d}, {d}, __doxa_out);\n" ++
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
    graph: *const ModuleGraph,
    record: *const ModuleRecord,
) !GeneratedModule {
    const Sha256 = std.crypto.hash.sha2.Sha256;

    const unit = record.zig.?;
    const sigs = unit.sigs;
    const location = unit.location;

    // The wrapper's content is its source plus the link names it exports,
    // which the record's mangling tag fixes.
    var h: [Sha256.digest_length]u8 = undefined;
    var hasher = Sha256.init(.{});
    hasher.update(cacheSeed());
    hasher.update("\n");
    hasher.update(unit.name);
    hasher.update("\n");
    hasher.update(record.mangle_tag.?);
    hasher.update("\n");
    hasher.update(unit.source);
    hasher.final(&h);

    const hex_buf = std.fmt.bytesToHex(h, .lower);
    const short_hex = hex_buf[0..16];

    var zig_path_buf: [256]u8 = undefined;
    const zig_path = try std.fmt.bufPrint(&zig_path_buf, "{s}/{s}-{s}.zig", .{ cache_dir, unit.name, short_hex });

    var file_buf = std.array_list.Managed(u8).init(allocator);
    defer file_buf.deinit();
    try appendZigSourceSanitized(&file_buf, unit.source);
    try file_buf.appendSlice("\n\n");

    // Inline-Zig ABI prologue. String returns are cloned into the call-site
    // scope arena by the runtime; array returns are materialized as a fresh
    // `ArrayHeader` in that same arena by `doxa_array_new`, so both follow the
    // ordinary arena ownership rules in docs/memory.md with no free hook.
    // `DoxaByte` is the marker a signature uses to spell `byte[]`: a bare
    // `[]const u8` is unambiguously a `string`, so bytes need their own name.
    try file_buf.appendSlice(
        "const __doxa_std = @import(\"std\");\n\n" ++
        "const DoxaByte = u8;\n\n" ++
        "const __DoxaArrayHeader = opaque {};\n\n" ++
        "extern fn doxa_str_clone_current(ptr: ?[*]const u8, len: u64, out_ptr: *?[*]u8, out_len: *u64) callconv(.c) void;\n" ++
        "extern fn doxa_array_new(elem_size: u64, elem_tag: u64, init_len: u64) callconv(.c) *__DoxaArrayHeader;\n" ++
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

    for (sigs) |sig| {
        const native_ident = try std.fmt.allocPrint(allocator, "__doxa_native__{s}", .{sig.name});
        defer allocator.free(native_ident);
        const native_sym = try graph.mangle(allocator, record.id, .function, &.{sig.name});
        defer allocator.free(native_sym);

        const native_ret_zig = nativeReturnType(sig.return_type) orelse {
            reporter.reportCompileError(location, ErrorCode.NOT_IMPLEMENTED, "inline zig: unsupported return type in native bridge for '{s}.{s}'", .{ unit.name, sig.name });
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

    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = zig_path, .data = file_buf.items });

    return .{
        .zig_path = try allocator.dupe(u8, zig_path),
    };
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
    include_dirs: []const []const u8,
    toolchain: []const u8,
    profiler: *Profiler,
) ![]const []const u8 {
    const units = try programZigUnits(memoryManager.getAllocator(), graph);
    defer memoryManager.getAllocator().free(units);

    const zig_cache_path = try std.fmt.allocPrint(memoryManager.getAllocator(), "{s}/zig/cache", .{cache_dir});
    defer memoryManager.getAllocator().free(zig_cache_path);
    try std.Io.Dir.cwd().createDirPath(io, zig_cache_path);

    var out_paths = std.array_list.Managed([]const u8).init(memoryManager.getAllocator());
    errdefer {
        for (out_paths.items) |p| memoryManager.getAllocator().free(@constCast(p));
        out_paths.deinit();
    }

    for (units) |record| {
        profiler.begin("wrapper-gen");
        var gen = try generateWrapperZigFile(io, memoryManager.getAllocator(), reporter, zig_cache_path, graph, record);
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
