const std = @import("std");
const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;

const PeekStringInfo = struct {
    name: []const u8,
    length: usize,
};

fn escapeLLVMString(allocator: std.mem.Allocator, text: []const u8) ![]u8 {
    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(allocator);
    const hex = "0123456789ABCDEF";
    for (text) |ch| {
        if (ch >= 32 and ch <= 126 and ch != '"' and ch != '\\') {
            try buffer.append(allocator, ch);
        } else {
            try buffer.append(allocator, '\\');
            try buffer.append(allocator, hex[(ch >> 4) & 0xF]);
            try buffer.append(allocator, hex[ch & 0xF]);
        }
    }
    return buffer.toOwnedSlice(allocator);
}

fn internPeekString(
    allocator: std.mem.Allocator,
    map: *std.StringHashMap(usize),
    strings: *std.array_list.Managed(PeekStringInfo),
    next_id: *usize,
    globals: *std.array_list.Managed([]const u8),
    value: []const u8,
) !PeekStringInfo {
    if (map.get(value)) |idx| {
        return strings.items[idx];
    }

    const key_copy = try allocator.dupe(u8, value);
    errdefer allocator.free(key_copy);

    const escaped = try escapeLLVMString(allocator, value);
    defer allocator.free(escaped);

    const global_name = try std.fmt.allocPrint(allocator, "@.peek.str.{d}", .{next_id.*});
    errdefer allocator.free(global_name);
    next_id.* += 1;

    const global_line = try std.fmt.allocPrint(
        allocator,
        "{s} = private unnamed_addr constant [{d} x i8] c\"{s}\\00\"\n",
        .{ global_name, escaped.len + 1, escaped },
    );
    errdefer allocator.free(global_line);

    try globals.append(global_line);

    const info = PeekStringInfo{
        .name = global_name,
        .length = escaped.len + 1,
    };
    try strings.append(info);
    try map.put(key_copy, strings.items.len - 1);

    return info;
}

pub const IRPrinter = struct {
    allocator: std.mem.Allocator,

    const StackType = enum { I64, F64, I8, I1, I2, PTR };
    const StackVal = struct { name: []const u8, ty: StackType };

    pub fn init(allocator: std.mem.Allocator) IRPrinter {
        return .{ .allocator = allocator };
    }

    pub fn emitToFile(self: *IRPrinter, hir: *const HIR.HIRProgram, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        var buffer: [4096]u8 = undefined;
        var file_writer = file.writer(&buffer);
        const w = &file_writer.interface;
        try self.writeModule(hir, w);
        try w.flush();
    }

    fn writeModule(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype) !void {
        // Declarations
        try w.writeAll("declare void @doxa_write_cstr(ptr)\n");
        try w.writeAll("declare void @doxa_print_i64(i64)\n");
        try w.writeAll("declare void @doxa_print_u64(i64)\n");
        try w.writeAll("declare void @doxa_print_f64(double)\n");
        try w.writeAll("declare void @doxa_debug_peek(ptr)\n");
        try w.writeAll("declare double @llvm.pow.f64(double, double)\n\n");

        // String pool globals
        for (hir.string_pool, 0..) |s, idx| {
            const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\\00\"\n", .{ idx, s.len + 1, s });
            defer self.allocator.free(str_line);
            try w.writeAll(str_line);
        }
        if (hir.string_pool.len > 0) try w.writeAll("\n");

        try w.writeAll("%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }\n\n");
        try w.writeAll("@.doxa.nl = private constant [2 x i8] c\"\\0A\\00\"\n\n");

        // Find function start labels
        var func_start_labels = std.StringHashMap(bool).init(self.allocator);
        defer func_start_labels.deinit();
        for (hir.function_table) |f| {
            try func_start_labels.put(f.start_label, true);
        }

        // Find where functions section starts
        const functions_start_idx = self.findFunctionsSectionStart(hir, &func_start_labels);

        // Process main program (instructions before functions)
        try self.writeMainProgram(hir, w, functions_start_idx);

        // Process each function
        for (hir.function_table) |func| {
            try self.writeFunction(hir, w, func, &func_start_labels);
        }
    }

    fn findFunctionsSectionStart(self: *IRPrinter, hir: *const HIR.HIRProgram, func_start_labels: *std.StringHashMap(bool)) usize {
        _ = self;
        for (hir.instructions, 0..) |inst, idx| {
            if (inst == .Label) {
                const lbl = inst.Label.name;
                if (func_start_labels.get(lbl) != null) return idx;
            }
        }
        return hir.instructions.len;
    }

    fn writeMainProgram(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype, functions_start_idx: usize) !void {
        var peek_globals = std.array_list.Managed([]const u8).init(self.allocator);
        defer {
            for (peek_globals.items) |g| self.allocator.free(g);
            peek_globals.deinit();
        }
        var peek_string_map = std.StringHashMap(usize).init(self.allocator);
        defer {
            var it = peek_string_map.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.key_ptr.*);
            }
            peek_string_map.deinit();
        }
        var peek_strings = std.array_list.Managed(PeekStringInfo).init(self.allocator);
        defer {
            for (peek_strings.items) |info| self.allocator.free(info.name);
            peek_strings.deinit();
        }
        var next_peek_string_id: usize = 0;

        // Main function
        try w.writeAll("define i32 @main() {\n");
        try w.writeAll("entry:\n");

        var id: usize = 0;
        var stack = std.array_list.Managed(StackVal).init(self.allocator);
        defer stack.deinit();

        for (hir.instructions[0..functions_start_idx]) |inst| {
            switch (inst) {
                .Const => |c| {
                    const hv = hir.constant_pool[c.constant_id];
                    switch (hv) {
                        .int => |i| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, i });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I64 });
                        },
                        .float => |f| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double 0.0, {d}\n", .{ name, f });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .F64 });
                        },
                        .byte => |b| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i8 0, {d}\n", .{ name, b });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I8 });
                        },
                        .tetra => |t| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i2 0, {d}\n", .{ name, t });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I2 });
                        },
                        .string => |s| {
                            var found: ?usize = null;
                            for (hir.string_pool, 0..) |sp, si| {
                                if (std.mem.eql(u8, sp, s)) {
                                    found = si;
                                    break;
                                }
                            }
                            const idx = found orelse 0;
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr @.str.{d}, i64 0, i64 0\n", .{ name, s.len + 1, idx });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .PTR });
                        },
                        else => {},
                    }
                },
                .Arith => |a| {
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    switch (a.operand_type) {
                        .Int => {
                            switch (a.op) {
                                .Add => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Sub => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mul => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = mul i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Div => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = sdiv i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mod => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = srem i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Pow => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double sitofp i64 {s} to double, double sitofp i64 {s} to double)\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                            }
                            try stack.append(.{ .name = name, .ty = .I64 });
                        },
                        .Float => {
                            switch (a.op) {
                                .Add => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Sub => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = fsub double {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mul => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = fmul double {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Div => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = fdiv double {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mod => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = frem double {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Pow => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                            }
                            try stack.append(.{ .name = name, .ty = .F64 });
                        },
                        .Byte => {
                            switch (a.op) {
                                .Add => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i8 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Sub => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = sub i8 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mul => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = mul i8 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Div => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = udiv i8 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Mod => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = urem i8 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                                .Pow => {
                                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i8 @doxa.byte.pow(i8 {s}, i8 {s})\n", .{ name, lhs.name, rhs.name });
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                },
                            }
                            try stack.append(.{ .name = name, .ty = .I8 });
                        },
                        else => {},
                    }
                },
                .Compare => |cmp| {
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    switch (cmp.operand_type) {
                        .Int => {
                            const pred = switch (cmp.op) {
                                .Eq => "eq",
                                .Ne => "ne",
                                .Lt => "slt",
                                .Le => "sle",
                                .Gt => "sgt",
                                .Ge => "sge",
                            };
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i64 {s}, {s}\n", .{ name, pred, lhs.name, rhs.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I1 });
                        },
                        .Byte => {
                            const pred = switch (cmp.op) {
                                .Eq => "eq",
                                .Ne => "ne",
                                .Lt => "ult",
                                .Le => "ule",
                                .Gt => "ugt",
                                .Ge => "uge",
                            };
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i8 {s}, {s}\n", .{ name, pred, lhs.name, rhs.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I1 });
                        },
                        .Float => {
                            const pred = switch (cmp.op) {
                                .Eq => "oeq",
                                .Ne => "one",
                                .Lt => "olt",
                                .Le => "ole",
                                .Gt => "ogt",
                                .Ge => "oge",
                            };
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fcmp {s} double {s}, {s}\n", .{ name, pred, lhs.name, rhs.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I1 });
                        },
                        else => {},
                    }
                },
                .LogicalOp => |lop| {
                    if (lop.op == .Not) {
                        if (stack.items.len < 1) continue;
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const t2 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ t2, v.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = t2, .ty = .I2 });
                    }
                },
                .Print => {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    switch (v.ty) {
                        .PTR => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        },
                        .I64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                        },
                        .F64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                        },
                        else => {},
                    }
                },
                .PrintNewline => {
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                },
                .Peek => |pk| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];

                    const type_slice = switch (v.ty) {
                        .I64 => "int",
                        .F64 => "float",
                        .PTR => "string",
                        .I8, .I1, .I2 => "value",
                    };
                    const type_info = try internPeekString(
                        self.allocator,
                        &peek_string_map,
                        &peek_strings,
                        &next_peek_string_id,
                        &peek_globals,
                        type_slice,
                    );

                    var name_info: ?PeekStringInfo = null;
                    if (pk.name) |nm| {
                        name_info = try internPeekString(
                            self.allocator,
                            &peek_string_map,
                            &peek_strings,
                            &next_peek_string_id,
                            &peek_globals,
                            nm,
                        );
                    }

                    var file_info: ?PeekStringInfo = null;
                    var has_loc: u32 = 0;
                    var line_val: u32 = 0;
                    var col_val: u32 = 0;
                    if (pk.location) |loc| {
                        file_info = try internPeekString(
                            self.allocator,
                            &peek_string_map,
                            &peek_strings,
                            &next_peek_string_id,
                            &peek_globals,
                            loc.file,
                        );
                        line_val = std.math.cast(u32, loc.range.start_line) orelse std.math.maxInt(u32);
                        col_val = std.math.cast(u32, loc.range.start_col) orelse std.math.maxInt(u32);
                        has_loc = 1;
                    }

                    const info_var = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(info_var);
                    const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaPeekInfo\n", .{info_var});
                    defer self.allocator.free(alloca_line);
                    try w.writeAll(alloca_line);

                    const file_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(file_field_ptr);
                    const file_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 0\n",
                        .{ file_field_ptr, info_var },
                    );
                    defer self.allocator.free(file_field_line);
                    try w.writeAll(file_field_line);

                    if (file_info) |fi| {
                        const file_ptr_var = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        defer self.allocator.free(file_ptr_var);
                        const file_gep = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                            .{ file_ptr_var, fi.length, fi.name },
                        );
                        defer self.allocator.free(file_gep);
                        try w.writeAll(file_gep);

                        const store_file = try std.fmt.allocPrint(
                            self.allocator,
                            "  store ptr {s}, ptr {s}\n",
                            .{ file_ptr_var, file_field_ptr },
                        );
                        defer self.allocator.free(store_file);
                        try w.writeAll(store_file);
                    } else {
                        const store_null_file = try std.fmt.allocPrint(
                            self.allocator,
                            "  store ptr null, ptr {s}\n",
                            .{file_field_ptr},
                        );
                        defer self.allocator.free(store_null_file);
                        try w.writeAll(store_null_file);
                    }

                    const name_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(name_field_ptr);
                    const name_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 1\n",
                        .{ name_field_ptr, info_var },
                    );
                    defer self.allocator.free(name_field_line);
                    try w.writeAll(name_field_line);

                    if (name_info) |ni| {
                        const name_ptr_var = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        defer self.allocator.free(name_ptr_var);
                        const name_gep = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                            .{ name_ptr_var, ni.length, ni.name },
                        );
                        defer self.allocator.free(name_gep);
                        try w.writeAll(name_gep);

                        const store_name = try std.fmt.allocPrint(
                            self.allocator,
                            "  store ptr {s}, ptr {s}\n",
                            .{ name_ptr_var, name_field_ptr },
                        );
                        defer self.allocator.free(store_name);
                        try w.writeAll(store_name);
                    } else {
                        const store_null_name = try std.fmt.allocPrint(
                            self.allocator,
                            "  store ptr null, ptr {s}\n",
                            .{name_field_ptr},
                        );
                        defer self.allocator.free(store_null_name);
                        try w.writeAll(store_null_name);
                    }

                    const type_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(type_field_ptr);
                    const type_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 2\n",
                        .{ type_field_ptr, info_var },
                    );
                    defer self.allocator.free(type_field_line);
                    try w.writeAll(type_field_line);

                    const type_ptr_var = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(type_ptr_var);
                    const type_gep = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                        .{ type_ptr_var, type_info.length, type_info.name },
                    );
                    defer self.allocator.free(type_gep);
                    try w.writeAll(type_gep);

                    const store_type = try std.fmt.allocPrint(
                        self.allocator,
                        "  store ptr {s}, ptr {s}\n",
                        .{ type_ptr_var, type_field_ptr },
                    );
                    defer self.allocator.free(store_type);
                    try w.writeAll(store_type);

                    const members_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(members_field_ptr);
                    const members_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 3\n",
                        .{ members_field_ptr, info_var },
                    );
                    defer self.allocator.free(members_field_line);
                    try w.writeAll(members_field_line);

                    const store_members = try std.fmt.allocPrint(
                        self.allocator,
                        "  store ptr null, ptr {s}\n",
                        .{members_field_ptr},
                    );
                    defer self.allocator.free(store_members);
                    try w.writeAll(store_members);

                    const count_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(count_field_ptr);
                    const count_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 4\n",
                        .{ count_field_ptr, info_var },
                    );
                    defer self.allocator.free(count_field_line);
                    try w.writeAll(count_field_line);

                    const store_count = try std.fmt.allocPrint(
                        self.allocator,
                        "  store i32 0, ptr {s}\n",
                        .{count_field_ptr},
                    );
                    defer self.allocator.free(store_count);
                    try w.writeAll(store_count);

                    const active_field_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(active_field_ptr);
                    const active_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 5\n",
                        .{ active_field_ptr, info_var },
                    );
                    defer self.allocator.free(active_field_line);
                    try w.writeAll(active_field_line);

                    const store_active = try std.fmt.allocPrint(
                        self.allocator,
                        "  store i32 -1, ptr {s}\n",
                        .{active_field_ptr},
                    );
                    defer self.allocator.free(store_active);
                    try w.writeAll(store_active);

                    const has_loc_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(has_loc_ptr);
                    const has_loc_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 6\n",
                        .{ has_loc_ptr, info_var },
                    );
                    defer self.allocator.free(has_loc_line);
                    try w.writeAll(has_loc_line);

                    const store_has_loc = try std.fmt.allocPrint(
                        self.allocator,
                        "  store i32 {d}, ptr {s}\n",
                        .{ has_loc, has_loc_ptr },
                    );
                    defer self.allocator.free(store_has_loc);
                    try w.writeAll(store_has_loc);

                    const line_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(line_ptr);
                    const line_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 7\n",
                        .{ line_ptr, info_var },
                    );
                    defer self.allocator.free(line_field_line);
                    try w.writeAll(line_field_line);

                    const store_line_val = try std.fmt.allocPrint(
                        self.allocator,
                        "  store i32 {d}, ptr {s}\n",
                        .{ line_val, line_ptr },
                    );
                    defer self.allocator.free(store_line_val);
                    try w.writeAll(store_line_val);

                    const col_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    defer self.allocator.free(col_ptr);
                    const col_field_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 8\n",
                        .{ col_ptr, info_var },
                    );
                    defer self.allocator.free(col_field_line);
                    try w.writeAll(col_field_line);

                    const store_col_val = try std.fmt.allocPrint(
                        self.allocator,
                        "  store i32 {d}, ptr {s}\n",
                        .{ col_val, col_ptr },
                    );
                    defer self.allocator.free(store_col_val);
                    try w.writeAll(store_col_val);

                    const call_debug = try std.fmt.allocPrint(
                        self.allocator,
                        "  call void @doxa_debug_peek(ptr {s})\n",
                        .{info_var},
                    );
                    defer self.allocator.free(call_debug);
                    try w.writeAll(call_debug);

                    switch (v.ty) {
                        .I64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                        },
                        .F64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                        },
                        .PTR => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                        },
                        else => {},
                    }
                },
                .Label => |lbl| {
                    const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl.name});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                },
                .Jump => |j| {
                    const line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                },
                .JumpCond => |jc| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const cnd = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    switch (jc.condition_type) {
                        .Int => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 {s}, 0\n", .{ cnd, v.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Byte => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i8 {s}, 0\n", .{ cnd, v.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Float => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fcmp one double {s}, 0.0\n", .{ cnd, v.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Tetra => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i2 and (i2 {s}, 1), 0\n", .{ cnd, v.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        else => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 0, 0\n", .{cnd});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                    }
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ cnd, jc.label_true, jc.label_false });
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                },
                .Halt => {
                    try w.writeAll("  ret i32 0\n");
                },
                .Return => |ret| {
                    if (ret.has_value) {
                        if (stack.items.len < 1) continue;
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        switch (ret.return_type) {
                            .Int => {
                                const line = try std.fmt.allocPrint(self.allocator, "  ret i32 {s}\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            else => {
                                try w.writeAll("  ret i32 0\n");
                            },
                        }
                    } else {
                        try w.writeAll("  ret i32 0\n");
                    }
                },
                else => {},
            }
        }

        try w.writeAll("}\n");

        if (peek_globals.items.len > 0) {
            try w.writeAll("\n");
            for (peek_globals.items) |global_line| {
                try w.writeAll(global_line);
            }
        }
    }

    fn writeFunction(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype, func: HIR.HIRProgram.HIRFunction, func_start_labels: *std.StringHashMap(bool)) !void {
        // Find function start and end indices
        const start_idx = self.findLabelIndex(hir, func.start_label) orelse return;
        var end_idx: usize = hir.instructions.len;
        var i: usize = start_idx + 1;
        while (i < hir.instructions.len) : (i += 1) {
            const ins = hir.instructions[i];
            if (ins == .Label) {
                const name = ins.Label.name;
                if (func_start_labels.get(name) != null) {
                    end_idx = i;
                    break;
                }
            }
        }

        // Generate function signature
        const return_type_str = switch (func.return_type) {
            .Int => "i64",
            .Float => "double",
            .Byte => "i8",
            .Tetra => "i2",
            .String => "ptr",
            .Nothing => "void",
            else => "i64",
        };

        var param_strs = std.array_list.Managed([]const u8).init(self.allocator);
        defer {
            for (param_strs.items) |param_str| {
                self.allocator.free(param_str);
            }
            param_strs.deinit();
        }

        for (func.param_types, 0..) |param_type, param_idx| {
            const param_type_str = switch (param_type) {
                .Int => "i64",
                .Float => "double",
                .Byte => "i8",
                .Tetra => "i2",
                .String => "ptr",
                else => "i64",
            };
            const param_str = try std.fmt.allocPrint(self.allocator, "{s} %{d}", .{ param_type_str, param_idx });
            try param_strs.append(param_str);
        }

        const params_str = if (param_strs.items.len == 0) "" else try std.mem.join(self.allocator, ", ", param_strs.items);
        defer if (param_strs.items.len > 0) self.allocator.free(params_str);

        const func_decl = try std.fmt.allocPrint(self.allocator, "define {s} @{s}({s}) {{\n", .{ return_type_str, func.qualified_name, params_str });
        defer self.allocator.free(func_decl);
        try w.writeAll(func_decl);

        // Add entry block
        try w.writeAll("entry:\n");

        // Process function body instructions
        var id: usize = func.param_types.len; // Start after parameters
        var stack = std.array_list.Managed(StackVal).init(self.allocator);
        defer stack.deinit();

        // Add parameters to stack
        for (func.param_types, 0..) |param_type, param_idx| {
            const param_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{param_idx});
            const stack_type = self.hirTypeToStackType(param_type);
            try stack.append(.{ .name = param_name, .ty = stack_type });
        }

        // Process function body instructions
        for (hir.instructions[start_idx..end_idx]) |inst| {
            switch (inst) {
                .Label => |lbl| {
                    // Only process function body labels, skip function start labels and invalid basic block names
                    if (!std.mem.eql(u8, lbl.name, func.start_label) and !std.mem.startsWith(u8, lbl.name, "func_")) {
                        const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl.name});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                    }
                },
                .Const => |c| {
                    const hv = hir.constant_pool[c.constant_id];
                    switch (hv) {
                        .int => |int_val| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, int_val });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I64 });
                        },
                        .float => |f| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double 0.0, {d}\n", .{ name, f });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .F64 });
                        },
                        .byte => |b| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i8 0, {d}\n", .{ name, b });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I8 });
                        },
                        .tetra => |t| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i2 0, {d}\n", .{ name, t });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I2 });
                        },
                        .string => |s| {
                            var found: ?usize = null;
                            for (hir.string_pool, 0..) |sp, si| {
                                if (std.mem.eql(u8, sp, s)) {
                                    found = si;
                                    break;
                                }
                            }
                            const idx = found orelse 0;
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr @.str.{d}, i64 0, i64 0\n", .{ name, s.len + 1, idx });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .PTR });
                        },
                        else => {},
                    }
                },
                .Return => |ret| {
                    if (ret.has_value and stack.items.len > 0) {
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, v.name });
                        defer self.allocator.free(ret_line);
                        try w.writeAll(ret_line);
                    } else {
                        const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s}\n", .{return_type_str});
                        defer self.allocator.free(ret_line);
                        try w.writeAll(ret_line);
                    }
                },
                .JumpCond => |jc| {
                    if (stack.items.len < 1) continue;
                    const cond = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ cond.name, jc.label_true, jc.label_false });
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                },
                .Jump => |j| {
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                },
                .LoadVar => |lv| {
                    _ = lv;
                    // For now, just push a placeholder value
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{name});
                    defer self.allocator.free(load_line);
                    try w.writeAll(load_line);
                    try stack.append(.{ .name = name, .ty = .I64 });
                },
                .StoreVar => |sv| {
                    _ = sv;
                    if (stack.items.len > 0) {
                        _ = stack.pop();
                    }
                },
                .Compare => |cmp| {
                    _ = cmp;
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const cmp_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp sgt i64 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                    defer self.allocator.free(cmp_line);
                    try w.writeAll(cmp_line);
                    try stack.append(.{ .name = name, .ty = .I1 });
                },
                .LogicalOp => |lop| {
                    _ = lop;
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const op_line = try std.fmt.allocPrint(self.allocator, "  {s} = or i1 {s}, {s}\n", .{ name, lhs.name, rhs.name });
                    defer self.allocator.free(op_line);
                    try w.writeAll(op_line);
                    try stack.append(.{ .name = name, .ty = .I1 });
                },
                // Add other instruction types as needed
                else => {},
            }
        }

        try w.writeAll("}\n\n");
    }

    fn findLabelIndex(self: *IRPrinter, hir: *const HIR.HIRProgram, label: []const u8) ?usize {
        _ = self;
        for (hir.instructions, 0..) |inst, idx| {
            if (inst == .Label and std.mem.eql(u8, inst.Label.name, label)) return idx;
        }
        return null;
    }

    fn hirTypeToStackType(self: *IRPrinter, hir_type: HIR.HIRType) StackType {
        _ = self;
        return switch (hir_type) {
            .Int => .I64,
            .Float => .F64,
            .Byte => .I8,
            .Tetra => .I2,
            .String => .PTR,
            else => .I64,
        };
    }
};
