const std = @import("std");
const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
const CompareInstruction = std.meta.TagPayload(HIRInstruction, .Compare);

const PeekStringInfo = struct {
    name: []const u8,
    length: usize,
};

const PeekEmitState = struct {
    allocator: std.mem.Allocator,
    globals: std.array_list.Managed([]const u8),
    string_map: std.StringHashMap(usize),
    strings: std.array_list.Managed(PeekStringInfo),
    next_id_ptr: *usize,

    pub fn init(allocator: std.mem.Allocator, next_id_ptr: *usize) PeekEmitState {
        return .{
            .allocator = allocator,
            .globals = std.array_list.Managed([]const u8).init(allocator),
            .string_map = std.StringHashMap(usize).init(allocator),
            .strings = std.array_list.Managed(PeekStringInfo).init(allocator),
            .next_id_ptr = next_id_ptr,
        };
    }

    pub fn deinit(self: *PeekEmitState) void {
        for (self.globals.items) |g| self.allocator.free(g);
        self.globals.deinit();

        var it = self.string_map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.string_map.deinit();

        for (self.strings.items) |info| self.allocator.free(info.name);
        self.strings.deinit();
    }
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
        .{ global_name, value.len + 1, escaped },
    );
    errdefer allocator.free(global_line);

    try globals.append(global_line);

    const info = PeekStringInfo{
        .name = global_name,
        .length = value.len + 1,
    };
    try strings.append(info);
    try map.put(key_copy, strings.items.len - 1);

    return info;
}

pub const IRPrinter = struct {
    allocator: std.mem.Allocator,
    peek_string_counter: usize,
    global_types: std.StringHashMap(StackType),
    global_array_types: std.StringHashMap(HIR.HIRType), // Store array element types for global variables
    global_enum_types: std.StringHashMap([]const u8), // Store enum type names for global variables
    global_struct_field_types: std.StringHashMap([]HIR.HIRType), // Store struct field types for globals
    defined_globals: std.StringHashMap(bool),
    function_struct_return_fields: std.StringHashMap([]HIR.HIRType),
    last_emitted_enum_value: ?u64 = null,

    const StackType = enum { I64, F64, I8, I1, I2, PTR, Nothing };
    const StackVal = struct {
        name: []const u8,
        ty: StackType,
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null, // For enum values, store the type name
        struct_field_types: ?[]HIR.HIRType = null,
    };
    const VariableInfo = struct {
        ptr_name: []const u8,
        stack_type: StackType,
        array_type: ?HIR.HIRType = null,
        enum_type_name: ?[]const u8 = null, // For enum variables, store the type name
    };
    const StackIncoming = struct {
        block: []const u8,
        value: StackVal,
    };
    const StackSlot = std.ArrayListUnmanaged(StackIncoming);
    const StackMergeState = struct {
        slots: []StackSlot,

        fn init(allocator: std.mem.Allocator, slot_count: usize) !StackMergeState {
            var slots = try allocator.alloc(StackSlot, slot_count);
            var i: usize = 0;
            while (i < slot_count) : (i += 1) {
                slots[i] = StackSlot{};
            }
            return .{ .slots = slots };
        }

        fn deinit(self: *StackMergeState, allocator: std.mem.Allocator) void {
            for (self.slots) |*slot| slot.deinit(allocator);
            allocator.free(self.slots);
        }
    };

    fn formatFloatLiteral(self: *IRPrinter, value: f64) ![]u8 {
        const raw = try std.fmt.allocPrint(self.allocator, "{d}", .{value});
        const has_decimal = std.mem.indexOfScalar(u8, raw, '.') != null or
            std.mem.indexOfScalar(u8, raw, 'e') != null or
            std.mem.indexOfScalar(u8, raw, 'E') != null;
        if (!has_decimal) {
            const with_fraction = try std.fmt.allocPrint(self.allocator, "{s}.0", .{raw});
            self.allocator.free(raw);
            return with_fraction;
        }
        return raw;
    }

    fn paramTypeMatchesStack(_: *IRPrinter, param_type: HIR.HIRType, stack_type: StackType) bool {
        return switch (stack_type) {
            .I64 => switch (param_type) {
                .Int, .Enum, .Union, .Unknown => true,
                else => false,
            },
            .F64 => switch (param_type) {
                .Float, .Union, .Unknown => true,
                else => false,
            },
            .I8 => param_type == .Byte,
            .I2 => param_type == .Tetra,
            .PTR => switch (param_type) {
                .String, .Struct, .Array, .Map => true,
                else => false,
            },
            else => false,
        };
    }

    fn recordStackForLabel(
        self: *IRPrinter,
        map: *std.StringHashMap(StackMergeState),
        label: []const u8,
        stack: []const StackVal,
        current_block: []const u8,
    ) !void {
        var entry = try map.getOrPut(label);
        if (!entry.found_existing) {
            entry.value_ptr.* = try StackMergeState.init(self.allocator, stack.len);
        } else if (entry.value_ptr.slots.len < stack.len) {
            // Need to grow the slots array
            const old_len = entry.value_ptr.slots.len;
            const new_slots = try self.allocator.realloc(entry.value_ptr.slots, stack.len);
            entry.value_ptr.slots = new_slots;
            for (old_len..stack.len) |i| {
                new_slots[i] = StackSlot{};
            }
        }
        // Note: If incoming stack is shorter, we only record up to its length

        for (entry.value_ptr.slots[0..stack.len], stack) |*slot, value| {
            try slot.append(self.allocator, .{ .block = current_block, .value = value });
        }
    }

    fn restoreStackForLabel(
        self: *IRPrinter,
        map: *std.StringHashMap(StackMergeState),
        label: []const u8,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        w: anytype,
    ) !void {
        if (map.fetchRemove(label)) |removed| {
            defer @constCast(&removed.value).deinit(self.allocator);
            stack.items.len = 0;

            for (removed.value.slots) |slot| {
                if (slot.items.len == 0) continue;
                if (slot.items.len == 1) {
                    try stack.append(slot.items[0].value);
                    continue;
                }

                const phi_name = try self.nextTempText(id);
                const type_str = self.stackTypeToLLVMType(slot.items[0].value.ty);

                var incoming = std.array_list.Managed([]const u8).init(self.allocator);
                defer {
                    for (incoming.items) |entry_str| self.allocator.free(entry_str);
                    incoming.deinit();
                }

                for (slot.items) |incoming_val| {
                    const blk_name: []const u8 = if (std.mem.startsWith(u8, incoming_val.block, "func_")) "entry" else incoming_val.block;
                    const pair = try std.fmt.allocPrint(self.allocator, "[ {s}, %{s} ]", .{ incoming_val.value.name, blk_name });
                    try incoming.append(pair);
                }

                const joined = if (incoming.items.len == 0) "" else try std.mem.join(self.allocator, ", ", incoming.items);
                defer if (incoming.items.len > 0) self.allocator.free(joined);

                const phi_line = try std.fmt.allocPrint(self.allocator, "  {s} = phi {s} {s}\n", .{ phi_name, type_str, joined });
                defer self.allocator.free(phi_line);
                try w.writeAll(phi_line);

                try stack.append(.{
                    .name = phi_name,
                    .ty = slot.items[0].value.ty,
                    .array_type = slot.items[0].value.array_type,
                });
            }
        }
    }

    pub fn init(allocator: std.mem.Allocator) IRPrinter {
        return .{
            .allocator = allocator,
            .peek_string_counter = 0,
            .global_types = std.StringHashMap(StackType).init(allocator),
            .global_array_types = std.StringHashMap(HIR.HIRType).init(allocator),
            .global_enum_types = std.StringHashMap([]const u8).init(allocator),
            .global_struct_field_types = std.StringHashMap([]HIR.HIRType).init(allocator),
            .defined_globals = std.StringHashMap(bool).init(allocator),
            .last_emitted_enum_value = null,
            .function_struct_return_fields = std.StringHashMap([]HIR.HIRType).init(allocator),
        };
    }

    pub fn deinit(self: *IRPrinter) void {
        self.peek_string_counter = 0;
        self.global_types.deinit();
        self.global_array_types.deinit();
        self.global_enum_types.deinit();
        self.global_struct_field_types.deinit();
        self.defined_globals.deinit();
        var ret_it = self.function_struct_return_fields.iterator();
        while (ret_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.*);
        }
        self.function_struct_return_fields.deinit();
    }

    fn mapBuiltinToRuntime(name: []const u8) []const u8 {
        // Map built-in function names to their runtime equivalents
        if (std.mem.eql(u8, name, "random")) return "doxa_random";
        if (std.mem.eql(u8, name, "int")) return "doxa_int";
        if (std.mem.eql(u8, name, "tick")) return "doxa_tick";
        if (std.mem.eql(u8, name, "string")) return "doxa_string";
        if (std.mem.eql(u8, name, "dice_roll")) return "doxa_dice_roll";
        return name;
    }

    fn mangleGlobalName(self: *IRPrinter, name: []const u8) ![]const u8 {
        return std.fmt.allocPrint(self.allocator, "@.glob.{s}", .{name});
    }

    pub fn emitToFile(self: *IRPrinter, hir: *const HIR.HIRProgram, path: []const u8) !void {
        const file = try std.fs.cwd().createFile(path, .{});
        defer file.close();
        var buffer: [4096]u8 = undefined;
        var file_writer = file.writer(&buffer);
        const w = &file_writer.interface;
        try self.writeModule(hir, w);
        try w.flush();
        self.deinit();
    }

    fn writeModule(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype) !void {
        // Declarations
        try w.writeAll("declare void @doxa_write_cstr(ptr)\n");
        try w.writeAll("declare void @doxa_print_i64(i64)\n");
        try w.writeAll("declare void @doxa_print_u64(i64)\n");
        try w.writeAll("declare void @doxa_print_f64(double)\n");
        try w.writeAll("declare void @doxa_print_enum(ptr, i64)\n");
        try w.writeAll("declare i64 @doxa_str_len(ptr)\n");
        try w.writeAll("declare i64 @doxa_byte_from_cstr(ptr)\n");
        try w.writeAll("declare void @doxa_debug_peek(ptr)\n");
        // Array printing helper (runtime)
        try w.writeAll("declare void @doxa_print_array_hdr(ptr)\n");
        try w.writeAll("declare i1 @doxa_str_eq(ptr, ptr)\n");
        try w.writeAll("declare ptr @doxa_array_new(i64, i64, i64)\n");
        try w.writeAll("declare i64 @doxa_array_len(ptr)\n");
        try w.writeAll("declare i64 @doxa_array_get_i64(ptr, i64)\n");
        try w.writeAll("declare void @doxa_array_set_i64(ptr, i64, i64)\n");
        try w.writeAll("declare double @llvm.pow.f64(double, double)\n");
        try w.writeAll("declare double @doxa_random()\n");
        try w.writeAll("declare i64 @doxa_tick()\n");
        try w.writeAll("declare ptr @malloc(i64)\n");
        try w.writeAll("declare i8 @doxa_exists_quantifier_gt(ptr, i64)\n");
        try w.writeAll("declare i8 @doxa_exists_quantifier_eq(ptr, i64)\n");
        try w.writeAll("declare i8 @doxa_forall_quantifier_gt(ptr, i64)\n");
        try w.writeAll("declare i8 @doxa_forall_quantifier_eq(ptr, i64)\n");

        // String pool globals
        for (hir.string_pool, 0..) |s, idx| {
            const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\\00\"\n", .{ idx, s.len + 1, s });
            defer self.allocator.free(str_line);
            try w.writeAll(str_line);
        }
        if (hir.string_pool.len > 0) try w.writeAll("\n");

        // String constants from constant pool
        for (hir.constant_pool, 0..) |hv, idx| {
            if (hv == .string) {
                const s = hv.string;
                // Escape the string content for LLVM IR (but preserve newlines as actual newlines)
                var escaped = std.ArrayListUnmanaged(u8){};
                defer escaped.deinit(self.allocator);
                for (s) |c| {
                    switch (c) {
                        '\n' => try escaped.append(self.allocator, '\n'), // Keep actual newlines
                        '\r' => try escaped.appendSlice(self.allocator, "\\r"),
                        '\t' => try escaped.appendSlice(self.allocator, "\\t"),
                        '\\' => try escaped.appendSlice(self.allocator, "\\\\"),
                        '"' => try escaped.appendSlice(self.allocator, "\\\""),
                        else => try escaped.append(self.allocator, c),
                    }
                }
                const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\\00\"\n", .{ idx, escaped.items.len + 1, escaped.items });
                defer self.allocator.free(str_line);
                try w.writeAll(str_line);
            }
        }

        // (globals emitted later once discovered)

        try w.writeAll("%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }\n");
        try w.writeAll("%ArrayHeader = type { ptr, i64, i64, i64, i64 }\n\n");
        try w.writeAll("@.doxa.nl = private constant [2 x i8] c\"\\0A\\00\"\n\n");
        try self.emitQuantifierWrappers(w);

        // Find function start labels
        var func_start_labels = std.StringHashMap(bool).init(self.allocator);
        defer func_start_labels.deinit();
        for (hir.function_table) |f| {
            try func_start_labels.put(f.start_label, true);
        }

        // Find where functions section starts
        const functions_start_idx = self.findFunctionsSectionStart(hir, &func_start_labels);

        // Pre-populate global_struct_field_types by scanning all StructNew instructions
        // This ensures struct field types are available when methods access 'this'
        for (hir.instructions) |inst| {
            if (inst == .StructNew) {
                const sn = inst.StructNew;
                // Store struct field types globally for later lookup
                // Only store if not already present (first occurrence wins)
                if (!self.global_struct_field_types.contains(sn.type_name)) {
                    _ = try self.global_struct_field_types.put(sn.type_name, try self.allocator.dupe(HIR.HIRType, sn.field_types));
                }
            }
        }

        // Collect struct return metadata for functions before emitting main program.
        for (hir.function_table) |func| {
            try self.collectFunctionStructReturnInfo(hir, func, &func_start_labels);
        }

        var peek_state = PeekEmitState.init(self.allocator, &self.peek_string_counter);
        defer peek_state.deinit();

        // If there is a user-declared entry function named 'main', do not emit a wrapper 'main'
        var has_user_main: bool = false;
        for (hir.function_table) |f| {
            if (f.is_entry and std.mem.eql(u8, f.qualified_name, "main")) {
                has_user_main = true;
                break;
            }
        }

        // Process main program (instructions before functions) only if no user 'main'
        if (!has_user_main) {
            try self.writeMainProgram(hir, w, functions_start_idx, &peek_state);
        }

        // Emit discovered globals after scanning main program
        if (self.defined_globals.count() > 0) {
            try w.writeAll("\n");
            var it2 = self.defined_globals.iterator();
            while (it2.next()) |entry| {
                const gname = entry.key_ptr.*;
                const st = self.global_types.get(gname) orelse continue;
                const llty = self.stackTypeToLLVMType(st);
                const mname = try self.mangleGlobalName(gname);
                defer self.allocator.free(mname);
                const line = try std.fmt.allocPrint(self.allocator, "{s} = global {s} zeroinitializer\n", .{ mname, llty });
                defer self.allocator.free(line);
                try w.writeAll(line);
            }
            try w.writeAll("\n");
        }

        // Process each function
        for (hir.function_table) |func| {
            try self.writeFunction(hir, w, func, &func_start_labels, &peek_state);
        }

        // If there is a user-declared main, emit a C-compatible wrapper `i32 @main()`
        if (has_user_main) {
            try w.writeAll("define i32 @main() {\n");
            try w.writeAll("entry:\n");
            try w.writeAll("  call void @doxa_user_main()\n");
            try w.writeAll("  ret i32 0\n");
            try w.writeAll("}\n");
        }

        if (peek_state.globals.items.len > 0) {
            try w.writeAll("\n");
            for (peek_state.globals.items) |global_line| {
                try w.writeAll(global_line);
            }
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

    fn writeMainProgram(
        self: *IRPrinter,
        hir: *const HIR.HIRProgram,
        w: anytype,
        functions_start_idx: usize,
        peek_state: *PeekEmitState,
    ) !void {
        // Main function - use C-compatible signature so Windows links a console subsystem
        try w.writeAll("define i32 @main() {\n");
        try w.writeAll("entry:\n");

        var id: usize = 0;
        var stack = std.array_list.Managed(StackVal).init(self.allocator);
        defer stack.deinit();

        var merge_map = std.StringHashMap(StackMergeState).init(self.allocator);
        defer {
            var it_merge = merge_map.iterator();
            while (it_merge.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
            }
            merge_map.deinit();
        }

        var current_block: []const u8 = "entry";
        var last_instruction_was_terminator = false;

        var variables = std.StringHashMap(VariableInfo).init(self.allocator);
        defer {
            var it = variables.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.value_ptr.ptr_name);
            }
            variables.deinit();
        }

        var had_return: bool = false;
        var synthetic_labels = std.array_list.Managed([]const u8).init(self.allocator);
        defer {
            for (synthetic_labels.items) |lbl| self.allocator.free(lbl);
            synthetic_labels.deinit();
        }
        var dead_block_counter: usize = 0;

        for (hir.instructions[0..functions_start_idx]) |inst| {
            const tag = std.meta.activeTag(inst);
            const requires_new_block = switch (tag) {
                .Label, .ExitScope => false,
                else => true,
            };
            if (last_instruction_was_terminator and requires_new_block) {
                const dead_label = try std.fmt.allocPrint(self.allocator, "dead_block_{d}", .{dead_block_counter});
                dead_block_counter += 1;
                try synthetic_labels.append(dead_label);
                const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{dead_label});
                defer self.allocator.free(line);
                try w.writeAll(line);
                current_block = dead_label;
                stack.items.len = 0;
                last_instruction_was_terminator = false;
            }
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
                            const literal = try self.formatFloatLiteral(f);
                            defer self.allocator.free(literal);
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double 0.0, {s}\n", .{ name, literal });
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
                            const info = try internPeekString(
                                self.allocator,
                                &peek_state.*.string_map,
                                &peek_state.*.strings,
                                peek_state.*.next_id_ptr,
                                &peek_state.*.globals,
                                s,
                            );
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ name, info.length, info.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .PTR });
                        },
                        .enum_variant => |ev| {
                            // Store enum variant as its index value (i64) but mark it as enum type
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, @as(i64, @intCast(ev.variant_index)) });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            // Store enum type name as a global string constant for later use
                            const type_name_global = try self.createEnumTypeNameGlobal(ev.type_name, &id);
                            try stack.append(.{ .name = name, .ty = .I64, .enum_type_name = type_name_global });
                            self.last_emitted_enum_value = ev.variant_index;
                        },
                        .nothing => {
                            // nothing type - don't push anything to stack (zero-sized type)
                        },
                        else => {},
                    }
                },
                .StoreAlias => |_| {
                    // Alias handling is only meaningful inside functions
                    last_instruction_was_terminator = false;
                },
                .ArrayNew => |a| try self.emitArrayNew(w, &stack, &id, a),
                .ArraySet => |_| try self.emitArraySet(w, &stack, &id),
                .ArrayGet => |_| try self.emitArrayGet(w, &stack, &id),
                .ArrayLen => try self.emitArrayLen(w, &stack, &id),
                .ArrayPop => try self.emitArrayPop(w, &stack, &id),
                .Range => |r| try self.emitRange(w, &stack, &id, r),
                .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn),
                .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                .Dup => {
                    if (stack.items.len < 1) continue;
                    const top = stack.items[stack.items.len - 1];
                    // Deep copy the StackVal to preserve struct_field_types reference
                    const duped: StackVal = .{
                        .name = top.name,
                        .ty = top.ty,
                        .array_type = top.array_type,
                        .enum_type_name = top.enum_type_name,
                        .struct_field_types = top.struct_field_types, // Copy the pointer reference
                    };
                    try stack.append(duped);
                },
                .Pop => {
                    if (stack.items.len < 1) continue;
                    stack.items.len -= 1;
                },
                .Swap => {
                    if (stack.items.len < 2) continue;
                    const top_idx = stack.items.len - 1;
                    std.mem.swap(StackVal, &stack.items[top_idx], &stack.items[top_idx - 1]);
                },
                .Arith => |a| {
                    if (stack.items.len < 2) continue;
                    var rhs = stack.items[stack.items.len - 1];
                    var lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    switch (a.operand_type) {
                        .Int => {
                            lhs = try self.ensureI64(w, lhs, &id);
                            rhs = try self.ensureI64(w, rhs, &id);
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
                    const result = try self.emitCompareInstruction(w, cmp, lhs, rhs, &id);
                    try stack.append(result);
                },
                .LogicalOp => |lop| {
                    if (lop.op == .Not) {
                        if (stack.items.len < 1) continue;
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const bool_val = try self.ensureBool(w, v, &id);
                        const t2 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ t2, bool_val.name });
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
                            if (v.array_type) |_| {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            } else {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            }
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
                .PrintBegin => |_| {
                    // no-op for now
                },
                .PrintStr => |ps| {
                    // Emit write of string literal by const id; guard against out-of-range
                    const idx: usize = @intCast(ps.const_id);
                    if (idx < hir.constant_pool.len) {
                        const hv = hir.constant_pool[idx];
                        if (hv == .string) {
                            const s = hv.string;
                            const tmp = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr @.str.{d}, i64 0, i64 0\n", .{ tmp, s.len + 1, idx });
                            defer self.allocator.free(gep);
                            try w.writeAll(gep);
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{tmp});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        }
                    }
                },
                .PrintVal => |_| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    switch (v.ty) {
                        .I64 => {
                            // Check if this is an enum value
                            if (v.enum_type_name) |type_name| {
                                try self.emitEnumPrint(peek_state, w, &id, type_name, v.name);
                            } else {
                                // Regular integer print
                                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            }
                        },
                        .F64 => {
                            const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .PTR => {
                            if (v.array_type) |_| {
                                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            } else {
                                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            }
                        },
                        else => {},
                    }
                },
                .PrintEnd => |_| {
                    // no-op for now (newlines are explicit constants in program)
                },
                .Peek => |pk| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];

                    try self.emitPeekInstruction(w, pk, v, &id, peek_state);

                    switch (v.ty) {
                        .I64 => {
                            // Check if this is an enum value
                            if (v.enum_type_name) |type_name| {
                                try self.emitEnumPrint(peek_state, w, &id, type_name, v.name);
                            } else {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            }
                        },
                        .F64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        },
                        .PTR => {
                            if (v.array_type) |_| {
                                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            } else {
                                // Print quoted string
                                const quote_info = try internPeekString(
                                    self.allocator,
                                    &peek_state.string_map,
                                    &peek_state.strings,
                                    peek_state.next_id_ptr,
                                    &peek_state.globals,
                                    "\"",
                                );

                                const qptr = try self.nextTemp(&id);
                                const qgep = try std.fmt.allocPrint(
                                    self.allocator,
                                    "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                                    .{ qptr, quote_info.length, quote_info.name },
                                );
                                defer self.allocator.free(qgep);
                                try w.writeAll(qgep);

                                const call_q1 = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{qptr});
                                defer self.allocator.free(call_q1);
                                try w.writeAll(call_q1);

                                const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_val);
                                try w.writeAll(call_val);

                                const call_q2 = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{qptr});
                                defer self.allocator.free(call_q2);
                                try w.writeAll(call_q2);
                            }
                        },
                        .I2 => {
                            // tetra: print true for non-zero, false otherwise
                            const is_true = try self.nextTemp(&id);
                            const icmp_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i2 {s}, 0\n", .{ is_true, v.name });
                            defer self.allocator.free(icmp_line);
                            try w.writeAll(icmp_line);

                            const true_info = try internPeekString(
                                self.allocator,
                                &peek_state.string_map,
                                &peek_state.strings,
                                peek_state.next_id_ptr,
                                &peek_state.globals,
                                "true",
                            );
                            const false_info = try internPeekString(
                                self.allocator,
                                &peek_state.string_map,
                                &peek_state.strings,
                                peek_state.next_id_ptr,
                                &peek_state.globals,
                                "false",
                            );

                            const tptr = try self.nextTemp(&id);
                            const fptr = try self.nextTemp(&id);
                            const tgep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ tptr, true_info.length, true_info.name });
                            const fgep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ fptr, false_info.length, false_info.name });
                            defer self.allocator.free(tgep);
                            defer self.allocator.free(fgep);
                            try w.writeAll(tgep);
                            try w.writeAll(fgep);

                            const sel = try self.nextTemp(&id);
                            const sel_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel, is_true, tptr, fptr });
                            defer self.allocator.free(sel_line);
                            try w.writeAll(sel_line);

                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{sel});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        },
                        else => {},
                    }
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                },
                .Label => |lbl| {
                    if (!last_instruction_was_terminator) {
                        const br_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{lbl.name});
                        defer self.allocator.free(br_line);
                        try w.writeAll(br_line);
                        last_instruction_was_terminator = true;
                    }
                    const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl.name});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    current_block = lbl.name;
                    last_instruction_was_terminator = false;
                    try self.restoreStackForLabel(&merge_map, lbl.name, &stack, &id, w);
                },
                .Jump => |j| {
                    try self.recordStackForLabel(&merge_map, j.label, stack.items, current_block);
                    const line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .JumpCond => |jc| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const bool_val = try self.ensureBool(w, v, &id);
                    try self.recordStackForLabel(&merge_map, jc.label_true, stack.items, current_block);
                    try self.recordStackForLabel(&merge_map, jc.label_false, stack.items, current_block);
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ bool_val.name, jc.label_true, jc.label_false });
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .Halt => {
                    try w.writeAll("  ret i32 0\n");
                    had_return = true;
                    last_instruction_was_terminator = true;
                },
                .Call => |c| {
                    const argc: usize = @intCast(c.arg_count);
                    if (stack.items.len < argc) continue;

                    var raw_args = std.array_list.Managed(StackVal).init(self.allocator);
                    defer raw_args.deinit();

                    var arg_idx: usize = 0;
                    while (arg_idx < argc) : (arg_idx += 1) {
                        const arg = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        try raw_args.append(arg);
                    }
                    std.mem.reverse(StackVal, raw_args.items);

                    var arg_strings = std.array_list.Managed([]const u8).init(self.allocator);
                    defer {
                        for (arg_strings.items) |s| self.allocator.free(s);
                        arg_strings.deinit();
                    }

                    const func_info = if (c.function_index < hir.function_table.len)
                        hir.function_table[c.function_index]
                    else
                        null;

                    for (raw_args.items, 0..) |arg, i| {
                        var declared_type: ?HIR.HIRType = null;
                        var is_alias = false;
                        if (func_info) |info| {
                            if (i < info.param_types.len) {
                                declared_type = info.param_types[i];
                            }
                            if (i < info.param_is_alias.len) {
                                is_alias = info.param_is_alias[i];
                            }
                        }
                        const llvm_ty = blk: {
                            if (is_alias) {
                                const ty = declared_type orelse .Int;
                                break :blk self.hirTypeToLLVMType(ty, true);
                            }
                            if (declared_type) |decl| {
                                if (self.paramTypeMatchesStack(decl, arg.ty)) {
                                    break :blk self.hirTypeToLLVMType(decl, false);
                                }
                            }
                            break :blk self.stackTypeToLLVMType(arg.ty);
                        };
                        const arg_str = try std.fmt.allocPrint(self.allocator, "{s} {s}", .{ llvm_ty, arg.name });
                        try arg_strings.append(arg_str);
                    }

                    const args_str = if (arg_strings.items.len == 0) "" else try std.mem.join(self.allocator, ", ", arg_strings.items);
                    defer if (arg_strings.items.len > 0) self.allocator.free(args_str);

                    // Determine actual return type: prefer Call's return_type, fall back to function's return_type
                    const actual_return_type = if (c.return_type != .Nothing) c.return_type else if (func_info) |info| info.return_type else .Nothing;

                    if (actual_return_type != .Nothing) {
                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const ret_ty = self.hirTypeToLLVMType(actual_return_type, false);
                        const runtime_name = mapBuiltinToRuntime(c.qualified_name);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call {s} @{s}({s})\n", .{ result_name, ret_ty, runtime_name, args_str });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                        const stack_ty = self.hirTypeToStackType(actual_return_type);
                        var pushed = StackVal{ .name = result_name, .ty = stack_ty };
                        if (stack_ty == .PTR and actual_return_type == .Struct) {
                            if (self.function_struct_return_fields.get(c.qualified_name)) |fts| {
                                pushed.struct_field_types = fts;
                            }
                        }
                        try stack.append(pushed);
                    } else {
                        const runtime_name = mapBuiltinToRuntime(c.qualified_name);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}({s})\n", .{ runtime_name, args_str });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StringOp => |sop| {
                    if (stack.items.len < 1) continue;
                    const arg = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    switch (sop.op) {
                        .Length => {
                            // Compute C-string length
                            if (arg.ty == .PTR) {
                                const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_str_len(ptr {s})\n", .{ result_name, arg.name });
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                                try stack.append(.{ .name = result_name, .ty = .I64 });
                            } else if (arg.ty == .I64) {
                                const tmp_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp_ptr, arg.name });
                                defer self.allocator.free(cast_line);
                                try w.writeAll(cast_line);

                                const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_str_len(ptr {s})\n", .{ result_name, tmp_ptr });
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                                self.allocator.free(tmp_ptr);
                                try stack.append(.{ .name = result_name, .ty = .I64 });
                            } else {
                                const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{result_name});
                                defer self.allocator.free(zero_line);
                                try w.writeAll(zero_line);
                                try stack.append(.{ .name = result_name, .ty = .I64 });
                            }
                        },
                        .ToInt => {
                            // Convert float to int using @doxa_int
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int(double {s})\n", .{ result_name, arg.name });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .I64 });
                        },
                        .ToByte => {
                            // Convert C-string to byte using doxa_byte_from_cstr
                            const ptr_val = blk: {
                                if (arg.ty == .PTR) {
                                    break :blk arg.name;
                                } else if (arg.ty == .I64) {
                                    const tmp_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp_ptr, arg.name });
                                    defer self.allocator.free(cast_line);
                                    try w.writeAll(cast_line);
                                    break :blk tmp_ptr;
                                } else {
                                    const zero = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{zero});
                                    defer self.allocator.free(zero_line);
                                    try w.writeAll(zero_line);
                                    try stack.append(.{ .name = zero, .ty = .I8 });
                                    break :blk "";
                                }
                            };

                            if (ptr_val.len == 0) {
                                // Already pushed a zero-byte above
                            } else {
                                const as_i64 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const call_line = try std.fmt.allocPrint(
                                    self.allocator,
                                    "  {s} = call i64 @doxa_byte_from_cstr(ptr {s})\n",
                                    .{ as_i64, ptr_val },
                                );
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);

                                const as_i8 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ as_i8, as_i64 });
                                defer self.allocator.free(trunc_line);
                                try w.writeAll(trunc_line);
                                try stack.append(.{ .name = as_i8, .ty = .I8 });
                            }
                        },
                        else => {
                            // Not implemented yet - push zero
                            const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = fallback, .ty = .I64 });
                        },
                    }
                    last_instruction_was_terminator = false;
                },
                .LoadVar => |lv| {
                    if (lv.scope_kind == .GlobalLocal) {
                        const gname = lv.var_name;
                        const st = self.global_types.get(gname) orelse .I64;

                        // For Nothing types, don't emit load instruction (zero-sized type)
                        if (st == .Nothing) {
                            // Push a dummy value for Nothing type (it won't be used)
                            const result_name = try self.nextTemp(&id);
                            try stack.append(.{ .name = result_name, .ty = .Nothing });
                            continue;
                        }

                        const llty = self.stackTypeToLLVMType(st);
                        const gptr = try self.mangleGlobalName(gname);
                        defer self.allocator.free(gptr);

                        // Ensure the global is declared by adding it to defined_globals if not already there
                        if (!self.defined_globals.contains(gname)) {
                            _ = try self.global_types.put(gname, st);
                            _ = try self.defined_globals.put(gname, true);
                        }

                        const result_name = try self.nextTemp(&id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result_name, llty, gptr });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        const array_type = self.global_array_types.get(gname);
                        const enum_type_name = self.global_enum_types.get(gname);
                        const struct_fields = self.global_struct_field_types.get(gname);
                        try stack.append(.{ .name = result_name, .ty = st, .array_type = array_type, .enum_type_name = enum_type_name, .struct_field_types = struct_fields });
                    } else if (variables.get(lv.var_name)) |entry| {
                        const result_name = try self.nextTemp(&id);
                        const ty_str = self.stackTypeToLLVMType(entry.stack_type);
                        const line = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = load {s}, ptr {s}\n",
                            .{ result_name, ty_str, entry.ptr_name },
                        );
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = result_name, .ty = entry.stack_type, .array_type = entry.array_type, .enum_type_name = entry.enum_type_name });
                    } else {
                        const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = fallback, .ty = .I64 });
                    }
                    last_instruction_was_terminator = false;
                },
                .PushStorageId => |psid| {
                    // PushStorageId pushes a pointer to the variable, not the value
                    // This is used for alias parameters in method calls
                    if (psid.scope_kind == .GlobalLocal) {
                        const gname = psid.var_name;
                        const st = self.global_types.get(gname) orelse .I64;

                        // Ensure the global is declared
                        if (!self.defined_globals.contains(gname)) {
                            _ = try self.global_types.put(gname, st);
                            _ = try self.defined_globals.put(gname, true);
                        }

                        // Get the global pointer name
                        const gptr = try self.mangleGlobalName(gname);

                        // If the global stores a pointer (like a struct pointer), we need to load it first
                        // because methods expect a pointer to the struct, not a pointer to the global variable
                        const struct_fields = self.global_struct_field_types.get(gname);
                        if (st == .PTR) {
                            // Load the pointer value from the global
                            const loaded_ptr = try self.nextTemp(&id);
                            const llty = self.stackTypeToLLVMType(st);
                            const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ loaded_ptr, llty, gptr });
                            defer self.allocator.free(load_line);
                            try w.writeAll(load_line);
                            self.allocator.free(gptr);
                            try stack.append(.{ .name = loaded_ptr, .ty = .PTR, .struct_field_types = struct_fields });
                        } else {
                            // For non-pointer globals, push the global pointer directly
                            // Note: We don't free gptr here because it will be used in the call instruction
                            // The memory will be cleaned up when the IR printer is destroyed
                            try stack.append(.{ .name = gptr, .ty = .PTR, .struct_field_types = struct_fields });
                        }
                    } else if (variables.get(psid.var_name)) |entry| {
                        // Push the local variable pointer directly (not loaded)
                        try stack.append(.{ .name = entry.ptr_name, .ty = .PTR, .array_type = entry.array_type, .enum_type_name = entry.enum_type_name });
                    } else {
                        // Fallback: create a null pointer
                        const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = fallback, .ty = .PTR });
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreVar => |sv| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);

                    if (sv.scope_kind == .GlobalLocal) {
                        // Handle global variables
                        _ = try self.global_types.put(sv.var_name, value.ty);
                        if (value.array_type) |array_type| {
                            _ = try self.global_array_types.put(sv.var_name, array_type);
                        }
                        if (value.enum_type_name) |enum_type_name| {
                            _ = try self.global_enum_types.put(sv.var_name, enum_type_name);
                        }
                        _ = try self.defined_globals.put(sv.var_name, true);
                        const gptr = try self.mangleGlobalName(sv.var_name);
                        defer self.allocator.free(gptr);
                        if (value.struct_field_types) |fts| {
                            _ = try self.global_struct_field_types.put(sv.var_name, fts);
                        }
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, gptr });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    } else {
                        // Handle local variables
                        var info_ptr = variables.getPtr(sv.var_name);
                        if (info_ptr == null) {
                            // Variable should have been allocated at function entry
                            // This is an error condition - variable not found
                            continue;
                        } else {
                            info_ptr.?.stack_type = value.ty;
                            info_ptr.?.array_type = value.array_type;
                            info_ptr.?.enum_type_name = value.enum_type_name;
                        }
                        const store_line = try std.fmt.allocPrint(
                            self.allocator,
                            "  store {s} {s}, ptr {s}\n",
                            .{ llvm_ty, value.name, info_ptr.?.ptr_name },
                        );
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreDecl => |sd| {
                    if (stack.items.len < 1) {
                        // For StoreDecl with empty stack (nothing), just declare the variable without initial value
                        if (sd.scope_kind == .GlobalLocal) {
                            const stack_type = self.hirTypeToStackType(sd.declared_type);
                            _ = try self.global_types.put(sd.var_name, stack_type);
                            _ = try self.defined_globals.put(sd.var_name, true);
                            const gptr = try self.mangleGlobalName(sd.var_name);
                            defer self.allocator.free(gptr);
                            // Don't emit store instruction, just declare the global
                        }
                        continue;
                    }
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);
                    if (sd.scope_kind == .GlobalLocal) {
                        // Record global type and emit store to module-level global
                        _ = try self.global_types.put(sd.var_name, value.ty);
                        if (value.array_type) |array_type| {
                            _ = try self.global_array_types.put(sd.var_name, array_type);
                        }
                        _ = try self.defined_globals.put(sd.var_name, true);
                        const gptr = try self.mangleGlobalName(sd.var_name);
                        defer self.allocator.free(gptr);
                        if (value.struct_field_types) |fts| {
                            _ = try self.global_struct_field_types.put(sd.var_name, fts);
                        }
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, gptr });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    } else {
                        var info_ptr = variables.getPtr(sd.var_name);
                        if (info_ptr == null) {
                            // Variable should have been allocated at function entry
                            // This is an error condition - variable not found
                            continue;
                        } else {
                            info_ptr.?.stack_type = value.ty;
                            info_ptr.?.array_type = value.array_type;
                            info_ptr.?.enum_type_name = value.enum_type_name;
                        }
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, info_ptr.?.ptr_name });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreConst => |sc| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    // If name looks corrupted/huge, fall back to last enum value (when available)
                    if (value.name.len > 1000 and sc.scope_kind == .GlobalLocal) {
                        if (self.last_emitted_enum_value) |enum_idx| {
                            _ = try self.global_types.put(sc.var_name, .I64);
                            _ = try self.defined_globals.put(sc.var_name, true);
                            const gptr = try self.mangleGlobalName(sc.var_name);
                            defer self.allocator.free(gptr);
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store i64 {d}, ptr {s}\n", .{ enum_idx, gptr });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                            stack.items.len -= 1;
                            continue;
                        }
                    }

                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);
                    const is_global = switch (sc.scope_kind) {
                        .GlobalLocal, .ModuleGlobal => true,
                        else => false,
                    };
                    if (is_global) {
                        _ = try self.global_types.put(sc.var_name, value.ty);
                        if (value.array_type) |array_type| {
                            _ = try self.global_array_types.put(sc.var_name, array_type);
                        }
                        if (value.enum_type_name) |enum_type_name| {
                            _ = try self.global_enum_types.put(sc.var_name, enum_type_name);
                        }
                        if (value.struct_field_types) |fts| {
                            _ = try self.global_struct_field_types.put(sc.var_name, fts);
                        }
                        _ = try self.defined_globals.put(sc.var_name, true);
                        const gptr = try self.mangleGlobalName(sc.var_name);
                        defer self.allocator.free(gptr);
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, gptr });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    } else {
                        var info_ptr = variables.getPtr(sc.var_name);
                        if (info_ptr == null) {
                            // Variable should have been allocated at function entry
                            // This is an error condition - variable not found
                            continue;
                        } else {
                            info_ptr.?.stack_type = value.ty;
                            info_ptr.?.array_type = value.array_type;
                            info_ptr.?.enum_type_name = value.enum_type_name;
                        }
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, info_ptr.?.ptr_name });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    }
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
                                had_return = true;
                            },
                            else => {
                                // Non-int returns at top-level: fall back to success exit code
                                try w.writeAll("  ret i32 0\n");
                                had_return = true;
                            },
                        }
                    } else {
                        try w.writeAll("  ret i32 0\n");
                        had_return = true;
                    }
                    last_instruction_was_terminator = true;
                },
                .Unreachable => |_| {
                    try w.writeAll("  unreachable\n");
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                else => {},
            }
        }

        // Ensure a valid exit if control falls through
        if (!had_return) {
            try w.writeAll("  ret i32 0\n");
        }

        try w.writeAll("}\n");
    }

    fn getFunctionRange(
        self: *IRPrinter,
        hir: *const HIR.HIRProgram,
        func: HIR.HIRProgram.HIRFunction,
        func_start_labels: *std.StringHashMap(bool),
    ) ?struct { start: usize, end: usize } {
        const start_idx = self.findLabelIndex(hir, func.start_label) orelse return null;
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
        return .{ .start = start_idx, .end = end_idx };
    }

    fn collectFunctionStructReturnInfo(
        self: *IRPrinter,
        hir: *const HIR.HIRProgram,
        func: HIR.HIRProgram.HIRFunction,
        func_start_labels: *std.StringHashMap(bool),
    ) !void {
        const range = self.getFunctionRange(hir, func, func_start_labels) orelse return;
        var pending_fields: ?[]HIR.HIRType = null;
        var idx: usize = range.start + 1;
        while (idx < range.end) : (idx += 1) {
            const inst = hir.instructions[idx];
            switch (inst) {
                .StructNew => |sn| {
                    if (pending_fields) |existing| {
                        self.allocator.free(existing);
                    }
                    pending_fields = try self.allocator.dupe(HIR.HIRType, sn.field_types);
                },
                .Return => |ret| {
                    if (pending_fields) |fields| {
                        defer pending_fields = null;
                        if (ret.has_value and !self.function_struct_return_fields.contains(func.qualified_name)) {
                            _ = try self.function_struct_return_fields.put(func.qualified_name, fields);
                        } else {
                            self.allocator.free(fields);
                        }
                    }
                },
                .Label => {},
                else => {
                    if (pending_fields) |fields| {
                        self.allocator.free(fields);
                        pending_fields = null;
                    }
                },
            }
        }
        if (pending_fields) |fields| {
            self.allocator.free(fields);
        }
    }

    fn writeFunction(
        self: *IRPrinter,
        hir: *const HIR.HIRProgram,
        w: anytype,
        func: HIR.HIRProgram.HIRFunction,
        func_start_labels: *std.StringHashMap(bool),
        peek_state: *PeekEmitState,
    ) !void {
        const range = self.getFunctionRange(hir, func, func_start_labels) orelse return;
        const start_idx = range.start;
        const end_idx = range.end;

        // First pass: Collect all variables that need allocation
        var variables_to_allocate = std.StringHashMap(VariableInfo).init(self.allocator);
        defer {
            var it = variables_to_allocate.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.value_ptr.ptr_name);
            }
            variables_to_allocate.deinit();
        }

        const AliasInfo = struct {
            ptr_name: []const u8,
            pointee_type: HIR.HIRType,
            array_type: ?HIR.HIRType = null,
            struct_field_types: ?[]HIR.HIRType = null,
            enum_type_name: ?[]const u8 = null,
        };
        var alias_slots = std.AutoHashMap(u32, AliasInfo).init(self.allocator);
        defer alias_slots.deinit();

        // Scan through instructions to find all variables that need allocation
        for (hir.instructions[start_idx..end_idx]) |inst| {
            switch (inst) {
                .StoreVar => |sv| {
                    if (variables_to_allocate.get(sv.var_name) == null) {
                        // We need to determine the type from the HIR context
                        // For now, assume i64 as default - this should be improved
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sv.var_name});
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = .I64, .array_type = null };
                        try variables_to_allocate.put(sv.var_name, info);
                    }
                },
                .StoreDecl => |sd| {
                    if (variables_to_allocate.get(sd.var_name) == null) {
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sd.var_name});
                        const declared_stack_type = self.hirTypeToStackType(sd.declared_type);
                        const array_hint: ?HIR.HIRType = switch (sd.declared_type) {
                            .Array => |inner| inner.*,
                            else => null,
                        };
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = declared_stack_type, .array_type = array_hint };
                        try variables_to_allocate.put(sd.var_name, info);
                    }
                },
                .StoreConst => |sc| {
                    if (variables_to_allocate.get(sc.var_name) == null) {
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sc.var_name});
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = .I64, .array_type = null };
                        try variables_to_allocate.put(sc.var_name, info);
                    }
                },
                else => {},
            }
        }

        // Generate function signature
        const return_type_str = switch (func.return_type) {
            .Int => "i64",
            .Float => "double",
            .Byte => "i8",
            .Tetra => "i2",
            .String => "ptr",
            .Array => "ptr",
            .Map => "ptr",
            .Struct => "ptr",
            .Enum => "ptr",
            .Function => "ptr",
            .Union => "ptr",
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
            const is_alias = if (param_idx < func.param_is_alias.len) func.param_is_alias[param_idx] else false;
            const param_type_str = if (is_alias) "ptr" else switch (param_type) {
                .Int => "i64",
                .Float => "double",
                .Byte => "i8",
                .Tetra => "i2",
                .String => "ptr",
                .Array => "ptr",
                .Map => "ptr",
                .Struct => "ptr",
                .Enum => "ptr",
                .Function => "ptr",
                .Union => "ptr",
                else => "i64",
            };
            const param_str = try std.fmt.allocPrint(self.allocator, "{s} %{d}", .{ param_type_str, param_idx });
            try param_strs.append(param_str);
        }

        const params_str = if (param_strs.items.len == 0) "" else try std.mem.join(self.allocator, ", ", param_strs.items);
        defer if (param_strs.items.len > 0) self.allocator.free(params_str);

        // Rename user entry `main` so we can emit a proper C wrapper `@main`
        const emitted_name = if (func.is_entry and std.mem.eql(u8, func.qualified_name, "main"))
            "doxa_user_main"
        else
            func.qualified_name;

        const func_decl = try std.fmt.allocPrint(self.allocator, "define {s} @{s}({s}) {{\n", .{ return_type_str, emitted_name, params_str });
        defer self.allocator.free(func_decl);
        try w.writeAll(func_decl);

        // Add entry block
        try w.writeAll("entry:\n");

        // Initialize variables map
        var variables = std.StringHashMap(VariableInfo).init(self.allocator);
        defer {
            var it = variables.iterator();
            while (it.next()) |entry| {
                self.allocator.free(entry.value_ptr.ptr_name);
            }
            variables.deinit();
        }

        // Allocate all variables at function entry
        var it = variables_to_allocate.iterator();
        while (it.next()) |entry| {
            const var_name = entry.key_ptr.*;
            const var_info = entry.value_ptr.*;
            const llvm_ty = self.stackTypeToLLVMType(var_info.stack_type);
            const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ var_info.ptr_name, llvm_ty });
            defer self.allocator.free(alloca_line);
            try w.writeAll(alloca_line);

            // Add to variables map for later use
            try variables.put(var_name, var_info);
        }

        // Process function body instructions
        var id: usize = func.param_types.len; // Start after parameters
        var stack = std.array_list.Managed(StackVal).init(self.allocator);
        defer stack.deinit();
        var merge_map = std.StringHashMap(StackMergeState).init(self.allocator);
        defer {
            var it_merge = merge_map.iterator();
            while (it_merge.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
            }
            merge_map.deinit();
        }

        var last_instruction_was_terminator = false;
        var current_block: []const u8 = "entry";
        var synthetic_labels = std.array_list.Managed([]const u8).init(self.allocator);
        defer {
            for (synthetic_labels.items) |lbl| self.allocator.free(lbl);
            synthetic_labels.deinit();
        }
        var dead_block_counter: usize = 0;

        // Add parameters to stack
        for (func.param_types, 0..) |param_type, param_idx| {
            const param_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{param_idx});
            // Check if this is an alias parameter
            const is_alias = if (param_idx < func.param_is_alias.len) func.param_is_alias[param_idx] else false;
            const stack_type = if (is_alias) .PTR else self.hirTypeToStackType(param_type);
            const array_hint: ?HIR.HIRType = switch (param_type) {
                .Array => |inner| inner.*,
                else => null,
            };
            try stack.append(.{ .name = param_name, .ty = stack_type, .array_type = array_hint });
        }

        // Process function body instructions
        for (hir.instructions[start_idx..end_idx]) |inst| {
            const tag = std.meta.activeTag(inst);
            const requires_new_block = switch (tag) {
                .Label, .ExitScope => false,
                else => true,
            };
            if (last_instruction_was_terminator and requires_new_block) {
                const dead_label = try std.fmt.allocPrint(self.allocator, "dead_block_{d}", .{dead_block_counter});
                dead_block_counter += 1;
                try synthetic_labels.append(dead_label);
                const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{dead_label});
                defer self.allocator.free(line);
                try w.writeAll(line);
                current_block = dead_label;
                stack.items.len = 0;
                last_instruction_was_terminator = false;
            }
            switch (inst) {
                .Label => |lbl| {
                    // Only process function body labels, skip function start labels and invalid basic block names
                    const should_print = !std.mem.eql(u8, lbl.name, func.start_label) and !std.mem.startsWith(u8, lbl.name, "func_");
                    if (should_print and !last_instruction_was_terminator) {
                        const br_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{lbl.name});
                        defer self.allocator.free(br_line);
                        try w.writeAll(br_line);
                        last_instruction_was_terminator = true;
                    }
                    if (should_print) {
                        const line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl.name});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        last_instruction_was_terminator = false;
                    }
                    current_block = lbl.name;
                    try self.restoreStackForLabel(&merge_map, lbl.name, &stack, &id, w);
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
                            last_instruction_was_terminator = false;
                        },
                        .float => |f| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const literal = try self.formatFloatLiteral(f);
                            defer self.allocator.free(literal);
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double 0.0, {s}\n", .{ name, literal });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .F64 });
                            last_instruction_was_terminator = false;
                        },
                        .byte => |b| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i8 0, {d}\n", .{ name, b });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I8 });
                            last_instruction_was_terminator = false;
                        },
                        .tetra => |t| {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i2 0, {d}\n", .{ name, t });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .I2 });
                            last_instruction_was_terminator = false;
                        },
                        .string => |s| {
                            const info = try internPeekString(
                                self.allocator,
                                &peek_state.*.string_map,
                                &peek_state.*.strings,
                                peek_state.*.next_id_ptr,
                                &peek_state.*.globals,
                                s,
                            );
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ name, info.length, info.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = name, .ty = .PTR });
                            last_instruction_was_terminator = false;
                        },
                        .enum_variant => |ev| {
                            // Store enum variant as its index value (i64) but mark it as enum type
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, @as(i64, @intCast(ev.variant_index)) });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            // Store enum type name as a global string constant for later use
                            const type_name_global = try self.createEnumTypeNameGlobal(ev.type_name, &id);
                            try stack.append(.{ .name = name, .ty = .I64, .enum_type_name = type_name_global });
                            last_instruction_was_terminator = false;
                            self.last_emitted_enum_value = ev.variant_index;
                        },
                        .nothing => {
                            // nothing type - don't push anything to stack
                            last_instruction_was_terminator = false;
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
                    last_instruction_was_terminator = true;
                },
                .Unreachable => |_| {
                    try w.writeAll("  unreachable\n");
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .JumpCond => |jc| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const bool_val = try self.ensureBool(w, v, &id);
                    try self.recordStackForLabel(&merge_map, jc.label_true, stack.items, current_block);
                    try self.recordStackForLabel(&merge_map, jc.label_false, stack.items, current_block);
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ bool_val.name, jc.label_true, jc.label_false });
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .Jump => |j| {
                    if (last_instruction_was_terminator) continue;
                    try self.recordStackForLabel(&merge_map, j.label, stack.items, current_block);
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .LoadVar => |lv| {
                    if (lv.scope_kind == .GlobalLocal) {
                        const gname = lv.var_name;
                        const st = self.global_types.get(gname) orelse .I64;

                        // For Nothing types, don't emit load instruction (zero-sized type)
                        if (st == .Nothing) {
                            // Push a dummy value for Nothing type (it won't be used)
                            const result_name = try self.nextTemp(&id);
                            try stack.append(.{ .name = result_name, .ty = .Nothing });
                            continue;
                        }

                        const llty = self.stackTypeToLLVMType(st);
                        const gptr = try self.mangleGlobalName(gname);
                        defer self.allocator.free(gptr);

                        // Ensure the global is declared by adding it to defined_globals if not already there
                        if (!self.defined_globals.contains(gname)) {
                            _ = try self.global_types.put(gname, st);
                            _ = try self.defined_globals.put(gname, true);
                        }

                        const result_name = try self.nextTemp(&id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result_name, llty, gptr });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        const array_type = self.global_array_types.get(gname);
                        const enum_type_name = self.global_enum_types.get(gname);
                        const struct_fields = self.global_struct_field_types.get(gname);
                        try stack.append(.{ .name = result_name, .ty = st, .array_type = array_type, .enum_type_name = enum_type_name, .struct_field_types = struct_fields });
                    } else if (variables.get(lv.var_name)) |entry| {
                        const result_name = try self.nextTemp(&id);
                        const ty_str = self.stackTypeToLLVMType(entry.stack_type);
                        const line = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = load {s}, ptr {s}\n",
                            .{ result_name, ty_str, entry.ptr_name },
                        );
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = result_name, .ty = entry.stack_type, .array_type = entry.array_type, .enum_type_name = entry.enum_type_name });
                    } else {
                        const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = fallback, .ty = .I64 });
                    }
                    last_instruction_was_terminator = false;
                },
                .PrintBegin => |_| {
                    // no-op for now
                    last_instruction_was_terminator = false;
                },
                .PrintStr => |ps| {
                    // Emit write of string literal by const id; guard against out-of-range
                    const idx: usize = @intCast(ps.const_id);
                    if (idx < hir.constant_pool.len) {
                        const hv = hir.constant_pool[idx];
                        if (hv == .string) {
                            const s = hv.string;
                            const tmp = try self.nextTemp(&id);
                            const gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr @.str.{d}, i64 0, i64 0\n", .{ tmp, s.len + 1, idx });
                            defer self.allocator.free(gep);
                            try w.writeAll(gep);
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{tmp});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        }
                    }
                    last_instruction_was_terminator = false;
                },
                .PrintVal => |_| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    switch (v.ty) {
                        .I64 => {
                            // Check if this is an enum value
                            if (v.enum_type_name) |type_name| {
                                try self.emitEnumPrint(peek_state, w, &id, type_name, v.name);
                            } else {
                                // Regular integer print
                                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            }
                        },
                        .F64 => {
                            const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .PTR => {
                            const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        else => {},
                    }
                    last_instruction_was_terminator = false;
                },
                .PrintEnd => |_| {
                    // no-op for now (newlines are explicit constants in program)
                    last_instruction_was_terminator = false;
                },
                .ArrayNew => |a| {
                    try self.emitArrayNew(w, &stack, &id, a);
                    last_instruction_was_terminator = false;
                },
                .ArraySet => |_| {
                    try self.emitArraySet(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .ArrayGet => |ag| {
                    _ = ag; // bounds_check not implemented yet
                    try self.emitArrayGet(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .ArrayLen => {
                    try self.emitArrayLen(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .ArrayPop => {
                    try self.emitArrayPop(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .Range => |r| {
                    try self.emitRange(w, &stack, &id, r);
                    last_instruction_was_terminator = false;
                },
                .Dup => {
                    if (stack.items.len < 1) continue;
                    const top = stack.items[stack.items.len - 1];
                    // Deep copy the StackVal to preserve struct_field_types reference
                    const duped: StackVal = .{
                        .name = top.name,
                        .ty = top.ty,
                        .array_type = top.array_type,
                        .enum_type_name = top.enum_type_name,
                        .struct_field_types = top.struct_field_types, // Copy the pointer reference
                    };
                    try stack.append(duped);
                    last_instruction_was_terminator = false;
                },
                .Pop => {
                    if (stack.items.len < 1) continue;
                    stack.items.len -= 1;
                    last_instruction_was_terminator = false;
                },
                .Swap => {
                    if (stack.items.len < 2) continue;
                    const top_idx = stack.items.len - 1;
                    std.mem.swap(StackVal, &stack.items[top_idx], &stack.items[top_idx - 1]);
                    last_instruction_was_terminator = false;
                },
                .Arith => |a| {
                    if (stack.items.len < 2) continue;
                    var rhs = stack.items[stack.items.len - 1];
                    var lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;

                    // Check if we need type promotion for mixed Int/Float operations OR division (always uses floats)
                    const needs_promotion = (lhs.ty == .F64 and rhs.ty == .I64) or (lhs.ty == .I64 and rhs.ty == .F64) or
                        (a.op == .Div and (lhs.ty == .I64 or rhs.ty == .I64));

                    // If the operation is integer but operands are not i64, coerce them first (handles ptr, i8, etc.)
                    if (a.operand_type == .Int and !needs_promotion) {
                        lhs = try self.ensureI64(w, lhs, &id);
                        rhs = try self.ensureI64(w, rhs, &id);
                    }

                    if (needs_promotion) {
                        // Promote both operands to double and use floating-point operations
                        // Generate type conversion instructions
                        const lhs_promoted = if (lhs.ty == .I64) blk: {
                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, lhs.name });
                            defer self.allocator.free(conv_line);
                            try w.writeAll(conv_line);
                            break :blk conv_name;
                        } else lhs.name;
                        const rhs_promoted = if (rhs.ty == .I64) blk: {
                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, rhs.name });
                            defer self.allocator.free(conv_line);
                            try w.writeAll(conv_line);
                            break :blk conv_name;
                        } else rhs.name;

                        // Generate the operation instruction name after conversions
                        const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;

                        switch (a.op) {
                            .Add => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double {s}, {s}\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            .Sub => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = fsub double {s}, {s}\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            .Mul => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = fmul double {s}, {s}\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            .Div => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = fdiv double {s}, {s}\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            .Mod => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = frem double {s}, {s}\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                            .Pow => {
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ name, lhs_promoted, rhs_promoted });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            },
                        }
                        try stack.append(.{ .name = name, .ty = .F64 });
                    } else {
                        // Use the original logic for same-type operations
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
                                        const lhs_f = try std.fmt.allocPrint(self.allocator, "sitofp i64 {s} to double", .{lhs.name});
                                        defer self.allocator.free(lhs_f);
                                        const rhs_f = try std.fmt.allocPrint(self.allocator, "sitofp i64 {s} to double", .{rhs.name});
                                        defer self.allocator.free(rhs_f);
                                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ name, lhs_f, rhs_f });
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
                    }
                    last_instruction_was_terminator = false;
                },
                .Call => |c| {
                    const argc: usize = @intCast(c.arg_count);
                    if (stack.items.len < argc) continue;

                    var raw_args = std.array_list.Managed(StackVal).init(self.allocator);
                    defer raw_args.deinit();

                    var arg_idx: usize = 0;
                    while (arg_idx < argc) : (arg_idx += 1) {
                        const arg = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        try raw_args.append(arg);
                    }
                    std.mem.reverse(StackVal, raw_args.items);

                    var arg_strings = std.array_list.Managed([]const u8).init(self.allocator);
                    defer {
                        for (arg_strings.items) |s| self.allocator.free(s);
                        arg_strings.deinit();
                    }

                    const func_info = if (c.function_index < hir.function_table.len)
                        hir.function_table[c.function_index]
                    else
                        null;

                    for (raw_args.items, 0..) |arg, idx| {
                        const llvm_ty = blk: {
                            if (func_info) |info| {
                                if (idx < info.param_types.len) {
                                    const is_alias = if (idx < info.param_is_alias.len) info.param_is_alias[idx] else false;
                                    break :blk self.hirTypeToLLVMType(info.param_types[idx], is_alias);
                                }
                            }
                            break :blk self.stackTypeToLLVMType(arg.ty);
                        };
                        const arg_str = try std.fmt.allocPrint(self.allocator, "{s} {s}", .{ llvm_ty, arg.name });
                        try arg_strings.append(arg_str);
                    }

                    const args_str = if (arg_strings.items.len == 0) "" else try std.mem.join(self.allocator, ", ", arg_strings.items);
                    defer if (arg_strings.items.len > 0) self.allocator.free(args_str);

                    if (c.return_type != .Nothing) {
                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const ret_ty = self.hirTypeToLLVMType(c.return_type, false);
                        const runtime_name = mapBuiltinToRuntime(c.qualified_name);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call {s} @{s}({s})\n", .{ result_name, ret_ty, runtime_name, args_str });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                        const stack_ty = self.hirTypeToStackType(c.return_type);
                        var pushed = StackVal{ .name = result_name, .ty = stack_ty };
                        if (stack_ty == .PTR and c.return_type == .Struct) {
                            if (self.function_struct_return_fields.get(c.qualified_name)) |fts| {
                                pushed.struct_field_types = fts;
                            }
                        }
                        try stack.append(pushed);
                    } else {
                        const runtime_name = mapBuiltinToRuntime(c.qualified_name);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}({s})\n", .{ runtime_name, args_str });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StringOp => |sop| {
                    if (stack.items.len < 1) continue;
                    const arg = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    switch (sop.op) {
                        .ToInt => {
                            // Convert float to int using @doxa_int
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int(double {s})\n", .{ result_name, arg.name });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .I64 });
                        },
                        else => {
                            // Not implemented yet - push zero
                            const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            try stack.append(.{ .name = fallback, .ty = .I64 });
                        },
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreVar => |sv| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);
                    var info_ptr = variables.getPtr(sv.var_name);
                    if (info_ptr == null) {
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sv.var_name});
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ ptr_name, llvm_ty });
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = value.ty, .array_type = value.array_type };
                        try variables.put(sv.var_name, info);
                        info_ptr = variables.getPtr(sv.var_name);
                    } else {
                        info_ptr.?.stack_type = value.ty;
                        info_ptr.?.array_type = value.array_type;
                    }
                    const store_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  store {s} {s}, ptr {s}\n",
                        .{ llvm_ty, value.name, info_ptr.?.ptr_name },
                    );
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    last_instruction_was_terminator = false;
                },
                .StoreDecl => |sd| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);
                    var info_ptr = variables.getPtr(sd.var_name);
                    if (info_ptr == null) {
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sd.var_name});
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ ptr_name, llvm_ty });
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = value.ty, .array_type = value.array_type };
                        try variables.put(sd.var_name, info);
                        info_ptr = variables.getPtr(sd.var_name);
                    } else {
                        info_ptr.?.stack_type = value.ty;
                        info_ptr.?.array_type = value.array_type;
                    }
                    const store_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  store {s} {s}, ptr {s}\n",
                        .{ llvm_ty, value.name, info_ptr.?.ptr_name },
                    );
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    last_instruction_was_terminator = false;
                },
                .StoreConst => |sc| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const llvm_ty = self.stackTypeToLLVMType(value.ty);
                    var info_ptr = variables.getPtr(sc.var_name);
                    if (info_ptr == null) {
                        // Variable should have been allocated at function entry
                        // This is an error condition - variable not found
                        continue;
                    } else {
                        info_ptr.?.stack_type = value.ty;
                        info_ptr.?.array_type = value.array_type;
                        info_ptr.?.enum_type_name = value.enum_type_name;
                    }
                    const store_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  store {s} {s}, ptr {s}\n",
                        .{ llvm_ty, value.name, info_ptr.?.ptr_name },
                    );
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    last_instruction_was_terminator = false;
                },
                .Compare => |cmp| {
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const result = try self.emitCompareInstruction(w, cmp, lhs, rhs, &id);
                    try stack.append(result);
                    last_instruction_was_terminator = false;
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
                    last_instruction_was_terminator = false;
                },
                .Peek => |pk| {
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];

                    try self.emitPeekInstruction(w, pk, v, &id, peek_state);

                    // Print the actual value
                    switch (v.ty) {
                        .I64 => {
                            if (v.enum_type_name) |type_name| {
                                try self.emitEnumPrint(peek_state, w, &id, type_name, v.name);
                            } else {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            }
                        },
                        .F64 => {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{v.name});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        },
                        .PTR => {
                            if (v.array_type) |_| {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            } else {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            }
                        },
                        else => {},
                    }
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                    last_instruction_was_terminator = false;
                },
                .LoadAlias => |la| {
                    if (alias_slots.get(la.slot_index)) |info| {
                        const stack_ty = self.hirTypeToStackType(info.pointee_type);
                        if (stack_ty == .PTR) {
                            try stack.append(.{ .name = info.ptr_name, .ty = .PTR, .array_type = info.array_type, .struct_field_types = info.struct_field_types });
                        } else {
                            const result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const llvm_ty = self.hirTypeToLLVMType(info.pointee_type, false);
                            const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result, llvm_ty, info.ptr_name });
                            defer self.allocator.free(load_line);
                            try w.writeAll(load_line);
                            try stack.append(.{ .name = result, .ty = stack_ty, .array_type = info.array_type, .struct_field_types = info.struct_field_types });
                        }
                    } else {
                        const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = fallback, .ty = .I64 });
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreAlias => |sa| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    if (alias_slots.get(sa.slot_index)) |info| {
                        const llvm_ty = self.hirTypeToLLVMType(info.pointee_type, false);
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, info.ptr_name });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StoreParamAlias => |spa| {
                    if (stack.items.len < 1) continue;
                    const ptr_val = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    var struct_fields: ?[]HIR.HIRType = null;
                    if (spa.param_type == .Struct) {
                        if (std.mem.eql(u8, spa.param_name, "this")) {
                            if (std.mem.indexOfScalar(u8, func.qualified_name, '.')) |dot_idx| {
                                const struct_name = func.qualified_name[0..dot_idx];
                                if (self.global_struct_field_types.get(struct_name)) |fts| {
                                    struct_fields = fts;
                                }
                            }
                        } else if (self.global_struct_field_types.get(spa.param_name)) |fts| {
                            struct_fields = fts;
                        }
                    }
                    const array_hint: ?HIR.HIRType = switch (spa.param_type) {
                        .Array => |inner| inner.*,
                        else => null,
                    };
                    const alias_info = AliasInfo{
                        .ptr_name = ptr_val.name,
                        .pointee_type = spa.param_type,
                        .array_type = array_hint,
                        .struct_field_types = struct_fields,
                    };
                    try alias_slots.put(spa.var_index, alias_info);
                    last_instruction_was_terminator = false;
                },
                .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn),
                .StoreFieldName => |_| {
                    // StoreFieldName is a no-op in LLVM IR generation
                    // It's used by the VM to track field names for PeekStruct, but in LLVM
                    // we don't need to generate code for it. However, if there's a field name
                    // string on the stack (from the VM's perspective), we should pop it to avoid
                    // corrupting the stack.
                    // In practice, StoreFieldName doesn't push anything, so this is a no-op.
                    last_instruction_was_terminator = false;
                },
                .PeekStruct => |ps| {
                    // PeekStruct peeks a struct without popping it
                    // Similar to Peek, but for structs
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];

                    // Emit peek debug info
                    try self.emitPeekInstruction(w, .{
                        .name = null,
                        .value_type = .Struct,
                        .location = ps.location,
                        .union_members = null,
                    }, v, &id, peek_state);

                    // Print struct (for now, just print as pointer - proper struct printing would require
                    // iterating through fields, which is complex)
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{v.name});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");

                    // Don't pop or push - struct remains on stack
                    last_instruction_was_terminator = false;
                },
                // Add other instruction types as needed
                else => {},
            }
        }

        try w.writeAll("}\n\n");
    }

    fn nextTemp(self: *IRPrinter, id: *usize) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        return name;
    }

    fn nextTempText(self: *IRPrinter, id: *usize) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{id.*});
        id.* += 1;
        return name;
    }

    fn emitEnumPrint(
        self: *IRPrinter,
        peek_state: *PeekEmitState,
        w: anytype,
        id: *usize,
        type_name: []const u8,
        value_name: []const u8,
    ) !void {
        const info = try internPeekString(
            self.allocator,
            &peek_state.*.string_map,
            &peek_state.*.strings,
            peek_state.*.next_id_ptr,
            &peek_state.*.globals,
            type_name,
        );
        const type_ptr_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const type_ptr_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
            .{ type_ptr_name, info.length, info.name },
        );
        defer self.allocator.free(type_ptr_line);
        try w.writeAll(type_ptr_line);

        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_enum(ptr {s}, i64 {s})\n", .{ type_ptr_name, value_name });
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);
    }

    fn emitQuantifierWrappers(self: *IRPrinter, w: anytype) !void {
        const wrappers = [_]struct { name: []const u8, runtime: []const u8 }{
            .{ .name = "exists_quantifier_gt", .runtime = "doxa_exists_quantifier_gt" },
            .{ .name = "exists_quantifier_eq", .runtime = "doxa_exists_quantifier_eq" },
            .{ .name = "forall_quantifier_gt", .runtime = "doxa_forall_quantifier_gt" },
            .{ .name = "forall_quantifier_eq", .runtime = "doxa_forall_quantifier_eq" },
        };
        for (wrappers) |wrap| {
            const header = try std.fmt.allocPrint(self.allocator, "define i2 @{s}(ptr %hdr, i64 %value) {{\n", .{wrap.name});
            defer self.allocator.free(header);
            try w.writeAll(header);
            try w.writeAll("entry:\n");
            const call_line = try std.fmt.allocPrint(self.allocator, "  %res = call i8 @{s}(ptr %hdr, i64 %value)\n", .{wrap.runtime});
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            try w.writeAll("  %cast = trunc i8 %res to i2\n");
            try w.writeAll("  ret i2 %cast\n}\n\n");
        }
    }

    fn createEnumTypeNameGlobal(self: *IRPrinter, type_name: []const u8, _: *usize) ![]const u8 {
        // For now, just return the type name directly
        // In a full implementation, we'd create proper global string constants
        return try self.allocator.dupe(u8, type_name);
    }

    fn ensurePointer(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .PTR) return value;

        var current_name = value.name;
        var current_ty = value.ty;

        switch (current_ty) {
            .I64 => {},
            .I1, .I2, .I8 => {
                const widened = try self.nextTemp(id);
                const src_ty = self.stackTypeToLLVMType(current_ty);
                const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, current_name });
                defer self.allocator.free(widen_line);
                try w.writeAll(widen_line);
                current_name = widened;
                current_ty = .I64;
            },
            .F64 => {
                const bitcasted = try self.nextTemp(id);
                const bitcast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ bitcasted, current_name });
                defer self.allocator.free(bitcast_line);
                try w.writeAll(bitcast_line);
                current_name = bitcasted;
                current_ty = .I64;
            },
            else => {},
        }

        if (current_ty != .I64) {
            const widened = try self.nextTemp(id);
            const src_ty = self.stackTypeToLLVMType(current_ty);
            const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, current_name });
            defer self.allocator.free(widen_line);
            try w.writeAll(widen_line);
            current_name = widened;
        }

        const ptr_name = try self.nextTemp(id);
        const inttoptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ ptr_name, current_name });
        defer self.allocator.free(inttoptr_line);
        try w.writeAll(inttoptr_line);
        return .{ .name = ptr_name, .ty = .PTR, .array_type = value.array_type };
    }

    const ArrayLenLoad = struct {
        array: StackVal,
        len_ptr: []const u8,
        len_value: StackVal,
    };

    fn ensureI64(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .I64) return value;

        switch (value.ty) {
            .I1, .I2, .I8 => {
                const widened = try self.nextTemp(id);
                const src_ty = self.stackTypeToLLVMType(value.ty);
                const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, value.name });
                defer self.allocator.free(widen_line);
                try w.writeAll(widen_line);
                return .{ .name = widened, .ty = .I64 };
            },
            .PTR => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            .F64 => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            else => return value,
        }
    }

    fn arrayElementSize(_: *IRPrinter, element_type: HIR.HIRType) u64 {
        return switch (element_type) {
            .Int => 8,
            .Byte => 1,
            .Float => 8,
            .String => 8,
            .Tetra => 1,
            .Nothing => 0,
            else => 8,
        };
    }

    fn arrayElementTag(_: *IRPrinter, element_type: HIR.HIRType) u64 {
        return switch (element_type) {
            .Int => 0,
            .Byte => 1,
            .Float => 2,
            .String => 3,
            .Tetra => 4,
            .Nothing => 5,
            .Array => 6,
            .Struct => 7,
            .Enum => 8,
            else => 255,
        };
    }

    fn convertValueToArrayStorage(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        element_type: HIR.HIRType,
        id: *usize,
    ) !StackVal {
        return switch (element_type) {
            .Int, .Byte, .Tetra => try self.ensureI64(w, value, id),
            .Float => blk: {
                if (value.ty != .F64) {
                    break :blk try self.ensureI64(w, value, id);
                }
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk StackVal{ .name = tmp, .ty = .I64 };
            },
            .String, .Array, .Map, .Struct, .Enum, .Function, .Union => blk_ptr: {
                var ptr_val = value;
                if (ptr_val.ty != .PTR) {
                    ptr_val = try self.ensurePointer(w, value, id);
                }
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, ptr_val.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .I64 };
            },
            else => try self.ensureI64(w, value, id),
        };
    }

    fn convertArrayStorageToValue(
        self: *IRPrinter,
        w: anytype,
        storage: StackVal,
        element_type: HIR.HIRType,
        id: *usize,
    ) !StackVal {
        const as_i64 = if (storage.ty == .I64) storage else try self.ensureI64(w, storage, id);
        return switch (element_type) {
            .Int => as_i64,
            .Byte => blk: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk StackVal{ .name = tmp, .ty = .I8 };
            },
            .Tetra => blk_tetra: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_tetra StackVal{ .name = tmp, .ty = .I2 };
            },
            .Float => blk_float: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_float StackVal{ .name = tmp, .ty = .F64 };
            },
            .String, .Map, .Struct, .Enum, .Function, .Union => blk_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .PTR };
            },
            .Array => |inner| blk_arr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_arr StackVal{ .name = tmp, .ty = .PTR, .array_type = inner.* };
            },
            else => as_i64,
        };
    }

    fn loadArrayLength(
        self: *IRPrinter,
        w: anytype,
        arr: StackVal,
        id: *usize,
    ) !ArrayLenLoad {
        var array_ptr = arr;
        if (array_ptr.ty != .PTR) {
            array_ptr = try self.ensurePointer(w, array_ptr, id);
        }
        const len_ptr = try self.nextTemp(id);
        const gep_line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 1\n", .{ len_ptr, array_ptr.name });
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        const len_reg = try self.nextTemp(id);
        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ len_reg, len_ptr });
        defer self.allocator.free(load_line);
        try w.writeAll(load_line);

        return .{
            .array = array_ptr,
            .len_ptr = len_ptr,
            .len_value = .{ .name = len_reg, .ty = .I64 },
        };
    }

    fn emitArrayNew(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        inst: std.meta.TagPayload(HIRInstruction, .ArrayNew),
    ) !void {
        const elem_size = self.arrayElementSize(inst.element_type);
        const elem_tag = self.arrayElementTag(inst.element_type);
        const reg = try self.nextTemp(id);
        const line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call ptr @doxa_array_new(i64 {d}, i64 {d}, i64 {d})\n",
            .{ reg, elem_size, elem_tag, inst.size },
        );
        defer self.allocator.free(line);
        try w.writeAll(line);

        const arr_val = StackVal{ .name = reg, .ty = .PTR, .array_type = inst.element_type };
        try stack.append(arr_val);
        try stack.append(arr_val);
    }

    fn emitArraySet(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 3) return;
        const value = stack.items[stack.items.len - 1];
        const idx_val = stack.items[stack.items.len - 2];
        const hdr_val = stack.items[stack.items.len - 3];
        stack.items.len -= 3;

        var arr_ptr = hdr_val;
        if (arr_ptr.ty != .PTR) {
            arr_ptr = try self.ensurePointer(w, arr_ptr, id);
        }
        const element_type = arr_ptr.array_type orelse HIR.HIRType{ .Int = {} };
        const idx_i64 = try self.ensureI64(w, idx_val, id);
        const stored_val = try self.convertValueToArrayStorage(w, value, element_type, id);

        const call_line = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})\n",
            .{ arr_ptr.name, idx_i64.name, stored_val.name },
        );
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);

        try stack.append(.{ .name = arr_ptr.name, .ty = .PTR, .array_type = arr_ptr.array_type });
    }

    fn emitArrayGet(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 2) return;
        const idx_val = stack.items[stack.items.len - 1];
        const hdr_val = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        var arr_ptr = hdr_val;
        if (arr_ptr.ty != .PTR) {
            arr_ptr = try self.ensurePointer(w, arr_ptr, id);
        }
        const idx_i64 = try self.ensureI64(w, idx_val, id);

        // If we know the array element type, use runtime array get; otherwise treat as string/byte buffer
        if (arr_ptr.array_type) |element_type| {
            const elem_reg = try self.nextTemp(id);
            const call_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call i64 @doxa_array_get_i64(ptr {s}, i64 {s})\n",
                .{ elem_reg, arr_ptr.name, idx_i64.name },
            );
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);

            const stored = StackVal{ .name = elem_reg, .ty = .I64 };
            const actual = try self.convertArrayStorageToValue(w, stored, element_type, id);
            try stack.append(actual);
        } else {
            // Treat as string (C string). Build a 1-char C string for the indexed character.
            // Compute pointer to source character
            const src_gep = try self.nextTemp(id);
            const src_gep_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds i8, ptr {s}, i64 {s}\n",
                .{ src_gep, arr_ptr.name, idx_i64.name },
            );
            defer self.allocator.free(src_gep_line);
            try w.writeAll(src_gep_line);

            // Load the character byte
            const ch_val = try self.nextTemp(id);
            const load_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = load i8, ptr {s}\n",
                .{ ch_val, src_gep },
            );
            defer self.allocator.free(load_line);
            try w.writeAll(load_line);

            // Allocate a 2-byte buffer on the stack: [char, 0]
            const buf_ptr = try self.nextTemp(id);
            const alloca_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = alloca [2 x i8]\n",
                .{buf_ptr},
            );
            defer self.allocator.free(alloca_line);
            try w.writeAll(alloca_line);

            // Store the character at index 0
            const dst0_ptr = try self.nextTemp(id);
            const dst0_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [2 x i8], ptr {s}, i64 0, i64 0\n",
                .{ dst0_ptr, buf_ptr },
            );
            defer self.allocator.free(dst0_gep);
            try w.writeAll(dst0_gep);

            const store_ch = try std.fmt.allocPrint(
                self.allocator,
                "  store i8 {s}, ptr {s}\n",
                .{ ch_val, dst0_ptr },
            );
            defer self.allocator.free(store_ch);
            try w.writeAll(store_ch);

            // Store null terminator at index 1
            const dst1_ptr = try self.nextTemp(id);
            const dst1_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [2 x i8], ptr {s}, i64 0, i64 1\n",
                .{ dst1_ptr, buf_ptr },
            );
            defer self.allocator.free(dst1_gep);
            try w.writeAll(dst1_gep);

            const store_nul = try std.fmt.allocPrint(
                self.allocator,
                "  store i8 0, ptr {s}\n",
                .{dst1_ptr},
            );
            defer self.allocator.free(store_nul);
            try w.writeAll(store_nul);

            // Return pointer to start of temporary C string
            try stack.append(.{ .name = dst0_ptr, .ty = .PTR });
        }
    }

    fn emitArrayLen(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 1) return;
        const hdr_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        const len_info = try self.loadArrayLength(w, hdr_val, id);
        try stack.append(len_info.len_value);
    }

    fn emitArrayPop(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 1) return;
        const hdr_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        const len_info = try self.loadArrayLength(w, hdr_val, id);
        const element_type = len_info.array.array_type orelse HIR.HIRType{ .Int = {} };

        const idx = try self.nextTemp(id);
        const idx_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, -1\n", .{ idx, len_info.len_value.name });
        defer self.allocator.free(idx_line);
        try w.writeAll(idx_line);

        const elem_reg = try self.nextTemp(id);
        const call_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call i64 @doxa_array_get_i64(ptr {s}, i64 {s})\n",
            .{ elem_reg, len_info.array.name, idx },
        );
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);

        const store_line = try std.fmt.allocPrint(self.allocator, "  store i64 {s}, ptr {s}\n", .{ idx, len_info.len_ptr });
        defer self.allocator.free(store_line);
        try w.writeAll(store_line);

        const stored = StackVal{ .name = elem_reg, .ty = .I64 };
        const actual = try self.convertArrayStorageToValue(w, stored, element_type, id);

        try stack.append(.{ .name = len_info.array.name, .ty = .PTR, .array_type = len_info.array.array_type });
        try stack.append(actual);
    }

    fn emitRange(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        inst: std.meta.TagPayload(HIRInstruction, .Range),
    ) !void {
        _ = inst;

        if (stack.items.len < 2) return;
        const end_val = stack.items[stack.items.len - 1];
        const start_val = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        const start_i64 = try self.ensureI64(w, start_val, id);
        const end_i64 = try self.ensureI64(w, end_val, id);

        const one = try self.nextTemp(id);
        const one_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 1\n", .{one});
        defer self.allocator.free(one_line);
        try w.writeAll(one_line);

        const diff = try self.nextTemp(id);
        const diff_line = try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ diff, end_i64.name, start_i64.name });
        defer self.allocator.free(diff_line);
        try w.writeAll(diff_line);

        const len_raw = try self.nextTemp(id);
        const len_raw_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ len_raw, diff, one });
        defer self.allocator.free(len_raw_line);
        try w.writeAll(len_raw_line);

        const cmp = try self.nextTemp(id);
        const cmp_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp sge i64 {s}, {s}\n", .{ cmp, end_i64.name, start_i64.name });
        defer self.allocator.free(cmp_line);
        try w.writeAll(cmp_line);

        const zero = try self.nextTemp(id);
        const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{zero});
        defer self.allocator.free(zero_line);
        try w.writeAll(zero_line);

        const size = try self.nextTemp(id);
        const sel_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, i64 {s}, i64 {s}\n", .{ size, cmp, len_raw, zero });
        defer self.allocator.free(sel_line);
        try w.writeAll(sel_line);

        const ah = try self.nextTemp(id);
        const new_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_new(i64 8, i64 0, i64 {s})\n", .{ ah, size });
        defer self.allocator.free(new_line);
        try w.writeAll(new_line);

        // Allocate the loop index before the loop (must be in a dominating block)
        const i_ptr = try self.nextTemp(id);
        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{i_ptr});
        defer self.allocator.free(alloca_line);
        try w.writeAll(alloca_line);
        const st0 = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{i_ptr});
        defer self.allocator.free(st0);
        try w.writeAll(st0);

        const is_empty = try self.nextTemp(id);
        const empty_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i64 {s}, 0\n", .{ is_empty, size });
        defer self.allocator.free(empty_line);
        try w.writeAll(empty_line);

        const lbl_after = try std.fmt.allocPrint(self.allocator, "range.after.{d}", .{id.*});
        defer self.allocator.free(lbl_after);
        const lbl_cond = try std.fmt.allocPrint(self.allocator, "range.cond.{d}", .{id.*});
        defer self.allocator.free(lbl_cond);
        const lbl_body = try std.fmt.allocPrint(self.allocator, "range.body.{d}", .{id.*});
        defer self.allocator.free(lbl_body);
        const lbl_step = try std.fmt.allocPrint(self.allocator, "range.step.{d}", .{id.*});
        defer self.allocator.free(lbl_step);

        const br0 = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ is_empty, lbl_after, lbl_cond });
        defer self.allocator.free(br0);
        try w.writeAll(br0);

        // Start of the range condition block
        const cond_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl_cond});
        defer self.allocator.free(cond_label_line);
        try w.writeAll(cond_label_line);

        const i_cur = try self.nextTemp(id);
        const ld_i = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ i_cur, i_ptr });
        defer self.allocator.free(ld_i);
        try w.writeAll(ld_i);

        const cmp2 = try self.nextTemp(id);
        const cmp2_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp slt i64 {s}, {s}\n", .{ cmp2, i_cur, size });
        defer self.allocator.free(cmp2_line);
        try w.writeAll(cmp2_line);

        const br1 = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ cmp2, lbl_body, lbl_after });
        defer self.allocator.free(br1);
        try w.writeAll(br1);

        const body_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl_body});
        defer self.allocator.free(body_label_line);
        try w.writeAll(body_label_line);

        const val = try self.nextTemp(id);
        const val_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ val, start_i64.name, i_cur });
        defer self.allocator.free(val_line);
        try w.writeAll(val_line);

        const set_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})\n", .{ ah, i_cur, val });
        defer self.allocator.free(set_line);
        try w.writeAll(set_line);

        const br2 = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{lbl_step});
        defer self.allocator.free(br2);
        try w.writeAll(br2);

        const step_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl_step});
        defer self.allocator.free(step_label_line);
        try w.writeAll(step_label_line);

        const next_i = try self.nextTemp(id);
        const add1 = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, 1\n", .{ next_i, i_cur });
        defer self.allocator.free(add1);
        try w.writeAll(add1);

        const st_next = try std.fmt.allocPrint(self.allocator, "  store i64 {s}, ptr {s}\n", .{ next_i, i_ptr });
        defer self.allocator.free(st_next);
        try w.writeAll(st_next);

        const br3 = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{lbl_cond});
        defer self.allocator.free(br3);
        try w.writeAll(br3);

        const after_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{lbl_after});
        defer self.allocator.free(after_label_line);
        try w.writeAll(after_label_line);

        try stack.append(.{ .name = ah, .ty = .PTR, .array_type = .Int });
    }

    fn ensureBool(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        switch (value.ty) {
            .I1 => return value,
            .I64 => {
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 {s}, 0\n", .{ name, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
            .I8 => {
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i8 {s}, 0\n", .{ name, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
            .F64 => {
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = fcmp one double {s}, 0.0\n", .{ name, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
            .I2 => {
                const masked = try self.nextTemp(id);
                const mask_line = try std.fmt.allocPrint(self.allocator, "  {s} = and i2 {s}, 1\n", .{ masked, value.name });
                defer self.allocator.free(mask_line);
                try w.writeAll(mask_line);
                const name = try self.nextTemp(id);
                const cmp_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i2 {s}, 0\n", .{ name, masked });
                defer self.allocator.free(cmp_line);
                try w.writeAll(cmp_line);
                return .{ .name = name, .ty = .I1 };
            },
            .PTR => {
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne ptr {s}, null\n", .{ name, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
            .Nothing => {
                // nothing is falsy, so return false
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i1 0, 0\n", .{name});
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
        }
    }

    fn emitCompareInstruction(
        self: *IRPrinter,
        w: anytype,
        cmp: CompareInstruction,
        lhs: StackVal,
        rhs: StackVal,
        id: *usize,
    ) !StackVal {
        // Guard against corrupted operand names causing huge allocations
        if (lhs.name.len > 1000 or rhs.name.len > 1000) {
            const result_name = try self.nextTemp(id);
            const false_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i1 0, 1\n", .{result_name});
            defer self.allocator.free(false_line);
            try w.writeAll(false_line);
            return .{ .name = result_name, .ty = .I1 };
        }
        var result_name: []const u8 = undefined;
        var operand_type = cmp.operand_type;
        if (operand_type == .Int) {
            if (lhs.ty == .I2 and rhs.ty == .I2) {
                operand_type = .Tetra;
            } else if (lhs.ty == .I8 and rhs.ty == .I8) {
                operand_type = .Byte;
            }
        }
        const line = blk: {
            switch (operand_type) {
                .Int => {
                    result_name = try self.nextTemp(id);
                    const pred = switch (cmp.op) {
                        .Eq => "eq",
                        .Ne => "ne",
                        .Lt => "slt",
                        .Le => "sle",
                        .Gt => "sgt",
                        .Ge => "sge",
                    };
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i64 {s}, {s}\n", .{ result_name, pred, lhs.name, rhs.name });
                },
                .Byte => {
                    result_name = try self.nextTemp(id);
                    const pred = switch (cmp.op) {
                        .Eq => "eq",
                        .Ne => "ne",
                        .Lt => "ult",
                        .Le => "ule",
                        .Gt => "ugt",
                        .Ge => "uge",
                    };
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i8 {s}, {s}\n", .{ result_name, pred, lhs.name, rhs.name });
                },
                .Float => {
                    result_name = try self.nextTemp(id);
                    const pred = switch (cmp.op) {
                        .Eq => "oeq",
                        .Ne => "one",
                        .Lt => "olt",
                        .Le => "ole",
                        .Gt => "ogt",
                        .Ge => "oge",
                    };
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = fcmp {s} double {s}, {s}\n", .{ result_name, pred, lhs.name, rhs.name });
                },
                .Tetra => {
                    result_name = try self.nextTemp(id);
                    const pred = switch (cmp.op) {
                        .Eq => "eq",
                        .Ne => "ne",
                        .Lt => "slt",
                        .Le => "sle",
                        .Gt => "sgt",
                        .Ge => "sge",
                    };
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i2 {s}, {s}\n", .{ result_name, pred, lhs.name, rhs.name });
                },
                .String => {
                    const lhs_ptr = try self.ensurePointer(w, lhs, id);
                    const rhs_ptr = try self.ensurePointer(w, rhs, id);
                    switch (cmp.op) {
                        .Eq => {
                            result_name = try self.nextTemp(id);
                            break :blk try std.fmt.allocPrint(self.allocator, "  {s} = call i1 @doxa_str_eq(ptr {s}, ptr {s})\n", .{ result_name, lhs_ptr.name, rhs_ptr.name });
                        },
                        .Ne => {
                            const tmp_name = try self.nextTemp(id);
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i1 @doxa_str_eq(ptr {s}, ptr {s})\n", .{ tmp_name, lhs_ptr.name, rhs_ptr.name });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            result_name = try self.nextTemp(id);
                            break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i1 {s}, 0\n", .{ result_name, tmp_name });
                        },
                        else => {
                            result_name = try self.nextTemp(id);
                            const false_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i1 0, 1\n", .{result_name});
                            break :blk false_line;
                        },
                    }
                },
                else => {
                    result_name = try self.nextTemp(id);
                    const pred = switch (cmp.op) {
                        .Eq => "eq",
                        .Ne => "ne",
                        .Lt => "slt",
                        .Le => "sle",
                        .Gt => "sgt",
                        .Ge => "sge",
                    };
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i64 {s}, {s}\n", .{ result_name, pred, lhs.name, rhs.name });
                },
            }
        };
        defer self.allocator.free(line);
        try w.writeAll(line);
        return .{ .name = result_name, .ty = .I1 };
    }

    fn emitPeekInstruction(
        self: *IRPrinter,
        w: anytype,
        pk: anytype,
        value: StackVal,
        id: *usize,
        state: *PeekEmitState,
    ) !void {
        const type_slice = if (value.enum_type_name) |enum_type_name|
            enum_type_name
        else switch (value.ty) {
            .I64 => "int",
            .F64 => "float",
            .PTR => blk: {
                // Distinguish between arrays, structs, and raw strings
                if (value.array_type != null) break :blk "array";

                // If we have struct field type metadata, try to recover the
                // original struct name so peeks show "Point" instead of "string".
                if (value.struct_field_types) |fts| {
                    var it = self.global_struct_field_types.iterator();
                    while (it.next()) |entry| {
                        const candidate = entry.value_ptr.*;
                        if (candidate.ptr == fts.ptr and candidate.len == fts.len) {
                            break :blk entry.key_ptr.*;
                        }
                    }
                    // Fallback when we can't find an exact metadata match
                    break :blk "struct";
                }

                break :blk "string";
            },
            .I8 => "value",
            .I1 => "value",
            .I2 => "tetra",
            .Nothing => "nothing",
        };
        const type_info = try internPeekString(
            self.allocator,
            &state.string_map,
            &state.strings,
            state.next_id_ptr,
            &state.globals,
            type_slice,
        );

        var name_info: ?PeekStringInfo = null;
        if (pk.name) |nm| {
            name_info = try internPeekString(
                self.allocator,
                &state.string_map,
                &state.strings,
                state.next_id_ptr,
                &state.globals,
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
                &state.string_map,
                &state.strings,
                state.next_id_ptr,
                &state.globals,
                loc.file,
            );
            line_val = std.math.cast(u32, loc.range.start_line) orelse std.math.maxInt(u32);
            col_val = std.math.cast(u32, loc.range.start_col) orelse std.math.maxInt(u32);
            has_loc = 1;
        }

        const info_var = try self.nextTemp(id);
        defer self.allocator.free(info_var);
        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaPeekInfo\n", .{info_var});
        defer self.allocator.free(alloca_line);
        try w.writeAll(alloca_line);

        const file_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(file_field_ptr);
        const file_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 0\n",
            .{ file_field_ptr, info_var },
        );
        defer self.allocator.free(file_field_line);
        try w.writeAll(file_field_line);

        if (file_info) |fi| {
            const file_ptr_var = try self.nextTemp(id);
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

        const name_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(name_field_ptr);
        const name_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 1\n",
            .{ name_field_ptr, info_var },
        );
        defer self.allocator.free(name_field_line);
        try w.writeAll(name_field_line);

        if (name_info) |ni| {
            const name_ptr_var = try self.nextTemp(id);
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

        const type_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(type_field_ptr);
        const type_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 2\n",
            .{ type_field_ptr, info_var },
        );
        defer self.allocator.free(type_field_line);
        try w.writeAll(type_field_line);

        const type_ptr_var = try self.nextTemp(id);
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

        const members_field_ptr = try self.nextTemp(id);
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

        const count_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(count_field_ptr);
        const count_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 4\n",
            .{ count_field_ptr, info_var },
        );
        defer self.allocator.free(count_field_line);
        try w.writeAll(count_field_line);

        const member_count: i32 = 0;
        const store_count = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ member_count, count_field_ptr },
        );
        defer self.allocator.free(store_count);
        try w.writeAll(store_count);

        const active_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(active_field_ptr);
        const active_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 5\n",
            .{ active_field_ptr, info_var },
        );
        defer self.allocator.free(active_field_line);
        try w.writeAll(active_field_line);

        const active_index: i32 = -1;
        const store_active = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ active_index, active_field_ptr },
        );
        defer self.allocator.free(store_active);
        try w.writeAll(store_active);

        const has_loc_ptr = try self.nextTemp(id);
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

        const line_ptr = try self.nextTemp(id);
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

        const col_ptr = try self.nextTemp(id);
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
    }

    fn findLabelIndex(self: *IRPrinter, hir: *const HIR.HIRProgram, label: []const u8) ?usize {
        _ = self;
        for (hir.instructions, 0..) |inst, idx| {
            if (inst == .Label and std.mem.eql(u8, inst.Label.name, label)) return idx;
        }
        return null;
    }

    fn emitStructNew(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sn: std.meta.TagPayload(HIRInstruction, .StructNew)) !void {
        // StructNew expects fields on stack in reverse order, plus field names
        // For each field: value, then name (both pushed)
        // We need to pop 2 * field_count items (field names and values)
        if (stack.items.len < @as(usize, @intCast(sn.field_count)) * 2) {
            return error.StackUnderflow;
        }

        // Build inline LLVM struct type from field types: { t0, t1, ... }
        var struct_buf = std.ArrayListUnmanaged(u8){};
        defer struct_buf.deinit(self.allocator);
        try struct_buf.appendSlice(self.allocator, "{ ");
        const fcount: usize = @intCast(sn.field_count);
        var i: usize = 0;
        while (i < fcount) : (i += 1) {
            const ft = sn.field_types[i];
            const st = self.hirTypeToStackType(ft);
            const lt = self.stackTypeToLLVMType(st);
            try struct_buf.appendSlice(self.allocator, lt);
            if (i + 1 < fcount) {
                try struct_buf.appendSlice(self.allocator, ", ");
            }
        }
        try struct_buf.appendSlice(self.allocator, " }");
        const struct_type_llvm = try struct_buf.toOwnedSlice(self.allocator);
        defer self.allocator.free(struct_type_llvm);

        // Allocate struct on heap using malloc (since we're returning a pointer)
        // Calculate struct size
        const size_temp = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(size_temp);

        // For now, assume all fields are i64 (8 bytes) - this is a simplification
        // In a real implementation, we'd need to calculate the actual size
        const field_count_u64: u64 = @intCast(sn.field_count);
        const struct_size = field_count_u64 * 8; // 8 bytes per i64 field
        const size_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ size_temp, struct_size });
        defer self.allocator.free(size_line);
        try w.writeAll(size_line);

        // Call malloc
        const malloc_temp = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(malloc_temp);

        const malloc_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @malloc(i64 {s})\n", .{ malloc_temp, size_temp });
        defer self.allocator.free(malloc_line);
        try w.writeAll(malloc_line);

        // Cast to struct pointer type
        const struct_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;

        const cast_malloc_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast ptr {s} to ptr\n", .{ struct_ptr, malloc_temp });
        defer self.allocator.free(cast_malloc_line);
        try w.writeAll(cast_malloc_line);

        // Pop name/value pairs off the stack in forward field order (see HIR push order)
        var idx_usize: usize = 0;
        while (idx_usize < @as(usize, @intCast(sn.field_count))) : (idx_usize += 1) {
            // Pop field name
            if (stack.items.len < 1) return error.StackUnderflow;
            stack.items.len -= 1;

            // Pop field value
            if (stack.items.len < 1) return error.StackUnderflow;
            const field_val = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            // Field type for this index
            const field_type = sn.field_types[idx_usize];
            const field_stack_type = self.hirTypeToStackType(field_type);
            const field_llvm_type = self.stackTypeToLLVMType(field_stack_type);

            // Compute field GEP
            const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            defer self.allocator.free(field_gep);

            const gep_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
                .{ field_gep, struct_type_llvm, struct_ptr, @as(i32, @intCast(idx_usize)) },
            );
            defer self.allocator.free(gep_line);
            try w.writeAll(gep_line);

            // Coerce and store
            var store_val = field_val;
            switch (field_stack_type) {
                .I64 => {
                    store_val = try self.ensureI64(w, field_val, id);
                },
                .PTR => {
                    if (field_val.ty != .PTR) {
                        store_val = try self.ensurePointer(w, field_val, id);
                    }
                },
                .F64 => {
                    if (field_val.ty != .F64) {
                        const as_i64 = try self.ensureI64(w, field_val, id);
                        const tmp = try self.nextTemp(id);
                        const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ tmp, as_i64.name });
                        defer self.allocator.free(cast_line);
                        try w.writeAll(cast_line);
                        store_val = .{ .name = tmp, .ty = .F64 };
                    }
                },
                .I8 => {
                    if (field_val.ty != .I8) {
                        const as_i64 = try self.ensureI64(w, field_val, id);
                        const tmp = try self.nextTemp(id);
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ tmp, as_i64.name });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);
                        store_val = .{ .name = tmp, .ty = .I8 };
                    }
                },
                .I1 => {
                    if (field_val.ty != .I1) {
                        const as_i64 = try self.ensureI64(w, field_val, id);
                        const tmp = try self.nextTemp(id);
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i1\n", .{ tmp, as_i64.name });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);
                        store_val = .{ .name = tmp, .ty = .I1 };
                    }
                },
                .I2 => {
                    if (field_val.ty != .I2) {
                        const as_i64 = try self.ensureI64(w, field_val, id);
                        const tmp = try self.nextTemp(id);
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ tmp, as_i64.name });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);
                        store_val = .{ .name = tmp, .ty = .I2 };
                    }
                },
                .Nothing => {},
            }

            const store_line = try std.fmt.allocPrint(
                self.allocator,
                "  store {s} {s}, ptr {s}\n",
                .{ field_llvm_type, store_val.name, field_gep },
            );
            defer self.allocator.free(store_line);
            try w.writeAll(store_line);
        }

        // Push struct pointer onto stack, remember field types for later GEPs
        // Push struct pointer onto stack with field types metadata
        try stack.append(.{ .name = struct_ptr, .ty = .PTR, .struct_field_types = try self.allocator.dupe(HIR.HIRType, sn.field_types) });

        // Store struct field types globally for later lookup
        try self.global_struct_field_types.put(sn.type_name, try self.allocator.dupe(HIR.HIRType, sn.field_types));
    }

    fn emitGetField(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, gf: std.meta.TagPayload(HIRInstruction, .GetField)) !void {
        if (stack.items.len < 1) return error.StackUnderflow;
        var struct_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        if (struct_val.ty != .PTR) {
            var as_ptr = try self.ensurePointer(w, struct_val, id);
            as_ptr.struct_field_types = struct_val.struct_field_types;
            struct_val = as_ptr;
        }

        // Rebuild the inline struct type from the source value if available
        // BUT: if we're accessing a nested struct field (e.g., mike.person.age),
        // we need to use the inner struct's type (Person) not the container's type (Employee)
        var struct_type_llvm: []const u8 = undefined;
        var needs_free = false;
        const actual_struct_field_types: ?[]HIR.HIRType = struct_val.struct_field_types;

        if (actual_struct_field_types) |fts| {
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(self.allocator);
            try buf.appendSlice(self.allocator, "{ ");
            var i: usize = 0;
            while (i < fts.len) : (i += 1) {
                const st = self.hirTypeToStackType(fts[i]);
                const lt = self.stackTypeToLLVMType(st);
                try buf.appendSlice(self.allocator, lt);
                if (i + 1 < fts.len) try buf.appendSlice(self.allocator, ", ");
            }
            try buf.appendSlice(self.allocator, " }");
            struct_type_llvm = try buf.toOwnedSlice(self.allocator);
            needs_free = true;
        } else {
            struct_type_llvm = try self.buildFallbackStructType(gf.field_index);
            needs_free = true;
        }
        defer if (needs_free) self.allocator.free(struct_type_llvm);

        // Get field type; prefer metadata from the struct value
        // BUT: if we're accessing a nested struct field, we might need to use the inner struct's field types
        const field_type: HIR.HIRType = if (actual_struct_field_types) |fts|
            fts[@intCast(gf.field_index)]
        else if (struct_val.struct_field_types) |fts|
            fts[@intCast(gf.field_index)]
        else switch (gf.container_type) {
            .Struct => |_| HIR.HIRType{ .Int = {} }, // Fallback to int
            else => HIR.HIRType{ .Int = {} },
        };
        const field_stack_type = self.hirTypeToStackType(field_type);
        const field_llvm_type = self.stackTypeToLLVMType(field_stack_type);

        // Generate GEP to get field pointer
        // IMPORTANT: If we're accessing a nested struct field (e.g., mike.person.age),
        // we need to use the inner struct's type (Person) not the container's type (Employee)
        // The struct_val.name is the pointer we're accessing, so we need to use the correct struct type
        const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(field_gep);

        const gep_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
            .{ field_gep, struct_type_llvm, struct_val.name, gf.field_index },
        );
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        // Load field value
        const field_val = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;

        const load_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = load {s}, ptr {s}\n",
            .{ field_val, field_llvm_type, field_gep },
        );
        defer self.allocator.free(load_line);
        try w.writeAll(load_line);

        // Push field value onto stack
        // If the field is a struct pointer, try to preserve the struct field types metadata
        // for subsequent nested field accesses.
        if (field_stack_type == .PTR) {
            const pushed: StackVal = .{ .name = field_val, .ty = field_stack_type };
            try stack.append(pushed);
        } else {
            try stack.append(.{ .name = field_val, .ty = field_stack_type });
        }
    }

    fn emitSetField(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sf: std.meta.TagPayload(HIRInstruction, .SetField)) !void {
        if (stack.items.len < 2) return error.StackUnderflow;
        const value = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        var struct_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        if (struct_val.ty != .PTR) {
            var as_ptr = try self.ensurePointer(w, struct_val, id);
            as_ptr.struct_field_types = struct_val.struct_field_types;
            struct_val = as_ptr;
        }

        // Rebuild the inline struct type from the source value if available
        // The struct_val should have struct_field_types set if it came from a GetField that accessed a struct
        var struct_type_llvm_set: []const u8 = undefined;
        var needs_free_set = false;
        const actual_struct_field_types_set: ?[]HIR.HIRType = struct_val.struct_field_types;

        if (actual_struct_field_types_set) |fts| {
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(self.allocator);
            try buf.appendSlice(self.allocator, "{ ");
            var i: usize = 0;
            while (i < fts.len) : (i += 1) {
                const st = self.hirTypeToStackType(fts[i]);
                const lt = self.stackTypeToLLVMType(st);
                try buf.appendSlice(self.allocator, lt);
                if (i + 1 < fts.len) try buf.appendSlice(self.allocator, ", ");
            }
            try buf.appendSlice(self.allocator, " }");
            struct_type_llvm_set = try buf.toOwnedSlice(self.allocator);
            needs_free_set = true;
        } else {
            struct_type_llvm_set = try self.buildFallbackStructType(sf.field_index);
            needs_free_set = true;
        }
        defer if (needs_free_set) self.allocator.free(struct_type_llvm_set);

        // Get field type; prefer metadata from the struct value
        const field_type: HIR.HIRType = if (actual_struct_field_types_set) |fts|
            fts[@intCast(sf.field_index)]
        else if (struct_val.struct_field_types) |fts|
            fts[@intCast(sf.field_index)]
        else switch (sf.container_type) {
            .Struct => |_| HIR.HIRType{ .Int = {} }, // Fallback to int
            else => HIR.HIRType{ .Int = {} },
        };
        const field_stack_type = self.hirTypeToStackType(field_type);
        const field_llvm_type = self.stackTypeToLLVMType(field_stack_type);

        // Generate GEP to get field pointer
        const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(field_gep);

        const gep_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
            .{ field_gep, struct_type_llvm_set, struct_val.name, sf.field_index },
        );
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        // Store value to field
        const store_line = try std.fmt.allocPrint(
            self.allocator,
            "  store {s} {s}, ptr {s}\n",
            .{ field_llvm_type, value.name, field_gep },
        );
        defer self.allocator.free(store_line);
        try w.writeAll(store_line);

        // Push struct pointer back onto stack
        try stack.append(struct_val);
    }

    fn buildFallbackStructType(self: *IRPrinter, field_index: u32) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);
        const fallback_fields: usize = @intCast(field_index + 1);
        try buf.appendSlice(self.allocator, "{ ");
        var i: usize = 0;
        while (i < fallback_fields) : (i += 1) {
            try buf.appendSlice(self.allocator, "i64");
            if (i + 1 < fallback_fields) try buf.appendSlice(self.allocator, ", ");
        }
        try buf.appendSlice(self.allocator, " }");
        return try buf.toOwnedSlice(self.allocator);
    }

    fn hirTypeToStackType(self: *IRPrinter, hir_type: HIR.HIRType) StackType {
        _ = self;
        return switch (hir_type) {
            .Int => .I64,
            .Float => .F64,
            .Byte => .I8,
            .Tetra => .I2,
            .String => .PTR,
            .Array => .PTR,
            .Map => .PTR,
            .Struct => .PTR,
            .Enum => .I64,
            .Function => .PTR,
            .Union => .PTR,
            .Nothing => .Nothing,
            else => .I64,
        };
    }

    fn stackTypeToLLVMType(self: *IRPrinter, stack_type: StackType) []const u8 {
        _ = self;
        return switch (stack_type) {
            .I64 => "i64",
            .F64 => "double",
            .I8 => "i8",
            .I1 => "i1",
            .I2 => "i2",
            .PTR => "ptr",
            .Nothing => "{}", // Zero-sized type
        };
    }

    fn hirTypeToLLVMType(self: *IRPrinter, hir_type: HIR.HIRType, as_pointer: bool) []const u8 {
        _ = self;
        if (as_pointer) return "ptr";
        return switch (hir_type) {
            .Int => "i64",
            .Float => "double",
            .Byte => "i8",
            .Tetra => "i2",
            .String => "ptr",
            .Array => "ptr",
            .Map => "ptr",
            .Struct => "ptr",
            .Enum => "ptr",
            .Function => "ptr",
            .Union => "ptr",
            .Nothing => "void",
            else => "i64",
        };
    }
};
