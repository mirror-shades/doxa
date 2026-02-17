const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const StackSlot = Ctx.StackSlot;
    const StackMergeState = Ctx.StackMergeState;
    const EnumVariantMeta = Ctx.EnumVariantMeta;

    return struct {
        pub fn formatFloatLiteral(self: *IRPrinter, value: f64) ![]u8 {
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

        pub fn paramTypeMatchesStack(_: *IRPrinter, param_type: HIR.HIRType, stack_type: StackType) bool {
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

        pub fn coerceForMerge(
            self: *IRPrinter,
            incoming: StackVal,
            target: StackType,
            id: *usize,
            w: anytype,
        ) !StackVal {
            if (incoming.ty == target) return incoming;

            if (incoming.ty == .Value) {
                const payload = try self.nextTemp(id);
                const payload_extract = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = extractvalue %DoxaValue {s}, 2\n",
                    .{ payload, incoming.name },
                );
                defer self.allocator.free(payload_extract);
                try w.writeAll(payload_extract);

                return switch (target) {
                    .I64 => .{ .name = payload, .ty = .I64 },
                    .F64 => blk: {
                        const as_f64 = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ as_f64, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        break :blk .{ .name = as_f64, .ty = .F64 };
                    },
                    .I8 => blk: {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ narrowed, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        break :blk .{ .name = narrowed, .ty = .I8 };
                    },
                    .I2 => blk: {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ narrowed, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        break :blk .{ .name = narrowed, .ty = .I2 };
                    },
                    .I1 => blk: {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i1\n", .{ narrowed, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        break :blk .{ .name = narrowed, .ty = .I1 };
                    },
                    .PTR => blk: {
                        const as_ptr = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        break :blk .{ .name = as_ptr, .ty = .PTR };
                    },
                    else => incoming,
                };
            }

            if (target == .Value and incoming.ty != .Value) {
                const tag_const: i32 = switch (incoming.ty) {
                    .I64 => 0,
                    .F64 => 1,
                    .I8 => 2,
                    .PTR => 3,
                    .I2, .I1 => 7,
                    else => 8,
                };
                const tag_reg = try self.nextTemp(id);
                const tag_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i32 0, {d}\n", .{ tag_reg, tag_const });
                defer self.allocator.free(tag_line);
                try w.writeAll(tag_line);

                const reserved_reg = try self.nextTemp(id);
                const reserved_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i32 0, 0\n", .{reserved_reg});
                defer self.allocator.free(reserved_line);
                try w.writeAll(reserved_line);

                const payload = try self.ensureI64(w, incoming, id);

                const dv0 = try self.nextTemp(id);
                const dv0_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = insertvalue %DoxaValue undef, i32 {s}, 0\n",
                    .{ dv0, tag_reg },
                );
                defer self.allocator.free(dv0_line);
                try w.writeAll(dv0_line);

                const dv1 = try self.nextTemp(id);
                const dv1_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = insertvalue %DoxaValue {s}, i32 {s}, 1\n",
                    .{ dv1, dv0, reserved_reg },
                );
                defer self.allocator.free(dv1_line);
                try w.writeAll(dv1_line);

                const dv2 = try self.nextTemp(id);
                const dv2_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = insertvalue %DoxaValue {s}, i64 {s}, 2\n",
                    .{ dv2, dv1, payload.name },
                );
                defer self.allocator.free(dv2_line);
                try w.writeAll(dv2_line);

                return .{ .name = dv2, .ty = .Value };
            }

            return incoming;
        }

        pub fn coerceForStore(
            self: *IRPrinter,
            incoming: StackVal,
            target: StackType,
            id: *usize,
            w: anytype,
        ) !StackVal {
            if (incoming.ty == target) return incoming;
            if (target == .Nothing or incoming.ty == .Nothing) return incoming;

            if (incoming.ty == .Value or target == .Value) {
                return self.coerceForMerge(incoming, target, id, w);
            }

            switch (target) {
                .I8 => switch (incoming.ty) {
                    .I64 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I8 };
                    },
                    .I2 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i8\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I8 };
                    },
                    .I1 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i8\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I8 };
                    },
                    else => {},
                },
                .I2 => switch (incoming.ty) {
                    .I64 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I2 };
                    },
                    .I8 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I2 };
                    },
                    .I1 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I2 };
                    },
                    else => {},
                },
                .I1 => switch (incoming.ty) {
                    .I64 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i1\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I1 };
                    },
                    .I8 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i1\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I1 };
                    },
                    .I2 => {
                        const narrowed = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i2 {s} to i1\n", .{ narrowed, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = narrowed, .ty = .I1 };
                    },
                    else => {},
                },
                .I64 => switch (incoming.ty) {
                    .I8 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I64 };
                    },
                    .I2 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I64 };
                    },
                    .I1 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i64\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .I64 };
                    },
                    .PTR => {
                        const as_i64 = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ as_i64, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = as_i64, .ty = .I64 };
                    },
                    else => {},
                },
                .PTR => switch (incoming.ty) {
                    .I64 => {
                        const as_ptr = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = as_ptr, .ty = .PTR };
                    },
                    .I1, .I2, .I8, .F64 => {
                        return self.ensurePointer(w, incoming, id);
                    },
                    else => {},
                },
                else => {},
            }

            return incoming;
        }

        pub fn recordStackForLabel(
            self: *IRPrinter,
            map: *std.StringHashMap(StackMergeState),
            label: []const u8,
            stack: []const StackVal,
            current_block: []const u8,
            id: *usize,
            w: anytype,
        ) !void {
            var entry = try map.getOrPut(label);
            if (!entry.found_existing) {
                entry.value_ptr.* = try StackMergeState.init(self.allocator, stack.len);
            } else if (entry.value_ptr.slots.len < stack.len) {
                const old_len = entry.value_ptr.slots.len;
                const new_slots = try self.allocator.realloc(entry.value_ptr.slots, stack.len);
                entry.value_ptr.slots = new_slots;
                for (old_len..stack.len) |i| {
                    new_slots[i] = StackSlot{};
                }
            }

            for (entry.value_ptr.slots[0..stack.len], stack) |*slot, value| {
                var coerced = value;
                if (slot.items.len > 0) {
                    const target_type = slot.items[0].value.ty;
                    coerced = try self.coerceForMerge(value, target_type, id, w);
                }
                try slot.append(self.allocator, .{ .block = current_block, .value = coerced });
            }
        }

        pub fn restoreStackForLabel(
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

                    // Skip Nothing values - they are zero-sized and cannot participate in phi nodes
                    if (slot.items[0].value.ty == .Nothing) {
                        try stack.append(slot.items[0].value);
                        continue;
                    }

                    var needs_i2_conversion = false;
                    var target_type = slot.items[0].value.ty;
                    for (slot.items) |incoming_val| {
                        if (incoming_val.value.ty == .I2) {
                            needs_i2_conversion = true;
                            target_type = .I2;
                            break;
                        }
                    }

                    const phi_name = try self.nextTempText(id);
                    const type_str = self.stackTypeToLLVMType(target_type);

                    var incoming = std.array_list.Managed([]const u8).init(self.allocator);
                    defer {
                        for (incoming.items) |entry_str| self.allocator.free(entry_str);
                        incoming.deinit();
                    }

                    for (slot.items) |incoming_val| {
                        var adjusted = incoming_val.value;
                        if (adjusted.ty != target_type) {
                            adjusted = try self.coerceForMerge(adjusted, target_type, id, w);
                        }
                        const blk_name: []const u8 = if (std.mem.startsWith(u8, incoming_val.block, "func_")) "entry" else incoming_val.block;
                        const value_name = adjusted.name;
                        const pair = try std.fmt.allocPrint(self.allocator, "[ {s}, %{s} ]", .{ value_name, blk_name });
                        try incoming.append(pair);
                    }

                    const joined = if (incoming.items.len == 0) "" else try std.mem.join(self.allocator, ", ", incoming.items);
                    defer if (incoming.items.len > 0) self.allocator.free(joined);

                    const phi_line = try std.fmt.allocPrint(self.allocator, "  {s} = phi {s} {s}\n", .{ phi_name, type_str, joined });
                    defer self.allocator.free(phi_line);
                    try w.writeAll(phi_line);

                    // Preserve the richest metadata across incoming values so that
                    // downstream printing/type checks don't lose struct/enum context.
                    var merged_array_type: ?HIR.HIRType = slot.items[0].value.array_type;
                    var merged_enum_type_name: ?[]const u8 = slot.items[0].value.enum_type_name;
                    var merged_struct_field_types: ?[]HIR.HIRType = slot.items[0].value.struct_field_types;
                    var merged_struct_field_names: ?[]const []const u8 = slot.items[0].value.struct_field_names;
                    var merged_struct_type_name: ?[]const u8 = slot.items[0].value.struct_type_name;
                    for (slot.items) |incoming_val| {
                        if (merged_array_type == null and incoming_val.value.array_type != null) {
                            merged_array_type = incoming_val.value.array_type;
                        }
                        if (merged_enum_type_name == null and incoming_val.value.enum_type_name != null) {
                            merged_enum_type_name = incoming_val.value.enum_type_name;
                        }
                        if (merged_struct_field_types == null and incoming_val.value.struct_field_types != null) {
                            merged_struct_field_types = incoming_val.value.struct_field_types;
                        }
                        if (merged_struct_field_names == null and incoming_val.value.struct_field_names != null) {
                            merged_struct_field_names = incoming_val.value.struct_field_names;
                        }
                        if (merged_struct_type_name == null and incoming_val.value.struct_type_name != null) {
                            merged_struct_type_name = incoming_val.value.struct_type_name;
                        }
                    }

                    try stack.append(.{
                        .name = phi_name,
                        .ty = target_type,
                        .array_type = merged_array_type,
                        .enum_type_name = merged_enum_type_name,
                        .struct_field_types = merged_struct_field_types,
                        .struct_field_names = merged_struct_field_names,
                        .struct_type_name = merged_struct_type_name,
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
                .global_struct_field_names = std.StringHashMap([]const []const u8).init(allocator),
                .global_struct_type_names = std.StringHashMap([]const u8).init(allocator),
                .struct_fields_by_id = std.AutoHashMap(HIR.StructId, []HIR.HIRType).init(allocator),
                .struct_type_names_by_id = std.AutoHashMap(HIR.StructId, []const u8).init(allocator),
                .defined_globals = std.StringHashMap(bool).init(allocator),
                .last_emitted_enum_value = null,
                .function_struct_return_fields = std.StringHashMap([]HIR.HIRType).init(allocator),
                .function_struct_return_type_names = std.StringHashMap([]const u8).init(allocator),
                .struct_field_names_by_type = std.StringHashMap([]const []const u8).init(allocator),
                .struct_field_enum_type_names_by_type = std.StringHashMap([]const ?[]const u8).init(allocator),
                .struct_desc_globals_by_type = std.StringHashMap([]const u8).init(allocator),
                .enum_desc_globals_by_type = std.StringHashMap([]const u8).init(allocator),
                .enum_print_map = std.StringHashMap(std.ArrayListUnmanaged(EnumVariantMeta)).init(allocator),
            };
        }

        pub fn deinit(self: *IRPrinter) void {
            self.peek_string_counter = 0;
            self.global_types.deinit();
            self.global_array_types.deinit();
            self.global_enum_types.deinit();
            self.global_struct_field_types.deinit();
            self.global_struct_field_names.deinit();
            self.global_struct_type_names.deinit();
            self.struct_fields_by_id.deinit();
            self.struct_type_names_by_id.deinit();
            self.defined_globals.deinit();
            var ret_it = self.function_struct_return_fields.iterator();
            while (ret_it.next()) |entry| {
                self.allocator.free(entry.value_ptr.*);
            }
            self.function_struct_return_fields.deinit();
            self.function_struct_return_type_names.deinit();
            var names_it = self.struct_field_names_by_type.iterator();
            while (names_it.next()) |entry| {
                for (entry.value_ptr.*) |n| self.allocator.free(n);
                self.allocator.free(entry.value_ptr.*);
            }
            self.struct_field_names_by_type.deinit();
            var enum_names_it = self.struct_field_enum_type_names_by_type.iterator();
            while (enum_names_it.next()) |entry| {
                self.allocator.free(entry.value_ptr.*);
            }
            self.struct_field_enum_type_names_by_type.deinit();

            var desc_it = self.struct_desc_globals_by_type.iterator();
            while (desc_it.next()) |entry| {
                self.allocator.free(entry.value_ptr.*);
            }
            self.struct_desc_globals_by_type.deinit();

            var enum_desc_it = self.enum_desc_globals_by_type.iterator();
            while (enum_desc_it.next()) |entry| {
                self.allocator.free(entry.value_ptr.*);
            }
            self.enum_desc_globals_by_type.deinit();

            var enum_it = self.enum_print_map.iterator();
            while (enum_it.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
            }
            self.enum_print_map.deinit();
        }

        pub fn mapBuiltinToRuntime(name: []const u8) []const u8 {
            if (std.mem.eql(u8, name, "random")) return "doxa_random";
            if (std.mem.eql(u8, name, "int")) return "doxa_int";
            if (std.mem.eql(u8, name, "tick")) return "doxa_tick";
            if (std.mem.eql(u8, name, "string")) return "doxa_string";
            if (std.mem.eql(u8, name, "dice_roll")) return "doxa_dice_roll";
            if (std.mem.eql(u8, name, "find")) return "doxa_find";
            if (std.mem.eql(u8, name, "clear")) return "doxa_clear";
            if (std.mem.eql(u8, name, "input")) return "doxa_input";
            return name;
        }

        pub fn mangleGlobalName(self: *IRPrinter, name: []const u8) ![]const u8 {
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
    };
}
