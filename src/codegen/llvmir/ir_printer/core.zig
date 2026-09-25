const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const StackMergeState = Ctx.StackMergeState;
    const EnumVariantMeta = Ctx.EnumVariantMeta;
    const Region = Ctx.Region;

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

        pub fn paramTypeMatchesStack(self: *IRPrinter, param_type: HIR.HIRType, stack_type: StackType) bool {
            return self.hirTypeToStackType(param_type) == stack_type;
        }

        pub fn coerceForMerge(
            self: *IRPrinter,
            incoming: StackVal,
            target: StackType,
            id: *usize,
            w: anytype,
        ) !StackVal {
            if (incoming.ty == target) return incoming;
            if (incoming.ty == .Nothing) {
                if (target == .PTR) {
                    return .{ .name = "null", .ty = .PTR, .array_type = incoming.array_type };
                }
                return .{ .name = "undef", .ty = target, .array_type = incoming.array_type };
            }

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
                    .STRING => blk: {
                        const as_ptr = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        const payload_len = try self.nextTemp(id);
                        const len_extract = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 3\n", .{ payload_len, incoming.name });
                        defer self.allocator.free(len_extract);
                        try w.writeAll(len_extract);
                        const tmp_ds = try self.nextTemp(id);
                        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_ds, as_ptr });
                        defer self.allocator.free(ins0);
                        try w.writeAll(ins0);
                        const str_name = try self.nextTemp(id);
                        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ str_name, tmp_ds, payload_len });
                        defer self.allocator.free(ins1);
                        try w.writeAll(ins1);
                        break :blk .{ .name = str_name, .ty = .STRING };
                    },
                    else => incoming,
                };
            }

            if (target == .Value and incoming.ty != .Value) {
                return self.buildDoxaValue(w, incoming, null, id);
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
                    .STRING => {
                        const ptr_ext = try self.nextTemp(id);
                        const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, incoming.name });
                        defer self.allocator.free(ext_line);
                        try w.writeAll(ext_line);
                        const as_i64 = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ as_i64, ptr_ext });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = as_i64, .ty = .I64 };
                    },
                    else => {},
                },
                .F64 => switch (incoming.ty) {
                    .I64 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
                    },
                    .I8 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i8 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
                    },
                    .I2 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i2 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
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
                .STRING => {
                    if (incoming.ty == .STRING) return incoming;
                    const ptr = try self.ensurePointer(w, incoming, id);
                    return self.ensureString(w, ptr, id);
                },
                else => {},
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
            if (incoming.ty == .Nothing) {
                if (target == .PTR) {
                    return .{ .name = "null", .ty = .PTR, .array_type = incoming.array_type };
                }
                return incoming;
            }
            if (target == .Nothing) return incoming;

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
                    .STRING => {
                        const ptr_ext = try self.nextTemp(id);
                        const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, incoming.name });
                        defer self.allocator.free(ext_line);
                        try w.writeAll(ext_line);
                        const as_i64 = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ as_i64, ptr_ext });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = as_i64, .ty = .I64 };
                    },
                    else => {},
                },
                .F64 => switch (incoming.ty) {
                    .I64 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
                    },
                    .I8 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i8 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
                    },
                    .I2 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i2 {s} to double\n", .{ widened, incoming.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        return .{ .name = widened, .ty = .F64 };
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
                .STRING => {
                    if (incoming.ty == .STRING) return incoming;
                    const ptr = try self.ensurePointer(w, incoming, id);
                    return self.ensureString(w, ptr, id);
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
                    new_slots[i] = .empty;
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

                    // A `Nothing` arm is zero-sized and cannot participate in a
                    // phi node. If every arm is Nothing the merge is Nothing too.
                    // Otherwise (e.g. a diverging `@panic` fallback arm that never
                    // actually reaches this merge) keep the real value and feed
                    // `undef` for the Nothing arms so the phi still carries an
                    // entry for each predecessor.
                    var all_nothing = true;
                    for (slot.items) |incoming_val| {
                        if (incoming_val.value.ty != .Nothing) {
                            all_nothing = false;
                            break;
                        }
                    }
                    if (all_nothing) {
                        try stack.append(slot.items[0].value);
                        continue;
                    }

                    var needs_i2_conversion = false;
                    var target_type: StackType = .Nothing;
                    for (slot.items) |incoming_val| {
                        if (incoming_val.value.ty == .Nothing) continue;
                        target_type = incoming_val.value.ty;
                        break;
                    }
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
                        const blk_name: []const u8 = if (std.mem.startsWith(u8, incoming_val.block, "func_")) "entry" else incoming_val.block;
                        if (incoming_val.value.ty == .Nothing) {
                            const pair = try std.fmt.allocPrint(self.allocator, "[ undef, %{s} ]", .{blk_name});
                            try incoming.append(pair);
                            continue;
                        }
                        var adjusted = incoming_val.value;
                        if (adjusted.ty != target_type) {
                            adjusted = try self.coerceForMerge(adjusted, target_type, id, w);
                        }
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

                    // A phi's provenance is definite only when every incoming arm
                    // agrees. All-outliving arms (`Root`/`Func`) collapse to `Func`
                    // (outlives the function body but is not provably root); any
                    // `Deep`/`Unknown` arm makes the merge conservative.
                    var merged_region: Region = slot.items[0].value.region;
                    for (slot.items) |incoming_val| {
                        const r = incoming_val.value.region;
                        if (r != merged_region) {
                            merged_region = switch (merged_region) {
                                .Root, .Func => switch (r) {
                                    .Root, .Func => .Func,
                                    else => .Unknown,
                                },
                                else => .Unknown,
                            };
                        }
                    }

                    try stack.append(.{
                        .name = phi_name,
                        .ty = target_type,
                        .region = merged_region,
                        .array_type = merged_array_type,
                        .enum_type_name = merged_enum_type_name,
                        .struct_field_types = merged_struct_field_types,
                        .struct_field_names = merged_struct_field_names,
                        .struct_type_name = merged_struct_type_name,
                    });
                }
            }
        }

        pub fn init(io: std.Io, allocator: std.mem.Allocator, group_table: ?*anyopaque, enum_table: ?*anyopaque, zig_fn_param_types: std.StringHashMap([]HIR.HIRType), reflected_structs: ?*const std.StringHashMap(void), force_struct_descriptors: bool) IRPrinter {
            return .{
                .allocator = allocator,
                .io = io,
                .zig_fn_param_types = zig_fn_param_types,
                .peek_string_counter = 0,
                .global_types = std.StringHashMap(StackType).init(allocator),
                .global_array_types = std.StringHashMap(HIR.HIRType).init(allocator),
                .global_enum_types = std.StringHashMap([]const u8).init(allocator),
                .global_struct_field_types = std.StringHashMap([]HIR.HIRType).init(allocator),
                .global_struct_field_names = std.StringHashMap([]const []const u8).init(allocator),
                .global_struct_type_names = std.StringHashMap([]const u8).init(allocator),
                .global_fixed_array_info = std.StringHashMap(IRPrinter.GlobalFixedArrayInfo).init(allocator),
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
                .group_table = group_table,
                .enum_table = enum_table,
                .entry_str_out_ptr = null,
                .entry_str_out_len = null,
                .entry_allocas = std.array_list.Managed([]const u8).init(allocator),
                .exited_scopes = std.AutoHashMap(u32, void).init(allocator),
                .narrowed_vars = std.StringHashMap(std.ArrayListUnmanaged(HIR.HIRType)).init(allocator),
                .var_regions = std.StringHashMap(Region).init(allocator),
                .reflected_structs = reflected_structs,
                .force_struct_descriptors = force_struct_descriptors,
                .skip_descriptor_structs = std.StringHashMap(void).init(allocator),
            };
        }

        pub fn deinit(self: *IRPrinter) void {
            self.peek_string_counter = 0;
            self.zig_fn_param_types.deinit();
            self.global_types.deinit();
            self.global_array_types.deinit();
            self.global_enum_types.deinit();
            self.global_struct_field_types.deinit();
            self.global_struct_field_names.deinit();
            self.global_struct_type_names.deinit();
            self.global_fixed_array_info.deinit();
            self.struct_fields_by_id.deinit();
            self.struct_type_names_by_id.deinit();
            self.defined_globals.deinit();
            self.exited_scopes.deinit();
            var narrowed_it = self.narrowed_vars.iterator();
            while (narrowed_it.next()) |entry| {
                entry.value_ptr.deinit(self.allocator);
            }
            self.narrowed_vars.deinit();
            self.var_regions.deinit();
            self.skip_descriptor_structs.deinit();
            for (self.entry_allocas.items) |line| self.allocator.free(line);
            self.entry_allocas.deinit();
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

        /// Region class of the arena a fresh heap value is allocated into at the
        /// current point of the function. The function-body scope (and any scope
        /// outside it) is the only arena that outlives every local store; values
        /// produced inside a reusable loop scope die on the next iteration reset.
        ///
        /// The top-level program pass (module global initialization and
        /// `doxa_program_main`) runs with no function frame, entirely inside the
        /// never-exited root arena, so everything it allocates is `Root`.
        pub fn currentRegionTag(self: *IRPrinter) Region {
            if (!self.in_function_context) return .Root;
            return if (self.scope_depth <= 1) .Func else .Deep;
        }

        /// A3: the runtime scope stack levels from the current arena up to the
        /// caller's arena (the arena active at the call site). A returned value
        /// placed here outlives the callee body; `cloneHeapValue` derives the same
        /// number for its `.caller` destination, and this is that arithmetic
        /// hoisted out so a construction site can allocate straight into it.
        pub fn callerLevels(self: *IRPrinter) usize {
            return (self.scope_depth -| @as(usize, @intFromBool(self.in_function_context))) + 1;
        }

        /// Heap types whose store path is a runtime *rehome* (identity preserved
        /// when the source already outlives the destination). Unions always
        /// deep-clone and maps are stored by pointer, so neither is a rehome
        /// site and neither may take the static plain-store shortcut.
        pub fn rehomeTypeEligible(t: HIR.HIRType) bool {
            return switch (t) {
                .String, .Array, .Struct => true,
                else => false,
            };
        }

        /// Heap types whose `.rehome` store is decided statically *even when the
        /// region analysis is inconclusive*. Strings are immutable, so cloning an
        /// unknown string is observably identical to preserving its identity; the
        /// runtime registry walk buys nothing and the whole string rehome path
        /// can be deleted. Arrays and structs keep the runtime rehome for
        /// `Unknown` — cloning there would disconnect in-place element/field
        /// mutation from the owning object, so it must be decided per site.
        pub fn rehomeUnknownToClone(t: HIR.HIRType) bool {
            return t == .String;
        }

        /// A1 static rehome decision: when the value's arena provably outlives
        /// the store destination (the function-body scope), the runtime rehome
        /// call would keep identity and copy nothing, so it can be skipped. This
        /// is only ever consulted on rehome-eligible types; anything the region
        /// analysis could not classify keeps the runtime call unchanged.
        pub fn plainStoreProven(self: *IRPrinter, value: StackVal, declared_type: HIR.HIRType) bool {
            _ = self;
            if (!rehomeTypeEligible(declared_type)) return false;
            return switch (value.region) {
                .Root, .Func => true,
                .Deep, .Caller, .Unknown => false,
            };
        }

        /// Record the region class of a local variable's payload after a store.
        /// The emitter walks the instruction stream linearly, so this is not
        /// path-sensitive: once a variable may hold a `Deep` (loop-arena) object
        /// on *some* path, loads of it must stay conservative (`Deep` wins the
        /// join) or a plain store could alias an object the loop reset is about
        /// to free. `Func`/`Root` only join upward — from `Unknown`/absent to
        /// the new class.
        pub fn recordVarRegion(self: *IRPrinter, var_name: []const u8, region: Region) !void {
            const merged = if (self.var_regions.get(var_name)) |cur|
                if (cur == .Deep) .Deep else region
            else
                region;
            try self.var_regions.put(var_name, merged);
        }

        /// A1 static decision for a store into a *global* (destination is the
        /// program-root arena). Only a value already resident in the root arena
        /// (`Root` — another global's payload or a module instance) provably
        /// outlives it; a function-local or loop-arena object must still be
        /// cloned up by the runtime rehome.
        pub fn plainGlobalStoreProven(self: *IRPrinter, value: StackVal, declared_type: HIR.HIRType) bool {
            _ = self;
            if (!rehomeTypeEligible(declared_type)) return false;
            return value.region == .Root;
        }

        /// A2: the local `.rehome` store decision, made statically. A local
        /// store's destination is always the function-body arena, so the runtime
        /// "does the source already outlive it?" walk is replaced by the region
        /// class:
        ///   - `Root`/`Func` → plain store (identity preserved, no copy);
        ///   - `Deep`/`Unknown` → explicit unconditional clone for types whose
        ///     unknown case is safe to clone (`rehomeUnknownToClone`, i.e.
        ///     strings). A `Deep` source is born in a reusable loop arena that
        ///     dies at the next iteration reset, so it never outlives the
        ///     function body — the runtime rehome would clone it, so emit that
        ///     clone directly and skip the registry walk.
        ///   - otherwise `Unknown` → the runtime rehome call, unchanged. Arrays
        ///     and structs must preserve identity here, so the registry decides.
        pub fn rehomeForLocalStore(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            if (self.plainStoreProven(value, declared_type)) return value;
            if (value.region == .Deep or rehomeUnknownToClone(declared_type))
                return self.cloneHeapValue(w, id, value, declared_type, .persistent, true);
            return self.cloneHeapForStore(w, id, value, declared_type);
        }

        /// A2: the global `.rehome` store decision, made statically. The root
        /// arena is the outermost, so nothing but a root object outlives it:
        ///   - `Root` → plain store (identity preserved);
        ///   - `Func`/`Deep`, or `Unknown` of a clone-safe type → unconditional
        ///     clone into the root arena. The runtime rehome would clone too
        ///     (none of these outlives root), so emit the clone directly and skip
        ///     the registry walk;
        ///   - otherwise `Unknown` → the runtime rehome call, unchanged.
        pub fn rehomeForGlobalStore(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            if (self.plainGlobalStoreProven(value, declared_type)) return value;
            if (value.region == .Func or value.region == .Deep or rehomeUnknownToClone(declared_type))
                return self.cloneHeapValue(w, id, value, declared_type, .program_root, true);
            return self.cloneHeapForGlobalStore(w, id, value, declared_type);
        }

        /// Where a heap clone is allocated.
        ///
        /// `scope_depth` is local to the function (or top-level script) being
        /// emitted: it does not count the program-root `doxa_scope_enter()` or
        /// any caller frames. Walking `scope_depth` levels therefore cannot
        /// reach the root from a nested callee — globals need `program_root`.
        const HeapCloneDest = enum {
            /// Function body (or current scope at top level). Survives inner blocks.
            persistent,
            /// One scope above the current function (the caller).
            caller,
            /// Program-root arena, never exited. Globals live here.
            program_root,
        };

        /// Deep-copy a heap value into the scope its variable is declared in,
        /// so it survives the exit of the current (possibly nested) scope. The
        /// destination is the function scope when inside a function, else the
        /// program root scope. Structs keep identity when they already live in
        /// that destination (or an ancestor of it). Arrays are always copied on
        /// assignment. Scalars are returned unchanged.
        pub fn cloneHeapForStore(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            return self.cloneHeapValue(w, id, value, declared_type, .persistent, false);
        }

        /// Always clone into the function's persistent scope. Used for by-value
        /// parameters so the callee cannot mutate the caller's heap object.
        pub fn cloneHeapForSnapshot(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            return self.cloneHeapValue(w, id, value, declared_type, .persistent, true);
        }

        /// Deep-copy a return value into the caller's scope (one level above the
        /// current function scope) so it survives the function scope being freed.
        pub fn cloneHeapForReturn(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            return self.cloneHeapValue(w, id, value, declared_type, .caller, true);
        }

        /// Deep-copy a heap value into the program-root arena. Used when storing
        /// into a global: cloning into the current function would leave the
        /// global dangling after that function's `doxa_scope_exit()`.
        pub fn cloneHeapForGlobalStore(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType) !StackVal {
            return self.cloneHeapValue(w, id, value, declared_type, .program_root, false);
        }

        pub fn cloneHeapValue(self: *IRPrinter, w: anytype, id: *usize, value: StackVal, declared_type: HIR.HIRType, dest: HeapCloneDest, snapshot: bool) !StackVal {
            const levels_up: usize = (self.scope_depth -| @as(usize, @intFromBool(self.in_function_context))) + @intFromBool(dest == .caller);

            switch (declared_type) {
                .String => {
                    if (value.ty != .STRING) return value;
                    const s_ptr = try self.nextTemp(id);
                    const s_len = try self.nextTemp(id);
                    const ext0 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, value.name });
                    defer self.allocator.free(ext0);
                    try w.writeAll(ext0);
                    const ext1 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, value.name });
                    defer self.allocator.free(ext1);
                    try w.writeAll(ext1);

                    const out_ptr_slot = try self.nextTemp(id);
                    const out_len_slot = try self.nextTemp(id);
                    const ap = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
                    defer self.allocator.free(ap);
                    try w.writeAll(ap);
                    const al = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
                    defer self.allocator.free(al);
                    try w.writeAll(al);
                    const ip = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
                    defer self.allocator.free(ip);
                    try w.writeAll(ip);
                    const il = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
                    defer self.allocator.free(il);
                    try w.writeAll(il);

                    // Strings are always cloned here, never rehomed: they are
                    // immutable, so identity is unobservable and A2 decides every
                    // string store statically (`rehomeUnknownToClone`). The only
                    // remaining `snapshot=false` callers are array/struct.
                    const call_line = if (dest == .program_root)
                        try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_clone_root(ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ s_ptr, s_len, out_ptr_slot, out_len_slot })
                    else
                        try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_clone_at(i64 {d}, ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ levels_up, s_ptr, s_len, out_ptr_slot, out_len_slot });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);

                    const loaded_ptr = try self.nextTemp(id);
                    const loaded_len = try self.nextTemp(id);
                    const lp = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ loaded_ptr, out_ptr_slot });
                    defer self.allocator.free(lp);
                    try w.writeAll(lp);
                    const ll = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ loaded_len, out_len_slot });
                    defer self.allocator.free(ll);
                    try w.writeAll(ll);

                    const tmp_name = try self.nextTemp(id);
                    const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, loaded_ptr });
                    defer self.allocator.free(ins0);
                    try w.writeAll(ins0);
                    const str_name = try self.nextTemp(id);
                    const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ str_name, tmp_name, loaded_len });
                    defer self.allocator.free(ins1);
                    try w.writeAll(ins1);

                    return .{ .name = str_name, .ty = .STRING, .array_type = value.array_type, .enum_type_name = value.enum_type_name, .struct_field_types = value.struct_field_types, .struct_field_names = value.struct_field_names, .struct_type_name = value.struct_type_name };
                },
                .Array => {
                    if (value.fixed_array_depth != 0) return value;
                    const src_ptr = if (value.ty == .PTR) value else try self.ensurePointer(w, value, id);
                    const clone_reg = try self.nextTemp(id);
                    const clone_line = if (dest == .program_root)
                        try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_{s}_root(ptr {s})\n", .{ clone_reg, if (snapshot) "clone" else "rehome", src_ptr.name })
                    else
                        try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_{s}_at(i64 {d}, ptr {s})\n", .{ clone_reg, if (snapshot) "clone" else "rehome", levels_up, src_ptr.name });
                    defer self.allocator.free(clone_line);
                    try w.writeAll(clone_line);
                    return .{ .name = clone_reg, .ty = .PTR, .array_type = value.array_type, .fixed_array_depth = value.fixed_array_depth, .fixed_array_sizes = value.fixed_array_sizes };
                },
                .Struct => {
                    const src_ptr = if (value.ty == .PTR) value else try self.ensurePointer(w, value, id);
                    const clone_reg = try self.nextTemp(id);

                    // B2/B3: a scalar-only struct that never needs the descriptor
                    // is cloned with a typed word copy — no registry lookup, so it
                    // is consistent with skipping its registration at construction.
                    if (value.struct_type_name) |name| {
                        if (self.skip_descriptor_structs.contains(name)) {
                            const field_types = value.struct_field_types orelse self.global_struct_field_types.get(name);
                            const words: usize = if (field_types) |fts| fts.len else 0;
                            if (words > 0) {
                                const scalar_line = if (dest == .program_root)
                                    try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_struct_clone_scalar_root(i64 {d}, ptr {s})\n", .{ clone_reg, words, src_ptr.name })
                                else
                                    try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_struct_clone_scalar_at(i64 {d}, i64 {d}, ptr {s})\n", .{ clone_reg, levels_up, words, src_ptr.name });
                                defer self.allocator.free(scalar_line);
                                try w.writeAll(scalar_line);
                                return .{ .name = clone_reg, .ty = .PTR, .struct_type_name = value.struct_type_name, .struct_field_types = value.struct_field_types, .struct_field_names = value.struct_field_names };
                            }
                        }
                    }

                    const clone_line = if (dest == .program_root)
                        try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @{s}(ptr {s})\n", .{ clone_reg, if (snapshot) "doxa_struct_clone_root" else "doxa_struct_rehome_root", src_ptr.name })
                    else
                        try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @{s}(i64 {d}, ptr {s})\n", .{ clone_reg, if (snapshot) "doxa_struct_clone_at" else "doxa_struct_rehome_at", levels_up, src_ptr.name });
                    defer self.allocator.free(clone_line);
                    try w.writeAll(clone_line);
                    return .{ .name = clone_reg, .ty = .PTR, .struct_type_name = value.struct_type_name, .struct_field_types = value.struct_field_types, .struct_field_names = value.struct_field_names };
                },
                .Union => {
                    if (value.ty != .Value) return value;
                    const slot = try self.nextTemp(id);
                    const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{slot});
                    defer self.allocator.free(alloca_line);
                    try w.writeAll(alloca_line);
                    const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ value.name, slot });
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    const call_line = if (dest == .program_root)
                        try std.fmt.allocPrint(self.allocator, "  call void @doxa_clone_doxa_value_root(ptr {s})\n", .{slot})
                    else
                        try std.fmt.allocPrint(self.allocator, "  call void @doxa_clone_doxa_value_at(i64 {d}, ptr {s})\n", .{ levels_up, slot });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                    const loaded = try self.nextTemp(id);
                    const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load %DoxaValue, ptr {s}\n", .{ loaded, slot });
                    defer self.allocator.free(load_line);
                    try w.writeAll(load_line);
                    return .{ .name = loaded, .ty = .Value };
                },
                else => return value,
            }
        }

        pub fn mapBuiltinToRuntime(name: []const u8) []const u8 {
            if (std.mem.eql(u8, name, "int")) return "doxa_int";
            if (std.mem.eql(u8, name, "clear")) return "doxa_clear";
            if (std.mem.eql(u8, name, "print")) return "doxa_write_cstr";
            if (std.mem.eql(u8, name, "exit")) return "doxa_exit";
            if (std.mem.eql(u8, name, "panic")) return "doxa_panic";
            return name;
        }

        /// Emitted LLVM symbol for a user-defined function. The generated Zig root
        /// owns the `main` symbol and the runtime owns every `doxa_*` export, so a
        /// non-entry function whose name would collide with either (e.g. a plain
        /// `function main()`) is renamed into the reserved namespace. The entry
        /// function is always renamed so `doxa_program_main` can call it without
        /// shadowing the root's `main`. Caller owns the returned slice.
        pub fn functionSymbol(self: *IRPrinter, func: HIR.HIRProgram.HIRFunction) ![]const u8 {
            const name = func.qualified_name;
            if (func.is_entry) {
                if (std.mem.eql(u8, name, "main")) return self.allocator.dupe(u8, "doxa_user_main");
                return std.fmt.allocPrint(self.allocator, "doxa_entry_{s}", .{name});
            }
            if (std.mem.eql(u8, name, "main") or std.mem.startsWith(u8, name, "doxa_")) {
                return std.fmt.allocPrint(self.allocator, "doxa_fn_{s}", .{name});
            }
            return self.allocator.dupe(u8, name);
        }

        pub fn mangleGlobalName(self: *IRPrinter, name: []const u8) ![]const u8 {
            return std.fmt.allocPrint(self.allocator, "@.glob.{s}", .{name});
        }

        pub fn emitToFile(self: *IRPrinter, hir: *const HIR.HIRProgram, path: []const u8) !void {
            const file = try std.Io.Dir.cwd().createFile(self.io, path, .{});
            defer file.close(self.io);
            var buffer: [4096]u8 = undefined;
            var file_writer = file.writer(self.io, &buffer);
            const w = &file_writer.interface;
            try self.writeModule(hir, w);
            try w.flush();
            self.deinit();
        }
    };
}
