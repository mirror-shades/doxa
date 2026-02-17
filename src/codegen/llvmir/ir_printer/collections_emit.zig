const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const HIRInstruction = Ctx.HIRInstruction;
    const CompareInstruction = Ctx.CompareInstruction;
    const StackVal = Ctx.StackVal;

    return struct {
    pub fn emitArrayNew(
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

        // Mirror the VM semantics: ArrayNew pushes a single array value onto the stack.
        // Callers that need to preserve the value (e.g. for declarations) should use
        // explicit Dup instructions, just like in the bytecode backend.
        const arr_val = StackVal{
            .name = reg,
            .ty = .PTR,
            .array_type = inst.element_type,
        };
        try stack.append(arr_val);
    }

    pub fn emitMap(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        inst: std.meta.TagPayload(HIRInstruction, .Map),
    ) !void {
        const entry_count: usize = inst.entries.len;
        const else_inputs: usize = if (inst.has_else_value) @as(usize, 1) else 0;
        const expected_inputs: usize = entry_count * 2 + else_inputs;
        if (stack.items.len < expected_inputs) return;

        const key_tag = self.arrayElementTag(inst.key_type);
        const val_tag = self.arrayElementTag(inst.value_type);

        var else_storage: ?StackVal = null;

        if (entry_count == 0) {
            if (inst.has_else_value) {
                const else_val = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                else_storage = try self.convertValueToArrayStorage(w, else_val, inst.value_type, id);
            }

            // Empty map – still create a header so subsequent MapGet/MapSet work.
            const reg = try self.nextTemp(id);
            const line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call ptr @doxa_map_new(i64 0, i64 {d}, i64 {d})\n",
                .{ reg, key_tag, val_tag },
            );
            defer self.allocator.free(line);
            try w.writeAll(line);

            if (else_storage) |else_val| {
                const set_else_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  call void @doxa_map_set_else_i64(ptr {s}, i64 {s})\n",
                    .{ reg, else_val.name },
                );
                defer self.allocator.free(set_else_line);
                try w.writeAll(set_else_line);
            }

            try stack.append(.{ .name = reg, .ty = .PTR, .array_type = inst.value_type });
            return;
        }

        const map_reg = try self.nextTemp(id);
        const new_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call ptr @doxa_map_new(i64 {d}, i64 {d}, i64 {d})\n",
            .{ map_reg, entry_count, key_tag, val_tag },
        );
        defer self.allocator.free(new_line);
        try w.writeAll(new_line);

        var remaining = entry_count;
        while (remaining > 0) : (remaining -= 1) {
            if (stack.items.len < 2) break;
            const value = stack.items[stack.items.len - 1];
            const key = stack.items[stack.items.len - 2];
            stack.items.len -= 2;

            const key_storage = try self.convertValueToArrayStorage(w, key, inst.key_type, id);
            const val_storage = try self.convertValueToArrayStorage(w, value, inst.value_type, id);

            const set_line = try std.fmt.allocPrint(
                self.allocator,
                "  call void @doxa_map_set_i64(ptr {s}, i64 {s}, i64 {s})\n",
                .{ map_reg, key_storage.name, val_storage.name },
            );
            defer self.allocator.free(set_line);
            try w.writeAll(set_line);
        }

        if (inst.has_else_value) {
            const else_val = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            else_storage = try self.convertValueToArrayStorage(w, else_val, inst.value_type, id);

            if (else_storage) |else_bits| {
                const set_else_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  call void @doxa_map_set_else_i64(ptr {s}, i64 {s})\n",
                    .{ map_reg, else_bits.name },
                );
                defer self.allocator.free(set_else_line);
                try w.writeAll(set_else_line);
            }
        }

        try stack.append(.{ .name = map_reg, .ty = .PTR, .array_type = inst.value_type });
    }

    pub fn emitMapGet(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        inst: std.meta.TagPayload(HIRInstruction, .MapGet),
    ) !void {
        if (stack.items.len < 2) return;
        const key_val = stack.items[stack.items.len - 1];
        var map_val = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        if (map_val.ty != .PTR) {
            map_val = try self.ensurePointer(w, map_val, id);
        }

        const key_storage = try self.convertValueToArrayStorage(w, key_val, inst.key_type, id);

        // Retrieve the value and whether the key existed
        const val_ptr = try self.nextTemp(id);
        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{val_ptr});
        defer self.allocator.free(alloca_line);
        try w.writeAll(alloca_line);

        const init_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{val_ptr});
        defer self.allocator.free(init_line);
        try w.writeAll(init_line);

        const found_i8 = try self.nextTemp(id);
        const try_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call i8 @doxa_map_try_get_i64(ptr {s}, i64 {s}, ptr {s})\n",
            .{ found_i8, map_val.name, key_storage.name, val_ptr },
        );
        defer self.allocator.free(try_line);
        try w.writeAll(try_line);

        const found = try self.nextTemp(id);
        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i1\n", .{ found, found_i8 });
        defer self.allocator.free(trunc_line);
        try w.writeAll(trunc_line);

        const res_reg = try self.nextTemp(id);
        const load_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = load i64, ptr {s}\n",
            .{ res_reg, val_ptr },
        );
        defer self.allocator.free(load_line);
        try w.writeAll(load_line);

        const storage_val = StackVal{ .name = res_reg, .ty = .I64 };

        // Determine the logical value type for this lookup
        var inferred_value_type: HIR.HIRType = inst.value_type;
        if (inferred_value_type == .Unknown) {
            inferred_value_type = map_val.array_type orelse HIR.HIRType.Int;
        }

        var has_nothing_branch = false;
        var concrete_value_type: HIR.HIRType = inferred_value_type;
        if (inferred_value_type == .Union) {
            const members = inferred_value_type.Union.members;
            for (members) |member_ptr| {
                const m = member_ptr.*;
                if (m == .Nothing) {
                    has_nothing_branch = true;
                    continue;
                }
                concrete_value_type = m;
            }
        }

        if (has_nothing_branch) {
            const actual_val = try self.convertArrayStorageToValue(w, storage_val, concrete_value_type, id);
            const dv_present = try self.buildDoxaValue(w, actual_val, inferred_value_type, id);

            const dv_absent = try self.buildDoxaValue(
                w,
                StackVal{ .name = "0", .ty = .Nothing },
                inferred_value_type,
                id,
            );

            const tag_present = try self.nextTemp(id);
            const tag_present_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 0\n", .{ tag_present, dv_present.name });
            defer self.allocator.free(tag_present_line);
            try w.writeAll(tag_present_line);

            const tag_absent = try self.nextTemp(id);
            const tag_absent_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 0\n", .{ tag_absent, dv_absent.name });
            defer self.allocator.free(tag_absent_line);
            try w.writeAll(tag_absent_line);

            const tag_sel = try self.nextTemp(id);
            const tag_sel_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = select i1 {s}, i32 {s}, i32 {s}\n",
                .{ tag_sel, found, tag_present, tag_absent },
            );
            defer self.allocator.free(tag_sel_line);
            try w.writeAll(tag_sel_line);

            const res_present = try self.nextTemp(id);
            const res_present_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 1\n", .{ res_present, dv_present.name });
            defer self.allocator.free(res_present_line);
            try w.writeAll(res_present_line);

            const res_absent = try self.nextTemp(id);
            const res_absent_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 1\n", .{ res_absent, dv_absent.name });
            defer self.allocator.free(res_absent_line);
            try w.writeAll(res_absent_line);

            const res_sel = try self.nextTemp(id);
            const res_sel_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = select i1 {s}, i32 {s}, i32 {s}\n",
                .{ res_sel, found, res_present, res_absent },
            );
            defer self.allocator.free(res_sel_line);
            try w.writeAll(res_sel_line);

            const payload_present = try self.nextTemp(id);
            const payload_present_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload_present, dv_present.name });
            defer self.allocator.free(payload_present_line);
            try w.writeAll(payload_present_line);

            const payload_absent = try self.nextTemp(id);
            const payload_absent_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload_absent, dv_absent.name });
            defer self.allocator.free(payload_absent_line);
            try w.writeAll(payload_absent_line);

            const payload_sel = try self.nextTemp(id);
            const payload_sel_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = select i1 {s}, i64 {s}, i64 {s}\n",
                .{ payload_sel, found, payload_present, payload_absent },
            );
            defer self.allocator.free(payload_sel_line);
            try w.writeAll(payload_sel_line);

            const dv0 = try self.nextTemp(id);
            const dv0_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = insertvalue %DoxaValue undef, i32 {s}, 0\n",
                .{ dv0, tag_sel },
            );
            defer self.allocator.free(dv0_line);
            try w.writeAll(dv0_line);

            const dv1 = try self.nextTemp(id);
            const dv1_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = insertvalue %DoxaValue {s}, i32 {s}, 1\n",
                .{ dv1, dv0, res_sel },
            );
            defer self.allocator.free(dv1_line);
            try w.writeAll(dv1_line);

            const dv2 = try self.nextTemp(id);
            const dv2_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = insertvalue %DoxaValue {s}, i64 {s}, 2\n",
                .{ dv2, dv1, payload_sel },
            );
            defer self.allocator.free(dv2_line);
            try w.writeAll(dv2_line);

            try stack.append(.{ .name = dv2, .ty = .Value });
        } else {
            const value_type = map_val.array_type orelse concrete_value_type;
            const actual_val = try self.convertArrayStorageToValue(w, storage_val, value_type, id);
            try stack.append(actual_val);
        }
    }

    pub fn emitMapSet(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        inst: std.meta.TagPayload(HIRInstruction, .MapSet),
    ) !void {
        if (stack.items.len < 3) return;
        const value = stack.items[stack.items.len - 1];
        const key_val = stack.items[stack.items.len - 2];
        var map_val = stack.items[stack.items.len - 3];
        stack.items.len -= 3;

        if (map_val.ty != .PTR) {
            map_val = try self.ensurePointer(w, map_val, id);
        }

        // Infer key type from the map's tracked element type when possible;
        // fall back to string keys for safety.
        const key_type: HIR.HIRType = inst.key_type;
        const key_storage = try self.convertValueToArrayStorage(w, key_val, key_type, id);

        const value_type = map_val.array_type orelse HIR.HIRType.Int;
        const val_storage = try self.convertValueToArrayStorage(w, value, value_type, id);

        const set_line = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_map_set_i64(ptr {s}, i64 {s}, i64 {s})\n",
            .{ map_val.name, key_storage.name, val_storage.name },
        );
        defer self.allocator.free(set_line);
        try w.writeAll(set_line);

        // Leave the (updated) map on the stack for further use.
        try stack.append(.{ .name = map_val.name, .ty = .PTR, .array_type = map_val.array_type });
    }

    pub fn emitArraySet(
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

    pub fn emitArrayGet(
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

            // Convert byte to a stable 1-char heap string.
            const str_ptr = try self.nextTemp(id);
            const call_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call ptr @doxa_char_to_string(i8 {s})\n",
                .{ str_ptr, ch_val },
            );
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            try stack.append(.{ .name = str_ptr, .ty = .PTR });
        }
    }

    const ArrayCompoundOp = enum { Add, Sub, Mul, Div, Mod, Pow };

    pub fn emitArrayGetAndArith(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        op: ArrayCompoundOp,
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

        const current_reg = try self.nextTemp(id);
        const get_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call i64 @doxa_array_get_i64(ptr {s}, i64 {s})\n",
            .{ current_reg, arr_ptr.name, idx_i64.name },
        );
        defer self.allocator.free(get_line);
        try w.writeAll(get_line);

        const current_bits: StackVal = .{ .name = current_reg, .ty = .I64 };
        const value_bits = try self.convertValueToArrayStorage(w, value, element_type, id);

        var result_bits_reg: []const u8 = undefined;
        switch (element_type) {
            .Float => {
                const current_double = try self.nextTemp(id);
                const cur_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ current_double, current_bits.name });
                defer self.allocator.free(cur_line);
                try w.writeAll(cur_line);

                const value_double = try self.nextTemp(id);
                const val_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ value_double, value_bits.name });
                defer self.allocator.free(val_line);
                try w.writeAll(val_line);

                const op_double = try self.nextTemp(id);
                const op_line = switch (op) {
                    .Add => try std.fmt.allocPrint(self.allocator, "  {s} = fadd double {s}, {s}\n", .{ op_double, current_double, value_double }),
                    .Sub => try std.fmt.allocPrint(self.allocator, "  {s} = fsub double {s}, {s}\n", .{ op_double, current_double, value_double }),
                    .Mul => try std.fmt.allocPrint(self.allocator, "  {s} = fmul double {s}, {s}\n", .{ op_double, current_double, value_double }),
                    .Div => try std.fmt.allocPrint(self.allocator, "  {s} = fdiv double {s}, {s}\n", .{ op_double, current_double, value_double }),
                    .Mod => try std.fmt.allocPrint(self.allocator, "  {s} = frem double {s}, {s}\n", .{ op_double, current_double, value_double }),
                    .Pow => try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ op_double, current_double, value_double }),
                };
                defer self.allocator.free(op_line);
                try w.writeAll(op_line);

                result_bits_reg = try self.nextTemp(id);
                const back_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ result_bits_reg, op_double });
                defer self.allocator.free(back_line);
                try w.writeAll(back_line);
            },
            .Byte => {
                const tmp = try self.nextTemp(id);
                const arith_line = switch (op) {
                    .Add => try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ tmp, current_bits.name, value_bits.name }),
                    .Sub => try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ tmp, current_bits.name, value_bits.name }),
                    .Mul => try std.fmt.allocPrint(self.allocator, "  {s} = mul i64 {s}, {s}\n", .{ tmp, current_bits.name, value_bits.name }),
                    .Div => try std.fmt.allocPrint(self.allocator, "  {s} = sdiv i64 {s}, {s}\n", .{ tmp, current_bits.name, value_bits.name }),
                    .Mod => try std.fmt.allocPrint(self.allocator, "  {s} = srem i64 {s}, {s}\n", .{ tmp, current_bits.name, value_bits.name }),
                    .Pow => blk: {
                        const lhs_double = try self.nextTemp(id);
                        const rhs_double = try self.nextTemp(id);
                        const lhs_line = try std.fmt.allocPrint(self.allocator, "  {s} = uitofp i64 {s} to double\n", .{ lhs_double, current_bits.name });
                        const rhs_line = try std.fmt.allocPrint(self.allocator, "  {s} = uitofp i64 {s} to double\n", .{ rhs_double, value_bits.name });
                        defer self.allocator.free(lhs_line);
                        defer self.allocator.free(rhs_line);
                        try w.writeAll(lhs_line);
                        try w.writeAll(rhs_line);
                        const pow_double = try self.nextTemp(id);
                        const pow_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ pow_double, lhs_double, rhs_double });
                        defer self.allocator.free(pow_line);
                        try w.writeAll(pow_line);
                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = fptoui double {s} to i64\n", .{ tmp, pow_double });
                        break :blk conv_line;
                    },
                };
                defer self.allocator.free(arith_line);
                try w.writeAll(arith_line);

                result_bits_reg = try self.nextTemp(id);
                const mask_line = try std.fmt.allocPrint(self.allocator, "  {s} = and i64 {s}, 255\n", .{ result_bits_reg, tmp });
                defer self.allocator.free(mask_line);
                try w.writeAll(mask_line);
            },
            else => {
                const arith_line = switch (op) {
                    .Add => blk: {
                        result_bits_reg = try self.nextTemp(id);
                        break :blk try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ result_bits_reg, current_bits.name, value_bits.name });
                    },
                    .Sub => blk: {
                        result_bits_reg = try self.nextTemp(id);
                        break :blk try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ result_bits_reg, current_bits.name, value_bits.name });
                    },
                    .Mul => blk: {
                        result_bits_reg = try self.nextTemp(id);
                        break :blk try std.fmt.allocPrint(self.allocator, "  {s} = mul i64 {s}, {s}\n", .{ result_bits_reg, current_bits.name, value_bits.name });
                    },
                    .Div => blk: {
                        result_bits_reg = try self.nextTemp(id);
                        break :blk try std.fmt.allocPrint(self.allocator, "  {s} = sdiv i64 {s}, {s}\n", .{ result_bits_reg, current_bits.name, value_bits.name });
                    },
                    .Mod => blk: {
                        result_bits_reg = try self.nextTemp(id);
                        break :blk try std.fmt.allocPrint(self.allocator, "  {s} = srem i64 {s}, {s}\n", .{ result_bits_reg, current_bits.name, value_bits.name });
                    },
                    .Pow => blk: {
                        const lhs_double = try self.nextTemp(id);
                        const rhs_double = try self.nextTemp(id);
                        const lhs_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ lhs_double, current_bits.name });
                        const rhs_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ rhs_double, value_bits.name });
                        defer self.allocator.free(lhs_line);
                        defer self.allocator.free(rhs_line);
                        try w.writeAll(lhs_line);
                        try w.writeAll(rhs_line);
                        const pow_double = try self.nextTemp(id);
                        const pow_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ pow_double, lhs_double, rhs_double });
                        defer self.allocator.free(pow_line);
                        try w.writeAll(pow_line);
                        result_bits_reg = try self.nextTemp(id);
                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = fptosi double {s} to i64\n", .{ result_bits_reg, pow_double });
                        break :blk conv_line;
                    },
                };
                defer self.allocator.free(arith_line);
                try w.writeAll(arith_line);
            },
        }

        const set_line = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})\n",
            .{ arr_ptr.name, idx_i64.name, result_bits_reg },
        );
        defer self.allocator.free(set_line);
        try w.writeAll(set_line);

        const result_bits: StackVal = .{ .name = result_bits_reg, .ty = .I64 };
        const result_value = try self.convertArrayStorageToValue(w, result_bits, element_type, id);
        try stack.append(result_value);
    }

    pub fn emitArrayPush(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 2) return;
        const value = stack.items[stack.items.len - 1];
        const hdr_val = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        const len_info = try self.loadArrayLength(w, hdr_val, id);
        var element_type = len_info.array.array_type orelse HIR.HIRType{ .Int = {} };
        if (element_type == .Nothing and value.ty != .Nothing) {
            element_type = switch (value.ty) {
                .I8 => .Byte,
                .F64 => .Float,
                .I2, .I1 => .Tetra,
                .PTR => value.array_type orelse .String,
                else => .Int,
            };
        }
        const stored_val = try self.convertValueToArrayStorage(w, value, element_type, id);

        const set_line = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_array_set_i64(ptr {s}, i64 {s}, i64 {s})\n",
            .{ len_info.array.name, len_info.len_value.name, stored_val.name },
        );
        defer self.allocator.free(set_line);
        try w.writeAll(set_line);

        try stack.append(.{ .name = len_info.array.name, .ty = .PTR, .array_type = element_type });
    }

    pub fn emitArrayLen(
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

    pub fn emitArrayPop(
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

    pub fn emitArrayInsert(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 3) return;
        const value = stack.items[stack.items.len - 1];
        const idx_val = stack.items[stack.items.len - 2];
        const target = stack.items[stack.items.len - 3];
        stack.items.len -= 3;

        const idx_i64 = if (idx_val.ty == .I64) idx_val else try self.ensureI64(w, idx_val, id);
        if (target.array_type) |elem_type_in| {
            var elem_type = elem_type_in;
            if (elem_type == .Nothing and value.ty != .Nothing) {
                elem_type = switch (value.ty) {
                    .I8 => .Byte,
                    .F64 => .Float,
                    .I2, .I1 => .Tetra,
                    .PTR => value.array_type orelse .String,
                    else => .Int,
                };
            }
            const hdr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
            const stored_val = try self.convertValueToArrayStorage(w, value, elem_type, id);
            const out = try self.nextTemp(id);
            const line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_insert(ptr {s}, i64 {s}, i64 {s})\n", .{ out, hdr.name, idx_i64.name, stored_val.name });
            defer self.allocator.free(line);
            try w.writeAll(line);
            try stack.append(.{ .name = out, .ty = .PTR, .array_type = elem_type });
            return;
        }

        const s_ptr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
        const ins_ptr = if (value.ty == .PTR) value else try self.ensurePointer(w, value, id);
        const out = try self.nextTemp(id);
        const line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_str_insert(ptr {s}, i64 {s}, ptr {s})\n", .{ out, s_ptr.name, idx_i64.name, ins_ptr.name });
        defer self.allocator.free(line);
        try w.writeAll(line);
        try stack.append(.{ .name = out, .ty = .PTR });
    }

    pub fn emitArrayRemove(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 2) return;
        const idx_val = stack.items[stack.items.len - 1];
        const target = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        const idx_i64 = if (idx_val.ty == .I64) idx_val else try self.ensureI64(w, idx_val, id);
        if (target.array_type) |elem_type| {
            const hdr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
            const removed_slot = try self.nextTemp(id);
            const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{removed_slot});
            defer self.allocator.free(alloca_line);
            try w.writeAll(alloca_line);
            const init_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{removed_slot});
            defer self.allocator.free(init_line);
            try w.writeAll(init_line);
            const out = try self.nextTemp(id);
            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_remove(ptr {s}, i64 {s}, ptr {s})\n", .{ out, hdr.name, idx_i64.name, removed_slot });
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            const removed = try self.nextTemp(id);
            const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ removed, removed_slot });
            defer self.allocator.free(load_line);
            try w.writeAll(load_line);
            const removed_val = try self.convertArrayStorageToValue(w, .{ .name = removed, .ty = .I64 }, elem_type, id);
            // Contract: [updated, removed]
            try stack.append(.{ .name = out, .ty = .PTR, .array_type = elem_type });
            try stack.append(removed_val);
            return;
        }

        const s_ptr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
        const rem_slot = try self.nextTemp(id);
        const popped_slot = try self.nextTemp(id);
        const rem_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{rem_slot});
        defer self.allocator.free(rem_alloca);
        try w.writeAll(rem_alloca);
        const pop_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{popped_slot});
        defer self.allocator.free(pop_alloca);
        try w.writeAll(pop_alloca);
        const rem_init = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{rem_slot});
        defer self.allocator.free(rem_init);
        try w.writeAll(rem_init);
        const pop_init = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{popped_slot});
        defer self.allocator.free(pop_init);
        try w.writeAll(pop_init);
        const ok = try self.nextTemp(id);
        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i8 @doxa_str_remove(ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ ok, s_ptr.name, idx_i64.name, rem_slot, popped_slot });
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);
        const rem_name = try self.nextTemp(id);
        const pop_name = try self.nextTemp(id);
        const rem_load = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ rem_name, rem_slot });
        defer self.allocator.free(rem_load);
        try w.writeAll(rem_load);
        const pop_load = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ pop_name, popped_slot });
        defer self.allocator.free(pop_load);
        try w.writeAll(pop_load);
        try stack.append(.{ .name = rem_name, .ty = .PTR });
        try stack.append(.{ .name = pop_name, .ty = .PTR });
    }

    pub fn emitArraySlice(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        if (stack.items.len < 3) return;
        const len_val = stack.items[stack.items.len - 1];
        const start_val = stack.items[stack.items.len - 2];
        const target = stack.items[stack.items.len - 3];
        stack.items.len -= 3;

        const start_i64 = if (start_val.ty == .I64) start_val else try self.ensureI64(w, start_val, id);
        const len_i64 = if (len_val.ty == .I64) len_val else try self.ensureI64(w, len_val, id);

        if (target.array_type) |elem_type| {
            const hdr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
            const out = try self.nextTemp(id);
            const line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_slice(ptr {s}, i64 {s}, i64 {s})\n", .{ out, hdr.name, start_i64.name, len_i64.name });
            defer self.allocator.free(line);
            try w.writeAll(line);
            try stack.append(.{ .name = out, .ty = .PTR, .array_type = elem_type });
            return;
        }

        const s_ptr = if (target.ty == .PTR) target else try self.ensurePointer(w, target, id);
        const out = try self.nextTemp(id);
        const line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_substring(ptr {s}, i64 {s}, i64 {s})\n", .{ out, s_ptr.name, start_i64.name, len_i64.name });
        defer self.allocator.free(line);
        try w.writeAll(line);
        try stack.append(.{ .name = out, .ty = .PTR });
    }

    pub fn emitArrayConcat(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
    ) !void {
        // Debug: emit comment showing stack state
        const debug_line = try std.fmt.allocPrint(self.allocator, "  ; ArrayConcat called with {d} items on stack\n", .{stack.items.len});
        defer self.allocator.free(debug_line);
        try w.writeAll(debug_line);

        if (stack.items.len < 2) {
            // If we don't have 2 items, try to generate a call anyway with dummy values
            // This shouldn't happen in correct code, but let's be robust
            const dummy_reg = try self.nextTemp(id);
            const call_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call ptr @doxa_array_concat(ptr null, ptr null, i64 8, i64 0)\n",
                .{dummy_reg},
            );
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            try stack.append(.{ .name = dummy_reg, .ty = .PTR });
            return;
        }
        var rhs = stack.items[stack.items.len - 1];
        var lhs = stack.items[stack.items.len - 2];
        stack.items.len -= 2;

        if (lhs.ty != .PTR) {
            lhs = try self.ensurePointer(w, lhs, id);
        }
        if (rhs.ty != .PTR) {
            rhs = try self.ensurePointer(w, rhs, id);
        }

        const elem_type = lhs.array_type orelse rhs.array_type orelse HIR.HIRType{ .Int = {} };
        const elem_size = self.arrayElementSize(elem_type);
        const elem_tag = self.arrayElementTag(elem_type);

        const concat_reg = try self.nextTemp(id);
        const call_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = call ptr @doxa_array_concat(ptr {s}, ptr {s}, i64 {d}, i64 {d})\n",
            .{ concat_reg, lhs.name, rhs.name, elem_size, elem_tag },
        );
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);

        try stack.append(.{ .name = concat_reg, .ty = .PTR, .array_type = elem_type });
    }

    pub fn emitRange(
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

    pub fn ensureBool(
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
            .Value => {
                // For now, treat Value as truthy if its payload is non-zero when
                // viewed as i64. This path is not yet used, but keeps the switch
                // exhaustive as we start introducing Value-typed stack entries.
                const as_i64 = try self.ensureI64(w, value, id);
                const name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 {s}, 0\n", .{ name, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = name, .ty = .I1 };
            },
        }
    }

    pub fn emitCompareInstruction(
        self: *IRPrinter,
        w: anytype,
        cmp: CompareInstruction,
        lhs: StackVal,
        rhs: StackVal,
        id: *usize,
    ) !StackVal {
        // Guard against corrupted operand names causing huge allocations
        if (lhs.name.len > IRPrinter.MAX_SANE_NAME_LEN or rhs.name.len > IRPrinter.MAX_SANE_NAME_LEN) {
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
                    const pred = switch (cmp.op) {
                        .Eq => "eq",
                        .Ne => "ne",
                        .Lt => "slt",
                        .Le => "sle",
                        .Gt => "sgt",
                        .Ge => "sge",
                    };
                    const lhs_i64 = if (lhs.ty != .I64) try self.ensureI64(w, lhs, id) else lhs;
                    const rhs_i64 = if (rhs.ty != .I64) try self.ensureI64(w, rhs, id) else rhs;
                    result_name = try self.nextTemp(id);
                    break :blk try std.fmt.allocPrint(self.allocator, "  {s} = icmp {s} i64 {s}, {s}\n", .{ result_name, pred, lhs_i64.name, rhs_i64.name });
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

        // For tetra comparisons, convert i1 result to i2 (tetra)
        if (operand_type == .Tetra) {
            const tetra_result_name = try self.nextTemp(id);
            const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ tetra_result_name, result_name });
            defer self.allocator.free(zext_line);
            try w.writeAll(zext_line);
            return .{ .name = tetra_result_name, .ty = .I2 };
        }

        return .{ .name = result_name, .ty = .I1 };
    }

    };
}
