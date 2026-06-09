const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const HIRInstruction = Ctx.HIRInstruction;
    const HIRValue = Ctx.HIRValue;
    const PeekEmitState = Ctx.PeekEmitState;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const internPeekString = Ctx.internPeekString;

    return struct {
        pub fn handleConst(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, peek_state: *PeekEmitState, constant_pool: []const HIRValue, constant_id: u32) !void {
            _ = peek_state;
            const hv = constant_pool[constant_id];
            switch (hv) {
                .int => |i| {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, i });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = name, .ty = .I64 });
                },
                .float => |f| {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const literal = try self.formatFloatLiteral(f);
                    defer self.allocator.free(literal);
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double 0.0, {s}\n", .{ name, literal });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = name, .ty = .F64 });
                },
                .byte => |b| {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i8 0, {d}\n", .{ name, b });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = name, .ty = .I8 });
                },
                .tetra => |t| {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i2 0, {d}\n", .{ name, t });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = name, .ty = .I2 });
                },
                .string => |s| {
                    if (s.len == 0) {
                        // Empty strings: GEP into the sentinel null byte so indexing
                        // (e.g. `""[0]`) doesn't dereference null.
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [1 x i8], ptr @.doxa.empty, i64 0, i64 0\n", .{ptr_name});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        const tmp_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{tmp_name, ptr_name});
                        defer self.allocator.free(ins0);
                        try w.writeAll(ins0);
                        const str_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 0, 1\n", .{ str_name, tmp_name });
                        defer self.allocator.free(ins1);
                        try w.writeAll(ins1);
                        try stack.append(.{ .name = str_name, .ty = .STRING, .string_literal_value = s });
                    } else {
                        const str_idx = self.string_pool_len + constant_id;
                        const global_name = try std.fmt.allocPrint(self.allocator, "@.str.{d}", .{str_idx});
                        defer self.allocator.free(global_name);

                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ ptr_name, s.len, global_name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);

                        const tmp_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, ptr_name });
                        defer self.allocator.free(ins0);
                        try w.writeAll(ins0);

                        const str_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {d}, 1\n", .{ str_name, tmp_name, s.len });
                        defer self.allocator.free(ins1);
                        try w.writeAll(ins1);

                        try stack.append(.{ .name = str_name, .ty = .STRING, .string_literal_value = s });
                    }
                },
                .enum_variant => |ev| {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, @as(i64, @intCast(ev.variant_index)) });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    const type_name_global = try self.createEnumTypeNameGlobal(ev.type_name, id);
                    try stack.append(.{ .name = name, .ty = .I64, .enum_type_name = type_name_global });
                    self.last_emitted_enum_value = ev.variant_index;
                },
                .nothing => {
                    try stack.append(.{ .name = "0", .ty = .Nothing });
                },
                // Remaining HIRValue variants (struct_instance, array_instance, etc.)
                // are not valid constant literals and are intentionally skipped.
                else => {},
            }
        }

        pub fn handleDup(_: *IRPrinter, stack: *std.array_list.Managed(StackVal)) !void {
            if (stack.items.len < 1) return;
            const top = stack.items[stack.items.len - 1];
            const duped: StackVal = .{
                .name = top.name,
                .ty = top.ty,
                .array_type = top.array_type,
                .enum_type_name = top.enum_type_name,
                .struct_field_types = top.struct_field_types,
                .struct_field_names = top.struct_field_names,
                .struct_type_name = top.struct_type_name,
                .string_literal_value = top.string_literal_value,
            };
            try stack.append(duped);
        }

        pub fn handlePop(_: *IRPrinter, stack: *std.array_list.Managed(StackVal)) void {
            if (stack.items.len < 1) return;
            stack.items.len -= 1;
        }

        pub fn handleSwap(_: *IRPrinter, stack: *std.array_list.Managed(StackVal)) void {
            if (stack.items.len < 2) return;
            const top_idx = stack.items.len - 1;
            std.mem.swap(StackVal, &stack.items[top_idx], &stack.items[top_idx - 1]);
        }

        pub fn handleArith(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, a: std.meta.TagPayload(HIRInstruction, .Arith)) !void {
            if (stack.items.len < 2) return;
            var rhs = stack.items[stack.items.len - 1];
            var lhs = stack.items[stack.items.len - 2];
            stack.items.len -= 2;
            switch (a.operand_type) {
                .Int => {
                    lhs = try self.ensureI64(w, lhs, id);
                    rhs = try self.ensureI64(w, rhs, id);
                    if (a.op == .Pow) {
                        const lhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const lhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ lhs_double, lhs.name });
                        defer self.allocator.free(lhs_conv_line);
                        try w.writeAll(lhs_conv_line);
                        const rhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const rhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ rhs_double, rhs.name });
                        defer self.allocator.free(rhs_conv_line);
                        try w.writeAll(rhs_conv_line);
                        const pow_result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const pow_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ pow_result, lhs_double, rhs_double });
                        defer self.allocator.free(pow_line);
                        try w.writeAll(pow_line);
                        const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const conv_back_line = try std.fmt.allocPrint(self.allocator, "  {s} = fptosi double {s} to i64\n", .{ name, pow_result });
                        defer self.allocator.free(conv_back_line);
                        try w.writeAll(conv_back_line);
                        try stack.append(.{ .name = name, .ty = .I64 });
                    } else {
                        const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        var pushed_name: ?[]const u8 = null;
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
                            .IntDiv => {
                                const q = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const r = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const r_nz = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const xor_v = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const sign_diff = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const adj_i1 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const adj = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const result_reg = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const l0 = try std.fmt.allocPrint(self.allocator, "  {s} = sdiv i64 {s}, {s}\n", .{ q, lhs.name, rhs.name });
                                const l1 = try std.fmt.allocPrint(self.allocator, "  {s} = srem i64 {s}, {s}\n", .{ r, lhs.name, rhs.name });
                                const l2 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 {s}, 0\n", .{ r_nz, r });
                                const l3 = try std.fmt.allocPrint(self.allocator, "  {s} = xor i64 {s}, {s}\n", .{ xor_v, lhs.name, rhs.name });
                                const l4 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp slt i64 {s}, 0\n", .{ sign_diff, xor_v });
                                const l5 = try std.fmt.allocPrint(self.allocator, "  {s} = and i1 {s}, {s}\n", .{ adj_i1, r_nz, sign_diff });
                                const l6 = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i64\n", .{ adj, adj_i1 });
                                const l7 = try std.fmt.allocPrint(self.allocator, "  {s} = sub i64 {s}, {s}\n", .{ result_reg, q, adj });
                                defer self.allocator.free(l7);
                                defer self.allocator.free(l6);
                                defer self.allocator.free(l5);
                                defer self.allocator.free(l4);
                                defer self.allocator.free(l3);
                                defer self.allocator.free(l2);
                                defer self.allocator.free(l1);
                                defer self.allocator.free(l0);
                                try w.writeAll(l0);
                                try w.writeAll(l1);
                                try w.writeAll(l2);
                                try w.writeAll(l3);
                                try w.writeAll(l4);
                                try w.writeAll(l5);
                                try w.writeAll(l6);
                                try w.writeAll(l7);
                                pushed_name = result_reg;
                            },
                            .Mod => {
                                const r = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const r_nz = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const xor_v = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const sign_diff = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const adj_i1 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const corr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const result_reg = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const l0 = try std.fmt.allocPrint(self.allocator, "  {s} = srem i64 {s}, {s}\n", .{ r, lhs.name, rhs.name });
                                const l1 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp ne i64 {s}, 0\n", .{ r_nz, r });
                                const l2 = try std.fmt.allocPrint(self.allocator, "  {s} = xor i64 {s}, {s}\n", .{ xor_v, lhs.name, rhs.name });
                                const l3 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp slt i64 {s}, 0\n", .{ sign_diff, xor_v });
                                const l4 = try std.fmt.allocPrint(self.allocator, "  {s} = and i1 {s}, {s}\n", .{ adj_i1, r_nz, sign_diff });
                                const l5 = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, i64 {s}, i64 0\n", .{ corr, adj_i1, rhs.name });
                                const l6 = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 {s}, {s}\n", .{ result_reg, r, corr });
                                defer self.allocator.free(l6);
                                defer self.allocator.free(l5);
                                defer self.allocator.free(l4);
                                defer self.allocator.free(l3);
                                defer self.allocator.free(l2);
                                defer self.allocator.free(l1);
                                defer self.allocator.free(l0);
                                try w.writeAll(l0);
                                try w.writeAll(l1);
                                try w.writeAll(l2);
                                try w.writeAll(l3);
                                try w.writeAll(l4);
                                try w.writeAll(l5);
                                try w.writeAll(l6);
                                pushed_name = result_reg;
                            },
                            else => unreachable,
                        }
                        if (pushed_name) |pn| {
                            try stack.append(.{ .name = pn, .ty = .I64 });
                        } else {
                            try stack.append(.{ .name = name, .ty = .I64 });
                        }
                    }
                },
                .Float => {
                    const lhs_double = if (lhs.ty == .F64) blk: {
                        break :blk lhs.name;
                    } else blk: {
                        const lhs_i64 = try self.ensureI64(w, lhs, id);
                        const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, lhs_i64.name });
                        defer self.allocator.free(conv_line);
                        try w.writeAll(conv_line);
                        break :blk conv_name;
                    };
                    const rhs_double = if (rhs.ty == .F64) blk: {
                        break :blk rhs.name;
                    } else blk: {
                        const rhs_i64 = try self.ensureI64(w, rhs, id);
                        const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, rhs_i64.name });
                        defer self.allocator.free(conv_line);
                        try w.writeAll(conv_line);
                        break :blk conv_name;
                    };
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    switch (a.op) {
                        .Add => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fadd double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Sub => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fsub double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Mul => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fmul double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Div => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = fdiv double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .IntDiv => {
                            unreachable;
                        },
                        .Mod => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = frem double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                        .Pow => {
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ name, lhs_double, rhs_double });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                        },
                    }
                    try stack.append(.{ .name = name, .ty = .F64 });
                },
                .Byte => {
                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
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
                        .IntDiv => {
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
                // Only Int, Float, and Byte are valid arithmetic operand types.
                else => {},
            }
        }

        pub fn handleCompare(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, cmp: std.meta.TagPayload(HIRInstruction, .Compare)) !void {
            if (stack.items.len < 2) return;
            const rhs = stack.items[stack.items.len - 1];
            const lhs = stack.items[stack.items.len - 2];
            stack.items.len -= 2;
            const result = try self.emitCompareInstruction(w, cmp, lhs, rhs, id);
            try stack.append(result);
        }

        pub fn handleLogicalOp(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, lop: std.meta.TagPayload(HIRInstruction, .LogicalOp)) !void {
            if (lop.op == .Not) {
                if (stack.items.len < 1) return;
                const v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                if (v.ty == .I2) {
                    const idx_i64 = try self.nextTemp(id);
                    const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ idx_i64, v.name });
                    defer self.allocator.free(zext_line);
                    try w.writeAll(zext_line);

                    const lut_ptr = try self.nextTemp(id);
                    const gep_line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr @tetra_not_lut, i64 0, i64 {s}\n", .{ lut_ptr, idx_i64 });
                    defer self.allocator.free(gep_line);
                    try w.writeAll(gep_line);

                    const result_i8 = try self.nextTemp(id);
                    const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, lut_ptr });
                    defer self.allocator.free(load_line);
                    try w.writeAll(load_line);

                    const result_i2 = try self.nextTemp(id);
                    const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                    defer self.allocator.free(trunc_line);
                    try w.writeAll(trunc_line);

                    try stack.append(.{ .name = result_i2, .ty = .I2 });
                } else {
                    const bool_val = try self.ensureBool(w, v, id);
                    const not_result = try self.nextTemp(id);
                    const not_line = try std.fmt.allocPrint(self.allocator, "  {s} = xor i1 {s}, true\n", .{ not_result, bool_val.name });
                    defer self.allocator.free(not_line);
                    try w.writeAll(not_line);
                    try stack.append(.{ .name = not_result, .ty = .I1 });
                }
            } else {
                if (stack.items.len < 2) return;
                const rhs = stack.items[stack.items.len - 1];
                const lhs = stack.items[stack.items.len - 2];
                stack.items.len -= 2;

                // Check if operands are tetra (i2) or boolean (i1)
                if (lhs.ty == .I2 and rhs.ty == .I2) {
                    // Tetra operations - use LUTs
                    // Convert both operands to i64 indices
                    const lhs_i64 = try self.nextTemp(id);
                    const lhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ lhs_i64, lhs.name });
                    defer self.allocator.free(lhs_zext);
                    try w.writeAll(lhs_zext);

                    const rhs_i64 = try self.nextTemp(id);
                    const rhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ rhs_i64, rhs.name });
                    defer self.allocator.free(rhs_zext);
                    try w.writeAll(rhs_zext);

                    // Select the appropriate LUT
                    const lut_name = switch (lop.op) {
                        .And => "@tetra_and_lut",
                        .Or => "@tetra_or_lut",
                        .Iff => "@tetra_iff_lut",
                        .Xor => "@tetra_xor_lut",
                        .Nand => "@tetra_nand_lut",
                        .Nor => "@tetra_nor_lut",
                        .Implies => "@tetra_implies_lut",
                        else => unreachable,
                    };

                    // Get pointer to the row: LUT[lhs]
                    const row_ptr = try self.nextTemp(id);
                    const row_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x [4 x i8]], ptr {s}, i64 0, i64 {s}\n", .{ row_ptr, lut_name, lhs_i64 });
                    defer self.allocator.free(row_gep);
                    try w.writeAll(row_gep);

                    // Get the element: row[rhs]
                    const elem_ptr = try self.nextTemp(id);
                    const elem_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr {s}, i64 0, i64 {s}\n", .{ elem_ptr, row_ptr, rhs_i64 });
                    defer self.allocator.free(elem_gep);
                    try w.writeAll(elem_gep);

                    // Load the result
                    const result_i8 = try self.nextTemp(id);
                    const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, elem_ptr });
                    defer self.allocator.free(load_line);
                    try w.writeAll(load_line);

                    // Convert back to i2
                    const result_i2 = try self.nextTemp(id);
                    const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                    defer self.allocator.free(trunc_line);
                    try w.writeAll(trunc_line);

                    try stack.append(.{ .name = result_i2, .ty = .I2 });
                } else {
                    // Boolean operations
                    const lhs_bool = try self.ensureBool(w, lhs, id);
                    const rhs_bool = try self.ensureBool(w, rhs, id);

                    if (lop.op == .And or lop.op == .Or or lop.op == .Xor) {
                        const result = try self.nextTemp(id);
                        const op_str = switch (lop.op) {
                            .And => "and",
                            .Or => "or",
                            .Xor => "xor",
                            else => unreachable,
                        };
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = {s} i1 {s}, {s}\n", .{ result, op_str, lhs_bool.name, rhs_bool.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = result, .ty = .I1 });
                    } else {
                        // Complex boolean ops (Nand, Nor, Iff, Implies):
                        // convert to i2 and use tetra LUTs
                        const lhs_i2 = try self.nextTemp(id);
                        const lhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ lhs_i2, lhs_bool.name });
                        defer self.allocator.free(lhs_zext);
                        try w.writeAll(lhs_zext);

                        const rhs_i2 = try self.nextTemp(id);
                        const rhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ rhs_i2, rhs_bool.name });
                        defer self.allocator.free(rhs_zext);
                        try w.writeAll(rhs_zext);

                        const lhs_i64 = try self.nextTemp(id);
                        const lhs_zext_i64 = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ lhs_i64, lhs_i2 });
                        defer self.allocator.free(lhs_zext_i64);
                        try w.writeAll(lhs_zext_i64);

                        const rhs_i64 = try self.nextTemp(id);
                        const rhs_zext_i64 = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ rhs_i64, rhs_i2 });
                        defer self.allocator.free(rhs_zext_i64);
                        try w.writeAll(rhs_zext_i64);

                        const lut_name = switch (lop.op) {
                            .Nand => "@tetra_nand_lut",
                            .Nor => "@tetra_nor_lut",
                            .Iff => "@tetra_iff_lut",
                            .Implies => "@tetra_implies_lut",
                            else => unreachable,
                        };

                        const row_ptr = try self.nextTemp(id);
                        const row_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x [4 x i8]], ptr {s}, i64 0, i64 {s}\n", .{ row_ptr, lut_name, lhs_i64 });
                        defer self.allocator.free(row_gep);
                        try w.writeAll(row_gep);

                        const elem_ptr = try self.nextTemp(id);
                        const elem_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr {s}, i64 0, i64 {s}\n", .{ elem_ptr, row_ptr, rhs_i64 });
                        defer self.allocator.free(elem_gep);
                        try w.writeAll(elem_gep);

                        const result_i8 = try self.nextTemp(id);
                        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, elem_ptr });
                        defer self.allocator.free(load_line);
                        try w.writeAll(load_line);

                        const result_i2 = try self.nextTemp(id);
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);

                        try stack.append(.{ .name = result_i2, .ty = .I2 });
                    }
                }
            }
        }

        pub fn handleConvert(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, conv: std.meta.TagPayload(HIRInstruction, .Convert)) !void {
            if (stack.items.len < 1) return;
            const arg = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            switch (conv.to_type) {
                .Byte => {
                    const as_i64 = switch (arg.ty) {
                        .I64 => arg,
                        .F64 => blk: {
                            const tmp = try self.nextTemp(id);
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_byte_from_f64(double {s})\n", .{ tmp, arg.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            break :blk StackVal{ .name = tmp, .ty = .I64 };
                        },
                        .PTR => blk: {
                            const tmp = try self.nextTemp(id);
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_byte_from_cstr(ptr {s})\n", .{ tmp, arg.name });
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            break :blk StackVal{ .name = tmp, .ty = .I64 };
                        },
                        else => try self.ensureI64(w, arg, id),
                    };
                    const out = try self.nextTemp(id);
                    const trunc = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ out, as_i64.name });
                    defer self.allocator.free(trunc);
                    try w.writeAll(trunc);
                    try stack.append(.{ .name = out, .ty = .I8 });
                },
                else => {
                    try stack.append(arg);
                },
            }
        }

        pub fn handleTypeCheck(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, tc: std.meta.TagPayload(HIRInstruction, .TypeCheck), peek_state: *PeekEmitState) !void {
            if (stack.items.len < 1) return;
            const value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            // Compute tag and payload bits; handle canonical %DoxaValue values specially.
            var value_i64 = StackVal{ .name = "", .ty = .I64 };
            var tag_reg: []const u8 = undefined;
            if (value.ty == .Value) {
                const tag_i32 = try self.nextTemp(id);
                const tag_extract = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = extractvalue %DoxaValue {s}, 0\n",
                    .{ tag_i32, value.name },
                );
                defer self.allocator.free(tag_extract);
                try w.writeAll(tag_extract);

                const tag_i64 = try self.nextTemp(id);
                const tag_zext = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = zext i32 {s} to i64\n",
                    .{ tag_i64, tag_i32 },
                );
                defer self.allocator.free(tag_zext);
                try w.writeAll(tag_zext);
                tag_reg = tag_i64;

                const payload = try self.nextTemp(id);
                const payload_extract = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = extractvalue %DoxaValue {s}, 2\n",
                    .{ payload, value.name },
                );
                defer self.allocator.free(payload_extract);
                try w.writeAll(payload_extract);
                value_i64 = .{ .name = payload, .ty = .I64 };
            } else {
                const value_type_tag: i64 = switch (value.ty) {
                    .I64 => if (value.enum_type_name != null) 6 else 0, // enum or int
                    .F64 => 1,
                    .I8 => 2,
                    .PTR => if (value.array_type != null) 4 else if (value.struct_field_types != null) 5 else 3, // array, struct, or string
                    .STRING => 3,
                    .I2 => 7, // tetra - not used in type checking
                    .I1 => 7, // bool - not used in type checking
                    .Nothing => 8,
                    .Value => 8,
                };

                const tag_tmp = try self.nextTemp(id);
                const tag_line2 = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = add i64 0, {d}\n",
                    .{ tag_tmp, value_type_tag },
                );
                defer self.allocator.free(tag_line2);
                try w.writeAll(tag_line2);
                tag_reg = tag_tmp;

                // Convert value to i64 payload bits for DoxaValue.
                value_i64 = try self.ensureI64(w, value, id);
            }

            // Create target type string constant
            const target_type_info = try internPeekString(
                self.allocator,
                &peek_state.*.string_map,
                &peek_state.*.strings,
                peek_state.*.next_id_ptr,
                &peek_state.*.globals,
                tc.target_type,
            );
            const target_type_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            const target_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                .{ target_type_ptr, target_type_info.length, target_type_info.name },
            );
            defer self.allocator.free(target_gep);
            try w.writeAll(target_gep);

            // Call the legacy ABI helper that takes value bits + type tag +
            // target type string. This avoids platform-specific struct
            // passing issues with %DoxaValue on MSVC ABIs.
            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            const call_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = call i64 @doxa_type_check(i64 {s}, i64 {s}, ptr {s})\n",
                .{ result_name, value_i64.name, tag_reg, target_type_ptr },
            );
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);

            // Convert i64 result to i2 (tetra) for boolean result
            const tetra_result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ tetra_result, result_name });
            defer self.allocator.free(trunc_line);
            try w.writeAll(trunc_line);

            try stack.append(.{ .name = tetra_result, .ty = .I2 });
        }

        pub fn handleStringOp(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sop: std.meta.TagPayload(HIRInstruction, .StringOp), peek_state: *PeekEmitState) !void {
            if (sop.op == .Concat) {
                if (stack.items.len < 2) return;
                const a = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const b = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                const a_val = try self.ensureString(w, a, id);
                const b_val = try self.ensureString(w, b, id);

                const a_ptr = try self.nextTemp(id);
                const a_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ a_ptr, a_val.name });
                defer self.allocator.free(a_ptr_line);
                try w.writeAll(a_ptr_line);
                const a_len = try self.nextTemp(id);
                const a_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ a_len, a_val.name });
                defer self.allocator.free(a_len_line);
                try w.writeAll(a_len_line);

                const b_ptr = try self.nextTemp(id);
                const b_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ b_ptr, b_val.name });
                defer self.allocator.free(b_ptr_line);
                try w.writeAll(b_ptr_line);
                const b_len = try self.nextTemp(id);
                const b_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ b_len, b_val.name });
                defer self.allocator.free(b_len_line);
                try w.writeAll(b_len_line);

                const args_line = try std.fmt.allocPrint(
                    self.allocator,
                    "ptr {s}, i64 {s}, ptr {s}, i64 {s}",
                    .{ a_ptr, a_len, b_ptr, b_len },
                );
                defer self.allocator.free(args_line);
                try self.emitRTCallReturningString(w, stack, id, "doxa_str_concat", args_line);
                return;
            }

            if (stack.items.len < 1) return;
            const arg = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            switch (sop.op) {
                .Length => {
                    if (arg.ty == .STRING) {
                        const ptr_ext = try self.nextTemp(id);
                        const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                        defer self.allocator.free(ext_line);
                        try w.writeAll(ext_line);
                        const len_ext = try self.nextTemp(id);
                        const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                        defer self.allocator.free(len_line);
                        try w.writeAll(len_line);
                        const result_name = try self.nextTemp(id);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_str_len(ptr {s}, i64 {s})\n", .{ result_name, ptr_ext, len_ext });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                        try stack.append(.{ .name = result_name, .ty = .I64 });
                    } else {
                        const fallback = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        try stack.append(.{ .name = fallback, .ty = .I64 });
                    }
                },
                .ToInt => {
                    switch (arg.ty) {
                        .STRING => {
                            const ptr_ext = try self.nextTemp(id);
                            const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                            defer self.allocator.free(ext_line);
                            try w.writeAll(ext_line);
                            const len_ext = try self.nextTemp(id);
                            const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                            defer self.allocator.free(len_line);
                            try w.writeAll(len_line);
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int_from_string(ptr {s}, i64 {s})\n", .{ result_name, ptr_ext, len_ext });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .I64 });
                        },
                        .F64 => {
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int(double {s})\n", .{ result_name, arg.name });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .I64 });
                        },
                        else => {
                            const arg_i64 = if (arg.ty == .I64) arg else try self.ensureI64(w, arg, id);
                            try stack.append(.{ .name = arg_i64.name, .ty = .I64 });
                        },
                    }
                },
                .ToFloat => {
                    switch (arg.ty) {
                        .F64 => try stack.append(arg),
                        .STRING => {
                            const ptr_ext = try self.nextTemp(id);
                            const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                            defer self.allocator.free(ext_line);
                            try w.writeAll(ext_line);
                            const len_ext = try self.nextTemp(id);
                            const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                            defer self.allocator.free(len_line);
                            try w.writeAll(len_line);
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @doxa_float_from_string(ptr {s}, i64 {s})\n", .{ result_name, ptr_ext, len_ext });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .F64 });
                        },
                        else => {
                            const arg_i64 = if (arg.ty == .I64) arg else try self.ensureI64(w, arg, id);
                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, arg_i64.name });
                            defer self.allocator.free(conv_line);
                            try w.writeAll(conv_line);
                            try stack.append(.{ .name = conv_name, .ty = .F64 });
                        },
                    }
                },
                .ToByte => {
                    if (arg.ty == .STRING) {
                        const ptr_ext = try self.nextTemp(id);
                        const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                        defer self.allocator.free(ext_line);
                        try w.writeAll(ext_line);
                        const len_ext = try self.nextTemp(id);
                        const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                        defer self.allocator.free(len_line);
                        try w.writeAll(len_line);
                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_byte_from_string(ptr {s}, i64 {s})\n", .{ result_name, ptr_ext, len_ext });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                        const as_i8 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                        id.* += 1;
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ as_i8, result_name });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);
                        try stack.append(.{ .name = as_i8, .ty = .I8 });
                    } else {
                        const zero = try self.nextTemp(id);
                        const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{zero});
                        defer self.allocator.free(zero_line);
                        try w.writeAll(zero_line);
                        try stack.append(.{ .name = zero, .ty = .I8 });
                    }
                },
                .ToString => {
                    switch (arg.ty) {
                        .STRING => {
                            try stack.append(arg);
                        },
                        .PTR => {
                            if (arg.array_type != null) {
                                const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}", .{arg.name});
                                defer self.allocator.free(args_line);
                                try self.emitRTCallReturningString(w, stack, id, "doxa_array_to_string", args_line);
                            } else if (arg.struct_field_types != null or arg.struct_type_name != null) {
                                const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}", .{arg.name});
                                defer self.allocator.free(args_line);
                                try self.emitRTCallReturningString(w, stack, id, "doxa_struct_to_string", args_line);
                            } else {
                                const tmp_name = try self.nextTemp(id);
                                const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, arg.name });
                                defer self.allocator.free(ins0);
                                try w.writeAll(ins0);
                                const str_name = try self.nextTemp(id);
                                const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 0, 1\n", .{ str_name, tmp_name });
                                defer self.allocator.free(ins1);
                                try w.writeAll(ins1);
                                try stack.append(.{ .name = str_name, .ty = .STRING });
                            }
                        },
                        .I64 => {
                            if (arg.enum_type_name) |enum_name| {
                                const info = try internPeekString(
                                    self.allocator,
                                    &peek_state.*.string_map,
                                    &peek_state.*.strings,
                                    peek_state.*.next_id_ptr,
                                    &peek_state.*.globals,
                                    enum_name,
                                );
                                const enum_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                                id.* += 1;
                                const gep = try std.fmt.allocPrint(
                                    self.allocator,
                                    "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                                    .{ enum_ptr, info.length, info.name },
                                );
                                defer self.allocator.free(gep);
                                try w.writeAll(gep);

                                const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}, i64 {d}, i64 {s}", .{ enum_ptr, enum_name.len, arg.name });
                                defer self.allocator.free(args_line);
                                try self.emitRTCallReturningString(w, stack, id, "doxa_enum_to_string", args_line);
                            } else {
                                const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{arg.name});
                                defer self.allocator.free(args_line);
                                try self.emitRTCallReturningString(w, stack, id, "doxa_int_to_string", args_line);
                            }
                        },
                        .F64 => {
                            const args_line = try std.fmt.allocPrint(self.allocator, "double {s}", .{arg.name});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_float_to_string", args_line);
                        },
                        .I8 => {
                            const widened = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ widened, arg.name });
                            defer self.allocator.free(zext_line);
                            try w.writeAll(zext_line);
                            const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{widened});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_byte_to_string", args_line);
                        },
                        .I1, .I2 => {
                            const src_ty = self.stackTypeToLLVMType(arg.ty);
                            const widened = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                            id.* += 1;
                            const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, arg.name });
                            defer self.allocator.free(zext_line);
                            try w.writeAll(zext_line);
                            const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{widened});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_tetra_to_string", args_line);
                        },
                        .Value => {
                            const payload = try self.nextTemp(id);
                            const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, arg.name });
                            defer self.allocator.free(extract_line);
                            try w.writeAll(extract_line);
                            const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{payload});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_int_to_string", args_line);
                        },
                        .Nothing => {
                            const args_line = try std.fmt.allocPrint(self.allocator, "", .{});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_nothing_to_string", args_line);
                        },
                    }
                },
                .Pack => {
                    const arr_ptr = if (arg.ty == .PTR) arg else try self.ensurePointer(w, arg, id);
                    const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}", .{arr_ptr.name});
                    defer self.allocator.free(args_line);
                    try self.emitRTCallReturningString(w, stack, id, "doxa_pack_bytes", args_line);
                },
                .Unpack => {
                    const str_val = try self.ensureString(w, arg, id);
                    const ptr_ext = try self.nextTemp(id);
                    const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, str_val.name });
                    defer self.allocator.free(ext_line);
                    try w.writeAll(ext_line);
                    const len_ext = try self.nextTemp(id);
                    const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, str_val.name });
                    defer self.allocator.free(len_line);
                    try w.writeAll(len_line);
                    const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_unpack_bytes(ptr {s}, i64 {s})\n", .{ result_name, ptr_ext, len_ext });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                    try stack.append(.{ .name = result_name, .ty = .PTR, .array_type = HIR.HIRType{ .Byte = {} } });
                },
                .Pop => {
                    const str_val = try self.ensureString(w, arg, id);
                    const s_ptr = try self.nextTemp(id);
                    const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, str_val.name });
                    defer self.allocator.free(s_ptr_line);
                    try w.writeAll(s_ptr_line);
                    const s_len = try self.nextTemp(id);
                    const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, str_val.name });
                    defer self.allocator.free(s_len_line);
                    try w.writeAll(s_len_line);
                    const rem_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const pop_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const rem_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaString\n", .{rem_slot});
                    defer self.allocator.free(rem_alloca);
                    try w.writeAll(rem_alloca);
                    const pop_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaString\n", .{pop_slot});
                    defer self.allocator.free(pop_alloca);
                    try w.writeAll(pop_alloca);

                    const ok_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i8 @doxa_str_pop(ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ ok_name, s_ptr, s_len, rem_slot, pop_slot });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);

                    const rem_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const pop_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const rem_load = try std.fmt.allocPrint(self.allocator, "  {s} = load %DoxaString, ptr {s}\n", .{ rem_name, rem_slot });
                    defer self.allocator.free(rem_load);
                    try w.writeAll(rem_load);
                    const pop_load = try std.fmt.allocPrint(self.allocator, "  {s} = load %DoxaString, ptr {s}\n", .{ pop_name, pop_slot });
                    defer self.allocator.free(pop_load);
                    try w.writeAll(pop_load);

                    try stack.append(.{ .name = rem_name, .ty = .STRING });
                    try stack.append(.{ .name = pop_name, .ty = .STRING });
                },
                .Substring => {
                    if (stack.items.len < 2) return;
                    const length = stack.items[stack.items.len - 1];
                    const start = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;
                    const str_val = try self.ensureString(w, arg, id);
                    const s_ptr = try self.nextTemp(id);
                    const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, str_val.name });
                    defer self.allocator.free(s_ptr_line);
                    try w.writeAll(s_ptr_line);
                    const s_len = try self.nextTemp(id);
                    const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, str_val.name });
                    defer self.allocator.free(s_len_line);
                    try w.writeAll(s_len_line);
                    const start_i64 = if (start.ty == .I64) start else try self.ensureI64(w, start, id);
                    const len_i64 = if (length.ty == .I64) length else try self.ensureI64(w, length, id);
                    const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}, i64 {s}, i64 {s}, i64 {s}", .{ s_ptr, s_len, start_i64.name, len_i64.name });
                    defer self.allocator.free(args_line);
                    try self.emitRTCallReturningString(w, stack, id, "doxa_substring", args_line);
                },
                else => {
                    const fallback = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{fallback});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = fallback, .ty = .I64 });
                },
            }
        }

        pub fn handlePeek(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, pk: std.meta.TagPayload(HIRInstruction, .Peek), peek_state: *PeekEmitState) !void {
            if (stack.items.len < 1) return;
            var v = stack.items[stack.items.len - 1];
            self.hydrateStructMetadata(&v, pk.name);
            // When peeking a struct, prefer the concrete HIR `StructId`
            // over variable-name based recovery so native printing can
            // include field names.
            if (pk.value_type == .Struct) {
                const sid = pk.value_type.Struct;
                if (v.struct_type_name == null) {
                    v.struct_type_name = self.struct_type_names_by_id.get(sid);
                }
                if (v.struct_field_types == null) {
                    v.struct_field_types = self.struct_fields_by_id.get(sid);
                }
                if (v.struct_field_names == null) {
                    if (v.struct_type_name) |tn| {
                        if (self.struct_field_names_by_type.get(tn)) |names| {
                            v.struct_field_names = names;
                        }
                    }
                }
            }
            stack.items[stack.items.len - 1] = v;
            const val = v;

            try self.emitPeekInstruction(w, pk, val, id, peek_state);

            // Nothing type - print "nothing" and return
            if (val.ty == .Nothing or pk.value_type == .Nothing) {
                const nothing_info = try internPeekString(
                    self.allocator,
                    &peek_state.*.string_map,
                    &peek_state.*.strings,
                    peek_state.*.next_id_ptr,
                    &peek_state.*.globals,
                    "nothing",
                );
                const nothing_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                id.* += 1;
                const nothing_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ nothing_ptr, nothing_info.length, nothing_info.name });
                defer self.allocator.free(nothing_gep);
                try w.writeAll(nothing_gep);
                const nothing_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s}, i64 {d})\n", .{ nothing_ptr, nothing_info.length });
                defer self.allocator.free(nothing_call);
                try w.writeAll(nothing_call);
                try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
                return;
            }

            // Prefer enum-aware printing when we know the enum type,
            // falling back to raw integers only when we have no metadata.
            if (pk.value_type == .Enum) {
                if (pk.enum_type_name) |etype| {
                    try self.emitEnumPrint(peek_state, w, id, etype, val.name);
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
                    return;
                } else if (val.enum_type_name) |etype2| {
                    try self.emitEnumPrint(peek_state, w, id, etype2, val.name);
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
                    return;
                }
            }

            // Use HIR type information when available to determine how to print,
            // especially for floats that might be stored as I64 bit patterns
            const should_print_as_float = switch (pk.value_type) {
                .Float => true,
                else => false,
            };
            const should_print_as_byte = switch (pk.value_type) {
                .Byte => true,
                else => false,
            };
            const should_print_as_string = switch (pk.value_type) {
                .String => true,
                else => false,
            };

            if (should_print_as_string and val.ty == .STRING) {
                // DoxaString value - extract ptr and len
                const s_ptr = try self.nextTemp(id);
                const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, val.name });
                defer self.allocator.free(s_ptr_line);
                try w.writeAll(s_ptr_line);
                const s_len = try self.nextTemp(id);
                const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, val.name });
                defer self.allocator.free(s_len_line);
                try w.writeAll(s_len_line);
                const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s}, i64 {s})\n", .{ s_ptr, s_len });
                defer self.allocator.free(call_val);
                try w.writeAll(call_val);
            } else if (should_print_as_string and val.ty == .PTR) {
                // String value - let runtime handle formatting
                const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s}, i64 0)\n", .{val.name});
                defer self.allocator.free(call_val);
                try w.writeAll(call_val);
            } else if (val.ty == .Value) {
                var printed_union_enum = false;
                if (pk.value_type == .Union and pk.union_members != null) {
                    const members = pk.union_members.?;
                    var enum_member_idx: ?usize = null;
                    var enum_member_name: ?[]const u8 = null;
                    for (members, 0..) |member_name, member_idx| {
                        if (self.enum_print_map.contains(member_name)) {
                            if (enum_member_idx != null) {
                                enum_member_idx = null;
                                enum_member_name = null;
                                break;
                            }
                            enum_member_idx = member_idx;
                            enum_member_name = member_name;
                        }
                    }

                    if (enum_member_idx) |enum_idx| {
                        const reserved = try self.nextTemp(id);
                        const reserved_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 1\n", .{ reserved, val.name });
                        defer self.allocator.free(reserved_line);
                        try w.writeAll(reserved_line);

                        const active_member = try self.nextTemp(id);
                        const active_member_line = try std.fmt.allocPrint(self.allocator, "  {s} = and i32 {s}, 65535\n", .{ active_member, reserved });
                        defer self.allocator.free(active_member_line);
                        try w.writeAll(active_member_line);

                        const enum_idx_i32: i32 = @intCast(enum_idx);
                        const is_enum_member = try self.nextTemp(id);
                        const is_enum_member_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i32 {s}, {d}\n", .{ is_enum_member, active_member, enum_idx_i32 });
                        defer self.allocator.free(is_enum_member_line);
                        try w.writeAll(is_enum_member_line);

                        const enum_label = try std.fmt.allocPrint(self.allocator, "peek_union_enum_{d}", .{id.*});
                        id.* += 1;
                        defer self.allocator.free(enum_label);
                        const fallback_label = try std.fmt.allocPrint(self.allocator, "peek_union_fallback_{d}", .{id.*});
                        id.* += 1;
                        defer self.allocator.free(fallback_label);
                        const merge_label = try std.fmt.allocPrint(self.allocator, "peek_union_merge_{d}", .{id.*});
                        id.* += 1;
                        defer self.allocator.free(merge_label);

                        const branch_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ is_enum_member, enum_label, fallback_label });
                        defer self.allocator.free(branch_line);
                        try w.writeAll(branch_line);

                        const enum_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{enum_label});
                        defer self.allocator.free(enum_label_line);
                        try w.writeAll(enum_label_line);

                        const payload_bits = try self.nextTemp(id);
                        const payload_bits_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload_bits, val.name });
                        defer self.allocator.free(payload_bits_line);
                        try w.writeAll(payload_bits_line);

                        try self.emitEnumPrint(peek_state, w, id, enum_member_name.?, payload_bits);

                        const enum_br_merge_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{merge_label});
                        defer self.allocator.free(enum_br_merge_line);
                        try w.writeAll(enum_br_merge_line);

                        const fallback_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{fallback_label});
                        defer self.allocator.free(fallback_label_line);
                        try w.writeAll(fallback_label_line);

                        const tmp_ptr = try self.nextTemp(id);
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ val.name, tmp_ptr });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);

                        const fallback_br_merge_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{merge_label});
                        defer self.allocator.free(fallback_br_merge_line);
                        try w.writeAll(fallback_br_merge_line);

                        const merge_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{merge_label});
                        defer self.allocator.free(merge_label_line);
                        try w.writeAll(merge_label_line);

                        printed_union_enum = true;
                    }
                }

                if (!printed_union_enum) {
                    // Store to stack and pass pointer to avoid ABI quirks
                    const tmp_ptr = try self.nextTemp(id);
                    const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                    defer self.allocator.free(alloca_line);
                    try w.writeAll(alloca_line);
                    const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ val.name, tmp_ptr });
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                }
            } else if (should_print_as_float and val.ty == .I64) {
                // Float stored as I64 bit pattern - convert to double for printing
                const double_val = try self.nextTemp(id);
                const bitcast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ double_val, val.name });
                defer self.allocator.free(bitcast_line);
                try w.writeAll(bitcast_line);
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{double_val});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
            } else if (should_print_as_byte and val.ty == .I64) {
                // Byte stored as I64 - use byte print function
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_byte(i64 {s})\n", .{val.name});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
            } else switch (val.ty) {
                .I64 => {
                    // Check if this is an enum value
                    if (val.enum_type_name) |type_name| {
                        try self.emitEnumPrint(peek_state, w, id, type_name, val.name);
                    } else {
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{val.name});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                },
                .F64 => {
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{val.name});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                .I8 => {
                    // Byte value - convert to i64 and use byte print function
                    const byte_i64 = try self.nextTemp(id);
                    const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ byte_i64, val.name });
                    defer self.allocator.free(zext_line);
                    try w.writeAll(zext_line);
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_byte(i64 {s})\n", .{byte_i64});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                .PTR => {
                    if (val.struct_field_types) |fts| {
                        _ = fts;
                        const dv = try self.buildDoxaValue(w, val, null, id);
                        const tmp_ptr = try self.nextTemp(id);
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ dv.name, tmp_ptr });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    } else if (val.array_type) |_| {
                        const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{val.name});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                    } else {
                        // Print string - let runtime handle formatting
                        const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s}, i64 0)\n", .{val.name});
                        defer self.allocator.free(call_val);
                        try w.writeAll(call_val);
                    }
                },
                .I2 => {
                    // tetra: print false (0), true (1), both (2), or neither (3)
                    const false_info = try internPeekString(
                        self.allocator,
                        &peek_state.string_map,
                        &peek_state.strings,
                        peek_state.next_id_ptr,
                        &peek_state.globals,
                        "false",
                    );
                    const true_info = try internPeekString(
                        self.allocator,
                        &peek_state.string_map,
                        &peek_state.strings,
                        peek_state.next_id_ptr,
                        &peek_state.globals,
                        "true",
                    );
                    const both_info = try internPeekString(
                        self.allocator,
                        &peek_state.string_map,
                        &peek_state.strings,
                        peek_state.next_id_ptr,
                        &peek_state.globals,
                        "both",
                    );
                    const neither_info = try internPeekString(
                        self.allocator,
                        &peek_state.string_map,
                        &peek_state.strings,
                        peek_state.next_id_ptr,
                        &peek_state.globals,
                        "neither",
                    );

                    // Convert i2 to i64 for comparison
                    const tetra_i64 = try self.nextTemp(id);
                    const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ tetra_i64, val.name });
                    defer self.allocator.free(zext_line);
                    try w.writeAll(zext_line);

                    // Compare with 0, 1, 2, 3
                    const eq0 = try self.nextTemp(id);
                    const eq1 = try self.nextTemp(id);
                    const eq2 = try self.nextTemp(id);
                    const eq3 = try self.nextTemp(id);
                    const icmp0 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i64 {s}, 0\n", .{ eq0, tetra_i64 });
                    const icmp1 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i64 {s}, 1\n", .{ eq1, tetra_i64 });
                    const icmp2 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i64 {s}, 2\n", .{ eq2, tetra_i64 });
                    const icmp3 = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i64 {s}, 3\n", .{ eq3, tetra_i64 });
                    defer self.allocator.free(icmp0);
                    defer self.allocator.free(icmp1);
                    defer self.allocator.free(icmp2);
                    defer self.allocator.free(icmp3);
                    try w.writeAll(icmp0);
                    try w.writeAll(icmp1);
                    try w.writeAll(icmp2);
                    try w.writeAll(icmp3);

                    // Get pointers to string constants
                    const fptr = try self.nextTemp(id);
                    const tptr = try self.nextTemp(id);
                    const bptr = try self.nextTemp(id);
                    const nptr = try self.nextTemp(id);
                    const fgep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ fptr, false_info.length, false_info.name });
                    const tgep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ tptr, true_info.length, true_info.name });
                    const bgep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ bptr, both_info.length, both_info.name });
                    const ngep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ nptr, neither_info.length, neither_info.name });
                    defer self.allocator.free(fgep);
                    defer self.allocator.free(tgep);
                    defer self.allocator.free(bgep);
                    defer self.allocator.free(ngep);
                    try w.writeAll(fgep);
                    try w.writeAll(tgep);
                    try w.writeAll(bgep);
                    try w.writeAll(ngep);

                    // Get length constants for tetra strings
                    const flen = try self.nextTemp(id);
                    const tlen = try self.nextTemp(id);
                    const blen = try self.nextTemp(id);
                    const nlen = try self.nextTemp(id);
                    const flen_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ flen, false_info.len_name });
                    const tlen_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ tlen, true_info.len_name });
                    const blen_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ blen, both_info.len_name });
                    const nlen_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ nlen, neither_info.len_name });
                    defer self.allocator.free(flen_line);
                    defer self.allocator.free(tlen_line);
                    defer self.allocator.free(blen_line);
                    defer self.allocator.free(nlen_line);
                    try w.writeAll(flen_line);
                    try w.writeAll(tlen_line);
                    try w.writeAll(blen_line);
                    try w.writeAll(nlen_line);

                    // Select: if eq3 then neither, else if eq2 then both, else if eq1 then true, else false
                    // First: if eq1 then true, else false
                    const sel01 = try self.nextTemp(id);
                    const sel01_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel01, eq1, tptr, fptr });
                    defer self.allocator.free(sel01_line);
                    try w.writeAll(sel01_line);

                    const sel01_len = try self.nextTemp(id);
                    const sel01_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, i64 {s}, i64 {s}\n", .{ sel01_len, eq1, tlen, flen });
                    defer self.allocator.free(sel01_len_line);
                    try w.writeAll(sel01_len_line);

                    // Second: if eq2 then both, else sel01 (true/false)
                    const sel012 = try self.nextTemp(id);
                    const sel012_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel012, eq2, bptr, sel01 });
                    defer self.allocator.free(sel012_line);
                    try w.writeAll(sel012_line);

                    const sel012_len = try self.nextTemp(id);
                    const sel012_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, i64 {s}, i64 {s}\n", .{ sel012_len, eq2, blen, sel01_len });
                    defer self.allocator.free(sel012_len_line);
                    try w.writeAll(sel012_len_line);

                    // Final: if eq3 then neither, else sel012 (both/true/false)
                    const sel_final = try self.nextTemp(id);
                    const sel_final_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel_final, eq3, nptr, sel012 });
                    defer self.allocator.free(sel_final_line);
                    try w.writeAll(sel_final_line);

                    const sel_final_len = try self.nextTemp(id);
                    const sel_final_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, i64 {s}, i64 {s}\n", .{ sel_final_len, eq3, nlen, sel012_len });
                    defer self.allocator.free(sel_final_len_line);
                    try w.writeAll(sel_final_len_line);

                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s}, i64 {s})\n", .{ sel_final, sel_final_len });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                .STRING => {
                    const s_ptr = try self.nextTemp(id);
                    const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, val.name });
                    defer self.allocator.free(s_ptr_line);
                    try w.writeAll(s_ptr_line);
                    const s_len = try self.nextTemp(id);
                    const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, val.name });
                    defer self.allocator.free(s_len_line);
                    try w.writeAll(s_len_line);
                    const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s}, i64 {s})\n", .{ s_ptr, s_len });
                    defer self.allocator.free(call_val);
                    try w.writeAll(call_val);
                },
                else => {},
            }
            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
        }

        pub fn handlePeekStruct(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, ps: std.meta.TagPayload(HIRInstruction, .PeekStruct), peek_state: *PeekEmitState) !void {
            // PeekStruct peeks a struct without popping it
            // Similar to Peek, but for structs
            if (stack.items.len < 1) return;
            const v = stack.items[stack.items.len - 1];

            // Emit peek debug info (type name and variable name prefix)
            try self.emitPeekInstruction(w, .{
                .name = null,
                .value_type = .Struct,
                .location = ps.location,
                .union_members = null,
                .enum_type_name = null,
            }, v, id, peek_state);

            const enum_names = try self.allocator.alloc(?[]const u8, ps.field_names.len);
            defer self.allocator.free(enum_names);
            @memset(enum_names, null);
            const desc_global = try self.getOrCreateStructDescGlobal(peek_state, ps.type_name, ps.field_names, ps.field_types, enum_names);
            const reg_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_struct_register(ptr {s}, ptr {s})\n", .{ v.name, desc_global });
            defer self.allocator.free(reg_line);
            try w.writeAll(reg_line);

            var print_v = v;
            print_v.struct_type_name = ps.type_name;
            const dv = try self.buildDoxaValue(w, print_v, null, id);
            const tmp_ptr = try self.nextTemp(id);
            const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
            defer self.allocator.free(alloca_line);
            try w.writeAll(alloca_line);
            const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ dv.name, tmp_ptr });
            defer self.allocator.free(store_line);
            try w.writeAll(store_line);
            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            // Print newline
            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
        }

        pub fn handleGroupCheck(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, gc: std.meta.TagPayload(HIRInstruction, .GroupCheck)) !void {
            if (stack.items.len < 1) return;
            const value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            if (value.ty == .Value) {
                const reserved_i32 = try self.nextTemp(id);
                const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 1\n", .{ reserved_i32, value.name });
                defer self.allocator.free(extract_line);
                try w.writeAll(extract_line);

                const member_i32 = try self.nextTemp(id);
                const shift_line = try std.fmt.allocPrint(self.allocator, "  {s} = lshr i32 {s}, 16\n", .{ member_i32, reserved_i32 });
                defer self.allocator.free(shift_line);
                try w.writeAll(shift_line);

                const expected = try self.nextTemp(id);
                const expected_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i32 0, {d}\n", .{ expected, gc.member_index });
                defer self.allocator.free(expected_line);
                try w.writeAll(expected_line);

                const eq_i1 = try self.nextTemp(id);
                const cmp_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i32 {s}, {s}\n", .{ eq_i1, member_i32, expected });
                defer self.allocator.free(cmp_line);
                try w.writeAll(cmp_line);

                const result = try self.nextTemp(id);
                const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ result, eq_i1 });
                defer self.allocator.free(zext_line);
                try w.writeAll(zext_line);

                try stack.append(.{ .name = result, .ty = .I2 });
            } else {
                const result = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i2 0, 0\n", .{result});
                defer self.allocator.free(line);
                try w.writeAll(line);
                try stack.append(.{ .name = result, .ty = .I2 });
            }
        }

        pub fn handleGroupExtractPayload(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize) !void {
            if (stack.items.len < 1) return;
            const value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            if (value.ty == .Value) {
                const payload = try self.nextTemp(id);
                const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, value.name });
                defer self.allocator.free(extract_line);
                try w.writeAll(extract_line);

                try stack.append(.{ .name = payload, .ty = .I64 });
            } else {
                try stack.append(value);
            }
        }

        pub fn handleUnionConstruct(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, uc: std.meta.TagPayload(HIRInstruction, .UnionConstruct)) !void {
            if (stack.items.len < 1) return;
            const value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            // Build a %DoxaValue for the union — same as buildDoxaValue
            const dv = try self.buildDoxaValue(w, value, uc.union_type, id);
            try stack.append(dv);
        }

        pub fn handleAssertFail(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, af: std.meta.TagPayload(HIRInstruction, .AssertFail), peek_state: *PeekEmitState) !void {
            const msg = if (af.has_message)
                (if (stack.items.len > 0) blk: {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    break :blk v;
                } else null)
            else
                null;
            if (msg) |m| {
                const ptr = if (m.ty == .PTR) m else try self.ensurePointer(w, m, id);
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_raw(ptr {s})\n", .{ptr.name});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
            }
            const location_msg = try std.fmt.allocPrint(self.allocator, "Assertion failed", .{});
            defer self.allocator.free(location_msg);
            const info = try internPeekString(
                self.allocator,
                &peek_state.*.string_map,
                &peek_state.*.strings,
                peek_state.*.next_id_ptr,
                &peek_state.*.globals,
                location_msg,
            );
            const loc_ptr = try self.nextTemp(id);
            const loc_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ loc_ptr, info.length, info.name });
            defer self.allocator.free(loc_gep);
            try w.writeAll(loc_gep);
            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s}, i64 {d})\n", .{ loc_ptr, info.length });
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0), i64 1)\n");
        }

        pub fn handleArrayConcat(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize) !void {
            // Pop the two arrays from stack
            if (stack.items.len >= 2) {
                const rhs = stack.items[stack.items.len - 1];
                const lhs = stack.items[stack.items.len - 2];
                stack.items.len -= 2;

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
        }

        pub fn handleCall(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, c: std.meta.TagPayload(HIRInstruction, .Call), peek_state: *PeekEmitState, hir: *const HIR.HIRProgram) !void {
            _ = peek_state;
            const argc: usize = @intCast(c.arg_count);
            if (stack.items.len < argc) return;

            var raw_args = std.array_list.Managed(StackVal).init(self.allocator);
            defer raw_args.deinit();

            var arg_idx: usize = 0;
            while (arg_idx < argc) : (arg_idx += 1) {
                const arg = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                try raw_args.append(arg);
            }
            std.mem.reverse(StackVal, raw_args.items);

            if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "find") and raw_args.items.len == 2) {
                const collection = raw_args.items[0];
                const needle = raw_args.items[1];

                const coll_ptr = if (collection.ty == .PTR) collection else try self.ensurePointer(w, collection, id);

                if (collection.array_type != null) {
                    const needle_i64 = if (needle.ty == .I64) needle else try self.ensureI64(w, needle, id);
                    const result_name = try self.nextTemp(id);
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_find_array(ptr {s}, i64 {s})\n", .{ result_name, coll_ptr.name, needle_i64.name });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = result_name, .ty = .I64 });
                } else {
                    const needle_str = if (needle.ty == .STRING) needle else try self.ensureString(w, needle, id);
                    const coll_str = if (collection.ty == .STRING) collection else try self.ensureString(w, collection, id);
                    const c_ptr = try self.nextTemp(id);
                    const c_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ c_ptr, coll_str.name });
                    defer self.allocator.free(c_ptr_line);
                    try w.writeAll(c_ptr_line);
                    const c_len = try self.nextTemp(id);
                    const c_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ c_len, coll_str.name });
                    defer self.allocator.free(c_len_line);
                    try w.writeAll(c_len_line);
                    const n_ptr = try self.nextTemp(id);
                    const n_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ n_ptr, needle_str.name });
                    defer self.allocator.free(n_ptr_line);
                    try w.writeAll(n_ptr_line);
                    const n_len = try self.nextTemp(id);
                    const n_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ n_len, needle_str.name });
                    defer self.allocator.free(n_len_line);
                    try w.writeAll(n_len_line);
                    const result_name = try self.nextTemp(id);
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_find_str(ptr {s}, i64 {s}, ptr {s}, i64 {s})\n", .{ result_name, c_ptr, c_len, n_ptr, n_len });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    try stack.append(.{ .name = result_name, .ty = .I64 });
                }

                return;
            }

            if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "string") and raw_args.items.len == 1) {
                const arg = raw_args.items[0];

                switch (arg.ty) {
                    .STRING => {
                        try stack.append(arg);
                    },
                    .I64 => {
                        const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{arg.name});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_int_to_string", args_line);
                    },
                    .F64 => {
                        const args_line = try std.fmt.allocPrint(self.allocator, "double {s}", .{arg.name});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_float_to_string", args_line);
                    },
                    .I8 => {
                        const widened = try self.nextTemp(id);
                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ widened, arg.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{widened});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_byte_to_string", args_line);
                    },
                    .PTR => {
                        if (arg.array_type != null) {
                            const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}", .{arg.name});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_array_to_string", args_line);
                        } else if (arg.struct_field_types != null or arg.struct_type_name != null) {
                            const args_line = try std.fmt.allocPrint(self.allocator, "ptr {s}", .{arg.name});
                            defer self.allocator.free(args_line);
                            try self.emitRTCallReturningString(w, stack, id, "doxa_struct_to_string", args_line);
                        } else {
                            const tmp_name = try self.nextTemp(id);
                            const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, arg.name });
                            defer self.allocator.free(ins0);
                            try w.writeAll(ins0);
                            const str_name = try self.nextTemp(id);
                            const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 0, 1\n", .{ str_name, tmp_name });
                            defer self.allocator.free(ins1);
                            try w.writeAll(ins1);
                            try stack.append(.{ .name = str_name, .ty = .STRING });
                        }
                    },
                    .I1, .I2 => {
                        const src_ty = self.stackTypeToLLVMType(arg.ty);
                        const widened = try self.nextTemp(id);
                        const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, arg.name });
                        defer self.allocator.free(zext_line);
                        try w.writeAll(zext_line);
                        const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{widened});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_tetra_to_string", args_line);
                    },
                    .Nothing => {
                        const args_line = try std.fmt.allocPrint(self.allocator, "", .{});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_nothing_to_string", args_line);
                    },
                    else => {
                        const arg_i64 = try self.ensureI64(w, arg, id);
                        const args_line = try std.fmt.allocPrint(self.allocator, "i64 {s}", .{arg_i64.name});
                        defer self.allocator.free(args_line);
                        try self.emitRTCallReturningString(w, stack, id, "doxa_int_to_string", args_line);
                    },
                }
                return;
            }

            if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "print") and raw_args.items.len == 1) {
                const arg = raw_args.items[0];
                if (arg.ty == .STRING) {
                    const ptr_ext = try self.nextTemp(id);
                    const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                    defer self.allocator.free(ext_line);
                    try w.writeAll(ext_line);
                    const len_ext = try self.nextTemp(id);
                    const len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                    defer self.allocator.free(len_line);
                    try w.writeAll(len_line);
                    const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s}, i64 {s})\n", .{ptr_ext, len_ext});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    return;
                }
                if (arg.ty == .PTR) {
                    const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_raw(ptr {s})\n", .{arg.name});
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    return;
                }
                const ptr = try self.ensurePointer(w, arg, id);
                const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_raw(ptr {s})\n", .{ptr.name});
                defer self.allocator.free(line);
                try w.writeAll(line);
                return;
            }

            if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "range") and raw_args.items.len == 2) {
                const start = raw_args.items[0];
                const end = raw_args.items[1];
                const start_i64 = if (start.ty == .I64) start else try self.ensureI64(w, start, id);
                const end_i64 = if (end.ty == .I64) end else try self.ensureI64(w, end, id);
                const result_name = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_range(i64 {s}, i64 {s})\n", .{ result_name, start_i64.name, end_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                try stack.append(.{ .name = result_name, .ty = .PTR, .array_type = HIR.HIRType.Int });
                return;
            }

            var arg_strings = std.array_list.Managed([]const u8).init(self.allocator);
            defer {
                for (arg_strings.items) |s| self.allocator.free(s);
                arg_strings.deinit();
            }

            const func_info = if (c.function_index < hir.function_table.len)
                blk: {
                    const candidate = hir.function_table[c.function_index];
                    if (std.mem.eql(u8, candidate.qualified_name, c.qualified_name))
                        break :blk candidate;
                    break :blk null;
                }
            else
                null;

            for (raw_args.items, 0..) |*arg_ptr, i| {
                var arg = arg_ptr.*;
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
                if (!is_alias) {
                    if (declared_type) |decl| {
                        if (arg.ty == .Value and decl != .Union) {
                            arg = try self.unwrapDoxaValueToType(w, arg, decl, id);
                            arg_ptr.* = arg;
                        }
                        if (arg.ty != .Value and decl == .Union) {
                            arg = try self.buildDoxaValue(w, arg, decl, id);
                            arg_ptr.* = arg;
                        }
                    }
                }

                if (c.call_kind == .ModuleFunction and arg.ty == .STRING) {
                    const ptr_ext = try self.nextTemp(id);
                    const len_ext = try self.nextTemp(id);
                    const ext0 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, arg.name });
                    const ext1 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ len_ext, arg.name });
                    defer self.allocator.free(ext0);
                    defer self.allocator.free(ext1);
                    try w.writeAll(ext0);
                    try w.writeAll(ext1);
                    const ptr_str = try std.fmt.allocPrint(self.allocator, "ptr {s}, i64 {s}", .{ ptr_ext, len_ext });
                    try arg_strings.append(ptr_str);
                    continue;
                }

                if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "find") and i == 1 and arg.ty == .PTR) {
                    const as_i64 = try self.nextTemp(id);
                    const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ as_i64, arg.name });
                    defer self.allocator.free(cast_line);
                    try w.writeAll(cast_line);
                    arg = .{ .name = as_i64, .ty = .I64 };
                    arg_ptr.* = arg;
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

            var actual_return_type: HIR.HIRType = if (c.return_type != .Nothing) c.return_type else .Nothing;
            if (func_info) |info| {
                if (actual_return_type == .Nothing) {
                    actual_return_type = info.return_type;
                } else if ((actual_return_type == .Unknown or actual_return_type == .String) and (info.return_type == .Array or info.return_type == .Map)) {
                    actual_return_type = info.return_type;
                }
            }

            if (actual_return_type != .Nothing) {
                if (c.call_kind == .ModuleFunction and actual_return_type == .String) {
                    // Module functions use out-params for string returns (ABI compat)
                    const out_ptr_slot = try self.nextTemp(id);
                    const out_len_slot = try self.nextTemp(id);
                    const alloca_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
                    const alloca_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
                    defer self.allocator.free(alloca_ptr_line);
                    defer self.allocator.free(alloca_len_line);
                    try w.writeAll(alloca_ptr_line);
                    try w.writeAll(alloca_len_line);
                    const init_ptr = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
                    const init_len = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
                    defer self.allocator.free(init_ptr);
                    defer self.allocator.free(init_len);
                    try w.writeAll(init_ptr);
                    try w.writeAll(init_len);

                    const out_args = try std.fmt.allocPrint(self.allocator, ", ptr {s}, ptr {s}", .{out_ptr_slot, out_len_slot});
                    defer self.allocator.free(out_args);
                    const full_args = if (args_str.len > 0)
                        try std.fmt.allocPrint(self.allocator, "{s}{s}", .{ args_str, out_args })
                    else
                        try std.fmt.allocPrint(self.allocator, "ptr {s}, ptr {s}", .{out_ptr_slot, out_len_slot});
                    defer if (full_args.ptr != args_str.ptr) self.allocator.free(full_args);

                    const runtime_name = IRPrinter.mapBuiltinToRuntime(c.qualified_name);
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}({s})\n", .{ runtime_name, full_args });
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);

                    const loaded_ptr = try self.nextTemp(id);
                    const loaded_len = try self.nextTemp(id);
                    const load_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{loaded_ptr, out_ptr_slot});
                    const load_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{loaded_len, out_len_slot});
                    defer self.allocator.free(load_ptr_line);
                    defer self.allocator.free(load_len_line);
                    try w.writeAll(load_ptr_line);
                    try w.writeAll(load_len_line);

                    const tmp_name = try self.nextTemp(id);
                    const str_name = try self.nextTemp(id);
                    const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{tmp_name, loaded_ptr});
                    const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{str_name, tmp_name, loaded_len});
                    defer self.allocator.free(ins0);
                    defer self.allocator.free(ins1);
                    try w.writeAll(ins0);
                    try w.writeAll(ins1);
                    try stack.append(.{ .name = str_name, .ty = .STRING });
                    return;
                }

                const result_name = try self.nextTemp(id);
                const ret_ty = self.hirTypeToLLVMType(actual_return_type, false);
                const runtime_name = IRPrinter.mapBuiltinToRuntime(c.qualified_name);
                const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call {s} @{s}({s})\n", .{ result_name, ret_ty, runtime_name, args_str });
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
                const stack_ty = self.hirTypeToStackType(actual_return_type);
                var pushed = StackVal{ .name = result_name, .ty = stack_ty };
                if (stack_ty == .PTR) {
                    switch (actual_return_type) {
                        .Array => |inner| pushed.array_type = inner.*,
                        .Map => |kv| pushed.array_type = kv.value.*,
                        else => {},
                    }
                }
                if (stack_ty == .PTR and actual_return_type == .Struct) {
                    if (self.function_struct_return_fields.get(c.qualified_name)) |fts| {
                        pushed.struct_field_types = fts;
                    }
                    if (self.function_struct_return_type_names.get(c.qualified_name)) |tname| {
                        pushed.struct_type_name = tname;
                        if (self.struct_field_names_by_type.get(tname)) |names| {
                            pushed.struct_field_names = names;
                        }
                    }
                }
                try stack.append(pushed);
            } else {
                const runtime_name = IRPrinter.mapBuiltinToRuntime(c.qualified_name);
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}({s})\n", .{ runtime_name, args_str });
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
                const nothing_name = try self.nextTemp(id);
                try stack.append(.{ .name = nothing_name, .ty = .Nothing });
            }
        }

        pub fn handleStoreDeclGlobal(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sd: std.meta.TagPayload(HIRInstruction, .StoreDecl)) !void {
            if (stack.items.len < 1) {
                const stack_type = self.hirTypeToStackType(sd.declared_type);
                _ = try self.global_types.put(sd.var_name, stack_type);
                _ = try self.defined_globals.put(sd.var_name, true);
                const gptr = try self.mangleGlobalName(sd.var_name);
                defer self.allocator.free(gptr);
                return;
            }
            var value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            if (sd.declared_type == .Array and value.array_type == null) {
                value.array_type = sd.declared_type.Array.*;
            }
            if (sd.declared_type == .Union) {
                value = try self.buildDoxaValue(w, value, sd.declared_type, id);
            }
            if (!sd.is_const and sd.declared_type == .Array) {
                const src_ptr = if (value.ty == .PTR) value else try self.ensurePointer(w, value, id);
                const clone_reg = try self.nextTemp(id);
                const clone_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_clone(ptr {s})\n", .{ clone_reg, src_ptr.name });
                defer self.allocator.free(clone_line);
                try w.writeAll(clone_line);
                value = .{ .name = clone_reg, .ty = .PTR, .array_type = value.array_type };
            }
            if (sd.declared_type == .Struct and value.ty == .PTR and value.struct_type_name == null) {
                value.struct_type_name = try self.hirTypeToTypeString(self.allocator, sd.declared_type);
                if (self.struct_fields_by_id.get(sd.declared_type.Struct)) |fts| {
                    value.struct_field_types = fts;
                }
                if (value.struct_type_name) |tname| {
                    if (self.struct_field_names_by_type.get(tname)) |names| {
                        value.struct_field_names = names;
                    }
                }
            }
            const declared_stack_type = self.hirTypeToStackType(sd.declared_type);
            const target_ty: StackType = if (sd.declared_type == .Unknown) value.ty else declared_stack_type;
            value = try self.coerceForStore(value, target_ty, id, w);
            const llvm_ty = self.stackTypeToLLVMType(target_ty);
            _ = try self.global_types.put(sd.var_name, target_ty);
            if (value.array_type) |array_type| {
                _ = try self.global_array_types.put(sd.var_name, array_type);
            }
            _ = try self.defined_globals.put(sd.var_name, true);
            const gptr = try self.mangleGlobalName(sd.var_name);
            defer self.allocator.free(gptr);
            if (value.struct_field_types) |fts| {
                _ = try self.global_struct_field_types.put(sd.var_name, fts);
            }
            const stored_names = if (value.struct_field_names) |names|
                names
            else if (value.struct_type_name) |tname|
                self.struct_field_names_by_type.get(tname)
            else
                null;
            if (stored_names) |names| {
                _ = try self.global_struct_field_names.put(sd.var_name, names);
            }
            if (value.struct_type_name) |tname| {
                _ = try self.global_struct_type_names.put(sd.var_name, tname);
            }
            const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, gptr });
            defer self.allocator.free(store_line);
            try w.writeAll(store_line);
        }

        pub fn handleStoreVarGlobal(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sv: std.meta.TagPayload(HIRInstruction, .StoreVar)) !void {
            if (stack.items.len < 1) return;
            var value = stack.items[stack.items.len - 1];
            stack.items.len -= 1;
            if (value.ty == .Nothing) return;
            const expected_array_type: ?HIR.HIRType = switch (sv.expected_type) {
                .Array => |inner| inner.*,
                else => null,
            };
            if (value.array_type == null and expected_array_type != null) {
                value.array_type = expected_array_type.?;
            }
            if (sv.expected_type == .Union) {
                value = try self.buildDoxaValue(w, value, sv.expected_type, id);
            }
            const llvm_ty = self.stackTypeToLLVMType(value.ty);
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
            const stored_names = if (value.struct_field_names) |names|
                names
            else if (value.struct_type_name) |tname|
                self.struct_field_names_by_type.get(tname)
            else
                null;
            if (stored_names) |names| {
                _ = try self.global_struct_field_names.put(sv.var_name, names);
            }
            if (value.struct_type_name) |tname| {
                _ = try self.global_struct_type_names.put(sv.var_name, tname);
            }
            const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, gptr });
            defer self.allocator.free(store_line);
            try w.writeAll(store_line);
        }

        pub fn handleLoadVarGlobal(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, gname: []const u8) !void {
            const st = self.global_types.get(gname) orelse .I64;
            if (st == .Nothing) {
                const result_name = try self.nextTemp(id);
                try stack.append(.{ .name = result_name, .ty = .Nothing });
                return;
            }
            const llty = self.stackTypeToLLVMType(st);
            const gptr = try self.mangleGlobalName(gname);
            defer self.allocator.free(gptr);
            if (!self.defined_globals.contains(gname)) {
                _ = try self.global_types.put(gname, st);
                _ = try self.defined_globals.put(gname, true);
            }
            const result_name = try self.nextTemp(id);
            const line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result_name, llty, gptr });
            defer self.allocator.free(line);
            try w.writeAll(line);
            const array_type = self.global_array_types.get(gname);
            const enum_type_name = self.global_enum_types.get(gname);
            const struct_fields = self.global_struct_field_types.get(gname);
            const struct_names = self.global_struct_field_names.get(gname);
            const struct_type_name = self.global_struct_type_names.get(gname);
            try stack.append(.{ .name = result_name, .ty = st, .array_type = array_type, .enum_type_name = enum_type_name, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
        }

        pub fn handlePushStorageIdGlobal(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, gname: []const u8) !void {
            _ = w;
            _ = id;
            const st = self.global_types.get(gname) orelse .I64;
            if (!self.defined_globals.contains(gname)) {
                _ = try self.global_types.put(gname, st);
                _ = try self.defined_globals.put(gname, true);
            }
            const gptr = try self.mangleGlobalName(gname);
            const struct_fields = self.global_struct_field_types.get(gname);
            const struct_names = self.global_struct_field_names.get(gname);
            const struct_type_name = self.global_struct_type_names.get(gname);
            try stack.append(.{ .name = gptr, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
        }
    };
}
