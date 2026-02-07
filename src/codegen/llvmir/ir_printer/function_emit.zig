const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const PeekEmitState = Ctx.PeekEmitState;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const VariableInfo = Ctx.VariableInfo;
    const StackMergeState = Ctx.StackMergeState;
    const EnumVariantMeta = Ctx.EnumVariantMeta;
    const internPeekString = Ctx.internPeekString;

    return struct {
    pub fn writeFunction(
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
            struct_field_names: ?[]const []const u8 = null,
            struct_type_name: ?[]const u8 = null,
        };
        var alias_slots = std.AutoHashMap(u32, AliasInfo).init(self.allocator);
        defer alias_slots.deinit();

        // Scan through instructions to find all variables that need allocation
        for (hir.instructions[start_idx..end_idx]) |inst| {
            switch (inst) {
                .StoreVar => |sv| {
                    if (variables_to_allocate.get(sv.var_name) == null) {
                        // We need to determine the type from the HIR context
                        // For now, prefer the expected type from HIR when available
                        const declared_stack_type = if (sv.expected_type != .Unknown)
                            self.hirTypeToStackType(sv.expected_type)
                        else
                            .I64;
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sv.var_name});
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = declared_stack_type, .array_type = null };
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
        const return_type_str = self.hirTypeToLLVMType(func.return_type, false);
        const target_return_stack_type = self.hirTypeToStackType(func.return_type);

        var param_strs = std.array_list.Managed([]const u8).init(self.allocator);
        defer {
            for (param_strs.items) |param_str| {
                self.allocator.free(param_str);
            }
            param_strs.deinit();
        }

        for (func.param_types, 0..) |param_type, param_idx| {
            const is_alias = if (param_idx < func.param_is_alias.len) func.param_is_alias[param_idx] else false;
            const param_stack_type = if (is_alias) .PTR else self.hirTypeToStackType(param_type);
            const param_type_str = self.stackTypeToLLVMType(param_stack_type);
            const param_str = try std.fmt.allocPrint(self.allocator, "{s} %{d}", .{ param_type_str, param_idx });
            try param_strs.append(param_str);
        }

        const params_str = if (param_strs.items.len == 0) "" else try std.mem.join(self.allocator, ", ", param_strs.items);
        defer if (param_strs.items.len > 0) self.allocator.free(params_str);

        // Rename user entry function so we can emit a proper C wrapper `@main`
        const emitted_name_owned = if (func.is_entry and !std.mem.eql(u8, func.qualified_name, "main"))
            try std.fmt.allocPrint(self.allocator, "doxa_entry_{s}", .{func.qualified_name})
        else
            null;
        defer if (emitted_name_owned) |name| self.allocator.free(name);
        const emitted_name = if (func.is_entry)
            (if (std.mem.eql(u8, func.qualified_name, "main"))
                "doxa_user_main"
            else
                emitted_name_owned.?)
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

            // Skip instructions after terminators (except labels which start new blocks)
            if (last_instruction_was_terminator and tag != .Label) {
                continue;
            }

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
                        var v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        if (func.return_type == .Union and v.ty != .Value) {
                            v = try self.buildDoxaValue(w, v, func.return_type, &id);
                        }
                        if (v.ty != target_return_stack_type) {
                            v = try self.coerceForStore(v, target_return_stack_type, &id, w);
                        }
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
                    try self.recordStackForLabel(&merge_map, jc.label_true, stack.items, current_block, &id, w);
                    try self.recordStackForLabel(&merge_map, jc.label_false, stack.items, current_block, &id, w);
                    const br_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ bool_val.name, jc.label_true, jc.label_false });
                    defer self.allocator.free(br_line);
                    try w.writeAll(br_line);
                    stack.items.len = 0;
                    last_instruction_was_terminator = true;
                },
                .Jump => |j| {
                    if (last_instruction_was_terminator) continue;
                    // Convert i1 to i2 for logical operation merge points
                    if (std.mem.startsWith(u8, j.label, "and_end") or std.mem.startsWith(u8, j.label, "or_end")) {
                        var i: usize = 0;
                        while (i < stack.items.len) : (i += 1) {
                            if (stack.items[i].ty == .I1) {
                                const converted_name = try self.nextTemp(&id);
                                const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ converted_name, stack.items[i].name });
                                defer self.allocator.free(zext_line);
                                try w.writeAll(zext_line);
                                stack.items[i].name = converted_name;
                                stack.items[i].ty = .I2;
                            }
                        }
                    }
                    try self.recordStackForLabel(&merge_map, j.label, stack.items, current_block, &id, w);
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
                            // Offset by string_pool.len to match the constant pool string index
                            const str_idx = hir.string_pool.len + idx;
                            const gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr @.str.{d}, i64 0, i64 0\n", .{ tmp, s.len + 1, str_idx });
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
                        .I8 => {
                            const byte_i64 = try self.nextTemp(&id);
                            const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ byte_i64, v.name });
                            defer self.allocator.free(zext_line);
                            try w.writeAll(zext_line);
                            const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_byte(i64 {s})\n", .{byte_i64});
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
                .ArrayGetAndAdd => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Add);
                    last_instruction_was_terminator = false;
                },
                .ArrayGetAndSub => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Sub);
                    last_instruction_was_terminator = false;
                },
                .ArrayGetAndMul => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Mul);
                    last_instruction_was_terminator = false;
                },
                .ArrayGetAndDiv => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Div);
                    last_instruction_was_terminator = false;
                },
                .ArrayGetAndMod => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Mod);
                    last_instruction_was_terminator = false;
                },
                .ArrayGetAndPow => |a| {
                    _ = a; // bounds_check not implemented yet
                    try self.emitArrayGetAndArith(w, &stack, &id, .Pow);
                    last_instruction_was_terminator = false;
                },
                .ArrayLen => {
                    try self.emitArrayLen(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .ArrayPush => {
                    try self.emitArrayPush(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .ArrayPop => {
                    try self.emitArrayPop(w, &stack, &id);
                    last_instruction_was_terminator = false;
                },
                .Map => |m| {
                    try self.emitMap(w, &stack, &id, m);
                    last_instruction_was_terminator = false;
                },
                .MapGet => |mg| {
                    try self.emitMapGet(w, &stack, &id, mg);
                    last_instruction_was_terminator = false;
                },
                .MapSet => |ms| {
                    try self.emitMapSet(w, &stack, &id, ms);
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
                    // Also promote when operand_type is Float but operands are integers
                    // CRITICAL: Always promote when we have mixed types, regardless of operand_type
                    // This ensures float + int always results in float, even if semantic analysis inferred Int
                    const has_mixed_types = (lhs.ty == .F64 and rhs.ty == .I64) or (lhs.ty == .I64 and rhs.ty == .F64);
                    const needs_promotion = has_mixed_types or
                        (a.op == .Div and (lhs.ty == .I64 or rhs.ty == .I64)) or
                        (a.operand_type == .Float and (lhs.ty != .F64 or rhs.ty != .F64));

                    // If the operation is integer but operands are not i64, coerce them first (handles ptr, i8, etc.)
                    if (a.operand_type == .Int and !needs_promotion) {
                        lhs = try self.ensureI64(w, lhs, &id);
                        rhs = try self.ensureI64(w, rhs, &id);
                    }

                    if (needs_promotion) {
                        // Promote both operands to double and use floating-point operations
                        // Convert each operand to double: I64 -> sitofp, F64 -> use directly, others -> ensureI64 then sitofp
                        const lhs_promoted = if (lhs.ty == .F64) blk: {
                            break :blk lhs.name;
                        } else blk: {
                            const lhs_i64 = try self.ensureI64(w, lhs, &id);
                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, lhs_i64.name });
                            defer self.allocator.free(conv_line);
                            try w.writeAll(conv_line);
                            break :blk conv_name;
                        };
                        const rhs_promoted = if (rhs.ty == .F64) blk: {
                            break :blk rhs.name;
                        } else blk: {
                            const rhs_i64 = try self.ensureI64(w, rhs, &id);
                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, rhs_i64.name });
                            defer self.allocator.free(conv_line);
                            try w.writeAll(conv_line);
                            break :blk conv_name;
                        };

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
                            .IntDiv => unreachable, // IntDiv should not be emitted as floating-point
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
                        // Handle Pow for Int specially (needs conversions before name allocation)
                        if (a.operand_type == .Int and a.op == .Pow) {
                            // Convert lhs to double
                            const lhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const lhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ lhs_double, lhs.name });
                            defer self.allocator.free(lhs_conv_line);
                            try w.writeAll(lhs_conv_line);
                            // Convert rhs to double
                            const rhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const rhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ rhs_double, rhs.name });
                            defer self.allocator.free(rhs_conv_line);
                            try w.writeAll(rhs_conv_line);
                            // Call pow with converted values (result is double, but we'll convert back)
                            // Allocate pow_result first so it gets a lower number
                            const pow_result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const pow_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ pow_result, lhs_double, rhs_double });
                            defer self.allocator.free(pow_line);
                            try w.writeAll(pow_line);
                            // Allocate name for the result after pow call
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            // Convert result back to i64
                            const conv_back_line = try std.fmt.allocPrint(self.allocator, "  {s} = fptosi double {s} to i64\n", .{ name, pow_result });
                            defer self.allocator.free(conv_back_line);
                            try w.writeAll(conv_back_line);
                            try stack.append(.{ .name = name, .ty = .I64 });
                        } else {
                            const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            switch (a.operand_type) {
                                .Int => {
                                    // Safety check: if we have mixed types, promote to float even if operand_type is Int
                                    const has_mixed = (lhs.ty == .F64 and rhs.ty == .I64) or (lhs.ty == .I64 and rhs.ty == .F64);
                                    if (has_mixed) {
                                        // Promote both operands to double
                                        const lhs_double = if (lhs.ty == .F64) lhs.name else blk: {
                                            const lhs_i64 = try self.ensureI64(w, lhs, &id);
                                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, lhs_i64.name });
                                            defer self.allocator.free(conv_line);
                                            try w.writeAll(conv_line);
                                            break :blk conv_name;
                                        };
                                        const rhs_double = if (rhs.ty == .F64) rhs.name else blk: {
                                            const rhs_i64 = try self.ensureI64(w, rhs, &id);
                                            const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, rhs_i64.name });
                                            defer self.allocator.free(conv_line);
                                            try w.writeAll(conv_line);
                                            break :blk conv_name;
                                        };
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
                                            .Mod => {
                                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = frem double {s}, {s}\n", .{ name, lhs_double, rhs_double });
                                                defer self.allocator.free(line);
                                                try w.writeAll(line);
                                            },
                                            else => unreachable,
                                        }
                                        try stack.append(.{ .name = name, .ty = .F64 });
                                    } else {
                                        // Pure integer operations (both operands are integers)
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
                                            else => unreachable,
                                        }
                                        try stack.append(.{ .name = name, .ty = .I64 });
                                    }
                                },
                                .Float => {
                                    // Convert integer operands to float if needed
                                    const lhs_double = if (lhs.ty == .F64) lhs.name else blk: {
                                        const lhs_i64 = try self.ensureI64(w, lhs, &id);
                                        const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, lhs_i64.name });
                                        defer self.allocator.free(conv_line);
                                        try w.writeAll(conv_line);
                                        break :blk conv_name;
                                    };
                                    const rhs_double = if (rhs.ty == .F64) rhs.name else blk: {
                                        const rhs_i64 = try self.ensureI64(w, rhs, &id);
                                        const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, rhs_i64.name });
                                        defer self.allocator.free(conv_line);
                                        try w.writeAll(conv_line);
                                        break :blk conv_name;
                                    };
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
                                        .IntDiv => unreachable, // IntDiv should not be emitted as floating-point
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
                                else => {},
                            }
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

                    for (raw_args.items, 0..) |*arg_ptr, idx| {
                        var arg = arg_ptr.*;
                        const llvm_ty = blk: {
                            if (func_info) |info| {
                                if (idx < info.param_types.len) {
                                    const is_alias = if (idx < info.param_is_alias.len) info.param_is_alias[idx] else false;
                                    if (!is_alias and arg.ty == .Value and info.param_types[idx] != .Union) {
                                        arg = try self.unwrapDoxaValueToType(w, arg, info.param_types[idx], &id);
                                        arg_ptr.* = arg;
                                    }
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

                    var actual_return_type: HIR.HIRType = c.return_type;
                    if (func_info) |info| {
                        if (actual_return_type == .Nothing) {
                            actual_return_type = info.return_type;
                        } else if ((actual_return_type == .Unknown or actual_return_type == .String) and (info.return_type == .Array or info.return_type == .Map)) {
                            actual_return_type = info.return_type;
                        }
                    }

                    if (actual_return_type != .Nothing) {
                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
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
                        }
                        try stack.append(pushed);
                    } else {
                        const runtime_name = IRPrinter.mapBuiltinToRuntime(c.qualified_name);
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}({s})\n", .{ runtime_name, args_str });
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                    last_instruction_was_terminator = false;
                },
                .StringOp => |sop| {
                    if (sop.op == .Concat) {
                        if (stack.items.len < 2) continue;
                        const a = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const b = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;

                        const a_ptr = try self.ensurePointer(w, a, &id);
                        const b_ptr = try self.ensurePointer(w, b, &id);

                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                        id += 1;
                        const call_line = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = call ptr @doxa_str_concat(ptr {s}, ptr {s})\n",
                            .{ result_name, a_ptr.name, b_ptr.name },
                        );
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                        try stack.append(.{ .name = result_name, .ty = .PTR });
                        last_instruction_was_terminator = false;
                        continue;
                    }

                    if (stack.items.len < 1) continue;
                    const arg = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    switch (sop.op) {
                        .Length => {
                            const ptr_name = blk: {
                                if (arg.ty == .PTR) break :blk arg.name;
                                const arg_i64 = if (arg.ty == .I64) arg else try self.ensureI64(w, arg, &id);
                                const tmp_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp_ptr, arg_i64.name });
                                defer self.allocator.free(cast_line);
                                try w.writeAll(cast_line);
                                break :blk tmp_ptr;
                            };

                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_str_len(ptr {s})\n", .{ result_name, ptr_name });
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                            try stack.append(.{ .name = result_name, .ty = .I64 });
                        },
                        .ToInt => {
                            // Convert to double first, then to int using @doxa_int
                            var arg_double = arg;
                            if (arg.ty != .F64) {
                                // Convert to i64 first if needed
                                const arg_i64 = if (arg.ty != .I64) try self.ensureI64(w, arg, &id) else arg;
                                // Then convert i64 to double
                                const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, arg_i64.name });
                                defer self.allocator.free(conv_line);
                                try w.writeAll(conv_line);
                                arg_double = .{ .name = conv_name, .ty = .F64 };
                            }
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int(double {s})\n", .{ result_name, arg_double.name });
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
                    var value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    if (sv.expected_type == .Union) {
                        value = try self.buildDoxaValue(w, value, sv.expected_type, &id);
                    }
                    var info_ptr = variables.getPtr(sv.var_name);
                    if (info_ptr == null) {
                        const inferred_ty: StackType = if (sv.expected_type != .Unknown) self.hirTypeToStackType(sv.expected_type) else value.ty;
                        value = try self.coerceForStore(value, inferred_ty, &id, w);
                        const llvm_ty = self.stackTypeToLLVMType(inferred_ty);
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sv.var_name});
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ ptr_name, llvm_ty });
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = inferred_ty, .array_type = value.array_type };
                        try variables.put(sv.var_name, info);
                        info_ptr = variables.getPtr(sv.var_name);
                    } else {
                        if (info_ptr.?.array_type == null) info_ptr.?.array_type = value.array_type;
                        const target_ty = info_ptr.?.stack_type;
                        value = try self.coerceForStore(value, target_ty, &id, w);
                    }
                    const store_llvm_ty = self.stackTypeToLLVMType(info_ptr.?.stack_type);
                    const store_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  store {s} {s}, ptr {s}\n",
                        .{ store_llvm_ty, value.name, info_ptr.?.ptr_name },
                    );
                    defer self.allocator.free(store_line);
                    try w.writeAll(store_line);
                    last_instruction_was_terminator = false;
                },
                .StoreDecl => |sd| {
                    if (stack.items.len < 1) continue;
                    var value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    if (sd.declared_type == .Union) {
                        value = try self.buildDoxaValue(w, value, sd.declared_type, &id);
                    }
                    const declared_stack_type = self.hirTypeToStackType(sd.declared_type);
                    const target_ty: StackType = if (sd.declared_type == .Unknown) value.ty else declared_stack_type;
                    value = try self.coerceForStore(value, target_ty, &id, w);
                    const llvm_ty = self.stackTypeToLLVMType(target_ty);
                    var info_ptr = variables.getPtr(sd.var_name);
                    if (info_ptr == null) {
                        const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}", .{sd.var_name});
                        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ ptr_name, llvm_ty });
                        defer self.allocator.free(alloca_line);
                        try w.writeAll(alloca_line);
                        const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = target_ty, .array_type = value.array_type };
                        try variables.put(sd.var_name, info);
                        info_ptr = variables.getPtr(sd.var_name);
                    } else {
                        if (sd.declared_type == .Unknown) info_ptr.?.stack_type = target_ty;
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
                    // Consts do not carry expected type; leave as-is for now.
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
                .TypeCheck => |tc| {
                    if (stack.items.len < 1) continue;
                    const value = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    // Compute tag and payload bits; handle canonical %DoxaValue values specially.
                    var value_i64 = StackVal{ .name = "", .ty = .I64 };
                    var tag_reg: []const u8 = undefined;
                    if (value.ty == .Value) {
                        const tag_i32 = try self.nextTemp(&id);
                        const tag_extract = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = extractvalue %DoxaValue {s}, 0\n",
                            .{ tag_i32, value.name },
                        );
                        defer self.allocator.free(tag_extract);
                        try w.writeAll(tag_extract);

                        const tag_i64 = try self.nextTemp(&id);
                        const tag_zext = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = zext i32 {s} to i64\n",
                            .{ tag_i64, tag_i32 },
                        );
                        defer self.allocator.free(tag_zext);
                        try w.writeAll(tag_zext);
                        tag_reg = tag_i64;

                        const payload = try self.nextTemp(&id);
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
                            .I2 => 7, // tetra - not used in type checking
                            .I1 => 7, // bool - not used in type checking
                            .Nothing => 8,
                            .Value => 8,
                        };

                        const tag_tmp = try self.nextTemp(&id);
                        const tag_line2 = try std.fmt.allocPrint(
                            self.allocator,
                            "  {s} = add i64 0, {d}\n",
                            .{ tag_tmp, value_type_tag },
                        );
                        defer self.allocator.free(tag_line2);
                        try w.writeAll(tag_line2);
                        tag_reg = tag_tmp;

                        // Convert value to i64 payload bits for DoxaValue.
                        value_i64 = try self.ensureI64(w, value, &id);
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
                    const target_type_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const target_gep = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                        .{ target_type_ptr, target_type_info.length, target_type_info.name },
                    );
                    defer self.allocator.free(target_gep);
                    try w.writeAll(target_gep);

                    // Call runtime type check function
                    // Allocate type_tag_name first so it gets a lower instruction number
                    const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const call_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = call i64 @doxa_type_check(i64 {s}, i64 {s}, ptr {s})\n",
                        .{ result_name, value_i64.name, tag_reg, target_type_ptr },
                    );
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);

                    // Convert i64 result to i2 (tetra) for boolean result
                    const tetra_result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                    id += 1;
                    const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ tetra_result, result_name });
                    defer self.allocator.free(trunc_line);
                    try w.writeAll(trunc_line);

                    try stack.append(.{ .name = tetra_result, .ty = .I2 });
                    last_instruction_was_terminator = false;
                },
                .LogicalOp => |lop| {
                    if (stack.items.len < 2) continue;
                    const rhs = stack.items[stack.items.len - 1];
                    const lhs = stack.items[stack.items.len - 2];
                    stack.items.len -= 2;

                    // Match the main logical-op emission: if both operands are tetra (i2), use LUTs;
                    // otherwise, coerce to i1 and emit boolean ops.
                    if (lhs.ty == .I2 and rhs.ty == .I2) {
                        // Tetra operations - use LUTs
                        const lhs_i64 = try self.nextTemp(&id);
                        const lhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ lhs_i64, lhs.name });
                        defer self.allocator.free(lhs_zext);
                        try w.writeAll(lhs_zext);

                        const rhs_i64 = try self.nextTemp(&id);
                        const rhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ rhs_i64, rhs.name });
                        defer self.allocator.free(rhs_zext);
                        try w.writeAll(rhs_zext);

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

                        const row_ptr = try self.nextTemp(&id);
                        const row_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x [4 x i8]], ptr {s}, i64 0, i64 {s}\n", .{ row_ptr, lut_name, lhs_i64 });
                        defer self.allocator.free(row_gep);
                        try w.writeAll(row_gep);

                        const elem_ptr = try self.nextTemp(&id);
                        const elem_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr {s}, i64 0, i64 {s}\n", .{ elem_ptr, row_ptr, rhs_i64 });
                        defer self.allocator.free(elem_gep);
                        try w.writeAll(elem_gep);

                        const result_i8 = try self.nextTemp(&id);
                        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, elem_ptr });
                        defer self.allocator.free(load_line);
                        try w.writeAll(load_line);

                        const result_i2 = try self.nextTemp(&id);
                        const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                        defer self.allocator.free(trunc_line);
                        try w.writeAll(trunc_line);

                        try stack.append(.{ .name = result_i2, .ty = .I2 });
                    } else {
                        const lhs_bool = try self.ensureBool(w, lhs, &id);
                        const rhs_bool = try self.ensureBool(w, rhs, &id);

                        const result = try self.nextTemp(&id);
                        const op_str = switch (lop.op) {
                            .And => "and",
                            .Or => "or",
                            .Xor => "xor",
                            else => blk: {
                                // For complex boolean operations, fall back to AND of truthiness.
                                break :blk "and";
                            },
                        };

                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = {s} i1 {s}, {s}\n", .{ result, op_str, lhs_bool.name, rhs_bool.name });
                        defer self.allocator.free(line);
                        try w.writeAll(line);

                        try stack.append(.{ .name = result, .ty = .I1 });
                    }
                    last_instruction_was_terminator = false;
                },
                .Peek => |pk| {
                    if (stack.items.len < 1) continue;
                    var v = stack.items[stack.items.len - 1];
                    self.hydrateStructMetadata(&v, pk.name);
                    stack.items[stack.items.len - 1] = v;

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
                        .Value => {
                            const tmp_ptr = try self.nextTemp(&id);
                            const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                            defer self.allocator.free(alloca_line);
                            try w.writeAll(alloca_line);
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ v.name, tmp_ptr });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        },
                        .PTR => {
                            if (v.struct_field_types) |fts| {
                                _ = fts;
                                const dv = try self.buildDoxaValue(w, v, null, &id);
                                const tmp_ptr = try self.nextTemp(&id);
                                const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                                defer self.allocator.free(alloca_line);
                                try w.writeAll(alloca_line);
                                const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ dv.name, tmp_ptr });
                                defer self.allocator.free(store_line);
                                try w.writeAll(store_line);
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            } else if (v.array_type) |_| {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            } else {
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s})\n", .{v.name});
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                            }
                        },
                        else => {},
                    }
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                    last_instruction_was_terminator = false;
                },
                .PushStorageId => |psid| {
                    // PushStorageId pushes a pointer to the variable, not the value
                    // This is used for alias parameters in function calls
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

                        // IMPORTANT: PushStorageId must always push the *storage address* (the variable's
                        // location), even if the variable itself stores a pointer (strings/arrays/structs).
                        // Alias parameters expect an address they can load/store through; loading here would
                        // pass the pointee value and break aliasing.
                        const struct_fields = self.global_struct_field_types.get(gname);
                        // Note: We intentionally don't free gptr here because it can be referenced later.
                        try stack.append(.{ .name = gptr, .ty = .PTR, .struct_field_types = struct_fields });
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
                .LoadAlias => |la| {
                    if (alias_slots.get(la.slot_index)) |info| {
                        const stack_ty = self.hirTypeToStackType(info.pointee_type);
                        if (stack_ty == .PTR) {
                            // Alias slots store the *address of the variable*, so for pointer-like
                            // values (strings/arrays/maps/structs) we must load the pointer value.
                            const loaded_ptr = try self.nextTemp(&id);
                            const load_line = try std.fmt.allocPrint(
                                self.allocator,
                                "  {s} = load ptr, ptr {s}\n",
                                .{ loaded_ptr, info.ptr_name },
                            );
                            defer self.allocator.free(load_line);
                            try w.writeAll(load_line);
                            try stack.append(.{
                                .name = loaded_ptr,
                                .ty = .PTR,
                                .array_type = info.array_type,
                                .struct_field_types = info.struct_field_types,
                                .struct_field_names = info.struct_field_names,
                                .struct_type_name = info.struct_type_name,
                            });
                        } else {
                            const result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const llvm_ty = self.hirTypeToLLVMType(info.pointee_type, false);
                            const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result, llvm_ty, info.ptr_name });
                            defer self.allocator.free(load_line);
                            try w.writeAll(load_line);
                            try stack.append(.{
                                .name = result,
                                .ty = stack_ty,
                                .array_type = info.array_type,
                                .struct_field_types = info.struct_field_types,
                                .struct_field_names = info.struct_field_names,
                                .struct_type_name = info.struct_type_name,
                            });
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
                    var struct_fields: ?[]HIR.HIRType = ptr_val.struct_field_types;
                    if (struct_fields == null and spa.param_type == .Struct) {
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
                        .struct_field_names = ptr_val.struct_field_names,
                        .struct_type_name = ptr_val.struct_type_name,
                        .enum_type_name = ptr_val.enum_type_name,
                    };
                    try alias_slots.put(spa.var_index, alias_info);
                    last_instruction_was_terminator = false;
                },
                .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                .StoreFieldName => |_| {
                    // StoreFieldName is a no-op in LLVM IR generation
                    // It's used by the VM to track field names for PeekStruct, but in LLVM
                    // we don't need to generate code for it. However, if there's a field name
                    // string on the stack (from the VM's perspective), we should pop it to avoid
                    // corrupting the stack.
                    // In practice, StoreFieldName doesn't push anything, so this is a no-op.
                    last_instruction_was_terminator = false;
                },
                .EnterScope => |_| {
                    // EnterScope is a no-op in LLVM IR generation
                    // It's used for scope tracking but doesn't generate LLVM IR
                    last_instruction_was_terminator = false;
                },
                .ExitScope => |_| {
                    // ExitScope is a no-op in LLVM IR generation
                    // It's used for scope tracking but doesn't generate LLVM IR
                    last_instruction_was_terminator = false;
                },
                .PeekStruct => |ps| {
                    // PeekStruct peeks a struct without popping it
                    // Similar to Peek, but for structs
                    if (stack.items.len < 1) continue;
                    const v = stack.items[stack.items.len - 1];

                    // Emit peek debug info (type name and variable name prefix)
                    try self.emitPeekInstruction(w, .{
                        .name = null,
                        .value_type = .Struct,
                        .location = ps.location,
                        .union_members = null,
                        .enum_type_name = null,
                    }, v, &id, peek_state);

                    const enum_names = try self.allocator.alloc(?[]const u8, ps.field_names.len);
                    defer self.allocator.free(enum_names);
                    @memset(enum_names, null);
                    const desc_global = try self.getOrCreateStructDescGlobal(peek_state, ps.type_name, ps.field_names, ps.field_types, enum_names);
                    const reg_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_struct_register(ptr {s}, ptr {s})\n", .{ v.name, desc_global });
                    defer self.allocator.free(reg_line);
                    try w.writeAll(reg_line);

                    var print_v = v;
                    print_v.struct_type_name = ps.type_name;
                    const dv = try self.buildDoxaValue(w, print_v, null, &id);
                    const tmp_ptr = try self.nextTemp(&id);
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
                    try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");

                    // Don't pop or push - struct remains on stack
                    last_instruction_was_terminator = false;
                },
                else => {},
            }
        }

        try w.writeAll("}\n\n");
    }

    pub fn nextTemp(self: *IRPrinter, id: *usize) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        return name;
    }

    pub fn nextTempText(self: *IRPrinter, id: *usize) ![]const u8 {
        const name = try std.fmt.allocPrint(self.allocator, "%t{d}", .{id.*});
        id.* += 1;
        return name;
    }

    /// Collect enum variant metadata from the constant pool so we can render
    /// enums by name in native code without needing a dynamic registry at
    /// runtime.
    pub fn buildEnumPrintMap(self: *IRPrinter, hir: *const HIR.HIRProgram) !void {
        const registerVariant = struct {
            fn add(printer: *IRPrinter, type_name: []const u8, variant_index: u32, variant_name: []const u8) !void {
                var entry = try printer.enum_print_map.getOrPut(type_name);
                if (!entry.found_existing) {
                    entry.value_ptr.* = std.ArrayListUnmanaged(EnumVariantMeta){};
                }

                for (entry.value_ptr.items) |existing| {
                    if (existing.index == variant_index and std.mem.eql(u8, existing.name, variant_name)) {
                        return;
                    }
                }

                try entry.value_ptr.append(printer.allocator, .{
                    .index = variant_index,
                    .name = variant_name,
                });
            }
        };

        for (hir.constant_pool) |hv| {
            if (hv == .enum_variant) {
                const ev = hv.enum_variant;
                try registerVariant.add(self, ev.type_name, ev.variant_index, ev.variant_name);
            }
        }

        for (hir.instructions) |inst| {
            if (inst == .EnumNew) {
                const ev = inst.EnumNew;
                try registerVariant.add(self, ev.enum_name, ev.variant_index, ev.variant_name);
            }
        }
    }

    pub fn emitEnumPrint(
        self: *IRPrinter,
        peek_state: *PeekEmitState,
        w: anytype,
        id: *usize,
        type_name: []const u8,
        value_name: []const u8,
    ) !void {
        // Look up known variants for this enum type (derived from constant pool).
        const meta_opt = self.enum_print_map.get(type_name);

        if (meta_opt) |meta_list| {
            const variants = meta_list.items;

            // Default label for unknown enum values.
            const unknown_info = try internPeekString(
                self.allocator,
                &peek_state.*.string_map,
                &peek_state.*.strings,
                peek_state.*.next_id_ptr,
                &peek_state.*.globals,
                ".Unknown",
            );
            const unknown_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            const unknown_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                .{ unknown_ptr, unknown_info.length, unknown_info.name },
            );
            defer self.allocator.free(unknown_gep);
            try w.writeAll(unknown_gep);

            var current_ptr = unknown_ptr;

            // Build a chain of selects that chooses the right variant label
            // based on the integer discriminant.
            for (variants) |variant_meta| {
                // Build ".VariantName" string for printing.
                const dotted_name = try std.fmt.allocPrint(self.allocator, ".{s}", .{variant_meta.name});
                defer self.allocator.free(dotted_name);

                const v_info = try internPeekString(
                    self.allocator,
                    &peek_state.*.string_map,
                    &peek_state.*.strings,
                    peek_state.*.next_id_ptr,
                    &peek_state.*.globals,
                    dotted_name,
                );

                const v_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                id.* += 1;
                const v_gep = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                    .{ v_ptr, v_info.length, v_info.name },
                );
                defer self.allocator.free(v_gep);
                try w.writeAll(v_gep);

                const cmp_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                id.* += 1;
                const cmp_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = icmp eq i64 {s}, {d}\n",
                    .{ cmp_name, value_name, variant_meta.index },
                );
                defer self.allocator.free(cmp_line);
                try w.writeAll(cmp_line);

                const sel_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                id.* += 1;
                const sel_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = select i1 {s}, ptr {s}, ptr {s}\n",
                    .{ sel_name, cmp_name, v_ptr, current_ptr },
                );
                defer self.allocator.free(sel_line);
                try w.writeAll(sel_line);

                current_ptr = sel_name;
            }

            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{current_ptr});
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
        }
    }

    pub fn emitQuantifierWrappers(self: *IRPrinter, w: anytype) !void {
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

    };
}
