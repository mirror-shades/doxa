const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const PeekEmitState = Ctx.PeekEmitState;
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
                            var struct_field_types: ?[]HIR.HIRType = null;
                            var struct_field_names: ?[]const []const u8 = null;
                            var struct_type_name: ?[]const u8 = null;
                            if (sd.declared_type == .Struct) {
                                const sid = sd.declared_type.Struct;
                                struct_field_types = self.struct_fields_by_id.get(sid);
                                struct_type_name = self.struct_type_names_by_id.get(sid);
                                if (struct_type_name) |tn| {
                                    struct_field_names = self.struct_field_names_by_type.get(tn);
                                }
                            }
                            const info = VariableInfo{
                                .ptr_name = ptr_name,
                                .stack_type = declared_stack_type,
                                .array_type = array_hint,
                                .struct_field_types = struct_field_types,
                                .struct_field_names = struct_field_names,
                                .struct_type_name = struct_type_name,
                            };
                            try variables_to_allocate.put(sd.var_name, info);
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
                                try stack.append(.{ .name = "0", .ty = .Nothing });
                                last_instruction_was_terminator = false;
                            },
                            else => {},
                        }
                    },
                    .Return => |ret| {
                        if (target_return_stack_type == .Nothing) {
                            if (ret.has_value and stack.items.len > 0) {
                                stack.items.len -= 1;
                            }
                            try w.writeAll("  ret void\n");
                            last_instruction_was_terminator = true;
                            continue;
                        }
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
                            if (std.mem.indexOf(u8, return_type_str, "%DoxaValue") != null) {
                                const zero = try self.nextTemp(&id);
                                const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} undef, i32 0, 0\n", .{ zero, return_type_str });
                                defer self.allocator.free(zero_line);
                                try w.writeAll(zero_line);
                                const zero2 = try self.nextTemp(&id);
                                const zero2_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i32 0, 1\n", .{ zero2, return_type_str, zero });
                                defer self.allocator.free(zero2_line);
                                try w.writeAll(zero2_line);
                                const zero3 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const zero3_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i64 0, 2\n", .{ zero3, return_type_str, zero2 });
                                defer self.allocator.free(zero3_line);
                                try w.writeAll(zero3_line);
                                const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, zero3 });
                                defer self.allocator.free(ret_line);
                                try w.writeAll(ret_line);
                            } else {
                                const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} zeroinitializer\n", .{return_type_str});
                                defer self.allocator.free(ret_line);
                                try w.writeAll(ret_line);
                            }
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
                    .LoadModule => |lm| {
                        const gname = lm.module_name;
                        const st = self.global_types.get(gname) orelse .PTR;

                        // Ensure the global is declared
                        if (!self.defined_globals.contains(gname)) {
                            _ = try self.global_types.put(gname, st);
                            _ = try self.defined_globals.put(gname, true);
                        }

                        const gptr = try self.mangleGlobalName(gname);
                        const struct_fields = self.global_struct_field_types.get(gname);
                        const struct_names = self.global_struct_field_names.get(gname);
                        const struct_type_name = self.global_struct_type_names.get(gname);
                        try stack.append(.{ .name = gptr, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
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
                            try stack.append(.{
                                .name = result_name,
                                .ty = entry.stack_type,
                                .array_type = entry.array_type,
                                .enum_type_name = entry.enum_type_name,
                                .struct_field_types = entry.struct_field_types,
                                .struct_field_names = entry.struct_field_names,
                                .struct_type_name = entry.struct_type_name,
                            });
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
                    .Peek => |pk| {
                        if (stack.items.len < 1) continue;
                        var v = stack.items[stack.items.len - 1];
                        self.hydrateStructMetadata(&v, pk.name);
                        stack.items[stack.items.len - 1] = v;

                        try self.emitPeekInstruction(w, pk, v, &id, peek_state);

                        if (v.ty == .Nothing or pk.value_type == .Nothing) {
                            const nothing_info = try internPeekString(
                                self.allocator,
                                &peek_state.*.string_map,
                                &peek_state.*.strings,
                                peek_state.*.next_id_ptr,
                                &peek_state.*.globals,
                                "nothing",
                            );
                            const nothing_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;
                            const nothing_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n", .{ nothing_ptr, nothing_info.length, nothing_info.name });
                            defer self.allocator.free(nothing_gep);
                            try w.writeAll(nothing_gep);
                            const nothing_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{nothing_ptr});
                            defer self.allocator.free(nothing_call);
                            try w.writeAll(nothing_call);
                            try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                            last_instruction_was_terminator = false;
                            continue;
                        }

                        // Print the actual value
                        var handled_union_enum = false;
                        if (v.ty == .Value and pk.value_type == .Union and pk.union_members != null) {
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
                                const reserved = try self.nextTemp(&id);
                                const reserved_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 1\n", .{ reserved, v.name });
                                defer self.allocator.free(reserved_line);
                                try w.writeAll(reserved_line);

                                const active_member = try self.nextTemp(&id);
                                const active_member_line = try std.fmt.allocPrint(self.allocator, "  {s} = and i32 {s}, 65535\n", .{ active_member, reserved });
                                defer self.allocator.free(active_member_line);
                                try w.writeAll(active_member_line);

                                const enum_idx_i32: i32 = @intCast(enum_idx);
                                const is_enum_member = try self.nextTemp(&id);
                                const is_enum_member_line = try std.fmt.allocPrint(self.allocator, "  {s} = icmp eq i32 {s}, {d}\n", .{ is_enum_member, active_member, enum_idx_i32 });
                                defer self.allocator.free(is_enum_member_line);
                                try w.writeAll(is_enum_member_line);

                                const enum_label = try std.fmt.allocPrint(self.allocator, "peek_union_enum_{d}", .{id});
                                id += 1;
                                defer self.allocator.free(enum_label);
                                const fallback_label = try std.fmt.allocPrint(self.allocator, "peek_union_fallback_{d}", .{id});
                                id += 1;
                                defer self.allocator.free(fallback_label);
                                const merge_label = try std.fmt.allocPrint(self.allocator, "peek_union_merge_{d}", .{id});
                                id += 1;
                                defer self.allocator.free(merge_label);

                                const branch_line = try std.fmt.allocPrint(self.allocator, "  br i1 {s}, label %{s}, label %{s}\n", .{ is_enum_member, enum_label, fallback_label });
                                defer self.allocator.free(branch_line);
                                try w.writeAll(branch_line);

                                const enum_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{enum_label});
                                defer self.allocator.free(enum_label_line);
                                try w.writeAll(enum_label_line);

                                const payload_bits = try self.nextTemp(&id);
                                const payload_bits_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload_bits, v.name });
                                defer self.allocator.free(payload_bits_line);
                                try w.writeAll(payload_bits_line);

                                try self.emitEnumPrint(peek_state, w, &id, enum_member_name.?, payload_bits);

                                const enum_br_merge_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{merge_label});
                                defer self.allocator.free(enum_br_merge_line);
                                try w.writeAll(enum_br_merge_line);

                                const fallback_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{fallback_label});
                                defer self.allocator.free(fallback_label_line);
                                try w.writeAll(fallback_label_line);

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

                                const fallback_br_merge_line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{merge_label});
                                defer self.allocator.free(fallback_br_merge_line);
                                try w.writeAll(fallback_br_merge_line);

                                const merge_label_line = try std.fmt.allocPrint(self.allocator, "{s}:\n", .{merge_label});
                                defer self.allocator.free(merge_label_line);
                                try w.writeAll(merge_label_line);

                                handled_union_enum = true;
                            }
                        }

                        if (!handled_union_enum) switch (v.ty) {
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
                        };
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
                    .BindAlias => |ba| {
                        if (stack.items.len < 1) continue;
                        const ptr_val = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        var struct_fields: ?[]HIR.HIRType = ptr_val.struct_field_types;
                        if (struct_fields == null and ba.target_type == .Struct) {
                            if (std.mem.eql(u8, ba.alias_name, "this")) {
                                if (std.mem.indexOfScalar(u8, func.qualified_name, '.')) |dot_idx| {
                                    const struct_name = func.qualified_name[0..dot_idx];
                                    if (self.global_struct_field_types.get(struct_name)) |fts| {
                                        struct_fields = fts;
                                    }
                                }
                            } else if (self.global_struct_field_types.get(ba.alias_name)) |fts| {
                                struct_fields = fts;
                            }
                        }
                        const array_hint: ?HIR.HIRType = switch (ba.target_type) {
                            .Array => |inner| inner.*,
                            else => null,
                        };
                        const alias_info = AliasInfo{
                            .ptr_name = ptr_val.name,
                            .pointee_type = ba.target_type,
                            .array_type = array_hint,
                            .struct_field_types = struct_fields,
                            .struct_field_names = ptr_val.struct_field_names,
                            .struct_type_name = ptr_val.struct_type_name,
                            .enum_type_name = ptr_val.enum_type_name,
                        };
                        try alias_slots.put(ba.alias_slot, alias_info);
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

            if (!last_instruction_was_terminator) {
                if (target_return_stack_type == .Nothing) {
                    try w.writeAll("  ret void\n");
                } else if (std.mem.indexOf(u8, return_type_str, "%DoxaValue") != null) {
                    const zero = try self.nextTemp(&id);
                    const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} undef, i32 0, 0\n", .{ zero, return_type_str });
                    defer self.allocator.free(zero_line);
                    try w.writeAll(zero_line);
                    const zero2 = try self.nextTemp(&id);
                    const zero2_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i32 0, 1\n", .{ zero2, return_type_str, zero });
                    defer self.allocator.free(zero2_line);
                    try w.writeAll(zero2_line);
                    const zero3 = try self.nextTemp(&id);
                    const zero3_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i64 0, 2\n", .{ zero3, return_type_str, zero2 });
                    defer self.allocator.free(zero3_line);
                    try w.writeAll(zero3_line);
                    const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, zero3 });
                    defer self.allocator.free(ret_line);
                    try w.writeAll(ret_line);
                } else {
                    const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} zeroinitializer\n", .{return_type_str});
                    defer self.allocator.free(ret_line);
                    try w.writeAll(ret_line);
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
                const header = try std.fmt.allocPrint(self.allocator, "define i2 @{s}(ptr %hdr, ptr %value) {{\n", .{wrap.name});
                defer self.allocator.free(header);
                try w.writeAll(header);
                try w.writeAll("entry:\n");
                try w.writeAll("  %value_bits = ptrtoint ptr %value to i64\n");
                const call_line = try std.fmt.allocPrint(self.allocator, "  %res = call i8 @{s}(ptr %hdr, i64 %value_bits)\n", .{wrap.runtime});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
                try w.writeAll("  %cast = trunc i8 %res to i2\n");
                try w.writeAll("  ret i2 %cast\n}\n\n");
            }
        }
    };
}
