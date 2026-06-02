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
                        try self.handleConst(w, &stack, &id, peek_state, hir.constant_pool, c.constant_id);
                        last_instruction_was_terminator = false;
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
                        const gptr = try self.mangleGlobalName(gname);
                        defer self.allocator.free(gptr);

                        // Load the struct pointer that was stored during global init
                        const loaded = try self.nextTemp(&id);
                        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ loaded, gptr });
                        defer self.allocator.free(load_line);
                        try w.writeAll(load_line);

                        const struct_fields = self.global_struct_field_types.get(gname);
                        const struct_names = self.global_struct_field_names.get(gname);
                        const struct_type_name = self.global_struct_type_names.get(gname);
                        try stack.append(.{ .name = loaded, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
                        last_instruction_was_terminator = false;
                    },
                    .LoadVar => |lv| {
                        if (lv.scope_kind == .GlobalLocal) {
                            try self.handleLoadVarGlobal(w, &stack, &id, lv.var_name);
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
                        try self.handlePeek(w, &stack, &id, pk, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .PushStorageId => |psid| {
                        if (psid.scope_kind == .GlobalLocal) {
                            try self.handlePushStorageIdGlobal(w, &stack, &id, psid.var_name);
                        } else if (variables.get(psid.var_name)) |entry| {
                            try stack.append(.{ .name = entry.ptr_name, .ty = .PTR, .array_type = entry.array_type, .enum_type_name = entry.enum_type_name, .struct_field_types = entry.struct_field_types, .struct_field_names = entry.struct_field_names, .struct_type_name = entry.struct_type_name });
                        } else {
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
                                    .enum_type_name = info.enum_type_name,
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
                                    .enum_type_name = info.enum_type_name,
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
                    .ArrayNew => |a| try self.emitArrayNew(w, &stack, &id, a),
                    .ArraySet => |_| try self.emitArraySet(w, &stack, &id),
                    .ArrayGet => |_| try self.emitArrayGet(w, &stack, &id),
                    .ArrayCompoundAssign => |a| try self.emitArrayGetAndArith(w, &stack, &id, a.op),
                    .ArrayLen => try self.emitArrayLen(w, &stack, &id),
                    .ArrayPush => |_| try self.emitArrayPush(w, &stack, &id),
                    .ArrayPop => try self.emitArrayPop(w, &stack, &id),
                    .ArrayInsert => try self.emitArrayInsert(w, &stack, &id),
                    .ArrayRemove => try self.emitArrayRemove(w, &stack, &id),
                    .ArraySlice => try self.emitArraySlice(w, &stack, &id),
                    .Map => |m| try self.emitMap(w, &stack, &id, m),
                    .MapGet => |mg| try self.emitMapGet(w, &stack, &id, mg),
                    .MapSet => |ms| try self.emitMapSet(w, &stack, &id, ms),
                    .Dup => {
                        try self.handleDup(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Swap => {
                        self.handleSwap(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .StoreFieldName => |_| {
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
                        try self.handlePeekStruct(w, &stack, &id, ps, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .Pop => {
                        self.handlePop(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .StoreDecl => |sd| {
                        const store_decl_is_global = switch (sd.scope_kind) {
                            .GlobalLocal, .ModuleGlobal => true,
                            else => false,
                        };
                        if (store_decl_is_global) {
                            try self.handleStoreDeclGlobal(w, &stack, &id, sd);
                        } else {
                            if (stack.items.len < 1) {
                                if (sd.declared_type == .Array or sd.declared_type == .Map) {
                                    const info_ptr = variables.getPtr(sd.var_name) orelse continue;
                                    switch (sd.declared_type) {
                                        .Array => |inner| {
                                            const elem_type = inner.*;
                                            const elem_size = self.arrayElementSize(elem_type);
                                            const elem_tag = self.arrayElementTag(elem_type);
                                            const reg = try self.nextTemp(&id);
                                            const new_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_new(i64 {d}, i64 {d}, i64 0)\n", .{ reg, elem_size, elem_tag });
                                            defer self.allocator.free(new_line);
                                            try w.writeAll(new_line);
                                            const store_line = try std.fmt.allocPrint(self.allocator, "  store ptr {s}, ptr {s}\n", .{ reg, info_ptr.ptr_name });
                                            defer self.allocator.free(store_line);
                                            try w.writeAll(store_line);
                                            info_ptr.stack_type = .PTR;
                                            info_ptr.array_type = elem_type;
                                        },
                                        .Map => |kv| {
                                            const key_tag = self.arrayElementTag(kv.key.*);
                                            const val_tag = self.arrayElementTag(kv.value.*);
                                            const reg = try self.nextTemp(&id);
                                            const new_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_map_new(i64 0, i64 {d}, i64 {d})\n", .{ reg, key_tag, val_tag });
                                            defer self.allocator.free(new_line);
                                            try w.writeAll(new_line);
                                            const store_line = try std.fmt.allocPrint(self.allocator, "  store ptr {s}, ptr {s}\n", .{ reg, info_ptr.ptr_name });
                                            defer self.allocator.free(store_line);
                                            try w.writeAll(store_line);
                                            info_ptr.stack_type = .PTR;
                                            info_ptr.array_type = kv.value.*;
                                        },
                                        else => {},
                                    }
                                }
                                continue;
                            }
                            var value = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                            if (sd.declared_type == .Array and value.array_type == null) {
                                value.array_type = sd.declared_type.Array.*;
                            }
                            if (sd.declared_type == .Union) {
                                value = try self.buildDoxaValue(w, value, sd.declared_type, &id);
                            }
                            if (!sd.is_const and sd.declared_type == .Array) {
                                const src_ptr = if (value.ty == .PTR) value else try self.ensurePointer(w, value, &id);
                                const clone_reg = try self.nextTemp(&id);
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
                            value = try self.coerceForStore(value, target_ty, &id, w);
                            var info_ptr = variables.getPtr(sd.var_name);
                            if (info_ptr == null) {
                                continue;
                            } else {
                                if (sd.declared_type != .Unknown) {
                                    const expected_stack_ty = self.hirTypeToStackType(sd.declared_type);
                                    if (info_ptr.?.stack_type == .I64 and expected_stack_ty != .I64) {
                                        info_ptr.?.stack_type = expected_stack_ty;
                                    }
                                }
                                if (info_ptr.?.array_type == null) info_ptr.?.array_type = value.array_type;
                                if (info_ptr.?.enum_type_name == null) info_ptr.?.enum_type_name = value.enum_type_name;
                                if (info_ptr.?.struct_field_types == null) info_ptr.?.struct_field_types = value.struct_field_types;
                                if (info_ptr.?.struct_field_names == null) info_ptr.?.struct_field_names = value.struct_field_names;
                                if (info_ptr.?.struct_type_name == null) info_ptr.?.struct_type_name = value.struct_type_name;
                            }
                            const target_local_ty = info_ptr.?.stack_type;
                            value = try self.coerceForStore(value, target_local_ty, &id, w);
                            const target_llvm_ty = self.stackTypeToLLVMType(target_local_ty);
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ target_llvm_ty, value.name, info_ptr.?.ptr_name });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                        }
                        last_instruction_was_terminator = false;
                    },
                    .StoreVar => |sv| {
                        if (sv.scope_kind == .GlobalLocal) {
                            try self.handleStoreVarGlobal(w, &stack, &id, sv);
                        } else {
                            if (stack.items.len < 1) continue;
                            var value = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                            if (value.ty == .Nothing) continue;
                            const expected_array_type: ?HIR.HIRType = switch (sv.expected_type) {
                                .Array => |inner| inner.*,
                                else => null,
                            };
                            if (value.array_type == null and expected_array_type != null) {
                                value.array_type = expected_array_type.?;
                            }
                            if (sv.expected_type == .Union) {
                                value = try self.buildDoxaValue(w, value, sv.expected_type, &id);
                            }
                            var info_ptr = variables.getPtr(sv.var_name);
                            if (info_ptr == null) {
                                continue;
                            } else {
                                if (sv.expected_type != .Unknown) {
                                    const expected_stack_ty = self.hirTypeToStackType(sv.expected_type);
                                    if (info_ptr.?.stack_type == .I64 and expected_stack_ty != .I64) {
                                        info_ptr.?.stack_type = expected_stack_ty;
                                    }
                                }
                                if (info_ptr.?.array_type == null) info_ptr.?.array_type = value.array_type;
                                if (info_ptr.?.array_type == null and expected_array_type != null) info_ptr.?.array_type = expected_array_type.?;
                                if (info_ptr.?.enum_type_name == null) info_ptr.?.enum_type_name = value.enum_type_name;
                                if (info_ptr.?.struct_field_types == null) info_ptr.?.struct_field_types = value.struct_field_types;
                                if (info_ptr.?.struct_field_names == null) info_ptr.?.struct_field_names = value.struct_field_names;
                                if (info_ptr.?.struct_type_name == null) info_ptr.?.struct_type_name = value.struct_type_name;
                            }
                            const target_ty = info_ptr.?.stack_type;
                            value = try self.coerceForStore(value, target_ty, &id, w);
                            const target_llvm_ty = self.stackTypeToLLVMType(target_ty);
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ target_llvm_ty, value.name, info_ptr.?.ptr_name });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                        }
                        last_instruction_was_terminator = false;
                    },
                    .Call => |c| {
                        try self.handleCall(w, &stack, &id, c, peek_state, hir);
                        last_instruction_was_terminator = false;
                    },
                    .Halt => {
                        try w.writeAll("  ret void\n");
                        last_instruction_was_terminator = true;
                    },
                    .Arith => |a| {
                        try self.handleArith(w, &stack, &id, a);
                        last_instruction_was_terminator = false;
                    },
                    .Compare => |cmp| {
                        try self.handleCompare(w, &stack, &id, cmp);
                        last_instruction_was_terminator = false;
                    },
                    .Convert => |conv| {
                        try self.handleConvert(w, &stack, &id, conv);
                        last_instruction_was_terminator = false;
                    },
                    .StringOp => |sop| {
                        try self.handleStringOp(w, &stack, &id, sop, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .TypeCheck => |tc| {
                        try self.handleTypeCheck(w, &stack, &id, tc, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .GroupCheck => |gc| {
                        try self.handleGroupCheck(w, &stack, &id, gc);
                        last_instruction_was_terminator = false;
                    },
                    .GroupExtractPayload => {
                        try self.handleGroupExtractPayload(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                    .UnionConstruct => |uc| {
                        try self.handleUnionConstruct(w, &stack, &id, uc);
                        last_instruction_was_terminator = false;
                    },
                    .LogicalOp => |lop| {
                        try self.handleLogicalOp(w, &stack, &id, lop);
                        last_instruction_was_terminator = false;
                    },
                    .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                    .ArrayConcat => {
                        try self.handleArrayConcat(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                    .AssertFail => |af| {
                        try self.handleAssertFail(w, &stack, &id, af, peek_state);
                        last_instruction_was_terminator = false;
                    },
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
