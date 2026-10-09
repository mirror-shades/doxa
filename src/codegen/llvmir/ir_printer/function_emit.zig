const std = @import("std");
const module_graph = @import("../../../module/graph.zig");
const DoxaTag = @import("../../../runtime/doxa_rt.zig").DoxaTag;

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
        /// Scope bookkeeping is suppressed wholesale for functions whose arenas
        /// are provably unused (see `scope_elision.zig`). Routing every emission
        /// through these helpers keeps enter/exit balanced: either both sides are
        /// written or neither is.
        fn emitScopeEnter(self: *IRPrinter, w: anytype) !void {
            if (self.scopes_elided) return;
            try w.writeAll("  call void @doxa_scope_enter()\n");
        }

        fn emitScopeExit(self: *IRPrinter, w: anytype) !void {
            if (self.scopes_elided) return;
            try w.writeAll("  call void @doxa_scope_exit()\n");
        }

        fn emitScopeReset(self: *IRPrinter, w: anytype) !void {
            if (self.scopes_elided) return;
            try w.writeAll("  call void @doxa_scope_reset()\n");
        }

        /// Unwind the reusable loop scopes still active at a `return`, then the
        /// function's own scope. Each reusable loop scope contributes two levels.
        fn emitReturnScopeExits(self: *IRPrinter, w: anytype, loop_scope_count: u32) !void {
            if (self.scopes_elided) return;
            var loop_i = loop_scope_count;
            while (loop_i > 0) : (loop_i -= 1) {
                try w.writeAll("  call void @doxa_scope_exit()\n");
                try w.writeAll("  call void @doxa_scope_exit()\n");
            }
            try w.writeAll("  call void @doxa_scope_exit()\n");
        }

        pub fn writeFunction(
            self: *IRPrinter,
            hir: *const HIR.HIRProgram,
            outer_w: anytype,
            func: HIR.HIRProgram.HIRFunction,
            /// The function never uses its scope arena (`ScopeElision.deadScopes`).
            scope_dead: bool,
            func_start_labels: *std.StringHashMap(bool),
            peek_state: *PeekEmitState,
        ) !void {
            const prev_peek_state = self.active_peek_state;
            self.active_peek_state = peek_state;
            defer self.active_peek_state = prev_peek_state;

            self.in_function_context = true;
            defer self.in_function_context = false;

            self.var_regions.clearRetainingCapacity();
            self.var_ranges.clearRetainingCapacity();
            self.var_range_blocks.clearRetainingCapacity();
            self.current_block = "entry";

            const range = self.getFunctionRange(hir, func, func_start_labels) orelse return;
            const start_idx = range.start;
            const end_idx = range.end;

            // A function that never puts a value in its scope arena does not need
            // one. Skipping the enter/exit pair removes two runtime calls per
            // call, and with them the barrier to inlining a leaf function.
            self.scopes_elided = scope_dead;
            defer self.scopes_elided = false;

            // First pass: Collect all variables that need allocation
            var variables_to_allocate = std.AutoHashMap(HIR.Slot, VariableInfo).init(self.allocator);
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
                /// Alias re-pass depth: frames between this frame and the one
                /// owning the aliased storage (0 = immediate caller).
                alias_extra: u8 = 0,
                /// Runtime depth value (an `i64` LLVM name) passed in by the
                /// caller for this alias parameter. Null for aliases created by
                /// re-passing within this frame (no cross-call boundary yet).
                depth_value: ?[]const u8 = null,
            };
            var alias_slots = std.AutoHashMap(u32, AliasInfo).init(self.allocator);
            defer alias_slots.deinit();

            // Scan through instructions to find all variables that need allocation
            for (hir.instructions[start_idx..end_idx]) |inst| {
                switch (inst) {
                    .StoreVar => |sv| {
                        const declared_stack_type = if (sv.expected_type != .Unknown)
                            self.hirTypeToStackType(sv.expected_type)
                        else
                            .I64;
                        // A store carries its slot's type, so a group or union one
                        // names the box the slot holds, as a declaration does.
                        const boxed_declared_type: ?HIR.HIRType = if (IRPrinter.isBoxedMemberType(sv.expected_type))
                            sv.expected_type
                        else
                            null;
                        if (variables_to_allocate.getPtr(sv.slot)) |existing| {
                            // Upgrade a placeholder i64 slot to a concrete, possibly
                            // wider type (e.g. a string bound first as a raw value and
                            // then narrowed). The alloca must be sized for the widest
                            // value the variable holds or stores overflow the slot.
                            if (existing.stack_type == .I64 and declared_stack_type != .I64) {
                                existing.stack_type = declared_stack_type;
                            }
                            if (existing.boxed_declared_type == null) existing.boxed_declared_type = boxed_declared_type;
                        } else {
                            const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}.{d}", .{ sv.var_name, sv.slot });
                            const info = VariableInfo{ .ptr_name = ptr_name, .stack_type = declared_stack_type, .boxed_declared_type = boxed_declared_type, .array_type = null };
                            try variables_to_allocate.put(sv.slot, info);
                        }
                    },
                    .StoreDecl => |sd| {
                        const declared_stack_type = self.hirTypeToStackType(sd.declared_type);
                        // The declaration is the slot's identity: a group or union
                        // declared type is the only thing a store into the slot can
                        // key a `%DoxaValue` box on.
                        const boxed_declared_type: ?HIR.HIRType = if (IRPrinter.isBoxedMemberType(sd.declared_type))
                            sd.declared_type
                        else
                            null;
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
                        if (variables_to_allocate.getPtr(sd.slot)) |existing| {
                            // A declaration carries the authoritative type; upgrade a
                            // placeholder i64 slot so the alloca is sized for the real
                            // (possibly wider) value.
                            existing.boxed_declared_type = boxed_declared_type;
                            if (existing.stack_type == .I64 and declared_stack_type != .I64) {
                                existing.stack_type = declared_stack_type;
                                if (existing.array_type == null) existing.array_type = array_hint;
                                if (existing.struct_field_types == null) existing.struct_field_types = struct_field_types;
                                if (existing.struct_field_names == null) existing.struct_field_names = struct_field_names;
                                if (existing.struct_type_name == null) existing.struct_type_name = struct_type_name;
                            }
                        } else {
                            const ptr_name = try std.fmt.allocPrint(self.allocator, "%var.{s}.{d}", .{ sd.var_name, sd.slot });
                            const info = VariableInfo{
                                .ptr_name = ptr_name,
                                .stack_type = declared_stack_type,
                                .boxed_declared_type = boxed_declared_type,
                                .array_type = array_hint,
                                .struct_field_types = struct_field_types,
                                .struct_field_names = struct_field_names,
                                .struct_type_name = struct_type_name,
                            };
                            try variables_to_allocate.put(sd.slot, info);
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

            // Trailing alias-depth arguments: one `i64` per alias parameter, in
            // parameter order. The caller supplies how many frames separate it
            // from the alias's owning frame, so a heap store through the alias
            // re-homes into the owner's arena and not an ancestor callee's.
            var alias_param_count: usize = 0;
            {
                var alias_arg_at: usize = func.param_types.len;
                for (func.param_types, 0..) |_, param_idx| {
                    const is_alias = if (param_idx < func.param_is_alias.len) func.param_is_alias[param_idx] else false;
                    if (!is_alias) continue;
                    alias_param_count += 1;
                    const arg_str = try std.fmt.allocPrint(self.allocator, "i64 %{d}", .{alias_arg_at});
                    try param_strs.append(arg_str);
                    alias_arg_at += 1;
                }
            }

            const params_str = if (param_strs.items.len == 0) "" else try std.mem.join(self.allocator, ", ", param_strs.items);
            defer if (param_strs.items.len > 0) self.allocator.free(params_str);

            const emitted_name = try self.functionSymbol(func);
            defer self.allocator.free(emitted_name);

            const func_decl = try std.fmt.allocPrint(self.allocator, "define {s} @{s}({s}){s} {{\n", .{ return_type_str, emitted_name, params_str, Ctx.function_attr_group });
            defer self.allocator.free(func_decl);
            try outer_w.writeAll(func_decl);

            // Add entry block
            try outer_w.writeAll("entry:\n");

            // Initialize variables map
            var variables = std.AutoHashMap(HIR.Slot, VariableInfo).init(self.allocator);
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
                const slot = entry.key_ptr.*;
                const var_info = entry.value_ptr.*;
                const llvm_ty = self.stackTypeToLLVMType(var_info.stack_type);
                const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca {s}\n", .{ var_info.ptr_name, llvm_ty });
                defer self.allocator.free(alloca_line);
                try outer_w.writeAll(alloca_line);

                // Add to variables map for later use
                try variables.put(slot, var_info);
            }

            // Reusable stack slots for string operations (avoid alloca-in-loop stack overflow)
            try outer_w.writeAll("  %str_out_ptr = alloca ptr\n");
            try outer_w.writeAll("  %str_out_len = alloca i64\n");
            self.entry_str_out_ptr = "%str_out_ptr";
            self.entry_str_out_len = "%str_out_len";

            // Stage the body so synthetic-header allocas discovered while
            // emitting it can be replayed in the entry block (see the
            // `entry_allocas` field). All writes from here on go to the buffer.
            self.entry_allocas.clearRetainingCapacity();
            self.synth_header_counter = 0;
            var body_alloc = std.Io.Writer.Allocating.init(self.allocator);
            defer body_alloc.deinit();
            const w = &body_alloc.writer;

            // Process function body instructions
            var id: usize = func.param_types.len + alias_param_count; // Start after parameters and their alias-depth args
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
            var alias_param_at: usize = 0;
            for (func.param_types, 0..) |param_type, param_idx| {
                const param_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{param_idx});
                // Check if this is an alias parameter
                const is_alias = if (param_idx < func.param_is_alias.len) func.param_is_alias[param_idx] else false;
                const stack_type = if (is_alias) .PTR else self.hirTypeToStackType(param_type);
                const array_hint: ?HIR.HIRType = switch (param_type) {
                    .Array => |inner| inner.*,
                    else => null,
                };
                var depth_value: ?[]const u8 = null;
                if (is_alias) {
                    depth_value = try std.fmt.allocPrint(self.allocator, "%{d}", .{func.param_types.len + alias_param_at});
                    alias_param_at += 1;
                }
                try stack.append(.{ .name = param_name, .ty = stack_type, .array_type = array_hint, .alias_depth_value = depth_value });
            }

            // Process function body instructions
            self.scope_depth = 0;
            var jump_targets = try self.collectLiveJumpTargets(hir.instructions[start_idx..end_idx]);
            defer jump_targets.deinit();
            for (hir.instructions[start_idx..end_idx], start_idx..) |inst, inst_index| {
                self.verifyEnter(module_graph.displayName(func.qualified_name), inst_index, inst);
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
                    self.current_block = dead_label;
                    stack.items.len = 0;
                    last_instruction_was_terminator = false;
                }
                switch (inst) {
                    .Label => |lbl| {
                        // Only process function body labels, skip function start labels and invalid basic block names
                        const should_print = !std.mem.eql(u8, lbl.name, func.start_label) and !std.mem.startsWith(u8, lbl.name, "func_");
                        // Not fallen into and not jumped to: the code under
                        // this label is unreachable. Leaving the terminator
                        // flag set skips it up to the next live label.
                        if (should_print and last_instruction_was_terminator and !jump_targets.contains(lbl.name)) continue;
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
                        self.current_block = lbl.name;
                        // Phase D-1 follow-on: activate the precomputed
                        // loop-head ranges while inside the loop.
                        if (std.mem.startsWith(u8, lbl.name, "loop_start")) {
                            if (self.loop_head_envs.getPtr(lbl.name)) |m| self.active_loop_range = m;
                        } else if (std.mem.startsWith(u8, lbl.name, "loop_exit")) {
                            self.active_loop_range = null;
                        }
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
                            try emitReturnScopeExits(self, w, ret.loop_scope_count);
                            try w.writeAll("  ret void\n");
                            last_instruction_was_terminator = true;
                            continue;
                        }
                        if (ret.has_value and stack.items.len > 0) {
                            var v = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                            if (IRPrinter.isBoxedMemberType(func.return_type)) {
                                v = try self.buildDoxaValue(w, v, func.return_type, &id);
                            }
                            if (v.ty != target_return_stack_type) {
                                v = try self.coerceForStore(v, target_return_stack_type, &id, w);
                            }
                            // A3: a value constructed directly in this return was
                            // already allocated in the caller's arena, so it
                            // outlives the callee body and needs no clone.
                            if (v.region != .Caller) {
                                v = try self.cloneHeapForReturn(w, &id, v, func.return_type);
                            }
                            try emitReturnScopeExits(self, w, ret.loop_scope_count);
                            const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, v.name });
                            defer self.allocator.free(ret_line);
                            try w.writeAll(ret_line);
                        } else {
                            if (std.mem.indexOf(u8, return_type_str, "%DoxaValue") != null) {
                                // A value-less `return` yields `nothing`; tag the union
                                // value accordingly so `as nothing` narrowing matches.
                                const zero = try self.nextTemp(&id);
                                const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} undef, i32 {d}, 0\n", .{ zero, return_type_str, @intFromEnum(DoxaTag.Nothing) });
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
                                const zero4 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const zero4_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i64 0, 3\n", .{ zero4, return_type_str, zero3 });
                                defer self.allocator.free(zero4_line);
                                try w.writeAll(zero4_line);
                                try emitReturnScopeExits(self, w, ret.loop_scope_count);
                                const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, zero4 });
                                defer self.allocator.free(ret_line);
                                try w.writeAll(ret_line);
                            } else {
                                try emitReturnScopeExits(self, w, ret.loop_scope_count);
                                const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} zeroinitializer\n", .{return_type_str});
                                defer self.allocator.free(ret_line);
                                try w.writeAll(ret_line);
                            }
                        }
                        last_instruction_was_terminator = true;
                    },
                    .Unreachable => {
                        try w.writeAll("  call void @doxa_trap_unreachable()\n");
                        try w.writeAll("  unreachable\n");
                        stack.items.len = 0;
                        last_instruction_was_terminator = true;
                    },
                    .JumpCond => |jc| {
                        try self.requireStack(&stack, 1);
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
                        if (lv.scope_kind == .GlobalLocal or lv.scope_kind == .ModuleGlobal) {
                            try self.handleLoadVarGlobal(w, &stack, &id, lv.var_name);
                        } else if (variables.get(lv.slot)) |entry| {
                            const result_name = try self.nextTemp(&id);
                            const ty_str = self.stackTypeToLLVMType(entry.stack_type);
                            const line = try std.fmt.allocPrint(
                                self.allocator,
                                "  {s} = load {s}, ptr {s}\n",
                                .{ result_name, ty_str, entry.ptr_name },
                            );
                            defer self.allocator.free(line);
                            try w.writeAll(line);
                            const loaded = StackVal{
                                .name = result_name,
                                .ty = entry.stack_type,
                                // A region recorded for a *variable* is an upper
                                // bound, not a fact: the Deep-wins join keeps a
                                // loop-arena classification even after a later
                                // `.rehome` store re-homed the payload into the
                                // function body. Only a producer that allocates in
                                // the current loop arena at the store site is a
                                // definite `Deep`. A load therefore must never
                                // present `Deep` as a clone signal — fold it to
                                // `Unknown` so the emitter keeps the runtime
                                // rehome call (which reads the object's real
                                // arena) instead of statically cloning an object
                                // the runtime would keep.
                                .region = switch (self.var_regions.get(lv.slot) orelse .Unknown) {
                                    .Deep => .Unknown,
                                    else => |r| r,
                                },
                                // Phase D: a variable's recorded range is a
                                // must-join over its reaching stores, so a load
                                // may present it as a fact.
                                .int_range = self.varRange(lv.slot),
                                .array_type = entry.array_type,
                                .enum_type_name = entry.enum_type_name,
                                .struct_field_types = entry.struct_field_types,
                                .struct_field_names = entry.struct_field_names,
                                .struct_type_name = entry.struct_type_name,
                                .fixed_array_depth = entry.fixed_array_depth,
                                .fixed_array_sizes = entry.fixed_array_sizes,
                                .boxed_type = entry.boxed_declared_type,
                            };
                            try stack.append(loaded);
                        } else {
                            return self.hirFault("loads slot {d} ('{s}'), which no store in this function declares", .{ lv.slot, lv.var_name });
                        }
                        last_instruction_was_terminator = false;
                    },
                    .Peek => |pk| {
                        try self.handlePeek(w, &stack, &id, pk, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .PushStorageId => |psid| {
                        if (psid.alias_slot) |alias_slot| {
                            const info = alias_slots.get(alias_slot).?;
                            // Re-passing an existing alias: the storage owner is
                            // one frame further up than it was for this alias.
                            // Carry both the compile-time re-pass count and the
                            // runtime owner depth so the callee can resolve the
                            // true owner across the call boundary.
                            try stack.append(.{ .name = info.ptr_name, .ty = .PTR, .array_type = info.array_type, .enum_type_name = info.enum_type_name, .struct_field_types = info.struct_field_types, .struct_field_names = info.struct_field_names, .struct_type_name = info.struct_type_name, .alias_extra = info.alias_extra + 1, .alias_owned = true, .alias_depth_value = info.depth_value });
                        } else if (psid.scope_kind == .GlobalLocal or psid.scope_kind == .ModuleGlobal) {
                            try self.handlePushStorageIdGlobal(w, &stack, &id, psid.var_name);
                        } else if (variables.get(psid.slot)) |entry| {
                            // A narrowed receiver's slot holds a boxed
                            // `%DoxaValue`, but the contract for a pushed storage
                            // id is the address of a slot holding the value a
                            // callee (a struct method's `this`) reads and writes.
                            // The box's payload word is exactly that slot: pointing
                            // `this` at it unwraps the member for the call and
                            // writes any replaced receiver back into the box.
                            if (psid.box_payload) |struct_id| {
                                {
                                    const payload_addr = try self.nextTemp(&id);
                                    const gep_line = try std.fmt.allocPrint(
                                        self.allocator,
                                        "  {s} = getelementptr inbounds %DoxaValue, ptr {s}, i32 0, i32 2\n",
                                        .{ payload_addr, entry.ptr_name },
                                    );
                                    defer self.allocator.free(gep_line);
                                    try w.writeAll(gep_line);
                                    const type_name = self.struct_type_names_by_id.get(struct_id);
                                    try stack.append(.{
                                        .name = payload_addr,
                                        .ty = .PTR,
                                        .struct_field_types = self.struct_fields_by_id.get(struct_id),
                                        .struct_field_names = if (type_name) |tn| self.struct_field_names_by_type.get(tn) else null,
                                        .struct_type_name = type_name orelse "struct",
                                    });
                                    last_instruction_was_terminator = false;
                                    continue;
                                }
                            }
                            if (entry.fixed_array_depth > 0) {
                                // Fixed-size arrays live as raw stack buffers, so the
                                // variable slot holds a pointer to the element storage
                                // rather than an ArrayHeader. Callees treat an array
                                // alias parameter as the address of a slot holding an
                                // ArrayHeader pointer, so build a non-owning header that
                                // views the caller's storage and pass a slot referencing
                                // it. Element mutations through the alias then land
                                // directly in the caller's array.
                                const raw_data = try self.nextTemp(&id);
                                const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ raw_data, entry.ptr_name });
                                defer self.allocator.free(load_line);
                                try w.writeAll(load_line);

                                const hdr = try self.wrapFixedArrayHeader(w, .{
                                    .name = raw_data,
                                    .ty = .PTR,
                                    .array_type = entry.array_type,
                                    .fixed_array_depth = entry.fixed_array_depth,
                                    .fixed_array_sizes = entry.fixed_array_sizes,
                                }, &id);

                                const slot = try self.nextTemp(&id);
                                const slot_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{slot});
                                defer self.allocator.free(slot_alloca);
                                try w.writeAll(slot_alloca);
                                const store_slot = try std.fmt.allocPrint(self.allocator, "  store ptr {s}, ptr {s}\n", .{ hdr.name, slot });
                                defer self.allocator.free(store_slot);
                                try w.writeAll(store_slot);

                                try stack.append(.{ .name = slot, .ty = .PTR, .array_type = entry.array_type });
                            } else {
                                try stack.append(.{ .name = entry.ptr_name, .ty = .PTR, .array_type = entry.array_type, .enum_type_name = entry.enum_type_name, .struct_field_types = entry.struct_field_types, .struct_field_names = entry.struct_field_names, .struct_type_name = entry.struct_type_name });
                            }
                        } else {
                            return self.hirFault("takes the address of slot {d} ('{s}'), which no store in this function declares", .{ psid.slot, psid.var_name });
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
                                    // An alias points into the caller's arena, which
                                    // outlives this callee's function body (`Func`).
                                    .region = .Func,
                                    .alias_extra = info.alias_extra,
                                    .alias_owned = true,
                                    .alias_depth_value = info.depth_value,
                                });
                            } else {
                                const result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const llvm_ty = self.hirTypeToLLVMType(info.pointee_type, false);
                                const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result, llvm_ty, info.ptr_name });
                                defer self.allocator.free(load_line);
                                try w.writeAll(load_line);
                                var loaded = StackVal{
                                    .name = result,
                                    .ty = stack_ty,
                                    .array_type = info.array_type,
                                    .struct_field_types = info.struct_field_types,
                                    .struct_field_names = info.struct_field_names,
                                    .struct_type_name = info.struct_type_name,
                                    .enum_type_name = info.enum_type_name,
                                    .boxed_type = if (IRPrinter.isBoxedMemberType(info.pointee_type)) info.pointee_type else null,
                                };
                                loaded.alias_extra = info.alias_extra;
                                loaded.alias_owned = true;
                                loaded.alias_depth_value = info.depth_value;
                                try stack.append(loaded);
                            }
                        } else {
                            return self.hirFault("loads through alias slot {d}, which nothing bound", .{la.slot_index});
                        }
                        last_instruction_was_terminator = false;
                    },
                    .StoreAlias => |sa| {
                        try self.requireStack(&stack, 1);
                        var value = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const info = alias_slots.get(sa.slot_index) orelse
                            return self.hirFault("stores through alias slot {d}, which nothing bound", .{sa.slot_index});
                        // The generator converted the value to the aliased
                        // storage's type; the store only checks it.
                        try self.verifyStore(value, info.pointee_type);
                        // The alias points at the caller's storage. A heap
                        // value produced here must be re-homed into the arena
                        // that owns that variable, or it dangles once this
                        // function's scope exits. `.keep` is the in-place-
                        // mutation store-back (arrays), whose identity must
                        // survive untouched.
                        switch (sa.heap_copy) {
                            .keep => {},
                            .rehome => value = try self.cloneHeapForAliasStore(w, &id, value, info.pointee_type, info.alias_extra, info.depth_value),
                            .snapshot => value = try self.cloneHeapForSnapshot(w, &id, value, info.pointee_type),
                        }
                        const llvm_ty = self.hirTypeToLLVMType(info.pointee_type, false);
                        const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ llvm_ty, value.name, info.ptr_name });
                        defer self.allocator.free(store_line);
                        try w.writeAll(store_line);
                        last_instruction_was_terminator = false;
                    },
                    .BindAlias => |ba| {
                        try self.requireStack(&stack, 1);
                        const ptr_val = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        var struct_fields: ?[]HIR.HIRType = ptr_val.struct_field_types;
                        var struct_field_names: ?[]const []const u8 = ptr_val.struct_field_names;
                        var struct_type_name: ?[]const u8 = ptr_val.struct_type_name;
                        if (struct_fields == null and ba.target_type == .Struct) {
                            // An alias parameter's pointee carries the struct id
                            // directly, so resolve the layout from it. The alias
                            // name is the parameter name (e.g. `req`), not a struct
                            // type name, so the name-based lookups below cannot
                            // find a plain `^param :: Struct` alias.
                            const sid = ba.target_type.Struct;
                            struct_fields = self.struct_fields_by_id.get(sid);
                            if (struct_type_name == null) {
                                struct_type_name = self.struct_type_names_by_id.get(sid);
                            }
                            if (struct_field_names == null) {
                                if (struct_type_name) |tn| {
                                    struct_field_names = self.struct_field_names_by_type.get(tn);
                                }
                            }
                            // A method's `this` is its receiver struct.
                            if (struct_fields == null and std.mem.eql(u8, ba.alias_name, "this")) {
                                if (func.receiver) |receiver| struct_fields = self.struct_fields_by_id.get(receiver);
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
                            .struct_field_names = struct_field_names,
                            .struct_type_name = struct_type_name,
                            .enum_type_name = ptr_val.enum_type_name,
                            .alias_extra = ptr_val.alias_extra,
                            .depth_value = ptr_val.alias_depth_value,
                        };
                        try alias_slots.put(ba.alias_slot, alias_info);
                        last_instruction_was_terminator = false;
                    },
                    .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                    .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                    .ArrayNew => |a| try self.emitArrayNew(w, &stack, &id, a),
                    .ArraySet => try self.emitArraySet(w, &stack, &id),
                    .ArrayGet => try self.emitArrayGet(w, &stack, &id),
                    .ArrayCompoundAssign => |a| try self.emitArrayGetAndArith(w, &stack, &id, a.op, &current_block),
                    .ArrayLen => try self.emitArrayLen(w, &stack, &id),
                    .ArrayPush => try self.emitArrayPush(w, &stack, &id),
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
                        try self.handleSwap(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .StoreFieldName => {
                        last_instruction_was_terminator = false;
                    },
                    .EnterScope => {
                        try emitScopeEnter(self, w);
                        self.scope_depth += 1;
                        last_instruction_was_terminator = false;
                    },
                    .ResetScope => {
                        try emitScopeReset(self, w);
                        last_instruction_was_terminator = false;
                    },
                    .ExitScope => |s| {
                        try emitScopeExit(self, w);
                        if (!self.exited_scopes.contains(s.scope_id)) {
                            self.scope_depth -|= 1;
                            self.exited_scopes.put(s.scope_id, {}) catch {};
                        }
                        last_instruction_was_terminator = false;
                    },
                    .PeekStruct => |ps| {
                        try self.handlePeekStruct(w, &stack, &id, ps, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .Pop => {
                        try self.handlePop(&stack);
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
                            try self.requireStack(&stack, 1);
                            var value = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                            try self.verifyStore(value, sd.declared_type);
                            if (sd.declared_type == .Array and value.array_type == null) {
                                value.array_type = sd.declared_type.Array.*;
                            }
                            // A1: a mutable declaration re-homes (or plain-stores)
                            // into the function-body arena, so the payload outlives
                            // every later store. A const keeps its initializer's own
                            // arena, which the analysis may not know — leave it
                            // unclassified rather than risk a plain store of a
                            // short-lived object.
                            if (!sd.is_const) {
                                value = try self.rehomeForLocalStore(w, &id, value, sd.declared_type);
                            }
                            if (sd.is_const) {
                                // A const keeps its initializer's own arena. Its value
                                // is single-assignment, so that region is a definite
                                // fact, not a may-class — record it exactly so later
                                // loads can be decided statically (A2).
                                try self.var_regions.put(sd.slot, value.region);
                            } else {
                                try self.recordVarRegion(sd.slot, .Func);
                            }
                            // Phase D: a declaration is single-assignment, so
                            // its initializer's range is installed outright. A
                            // non-const `var` is assigned in its initializer
                            // too — there is no separate definition to merge
                            // with — and every later `StoreVar` hulls into it.
                            if (value.ty == .I64 or value.ty == .I8) {
                                try self.recordVarRange(sd.slot, value.int_range);
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
                            value = try self.coerceForStore(value, declared_stack_type, &id, w);
                            var info_ptr = variables.getPtr(sd.slot);
                            if (info_ptr == null) {
                                continue;
                            } else {
                                if (info_ptr.?.stack_type == .I64 and declared_stack_type != .I64) {
                                    info_ptr.?.stack_type = declared_stack_type;
                                }
                                if (info_ptr.?.array_type == null) info_ptr.?.array_type = value.array_type;
                                if (info_ptr.?.enum_type_name == null) info_ptr.?.enum_type_name = value.enum_type_name;
                                if (info_ptr.?.struct_field_types == null) info_ptr.?.struct_field_types = value.struct_field_types;
                                if (info_ptr.?.struct_field_names == null) info_ptr.?.struct_field_names = value.struct_field_names;
                                if (info_ptr.?.struct_type_name == null) info_ptr.?.struct_type_name = value.struct_type_name;
                                if (info_ptr.?.fixed_array_depth == 0 and value.fixed_array_depth > 0) {
                                    info_ptr.?.fixed_array_depth = value.fixed_array_depth;
                                    info_ptr.?.fixed_array_sizes = value.fixed_array_sizes;
                                }
                            }
                            const target_local_ty = info_ptr.?.stack_type;
                            // `nothing` is zero-sized, so there is nothing to store. This
                            // arises when a value is bound through `as nothing` narrowing,
                            // whose success result carries no data.
                            if (target_local_ty != .Nothing) {
                                value = try self.coerceForStore(value, target_local_ty, &id, w);
                                const target_llvm_ty = self.stackTypeToLLVMType(target_local_ty);
                                const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ target_llvm_ty, value.name, info_ptr.?.ptr_name });
                                defer self.allocator.free(store_line);
                                try w.writeAll(store_line);
                            }
                        }
                        last_instruction_was_terminator = false;
                    },
                    .StoreVar => |sv| {
                        if (sv.scope_kind == .GlobalLocal or sv.scope_kind == .ModuleGlobal) {
                            try self.handleStoreVarGlobal(w, &stack, &id, sv);
                        } else {
                            try self.requireStack(&stack, 1);
                            var value = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                            try self.verifyStore(value, sv.expected_type);
                            // `nothing` is zero-sized: there is nothing to write.
                            if (sv.expected_type == .Nothing) continue;
                            const expected_array_type: ?HIR.HIRType = switch (sv.expected_type) {
                                .Array => |inner| inner.*,
                                else => null,
                            };
                            if (value.array_type == null and expected_array_type != null) {
                                value.array_type = expected_array_type.?;
                            }
                            value = switch (sv.heap_copy) {
                                .snapshot => try self.cloneHeapForSnapshot(w, &id, value, sv.expected_type),
                                .keep => value,
                                .rehome => try self.rehomeForLocalStore(w, &id, value, sv.expected_type),
                            };
                            // A1 region analysis: every local store except an
                            // in-place `.keep` store-back leaves the payload in the
                            // function-body arena (or an ancestor), so a later load
                            // may take the static plain-store path. `.keep` rewrites
                            // the same object the variable already holds; preserve a
                            // `Deep` classification (loop-arena payload) if one was
                            // recorded.
                            if (sv.heap_copy != .keep or !self.var_regions.contains(sv.slot)) {
                                try self.recordVarRegion(sv.slot, .Func);
                            }
                            // Phase D: hull the stored value's range into the
                            // variable's, so the recorded fact covers every
                            // path that reaches a later load. A `.keep`
                            // store-back carries the *post-mutation* value, so
                            // it widens the range just as a plain store does.
                            if (value.ty == .I64 or value.ty == .I8) {
                                try self.recordVarRange(sv.slot, value.int_range);
                            }
                            var info_ptr = variables.getPtr(sv.slot);
                            if (info_ptr == null) {
                                continue;
                            } else {
                                const expected_stack_ty = self.hirTypeToStackType(sv.expected_type);
                                if (info_ptr.?.stack_type == .I64 and expected_stack_ty != .I64) {
                                    info_ptr.?.stack_type = expected_stack_ty;
                                }
                                if (info_ptr.?.array_type == null) info_ptr.?.array_type = value.array_type;
                                if (info_ptr.?.array_type == null and expected_array_type != null) info_ptr.?.array_type = expected_array_type.?;
                                if (info_ptr.?.enum_type_name == null) info_ptr.?.enum_type_name = value.enum_type_name;
                                if (info_ptr.?.struct_field_types == null) info_ptr.?.struct_field_types = value.struct_field_types;
                                if (info_ptr.?.struct_field_names == null) info_ptr.?.struct_field_names = value.struct_field_names;
                                if (info_ptr.?.struct_type_name == null) info_ptr.?.struct_type_name = value.struct_type_name;
                                if (info_ptr.?.fixed_array_depth == 0 and value.fixed_array_depth > 0) {
                                    info_ptr.?.fixed_array_depth = value.fixed_array_depth;
                                    info_ptr.?.fixed_array_sizes = value.fixed_array_sizes;
                                }
                            }
                            const target_ty = info_ptr.?.stack_type;
                            // `nothing` is zero-sized; skip the store (no data to write).
                            if (target_ty != .Nothing) {
                                value = try self.coerceForStore(value, target_ty, &id, w);
                                const target_llvm_ty = self.stackTypeToLLVMType(target_ty);
                                const store_line = try std.fmt.allocPrint(self.allocator, "  store {s} {s}, ptr {s}\n", .{ target_llvm_ty, value.name, info_ptr.?.ptr_name });
                                defer self.allocator.free(store_line);
                                try w.writeAll(store_line);
                            }
                        }
                        last_instruction_was_terminator = false;
                    },
                    .Call => |c| {
                        const call_range = self.computeCallResultRange(c, &stack);
                        try self.handleCall(w, &stack, &id, c, hir);
                        if (IRPrinter.callDiverges(c)) {
                            // `@panic` and `@exit` never return, so the block
                            // ends here exactly as it does after `Return`: no
                            // value reaches the merge point of an enclosing
                            // `as`/`if`, and the dead tail is skipped.
                            try w.writeAll("  unreachable\n");
                            stack.items.len = 0;
                            last_instruction_was_terminator = true;
                            continue;
                        }
                        if (call_range) |r| {
                            if (stack.items.len > 0) {
                                const top = &stack.items[stack.items.len - 1];
                                if (top.ty == .I64 or top.ty == .I8) top.int_range = r;
                            }
                        }
                        last_instruction_was_terminator = false;
                    },
                    .Halt => {
                        try w.writeAll("  ret void\n");
                        last_instruction_was_terminator = true;
                    },
                    .Arith => |a| {
                        try self.handleArith(w, &stack, &id, a, &current_block);
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
                    .MemberCheck => |mc| {
                        try self.handleMemberCheck(w, &stack, &id, mc);
                        last_instruction_was_terminator = false;
                    },
                    .UnboxPayload => {
                        try self.handleUnboxPayload(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                    .Box => |b| {
                        try self.handleBox(w, &stack, &id, b);
                        last_instruction_was_terminator = false;
                    },
                    .Unbox => |u| {
                        try self.handleUnbox(w, &stack, &id, u);
                        last_instruction_was_terminator = false;
                    },
                    .LogicalOp => |lop| {
                        try self.handleLogicalOp(w, &stack, &id, lop);
                        last_instruction_was_terminator = false;
                    },
                    .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                    .ArrayConcat => {
                        try self.emitArrayConcat(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                    .AssertFail => |af| {
                        try self.handleAssertFail(w, &stack, &id, af, peek_state);
                        last_instruction_was_terminator = true;
                    },
                }
            }

            if (!last_instruction_was_terminator) {
                if (target_return_stack_type == .Nothing) {
                    try emitScopeExit(self, w);
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
                    const zero4 = try self.nextTemp(&id);
                    const zero4_line = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue {s} {s}, i64 0, 3\n", .{ zero4, return_type_str, zero3 });
                    defer self.allocator.free(zero4_line);
                    try w.writeAll(zero4_line);
                    try emitScopeExit(self, w);
                    const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} {s}\n", .{ return_type_str, zero4 });
                    defer self.allocator.free(ret_line);
                    try w.writeAll(ret_line);
                } else {
                    try emitScopeExit(self, w);
                    const ret_line = try std.fmt.allocPrint(self.allocator, "  ret {s} zeroinitializer\n", .{return_type_str});
                    defer self.allocator.free(ret_line);
                    try w.writeAll(ret_line);
                }
            }

            try w.writeAll("}\n\n");

            for (self.entry_allocas.items) |line| {
                try outer_w.writeAll(line);
                self.allocator.free(line);
            }
            self.entry_allocas.clearRetainingCapacity();
            const body_bytes = try body_alloc.toOwnedSlice();
            defer self.allocator.free(body_bytes);
            try outer_w.writeAll(body_bytes);
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
                        entry.value_ptr.* = std.ArrayListUnmanaged(EnumVariantMeta).empty;
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

            for (self.group_table.entries.items) |group_entry| {
                const group_name = group_entry.key.?;
                for (group_entry.members) |member| {
                    if (member.kind != .Enum) continue;
                    const variants = self.enum_table.variants(member.id) orelse continue;
                    for (variants) |variant| {
                        try registerVariant.add(self, group_name, variant.index, variant.name);
                    }
                }
            }

            // Seed every declared enum's variants under its own name, not just
            // those that appeared as a literal in the constant pool. A value
            // produced across the inline-Zig boundary (a discriminant) has no
            // literal, so without this `@print` renders `<enum:N>`. Populating
            // the in-memory map costs nothing until `emitEnumPrint` interns the
            // names it actually needs.
            for (self.enum_table.entries.items) |entry| {
                for (entry.variants) |variant| {
                    try registerVariant.add(self, entry.key.?, variant.index, variant.name);
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

                const unknown_len = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                id.* += 1;
                const unknown_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ unknown_len, unknown_info.len_name });
                defer self.allocator.free(unknown_len_line);
                try w.writeAll(unknown_len_line);

                var current_ptr = unknown_ptr;
                var current_len = unknown_len;

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

                    const v_len = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const v_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ v_len, v_info.len_name });
                    defer self.allocator.free(v_len_line);
                    try w.writeAll(v_len_line);

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

                    const sel_len_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
                    id.* += 1;
                    const sel_len_line = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = select i1 {s}, i64 {s}, i64 {s}\n",
                        .{ sel_len_name, cmp_name, v_len, current_len },
                    );
                    defer self.allocator.free(sel_len_line);
                    try w.writeAll(sel_len_line);

                    current_ptr = sel_name;
                    current_len = sel_len_name;
                }

                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s}, i64 {s})\n", .{ current_ptr, current_len });
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
                const header = try std.fmt.allocPrint(self.allocator, "define i2 @{s}(ptr %hdr, ptr %value_ptr, i64 %value_len){s} {{\n", .{ wrap.name, Ctx.function_attr_group });
                defer self.allocator.free(header);
                try w.writeAll(header);
                try w.writeAll("entry:\n");
                const call_line = try std.fmt.allocPrint(self.allocator, "  %res = call i8 @{s}(ptr %hdr, ptr %value_ptr, i64 %value_len)\n", .{wrap.runtime});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
                try w.writeAll("  %cast = trunc i8 %res to i2\n");
                try w.writeAll("  ret i2 %cast\n}\n\n");
            }
        }
    };
}
