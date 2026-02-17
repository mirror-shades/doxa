const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const PeekEmitState = Ctx.PeekEmitState;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const VariableInfo = Ctx.VariableInfo;
    const StackMergeState = Ctx.StackMergeState;
    const escapeLLVMString = Ctx.escapeLLVMString;
    const internPeekString = Ctx.internPeekString;

    return struct {
        pub fn writeModule(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype) !void {
            try w.writeAll("declare void @doxa_write_cstr(ptr)\n");
            try w.writeAll("");
            try w.writeAll("declare void @doxa_print_i64(i64)\n");
            try w.writeAll("declare void @doxa_print_u64(i64)\n");
            try w.writeAll("declare void @doxa_print_f64(double)\n");
            try w.writeAll("declare void @doxa_print_byte(i64)\n");
            try w.writeAll("declare i64 @doxa_str_len(ptr)\n");
            try w.writeAll("declare ptr @doxa_str_concat(ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_str_clone(ptr)\n");
            try w.writeAll("declare ptr @doxa_substring(ptr, i64, i64)\n");
            try w.writeAll("declare i8 @doxa_str_pop(ptr, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_str_insert(ptr, i64, ptr)\n");
            try w.writeAll("declare i8 @doxa_str_remove(ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_char_to_string(i8)\n");
            try w.writeAll("declare i64 @doxa_int_from_cstr(ptr)\n");
            try w.writeAll("declare double @doxa_float_from_cstr(ptr)\n");
            try w.writeAll("declare i64 @doxa_byte_from_cstr(ptr)\n");
            try w.writeAll("declare i64 @doxa_byte_from_f64(double)\n");
            try w.writeAll("declare ptr @doxa_int_to_string(i64)\n");
            try w.writeAll("declare ptr @doxa_float_to_string(double)\n");
            try w.writeAll("declare ptr @doxa_byte_to_string(i64)\n");
            try w.writeAll("declare ptr @doxa_tetra_to_string(i64)\n");
            try w.writeAll("declare ptr @doxa_nothing_to_string()\n");
            try w.writeAll("declare ptr @doxa_enum_to_string(ptr, i64)\n");
            try w.writeAll("declare ptr @doxa_struct_to_string(ptr)\n");
            try w.writeAll("declare ptr @doxa_array_to_string(ptr)\n");
            try w.writeAll("declare void @doxa_debug_peek(ptr)\ndeclare void @doxa_peek_string(ptr)\n");
            try w.writeAll("declare void @doxa_print_array_hdr(ptr)\n");
            try w.writeAll("declare i1 @doxa_str_eq(ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_new(i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_clone(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_len(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_get_i64(ptr, i64)\n");
            try w.writeAll("declare void @doxa_array_set_i64(ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_concat(ptr, ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_insert(ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_remove(ptr, i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_slice(ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_map_new(i64, i64, i64)\n");
            try w.writeAll("declare void @doxa_map_set_i64(ptr, i64, i64)\n");
            try w.writeAll("declare void @doxa_map_set_else_i64(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_map_get_i64(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_map_try_get_i64(ptr, i64, ptr)\n");
            try w.writeAll("declare double @llvm.pow.f64(double, double)\n");
            try w.writeAll("declare double @doxa_random()\n");
            try w.writeAll("declare i64 @doxa_tick()\n");
            try w.writeAll("declare i64 @doxa_int(double)\n");
            try w.writeAll("declare ptr @doxa_input()\n");
            // Legacy type check ABI (i64 + tag + ptr). Implemented as a shim over
            // the canonical DoxaValue-based helper so older IR keeps working.
            try w.writeAll("declare i64 @doxa_type_check(i64, i64, ptr)\n");
            try w.writeAll("declare i64 @doxa_type_check_value(%DoxaValue, ptr)\n");
            try w.writeAll("declare void @doxa_print_value(ptr)\n");
            try w.writeAll("declare i64 @doxa_find(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_array(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_str(ptr, ptr)\n");
            try w.writeAll("declare void @doxa_struct_register(ptr, ptr)\n");
            try w.writeAll("declare void @doxa_enum_register(ptr)\n");
            try w.writeAll("declare ptr @malloc(i64)\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_gt(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_eq(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_gt(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_eq(ptr, i64)\n");
            try w.writeAll("declare void @doxa_clear(ptr)\n");

            // Inline zig module functions (external): declare them as varargs so we don't need full type info here.
            // We only declare calls that are NOT backed by a HIR function_table entry.
            var declared_inline = std.StringHashMap(void).init(self.allocator);
            defer declared_inline.deinit();
            for (hir.instructions) |inst| {
                if (inst != .Call) continue;
                const c = inst.Call;
                if (c.call_kind != .ModuleFunction) continue;
                if (std.mem.indexOfScalar(u8, c.qualified_name, '.') == null) continue;
                if (c.qualified_name.len == 0) continue;

                // If this name exists as a defined HIR function, don't declare varargs (would conflict with definition).
                var is_defined = false;
                for (hir.function_table) |ft| {
                    if (std.mem.eql(u8, ft.qualified_name, c.qualified_name)) {
                        is_defined = true;
                        break;
                    }
                }
                if (is_defined) continue;
                if (declared_inline.contains(c.qualified_name)) continue;
                try declared_inline.put(c.qualified_name, {});

                const ret_ty = self.hirTypeToLLVMType(c.return_type, false);
                const decl = try std.fmt.allocPrint(self.allocator, "declare {s} @{s}(...)\n", .{ ret_ty, c.qualified_name });
                defer self.allocator.free(decl);
                try w.writeAll(decl);
            }

            try self.buildEnumPrintMap(hir);
            // Struct field names are captured directly from StructNew name/value pairs
            // during codegen; the old backward-scan heuristic was brittle and could
            // associate unrelated string constants (like "name") with a struct type.

            for (hir.string_pool, 0..) |s, idx| {
                const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\\00\"\n", .{ idx, s.len + 1, s });
                defer self.allocator.free(str_line);
                try w.writeAll(str_line);
            }
            if (hir.string_pool.len > 0) try w.writeAll("\n");

            for (hir.constant_pool, 0..) |hv, idx| {
                if (hv == .string) {
                    const s = hv.string;
                    const escaped = try escapeLLVMString(self.allocator, s);
                    defer self.allocator.free(escaped);
                    const str_idx = hir.string_pool.len + idx;
                    const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\\00\"\n", .{ str_idx, s.len + 1, escaped });
                    defer self.allocator.free(str_line);
                    try w.writeAll(str_line);
                }
            }

            try w.writeAll("%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }\n");
            // Canonical value representation shared with the runtime. The layout
            // must stay in sync with `DoxaValue` in `src/runtime/doxa_rt.zig`.
            try w.writeAll("%DoxaValue = type { i32, i32, i64 }\n");
            try w.writeAll("%ArrayHeader = type { ptr, i64, i64, i64, i64 }\n\n");
            try w.writeAll("@.doxa.nl = private constant [2 x i8] c\"\\0A\\00\"\n");

            try w.writeAll("@tetra_not_lut = private constant [4 x i8] [i8 1, i8 0, i8 3, i8 2]\n");
            try w.writeAll("@tetra_and_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 0, i8 0, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 2, i8 2, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 3, i8 0, i8 3]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_or_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 2, i8 1, i8 2, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 3, i8 1, i8 2, i8 3]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_iff_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 3, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 3, i8 2, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 2, i8 3, i8 3, i8 2]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_xor_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 3, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 2, i8 3, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 3, i8 2, i8 3, i8 2]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_nand_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 3, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 3, i8 3, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 2, i8 1, i8 2]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_nor_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 3, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 0, i8 0, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 3, i8 0, i8 3, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 2, i8 0, i8 3, i8 2]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_implies_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 2, i8 3],\n");
            try w.writeAll("  [4 x i8] [i8 2, i8 1, i8 2, i8 2],\n");
            try w.writeAll("  [4 x i8] [i8 3, i8 1, i8 2, i8 3]\n");
            try w.writeAll("]\n\n");

            try self.emitQuantifierWrappers(w);

            var func_start_labels = std.StringHashMap(bool).init(self.allocator);
            defer func_start_labels.deinit();
            for (hir.function_table) |f| {
                try func_start_labels.put(f.start_label, true);
            }

            const functions_start_idx = self.findFunctionsSectionStart(hir, &func_start_labels);

            for (hir.instructions) |inst| {
                if (inst == .StructNew) {
                    const sn = inst.StructNew;
                    if (!self.global_struct_field_types.contains(sn.type_name)) {
                        _ = try self.global_struct_field_types.put(sn.type_name, try self.allocator.dupe(HIR.HIRType, sn.field_types));
                    }
                    if (!self.struct_field_names_by_type.contains(sn.type_name)) {
                        _ = try self.struct_field_names_by_type.put(sn.type_name, try self.allocator.dupe([]const u8, sn.field_names));
                    }
                    if (!self.struct_fields_by_id.contains(sn.struct_id)) {
                        _ = try self.struct_fields_by_id.put(sn.struct_id, try self.allocator.dupe(HIR.HIRType, sn.field_types));
                    }
                    if (!self.struct_type_names_by_id.contains(sn.struct_id)) {
                        _ = try self.struct_type_names_by_id.put(sn.struct_id, sn.type_name);
                    }
                }
            }

            for (hir.function_table) |func| {
                try self.collectFunctionStructReturnInfo(hir, func, &func_start_labels);
            }

            var peek_state = PeekEmitState.init(self.allocator, &self.peek_string_counter);
            defer peek_state.deinit();

            var has_entry_function: bool = false;
            var entry_function_name: ?[]const u8 = null;
            for (hir.function_table) |f| {
                if (f.is_entry) {
                    has_entry_function = true;
                    entry_function_name = f.qualified_name;
                    break;
                }
            }

            if (!has_entry_function) {
                try self.writeMainProgram(hir, w, functions_start_idx, &peek_state);
            }

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

            for (hir.function_table) |func| {
                try self.writeFunction(hir, w, func, &func_start_labels, &peek_state);
            }

            if (has_entry_function) {
                const entry_mangled_name_owned = if (entry_function_name) |name|
                    (if (std.mem.eql(u8, name, "main")) null else try std.fmt.allocPrint(self.allocator, "doxa_entry_{s}", .{name}))
                else
                    null;
                defer if (entry_mangled_name_owned) |name| self.allocator.free(name);
                const entry_mangled_name = if (entry_function_name) |name|
                    (if (std.mem.eql(u8, name, "main")) "doxa_user_main" else entry_mangled_name_owned.?)
                else
                    "doxa_user_main";

                try w.writeAll("define i32 @main() {\n");
                try w.writeAll("entry:\n");
                var init_id: usize = 0;
                try self.emitEnumInitCalls(w, &peek_state, &init_id);
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}()\n", .{entry_mangled_name});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
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

        pub fn findFunctionsSectionStart(self: *IRPrinter, hir: *const HIR.HIRProgram, func_start_labels: *std.StringHashMap(bool)) usize {
            _ = self;
            for (hir.instructions, 0..) |inst, idx| {
                if (inst == .Label) {
                    const lbl = inst.Label.name;
                    if (func_start_labels.get(lbl) != null) return idx;
                }
            }
            return hir.instructions.len;
        }

        pub fn writeMainProgram(
            self: *IRPrinter,
            hir: *const HIR.HIRProgram,
            w: anytype,
            functions_start_idx: usize,
            peek_state: *PeekEmitState,
        ) !void {
            try w.writeAll("define i32 @main() {\n");
            try w.writeAll("entry:\n");

            var id: usize = 0;
            var stack = std.array_list.Managed(StackVal).init(self.allocator);
            defer stack.deinit();

            try self.emitEnumInitCalls(w, peek_state, &id);

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
                                try stack.append(.{ .name = name, .ty = .PTR, .string_literal_value = s });
                            },
                            .enum_variant => |ev| {
                                const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ name, @as(i64, @intCast(ev.variant_index)) });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                                const type_name_global = try self.createEnumTypeNameGlobal(ev.type_name, &id);
                                try stack.append(.{ .name = name, .ty = .I64, .enum_type_name = type_name_global });
                                self.last_emitted_enum_value = ev.variant_index;
                            },
                            .nothing => {
                                try stack.append(.{ .name = "0", .ty = .Nothing });
                            },
                            else => {},
                        }
                    },
                    .StoreAlias => |_| {
                        last_instruction_was_terminator = false;
                    },
                    .ArrayNew => |a| try self.emitArrayNew(w, &stack, &id, a),
                    .ArraySet => |_| try self.emitArraySet(w, &stack, &id),
                    .ArrayGet => |_| try self.emitArrayGet(w, &stack, &id),
                    .ArrayGetAndAdd => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Add),
                    .ArrayGetAndSub => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Sub),
                    .ArrayGetAndMul => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Mul),
                    .ArrayGetAndDiv => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Div),
                    .ArrayGetAndMod => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Mod),
                    .ArrayGetAndPow => |_| try self.emitArrayGetAndArith(w, &stack, &id, .Pow),
                    .ArrayLen => try self.emitArrayLen(w, &stack, &id),
                    .ArrayPush => |_| try self.emitArrayPush(w, &stack, &id),
                    .ArrayPop => try self.emitArrayPop(w, &stack, &id),
                    .ArrayInsert => try self.emitArrayInsert(w, &stack, &id),
                    .ArrayRemove => try self.emitArrayRemove(w, &stack, &id),
                    .ArraySlice => try self.emitArraySlice(w, &stack, &id),
                    .Map => |m| try self.emitMap(w, &stack, &id, m),
                    .MapGet => |mg| try self.emitMapGet(w, &stack, &id, mg),
                    .MapSet => |ms| try self.emitMapSet(w, &stack, &id, ms),
                    .Range => |r| try self.emitRange(w, &stack, &id, r),
                    .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                    .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                    .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                    .Dup => {
                        if (stack.items.len < 1) continue;
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
                        switch (a.operand_type) {
                            .Int => {
                                lhs = try self.ensureI64(w, lhs, &id);
                                rhs = try self.ensureI64(w, rhs, &id);
                                if (a.op == .Pow) {
                                    const lhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const lhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ lhs_double, lhs.name });
                                    defer self.allocator.free(lhs_conv_line);
                                    try w.writeAll(lhs_conv_line);
                                    const rhs_double = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const rhs_conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ rhs_double, rhs.name });
                                    defer self.allocator.free(rhs_conv_line);
                                    try w.writeAll(rhs_conv_line);
                                    const pow_result = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const pow_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @llvm.pow.f64(double {s}, double {s})\n", .{ pow_result, lhs_double, rhs_double });
                                    defer self.allocator.free(pow_line);
                                    try w.writeAll(pow_line);
                                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
                                    const conv_back_line = try std.fmt.allocPrint(self.allocator, "  {s} = fptosi double {s} to i64\n", .{ name, pow_result });
                                    defer self.allocator.free(conv_back_line);
                                    try w.writeAll(conv_back_line);
                                    try stack.append(.{ .name = name, .ty = .I64 });
                                } else {
                                    const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                    id += 1;
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
                                const lhs_double = if (lhs.ty == .F64) blk: {
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
                                const rhs_double = if (rhs.ty == .F64) blk: {
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
                                const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
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
                                const name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
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

                            if (v.ty == .I2) {
                                const idx_i64 = try self.nextTemp(&id);
                                const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ idx_i64, v.name });
                                defer self.allocator.free(zext_line);
                                try w.writeAll(zext_line);

                                const lut_ptr = try self.nextTemp(&id);
                                const gep_line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr @tetra_not_lut, i64 0, i64 {s}\n", .{ lut_ptr, idx_i64 });
                                defer self.allocator.free(gep_line);
                                try w.writeAll(gep_line);

                                const result_i8 = try self.nextTemp(&id);
                                const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, lut_ptr });
                                defer self.allocator.free(load_line);
                                try w.writeAll(load_line);

                                const result_i2 = try self.nextTemp(&id);
                                const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                                defer self.allocator.free(trunc_line);
                                try w.writeAll(trunc_line);

                                try stack.append(.{ .name = result_i2, .ty = .I2 });
                            } else {
                                const bool_val = try self.ensureBool(w, v, &id);
                                const not_result = try self.nextTemp(&id);
                                const not_line = try std.fmt.allocPrint(self.allocator, "  {s} = xor i1 {s}, true\n", .{ not_result, bool_val.name });
                                defer self.allocator.free(not_line);
                                try w.writeAll(not_line);
                                try stack.append(.{ .name = not_result, .ty = .I1 });
                            }
                        } else {
                            if (stack.items.len < 2) continue;
                            const rhs = stack.items[stack.items.len - 1];
                            const lhs = stack.items[stack.items.len - 2];
                            stack.items.len -= 2;

                            // Check if operands are tetra (i2) or boolean (i1)
                            if (lhs.ty == .I2 and rhs.ty == .I2) {
                                // Tetra operations - use LUTs
                                // Convert both operands to i64 indices
                                const lhs_i64 = try self.nextTemp(&id);
                                const lhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ lhs_i64, lhs.name });
                                defer self.allocator.free(lhs_zext);
                                try w.writeAll(lhs_zext);

                                const rhs_i64 = try self.nextTemp(&id);
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
                                const row_ptr = try self.nextTemp(&id);
                                const row_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x [4 x i8]], ptr {s}, i64 0, i64 {s}\n", .{ row_ptr, lut_name, lhs_i64 });
                                defer self.allocator.free(row_gep);
                                try w.writeAll(row_gep);

                                // Get the element: row[rhs]
                                const elem_ptr = try self.nextTemp(&id);
                                const elem_gep = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds [4 x i8], ptr {s}, i64 0, i64 {s}\n", .{ elem_ptr, row_ptr, rhs_i64 });
                                defer self.allocator.free(elem_gep);
                                try w.writeAll(elem_gep);

                                // Load the result
                                const result_i8 = try self.nextTemp(&id);
                                const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i8, ptr {s}\n", .{ result_i8, elem_ptr });
                                defer self.allocator.free(load_line);
                                try w.writeAll(load_line);

                                // Convert back to i2
                                const result_i2 = try self.nextTemp(&id);
                                const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i8 {s} to i2\n", .{ result_i2, result_i8 });
                                defer self.allocator.free(trunc_line);
                                try w.writeAll(trunc_line);

                                try stack.append(.{ .name = result_i2, .ty = .I2 });
                            } else {
                                // Boolean operations - use regular LLVM logical operations
                                const lhs_bool = try self.ensureBool(w, lhs, &id);
                                const rhs_bool = try self.ensureBool(w, rhs, &id);

                                const result = try self.nextTemp(&id);
                                const op_str = switch (lop.op) {
                                    .And => "and",
                                    .Or => "or",
                                    .Xor => "xor",
                                    else => blk: {
                                        // For complex boolean operations, convert to i2 and use tetra logic
                                        // First convert booleans to i2 (false=0, true=1)
                                        const lhs_i2 = try self.nextTemp(&id);
                                        const lhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ lhs_i2, lhs_bool.name });
                                        defer self.allocator.free(lhs_zext);
                                        try w.writeAll(lhs_zext);

                                        const rhs_i2 = try self.nextTemp(&id);
                                        const rhs_zext = try std.fmt.allocPrint(self.allocator, "  {s} = zext i1 {s} to i2\n", .{ rhs_i2, rhs_bool.name });
                                        defer self.allocator.free(rhs_zext);
                                        try w.writeAll(rhs_zext);

                                        // Push them back to stack and recurse (this will use tetra logic)
                                        try stack.append(.{ .name = lhs_i2, .ty = .I2 });
                                        try stack.append(.{ .name = rhs_i2, .ty = .I2 });

                                        // Re-process this instruction with tetra operands
                                        stack.items.len -= 2; // Remove what we just added
                                        // This is a bit hacky - we'd need to re-emit the instruction
                                        // For now, just handle the complex cases by falling through

                                        // Default to AND for unknown operations
                                        break :blk "and";
                                    },
                                };

                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = {s} i1 {s}, {s}\n", .{ result, op_str, lhs_bool.name, rhs_bool.name });
                                defer self.allocator.free(line);
                                try w.writeAll(line);

                                try stack.append(.{ .name = result, .ty = .I1 });
                            }
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
                            .I8 => {
                                const byte_i64 = try self.nextTemp(&id);
                                const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ byte_i64, v.name });
                                defer self.allocator.free(zext_line);
                                try w.writeAll(zext_line);
                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_byte(i64 {s})\n", .{byte_i64});
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
                    },
                    .PrintEnd => |_| {
                        // no-op for now (newlines are explicit constants in program)
                    },
                    .Peek => |pk| {
                        if (stack.items.len < 1) continue;
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

                        try self.emitPeekInstruction(w, pk, val, &id, peek_state);

                        // Nothing type - print "nothing" and continue
                        if (val.ty == .Nothing or pk.value_type == .Nothing) {
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
                            continue;
                        }

                        // Prefer enum-aware printing when we know the enum type,
                        // falling back to raw integers only when we have no metadata.
                        if (pk.value_type == .Enum) {
                            if (pk.enum_type_name) |etype| {
                                try self.emitEnumPrint(peek_state, w, &id, etype, val.name);
                                try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                                continue;
                            } else if (val.enum_type_name) |etype2| {
                                try self.emitEnumPrint(peek_state, w, &id, etype2, val.name);
                                try w.writeAll("  call void @doxa_write_cstr(ptr getelementptr inbounds ([2 x i8], ptr @.doxa.nl, i64 0, i64 0))\n");
                                continue;
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

                        if (should_print_as_string and val.ty == .PTR) {
                            // String value - let runtime handle formatting
                            const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s})\n", .{val.name});
                            defer self.allocator.free(call_val);
                            try w.writeAll(call_val);
                        } else if (val.ty == .Value) {
                            // Store to stack and pass pointer to avoid ABI quirks
                            const tmp_ptr = try self.nextTemp(&id);
                            const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{tmp_ptr});
                            defer self.allocator.free(alloca_line);
                            try w.writeAll(alloca_line);
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ val.name, tmp_ptr });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_value(ptr {s})\n", .{tmp_ptr});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        } else if (should_print_as_float and val.ty == .I64) {
                            // Float stored as I64 bit pattern - convert to double for printing
                            const double_val = try self.nextTemp(&id);
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
                                    try self.emitEnumPrint(peek_state, w, &id, type_name, val.name);
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
                                const byte_i64 = try self.nextTemp(&id);
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
                                    const dv = try self.buildDoxaValue(w, val, null, &id);
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
                                } else if (val.array_type) |_| {
                                    const line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{val.name});
                                    defer self.allocator.free(line);
                                    try w.writeAll(line);
                                } else {
                                    // Print string - let runtime handle formatting
                                    const call_val = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s})\n", .{val.name});
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
                                const tetra_i64 = try self.nextTemp(&id);
                                const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ tetra_i64, val.name });
                                defer self.allocator.free(zext_line);
                                try w.writeAll(zext_line);

                                // Compare with 0, 1, 2, 3
                                const eq0 = try self.nextTemp(&id);
                                const eq1 = try self.nextTemp(&id);
                                const eq2 = try self.nextTemp(&id);
                                const eq3 = try self.nextTemp(&id);
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
                                const fptr = try self.nextTemp(&id);
                                const tptr = try self.nextTemp(&id);
                                const bptr = try self.nextTemp(&id);
                                const nptr = try self.nextTemp(&id);
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

                                // Select: if eq3 then neither, else if eq2 then both, else if eq1 then true, else false
                                // First: if eq1 then true, else false
                                const sel01 = try self.nextTemp(&id);
                                const sel01_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel01, eq1, tptr, fptr });
                                defer self.allocator.free(sel01_line);
                                try w.writeAll(sel01_line);

                                // Second: if eq2 then both, else sel01 (true/false)
                                const sel012 = try self.nextTemp(&id);
                                const sel012_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel012, eq2, bptr, sel01 });
                                defer self.allocator.free(sel012_line);
                                try w.writeAll(sel012_line);

                                // Final: if eq3 then neither, else sel012 (both/true/false)
                                const sel_final = try self.nextTemp(&id);
                                const sel_final_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel_final, eq3, nptr, sel012 });
                                defer self.allocator.free(sel_final_line);
                                try w.writeAll(sel_final_line);

                                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{sel_final});
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
                        try self.recordStackForLabel(&merge_map, j.label, stack.items, current_block, &id, w);
                        const line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        stack.items.len = 0;
                        last_instruction_was_terminator = true;
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

                        // Call the legacy ABI helper that takes value bits + type tag +
                        // target type string. This avoids platform-specific struct
                        // passing issues with %DoxaValue on MSVC ABIs.
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
                        last_instruction_was_terminator = false;
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

                        if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "find") and raw_args.items.len == 2) {
                            const collection = raw_args.items[0];
                            const needle = raw_args.items[1];

                            const coll_ptr = if (collection.ty == .PTR) collection else try self.ensurePointer(w, collection, &id);
                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                            id += 1;

                            if (collection.array_type != null) {
                                const needle_i64 = if (needle.ty == .I64) needle else try self.ensureI64(w, needle, &id);
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_find_array(ptr {s}, i64 {s})\n", .{ result_name, coll_ptr.name, needle_i64.name });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            } else {
                                const needle_ptr = if (needle.ty == .PTR) needle else try self.ensurePointer(w, needle, &id);
                                const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_find_str(ptr {s}, ptr {s})\n", .{ result_name, coll_ptr.name, needle_ptr.name });
                                defer self.allocator.free(line);
                                try w.writeAll(line);
                            }

                            try stack.append(.{ .name = result_name, .ty = .I64 });
                            last_instruction_was_terminator = false;
                            continue;
                        }

                        var arg_strings = std.array_list.Managed([]const u8).init(self.allocator);
                        defer {
                            for (arg_strings.items) |s| self.allocator.free(s);
                            arg_strings.deinit();
                        }

                        const func_info = if (c.function_index < hir.function_table.len)
                            hir.function_table[c.function_index]
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
                                        arg = try self.unwrapDoxaValueToType(w, arg, decl, &id);
                                        arg_ptr.* = arg;
                                    }
                                }
                            }

                            // Builtin ABI fix: doxa_find(collection: ptr, value: i64).
                            // When searching in strings, the second argument can be a ptr;
                            // normalize it to i64 payload bits for stable calling convention.
                            if (c.call_kind == .BuiltinFunction and std.mem.eql(u8, c.qualified_name, "find") and i == 1 and arg.ty == .PTR) {
                                const as_i64 = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
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

                        // Determine actual return type:
                        // - Prefer the Call's return_type when it's specific
                        // - Fall back to the function signature when Call's type is missing/too-generic (e.g. arrays typed as String/PTR)
                        var actual_return_type: HIR.HIRType = if (c.return_type != .Nothing) c.return_type else .Nothing;
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
                        }
                        last_instruction_was_terminator = false;
                    },
                    .Convert => |conv| {
                        if (stack.items.len < 1) continue;
                        const arg = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        switch (conv.to_type) {
                            .Byte => {
                                const as_i64 = switch (arg.ty) {
                                    .I64 => arg,
                                    .F64 => blk: {
                                        const tmp = try self.nextTemp(&id);
                                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_byte_from_f64(double {s})\n", .{ tmp, arg.name });
                                        defer self.allocator.free(line);
                                        try w.writeAll(line);
                                        break :blk StackVal{ .name = tmp, .ty = .I64 };
                                    },
                                    .PTR => blk: {
                                        const tmp = try self.nextTemp(&id);
                                        const line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_byte_from_cstr(ptr {s})\n", .{ tmp, arg.name });
                                        defer self.allocator.free(line);
                                        try w.writeAll(line);
                                        break :blk StackVal{ .name = tmp, .ty = .I64 };
                                    },
                                    else => try self.ensureI64(w, arg, &id),
                                };
                                const out = try self.nextTemp(&id);
                                const trunc = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ out, as_i64.name });
                                defer self.allocator.free(trunc);
                                try w.writeAll(trunc);
                                try stack.append(.{ .name = out, .ty = .I8 });
                            },
                            else => {
                                try stack.append(arg);
                            },
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
                                // Compute C-string length
                                const ptr_name = blk: {
                                    if (arg.ty == .PTR) break :blk arg.name;
                                    // Best-effort: coerce anything else to i64 then treat as pointer.
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
                                switch (arg.ty) {
                                    .PTR => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int_from_cstr(ptr {s})\n", .{ result_name, arg.name });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .I64 });
                                    },
                                    .F64 => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i64 @doxa_int(double {s})\n", .{ result_name, arg.name });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .I64 });
                                    },
                                    else => {
                                        const arg_i64 = if (arg.ty == .I64) arg else try self.ensureI64(w, arg, &id);
                                        try stack.append(.{ .name = arg_i64.name, .ty = .I64 });
                                    },
                                }
                            },
                            .ToFloat => {
                                switch (arg.ty) {
                                    .F64 => try stack.append(arg),
                                    .PTR => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call double @doxa_float_from_cstr(ptr {s})\n", .{ result_name, arg.name });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .F64 });
                                    },
                                    else => {
                                        const arg_i64 = if (arg.ty == .I64) arg else try self.ensureI64(w, arg, &id);
                                        const conv_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const conv_line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ conv_name, arg_i64.name });
                                        defer self.allocator.free(conv_line);
                                        try w.writeAll(conv_line);
                                        try stack.append(.{ .name = conv_name, .ty = .F64 });
                                    },
                                }
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
                            .ToString => {
                                switch (arg.ty) {
                                    .PTR => {
                                        if (arg.array_type != null) {
                                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_array_to_string(ptr {s})\n", .{ result_name, arg.name });
                                            defer self.allocator.free(call_line);
                                            try w.writeAll(call_line);
                                            try stack.append(.{ .name = result_name, .ty = .PTR });
                                        } else if (arg.struct_field_types != null or arg.struct_type_name != null) {
                                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_struct_to_string(ptr {s})\n", .{ result_name, arg.name });
                                            defer self.allocator.free(call_line);
                                            try w.writeAll(call_line);
                                            try stack.append(.{ .name = result_name, .ty = .PTR });
                                        } else {
                                            try stack.append(arg);
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
                                            const enum_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const gep = try std.fmt.allocPrint(
                                                self.allocator,
                                                "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                                                .{ enum_ptr, info.length, info.name },
                                            );
                                            defer self.allocator.free(gep);
                                            try w.writeAll(gep);

                                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_enum_to_string(ptr {s}, i64 {s})\n", .{ result_name, enum_ptr, arg.name });
                                            defer self.allocator.free(call_line);
                                            try w.writeAll(call_line);
                                            try stack.append(.{ .name = result_name, .ty = .PTR });
                                        } else {
                                            const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                            id += 1;
                                            const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_int_to_string(i64 {s})\n", .{ result_name, arg.name });
                                            defer self.allocator.free(call_line);
                                            try w.writeAll(call_line);
                                            try stack.append(.{ .name = result_name, .ty = .PTR });
                                        }
                                    },
                                    .F64 => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_float_to_string(double {s})\n", .{ result_name, arg.name });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .PTR });
                                    },
                                    .I8 => {
                                        const widened = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ widened, arg.name });
                                        defer self.allocator.free(zext_line);
                                        try w.writeAll(zext_line);
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_byte_to_string(i64 {s})\n", .{ result_name, widened });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .PTR });
                                    },
                                    .I1, .I2 => {
                                        const src_ty = self.stackTypeToLLVMType(arg.ty);
                                        const widened = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, arg.name });
                                        defer self.allocator.free(zext_line);
                                        try w.writeAll(zext_line);
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_tetra_to_string(i64 {s})\n", .{ result_name, widened });
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .PTR });
                                    },
                                    .Nothing => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_nothing_to_string()\n", .{result_name});
                                        defer self.allocator.free(call_line);
                                        try w.writeAll(call_line);
                                        try stack.append(.{ .name = result_name, .ty = .PTR });
                                    },
                                    else => {
                                        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                        id += 1;
                                        const zero_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, 0\n", .{result_name});
                                        defer self.allocator.free(zero_line);
                                        try w.writeAll(zero_line);
                                        try stack.append(.{ .name = result_name, .ty = .I64 });
                                    },
                                }
                            },
                            .Pop => {
                                const ptr_val = if (arg.ty == .PTR) arg else try self.ensurePointer(w, arg, &id);
                                const rem_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const pop_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const rem_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{rem_slot});
                                defer self.allocator.free(rem_alloca);
                                try w.writeAll(rem_alloca);
                                const pop_alloca = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{pop_slot});
                                defer self.allocator.free(pop_alloca);
                                try w.writeAll(pop_alloca);
                                const rem_init = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{rem_slot});
                                defer self.allocator.free(rem_init);
                                try w.writeAll(rem_init);
                                const pop_init = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{pop_slot});
                                defer self.allocator.free(pop_init);
                                try w.writeAll(pop_init);

                                const ok_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call i8 @doxa_str_pop(ptr {s}, ptr {s}, ptr {s})\n", .{ ok_name, ptr_val.name, rem_slot, pop_slot });
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);

                                const rem_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const pop_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const rem_load = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ rem_name, rem_slot });
                                defer self.allocator.free(rem_load);
                                try w.writeAll(rem_load);
                                const pop_load = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ pop_name, pop_slot });
                                defer self.allocator.free(pop_load);
                                try w.writeAll(pop_load);

                                try stack.append(.{ .name = rem_name, .ty = .PTR });
                                try stack.append(.{ .name = pop_name, .ty = .PTR });
                            },
                            .Substring => {
                                if (stack.items.len < 2) continue;
                                const length = stack.items[stack.items.len - 1];
                                const start = stack.items[stack.items.len - 2];
                                stack.items.len -= 2;
                                const s_ptr = if (arg.ty == .PTR) arg else try self.ensurePointer(w, arg, &id);
                                const start_i64 = if (start.ty == .I64) start else try self.ensureI64(w, start, &id);
                                const len_i64 = if (length.ty == .I64) length else try self.ensureI64(w, length, &id);
                                const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id});
                                id += 1;
                                const call_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_substring(ptr {s}, i64 {s}, i64 {s})\n", .{ result_name, s_ptr.name, start_i64.name, len_i64.name });
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);
                                try stack.append(.{ .name = result_name, .ty = .PTR });
                            },
                            else => {
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
                            const struct_names = self.global_struct_field_names.get(gname);
                            const struct_type_name = self.global_struct_type_names.get(gname);
                            try stack.append(.{
                                .name = result_name,
                                .ty = st,
                                .array_type = array_type,
                                .enum_type_name = enum_type_name,
                                .struct_field_types = struct_fields,
                                .struct_field_names = struct_names,
                                .struct_type_name = struct_type_name,
                            });
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

                            // IMPORTANT: PushStorageId must always push the *storage address* (the variable's
                            // location), even if the variable itself stores a pointer (strings/arrays/structs).
                            // Alias parameters (including `this` in instance methods) expect an address they can
                            // load/store through; loading here would pass the pointee value and break aliasing.
                            const struct_fields = self.global_struct_field_types.get(gname);
                            const struct_names = self.global_struct_field_names.get(gname);
                            const struct_type_name = self.global_struct_type_names.get(gname);
                            // Note: We intentionally don't free gptr here because it can be referenced later
                            // (e.g. by a call instruction). It's cleaned up when the IR printer is destroyed.
                            try stack.append(.{ .name = gptr, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
                        } else if (variables.get(psid.var_name)) |entry| {
                            // Push the local variable pointer directly (not loaded)
                            try stack.append(.{
                                .name = entry.ptr_name,
                                .ty = .PTR,
                                .array_type = entry.array_type,
                                .enum_type_name = entry.enum_type_name,
                                .struct_field_types = entry.struct_field_types,
                                .struct_field_names = entry.struct_field_names,
                                .struct_type_name = entry.struct_type_name,
                            });
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
                        var value = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        // Nothing is a zero-sized type that cannot be stored in LLVM IR
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
                        } else {
                            // Handle local variables
                            var info_ptr = variables.getPtr(sv.var_name);
                            if (info_ptr == null) {
                                // Variable should have been allocated at function entry
                                // This is an error condition - variable not found
                                continue;
                            } else {
                                // IMPORTANT: parameters and locals may be pre-allocated with a default
                                // stack type (often i64). For native codegen, we must honor the HIR's
                                // expected type so later ops (e.g. StringOp.Length) get correct lowering.
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
                            const store_line = try std.fmt.allocPrint(
                                self.allocator,
                                "  store {s} {s}, ptr {s}\n",
                                .{ target_llvm_ty, value.name, info_ptr.?.ptr_name },
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
                            } else {
                                // For local declarations without an initializer, ensure arrays/maps are initialized
                                // so subsequent operations like @push/@set have a valid header to mutate.
                                if (sd.declared_type == .Array or sd.declared_type == .Map) {
                                    const info_ptr = variables.getPtr(sd.var_name) orelse continue;
                                    switch (sd.declared_type) {
                                        .Array => |inner| {
                                            const elem_type = inner.*;
                                            const elem_size = self.arrayElementSize(elem_type);
                                            const elem_tag = self.arrayElementTag(elem_type);
                                            const reg = try self.nextTemp(&id);
                                            const new_line = try std.fmt.allocPrint(
                                                self.allocator,
                                                "  {s} = call ptr @doxa_array_new(i64 {d}, i64 {d}, i64 0)\n",
                                                .{ reg, elem_size, elem_tag },
                                            );
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
                                            const new_line = try std.fmt.allocPrint(
                                                self.allocator,
                                                "  {s} = call ptr @doxa_map_new(i64 0, i64 {d}, i64 {d})\n",
                                                .{ reg, key_tag, val_tag },
                                            );
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
                        // Clone arrays for mutable variable declarations to avoid aliasing
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
                        const llvm_ty = self.stackTypeToLLVMType(target_ty);
                        if (sd.scope_kind == .GlobalLocal) {
                            // Record global type and emit store to module-level global
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
                        } else {
                            var info_ptr = variables.getPtr(sd.var_name);
                            if (info_ptr == null) {
                                // Variable should have been allocated at function entry
                                // This is an error condition - variable not found
                                continue;
                            } else {
                                if (sd.declared_type == .Unknown) info_ptr.?.stack_type = target_ty;
                                info_ptr.?.array_type = value.array_type;
                                info_ptr.?.enum_type_name = value.enum_type_name;
                                info_ptr.?.struct_field_types = value.struct_field_types;
                                info_ptr.?.struct_field_names = value.struct_field_names;
                                info_ptr.?.struct_type_name = value.struct_type_name;
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
                        if (value.name.len > IRPrinter.MAX_SANE_NAME_LEN and sc.scope_kind == .GlobalLocal) {
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
                            const stored_names = if (value.struct_field_names) |names|
                                names
                            else if (value.struct_type_name) |tname|
                                self.struct_field_names_by_type.get(tname)
                            else
                                null;
                            if (stored_names) |names| {
                                _ = try self.global_struct_field_names.put(sc.var_name, names);
                            }
                            if (value.struct_type_name) |tname| {
                                _ = try self.global_struct_type_names.put(sc.var_name, tname);
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
                                info_ptr.?.struct_field_types = value.struct_field_types;
                                info_ptr.?.struct_field_names = value.struct_field_names;
                                info_ptr.?.struct_type_name = value.struct_type_name;
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
                    else => {
                        // Handle ArrayConcat instruction (fallback since switch case doesn't work)
                        const tag_name = @tagName(inst);
                        if (std.mem.eql(u8, tag_name, "ArrayConcat")) {
                            // Pop the two arrays from stack (they should be loaded by previous LoadVar instructions)
                            if (stack.items.len >= 2) {
                                const rhs = stack.items[stack.items.len - 1];
                                const lhs = stack.items[stack.items.len - 2];
                                stack.items.len -= 2;

                                // Determine element type
                                const elem_type = lhs.array_type orelse rhs.array_type orelse HIR.HIRType{ .Int = {} };
                                const elem_size = self.arrayElementSize(elem_type);
                                const elem_tag = self.arrayElementTag(elem_type);

                                // Generate array concatenation call
                                const concat_reg = try self.nextTemp(&id);
                                const call_line = try std.fmt.allocPrint(
                                    self.allocator,
                                    "  {s} = call ptr @doxa_array_concat(ptr {s}, ptr {s}, i64 {d}, i64 {d})\n",
                                    .{ concat_reg, lhs.name, rhs.name, elem_size, elem_tag },
                                );
                                defer self.allocator.free(call_line);
                                try w.writeAll(call_line);

                                // Push result to stack
                                try stack.append(.{ .name = concat_reg, .ty = .PTR, .array_type = elem_type });
                            }
                            last_instruction_was_terminator = false;
                        }
                    },
                }
            }

            // Ensure a valid exit if control falls through
            if (!had_return) {
                try w.writeAll("  ret i32 0\n");
            }

            try w.writeAll("}\n");
        }

        pub fn getFunctionRange(
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

        pub fn collectFunctionStructReturnInfo(
            self: *IRPrinter,
            hir: *const HIR.HIRProgram,
            func: HIR.HIRProgram.HIRFunction,
            func_start_labels: *std.StringHashMap(bool),
        ) !void {
            const range = self.getFunctionRange(hir, func, func_start_labels) orelse return;
            var pending_fields: ?[]HIR.HIRType = null;
            var pending_type_name: ?[]const u8 = null;
            var idx: usize = range.start + 1;
            while (idx < range.end) : (idx += 1) {
                const inst = hir.instructions[idx];
                switch (inst) {
                    .StructNew => |sn| {
                        if (pending_fields) |existing| {
                            self.allocator.free(existing);
                        }
                        pending_fields = try self.allocator.dupe(HIR.HIRType, sn.field_types);
                        pending_type_name = sn.type_name;
                    },
                    .Return => |ret| {
                        if (pending_fields) |fields| {
                            defer pending_fields = null;
                            if (ret.has_value and !self.function_struct_return_fields.contains(func.qualified_name)) {
                                _ = try self.function_struct_return_fields.put(func.qualified_name, fields);
                                if (pending_type_name) |tn| {
                                    _ = try self.function_struct_return_type_names.put(func.qualified_name, tn);
                                }
                            } else {
                                self.allocator.free(fields);
                            }
                            pending_type_name = null;
                        }
                    },
                    .Label => {},
                    else => {
                        if (pending_fields) |fields| {
                            self.allocator.free(fields);
                            pending_fields = null;
                            pending_type_name = null;
                        }
                    },
                }
            }
            if (pending_fields) |fields| {
                self.allocator.free(fields);
            }
            pending_type_name = null;
        }

        pub fn collectStructFieldNames(self: *IRPrinter, hir: *const HIR.HIRProgram) !void {
            for (hir.instructions, 0..) |inst, idx| {
                if (inst != .StructNew) continue;
                const sn = inst.StructNew;

                if (self.struct_field_names_by_type.contains(sn.type_name)) {
                    continue;
                }

                const field_count: usize = @intCast(sn.field_count);
                if (field_count == 0) {
                    const empty = try self.allocator.alloc([]const u8, 0);
                    try self.struct_field_names_by_type.put(sn.type_name, empty);
                    continue;
                }

                var names = try self.allocator.alloc([]const u8, field_count);
                var found: usize = 0;
                var search_idx = idx;
                while (search_idx > 0 and found < field_count) {
                    search_idx -= 1;
                    const prev = hir.instructions[search_idx];
                    if (prev == .Const) {
                        const payload = prev.Const;
                        if (payload.value == .string) {
                            const slot = found;
                            names[slot] = payload.value.string;
                            found += 1;
                        }
                    }
                }

                if (found == field_count) {
                    try self.struct_field_names_by_type.put(sn.type_name, names);
                } else {
                    self.allocator.free(names);
                }
            }
        }
    };
}
