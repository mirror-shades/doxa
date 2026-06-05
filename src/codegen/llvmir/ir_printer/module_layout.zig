const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const PeekEmitState = Ctx.PeekEmitState;
    const StackVal = Ctx.StackVal;
    const VariableInfo = Ctx.VariableInfo;
    const StackMergeState = Ctx.StackMergeState;
    const escapeLLVMString = Ctx.escapeLLVMString;

    return struct {
        pub fn writeModule(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype) !void {
            try w.writeAll("declare void @doxa_write_cstr(ptr, i64)\n");
            try w.writeAll("declare void @doxa_write_raw(ptr)\n");
            try w.writeAll("");
            try w.writeAll("declare void @doxa_print_i64(i64)\n");
            try w.writeAll("declare void @doxa_print_u64(i64)\n");
            try w.writeAll("declare void @doxa_print_f64(double)\n");
            try w.writeAll("declare void @doxa_print_byte(i64)\n");
            try w.writeAll("declare i64 @doxa_str_len(ptr, i64)\n");
            try w.writeAll("declare void @doxa_str_concat(ptr, i64, ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_str_clone(ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_str_from_cstr(ptr, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_str_clone_raw(ptr, i64)\n");
            try w.writeAll("declare void @doxa_substring(ptr, i64, i64, i64, ptr, ptr)\n");
            try w.writeAll("declare i8 @doxa_str_pop(ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_str_insert(ptr, i64, i64, ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare i8 @doxa_str_remove(ptr, i64, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_char_to_string(i8, ptr, ptr)\n");
            try w.writeAll("declare i64 @doxa_int_from_string(ptr, i64)\n");
            try w.writeAll("declare double @doxa_float_from_string(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_byte_from_string(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_byte_from_f64(double)\n");
            try w.writeAll("declare void @doxa_int_to_string(i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_float_to_string(double, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_byte_to_string(i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_tetra_to_string(i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_nothing_to_string(ptr, ptr)\n");
            try w.writeAll("declare void @doxa_enum_to_string(ptr, i64, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_struct_to_string(ptr, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_array_to_string(ptr, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_pack_bytes(ptr, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_unpack_bytes(ptr, i64)\n");
            try w.writeAll("declare void @doxa_debug_peek(ptr)\ndeclare void @doxa_peek_string(ptr, i64)\n");
            try w.writeAll("declare void @doxa_print_array_hdr(ptr)\n");
            try w.writeAll("declare i1 @doxa_str_eq(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare ptr @doxa_array_new(i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_new_nested(i64, i64, i64, ptr, i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_clone(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_len(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_get_i64(ptr, i64)\n");
            try w.writeAll("declare void @doxa_array_set_i64(ptr, i64, i64)\n");
            try w.writeAll("declare void @doxa_array_set_str(ptr, i64, ptr, i64)\n");
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
            try w.writeAll("declare void @doxa_set_args(i32, ptr)\n");
            try w.writeAll("declare i64 @doxa_int(double)\n");
            // Legacy type check ABI (i64 + tag + ptr). Implemented as a shim over
            // the canonical DoxaValue-based helper so older IR keeps working.
            try w.writeAll("declare i64 @doxa_type_check(i64, i64, ptr)\n");
            try w.writeAll("declare i64 @doxa_type_check_value(%DoxaValue, ptr)\n");
            try w.writeAll("declare void @doxa_print_value(ptr)\n");
            try w.writeAll("declare i64 @doxa_find(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_array(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_str(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare void @doxa_struct_register(ptr, ptr)\n");
            try w.writeAll("declare void @doxa_enum_register(ptr)\n");
            try w.writeAll("declare ptr @malloc(i64)\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_gt(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_eq(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_gt(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_eq(ptr, i64)\n");
            try w.writeAll("declare void @doxa_clear(ptr)\n");
            try w.writeAll("declare ptr @doxa_array_range(i64, i64)\n");

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

                const ret_ty = if (c.call_kind == .ModuleFunction and c.return_type == .String) "void" else self.hirTypeToLLVMType(c.return_type, false);
                const decl = try std.fmt.allocPrint(self.allocator, "declare {s} @{s}(...)\n", .{ ret_ty, c.qualified_name });
                defer self.allocator.free(decl);
                try w.writeAll(decl);
            }

            try self.buildEnumPrintMap(hir);
            // Struct field names are captured directly from StructNew name/value pairs
            // during codegen; the old backward-scan heuristic was brittle and could
            // associate unrelated string constants (like "name") with a struct type.

            self.string_pool_len = hir.string_pool.len;

            for (hir.string_pool, 0..) |s, idx| {
                const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\"\n", .{ idx, s.len, s });
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
                    const str_line = try std.fmt.allocPrint(self.allocator, "@.str.{d} = private constant [{d} x i8] c\"{s}\"\n", .{ str_idx, s.len, escaped });
                    defer self.allocator.free(str_line);
                    try w.writeAll(str_line);
                }
            }

            try w.writeAll("%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }\n");
            // Canonical value representation shared with the runtime. The layout
            // must stay in sync with `DoxaValue` in `src/runtime/doxa_rt.zig`.
            try w.writeAll("%DoxaValue = type { i32, i32, i64 }\n");
            try w.writeAll("%DoxaString = type { ptr, i64 }\n");
            try w.writeAll("%ArrayHeader = type { ptr, i64, i64, i64, i64 }\n\n");
            try w.writeAll("@.doxa.nl = private constant [2 x i8] c\"\\0A\\00\"\n");
            try w.writeAll("@.doxa.empty = private constant [1 x i8] c\"\\00\"\n");

            try w.writeAll("@tetra_not_lut = private constant [4 x i8] [i8 1, i8 0, i8 2, i8 3]\n");
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
                if (inst == .LoadModule) {
                    const lm = inst.LoadModule;
                    if (!self.global_struct_field_types.contains(lm.module_name)) {
                        const fcount = lm.field_names.len;
                        const field_types = try self.allocator.alloc(HIR.HIRType, fcount);
                        @memset(field_types, HIR.HIRType{ .String = {} });
                        _ = try self.global_struct_field_types.put(lm.module_name, field_types);
                        _ = try self.global_struct_field_names.put(lm.module_name, try self.allocator.dupe([]const u8, lm.field_names));
                        _ = try self.global_struct_type_names.put(lm.module_name, lm.module_name);
                    }
                }
            }

            for (hir.function_table) |func| {
                try self.collectFunctionStructReturnInfo(hir, func, &func_start_labels);
            }

            // Pre-scan all function instructions to discover referenced globals
            // before the global declaration pass. Functions may reference globals
            // (e.g. module names via LoadModule) that need to be declared in the
            // IR ahead of use.
            for (hir.function_table) |func| {
                const range = self.getFunctionRange(hir, func, &func_start_labels) orelse continue;
                for (hir.instructions[range.start..range.end]) |inst| {
                    switch (inst) {
                        .PushStorageId => |psid| {
                            if (psid.scope_kind == .GlobalLocal) {
                                if (!self.defined_globals.contains(psid.var_name)) {
                                    _ = try self.global_types.put(psid.var_name, .PTR);
                                    _ = try self.defined_globals.put(psid.var_name, true);
                                }
                            }
                        },
                        .LoadVar => |lv| {
                            if (lv.scope_kind == .GlobalLocal) {
                                if (!self.defined_globals.contains(lv.var_name)) {
                                    _ = try self.global_types.put(lv.var_name, .PTR);
                                    _ = try self.defined_globals.put(lv.var_name, true);
                                }
                            }
                        },
                        .LoadModule => |lm| {
                            if (!self.defined_globals.contains(lm.module_name)) {
                                _ = try self.global_types.put(lm.module_name, .PTR);
                                _ = try self.defined_globals.put(lm.module_name, true);
                            }
                        },
                        else => {},
                    }
                }
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

            const entry_mangled_name_owned = if (entry_function_name) |name|
                (if (std.mem.eql(u8, name, "main")) null else try std.fmt.allocPrint(self.allocator, "doxa_entry_{s}", .{name}))
            else
                null;
            defer if (entry_mangled_name_owned) |name| self.allocator.free(name);
            const entry_mangled_name: ?[]const u8 = if (has_entry_function)
                if (entry_function_name) |name|
                    (if (std.mem.eql(u8, name, "main")) "doxa_user_main" else entry_mangled_name_owned.?)
                else
                    "doxa_user_main"
            else
                null;

            const top_level_end_idx: usize = if (entry_mangled_name != null)
                self.findTopLevelInitEnd(hir, functions_start_idx) orelse functions_start_idx
            else
                functions_start_idx;

            try self.writeMainProgram(hir, w, top_level_end_idx, &peek_state, entry_mangled_name);

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

        /// Index in `hir.instructions` of the synthetic `Call` to the entry function at the end of
        /// the top-level program section (global init + main statements). Used to omit that call,
        /// trailing `Pop`, and `Halt` when `@doxa_program_main` invokes the entry function directly.
        pub fn findTopLevelInitEnd(self: *IRPrinter, hir: *const HIR.HIRProgram, limit: usize) ?usize {
            _ = self;
            for (hir.instructions[0..limit], 0..) |inst, idx| {
                if (inst != .Call) continue;
                const c = inst.Call;
                if (c.function_index >= hir.function_table.len) continue;
                const f = hir.function_table[c.function_index];
                if (f.is_entry and std.mem.eql(u8, f.qualified_name, c.qualified_name)) return idx;
            }
            return null;
        }

        pub fn writeMainProgram(
            self: *IRPrinter,
            hir: *const HIR.HIRProgram,
            w: anytype,
            top_level_end_idx: usize,
            peek_state: *PeekEmitState,
            entry_mangled_name: ?[]const u8,
        ) !void {
            try w.writeAll("define void @doxa_program_main() {\n");
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

            for (hir.instructions[0..top_level_end_idx]) |inst| {
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
                        try self.handleConst(w, &stack, &id, peek_state, hir.constant_pool, c.constant_id);
                        last_instruction_was_terminator = false;
                    },
                    .StoreAlias => |_| {
                        last_instruction_was_terminator = false;
                    },
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
                    .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                    .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                    .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                    .Dup => {
                        try self.handleDup(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Pop => {
                        self.handlePop(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Swap => {
                        self.handleSwap(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Arith => |a| {
                        try self.handleArith(w, &stack, &id, a);
                        last_instruction_was_terminator = false;
                    },
                    .Compare => |cmp| {
                        try self.handleCompare(w, &stack, &id, cmp);
                        last_instruction_was_terminator = false;
                    },
                    .LogicalOp => |lop| {
                        try self.handleLogicalOp(w, &stack, &id, lop);
                        last_instruction_was_terminator = false;
                },

                .Peek => |pk| {
                        try self.handlePeek(w, &stack, &id, pk, peek_state);
                        last_instruction_was_terminator = false;
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
                        const line = try std.fmt.allocPrint(self.allocator, "  br label %{s}\n", .{j.label});
                        defer self.allocator.free(line);
                        try w.writeAll(line);
                        stack.items.len = 0;
                        last_instruction_was_terminator = true;
                    },
                    .TypeCheck => |tc| {
                        try self.handleTypeCheck(w, &stack, &id, tc, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .PeekStruct => |ps| {
                        try self.handlePeekStruct(w, &stack, &id, ps, peek_state);
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
                        try w.writeAll("  ret void\n");
                        had_return = true;
                        last_instruction_was_terminator = true;
                    },
                    .Call => |c| {
                        try self.handleCall(w, &stack, &id, c, peek_state, hir);
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
                    .LoadModule => |lm| {
                        const gname = lm.module_name;
                        const fcount = lm.field_names.len;

                        // Look up struct metadata (pre-populated by writeModule scan)
                        const struct_fields = self.global_struct_field_types.get(gname);
                        const struct_names = self.global_struct_field_names.get(gname);
                        const struct_type_name = self.global_struct_type_names.get(gname);

                        if (fcount == 0) {
                            // Empty module struct — push a non-null sentinel pointer
                            const sentinel = try self.nextTemp(&id);
                            const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 1 to ptr\n", .{sentinel});
                            defer self.allocator.free(line);
                            try w.writeAll(line);

                            // Store the sentinel to the global so functions can load it
                            const module_gptr = try self.mangleGlobalName(gname);
                            defer self.allocator.free(module_gptr);
                            const store_module_line = try std.fmt.allocPrint(self.allocator, "  store ptr {s}, ptr {s}\n", .{ sentinel, module_gptr });
                            defer self.allocator.free(store_module_line);
                            try w.writeAll(store_module_line);

                            try stack.append(.{ .name = sentinel, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
                            last_instruction_was_terminator = false;
                            continue;
                        }

                        const struct_type_llvm = try self.buildI64StructType(fcount);
                        defer self.allocator.free(struct_type_llvm);

                        // Allocate struct on heap
                        const struct_size = fcount * @sizeOf(i64);
                        const size_reg = try self.nextTemp(&id);
                        const size_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ size_reg, struct_size });
                        defer self.allocator.free(size_line);
                        try w.writeAll(size_line);

                        const malloc_reg = try self.nextTemp(&id);
                        const malloc_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @malloc(i64 {s})\n", .{ malloc_reg, size_reg });
                        defer self.allocator.free(malloc_line);
                        try w.writeAll(malloc_line);

                        // Cast to struct pointer
                        const struct_ptr = try self.nextTemp(&id);
                        const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast ptr {s} to ptr\n", .{ struct_ptr, malloc_reg });
                        defer self.allocator.free(cast_line);
                        try w.writeAll(cast_line);

                        // Populate each field from module globals
                        var fi: usize = 0;
                        while (fi < fcount) : (fi += 1) {
                            const field_name = lm.field_names[fi];
                            const field_gptr = try self.mangleGlobalName(field_name);
                            defer self.allocator.free(field_gptr);

                            // Determine the module global's type and load accordingly
                            const field_st = self.global_types.get(field_name) orelse .PTR;
                            const field_llty = self.stackTypeToLLVMType(field_st);

                            const loaded_val = try self.nextTemp(&id);
                            const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ loaded_val, field_llty, field_gptr });
                            defer self.allocator.free(load_line);
                            try w.writeAll(load_line);

                            // Convert value to i64 storage representation
                            const loaded_sv = StackVal{ .name = loaded_val, .ty = field_st };
                            const loaded_storage = try self.convertValueToArrayStorage(w, loaded_sv, HIR.HIRType{ .String = {} }, &id);

                            // GEP to field position
                            const field_gep = try self.nextTemp(&id);
                            const gep_line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n", .{ field_gep, struct_type_llvm, struct_ptr, @as(i32, @intCast(fi)) });
                            defer self.allocator.free(gep_line);
                            try w.writeAll(gep_line);

                            // Store into struct field
                            const store_line = try std.fmt.allocPrint(self.allocator, "  store i64 {s}, ptr {s}\n", .{ loaded_storage.name, field_gep });
                            defer self.allocator.free(store_line);
                            try w.writeAll(store_line);
                        }

                        // Store the module struct pointer to the global so functions
                        // can load it when they encounter LoadModule for this module
                        // (critical for lazy-loaded modules accessed inside function bodies).
                        const module_gptr = try self.mangleGlobalName(gname);
                        defer self.allocator.free(module_gptr);
                        const store_module_line = try std.fmt.allocPrint(self.allocator, "  store ptr {s}, ptr {s}\n", .{ struct_ptr, module_gptr });
                        defer self.allocator.free(store_module_line);
                        try w.writeAll(store_module_line);

                        try stack.append(.{ .name = struct_ptr, .ty = .PTR, .struct_field_types = struct_fields, .struct_field_names = struct_names, .struct_type_name = struct_type_name });
                        last_instruction_was_terminator = false;
                    },
                    .LoadVar => |lv| {
                        try self.handleLoadVarGlobal(w, &stack, &id, lv.var_name);
                        last_instruction_was_terminator = false;
                    },
                    .PushStorageId => |psid| {
                        try self.handlePushStorageIdGlobal(w, &stack, &id, psid.var_name);
                        last_instruction_was_terminator = false;
                    },
                    .StoreVar => |sv| {
                        try self.handleStoreVarGlobal(w, &stack, &id, sv);
                        last_instruction_was_terminator = false;
                    },
                    .StoreDecl => |sd| {
                        try self.handleStoreDeclGlobal(w, &stack, &id, sd);
                        last_instruction_was_terminator = false;
                    },
                    .Return => |ret| {
                        if (ret.has_value) {
                            if (stack.items.len < 1) continue;
                            _ = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                        }
                        try w.writeAll("  ret void\n");
                        had_return = true;
                        last_instruction_was_terminator = true;
                    },
                    .Unreachable => |_| {
                        try w.writeAll("  unreachable\n");
                        stack.items.len = 0;
                        last_instruction_was_terminator = true;
                    },
                    .EnterScope => |_| {
                        // EnterScope is a no-op in LLVM IR generation
                        last_instruction_was_terminator = false;
                    },
                    .ExitScope => |_| {
                        // ExitScope is a no-op in LLVM IR generation
                        last_instruction_was_terminator = false;
                    },
                    .StoreFieldName => |_| {
                        // No-op: field names are captured at StructNew time
                        last_instruction_was_terminator = false;
                    },
                    .LoadAlias => |la| {
                        // Alias loads in global init — load from the global variable
                        const field_name = la.var_name;
                        const field_gptr = try self.mangleGlobalName(field_name);
                        defer self.allocator.free(field_gptr);
                        const field_st = self.global_types.get(field_name) orelse .PTR;
                        const field_llty = self.stackTypeToLLVMType(field_st);
                        const result = try self.nextTemp(&id);
                        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load {s}, ptr {s}\n", .{ result, field_llty, field_gptr });
                        defer self.allocator.free(load_line);
                        try w.writeAll(load_line);
                        try stack.append(.{ .name = result, .ty = field_st });
                        last_instruction_was_terminator = false;
                    },
                    .BindAlias => |_| {
                        if (stack.items.len < 1) continue;
                        _ = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        // BindAlias is a no-op in global init — the alias target
                        // is already a global that can be accessed directly.
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
                    .AssertFail => |af| {
                        try self.handleAssertFail(w, &stack, &id, af, peek_state);
                        last_instruction_was_terminator = false;
                    },
                    .ArrayConcat => {
                        try self.handleArrayConcat(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                }
            }

            if (entry_mangled_name) |mangled| {
                const call_line = try std.fmt.allocPrint(self.allocator, "  call void @{s}()\n", .{mangled});
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
            }

            // Ensure a valid exit if control falls through
            if (!had_return) {
                try w.writeAll("  ret void\n");
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
    };
}
