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
        /// B2: classify struct types that never need the runtime descriptor
        /// registry. A struct may skip it only when every field is scalar (so the
        /// typed word-copy clone can reproduce it) and it never needs the
        /// descriptor for reflection or the scope-tracking rehome walk. That is:
        /// it is not reflected (per the generator's whole-program predicate) and
        /// its type never appears in a function signature, container, union, or
        /// global declaration — the only ways a value can reach a runtime
        /// `Unknown` rehome or an out-of-function clone.
        pub fn computeDescriptorSkips(self: *IRPrinter, hir: *const HIR.HIRProgram) !void {
            const alloc = self.allocator;
            var fields_by_id = std.AutoHashMap(HIR.StructId, []HIR.HIRType).init(alloc);
            defer fields_by_id.deinit();
            var name_by_id = std.AutoHashMap(HIR.StructId, []const u8).init(alloc);
            defer name_by_id.deinit();

            for (hir.instructions) |inst| {
                switch (inst) {
                    .StructNew => |sn| {
                        fields_by_id.put(sn.struct_id, sn.field_types) catch {};
                        name_by_id.put(sn.struct_id, sn.type_name) catch {};
                    },
                    else => {},
                }
            }

            var needs = std.AutoHashMap(HIR.StructId, void).init(alloc);
            defer needs.deinit();

            for (hir.function_table) |func| {
                // A struct that crosses a call boundary as a by-value parameter
                // or result is snapshotted/rehomed from the callee side; keep its
                // descriptor so those runtime clones can resolve its layout
                // regardless of source spelling.
                self.markNestedStructs(&needs, func.return_type);
                for (func.param_types) |pt| self.markNestedStructs(&needs, pt);
            }
            for (hir.instructions) |inst| self.markInstructionNeeds(&needs, inst);

            if (self.force_struct_descriptors) return;

            var it = fields_by_id.iterator();
            while (it.next()) |entry| {
                const sid = entry.key_ptr.*;
                const field_types = entry.value_ptr.*;
                if (needs.contains(sid)) continue;
                if (!structFieldsAllScalar(field_types)) continue;
                const name = name_by_id.get(sid) orelse continue;
                if (self.reflectedContains(name)) continue;
                self.skip_descriptor_structs.put(name, {}) catch {};
            }
        }

        pub fn structFieldsAllScalar(field_types: []const HIR.HIRType) bool {
            if (field_types.len == 0) return false;
            for (field_types) |t| {
                switch (t) {
                    .Int, .Byte, .Float, .Tetra, .Enum, .Nothing => {},
                    else => return false,
                }
            }
            return true;
        }

        /// Whether a struct type name matches the generator's reflected set. The
        /// struct table uses qualified names while a literal carries the source
        /// spelling, so match on the final dot-segment too. Over-matching is safe
        /// (it only keeps a descriptor).
        pub fn reflectedContains(self: *IRPrinter, name: []const u8) bool {
            const reflected = self.reflected_structs orelse return false;
            if (reflected.contains(name)) return true;
            const bare = if (std.mem.lastIndexOfScalar(u8, name, '.')) |d| name[d + 1 ..] else name;
            var it = reflected.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                if (std.mem.eql(u8, key, name)) return true;
                const key_bare = if (std.mem.lastIndexOfScalar(u8, key, '.')) |d| key[d + 1 ..] else key;
                if (std.mem.eql(u8, key_bare, bare)) return true;
            }
            return false;
        }

        /// A type in a *value* position that merely *is* a struct does not by
        /// itself need a descriptor (typed clones cover scalar structs). Only a
        /// struct reachable *inside* a container whose runtime operations walk the
        /// registry — an array/map element, a union member, a function-type
        /// signature — forces it.
        pub fn markTypeNeeds(self: *IRPrinter, needs: *std.AutoHashMap(HIR.StructId, void), t: HIR.HIRType) void {
            switch (t) {
                .Struct => {},
                .Array => |inner| self.markNestedStructs(needs, inner.*),
                .Map => |kv| {
                    self.markNestedStructs(needs, kv.key.*);
                    self.markNestedStructs(needs, kv.value.*);
                },
                .Union => |u| for (u.members) |m| self.markNestedStructs(needs, m.*),
                .Function => |f| {
                    for (f.params) |p| self.markNestedStructs(needs, p.*);
                    self.markNestedStructs(needs, f.ret.*);
                },
                .Group, .Unknown, .Poison => self.force_struct_descriptors = true,
                else => {},
            }
        }

        /// Mark every struct reachable through container types. Used for positions
        /// whose runtime operations consult the descriptor: array/map elements,
        /// union members, and (nested) struct fields.
        pub fn markNestedStructs(self: *IRPrinter, needs: *std.AutoHashMap(HIR.StructId, void), t: HIR.HIRType) void {
            switch (t) {
                .Struct => |sid| {
                    if (sid == 0) {
                        self.force_struct_descriptors = true;
                    } else {
                        needs.put(sid, {}) catch {};
                    }
                },
                .Array => |inner| self.markNestedStructs(needs, inner.*),
                .Map => |kv| {
                    self.markNestedStructs(needs, kv.key.*);
                    self.markNestedStructs(needs, kv.value.*);
                },
                .Union => |u| for (u.members) |m| self.markNestedStructs(needs, m.*),
                .Function => |f| {
                    for (f.params) |p| self.markNestedStructs(needs, p.*);
                    self.markNestedStructs(needs, f.ret.*);
                },
                .Group, .Unknown, .Poison => self.force_struct_descriptors = true,
                else => {},
            }
        }

        /// Per-instruction descriptor requirements. A struct stored, returned, or
        /// held by value is cloned with the typed scalar path and needs nothing;
        /// what forces a descriptor is a struct that is an element/member of a
        /// container or a nested field of another struct.
        pub fn markInstructionNeeds(self: *IRPrinter, needs: *std.AutoHashMap(HIR.StructId, void), inst: Ctx.HIRInstruction) void {
            switch (inst) {
                .ArrayNew => |a| {
                    self.markNestedStructs(needs, a.element_type);
                    if (a.nested_element_type) |ne| self.markNestedStructs(needs, ne);
                },
                .Map => |m| {
                    self.markNestedStructs(needs, m.key_type);
                    self.markNestedStructs(needs, m.value_type);
                },
                .MapGet => |m| {
                    self.markNestedStructs(needs, m.key_type);
                    self.markNestedStructs(needs, m.value_type);
                },
                .MapSet => |m| self.markNestedStructs(needs, m.key_type),
                .StructNew => |sn| for (sn.field_types) |ft| self.markNestedStructs(needs, ft),
                .UnionConstruct => |u| self.markNestedStructs(needs, u.union_type),
                .GetField => |g| {
                    self.markTypeNeeds(needs, g.container_type);
                    self.markNestedStructs(needs, g.field_type);
                },
                .SetField => |s| {
                    self.markTypeNeeds(needs, s.container_type);
                    self.markNestedStructs(needs, s.field_type);
                },
                // Value-position types: a top-level struct here needs no
                // descriptor, but a container type can hide a struct element or
                // member that a runtime operation will walk the registry for.
                .StoreDecl => |sd| self.markTypeNeeds(needs, sd.declared_type),
                .StoreVar => |sv| self.markTypeNeeds(needs, sv.expected_type),
                .StoreAlias => |sa| self.markTypeNeeds(needs, sa.expected_type),
                .BindAlias => |ba| self.markTypeNeeds(needs, ba.target_type),
                .NarrowVar => |nv| self.markTypeNeeds(needs, nv.narrowed_type),
                .Return => |r| self.markTypeNeeds(needs, r.return_type),
                .Call => |c| self.markTypeNeeds(needs, c.return_type),
                .Peek => |p| self.markTypeNeeds(needs, p.value_type),
                .PeekStruct => |p| for (p.field_types) |ft| self.markNestedStructs(needs, ft),
                .Arith => |a| self.markTypeNeeds(needs, a.operand_type),
                .Convert => |c| {
                    self.markTypeNeeds(needs, c.from_type);
                    self.markTypeNeeds(needs, c.to_type);
                },
                .Compare => |c| self.markTypeNeeds(needs, c.operand_type),
                else => {},
            }
        }

        /// Backfill layout metadata for every struct the program did not
        /// construct. An entry whose field HIR types are not fully resolved is
        /// skipped: a half-known layout would silently mis-size a GEP, and the
        /// single-word fallback stays in effect instead.
        pub fn registerStructTableLayouts(self: *IRPrinter) !void {
            for (self.struct_table.entries.items) |entry| {
                const field_types = try self.allocator.alloc(HIR.HIRType, entry.fields.len);
                defer self.allocator.free(field_types);

                var resolved = true;
                for (entry.fields, 0..) |field, i| {
                    field_types[i] = field.hir_type;
                    if (field.hir_type == .Unknown) resolved = false;
                }
                if (!resolved) continue;

                if (!self.global_struct_field_types.contains(entry.key.?)) {
                    _ = try self.global_struct_field_types.put(entry.key.?, try self.allocator.dupe(HIR.HIRType, field_types));
                }
                if (!self.struct_fields_by_id.contains(entry.id)) {
                    _ = try self.struct_fields_by_id.put(entry.id, try self.allocator.dupe(HIR.HIRType, field_types));
                }
                if (!self.struct_type_names_by_id.contains(entry.id)) {
                    _ = try self.struct_type_names_by_id.put(entry.id, entry.key.?);
                }
                // `IRPrinter.deinit` frees every inner string of
                // `struct_field_names_by_type` unconditionally, so each one must
                // be printer-owned. The table's names belong to the analysis
                // arena — copying here (rather than borrowing `field.name`)
                // keeps that free from releasing the table's storage.
                if (!self.struct_field_names_by_type.contains(entry.key.?)) {
                    const owned_names = try self.allocator.alloc([]const u8, entry.fields.len);
                    var built: usize = 0;
                    errdefer {
                        for (owned_names[0..built]) |name| self.allocator.free(name);
                        self.allocator.free(owned_names);
                    }
                    while (built < entry.fields.len) : (built += 1) {
                        owned_names[built] = try self.allocator.dupe(u8, entry.fields[built].name);
                    }
                    _ = try self.struct_field_names_by_type.put(entry.key.?, owned_names);
                }
                // Enum field type names must be known before the descriptor is
                // first created. A fixed-array default fill can construct an
                // element struct (and materialize its descriptor) before any
                // `StructNew` for that type, so relying on the construction site
                // alone would cache a descriptor whose enum fields render as bare
                // discriminants. Names are borrowed from the analysis arena.
                if (!self.struct_field_enum_type_names_by_type.contains(entry.key.?)) {
                    const enum_names = try self.allocator.alloc(?[]const u8, entry.fields.len);
                    for (entry.fields, 0..) |field, i| {
                        enum_names[i] = null;
                        if (field.type_info.base != .Custom) continue;
                        const custom = field.type_info.custom_type orelse continue;
                        if (self.enum_table.idOf(custom.resolved())) |eid| enum_names[i] = self.enum_table.keyOf(eid);
                    }
                    _ = try self.struct_field_enum_type_names_by_type.put(entry.key.?, enum_names);
                }
            }
        }

        pub fn writeModule(self: *IRPrinter, hir: *const HIR.HIRProgram, w: anytype) !void {
            for (hir.zig_functions) |function| try self.zig_fn_param_types.put(function.link_name, function.param_types);
            try self.computeDescriptorSkips(hir);
            // Phase D-1 follow-on: compute loop-head variable ranges before any
            // body is emitted, so `varRange` can answer for loop-carried values.
            try self.prepareLoopRanges(hir);
            try w.writeAll("declare void @doxa_write_cstr(ptr, i64)\n");
            try w.writeAll("declare void @doxa_write_raw(ptr)\n");
            try w.writeAll("declare void @doxa_write_stderr(ptr, i64)\n");
            try w.writeAll("declare void @doxa_exit(i64) noreturn\n");
            try w.writeAll("declare void @doxa_panic(ptr, i64) noreturn\n");
            try w.writeAll("declare void @doxa_trap_div_by_zero() noreturn\n");
            try w.writeAll("");
            try w.writeAll("declare void @doxa_print_i64(i64)\n");
            try w.writeAll("declare void @doxa_print_u64(i64)\n");
            try w.writeAll("declare void @doxa_print_f64(double)\n");
            try w.writeAll("declare void @doxa_print_byte(i64)\n");
            try w.writeAll("declare i64 @doxa_str_len(ptr, i64)\n");
            try w.writeAll("declare void @doxa_str_concat(ptr, i64, ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_str_clone_at(i64, ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_str_clone_root(ptr, i64, ptr, ptr)\n");
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
            try w.writeAll("declare void @doxa_value_to_string(ptr, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_pack_bytes(ptr, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_unpack_bytes(ptr, i64)\n");
            try w.writeAll("declare void @doxa_debug_peek(ptr)\ndeclare void @doxa_peek_string(ptr, i64)\ndeclare void @doxa_peek_end()\n");
            try w.writeAll("declare void @doxa_print_array_hdr(ptr)\n");
            try w.writeAll("declare i1 @doxa_str_eq(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare ptr @doxa_array_new(i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_new_at(i64, i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_new_nested(i64, i64, i64, ptr, i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_from_fixed_at(i64, ptr, ptr, i64, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_from_fixed_structs_at(i64, ptr, i64, i64, ptr)\n");
            try w.writeAll("declare void @doxa_array_fill_default_structs(ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_clone(ptr)\n");
            try w.writeAll("declare ptr @doxa_array_clone_at(i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_clone_root(ptr)\n");
            try w.writeAll("declare ptr @doxa_array_rehome_at(i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_rehome_root(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_len(ptr)\n");
            try w.writeAll("declare i64 @doxa_array_get_i64(ptr, i64)\n");
            try w.writeAll("declare void @doxa_array_get_str(ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare void @doxa_array_set_i64(ptr, i64, i64)\n");
            try w.writeAll("declare void @doxa_array_set_str(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare ptr @doxa_array_concat(ptr, ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_insert(ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_array_insert_str(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare ptr @doxa_array_remove(ptr, i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_remove_str(ptr, i64, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_array_slice(ptr, i64, i64)\n");
            try w.writeAll("declare ptr @doxa_map_new(i64, i64, i64)\n");
            try w.writeAll("declare void @doxa_map_set_i64(ptr, i64, i64)\n");
            try w.writeAll("declare void @doxa_map_set_else_i64(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_map_get_i64(ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_map_try_get_i64(ptr, i64, ptr)\n");
            try w.writeAll("declare double @llvm.pow.f64(double, double)\n");
            try w.writeAll("declare void @doxa_set_args(i32, ptr)\n");
            try w.writeAll("declare i64 @doxa_int(double)\n");
            // Type check ABI (i64 payload + type tag + target type string).
            try w.writeAll("declare i64 @doxa_type_check(i64, i64, ptr)\n");
            try w.writeAll("declare void @doxa_print_value(ptr)\n");
            try w.writeAll("declare void @doxa_clone_doxa_value_at(i64, ptr)\n");
            try w.writeAll("declare void @doxa_clone_doxa_value_root(ptr)\n");
            try w.writeAll("declare i64 @doxa_find_array(ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_array_str(ptr, ptr, i64)\n");
            try w.writeAll("declare i64 @doxa_find_str(ptr, i64, ptr, i64)\n");
            try w.writeAll("declare void @doxa_struct_register(ptr, ptr)\n");
            try w.writeAll("declare void @doxa_struct_register_at(i64, ptr, ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_clone_at(i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_clone_root(ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_clone_scalar_at(i64, i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_clone_scalar_root(i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_rehome_at(i64, ptr)\n");
            try w.writeAll("declare ptr @doxa_struct_rehome_root(ptr)\n");
            try w.writeAll("declare void @doxa_enum_register(ptr)\n");
            try w.writeAll("declare ptr @doxa_scope_alloc(i64, i64)\n");
            try w.writeAll("declare ptr @doxa_scope_alloc_at(i64, i64, i64)\n");
            try w.writeAll("declare void @doxa_scope_enter()\n");
            try w.writeAll("declare void @doxa_scope_exit()\n");
            try w.writeAll("declare void @doxa_scope_reset()\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_gt(ptr, ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_exists_quantifier_eq(ptr, ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_gt(ptr, ptr, i64)\n");
            try w.writeAll("declare i8 @doxa_forall_quantifier_eq(ptr, ptr, i64)\n");
            try w.writeAll("declare void @doxa_clear(ptr)\n");
            try w.writeAll("declare ptr @doxa_array_range(i64, i64)\n");
            try w.writeAll("declare void @doxa_trap_unreachable()\n");
            try w.writeAll("declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)\n");
            // Phase D-1: the checked arithmetic lowering uses the overflow
            // intrinsics and a real trap. Declared only when the policy is
            // trapping, so a wrapping build carries no dead intrinsic.
            if (self.arith_overflow == .Trap) {
                try w.writeAll("declare void @llvm.trap()\n");
                try w.writeAll("declare { i64, i1 } @llvm.sadd.with.overflow.i64(i64, i64)\n");
                try w.writeAll("declare { i64, i1 } @llvm.ssub.with.overflow.i64(i64, i64)\n");
                try w.writeAll("declare { i64, i1 } @llvm.smul.with.overflow.i64(i64, i64)\n");
            }

            // The program's inline-Zig callees are external: declare each with
            // typed parameters for the correct ABI on every architecture.
            for (hir.zig_functions) |function| {
                const ret_ty = if (function.return_type == .String) "void" else self.hirTypeToLLVMType(function.return_type, false);

                var params_buf = std.array_list.Managed(u8).init(self.allocator);
                defer params_buf.deinit();
                for (function.param_types, 0..) |pt, i| {
                    if (i > 0) try params_buf.appendSlice(", ");
                    switch (pt) {
                        .String => try params_buf.appendSlice("ptr, i64"),
                        .Int => try params_buf.appendSlice("i64"),
                        .Float => try params_buf.appendSlice("double"),
                        .Byte => try params_buf.appendSlice("i8"),
                        .Tetra => try params_buf.appendSlice("i1"),
                        .Nothing => try params_buf.appendSlice("void"),
                        // Arrays cross the inline-Zig ABI as a single opaque
                        // pointer, matching the generated wrapper's signature.
                        .Array => try params_buf.appendSlice("ptr"),
                        else => try params_buf.appendSlice("i64"),
                    }
                }
                // A string return is written through two out-pointers.
                if (function.return_type == .String) {
                    if (params_buf.items.len > 0) try params_buf.appendSlice(", ");
                    try params_buf.appendSlice("ptr, ptr");
                }

                const decl = try std.fmt.allocPrint(self.allocator, "declare {s} @{s}({s})\n", .{ ret_ty, function.link_name, params_buf.items });
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
            try w.writeAll("%DoxaValue = type { i32, i32, i64, i64 }\n");
            try w.writeAll("%DoxaString = type { ptr, i64 }\n");
            try w.writeAll("%ArrayHeader = type { ptr, i64, i64, i64, i64, ptr }\n\n");
            try w.writeAll("@.doxa.nl = private constant [2 x i8] c\"\\0A\\00\"\n");
            try w.writeAll("@.doxa.empty = private constant [1 x i8] c\"\\00\"\n");
            try w.writeAll("@.doxa.arr_open = private constant [2 x i8] c\"[\\00\"\n");
            try w.writeAll("@.doxa.arr_close = private constant [2 x i8] c\"]\\00\"\n");
            try w.writeAll("@.doxa.arr_sep = private constant [3 x i8] c\", \\00\"\n");

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
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_xor_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_nand_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_nor_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 0, i8 0, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 0, i8 0, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 0, i8 0, i8 1]\n");
            try w.writeAll("]\n");
            try w.writeAll("@tetra_implies_lut = private constant [4 x [4 x i8]] [\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 0, i8 1, i8 1, i8 0],\n");
            try w.writeAll("  [4 x i8] [i8 1, i8 1, i8 1, i8 1]\n");
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

            // `StructNew` is the usual source of struct layout, but a struct can
            // be read without ever being constructed in this program: a match arm
            // deconstructs a group payload it never receives, or an `as` narrowing
            // types a binding the program never builds. The semantic struct table
            // owns layout for every struct, so backfill what `StructNew` did not
            // declare — construction sites must not decide what a GEP may index.
            try self.registerStructTableLayouts();

            for (hir.function_table) |func| {
                try self.collectFunctionStructReturnInfo(hir, func, &func_start_labels);
            }

            // Pre-scan all function instructions to discover referenced globals
            // before the global declaration pass, so each is declared in the IR
            // ahead of use.
            for (hir.function_table) |func| {
                const range = self.getFunctionRange(hir, func, &func_start_labels) orelse continue;
                for (hir.instructions[range.start..range.end]) |inst| {
                    switch (inst) {
                        .PushStorageId => |psid| {
                            if (psid.scope_kind == .GlobalLocal or psid.scope_kind == .ModuleGlobal) {
                                if (!self.defined_globals.contains(psid.var_name)) {
                                    _ = try self.global_types.put(psid.var_name, .PTR);
                                    _ = try self.defined_globals.put(psid.var_name, true);
                                }
                            }
                        },
                        .LoadVar => |lv| {
                            if (lv.scope_kind == .GlobalLocal or lv.scope_kind == .ModuleGlobal) {
                                if (!self.defined_globals.contains(lv.var_name)) {
                                    _ = try self.global_types.put(lv.var_name, .PTR);
                                    _ = try self.defined_globals.put(lv.var_name, true);
                                }
                            }
                        },
                        else => {},
                    }
                }
            }

            var peek_state = PeekEmitState.init(self.allocator, &self.peek_string_counter);
            defer peek_state.deinit();

            const entry_function = blk: {
                for (hir.function_table) |f| {
                    if (f.is_entry) break :blk f;
                }
                break :blk null;
            };
            const entry_mangled_name_owned = if (entry_function) |ef|
                try self.functionSymbol(ef)
            else
                null;
            defer if (entry_mangled_name_owned) |name| self.allocator.free(name);
            const entry_mangled_name: ?[]const u8 = if (entry_mangled_name_owned) |name|
                name
            else
                null;

            const top_level_end_idx: usize = if (entry_mangled_name != null)
                self.findTopLevelInitEnd(hir, functions_start_idx) orelse functions_start_idx
            else
                functions_start_idx;

            try self.writeMainProgram(hir, w, top_level_end_idx, &peek_state, entry_mangled_name, if (entry_function) |ef| ef.return_type else null);

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

            // Every `define` above references `#0`; emit its body once here.
            try w.writeAll(Ctx.function_attr_block);
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
                const fi = c.function_index orelse continue;
                if (fi >= hir.function_table.len) continue;
                const f = hir.function_table[fi];
                if (f.is_entry and std.mem.eql(u8, f.qualified_name, c.qualified_name)) return idx;
            }
            return null;
        }

        pub fn writeMainProgram(
            self: *IRPrinter,
            hir: *const HIR.HIRProgram,
            outer_w: anytype,
            top_level_end_idx: usize,
            peek_state: *PeekEmitState,
            entry_mangled_name: ?[]const u8,
            entry_return_type: ?HIR.HIRType,
        ) !void {
            const prev_peek_state = self.active_peek_state;
            self.active_peek_state = peek_state;
            defer self.active_peek_state = prev_peek_state;

            try outer_w.writeAll("define void @doxa_program_main()" ++ Ctx.function_attr_group ++ " {\n");
            try outer_w.writeAll("entry:\n");
            try outer_w.writeAll("  %str_out_ptr = alloca ptr\n");
            try outer_w.writeAll("  %str_out_len = alloca i64\n");
            // Root scope arena: lives for the whole program and is never exited.
            try outer_w.writeAll("  call void @doxa_scope_enter()\n");
            self.entry_str_out_ptr = "%str_out_ptr";
            self.entry_str_out_len = "%str_out_len";

            // Stage the body so synthetic-header allocas discovered while
            // emitting it can be replayed in the entry block. See
            // `entry_allocas`.
            self.entry_allocas.clearRetainingCapacity();
            self.synth_header_counter = 0;
            var body_alloc = std.Io.Writer.Allocating.init(self.allocator);
            defer body_alloc.deinit();
            const w = &body_alloc.writer;

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

            self.clearNarrowedVars();
            self.var_regions.clearRetainingCapacity();
            self.var_ranges.clearRetainingCapacity();
            self.var_range_blocks.clearRetainingCapacity();
            self.current_block = "entry";
            self.scope_depth = 0;
            var jump_targets = try self.collectLiveJumpTargets(hir.instructions[0..top_level_end_idx]);
            defer jump_targets.deinit();
            for (hir.instructions[0..top_level_end_idx], 0..) |inst, inst_index| {
                self.verifyEnter("<top level>", inst_index, inst);
                const tag = std.meta.activeTag(inst);
                const requires_new_block = switch (tag) {
                    .Label => false,
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
                    self.current_block = dead_label;
                    stack.items.len = 0;
                    last_instruction_was_terminator = false;
                }
                switch (inst) {
                    .Const => |c| {
                        try self.handleConst(w, &stack, &id, peek_state, hir.constant_pool, c.constant_id);
                        last_instruction_was_terminator = false;
                    },
                    .StoreAlias => {
                        last_instruction_was_terminator = false;
                    },
                    .NarrowVar => |nv| {
                        try self.narrowVariable(nv.var_name, nv.narrowed_type);
                        last_instruction_was_terminator = false;
                    },
                    .RestoreVar => |rv| {
                        self.restoreVariable(rv.var_name);
                        last_instruction_was_terminator = false;
                    },
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
                    .StructNew => |sn| try self.emitStructNew(w, &stack, &id, sn, peek_state),
                    .GetField => |gf| try self.emitGetField(w, &stack, &id, gf),
                    .SetField => |sf| try self.emitSetField(w, &stack, &id, sf),
                    .Dup => {
                        try self.handleDup(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Pop => {
                        try self.handlePop(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Swap => {
                        try self.handleSwap(&stack);
                        last_instruction_was_terminator = false;
                    },
                    .Arith => |a| {
                        try self.handleArith(w, &stack, &id, a, &current_block);
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
                        // Not fallen into and not jumped to: unreachable code,
                        // skipped up to the next live label.
                        if (last_instruction_was_terminator and !jump_targets.contains(lbl.name)) continue;
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
                        self.current_block = lbl.name;
                        if (std.mem.startsWith(u8, lbl.name, "loop_start")) {
                            if (self.loop_head_envs.getPtr(lbl.name)) |m| self.active_loop_range = m;
                        } else if (std.mem.startsWith(u8, lbl.name, "loop_exit")) {
                            self.active_loop_range = null;
                        }
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
                    .Halt => {
                        try w.writeAll("  ret void\n");
                        had_return = true;
                        last_instruction_was_terminator = true;
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
                    .Convert => |conv| {
                        try self.handleConvert(w, &stack, &id, conv);
                        last_instruction_was_terminator = false;
                    },
                    .StringOp => |sop| {
                        try self.handleStringOp(w, &stack, &id, sop, peek_state);
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
                            try self.requireStack(&stack, 1);
                            _ = stack.items[stack.items.len - 1];
                            stack.items.len -= 1;
                        }
                        try w.writeAll("  ret void\n");
                        had_return = true;
                        last_instruction_was_terminator = true;
                    },
                    .Unreachable => {
                        try w.writeAll("  call void @doxa_trap_unreachable()\n");
                        try w.writeAll("  unreachable\n");
                        stack.items.len = 0;
                        last_instruction_was_terminator = true;
                    },
                    .EnterScope => {
                        try w.writeAll("  call void @doxa_scope_enter()\n");
                        self.scope_depth += 1;
                        last_instruction_was_terminator = false;
                    },
                    .ResetScope => {
                        try w.writeAll("  call void @doxa_scope_reset()\n");
                        last_instruction_was_terminator = false;
                    },
                    .ExitScope => |s| {
                        try w.writeAll("  call void @doxa_scope_exit()\n");
                        if (!self.exited_scopes.contains(s.scope_id)) {
                            self.scope_depth -|= 1;
                            self.exited_scopes.put(s.scope_id, {}) catch {};
                        }
                        last_instruction_was_terminator = false;
                    },
                    .StoreFieldName => {
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
                    .BindAlias => {
                        try self.requireStack(&stack, 1);
                        _ = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        // BindAlias is a no-op in global init — the alias target
                        // is already a global that can be accessed directly.
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
                    .UnionConstruct => |uc| {
                        try self.handleUnionConstruct(w, &stack, &id, uc);
                        last_instruction_was_terminator = false;
                    },
                    .AssertFail => |af| {
                        try self.handleAssertFail(w, &stack, &id, af, peek_state);
                        last_instruction_was_terminator = true;
                    },
                    .ArrayConcat => {
                        try self.emitArrayConcat(w, &stack, &id);
                        last_instruction_was_terminator = false;
                    },
                }
            }

            if (entry_mangled_name) |mangled| {
                // The call must carry the entry's real return type. Emitting a
                // hardcoded `void` call for an entry that returns a by-value
                // aggregate (union/group -> `%DoxaValue`, string ->
                // `%DoxaString`) leaves the hidden sret pointer uninitialised,
                // so the callee writes the result through garbage and the
                // process dies at exit.
                const entry_ret = if (entry_return_type) |rt| self.hirTypeToLLVMType(rt, false) else "void";
                const call_line = try std.fmt.allocPrint(self.allocator, "  call {s} @{s}()\n", .{ entry_ret, mangled });
                defer self.allocator.free(call_line);
                try w.writeAll(call_line);
            }

            // Ensure a valid exit if control falls through
            if (!had_return) {
                try w.writeAll("  ret void\n");
            }

            try w.writeAll("}\n");

            for (self.entry_allocas.items) |line| {
                try outer_w.writeAll(line);
                self.allocator.free(line);
            }
            self.entry_allocas.clearRetainingCapacity();
            const body_bytes = try body_alloc.toOwnedSlice();
            defer self.allocator.free(body_bytes);
            try outer_w.writeAll(body_bytes);
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
                        } else if (ret.has_value and func.return_type == .Struct and !self.function_struct_return_fields.contains(func.qualified_name)) {
                            // Pass-through factories (e.g. `executable` delegating to
                            // `Builder.new`) contain no StructNew of their own, so the
                            // field metadata is taken from the declared return type.
                            const sid = func.return_type.Struct;
                            if (self.struct_fields_by_id.get(sid)) |fts| {
                                _ = try self.function_struct_return_fields.put(func.qualified_name, try self.allocator.dupe(HIR.HIRType, fts));
                                if (self.struct_type_names_by_id.get(sid)) |tn| {
                                    _ = try self.function_struct_return_type_names.put(func.qualified_name, tn);
                                }
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
