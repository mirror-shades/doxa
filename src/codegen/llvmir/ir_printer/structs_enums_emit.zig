const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const HIRInstruction = Ctx.HIRInstruction;
    const PeekEmitState = Ctx.PeekEmitState;
    const PeekStringInfo = Ctx.PeekStringInfo;
    const StackType = Ctx.StackType;
    const StackVal = Ctx.StackVal;
    const EnumVariantMeta = Ctx.EnumVariantMeta;
    const internPeekString = Ctx.internPeekString;

    return struct {
    pub fn emitPeekInstruction(
        self: *IRPrinter,
        w: anytype,
        pk: anytype,
        value: StackVal,
        id: *usize,
        state: *PeekEmitState,
    ) !void {
        var array_type_str_owned: ?[]const u8 = null;
        var peek_type_str_owned: ?[]const u8 = null;
        defer {
            if (array_type_str_owned) |s| self.allocator.free(s);
            if (peek_type_str_owned) |s| self.allocator.free(s);
        }

        const type_slice = blk_type: {
            // 1) Prefer explicit enum type names from HIR Peek (e.g., "Species")
            if (pk.enum_type_name) |enum_type_name| break :blk_type enum_type_name;

            // 2) Then prefer enum type names attached to the value (for plain enums)
            if (value.enum_type_name) |enum_type_name| break :blk_type enum_type_name;

            // 2.5) Hard guard: if the value is a tetra on the stack, never label it as something else.
            // This prevents incorrect "string" labels when the peek's HIR type is missing/mismatched.
            if (value.ty == .I2) break :blk_type "tetra";

            // 3) Next, prefer the HIR Peek value_type when it carries more precise
            //    information than the raw stack type.
            switch (pk.value_type) {
                .String => break :blk_type "string",
                .Float => break :blk_type "float",
                .Int => break :blk_type "int",
                .Byte => break :blk_type "byte",
                .Tetra => break :blk_type "tetra",
                .Enum => break :blk_type "enum",
                .Array => {
                    peek_type_str_owned = try self.hirTypeToTypeString(self.allocator, pk.value_type);
                    break :blk_type peek_type_str_owned.?;
                },
                else => {},
            }

            // 4) Fallback to stack type and any attached struct metadata.
            break :blk_type switch (value.ty) {
                .I64 => "int",
                .F64 => "float",
                .PTR => blk: {
                    // Distinguish between arrays, structs, and raw strings
                    if (value.array_type) |element_type| {
                        const elem_type_str = try self.hirTypeToTypeString(self.allocator, element_type);
                        defer self.allocator.free(elem_type_str);
                        array_type_str_owned = try std.fmt.allocPrint(self.allocator, "{s}[]", .{elem_type_str});
                        break :blk array_type_str_owned.?;
                    }

                    if (value.struct_type_name) |tn| {
                        break :blk tn;
                    }

                    // If we have struct field type metadata, try to recover the
                    // original struct name so peeks show "Point" instead of "string".
                    if (value.struct_field_types) |fts| {
                        var it = self.global_struct_field_types.iterator();
                        while (it.next()) |entry| {
                            const candidate = entry.value_ptr.*;
                            if (candidate.ptr == fts.ptr and candidate.len == fts.len) {
                                break :blk entry.key_ptr.*;
                            }
                        }
                        // Fallback when we can't find an exact metadata match
                        break :blk "struct";
                    }

                    break :blk "string";
                },
                .I8 => "value",
                .I1 => "value",
                .I2 => "tetra",
                .Nothing => "nothing",
                .Value => "value",
            };
        };
        const type_info = try internPeekString(
            self.allocator,
            &state.string_map,
            &state.strings,
            state.next_id_ptr,
            &state.globals,
            type_slice,
        );

        var name_info: ?PeekStringInfo = null;
        if (pk.name) |nm| {
            name_info = try internPeekString(
                self.allocator,
                &state.string_map,
                &state.strings,
                state.next_id_ptr,
                &state.globals,
                nm,
            );
        }

        var file_info: ?PeekStringInfo = null;
        var has_loc: u32 = 0;
        var line_val: u32 = 0;
        var col_val: u32 = 0;
        if (pk.location) |loc| {
            file_info = try internPeekString(
                self.allocator,
                &state.string_map,
                &state.strings,
                state.next_id_ptr,
                &state.globals,
                loc.file,
            );
            line_val = std.math.cast(u32, loc.range.start_line) orelse std.math.maxInt(u32);
            col_val = std.math.cast(u32, loc.range.start_col) orelse std.math.maxInt(u32);
            has_loc = 1;
        }

        const info_var = try self.nextTemp(id);
        defer self.allocator.free(info_var);
        const alloca_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaPeekInfo\n", .{info_var});
        defer self.allocator.free(alloca_line);
        try w.writeAll(alloca_line);

        const file_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(file_field_ptr);
        const file_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 0\n",
            .{ file_field_ptr, info_var },
        );
        defer self.allocator.free(file_field_line);
        try w.writeAll(file_field_line);

        if (file_info) |fi| {
            const file_ptr_var = try self.nextTemp(id);
            defer self.allocator.free(file_ptr_var);
            const file_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                .{ file_ptr_var, fi.length, fi.name },
            );
            defer self.allocator.free(file_gep);
            try w.writeAll(file_gep);

            const store_file = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr {s}, ptr {s}\n",
                .{ file_ptr_var, file_field_ptr },
            );
            defer self.allocator.free(store_file);
            try w.writeAll(store_file);
        } else {
            const store_null_file = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr null, ptr {s}\n",
                .{file_field_ptr},
            );
            defer self.allocator.free(store_null_file);
            try w.writeAll(store_null_file);
        }

        const name_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(name_field_ptr);
        const name_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 1\n",
            .{ name_field_ptr, info_var },
        );
        defer self.allocator.free(name_field_line);
        try w.writeAll(name_field_line);

        if (name_info) |ni| {
            const name_ptr_var = try self.nextTemp(id);
            defer self.allocator.free(name_ptr_var);
            const name_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                .{ name_ptr_var, ni.length, ni.name },
            );
            defer self.allocator.free(name_gep);
            try w.writeAll(name_gep);

            const store_name = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr {s}, ptr {s}\n",
                .{ name_ptr_var, name_field_ptr },
            );
            defer self.allocator.free(store_name);
            try w.writeAll(store_name);
        } else {
            const store_null_name = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr null, ptr {s}\n",
                .{name_field_ptr},
            );
            defer self.allocator.free(store_null_name);
            try w.writeAll(store_null_name);
        }

        const type_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(type_field_ptr);
        const type_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 2\n",
            .{ type_field_ptr, info_var },
        );
        defer self.allocator.free(type_field_line);
        try w.writeAll(type_field_line);

        const type_ptr_var = try self.nextTemp(id);
        defer self.allocator.free(type_ptr_var);
        const type_gep = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
            .{ type_ptr_var, type_info.length, type_info.name },
        );
        defer self.allocator.free(type_gep);
        try w.writeAll(type_gep);

        const store_type = try std.fmt.allocPrint(
            self.allocator,
            "  store ptr {s}, ptr {s}\n",
            .{ type_ptr_var, type_field_ptr },
        );
        defer self.allocator.free(store_type);
        try w.writeAll(store_type);

        const members_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(members_field_ptr);
        const members_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 3\n",
            .{ members_field_ptr, info_var },
        );
        defer self.allocator.free(members_field_line);
        try w.writeAll(members_field_line);

        var member_count: i32 = 0;
        var active_index: i32 = -1;
        var active_index_var: ?[]const u8 = null;

        if (pk.union_members) |members| {
            member_count = @intCast(members.len);

            // Intern all union member strings
            var member_infos = try self.allocator.alloc(PeekStringInfo, members.len);
            defer self.allocator.free(member_infos);
            for (members, 0..) |member, idx| {
                member_infos[idx] = try internPeekString(
                    self.allocator,
                    &state.string_map,
                    &state.strings,
                    state.next_id_ptr,
                    &state.globals,
                    member,
                );
            }

            // If the value is a canonical union, derive active index from reserved bits
            if (value.ty == .Value) {
                const reserved = try self.nextTemp(id);
                const reserved_extract = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = extractvalue %DoxaValue {s}, 1\n",
                    .{ reserved, value.name },
                );
                defer self.allocator.free(reserved_extract);
                try w.writeAll(reserved_extract);

                const is_union = try self.nextTemp(id);
                const is_union_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = and i32 {s}, 2147483648\n",
                    .{ is_union, reserved },
                );
                defer self.allocator.free(is_union_line);
                try w.writeAll(is_union_line);

                const cmp = try self.nextTemp(id);
                const cmp_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = icmp ne i32 {s}, 0\n",
                    .{ cmp, is_union },
                );
                defer self.allocator.free(cmp_line);
                try w.writeAll(cmp_line);

                const member_bits = try self.nextTemp(id);
                const member_line = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = and i32 {s}, 65535\n",
                    .{ member_bits, reserved },
                );
                defer self.allocator.free(member_line);
                try w.writeAll(member_line);

                const member_i32 = try self.nextTemp(id);
                const member_sel = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = select i1 {s}, i32 {s}, i32 -1\n",
                    .{ member_i32, cmp, member_bits },
                );
                defer self.allocator.free(member_sel);
                try w.writeAll(member_sel);

                active_index_var = member_i32;
            }

            if (active_index_var == null and pk.value_type == .Union) {
                const idx = self.findUnionMemberIndex(pk.value_type, value);
                active_index = @intCast(idx);
            }

            // Create array of pointers to union member strings
            const array_name = try std.fmt.allocPrint(self.allocator, "@.peek.union.{d}", .{state.next_id_ptr.*});
            defer self.allocator.free(array_name);
            state.next_id_ptr.* += 1;

            // Emit global array definition
            var array_def = std.array_list.Managed(u8).init(self.allocator);
            defer array_def.deinit();
            try array_def.writer().print("  {s} = constant [{d} x ptr] [", .{ array_name, members.len });
            for (member_infos, 0..) |mi, idx| {
                if (idx > 0) try array_def.writer().print(", ", .{});
                try array_def.writer().print("ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)", .{ mi.length, mi.name });
            }
            try array_def.writer().print("]\n", .{});
            try state.globals.append(try array_def.toOwnedSlice());

            // Get pointer to array
            const array_ptr = try self.nextTemp(id);
            defer self.allocator.free(array_ptr);
            const array_gep = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds [{d} x ptr], ptr {s}, i64 0, i64 0\n",
                .{ array_ptr, members.len, array_name },
            );
            defer self.allocator.free(array_gep);
            try w.writeAll(array_gep);

            const store_members = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr {s}, ptr {s}\n",
                .{ array_ptr, members_field_ptr },
            );
            defer self.allocator.free(store_members);
            try w.writeAll(store_members);
        } else {
            const store_members = try std.fmt.allocPrint(
                self.allocator,
                "  store ptr null, ptr {s}\n",
                .{members_field_ptr},
            );
            defer self.allocator.free(store_members);
            try w.writeAll(store_members);
        }

        const count_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(count_field_ptr);
        const count_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 4\n",
            .{ count_field_ptr, info_var },
        );
        defer self.allocator.free(count_field_line);
        try w.writeAll(count_field_line);

        const store_count = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ member_count, count_field_ptr },
        );
        defer self.allocator.free(store_count);
        try w.writeAll(store_count);

        const active_field_ptr = try self.nextTemp(id);
        defer self.allocator.free(active_field_ptr);
        const active_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 5\n",
            .{ active_field_ptr, info_var },
        );
        defer self.allocator.free(active_field_line);
        try w.writeAll(active_field_line);

        if (active_index_var) |active_var| {
            const store_active = try std.fmt.allocPrint(
                self.allocator,
                "  store i32 {s}, ptr {s}\n",
                .{ active_var, active_field_ptr },
            );
            defer self.allocator.free(store_active);
            try w.writeAll(store_active);
        } else {
            const store_active = try std.fmt.allocPrint(
                self.allocator,
                "  store i32 {d}, ptr {s}\n",
                .{ active_index, active_field_ptr },
            );
            defer self.allocator.free(store_active);
            try w.writeAll(store_active);
        }

        const has_loc_ptr = try self.nextTemp(id);
        defer self.allocator.free(has_loc_ptr);
        const has_loc_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 6\n",
            .{ has_loc_ptr, info_var },
        );
        defer self.allocator.free(has_loc_line);
        try w.writeAll(has_loc_line);

        const store_has_loc = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ has_loc, has_loc_ptr },
        );
        defer self.allocator.free(store_has_loc);
        try w.writeAll(store_has_loc);

        const line_ptr = try self.nextTemp(id);
        defer self.allocator.free(line_ptr);
        const line_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 7\n",
            .{ line_ptr, info_var },
        );
        defer self.allocator.free(line_field_line);
        try w.writeAll(line_field_line);

        const store_line_val = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ line_val, line_ptr },
        );
        defer self.allocator.free(store_line_val);
        try w.writeAll(store_line_val);

        const col_ptr = try self.nextTemp(id);
        defer self.allocator.free(col_ptr);
        const col_field_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds %DoxaPeekInfo, ptr {s}, i32 0, i32 8\n",
            .{ col_ptr, info_var },
        );
        defer self.allocator.free(col_field_line);
        try w.writeAll(col_field_line);

        const store_col_val = try std.fmt.allocPrint(
            self.allocator,
            "  store i32 {d}, ptr {s}\n",
            .{ col_val, col_ptr },
        );
        defer self.allocator.free(store_col_val);
        try w.writeAll(store_col_val);

        const call_debug = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_debug_peek(ptr {s})\n",
            .{info_var},
        );
        defer self.allocator.free(call_debug);
        try w.writeAll(call_debug);
    }

    pub fn hydrateStructMetadata(
        self: *IRPrinter,
        val: *StackVal,
        peek_name: ?[]const u8,
    ) void {
        if (val.struct_field_types == null or val.struct_field_names == null or val.struct_type_name == null) {
            if (peek_name) |name| {
                if (val.struct_field_types == null) {
                    if (self.global_struct_field_types.get(name)) |fts| {
                        val.struct_field_types = fts;
                    }
                }
                if (val.struct_field_names == null) {
                    if (self.global_struct_field_names.get(name)) |names| {
                        val.struct_field_names = names;
                    }
                }
                if (val.struct_type_name == null) {
                    if (self.global_struct_type_names.get(name)) |tn| {
                        val.struct_type_name = tn;
                    }
                }
            }
        }

        // If we recovered a concrete struct type name (e.g., "Employee"), try to
        // populate field metadata from type-level tables so that peeks can render
        // the struct body even when the value itself came from an untyped source
        // (like a map lookup payload).
        if (val.struct_type_name) |tn| {
            if (val.struct_field_types == null) {
                if (self.global_struct_field_types.get(tn)) |fts| {
                    val.struct_field_types = fts;
                }
            }
            if (val.struct_field_names == null) {
                if (self.struct_field_names_by_type.get(tn)) |names| {
                    val.struct_field_names = names;
                }
            }
        }
    }

    pub fn resolveStructFieldNames(self: *IRPrinter, value: StackVal) ?[]const []const u8 {
        if (value.struct_field_names) |names| return names;
        if (value.struct_type_name) |type_name| {
            return self.struct_field_names_by_type.get(type_name);
        }
        return null;
    }

    pub fn emitStructValuePeek(
        self: *IRPrinter,
        w: anytype,
        value_name: []const u8,
        field_types: []HIR.HIRType,
        field_names: ?[]const []const u8,
        id: *usize,
        peek_state: *PeekEmitState,
        struct_type_name: ?[]const u8,
    ) !void {
        // Build struct type string for GEPs
        var struct_type_llvm: []const u8 = undefined;
        var needs_free = false;
        if (field_types.len > 0) {
            var buf = std.ArrayListUnmanaged(u8){};
            defer buf.deinit(self.allocator);
            try buf.appendSlice(self.allocator, "{ ");
            var i: usize = 0;
            while (i < field_types.len) : (i += 1) {
                const st = self.hirTypeToStackType(field_types[i]);
                const lt = self.stackTypeToLLVMType(st);
                try buf.appendSlice(self.allocator, lt);
                if (i + 1 < field_types.len) try buf.appendSlice(self.allocator, ", ");
            }
            try buf.appendSlice(self.allocator, " }");
            struct_type_llvm = try buf.toOwnedSlice(self.allocator);
            needs_free = true;
        } else {
            struct_type_llvm = try std.fmt.allocPrint(self.allocator, "{{ {s} }}", .{"i64"});
            needs_free = true;
        }
        defer if (needs_free) self.allocator.free(struct_type_llvm);

        // Print opening brace
        const open_brace_info = try internPeekString(
            self.allocator,
            &peek_state.*.string_map,
            &peek_state.*.strings,
            peek_state.*.next_id_ptr,
            &peek_state.*.globals,
            "{ ",
        );
        const open_brace_ptr = try self.nextTemp(id);
        const open_brace_gep = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
            .{ open_brace_ptr, open_brace_info.length, open_brace_info.name },
        );
        defer self.allocator.free(open_brace_gep);
        try w.writeAll(open_brace_gep);
        const open_brace_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{open_brace_ptr});
        defer self.allocator.free(open_brace_call);
        try w.writeAll(open_brace_call);

        // Print each field (reverse order to match VM peek formatting)
        var field_index: usize = field_types.len;
        while (field_index > 0) {
            field_index -= 1;
            const field_type = field_types[field_index];

            if (field_names) |names| {
                if (field_index < names.len) {
                    const name_with_colon = try std.fmt.allocPrint(self.allocator, "{s}: ", .{names[field_index]});
                    defer self.allocator.free(name_with_colon);
                    const name_info = try internPeekString(
                        self.allocator,
                        &peek_state.*.string_map,
                        &peek_state.*.strings,
                        peek_state.*.next_id_ptr,
                        &peek_state.*.globals,
                        name_with_colon,
                    );
                    const name_ptr = try self.nextTemp(id);
                    const name_gep = try std.fmt.allocPrint(
                        self.allocator,
                        "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                        .{ name_ptr, name_info.length, name_info.name },
                    );
                    defer self.allocator.free(name_gep);
                    try w.writeAll(name_gep);
                    const name_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{name_ptr});
                    defer self.allocator.free(name_call);
                    try w.writeAll(name_call);
                }
            }

            const field_gep = try self.nextTemp(id);
            const gep_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
                .{ field_gep, struct_type_llvm, value_name, @as(i32, @intCast(field_index)) },
            );
            defer self.allocator.free(gep_line);
            try w.writeAll(gep_line);

            const field_stack_type = self.hirTypeToStackType(field_type);
            const field_llvm_type = self.stackTypeToLLVMType(field_stack_type);
            const field_val = try self.nextTemp(id);
            const load_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = load {s}, ptr {s}\n",
                .{ field_val, field_llvm_type, field_gep },
            );
            defer self.allocator.free(load_line);
            try w.writeAll(load_line);

            switch (field_stack_type) {
                .I64 => {
                    if (field_type == .Enum) {
                        const enum_name = struct_type_name orelse "enum";
                        try self.emitEnumPrint(peek_state, w, id, enum_name, field_val);
                    } else {
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_i64(i64 {s})\n", .{field_val});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                },
                .F64 => {
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_f64(double {s})\n", .{field_val});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                .PTR => {
                    if (field_type == .Struct) {
                        const nested_id = field_type.Struct;
                        const nested_types = self.struct_fields_by_id.get(nested_id);
                        const nested_name = self.struct_type_names_by_id.get(nested_id);
                        if (nested_types) |nts| {
                            const nm = nested_name orelse "struct";
                            const nested_names = self.struct_field_names_by_type.get(nm);
                            try self.emitStructValuePeek(w, field_val, nts, nested_names, id, peek_state, nm);
                        } else {
                            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s})\n", .{field_val});
                            defer self.allocator.free(call_line);
                            try w.writeAll(call_line);
                        }
                    } else if (field_type == .Array) {
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_array_hdr(ptr {s})\n", .{field_val});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    } else {
                        const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_peek_string(ptr {s})\n", .{field_val});
                        defer self.allocator.free(call_line);
                        try w.writeAll(call_line);
                    }
                },
                .I8 => {
                    const byte_i64 = try self.nextTemp(id);
                    const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i8 {s} to i64\n", .{ byte_i64, field_val });
                    defer self.allocator.free(zext_line);
                    try w.writeAll(zext_line);
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_print_byte(i64 {s})\n", .{byte_i64});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                .I2 => {
                    const false_info = try internPeekString(
                        self.allocator,
                        &peek_state.*.string_map,
                        &peek_state.*.strings,
                        peek_state.*.next_id_ptr,
                        &peek_state.*.globals,
                        "false",
                    );
                    const true_info = try internPeekString(
                        self.allocator,
                        &peek_state.*.string_map,
                        &peek_state.*.strings,
                        peek_state.*.next_id_ptr,
                        &peek_state.*.globals,
                        "true",
                    );
                    const both_info = try internPeekString(
                        self.allocator,
                        &peek_state.*.string_map,
                        &peek_state.*.strings,
                        peek_state.*.next_id_ptr,
                        &peek_state.*.globals,
                        "both",
                    );
                    const neither_info = try internPeekString(
                        self.allocator,
                        &peek_state.*.string_map,
                        &peek_state.*.strings,
                        peek_state.*.next_id_ptr,
                        &peek_state.*.globals,
                        "neither",
                    );
                    const tetra_i64 = try self.nextTemp(id);
                    const zext_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext i2 {s} to i64\n", .{ tetra_i64, field_val });
                    defer self.allocator.free(zext_line);
                    try w.writeAll(zext_line);
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
                    const sel23 = try self.nextTemp(id);
                    const sel23_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel23, eq3, nptr, bptr });
                    defer self.allocator.free(sel23_line);
                    try w.writeAll(sel23_line);
                    const sel01 = try self.nextTemp(id);
                    const sel01_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel01, eq1, tptr, fptr });
                    defer self.allocator.free(sel01_line);
                    try w.writeAll(sel01_line);
                    const sel_final = try self.nextTemp(id);
                    const sel_final_line = try std.fmt.allocPrint(self.allocator, "  {s} = select i1 {s}, ptr {s}, ptr {s}\n", .{ sel_final, eq2, sel23, sel01 });
                    defer self.allocator.free(sel_final_line);
                    try w.writeAll(sel_final_line);
                    const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{sel_final});
                    defer self.allocator.free(call_line);
                    try w.writeAll(call_line);
                },
                else => {},
            }

            if (field_index > 0) {
                const comma_info = try internPeekString(
                    self.allocator,
                    &peek_state.*.string_map,
                    &peek_state.*.strings,
                    peek_state.*.next_id_ptr,
                    &peek_state.*.globals,
                    ", ",
                );
                const comma_ptr = try self.nextTemp(id);
                const comma_gep = try std.fmt.allocPrint(
                    self.allocator,
                    "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
                    .{ comma_ptr, comma_info.length, comma_info.name },
                );
                defer self.allocator.free(comma_gep);
                try w.writeAll(comma_gep);
                const comma_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{comma_ptr});
                defer self.allocator.free(comma_call);
                try w.writeAll(comma_call);
            }
        }

        const close_brace_info = try internPeekString(
            self.allocator,
            &peek_state.*.string_map,
            &peek_state.*.strings,
            peek_state.*.next_id_ptr,
            &peek_state.*.globals,
            " }",
        );
        const close_brace_ptr = try self.nextTemp(id);
        const close_brace_gep = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds [{d} x i8], ptr {s}, i64 0, i64 0\n",
            .{ close_brace_ptr, close_brace_info.length, close_brace_info.name },
        );
        defer self.allocator.free(close_brace_gep);
        try w.writeAll(close_brace_gep);
        const close_brace_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_write_cstr(ptr {s})\n", .{close_brace_ptr});
        defer self.allocator.free(close_brace_call);
        try w.writeAll(close_brace_call);
    }

    pub fn findLabelIndex(self: *IRPrinter, hir: *const HIR.HIRProgram, label: []const u8) ?usize {
        _ = self;
        for (hir.instructions, 0..) |inst, idx| {
            if (inst == .Label and std.mem.eql(u8, inst.Label.name, label)) return idx;
        }
        return null;
    }

    pub fn emitStructNew(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        sn: std.meta.TagPayload(HIRInstruction, .StructNew),
        peek_state: *PeekEmitState,
    ) !void {
        // StructNew expects fields on stack in reverse order, plus field names
        // For each field: value, then name (both pushed)
        // We need to pop 2 * field_count items (field names and values)
        if (stack.items.len < @as(usize, @intCast(sn.field_count)) * 2) {
            return error.StackUnderflow;
        }

        const fcount: usize = @intCast(sn.field_count);
        // Native structs are stored as i64 "payload bits" slots (mirroring array/map storage).
        const struct_type_llvm = try self.buildI64StructType(fcount);
        defer self.allocator.free(struct_type_llvm);

        // Allocate struct on heap using malloc (since we're returning a pointer)
        // Calculate struct size
        const size_temp = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(size_temp);

        const struct_size: u64 = @intCast(fcount * 8);
        const size_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i64 0, {d}\n", .{ size_temp, struct_size });
        defer self.allocator.free(size_line);
        try w.writeAll(size_line);

        // Call malloc
        const malloc_temp = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(malloc_temp);

        const malloc_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @malloc(i64 {s})\n", .{ malloc_temp, size_temp });
        defer self.allocator.free(malloc_line);
        try w.writeAll(malloc_line);

        // Cast to struct pointer type
        const struct_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;

        const cast_malloc_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast ptr {s} to ptr\n", .{ struct_ptr, malloc_temp });
        defer self.allocator.free(cast_malloc_line);
        try w.writeAll(cast_malloc_line);

        // Pop name/value pairs off the stack in forward field order (see HIR push order).
        // Always capture names from the actual StructNew operands so we don't rely on
        // fragile pre-pass heuristics.
        var pending_names = try self.allocator.alloc([]const u8, @intCast(sn.field_count));
        errdefer {
            for (pending_names) |n| self.allocator.free(n);
            self.allocator.free(pending_names);
        }
        var pending_enum_type_names = try self.allocator.alloc(?[]const u8, @intCast(sn.field_count));
        defer self.allocator.free(pending_enum_type_names);
        @memset(pending_enum_type_names, null);
        var idx_usize: usize = 0;
        while (idx_usize < @as(usize, @intCast(sn.field_count))) : (idx_usize += 1) {
            // Pop field name
            if (stack.items.len < 1) return error.StackUnderflow;
            const field_name_val = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            // Pop field value
            if (stack.items.len < 1) return error.StackUnderflow;
            const field_val = stack.items[stack.items.len - 1];
            stack.items.len -= 1;

            const literal = field_name_val.string_literal_value orelse "";
            pending_names[idx_usize] = try self.allocator.dupe(u8, literal);

            // Field type for this index
            const field_type = sn.field_types[idx_usize];
            switch (field_type) {
                .Enum => pending_enum_type_names[idx_usize] = field_val.enum_type_name,
                else => {},
            }
            const storage_bits: StackVal = switch (field_type) {
                .Nothing => StackVal{ .name = "0", .ty = .I64 },
                else => try self.convertValueToArrayStorage(w, field_val, field_type, id),
            };

            // Compute field GEP
            const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
            id.* += 1;
            defer self.allocator.free(field_gep);

            const gep_line = try std.fmt.allocPrint(
                self.allocator,
                "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
                .{ field_gep, struct_type_llvm, struct_ptr, @as(i32, @intCast(idx_usize)) },
            );
            defer self.allocator.free(gep_line);
            try w.writeAll(gep_line);

            const store_line = try std.fmt.allocPrint(
                self.allocator,
                "  store i64 {s}, ptr {s}\n",
                .{ storage_bits.name, field_gep },
            );
            defer self.allocator.free(store_line);
            try w.writeAll(store_line);
        }

        const desc_global = try self.getOrCreateStructDescGlobal(peek_state, sn.type_name, sn.field_names, sn.field_types, pending_enum_type_names);
        const reg_line = try std.fmt.allocPrint(
            self.allocator,
            "  call void @doxa_struct_register(ptr {s}, ptr {s})\n",
            .{ struct_ptr, desc_global },
        );
        defer self.allocator.free(reg_line);
        try w.writeAll(reg_line);

        // Cache type-level field names only once (first observed StructNew).
        // Other code may retain references to the stored slice, so replacing it
        // can create dangling pointers.
        const struct_field_names: ?[]const []const u8 = if (self.struct_field_names_by_type.get(sn.type_name)) |existing| blk: {
            for (pending_names) |n| self.allocator.free(n);
            self.allocator.free(pending_names);
            break :blk existing;
        } else blk: {
            try self.struct_field_names_by_type.put(sn.type_name, pending_names);
            break :blk pending_names;
        };

        const field_type_copy = try self.allocator.dupe(HIR.HIRType, sn.field_types);
        try stack.append(.{
            .name = struct_ptr,
            .ty = .PTR,
            .struct_field_types = field_type_copy,
            .struct_field_names = struct_field_names,
            .struct_type_name = sn.type_name,
        });

        // Store struct field metadata globally for later lookup
        try self.global_struct_field_types.put(sn.type_name, try self.allocator.dupe(HIR.HIRType, sn.field_types));
    }

    pub fn emitGetField(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, gf: std.meta.TagPayload(HIRInstruction, .GetField)) !void {
        if (stack.items.len < 1) return error.StackUnderflow;
        var struct_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        if (struct_val.ty != .PTR) {
            var as_ptr = try self.ensurePointer(w, struct_val, id);
            as_ptr.struct_field_types = struct_val.struct_field_types;
            struct_val = as_ptr;
        }

        const struct_type_llvm = try self.buildFallbackStructType(gf.field_index);
        defer self.allocator.free(struct_type_llvm);
        const actual_struct_field_types: ?[]HIR.HIRType = struct_val.struct_field_types;

        // Get field type; prefer metadata from the struct value.
        // If we don't have metadata, fall back to the HIR-provided field_type
        // instead of blindly treating everything as int. This is critical for
        // fields like strings/enums coming from arrays of structs (e.g. Animal[]).
        const field_type: HIR.HIRType = if (actual_struct_field_types) |fts|
            fts[@intCast(gf.field_index)]
        else if (struct_val.struct_field_types) |fts|
            fts[@intCast(gf.field_index)]
        else if (gf.field_type != .Unknown and gf.field_type != .Nothing)
            gf.field_type
        else switch (gf.container_type) {
            .Struct => |_| HIR.HIRType{ .Int = {} }, // Final conservative fallback
            else => HIR.HIRType{ .Int = {} },
        };
        // Generate GEP to get field pointer
        // IMPORTANT: If we're accessing a nested struct field (e.g., mike.person.age),
        // we need to use the inner struct's type (Person) not the container's type (Employee)
        // The struct_val.name is the pointer we're accessing, so we need to use the correct struct type
        const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(field_gep);

        const gep_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
            .{ field_gep, struct_type_llvm, struct_val.name, gf.field_index },
        );
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        // Load field value
        const field_val = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;

        const load_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = load i64, ptr {s}\n",
            .{ field_val, field_gep },
        );
        defer self.allocator.free(load_line);
        try w.writeAll(load_line);

        const storage = StackVal{ .name = field_val, .ty = .I64 };
        var pushed = try self.convertArrayStorageToValue(w, storage, field_type, id);
        if (pushed.ty == .PTR) {
            switch (field_type) {
                .Array => |elem_ptr| pushed.array_type = elem_ptr.*,
                .Struct => |sid| {
                    if (self.struct_fields_by_id.get(sid)) |fts| pushed.struct_field_types = fts;
                },
                else => {},
            }
        }
        try stack.append(pushed);
    }

    pub fn emitSetField(self: *IRPrinter, w: anytype, stack: *std.array_list.Managed(StackVal), id: *usize, sf: std.meta.TagPayload(HIRInstruction, .SetField)) !void {
        if (stack.items.len < 2) return error.StackUnderflow;
        const value = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        var struct_val = stack.items[stack.items.len - 1];
        stack.items.len -= 1;
        if (struct_val.ty != .PTR) {
            var as_ptr = try self.ensurePointer(w, struct_val, id);
            as_ptr.struct_field_types = struct_val.struct_field_types;
            struct_val = as_ptr;
        }

        const struct_type_llvm_set = try self.buildFallbackStructType(sf.field_index);
        defer self.allocator.free(struct_type_llvm_set);
        const actual_struct_field_types_set: ?[]HIR.HIRType = struct_val.struct_field_types;

        // Get field type; prefer metadata from the struct value.
        // If metadata is missing, use the HIR field_type when available so that
        // we store the correct representation (e.g. ptr for strings).
        const field_type: HIR.HIRType = if (actual_struct_field_types_set) |fts|
            fts[@intCast(sf.field_index)]
        else if (struct_val.struct_field_types) |fts|
            fts[@intCast(sf.field_index)]
        else if (sf.field_type != .Unknown and sf.field_type != .Nothing)
            sf.field_type
        else switch (sf.container_type) {
            .Struct => |_| HIR.HIRType{ .Int = {} }, // Final conservative fallback
            else => HIR.HIRType{ .Int = {} },
        };

        // Generate GEP to get field pointer
        const field_gep = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        defer self.allocator.free(field_gep);

        const gep_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = getelementptr inbounds {s}, ptr {s}, i32 0, i32 {d}\n",
            .{ field_gep, struct_type_llvm_set, struct_val.name, sf.field_index },
        );
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        const storage_bits: StackVal = switch (field_type) {
            .Nothing => StackVal{ .name = "0", .ty = .I64 },
            else => try self.convertValueToArrayStorage(w, value, field_type, id),
        };

        // Store value to field
        const store_line = try std.fmt.allocPrint(
            self.allocator,
            "  store i64 {s}, ptr {s}\n",
            .{ storage_bits.name, field_gep },
        );
        defer self.allocator.free(store_line);
        try w.writeAll(store_line);

        // Push struct pointer back onto stack
        try stack.append(struct_val);
    }

    pub fn buildFallbackStructType(self: *IRPrinter, field_index: u32) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);
        const fallback_fields: usize = @intCast(field_index + 1);
        try buf.appendSlice(self.allocator, "{ ");
        var i: usize = 0;
        while (i < fallback_fields) : (i += 1) {
            try buf.appendSlice(self.allocator, "i64");
            if (i + 1 < fallback_fields) try buf.appendSlice(self.allocator, ", ");
        }
        try buf.appendSlice(self.allocator, " }");
        return try buf.toOwnedSlice(self.allocator);
    }

    pub fn buildI64StructType(self: *IRPrinter, field_count: usize) ![]u8 {
        var buf = std.ArrayListUnmanaged(u8){};
        defer buf.deinit(self.allocator);
        try buf.appendSlice(self.allocator, "{ ");
        var i: usize = 0;
        while (i < field_count) : (i += 1) {
            try buf.appendSlice(self.allocator, "i64");
            if (i + 1 < field_count) try buf.appendSlice(self.allocator, ", ");
        }
        try buf.appendSlice(self.allocator, " }");
        return try buf.toOwnedSlice(self.allocator);
    }

    pub fn getOrCreateStructDescGlobal(
        self: *IRPrinter,
        peek_state: *PeekEmitState,
        type_name: []const u8,
        field_names: []const []const u8,
        field_types: []HIR.HIRType,
        field_enum_type_names: []const ?[]const u8,
    ) ![]const u8 {
        if (self.struct_desc_globals_by_type.get(type_name)) |existing| return existing;

        const field_count: usize = field_names.len;

        // Reuse the same string interning machinery used by peek/debug output.
        const type_info = try internPeekString(
            self.allocator,
            &peek_state.string_map,
            &peek_state.strings,
            peek_state.next_id_ptr,
            &peek_state.globals,
            type_name,
        );

        const type_gep_expr = try std.fmt.allocPrint(
            self.allocator,
            "ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)",
            .{ type_info.length, type_info.name },
        );
        defer self.allocator.free(type_gep_expr);

        const names_global = try std.fmt.allocPrint(self.allocator, "@.doxa.struct.names.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;
        defer self.allocator.free(names_global);

        const tags_global = try std.fmt.allocPrint(self.allocator, "@.doxa.struct.tags.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;
        defer self.allocator.free(tags_global);

        const enum_types_global = try std.fmt.allocPrint(self.allocator, "@.doxa.struct.enumtys.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;
        defer self.allocator.free(enum_types_global);

        const desc_global = try std.fmt.allocPrint(self.allocator, "@.doxa.struct.desc.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;

        // Field names array
        if (field_count == 0) {
            const names_line = try std.fmt.allocPrint(self.allocator, "{s} = private constant [0 x ptr] []\n", .{names_global});
            try peek_state.globals.append(names_line);
        } else {
            var elems = std.ArrayListUnmanaged(u8){};
            defer elems.deinit(self.allocator);
            var i: usize = 0;
            while (i < field_count) : (i += 1) {
                const finfo = try internPeekString(
                    self.allocator,
                    &peek_state.string_map,
                    &peek_state.strings,
                    peek_state.next_id_ptr,
                    &peek_state.globals,
                    field_names[i],
                );
                const gep_expr = try std.fmt.allocPrint(
                    self.allocator,
                    "ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)",
                    .{ finfo.length, finfo.name },
                );
                defer self.allocator.free(gep_expr);
                if (i != 0) try elems.appendSlice(self.allocator, ", ");
                const piece = try std.fmt.allocPrint(self.allocator, "{s}", .{gep_expr});
                defer self.allocator.free(piece);
                try elems.appendSlice(self.allocator, piece);
            }
            const names_line = try std.fmt.allocPrint(
                self.allocator,
                "{s} = private constant [{d} x ptr] [{s}]\n",
                .{ names_global, field_count, elems.items },
            );
            try peek_state.globals.append(names_line);
        }

        // Field tags array
        if (field_count == 0) {
            const tags_line = try std.fmt.allocPrint(self.allocator, "{s} = private constant [0 x i64] []\n", .{tags_global});
            try peek_state.globals.append(tags_line);
        } else {
            var elems = std.ArrayListUnmanaged(u8){};
            defer elems.deinit(self.allocator);
            var i: usize = 0;
            while (i < field_count) : (i += 1) {
                if (i != 0) try elems.appendSlice(self.allocator, ", ");
                const tag_val = self.arrayElementTag(field_types[i]);
                const piece = try std.fmt.allocPrint(self.allocator, "i64 {d}", .{tag_val});
                defer self.allocator.free(piece);
                try elems.appendSlice(self.allocator, piece);
            }
            const tags_line = try std.fmt.allocPrint(
                self.allocator,
                "{s} = private constant [{d} x i64] [{s}]\n",
                .{ tags_global, field_count, elems.items },
            );
            try peek_state.globals.append(tags_line);
        }

        // Field enum type names array (only meaningful for tag==8 fields)
        if (field_count == 0) {
            const enumtys_line = try std.fmt.allocPrint(self.allocator, "{s} = private constant [0 x ptr] []\n", .{enum_types_global});
            try peek_state.globals.append(enumtys_line);
        } else {
            var elems = std.ArrayListUnmanaged(u8){};
            defer elems.deinit(self.allocator);
            var i: usize = 0;
            while (i < field_count) : (i += 1) {
                if (i != 0) try elems.appendSlice(self.allocator, ", ");
                if (i < field_enum_type_names.len) {
                    if (field_enum_type_names[i]) |etn| {
                        const einfo = try internPeekString(
                            self.allocator,
                            &peek_state.string_map,
                            &peek_state.strings,
                            peek_state.next_id_ptr,
                            &peek_state.globals,
                            etn,
                        );
                        const gep_expr = try std.fmt.allocPrint(
                            self.allocator,
                            "ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)",
                            .{ einfo.length, einfo.name },
                        );
                        defer self.allocator.free(gep_expr);
                        try elems.appendSlice(self.allocator, gep_expr);
                    } else {
                        try elems.appendSlice(self.allocator, "ptr null");
                    }
                } else {
                    try elems.appendSlice(self.allocator, "ptr null");
                }
            }
            const enumtys_line = try std.fmt.allocPrint(
                self.allocator,
                "{s} = private constant [{d} x ptr] [{s}]\n",
                .{ enum_types_global, field_count, elems.items },
            );
            try peek_state.globals.append(enumtys_line);
        }

        const names_ptr_expr = if (field_count == 0)
            "ptr null"
        else
            try std.fmt.allocPrint(
                self.allocator,
                "ptr getelementptr inbounds ([{d} x ptr], ptr {s}, i64 0, i64 0)",
                .{ field_count, names_global },
            );
        defer if (field_count != 0) self.allocator.free(names_ptr_expr);

        const tags_ptr_expr = if (field_count == 0)
            "ptr null"
        else
            try std.fmt.allocPrint(
                self.allocator,
                "ptr getelementptr inbounds ([{d} x i64], ptr {s}, i64 0, i64 0)",
                .{ field_count, tags_global },
            );
        defer if (field_count != 0) self.allocator.free(tags_ptr_expr);

        const enumtys_ptr_expr = if (field_count == 0)
            "ptr null"
        else
            try std.fmt.allocPrint(
                self.allocator,
                "ptr getelementptr inbounds ([{d} x ptr], ptr {s}, i64 0, i64 0)",
                .{ field_count, enum_types_global },
            );
        defer if (field_count != 0) self.allocator.free(enumtys_ptr_expr);

        const desc_line = try std.fmt.allocPrint(
            self.allocator,
            "{s} = private constant {{ ptr, i64, ptr, ptr, ptr }} {{ {s}, i64 {d}, {s}, {s}, {s} }}\n",
            .{ desc_global, type_gep_expr, field_count, names_ptr_expr, tags_ptr_expr, enumtys_ptr_expr },
        );
        try peek_state.globals.append(desc_line);

        try self.struct_desc_globals_by_type.put(type_name, desc_global);
        return desc_global;
    }

    pub fn getOrCreateEnumDescGlobal(
        self: *IRPrinter,
        peek_state: *PeekEmitState,
        type_name: []const u8,
    ) ![]const u8 {
        if (self.enum_desc_globals_by_type.get(type_name)) |existing| return existing;

        const meta_opt = self.enum_print_map.get(type_name);
        const variants = if (meta_opt) |m| m.items else &[_]EnumVariantMeta{};
        var max_index: u32 = 0;
        for (variants) |v| {
            if (v.index > max_index) max_index = v.index;
        }
        const variant_count: usize = if (variants.len == 0) 0 else @as(usize, max_index) + 1;

        const type_info = try internPeekString(
            self.allocator,
            &peek_state.string_map,
            &peek_state.strings,
            peek_state.next_id_ptr,
            &peek_state.globals,
            type_name,
        );
        const type_gep_expr = try std.fmt.allocPrint(
            self.allocator,
            "ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)",
            .{ type_info.length, type_info.name },
        );
        defer self.allocator.free(type_gep_expr);

        const names_global = try std.fmt.allocPrint(self.allocator, "@.doxa.enum.names.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;
        defer self.allocator.free(names_global);

        const desc_global = try std.fmt.allocPrint(self.allocator, "@.doxa.enum.desc.{d}", .{peek_state.next_id_ptr.*});
        peek_state.next_id_ptr.* += 1;

        if (variant_count == 0) {
            const names_line = try std.fmt.allocPrint(self.allocator, "{s} = private constant [0 x ptr] []\n", .{names_global});
            try peek_state.globals.append(names_line);
        } else {
            var elems = std.ArrayListUnmanaged(u8){};
            defer elems.deinit(self.allocator);
            var i: usize = 0;
            while (i < variant_count) : (i += 1) {
                if (i != 0) try elems.appendSlice(self.allocator, ", ");
                var found: ?[]const u8 = null;
                for (variants) |v| {
                    if (v.index == @as(u32, @intCast(i))) {
                        found = v.name;
                        break;
                    }
                }
                if (found) |name| {
                    const info = try internPeekString(
                        self.allocator,
                        &peek_state.string_map,
                        &peek_state.strings,
                        peek_state.next_id_ptr,
                        &peek_state.globals,
                        name,
                    );
                    const gep_expr = try std.fmt.allocPrint(
                        self.allocator,
                        "ptr getelementptr inbounds ([{d} x i8], ptr {s}, i64 0, i64 0)",
                        .{ info.length, info.name },
                    );
                    defer self.allocator.free(gep_expr);
                    try elems.appendSlice(self.allocator, gep_expr);
                } else {
                    try elems.appendSlice(self.allocator, "ptr null");
                }
            }
            const names_line = try std.fmt.allocPrint(
                self.allocator,
                "{s} = private constant [{d} x ptr] [{s}]\n",
                .{ names_global, variant_count, elems.items },
            );
            try peek_state.globals.append(names_line);
        }

        const names_ptr_expr = if (variant_count == 0)
            "ptr null"
        else
            try std.fmt.allocPrint(
                self.allocator,
                "ptr getelementptr inbounds ([{d} x ptr], ptr {s}, i64 0, i64 0)",
                .{ variant_count, names_global },
            );
        defer if (variant_count != 0) self.allocator.free(names_ptr_expr);

        const desc_line = try std.fmt.allocPrint(
            self.allocator,
            "{s} = private constant {{ ptr, i64, ptr }} {{ {s}, i64 {d}, {s} }}\n",
            .{ desc_global, type_gep_expr, variant_count, names_ptr_expr },
        );
        try peek_state.globals.append(desc_line);

        try self.enum_desc_globals_by_type.put(type_name, desc_global);
        return desc_global;
    }

    pub fn emitEnumInitCalls(self: *IRPrinter, w: anytype, peek_state: *PeekEmitState, id: *usize) !void {
        _ = id;
        var it = self.enum_print_map.iterator();
        while (it.next()) |entry| {
            const type_name = entry.key_ptr.*;
            const desc_global = try self.getOrCreateEnumDescGlobal(peek_state, type_name);
            const call_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_enum_register(ptr {s})\n", .{desc_global});
            defer self.allocator.free(call_line);
            try w.writeAll(call_line);
        }
    }

    pub fn hirTypeToStackType(self: *IRPrinter, hir_type: HIR.HIRType) StackType {
        _ = self;
        return switch (hir_type) {
            .Int => .I64,
            .Float => .F64,
            .Byte => .I8,
            .Tetra => .I2,
            .String => .PTR,
            .Array => .PTR,
            .Map => .PTR,
            .Struct => .PTR,
            .Enum => .I64,
            .Function => .PTR,
            .Union => .Value,
            .Nothing => .Nothing,
            else => .I64,
        };
    }

    pub fn hirTypeToTypeString(self: *IRPrinter, allocator: std.mem.Allocator, hir_type: HIR.HIRType) ![]const u8 {
        return switch (hir_type) {
            .Int => try allocator.dupe(u8, "int"),
            .Float => try allocator.dupe(u8, "float"),
            .Byte => try allocator.dupe(u8, "byte"),
            .Tetra => try allocator.dupe(u8, "tetra"),
            .String => try allocator.dupe(u8, "string"),
            .Array => |inner| {
                const inner_str = try self.hirTypeToTypeString(allocator, inner.*);
                defer allocator.free(inner_str);
                return try std.fmt.allocPrint(allocator, "{s}[]", .{inner_str});
            },
            .Map => try allocator.dupe(u8, "map"),
            .Struct => |sid| {
                if (self.struct_type_names_by_id.get(sid)) |tn| {
                    return try allocator.dupe(u8, tn);
                }
                return try allocator.dupe(u8, "struct");
            },
            .Enum => try allocator.dupe(u8, "enum"),
            .Function => try allocator.dupe(u8, "function"),
            .Union => try allocator.dupe(u8, "union"),
            .Nothing => try allocator.dupe(u8, "nothing"),
            .Unknown => try allocator.dupe(u8, "nothing"),
            else => try allocator.dupe(u8, "value"),
        };
    }

    pub fn stackTypeToLLVMType(self: *IRPrinter, stack_type: StackType) []const u8 {
        _ = self;
        return switch (stack_type) {
            .I64 => "i64",
            .F64 => "double",
            .I8 => "i8",
            .I1 => "i1",
            .I2 => "i2",
            .PTR => "ptr",
            .Value => "%DoxaValue",
            .Nothing => "{}", // Zero-sized type
        };
    }

    pub fn hirTypeToLLVMType(self: *IRPrinter, hir_type: HIR.HIRType, as_pointer: bool) []const u8 {
        _ = self;
        if (as_pointer) return "ptr";
        return switch (hir_type) {
            .Int => "i64",
            .Float => "double",
            .Byte => "i8",
            .Tetra => "i2",
            .String => "ptr",
            .Array => "ptr",
            .Map => "ptr",
            .Struct => "ptr",
            .Enum => "ptr",
            .Function => "ptr",
            .Union => "%DoxaValue",
            .Nothing => "void",
            else => "i64",
        };
    }
    };
}
