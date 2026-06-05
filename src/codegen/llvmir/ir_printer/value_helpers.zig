const std = @import("std");
const doxa_rt = @import("../../../runtime/doxa_rt.zig");
const DoxaTag = doxa_rt.DoxaTag;
const DoxaUnionMeta = doxa_rt.DoxaUnionMeta;

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const StackVal = Ctx.StackVal;

    return struct {
    pub fn emitRTCallReturningString(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        fn_name: []const u8,
        args_line: []const u8,
    ) !void {
        const out_ptr_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const out_len_slot = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;

        const alloca_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
        const alloca_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
        defer self.allocator.free(alloca_ptr_line);
        defer self.allocator.free(alloca_len_line);
        try w.writeAll(alloca_ptr_line);
        try w.writeAll(alloca_len_line);

        const init_ptr_line = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
        const init_len_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
        defer self.allocator.free(init_ptr_line);
        defer self.allocator.free(init_len_line);
        try w.writeAll(init_ptr_line);
        try w.writeAll(init_len_line);

        const call_line = if (args_line.len > 0)
            try std.fmt.allocPrint(self.allocator, "  call void @{s}({s}, ptr {s}, ptr {s})\n", .{ fn_name, args_line, out_ptr_slot, out_len_slot })
        else
            try std.fmt.allocPrint(self.allocator, "  call void @{s}(ptr {s}, ptr {s})\n", .{ fn_name, out_ptr_slot, out_len_slot });
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);

        const loaded_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const loaded_len = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const load_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{loaded_ptr, out_ptr_slot});
        const load_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{loaded_len, out_len_slot});
        defer self.allocator.free(load_ptr_line);
        defer self.allocator.free(load_len_line);
        try w.writeAll(load_ptr_line);
        try w.writeAll(load_len_line);

        const tmp_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{tmp_name, loaded_ptr});
        defer self.allocator.free(ins0);
        try w.writeAll(ins0);

        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{result_name, tmp_name, loaded_len});
        defer self.allocator.free(ins1);
        try w.writeAll(ins1);

        try stack.append(.{ .name = result_name, .ty = .STRING });
    }

    pub fn createEnumTypeNameGlobal(self: *IRPrinter, type_name: []const u8, _: *usize) ![]const u8 {
        // TODO: emit proper global constant for enum type name
        // In a full implementation, we'd create proper global string constants
        return try self.allocator.dupe(u8, type_name);
    }

    pub fn ensurePointer(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .PTR) return value;

        var current_name = value.name;
        var current_ty = value.ty;

        switch (current_ty) {
            .I64 => {},
            .STRING => {
                const ptr_ext = try self.nextTemp(id);
                const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, current_name });
                defer self.allocator.free(ext_line);
                try w.writeAll(ext_line);
                current_name = ptr_ext;
                current_ty = .PTR;
            },
            .I1, .I2, .I8 => {
                const widened = try self.nextTemp(id);
                const src_ty = self.stackTypeToLLVMType(current_ty);
                const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, current_name });
                defer self.allocator.free(widen_line);
                try w.writeAll(widen_line);
                current_name = widened;
                current_ty = .I64;
            },
            .F64 => {
                const bitcasted = try self.nextTemp(id);
                const bitcast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ bitcasted, current_name });
                defer self.allocator.free(bitcast_line);
                try w.writeAll(bitcast_line);
                current_name = bitcasted;
                current_ty = .I64;
            },
            else => {},
        }

        if (current_ty == .STRING) {
            const ptr_ext = try self.nextTemp(id);
            const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, current_name });
            defer self.allocator.free(ext_line);
            try w.writeAll(ext_line);
            return .{
                .name = ptr_ext,
                .ty = .PTR,
                .array_type = value.array_type,
                .enum_type_name = value.enum_type_name,
                .struct_field_types = value.struct_field_types,
                .struct_field_names = value.struct_field_names,
                .struct_type_name = value.struct_type_name,
            };
        }

        if (current_ty == .PTR) return .{
            .name = current_name,
            .ty = .PTR,
            .array_type = value.array_type,
            .enum_type_name = value.enum_type_name,
            .struct_field_types = value.struct_field_types,
            .struct_field_names = value.struct_field_names,
            .struct_type_name = value.struct_type_name,
        };

        if (current_ty != .I64) {
            const widened = try self.nextTemp(id);
            const src_ty = self.stackTypeToLLVMType(current_ty);
            const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, current_name });
            defer self.allocator.free(widen_line);
            try w.writeAll(widen_line);
            current_name = widened;
        }

        const ptr_name = try self.nextTemp(id);
        const inttoptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ ptr_name, current_name });
        defer self.allocator.free(inttoptr_line);
        try w.writeAll(inttoptr_line);
        return .{
            .name = ptr_name,
            .ty = .PTR,
            .array_type = value.array_type,
            .enum_type_name = value.enum_type_name,
            .struct_field_types = value.struct_field_types,
            .struct_field_names = value.struct_field_names,
            .struct_type_name = value.struct_type_name,
        };
    }

    /// Coerce a stack value to .STRING by wrapping it in a DoxaString.
    ///
    /// Callers that need a valid length must check for .STRING before
    /// calling this function. Conversion from .PTR produces len=0 because
    /// the LLVM printer has no way to materialize the C-string length at
    /// IR emission time. Code paths that consume the length (concat,
    /// substring, unpack) guard with `arg.ty == .STRING` and only fall
    /// back to `ensureString` for legacy .PTR values.
    pub fn ensureString(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .STRING) return value;
        if (value.ty == .PTR) {
            const tmp_name = try self.nextTemp(id);
            const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, value.name });
            defer self.allocator.free(ins0);
            try w.writeAll(ins0);
            const str_name = try self.nextTemp(id);
            // len = 0: the caller must not rely on this value.
            const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 0, 1\n", .{ str_name, tmp_name });
            defer self.allocator.free(ins1);
            try w.writeAll(ins1);
            return .{ .name = str_name, .ty = .STRING, .array_type = value.array_type, .enum_type_name = value.enum_type_name, .struct_field_types = value.struct_field_types, .struct_field_names = value.struct_field_names, .struct_type_name = value.struct_type_name };
        }
        const ptr = try self.ensurePointer(w, value, id);
        const tmp_name = try self.nextTemp(id);
        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, ptr.name });
        defer self.allocator.free(ins0);
        try w.writeAll(ins0);
        const str_name = try self.nextTemp(id);
        // len = 0: caller must not consume the length field.
        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 0, 1\n", .{ str_name, tmp_name });
        defer self.allocator.free(ins1);
        try w.writeAll(ins1);
        return .{ .name = str_name, .ty = .STRING };
    }

    const ArrayLenLoad = struct {
        array: StackVal,
        len_ptr: []const u8,
        len_value: StackVal,
    };

    pub fn ensureI64(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .I64) return value;

        switch (value.ty) {
            .I1, .I2, .I8 => {
                const widened = try self.nextTemp(id);
                const src_ty = self.stackTypeToLLVMType(value.ty);
                const widen_line = try std.fmt.allocPrint(self.allocator, "  {s} = zext {s} {s} to i64\n", .{ widened, src_ty, value.name });
                defer self.allocator.free(widen_line);
                try w.writeAll(widen_line);
                return .{ .name = widened, .ty = .I64 };
            },
            .PTR => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            .STRING => {
                const ptr_ext = try self.nextTemp(id);
                const ext_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ ptr_ext, value.name });
                defer self.allocator.free(ext_line);
                try w.writeAll(ext_line);
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, ptr_ext });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            .F64 => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            .Value => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                return .{ .name = tmp, .ty = .I64 };
            },
            else => return value,
        }
    }

    pub fn unwrapDoxaValueToType(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        target: HIR.HIRType,
        id: *usize,
    ) !StackVal {
        if (value.ty != .Value) return value;

        const payload = try self.nextTemp(id);
        const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, value.name });
        defer self.allocator.free(extract_line);
        try w.writeAll(extract_line);

        return switch (target) {
            .Int, .Enum => .{ .name = payload, .ty = .I64 },
            .Float => blk: {
                const as_f64 = try self.nextTemp(id);
                const bitcast_line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ as_f64, payload });
                defer self.allocator.free(bitcast_line);
                try w.writeAll(bitcast_line);
                break :blk StackVal{ .name = as_f64, .ty = .F64 };
            },
            .Byte => blk: {
                const narrowed = try self.nextTemp(id);
                const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ narrowed, payload });
                defer self.allocator.free(trunc_line);
                try w.writeAll(trunc_line);
                break :blk StackVal{ .name = narrowed, .ty = .I8 };
            },
            .Tetra => blk: {
                const narrowed = try self.nextTemp(id);
                const trunc_line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ narrowed, payload });
                defer self.allocator.free(trunc_line);
                try w.writeAll(trunc_line);
                break :blk StackVal{ .name = narrowed, .ty = .I2 };
            },
            .String, .Array, .Map, .Struct, .Function => blk: {
                const as_ptr = try self.nextTemp(id);
                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                defer self.allocator.free(cast_line);
                try w.writeAll(cast_line);
                break :blk StackVal{ .name = as_ptr, .ty = .PTR };
            },
            else => StackVal{ .name = payload, .ty = .I64 },
        };
    }

    pub fn findUnionMemberIndex(_: *IRPrinter, union_type: HIR.HIRType, value: StackVal) u32 {
        if (union_type != .Union) return 0;
        const members = union_type.Union.members;
        for (members, 0..) |m_ptr, idx| {
            const m = m_ptr.*;
            switch (value.ty) {
                .I64 => {
                    if (m == .Int) return @intCast(idx);
                    if (m == .Enum) return @intCast(idx);
                },
                .F64 => if (m == .Float) return @intCast(idx),
                .I8 => if (m == .Byte) return @intCast(idx),
                .I2, .I1 => if (m == .Tetra) return @intCast(idx),
                .PTR => {
                    if (value.array_type != null and m == .Array) return @intCast(idx);
                    if (value.struct_field_types != null and m == .Struct) return @intCast(idx);
                    if (value.struct_type_name != null and m == .Struct) return @intCast(idx);
                    if (m == .String) return @intCast(idx);
                    if (m == .Map) return @intCast(idx);
                    if (m == .Function) return @intCast(idx);
                },
                .STRING => if (m == .String) return @intCast(idx),
                .Nothing => if (m == .Nothing) return @intCast(idx),
                .Value => {},
            }
        }
        return 0;
    }

    pub fn buildDoxaValue(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        target_union: ?HIR.HIRType,
        id: *usize,
    ) !StackVal {
        // If it's already a canonical value, reuse it.
        if (value.ty == .Value) return value;

        // Determine tag based on stack type (must match DoxaTag in doxa_rt.zig)
        const tag = DoxaTag;
        var tag_const: i32 = @intFromEnum(tag.Nothing); // default
        switch (value.ty) {
            .I64 => {
                // Distinguish enums vs ints when metadata exists
                tag_const = if (value.enum_type_name != null) @intFromEnum(tag.Enum) else @intFromEnum(tag.Int);
            },
            .F64 => tag_const = @intFromEnum(tag.Float),
            .I8 => tag_const = @intFromEnum(tag.Byte),
            .PTR => {
                if (value.array_type != null) {
                    tag_const = @intFromEnum(tag.Array);
                } else if (value.struct_field_types != null or value.struct_type_name != null) {
                    tag_const = @intFromEnum(tag.Struct);
                } else {
                    tag_const = @intFromEnum(tag.String);
                }
            },
            .STRING => tag_const = @intFromEnum(tag.String),
            .I2, .I1 => tag_const = @intFromEnum(tag.Tetra),
            .Nothing => tag_const = @intFromEnum(tag.Nothing),
            .Value => tag_const = @intFromEnum(tag.Nothing),
        }

        // Compute reserved bits if targeting a union
        var reserved_const: u32 = 0;
        if (target_union) |ut| {
            if (ut == .Union) {
                const idx = self.findUnionMemberIndex(ut, value);
                const uid = ut.Union.id & (DoxaUnionMeta.union_id_mask >> DoxaUnionMeta.union_id_shift);
                reserved_const = DoxaUnionMeta.is_union_bit | (uid << DoxaUnionMeta.union_id_shift) | (idx & DoxaUnionMeta.member_index_mask);
            }
        }

        const tag_reg = try self.nextTemp(id);
        const tag_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i32 0, {d}\n", .{ tag_reg, tag_const });
        defer self.allocator.free(tag_line);
        try w.writeAll(tag_line);

        const reserved_reg = try self.nextTemp(id);
        const reserved_line = try std.fmt.allocPrint(self.allocator, "  {s} = add i32 0, {d}\n", .{ reserved_reg, reserved_const });
        defer self.allocator.free(reserved_line);
        try w.writeAll(reserved_line);

        const payload = try self.ensureI64(w, value, id);

        const dv0 = try self.nextTemp(id);
        const dv0_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = insertvalue %DoxaValue undef, i32 {s}, 0\n",
            .{ dv0, tag_reg },
        );
        defer self.allocator.free(dv0_line);
        try w.writeAll(dv0_line);

        const dv1 = try self.nextTemp(id);
        const dv1_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = insertvalue %DoxaValue {s}, i32 {s}, 1\n",
            .{ dv1, dv0, reserved_reg },
        );
        defer self.allocator.free(dv1_line);
        try w.writeAll(dv1_line);

        const dv2 = try self.nextTemp(id);
        const dv2_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = insertvalue %DoxaValue {s}, i64 {s}, 2\n",
            .{ dv2, dv1, payload.name },
        );
        defer self.allocator.free(dv2_line);
        try w.writeAll(dv2_line);

        return .{ .name = dv2, .ty = .Value };
    }

    pub fn arrayElementSize(_: *IRPrinter, element_type: HIR.HIRType) u64 {
        return switch (element_type) {
            .Int => 8,
            .Byte => 1,
            .Float => 8,
            .String => 16,
            .Tetra => 1,
            .Nothing => 0,
            else => 8,
        };
    }

    pub fn arrayElementTag(_: *IRPrinter, element_type: HIR.HIRType) u64 {
        return switch (element_type) {
            .Int => 0,
            .Byte => 1,
            .Float => 2,
            .String => 3,
            .Tetra => 4,
            .Nothing => 5,
            .Array => 6,
            .Struct => 7,
            .Enum => 8,
            else => 255,
        };
    }

    pub fn convertValueToArrayStorage(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        element_type: HIR.HIRType,
        id: *usize,
    ) !StackVal {
        return switch (element_type) {
            .Int, .Byte, .Tetra => try self.ensureI64(w, value, id),
            .Float => blk: {
                if (value.ty != .F64) {
                    break :blk try self.ensureI64(w, value, id);
                }
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk StackVal{ .name = tmp, .ty = .I64 };
            },
            .Enum => try self.ensureI64(w, value, id),
            .String => blk_ptr: {
                var str_val = value;
                if (str_val.ty != .STRING) {
                    str_val = try self.ensureString(w, value, id);
                }
                // Clone to avoid dangling pointers
                const s_ptr = try self.nextTemp(id);
                const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, str_val.name });
                defer self.allocator.free(s_ptr_line);
                try w.writeAll(s_ptr_line);
                const s_len = try self.nextTemp(id);
                const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, str_val.name });
                defer self.allocator.free(s_len_line);
                try w.writeAll(s_len_line);
                const out_ptr_slot = try self.nextTemp(id);
                const out_len_slot = try self.nextTemp(id);
                const alloca_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
                const alloca_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
                defer self.allocator.free(alloca_ptr_line);
                defer self.allocator.free(alloca_len_line);
                try w.writeAll(alloca_ptr_line);
                try w.writeAll(alloca_len_line);
                const init_ptr_line = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
                const init_len_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
                defer self.allocator.free(init_ptr_line);
                defer self.allocator.free(init_len_line);
                try w.writeAll(init_ptr_line);
                try w.writeAll(init_len_line);
                const clone_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_clone(ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ s_ptr, s_len, out_ptr_slot, out_len_slot });
                defer self.allocator.free(clone_call);
                try w.writeAll(clone_call);
                const cloned_ptr = try self.nextTemp(id);
                const load_cloned_ptr = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ cloned_ptr, out_ptr_slot });
                defer self.allocator.free(load_cloned_ptr);
                try w.writeAll(load_cloned_ptr);
                // Return the cloned pointer as i64 for array storage
                const ptr_i64 = try self.nextTemp(id);
                const pi = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ ptr_i64, cloned_ptr });
                defer self.allocator.free(pi);
                try w.writeAll(pi);
                break :blk_ptr StackVal{ .name = ptr_i64, .ty = .I64 };
            },
            .Array, .Map, .Struct, .Function, .Union => blk_ptr: {
                var ptr_val = value;
                if (ptr_val.ty != .PTR) {
                    ptr_val = try self.ensurePointer(w, value, id);
                }
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, ptr_val.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .I64 };
            },
            else => try self.ensureI64(w, value, id),
        };
    }

    pub fn convertArrayStorageToValue(
        self: *IRPrinter,
        w: anytype,
        storage: StackVal,
        element_type: HIR.HIRType,
        id: *usize,
    ) !StackVal {
        const as_i64 = if (storage.ty == .I64) storage else try self.ensureI64(w, storage, id);
        return switch (element_type) {
            .Int => as_i64,
            .Byte => blk: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i8\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk StackVal{ .name = tmp, .ty = .I8 };
            },
            .Tetra => blk_tetra: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = trunc i64 {s} to i2\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_tetra StackVal{ .name = tmp, .ty = .I2 };
            },
            .Float => blk_float: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast i64 {s} to double\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_float StackVal{ .name = tmp, .ty = .F64 };
            },
            .Enum => as_i64,
            .String => blk_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                const out_ptr_slot = try self.nextTemp(id);
                const out_len_slot = try self.nextTemp(id);
                const alloca_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
                const alloca_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
                defer self.allocator.free(alloca_ptr_line);
                defer self.allocator.free(alloca_len_line);
                try w.writeAll(alloca_ptr_line);
                try w.writeAll(alloca_len_line);
                const init_ptr_line = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
                const init_len_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
                defer self.allocator.free(init_ptr_line);
                defer self.allocator.free(init_len_line);
                try w.writeAll(init_ptr_line);
                try w.writeAll(init_len_line);
                const clone_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_clone(ptr {s}, i64 0, ptr {s}, ptr {s})\n", .{ tmp, out_ptr_slot, out_len_slot });
                defer self.allocator.free(clone_call);
                try w.writeAll(clone_call);
                const loaded_ptr = try self.nextTemp(id);
                const loaded_len = try self.nextTemp(id);
                const load_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ loaded_ptr, out_ptr_slot });
                const load_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ loaded_len, out_len_slot });
                defer self.allocator.free(load_ptr_line);
                defer self.allocator.free(load_len_line);
                try w.writeAll(load_ptr_line);
                try w.writeAll(load_len_line);
                const tmp_ds = try self.nextTemp(id);
                const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_ds, loaded_ptr });
                defer self.allocator.free(ins0);
                try w.writeAll(ins0);
                const cloned = try self.nextTemp(id);
                const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ cloned, tmp_ds, loaded_len });
                defer self.allocator.free(ins1);
                try w.writeAll(ins1);
                break :blk_ptr StackVal{ .name = cloned, .ty = .STRING };
            },
            .Map, .Function, .Union => blk_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .PTR };
            },
            .Struct => |sid| blk_struct_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                // Preserve concrete struct metadata so downstream GetField can
                // load the right storage type for each field.
                const type_name = self.struct_type_names_by_id.get(sid);
                break :blk_struct_ptr StackVal{
                    .name = tmp,
                    .ty = .PTR,
                    .struct_field_types = self.struct_fields_by_id.get(sid),
                    .struct_field_names = if (type_name) |tn| self.struct_field_names_by_type.get(tn) else null,
                    .struct_type_name = type_name orelse "struct",
                };
            },
            .Array => |inner| blk_arr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_arr StackVal{ .name = tmp, .ty = .PTR, .array_type = inner.* };
            },
            else => as_i64,
        };
    }

    pub fn loadArrayLength(
        self: *IRPrinter,
        w: anytype,
        arr: StackVal,
        id: *usize,
    ) !ArrayLenLoad {
        var array_ptr = arr;
        if (array_ptr.ty != .PTR) {
            array_ptr = try self.ensurePointer(w, array_ptr, id);
        }
        const len_ptr = try self.nextTemp(id);
        const gep_line = try std.fmt.allocPrint(self.allocator, "  {s} = getelementptr inbounds %ArrayHeader, ptr {s}, i32 0, i32 1\n", .{ len_ptr, array_ptr.name });
        defer self.allocator.free(gep_line);
        try w.writeAll(gep_line);

        const len_reg = try self.nextTemp(id);
        const load_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ len_reg, len_ptr });
        defer self.allocator.free(load_line);
        try w.writeAll(load_line);

        return .{
            .array = array_ptr,
            .len_ptr = len_ptr,
            .len_value = .{ .name = len_reg, .ty = .I64 },
        };
    }

    };
}
