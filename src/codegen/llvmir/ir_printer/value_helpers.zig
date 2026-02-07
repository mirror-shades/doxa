const std = @import("std");

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const StackVal = Ctx.StackVal;

    return struct {
    pub fn createEnumTypeNameGlobal(self: *IRPrinter, type_name: []const u8, _: *usize) ![]const u8 {
        // For now, just return the type name directly
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
            .F64 => {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
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

        // Determine tag based on stack type (align with DoxaTag values)
        var tag_const: i32 = 8; // default to Nothing
        switch (value.ty) {
            .I64 => {
                // Distinguish enums vs ints when metadata exists
                tag_const = if (value.enum_type_name != null) 6 else 0;
            },
            .F64 => tag_const = 1,
            .I8 => tag_const = 2,
            .PTR => {
                if (value.array_type != null) {
                    tag_const = 4;
                } else if (value.struct_field_types != null or value.struct_type_name != null) {
                    tag_const = 5;
                } else {
                    tag_const = 3; // string by default
                }
            },
            .I2, .I1 => tag_const = 7,
            .Nothing => tag_const = 8,
            .Value => tag_const = 8,
        }

        // Compute reserved bits if targeting a union
        var reserved_const: u32 = 0;
        if (target_union) |ut| {
            if (ut == .Union) {
                const idx = self.findUnionMemberIndex(ut, value);
                const uid = ut.Union.id & 0x7FFF;
                reserved_const = 0x80000000 | (uid << 16) | (idx & 0xFFFF);
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
            .String => 8,
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
                var ptr_val = value;
                if (ptr_val.ty != .PTR) {
                    ptr_val = try self.ensurePointer(w, value, id);
                }
                // Strings may be stack temporaries (e.g. from `each c in someString`),
                // so clone before storing into arrays/maps to avoid dangling pointers.
                const cloned = try self.nextTemp(id);
                const clone_line = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_str_clone(ptr {s})\n", .{ cloned, ptr_val.name });
                defer self.allocator.free(clone_line);
                try w.writeAll(clone_line);
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ tmp, cloned });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .I64 };
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
            .String, .Map, .Function, .Union => blk_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                break :blk_ptr StackVal{ .name = tmp, .ty = .PTR };
            },
            .Struct => blk_struct_ptr: {
                const tmp = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ tmp, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                // Preserve the fact that this pointer is a struct value even when we
                // don't have concrete struct field metadata available (e.g., values
                // retrieved from a map and wrapped into a union).
                break :blk_struct_ptr StackVal{ .name = tmp, .ty = .PTR, .struct_type_name = "struct" };
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
