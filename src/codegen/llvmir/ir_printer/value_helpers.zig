const std = @import("std");
const doxa_rt = @import("../../../runtime/doxa_rt.zig");
const DoxaTag = doxa_rt.DoxaTag;
const DoxaBoxMeta = doxa_rt.DoxaBoxMeta;

pub fn Methods(comptime Ctx: type) type {
    const IRPrinter = Ctx.IRPrinter;
    const HIR = Ctx.HIR;
    const StackVal = Ctx.StackVal;

    return struct {
    /// The `%str_out_ptr` / `%str_out_len` slots a string-returning runtime call
    /// writes through. Both body emitters allocate them in the entry block
    /// before the first body instruction, so every arm of a branch that calls
    /// into the runtime writes through memory that dominates them all.
    pub const StrOutSlots = struct { ptr: []const u8, len: []const u8 };

    pub fn strOutSlots(self: *IRPrinter) StrOutSlots {
        return .{ .ptr = self.entry_str_out_ptr.?, .len = self.entry_str_out_len.? };
    }

    /// Emit the call and leave its result in `slots`. Reading the slots is the
    /// caller's move, so a branch can write every arm first and read once where
    /// they merge.
    pub fn callReturningString(
        self: *IRPrinter,
        w: anytype,
        fn_name: []const u8,
        args_line: []const u8,
        slots: StrOutSlots,
    ) !void {
        const init_ptr_line = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{slots.ptr});
        const init_len_line = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{slots.len});
        defer self.allocator.free(init_ptr_line);
        defer self.allocator.free(init_len_line);
        try w.writeAll(init_ptr_line);
        try w.writeAll(init_len_line);

        const call_line = if (args_line.len > 0)
            try std.fmt.allocPrint(self.allocator, "  call void @{s}({s}, ptr {s}, ptr {s})\n", .{ fn_name, args_line, slots.ptr, slots.len })
        else
            try std.fmt.allocPrint(self.allocator, "  call void @{s}(ptr {s}, ptr {s})\n", .{ fn_name, slots.ptr, slots.len });
        defer self.allocator.free(call_line);
        try w.writeAll(call_line);
    }

    /// Read `slots` and push the `%DoxaString` a string call left there.
    pub fn pushStringResult(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        slots: StrOutSlots,
    ) !void {
        const loaded_ptr = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const loaded_len = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const load_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ loaded_ptr, slots.ptr });
        const load_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ loaded_len, slots.len });
        defer self.allocator.free(load_ptr_line);
        defer self.allocator.free(load_len_line);
        try w.writeAll(load_ptr_line);
        try w.writeAll(load_len_line);

        const tmp_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, loaded_ptr });
        defer self.allocator.free(ins0);
        try w.writeAll(ins0);

        const result_name = try std.fmt.allocPrint(self.allocator, "%{d}", .{id.*});
        id.* += 1;
        const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ result_name, tmp_name, loaded_len });
        defer self.allocator.free(ins1);
        try w.writeAll(ins1);

        // The runtime string helpers allocate their result into the scope active
        // at this call (concat, conversions, substring, ...), so the result is a
        // fresh value of the current region. Callers that instead read an existing
        // string (e.g. `doxa_array_get_str`) override the region with the
        // container's region immediately after.
        try stack.append(.{ .name = result_name, .ty = .STRING, .region = self.currentRegionTag() });
    }

    pub fn emitRTCallReturningString(
        self: *IRPrinter,
        w: anytype,
        stack: *std.array_list.Managed(StackVal),
        id: *usize,
        fn_name: []const u8,
        args_line: []const u8,
    ) !void {
        const slots = self.strOutSlots();
        try self.callReturningString(w, fn_name, args_line, slots);
        try self.pushStringResult(w, stack, id, slots);
    }

    /// An uninitialised `%DoxaValue` slot in the entry block, for a runtime
    /// entry that writes a box through `ptr`. Hoisted for the reason
    /// `boxDoxaValue` gives.
    pub fn doxaValueSlot(self: *IRPrinter) ![]const u8 {
        const slot_name = try std.fmt.allocPrint(self.allocator, "%doxa.value.box.{d}", .{self.synth_header_counter});
        self.synth_header_counter += 1;
        try self.entry_allocas.append(try std.fmt.allocPrint(self.allocator, "  {s} = alloca %DoxaValue\n", .{slot_name}));
        return slot_name;
    }

    /// An addressable copy of a boxed `%DoxaValue`, for a runtime entry that
    /// takes `ptr`. The slot is hoisted to the entry block: this value can sit
    /// in a loop, and a per-iteration alloca would grow the shadow stack every
    /// time round. Named rather than a numeric temp because the alloca is
    /// replayed in the entry block, and LLVM requires unnamed temps to be
    /// numbered in order.
    pub fn boxDoxaValue(self: *IRPrinter, w: anytype, val: StackVal) ![]const u8 {
        const box_name = try self.doxaValueSlot();
        const store_line = try std.fmt.allocPrint(self.allocator, "  store %DoxaValue {s}, ptr {s}\n", .{ val.name, box_name });
        defer self.allocator.free(store_line);
        try w.writeAll(store_line);
        return box_name;
    }

    /// The canonical key of the enum a value of `hir_type` renders as. A boxed
    /// enum is named by the runtime through the box registry instead.
    pub fn enumTypeNameFor(self: *IRPrinter, hir_type: HIR.HIRType) ?[]const u8 {
        return switch (hir_type) {
            .Enum => |eid| self.enum_table.keyOf(eid),
            else => null,
        };
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
            .Value => {
                const payload = try self.nextTemp(id);
                const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, current_name });
                defer self.allocator.free(extract_line);
                try w.writeAll(extract_line);
                current_name = payload;
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
            // TODO(struct default): `.Nothing` reaches here from an
            // uninitialized struct declaration and widens `"{}"` — a
            // zero-sized type — into `zext {} 0 to i64`, which `zig cc` rejects.
            // `zext` is only ever correct for an integer stack type, so this
            // fall-through must not be how a non-integer arrives. The fix is
            // gated on the struct-default decision in
            // plan/uninitialized-declarations.md; behaviour is unchanged until
            // that lands.
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
    pub fn ensureString(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        id: *usize,
    ) !StackVal {
        if (value.ty == .STRING) return value;
        if (value.ty == .Value) {
            // Boxed union value holding a string member: recover (ptr, len) from
            // the two payload words so downstream consumers see the full string.
            const payload = try self.nextTemp(id);
            const extract_bits = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, value.name });
            defer self.allocator.free(extract_bits);
            try w.writeAll(extract_bits);
            const as_ptr = try self.nextTemp(id);
            const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
            defer self.allocator.free(cast_line);
            try w.writeAll(cast_line);
            const payload_len = try self.nextTemp(id);
            const len_extract = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 3\n", .{ payload_len, value.name });
            defer self.allocator.free(len_extract);
            try w.writeAll(len_extract);
            const tmp_name = try self.nextTemp(id);
            const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_name, as_ptr });
            defer self.allocator.free(ins0);
            try w.writeAll(ins0);
            const str_name = try self.nextTemp(id);
            const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ str_name, tmp_name, payload_len });
            defer self.allocator.free(ins1);
            try w.writeAll(ins1);
            return .{ .name = str_name, .ty = .STRING };
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

        // Narrowed to a group (`string | Error` as `Error`): the value stays
        // boxed, re-packed to name its member among the group's.
        if (isBoxedMemberType(target)) {
            const source = value.boxed_type orelse return self.hirFault("a box narrowed to a {s} does not say which box it is", .{@tagName(target)});
            return if (source.eql(target)) value else try repackBox(self, w, value, source, target, id);
        }
        // `nothing` carries no payload.
        if (target == .Nothing) return .{ .name = "undef", .ty = .Nothing };

        const payload = try self.nextTemp(id);
        const extract_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 2\n", .{ payload, value.name });
        defer self.allocator.free(extract_line);
        try w.writeAll(extract_line);

        return switch (target) {
            .Int => .{ .name = payload, .ty = .I64 },
            .Enum => blk: {
                break :blk StackVal{ .name = payload, .ty = .I64, .enum_type_name = self.enum_table.keyOf(target.Enum) };
            },
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
            .String => blk: {
                const as_ptr = try self.nextTemp(id);
                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                defer self.allocator.free(cast_line);
                try w.writeAll(cast_line);

                const payload_len = try self.nextTemp(id);
                const len_extract = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaValue {s}, 3\n", .{ payload_len, value.name });
                defer self.allocator.free(len_extract);
                try w.writeAll(len_extract);

                const tmp_ds = try self.nextTemp(id);
                const ins0 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString undef, ptr {s}, 0\n", .{ tmp_ds, as_ptr });
                defer self.allocator.free(ins0);
                try w.writeAll(ins0);
                const str_name = try self.nextTemp(id);
                const ins1 = try std.fmt.allocPrint(self.allocator, "  {s} = insertvalue %DoxaString {s}, i64 {s}, 1\n", .{ str_name, tmp_ds, payload_len });
                defer self.allocator.free(ins1);
                try w.writeAll(ins1);
                break :blk StackVal{ .name = str_name, .ty = .STRING, .array_type = value.array_type, .enum_type_name = value.enum_type_name, .struct_field_types = value.struct_field_types, .struct_field_names = value.struct_field_names, .struct_type_name = value.struct_type_name };
            },
            .Array => |inner| blk: {
                const as_ptr = try self.nextTemp(id);
                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                defer self.allocator.free(cast_line);
                try w.writeAll(cast_line);
                break :blk StackVal{ .name = as_ptr, .ty = .PTR, .array_type = inner.* };
            },
            .Map, .Function => blk: {
                const as_ptr = try self.nextTemp(id);
                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                defer self.allocator.free(cast_line);
                try w.writeAll(cast_line);
                break :blk StackVal{ .name = as_ptr, .ty = .PTR };
            },
            .Struct => |sid| blk: {
                const as_ptr = try self.nextTemp(id);
                const cast_line = try std.fmt.allocPrint(self.allocator, "  {s} = inttoptr i64 {s} to ptr\n", .{ as_ptr, payload });
                defer self.allocator.free(cast_line);
                try w.writeAll(cast_line);
                const type_name = self.struct_type_names_by_id.get(sid);
                break :blk StackVal{
                    .name = as_ptr,
                    .ty = .PTR,
                    .struct_field_types = self.struct_fields_by_id.get(sid),
                    .struct_field_names = if (type_name) |tn| self.struct_field_names_by_type.get(tn) else null,
                    .struct_type_name = type_name orelse "struct",
                };
            },
            else => StackVal{ .name = payload, .ty = .I64 },
        };
    }

    /// The member type a box of type `boxed` names by `index`: a union's member,
    /// or a group's flattened member. A union never has a group member
    /// (`UnionTable` flattens it), so every member is a concrete type.
    pub fn boxMember(self: *IRPrinter, boxed: HIR.HIRType, index: usize) ?HIR.HIRType {
        return switch (boxed) {
            .Union => |u| if (index < u.members.len) u.members[index].* else null,
            .Group => |gid| blk: {
                const members = self.group_table.members(gid) orelse break :blk null;
                if (index >= members.len) break :blk null;
                const member = members[index];
                break :blk switch (member.kind) {
                    .Enum => HIR.HIRType{ .Enum = member.id },
                    .Struct => HIR.HIRType{ .Struct = member.id },
                    .Group => HIR.HIRType{ .Group = member.id },
                };
            },
            else => null,
        };
    }

    /// How many members a box of type `boxed` can name.
    pub fn boxMemberCount(self: *IRPrinter, boxed: HIR.HIRType) usize {
        return switch (boxed) {
            .Union => |u| u.members.len,
            .Group => |gid| if (self.group_table.members(gid)) |members| members.len else 0,
            else => 0,
        };
    }

    /// The canonical key of a named member type, which a stack value carries
    /// as its `enum_type_name` or `struct_type_name`.
    fn namedMemberKey(self: *IRPrinter, member: HIR.HIRType) ?[]const u8 {
        return switch (member) {
            .Enum => |id| self.enum_table.keyOf(id),
            .Struct => |id| self.struct_table.keyOf(id),
            else => null,
        };
    }

    /// Active member index for an unboxed `value` placed into a box of type
    /// `boxed`. A named value (an enum word, a struct pointer) is the member
    /// whose type it carries; any other value is the member of its
    /// representation.
    pub fn findMemberIndex(self: *IRPrinter, boxed: HIR.HIRType, value: StackVal) u32 {
        const count = self.boxMemberCount(boxed);
        const named_key: ?[]const u8 = switch (value.ty) {
            .I64 => value.enum_type_name,
            .PTR => value.struct_type_name,
            else => null,
        };
        if (named_key) |key| {
            for (0..count) |idx| {
                const member_key = namedMemberKey(self, self.boxMember(boxed, idx).?) orelse continue;
                if (std.mem.eql(u8, member_key, key)) return @intCast(idx);
            }
        }
        for (0..count) |idx| {
            const member = self.boxMember(boxed, idx).?;
            const fits = switch (value.ty) {
                // An unnamed word is an int; an enum flowing into a union
                // without its own enum member keeps the first word member.
                .I64 => member == .Int or (named_key != null and member == .Enum),
                .F64 => member == .Float,
                .I8 => member == .Byte,
                .I2, .I1 => member == .Tetra,
                .PTR => if (value.array_type != null) member == .Array else member == .Struct or member == .Map or member == .Function,
                .STRING => member == .String,
                .Nothing => member == .Nothing,
                .Value => false,
            };
            if (fits) return @intCast(idx);
        }
        return 0;
    }

    /// Whether `t` stores as a boxed `%DoxaValue`: unions and groups are the
    /// same runtime shape, differing only in how `reserved` names the member.
    pub fn isBoxedMemberType(t: HIR.HIRType) bool {
        return t == .Union or t == .Group;
    }

    /// The type id packed beside the member index: a union's id or a group's id.
    /// The `reserved` word of a box of type `boxed` before its member index
    /// is or-ed in: the boxed flag and the type's box id, numbered here the
    /// first time the type is boxed.
    pub fn boxHeader(self: *IRPrinter, boxed: HIR.HIRType) !u32 {
        const key: IRPrinter.BoxKey = switch (boxed) {
            .Union => |u| .{ .kind = .Union, .id = u.id },
            .Group => |gid| .{ .kind = .Group, .id = gid },
            else => return self.hirFault("boxes a {s}, which is not a union or a group", .{@tagName(boxed)}),
        };
        const entry = try self.box_ids.getOrPut(self.allocator, key);
        if (!entry.found_existing) {
            const box_id: u32 = @intCast(self.boxed_types.items.len);
            if (box_id > DoxaBoxMeta.max_box_id) {
                _ = self.box_ids.remove(key);
                return self.hirFault("boxes more than {d} union and group types", .{DoxaBoxMeta.max_box_id + 1});
            }
            try self.boxed_types.append(self.allocator, boxed);
            entry.value_ptr.* = box_id;
        }
        return DoxaBoxMeta.is_boxed_bit | (entry.value_ptr.* << DoxaBoxMeta.box_id_shift);
    }

    /// Re-pack a box for another box type: `int | string` stored into
    /// `int | float | string`, an `Error` returned through `string | Error`,
    /// or a `string | Error` narrowed to `Error`. The box names its member by
    /// index into the source's members; that index is rewritten to the
    /// target's index for the same member type. A source member the target
    /// does not hold is one a type test has already excluded.
    fn repackBox(self: *IRPrinter, w: anytype, value: StackVal, source: HIR.HIRType, target: HIR.HIRType, id: *usize) !StackVal {
        const header = try self.boxHeader(target);

        const reserved = try self.nextTemp(id);
        try w.print("  {s} = extractvalue %DoxaValue {s}, 1\n", .{ reserved, value.name });
        const member = try self.nextTemp(id);
        try w.print("  {s} = and i32 {s}, {d}\n", .{ member, reserved, DoxaBoxMeta.member_index_mask });

        var acc: ?[]const u8 = null;
        for (0..self.boxMemberCount(source)) |source_idx| {
            const source_member = self.boxMember(source, source_idx).?;
            const target_idx = for (0..self.boxMemberCount(target)) |idx| {
                if (source_member.eql(self.boxMember(target, idx).?)) break idx;
            } else continue;
            const repacked = header | (@as(u32, @intCast(target_idx)) & DoxaBoxMeta.member_index_mask);
            if (acc) |previous| {
                const is_member = try self.nextTemp(id);
                const next = try self.nextTemp(id);
                try w.print("  {s} = icmp eq i32 {s}, {d}\n", .{ is_member, member, source_idx });
                try w.print("  {s} = select i1 {s}, i32 {d}, i32 {s}\n", .{ next, is_member, repacked, previous });
                acc = next;
            } else {
                const next = try self.nextTemp(id);
                try w.print("  {s} = add i32 0, {d}\n", .{ next, repacked });
                acc = next;
            }
        }
        const repacked_reserved = acc orelse return self.hirFault("a {s} box is re-packed as a {s} that holds none of its members", .{ @tagName(source), @tagName(target) });

        const repacked_value = try self.nextTemp(id);
        try w.print("  {s} = insertvalue %DoxaValue {s}, i32 {s}, 1\n", .{ repacked_value, value.name, repacked_reserved });
        return StackVal{ .name = repacked_value, .ty = .Value, .boxed_type = target };
    }

    pub fn buildDoxaValue(
        self: *IRPrinter,
        w: anytype,
        value: StackVal,
        target_union: ?HIR.HIRType,
        id: *usize,
    ) !StackVal {
        // If it's already a canonical value, reuse it — unless it is being
        // placed into a different box (a group returned through a union, say).
        // The box's reserved word names its own member index; leaving it would
        // hand the caller the index of the source box, not the target's.
        if (value.ty == .Value) {
            if (target_union) |ut| {
                if (value.boxed_type) |src| {
                    if (isBoxedMemberType(ut) and !src.eql(ut)) return try repackBox(self, w, value, src, ut, id);
                }
            }
            return value;
        }

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
                    tag_const = @intFromEnum(tag.Function);
                }
            },
            .STRING => tag_const = @intFromEnum(tag.String),
            .I2, .I1 => tag_const = @intFromEnum(tag.Tetra),
            .Nothing => tag_const = @intFromEnum(tag.Nothing),
            .Value => tag_const = @intFromEnum(tag.Nothing),
        }

        // A box's reserved word names its box type and the member it holds.
        var reserved_const: u32 = 0;
        if (target_union) |ut| {
            if (isBoxedMemberType(ut)) {
                const idx = self.findMemberIndex(ut, value);
                reserved_const = try self.boxHeader(ut) | (idx & DoxaBoxMeta.member_index_mask);
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

        // For string values, store (ptr, len) in the two payload words. The
        // string is cloned into the current scope so the boxed value never
        // holds a pointer into a shorter-lived arena.
        var payload_bits: StackVal = undefined;
        var payload_len: StackVal = undefined;
        if (value.ty == .STRING) {
            const s_ptr = try self.nextTemp(id);
            const s_len = try self.nextTemp(id);
            const ext0 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, value.name });
            const ext1 = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, value.name });
            defer self.allocator.free(ext0);
            defer self.allocator.free(ext1);
            try w.writeAll(ext0);
            try w.writeAll(ext1);

            const out_ptr_slot = try self.nextTemp(id);
            const out_len_slot = try self.nextTemp(id);
            const alloca_ptr = try std.fmt.allocPrint(self.allocator, "  {s} = alloca ptr\n", .{out_ptr_slot});
            const alloca_len = try std.fmt.allocPrint(self.allocator, "  {s} = alloca i64\n", .{out_len_slot});
            defer self.allocator.free(alloca_ptr);
            defer self.allocator.free(alloca_len);
            try w.writeAll(alloca_ptr);
            try w.writeAll(alloca_len);
            const store_null = try std.fmt.allocPrint(self.allocator, "  store ptr null, ptr {s}\n", .{out_ptr_slot});
            const store_zero = try std.fmt.allocPrint(self.allocator, "  store i64 0, ptr {s}\n", .{out_len_slot});
            defer self.allocator.free(store_null);
            defer self.allocator.free(store_zero);
            try w.writeAll(store_null);
            try w.writeAll(store_zero);
            const clone_line = try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_clone_at(i64 0, ptr {s}, i64 {s}, ptr {s}, ptr {s})\n", .{ s_ptr, s_len, out_ptr_slot, out_len_slot });
            defer self.allocator.free(clone_line);
            try w.writeAll(clone_line);

            const cloned_ptr = try self.nextTemp(id);
            const cloned_len = try self.nextTemp(id);
            const load_ptr = try std.fmt.allocPrint(self.allocator, "  {s} = load ptr, ptr {s}\n", .{ cloned_ptr, out_ptr_slot });
            const load_len = try std.fmt.allocPrint(self.allocator, "  {s} = load i64, ptr {s}\n", .{ cloned_len, out_len_slot });
            defer self.allocator.free(load_ptr);
            defer self.allocator.free(load_len);
            try w.writeAll(load_ptr);
            try w.writeAll(load_len);

            const as_i64 = try self.nextTemp(id);
            const pi = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ as_i64, cloned_ptr });
            defer self.allocator.free(pi);
            try w.writeAll(pi);
            payload_bits = StackVal{ .name = as_i64, .ty = .I64 };
            payload_len = StackVal{ .name = cloned_len, .ty = .I64 };
        } else {
            payload_bits = try self.ensureI64(w, value, id);
            payload_len = StackVal{ .name = "0", .ty = .I64 };
        }

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
            .{ dv2, dv1, payload_bits.name },
        );
        defer self.allocator.free(dv2_line);
        try w.writeAll(dv2_line);

        const dv3 = try self.nextTemp(id);
        const dv3_line = try std.fmt.allocPrint(
            self.allocator,
            "  {s} = insertvalue %DoxaValue {s}, i64 {s}, 3\n",
            .{ dv3, dv2, payload_len.name },
        );
        defer self.allocator.free(dv3_line);
        try w.writeAll(dv3_line);

        return .{ .name = dv3, .ty = .Value };
    }

    pub fn arrayElementSize(_: *IRPrinter, element_type: HIR.HIRType) u64 {
        return switch (element_type) {
            .Int => 8,
            .Byte => 1,
            .Float => 8,
            .String => 16,
            .Tetra => 1,
            .Nothing => 0,
            // A union or group element is its whole box, so it keeps the
            // member it holds (`%DoxaValue`, runtime tag 9).
            .Union, .Group => 24,
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
            .Union, .Group => 9,
            else => 255,
        };
    }

    pub fn fixedArrayInnermostLLVMType(_: *IRPrinter, element_type: HIR.HIRType) []const u8 {
        var cursor = element_type;
        while (true) {
            switch (cursor) {
                .Array => |inner| cursor = inner.*,
                .Int => return "i64",
                .Float => return "double",
                .Byte => return "i8",
                .Tetra => return "i2",
                else => return "i64",
            }
        }
    }

    pub fn buildFixedArrayLLVMTypeStr(
        self: *IRPrinter,
        element_type: HIR.HIRType,
        size: u32,
        nested_sizes: [4]u32,
        nested_depth: u3,
    ) ![]const u8 {
        const base = self.fixedArrayInnermostLLVMType(element_type);
        var result = try self.allocator.dupe(u8, base);

        var i: i32 = @as(i32, @intCast(nested_depth)) - 1;
        while (i >= 0) : (i -= 1) {
            const wrapped = try std.fmt.allocPrint(self.allocator, "[{d} x {s}]", .{ nested_sizes[@intCast(i)], result });
            result = wrapped;
        }

        const full = try std.fmt.allocPrint(self.allocator, "[{d} x {s}]", .{ size, result });
        return full;
    }

    pub fn fixedArrayLevelLLVMType(
        self: *IRPrinter,
        base_llvm_type: []const u8,
        sizes: [4]u32,
        depth: u3,
    ) ![]const u8 {
        var result = try self.allocator.dupe(u8, base_llvm_type);
        var i: i32 = @as(i32, @intCast(depth)) - 1;
        while (i >= 0) : (i -= 1) {
            const wrapped = try std.fmt.allocPrint(self.allocator, "[{d} x {s}]", .{ sizes[@intCast(i)], result });
            result = wrapped;
        }
        return result;
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
                if (value.ty == .F64) {
                    const tmp = try self.nextTemp(id);
                    const line = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, value.name });
                    defer self.allocator.free(line);
                    try w.writeAll(line);
                    break :blk StackVal{ .name = tmp, .ty = .I64 };
                }
                const as_i64 = try self.ensureI64(w, value, id);
                const as_double = try self.nextTemp(id);
                const line = try std.fmt.allocPrint(self.allocator, "  {s} = sitofp i64 {s} to double\n", .{ as_double, as_i64.name });
                defer self.allocator.free(line);
                try w.writeAll(line);
                const tmp = try self.nextTemp(id);
                const line2 = try std.fmt.allocPrint(self.allocator, "  {s} = bitcast double {s} to i64\n", .{ tmp, as_double });
                defer self.allocator.free(line2);
                try w.writeAll(line2);
                break :blk StackVal{ .name = tmp, .ty = .I64 };
            },
            .Enum => try self.ensureI64(w, value, id),
            .String => blk_ptr: {
                var str_val = value;
                if (str_val.ty != .STRING) {
                    str_val = try self.ensureString(w, value, id);
                }
                const s_ptr = try self.nextTemp(id);
                const s_ptr_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 0\n", .{ s_ptr, str_val.name });
                defer self.allocator.free(s_ptr_line);
                try w.writeAll(s_ptr_line);
                const s_len = try self.nextTemp(id);
                const s_len_line = try std.fmt.allocPrint(self.allocator, "  {s} = extractvalue %DoxaString {s}, 1\n", .{ s_len, str_val.name });
                defer self.allocator.free(s_len_line);
                try w.writeAll(s_len_line);
                const cloned_raw = try self.nextTemp(id);
                const clone_call = try std.fmt.allocPrint(self.allocator, "  {s} = call ptr @doxa_str_clone_raw(ptr {s}, i64 {s})\n", .{ cloned_raw, s_ptr, s_len });
                defer self.allocator.free(clone_call);
                try w.writeAll(clone_call);
                // Return the cloned pointer as i64 for array storage
                const ptr_i64 = try self.nextTemp(id);
                const pi = try std.fmt.allocPrint(self.allocator, "  {s} = ptrtoint ptr {s} to i64\n", .{ ptr_i64, cloned_raw });
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
                const clone_call = try std.fmt.allocPrint(self.allocator, "  call void @doxa_str_from_cstr(ptr {s}, ptr {s}, ptr {s})\n", .{ tmp, out_ptr_slot, out_len_slot });
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
