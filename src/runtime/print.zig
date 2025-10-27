const std = @import("std");
const HIRValue = @import("../codegen/hir/soxa_values.zig").HIRValue;
const TokenLiteral = @import("../types/types.zig").TokenLiteral;
const hir_instructions = @import("../codegen/hir/soxa_instructions.zig");
const HIRInstruction = hir_instructions.HIRInstruction;
const Core = @import("../interpreter/core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
pub const getTypeString = PrintOps.getTypeString;
pub const formatHIRValue = PrintOps.formatHIRValue;
pub const formatHIRValueRaw = PrintOps.formatHIRValueRaw;
pub const printHIRValue = PrintOps.printHIRValue;
const SoxaTypes = @import("../codegen/hir/soxa_types.zig");

fn printToStdout(comptime format: []const u8, args: anytype) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    try stdout.print(format, args);
    try stdout.flush();
}

pub const PrintOps = struct {
    pub fn printRaw(vm: anytype, s: []const u8) !void {
        _ = vm; // unused for now
        try printToStdout("{s}", .{s});
    }
    fn getVmConstant(vm: anytype, id: usize) ?HIRValue {
        const vm_type = @TypeOf(vm);
        const info = @typeInfo(vm_type);
        switch (info) {
            .pointer => |ptr| {
                const child = ptr.child;
                if (@hasField(child, "program")) {
                    if (id < vm.program.constant_pool.len) {
                        return vm.program.constant_pool[id];
                    }
                }
                if (@hasField(child, "bytecode")) {
                    const module_ptr = vm.bytecode;
                    if (module_ptr.*.constants.len > id) {
                        return module_ptr.*.constants[id];
                    }
                }
            },
            else => {},
        }
        return null;
    }

    pub fn execPrint(vm: anytype) !void {
        const value = try vm.stack.pop();

        try PrintOps.formatHIRValueRaw(vm, value.value);
        try printToStdout("\n", .{});

        try vm.stack.push(value);
    }

    pub fn execPeek(vm: anytype, peek: anytype) !void {
        const value = try vm.stack.peek();

        if (peek.location) |location| {
            try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
        }

        if (peek.name) |name| {
            if (peek.union_members) |members| {
                if (members.len > 1) {
                    const active = PrintOps.getTypeString(vm, value.value);
                    try printToStdout("{s} :: ", .{name});
                    for (members, 0..) |m, i| {
                        if (i > 0) try printToStdout(" | ", .{});
                        if (std.mem.eql(u8, m, active)) try printToStdout(">", .{});
                        try printToStdout("{s}", .{m});
                    }
                    try printToStdout(" is ", .{});
                } else {
                    const type_string = PrintOps.getTypeString(vm, value.value);
                    try printToStdout("{s} :: {s} is ", .{ name, type_string });
                }
            } else {
                const type_string = PrintOps.getTypeString(vm, value.value);
                try printToStdout("{s} :: {s} is ", .{ name, type_string });
            }
        } else {
            if (peek.union_members) |members| {
                if (members.len > 1) {
                    const active = PrintOps.getTypeString(vm, value.value);
                    try printToStdout(":: ", .{});
                    for (members, 0..) |m, i| {
                        if (i > 0) try printToStdout(" | ", .{});
                        if (std.mem.eql(u8, m, active)) try printToStdout(">", .{});
                        try printToStdout("{s}", .{m});
                    }
                    try printToStdout(" is ", .{});
                } else {
                    const type_string = PrintOps.getTypeString(vm, value.value);
                    try printToStdout(":: {s} is ", .{type_string});
                }
            } else {
                const type_string = PrintOps.getTypeString(vm, value.value);
                try printToStdout(":: {s} is ", .{type_string});
            }
        }

        try PrintOps.formatHIRValue(vm, value.value);
        try printToStdout("\n", .{});

        try vm.stack.push(value);
    }

    pub fn execPeekStruct(vm: anytype, i: anytype) !void {
        const value = try vm.stack.peek();

        switch (value.value) {
            .struct_instance => |s| {
                for (s.fields) |field| {
                    if (i.location) |location| {
                        try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                    }

                    var field_path: []const u8 = undefined;
                    if (s.path) |path| {
                        field_path = try std.fmt.allocPrint(vm.allocator, "{s}.{s}", .{ path, field.name });
                    } else {
                        field_path = try std.fmt.allocPrint(vm.allocator, "{s}.{s}", .{ s.type_name, field.name });
                    }
                    defer vm.allocator.free(field_path);

                    const field_type_string = PrintOps.getTypeString(vm, field.value);
                    try printToStdout(":: {s} is ", .{field_type_string});

                    switch (field.value) {
                        .struct_instance => |nested| {
                            const field_names = try vm.allocator.alloc([]const u8, nested.fields.len);
                            const field_types = try vm.allocator.alloc(@TypeOf(nested.fields[0].field_type), nested.fields.len);
                            for (nested.fields, 0..) |nested_field, field_idx| {
                                field_names[field_idx] = nested_field.name;
                                field_types[field_idx] = nested_field.field_type;
                            }
                            const nested_peek = HIRInstruction{
                                .PeekStruct = .{
                                    .location = i.location,
                                    .field_names = field_names,
                                    .type_name = nested.type_name,
                                    .field_count = @intCast(nested.fields.len),
                                    .field_types = field_types,
                                    .should_pop_after_peek = i.should_pop_after_peek,
                                },
                            };
                            var nested_with_path = nested;
                            nested_with_path.path = field_path;
                            try vm.stack.push(HIRFrame{ .value = HIRValue{ .struct_instance = nested_with_path } });
                            const vm_type = @TypeOf(vm);
                            const info = @typeInfo(vm_type);
                            switch (info) {
                                .pointer => |ptr| {
                                    const child = ptr.child;
                                    if (@hasField(child, "program")) {
                                        try vm.executeInstruction(nested_peek);
                                    } else if (@hasField(child, "bytecode")) {
                                        try PrintOps.formatHIRValue(vm, field.value);
                                        try printToStdout("\n", .{});
                                        return;
                                    }
                                },
                                else => {
                                    try vm.executeInstruction(nested_peek);
                                },
                            }
                        },
                        else => {
                            try PrintOps.formatHIRValue(vm, field.value);
                            try printToStdout("\n", .{});
                        },
                    }
                }
            },
            else => {
                if (i.field_names.len > 0) {
                    if (i.location) |location| {
                        try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                    }

                    const value_type_string = PrintOps.getTypeString(vm, value.value);
                    try printToStdout(":: {s} is ", .{value_type_string});
                    try PrintOps.formatHIRValue(vm, value.value);
                    try printToStdout("\n", .{});
                }
            },
        }

        if (!i.should_pop_after_peek) {
            try vm.stack.push(value);
        }
    }

    pub fn execPrintInterpolated(vm: anytype, interp: anytype) !void {
        var args = try vm.allocator.alloc(HIRValue, interp.argument_count);
        defer vm.allocator.free(args);

        for (0..interp.argument_count) |i| {
            const arg = try vm.stack.pop();
            args[interp.argument_count - 1 - i] = arg.value;
        }

        var actual_format_parts = std.array_list.Managed([]const u8).init(vm.allocator);
        defer actual_format_parts.deinit();

        for (interp.format_part_ids) |id| {
            if (getVmConstant(vm, id)) |constant| {
                if (constant == .string) {
                    try actual_format_parts.append(constant.string);
                    continue;
                }
            }
            try actual_format_parts.append("");
        }

        for (actual_format_parts.items, 0..) |part, i| {
            if (part.len != 0) {
                try printToStdout("{s}", .{part});
            }

            if (i < interp.placeholder_indices.len) {
                const arg_index = interp.placeholder_indices[i];
                if (arg_index < args.len) {
                    try PrintOps.formatHIRValueRaw(vm, args[arg_index]);
                }
            }
        }
    }

    pub fn printHIRValue(vm: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try printToStdout("{}", .{i}),
            .float => |f| try printToStdout("{d}", .{f}),
            .string => |s| try printToStdout("\"{s}\"", .{s}),
            .tetra => |b| try printToStdout("{}", .{b}),
            .byte => |u| try printToStdout("{}", .{u}),
            .nothing => try printToStdout("nothing", .{}),
            .array => |arr| {
                try printToStdout("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.printHIRValue(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_instance => |s| {
                try printToStdout("{{ ", .{});
                for (s.fields, 0..) |field, i| {
                    try printToStdout("{s}: ", .{field.name});
                    try PrintOps.printHIRValue(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{});
            },
            .map => try printToStdout("{{map}}", .{}),
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                if (vm.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    try PrintOps.formatTokenLiteral(vm, storage.value);
                } else {
                    try printToStdout("storage_id_ref({})", .{storage_id});
                }
            },
        }
    }

    var type_buf: [128]u8 = undefined;

    fn mapBaseTypeNameFromHIRType(t: SoxaTypes.HIRType) []const u8 {
        return switch (t) {
            .Int => "int",
            .Byte => "byte",
            .Float => "float",
            .String => "string",
            .Tetra => "tetra",
            .Nothing => "nothing",
            .Struct => "struct",
            .Map => "map",
            .Enum => "enum",
            .Function => "function",
            .Union => "union",
            else => "unknown",
        };
    }

    fn mapBaseTypeNameFromValue(v: HIRValue) []const u8 {
        return switch (v) {
            .int => "int",
            .byte => "byte",
            .float => "float",
            .string => "string",
            .tetra => "tetra",
            .nothing => "nothing",
            .struct_instance => "struct",
            .map => "map",
            .enum_variant => "enum",
            else => "unknown",
        };
    }

    fn firstNonNothingElement(elems: []HIRValue) ?HIRValue {
        for (elems) |e| {
            if (!@import("std").meta.eql(e, HIRValue.nothing)) return e;
        }
        return null;
    }

    fn buildArrayTypeStringFromValue(value: HIRValue) []const u8 {
        var depth: usize = 0;
        var cursor: HIRValue = value;

        while (true) {
            switch (cursor) {
                .array => |arr| {
                    depth += 1;
                    if (firstNonNothingElement(arr.elements)) |first_elem| {
                        switch (first_elem) {
                            .array => {
                                cursor = first_elem;
                                continue;
                            },
                            else => {
                                const base = mapBaseTypeNameFromValue(first_elem);
                                return writeTypeWithBrackets(base, depth);
                            },
                        }
                    } else {
                        if (arr.element_type != .Unknown) {
                            const base = mapBaseTypeNameFromHIRType(arr.element_type);
                            return writeTypeWithBrackets(base, depth);
                        } else {
                            return writeTypeWithBrackets("unknown", depth);
                        }
                    }
                },
                else => {
                    const base2 = mapBaseTypeNameFromValue(cursor);
                    return writeTypeWithBrackets(base2, depth);
                },
            }
        }
    }

    fn writeTypeWithBrackets(base: []const u8, depth: usize) []const u8 {
        var i: usize = 0;
        while (i < base.len and i < type_buf.len) : (i += 1) type_buf[i] = base[i];
        var written: usize = i;
        var d: usize = 0;
        while (d < depth and written + 2 <= type_buf.len) : (d += 1) {
            type_buf[written] = '[';
            type_buf[written + 1] = ']';
            written += 2;
        }
        return type_buf[0..written];
    }

    pub fn getTypeString(vm: anytype, value: HIRValue) []const u8 {
        _ = vm;
        return switch (value) {
            .int => "int",
            .byte => "byte",
            .float => "float",
            .string => "string",
            .tetra => "tetra",
            .nothing => "nothing",
            .array => buildArrayTypeStringFromValue(value),
            .struct_instance => |s| {
                if (s.type_name.len == 0) {
                    return "struct";
                } else {
                    return s.type_name;
                }
            },
            .map => "map",
            .enum_variant => |e| e.type_name,
            .storage_id_ref => "storage_id_ref",
        };
    }

    pub fn formatHIRValue(vm: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try printToStdout("{}", .{i}),
            .byte => |u| try printToStdout("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(f),
            .string => |s| try printToStdout("\"{s}\"", .{s}),
            .tetra => |t| try printToStdout("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try printToStdout("nothing", .{}),
            .array => |arr| {
                try printToStdout("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.formatHIRValue(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_instance => |s| {
                try printToStdout("{{ ", .{});
                for (s.fields, 0..) |field, i| {
                    try printToStdout("{s}: ", .{field.name});
                    try PrintOps.formatHIRValue(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{});
            },
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                if (vm.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    try PrintOps.formatTokenLiteral(vm, storage.value);
                } else {
                    try printToStdout("storage_id_ref({})", .{storage_id});
                }
            },
            else => try printToStdout("{s}", .{@tagName(value)}),
        }
    }

    pub fn formatTokenLiteral(vm: anytype, value: TokenLiteral) !void {
        switch (value) {
            .int => |i| try printToStdout("{}", .{i}),
            .byte => |u| try printToStdout("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(f),
            .string => |s| try printToStdout("\"{s}\"", .{s}),
            .tetra => |t| try printToStdout("{s}", .{switch (t) {
                .false => "false",
                .true => "true",
                .both => "both",
                .neither => "neither",
            }}),
            .nothing => try printToStdout("nothing", .{}),
            .array => |arr| {
                try printToStdout("[", .{});
                var first = true;
                for (arr) |elem| {
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.formatTokenLiteral(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_value => |s| {
                try printToStdout("{{ ", .{});
                for (s.fields, 0..) |field, i| {
                    try printToStdout("{s}: ", .{field.name});
                    try PrintOps.formatTokenLiteral(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{});
            },
            .map => |m| {
                try printToStdout("{{", .{});
                var first = true;
                var iter = m.iterator();
                while (iter.next()) |entry| {
                    if (!first) try printToStdout(", ", .{});
                    try printToStdout("{s}: ", .{entry.key_ptr.*});
                    try PrintOps.formatTokenLiteral(vm, entry.value_ptr.*);
                    first = false;
                }
                try printToStdout("}}", .{});
            },
            .enum_variant => |e| try printToStdout(".{s}", .{e}),
            else => try printToStdout("{s}", .{@tagName(value)}),
        }
    }

    pub fn formatHIRValueRaw(vm: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try printToStdout("{}", .{i}),
            .byte => |u| try printToStdout("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(f),
            .string => |s| try printToStdout("{s}", .{s}),
            .tetra => |t| try printToStdout("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try printToStdout("nothing", .{}),
            .array => |arr| {
                try printToStdout("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.formatHIRValueRaw(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_instance => |s| {
                try printToStdout("{{ ", .{});
                for (s.fields, 0..) |field, i| {
                    try printToStdout(" {s}: ", .{field.name});
                    try PrintOps.formatHIRValueRaw(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{});
            },
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                if (vm.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    try PrintOps.formatTokenLiteral(vm, storage.value);
                } else {
                    try printToStdout("storage_id_ref({})", .{storage_id});
                }
            },
            else => try printToStdout("{s}", .{@tagName(value)}),
        }
    }

    fn printFloat(f: f64) !void {
        const roundedDown = std.math.floor(f);
        if (f - roundedDown == 0) {
            try printToStdout("{d}.0", .{f});
        } else {
            try printToStdout("{d}", .{f});
        }
    }

    pub fn execPrintStruct(vm: anytype, ps: anytype) !void {
        const value = try vm.stack.pop();

        try PrintOps.printHIRValue(vm, value.value);

        if (!ps.should_pop_after_peek) {
            try vm.stack.push(HIRFrame.initFromHIRValue(value.value));
        }
    }
};

// Shared runtime printing for LLVM callers
pub const ArrayHeader = extern struct {
    data: ?*anyopaque,
    len: u64,
    cap: u64,
    elem_size: u64,
    elem_tag: u64,
};

pub export fn doxa_print_array_hdr(hdr: *ArrayHeader) void {
    var buf: [1024]u8 = undefined;
    var bw = std.fs.File.stdout().writer(&buf);
    const out = &bw.interface;
    _ = out.print("[", .{}) catch return;
    const base: [*]const u8 = @ptrCast(@alignCast(if (hdr.data) |p| p else return));
    var i: u64 = 0;
    while (i < hdr.len) : (i += 1) {
        if (i != 0) _ = out.print(", ", .{}) catch return;
        const off: usize = @intCast(i * hdr.elem_size);
        const p = base + off;
        switch (hdr.elem_tag) {
            0 => { // int (i64)
                const ip: *const i64 = @ptrCast(@alignCast(p));
                _ = out.print("{d}", .{ip.*}) catch return;
            },
            1 => { // byte (u8)
                const bp: *const u8 = @ptrCast(p);
                _ = out.print("{d}", .{bp.*}) catch return;
            },
            2 => { // float (f64)
                const fp: *const f64 = @ptrCast(@alignCast(p));
                _ = out.print("{d}", .{fp.*}) catch return;
            },
            3 => { // string (i8*)
                const sp: *const ?[*:0]const u8 = @ptrCast(@alignCast(p));
                const s = sp.* orelse "";
                _ = out.print("{s}", .{s}) catch return;
            },
            4 => { // tetra (2-bit stored in u8)
                const tp: *const u8 = @ptrCast(p);
                const v: u8 = tp.* & 0x3;
                const name = switch (v) {
                    0 => "false",
                    1 => "true",
                    2 => "both",
                    3 => "neither",
                    else => "invalid",
                };
                _ = out.print("{s}", .{name}) catch return;
            },
            else => {
                _ = out.print("?", .{}) catch return;
            },
        }
    }
    _ = out.print("]", .{}) catch return;
    _ = out.flush() catch {};
}
