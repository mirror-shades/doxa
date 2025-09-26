const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const hir_instructions = @import("../../codegen/hir/soxa_instructions.zig");
const HIRInstruction = hir_instructions.HIRInstruction;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
pub const getTypeString = PrintOps.getTypeString;
pub const formatHIRValue = PrintOps.formatHIRValue;
pub const formatHIRValueRaw = PrintOps.formatHIRValueRaw;
pub const printHIRValue = PrintOps.printHIRValue;

fn printToStdout(comptime format: []const u8, args: anytype) !void {
    const stdout_file = std.fs.File.stdout();
    const formatted = try std.fmt.allocPrint(std.heap.page_allocator, format, args);
    defer std.heap.page_allocator.free(formatted);
    _ = try stdout_file.write(formatted);
}

pub const PrintOps = struct {
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

    // Execute Print instruction
    pub fn execPrint(vm: anytype) !void {
        const value = try vm.stack.pop();

        // Print instruction doesn't include variable name, just the value
        // Format the value directly without quotes or extra formatting
        try PrintOps.formatHIRValueRaw(vm, value.value);

        // Output will be flushed automatically on newline

        // Push the value back onto the stack for potential further use
        try vm.stack.push(value);
    }

    // Execute Peek instruction
    pub fn execPeek(vm: anytype, peek: anytype) !void {
        const value = try vm.stack.peek();

        if (peek.location) |location| {
            try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
        }

        // Print variable name before :: if available, then always show type

        // If we have union member info, print union with '>' on active member; else fallback to concrete type
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
                    // If a union was detected but only one member present, still print as that type
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

        // Format the value
        try PrintOps.formatHIRValue(vm, value.value);
        try printToStdout("\n", .{});

        // Push the value back onto the stack for potential further use
        try vm.stack.push(value);
    }

    // Execute PeekStruct instruction
    pub fn execPeekStruct(vm: anytype, i: anytype) !void {
        const value = try vm.stack.peek();

        // Handle both struct instances and field values
        switch (value.value) {
            .struct_instance => |s| {
                // Print each field with proper location formatting
                for (s.fields) |field| {
                    // Format with location information like the regular Peek instruction
                    if (i.location) |location| {
                        try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                    }

                    // Build the full field path
                    var field_path: []const u8 = undefined;
                    if (s.path) |path| {
                        field_path = try std.fmt.allocPrint(vm.allocator, "{s}.{s}", .{ path, field.name });
                    } else {
                        field_path = try std.fmt.allocPrint(vm.allocator, "{s}.{s}", .{ s.type_name, field.name });
                    }
                    defer vm.allocator.free(field_path);

                    const field_type_string = PrintOps.getTypeString(vm, field.value);
                    try printToStdout(":: {s} is ", .{field_type_string});

                    // For nested structs, recursively print their fields
                    switch (field.value) {
                        .struct_instance => |nested| {
                            // Create a new PeekStruct instruction for the nested struct
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
                                    .should_pop_after_peek = i.should_pop_after_peek, // Pass the flag down
                                },
                            };
                            // Push the nested struct onto the stack with updated path
                            var nested_with_path = nested;
                            nested_with_path.path = field_path;
                            try vm.stack.push(HIRFrame{ .value = HIRValue{ .struct_instance = nested_with_path } });
                            // Recursively peek the nested struct
                            const vm_type = @TypeOf(vm);
                            const info = @typeInfo(vm_type);
                            switch (info) {
                                .pointer => |ptr| {
                                    const child = ptr.child;
                                    if (@hasField(child, "program")) {
                                        // HIRVM - can execute HIR instructions
                                        try vm.executeInstruction(nested_peek);
                                    } else if (@hasField(child, "bytecode")) {
                                        // BytecodeVM - cannot execute HIR instructions directly
                                        // Skip recursive execution for BytecodeVM
                                        try PrintOps.formatHIRValue(vm, field.value);
                                        try printToStdout("\n", .{});
                                        return;
                                    }
                                },
                                else => {
                                    // Fallback - try HIRVM method only
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
                // For non-struct values (like field access results), print as a single value with location
                if (i.field_names.len > 0) {
                    if (i.location) |location| {
                        try printToStdout("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                    }

                    // Show type information for non-struct values
                    const value_type_string = PrintOps.getTypeString(vm, value.value);
                    try printToStdout(":: {s} is ", .{value_type_string});
                    try PrintOps.formatHIRValue(vm, value.value);
                    try printToStdout("\n", .{});
                }
            },
        }

        if (!i.should_pop_after_peek) {
            // Push the value back onto the stack for potential further use
            try vm.stack.push(value);
        }
    }

    // Execute PrintInterpolated instruction
    pub fn execPrintInterpolated(vm: anytype, interp: anytype) !void {
        // Pop the arguments from the stack (they were pushed in reverse order)
        var args = try vm.allocator.alloc(HIRValue, interp.argument_count);
        defer vm.allocator.free(args);

        for (0..interp.argument_count) |i| {
            const arg = try vm.stack.pop();
            args[interp.argument_count - 1 - i] = arg.value; // Reverse to get correct order
        }

        // Get the actual format parts from the constants using format_part_ids
        var actual_format_parts = std.array_list.Managed([]const u8).init(vm.allocator);
        defer actual_format_parts.deinit();

        for (interp.format_part_ids) |id| {
            if (getVmConstant(vm, id)) |constant| {
                if (constant == .string) {
                    try actual_format_parts.append(constant.string);
                    continue;
                }
            }
            try actual_format_parts.append(""); // Fallback
        }

        // Stream parts and arguments directly to stdout in correct order
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

    // Print a HIR value for debugging
    pub fn printHIRValue(vm: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try printToStdout("{}", .{i}),
            .float => |f| try printToStdout("{d}", .{f}),
            .string => |s| try printToStdout("\"{s}\"", .{s}),
            .tetra => |b| try printToStdout("{}", .{b}),
            .byte => |u| try printToStdout("{}", .{u}),
            .nothing => try printToStdout("nothing", .{}),
            // Complex types - show contents for arrays
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
                try printToStdout("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try printToStdout("{s}: ", .{field.name});
                    try PrintOps.printHIRValue(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{}); // Fix closing bracket
            },
            .map => try printToStdout("{{map}}", .{}),
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                // Dereference the storage_id_ref to get the actual value
                if (vm.memory_manager.scope_manager.value_storage.get(storage_id)) |storage| {
                    try PrintOps.formatTokenLiteral(vm, storage.value);
                } else {
                    try printToStdout("storage_id_ref({})", .{storage_id});
                }
            },
        }
    }

    // Internal, reused buffer for building nested array type strings.
    // This is intentionally a single shared buffer used synchronously by printing.
    var type_buf: [128]u8 = undefined;

    fn mapBaseTypeNameFromHIRType(t: @import("../../codegen/hir/soxa_types.zig").HIRType) []const u8 {
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
        // Determine base type and depth by inspecting nested array values.
        var depth: usize = 0;
        var cursor: HIRValue = value;

        while (true) {
            switch (cursor) {
                .array => |arr| {
                    depth += 1;
                    // Check if this is a nested array by looking at the first element
                    if (firstNonNothingElement(arr.elements)) |first_elem| {
                        switch (first_elem) {
                            .array => {
                                // This is a nested array, continue to next level
                                cursor = first_elem;
                                continue;
                            },
                            else => {
                                // This is the leaf level, use the element type
                                const base = mapBaseTypeNameFromValue(first_elem);
                                return writeTypeWithBrackets(base, depth);
                            },
                        }
                    } else {
                        // Empty array - try to use element_type if available
                        if (arr.element_type != .Unknown) {
                            const base = mapBaseTypeNameFromHIRType(arr.element_type);
                            return writeTypeWithBrackets(base, depth);
                        } else {
                            // Unknown nested array
                            return writeTypeWithBrackets("unknown", depth);
                        }
                    }
                },
                else => {
                    // Non-array encountered; use its base type name with current depth.
                    const base2 = mapBaseTypeNameFromValue(cursor);
                    return writeTypeWithBrackets(base2, depth);
                },
            }
        }
    }

    fn writeTypeWithBrackets(base: []const u8, depth: usize) []const u8 {
        var i: usize = 0;
        // Copy base
        while (i < base.len and i < type_buf.len) : (i += 1) type_buf[i] = base[i];
        var written: usize = i;
        // Append [] depth times
        var d: usize = 0;
        while (d < depth and written + 2 <= type_buf.len) : (d += 1) {
            type_buf[written] = '[';
            type_buf[written + 1] = ']';
            written += 2;
        }
        return type_buf[0..written];
    }

    // Get a readable type string from HIRValue for debug output
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
                    return "struct"; // fallback for empty type name
                } else {
                    return s.type_name;
                }
            },
            .map => "map",
            .enum_variant => |e| e.type_name,
            .storage_id_ref => "storage_id_ref",
        };
    }

    // Format HIR value to a writer (for proper UTF-8 buffered output)
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
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.formatHIRValue(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_instance => |s| {
                try printToStdout("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try printToStdout("{s}: ", .{field.name});
                    try PrintOps.formatHIRValue(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                // Dereference the storage_id_ref to get the actual value
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
            .string => |s| try printToStdout("{s}", .{s}), // No quotes around strings, direct write
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
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try printToStdout(", ", .{});
                    try PrintOps.formatHIRValueRaw(vm, elem);
                    first = false;
                }
                try printToStdout("]", .{});
            },
            .struct_instance => |s| {
                try printToStdout("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try printToStdout(" {s}: ", .{field.name});
                    try PrintOps.formatHIRValueRaw(vm, field.value);
                    if (i < s.fields.len - 1) try printToStdout(", ", .{});
                }
                try printToStdout(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try printToStdout(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| {
                // Dereference the storage_id_ref to get the actual value
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

    /// Print struct operation - similar to PeekStruct but prints to stdout
    pub fn execPrintStruct(vm: anytype, ps: anytype) !void {
        const value = try vm.stack.pop();

        // Print the struct to stdout
        try PrintOps.printHIRValue(vm, value.value);

        // If should_pop_after_peek is false, push the value back
        if (!ps.should_pop_after_peek) {
            try vm.stack.push(HIRFrame.initFromHIRValue(value.value));
        }
    }
};
