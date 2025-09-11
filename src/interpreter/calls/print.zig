const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
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

pub const PrintOps = struct {
    // Execute Print instruction
    pub fn execPrint(vm: anytype) !void {
        const value = try vm.stack.pop();

        // Print instruction doesn't include variable name, just the value
        // Format the value directly without quotes or extra formatting
        try PrintOps.formatHIRValueRaw(vm, std.io.getStdOut().writer(), value.value);

        // Output will be flushed automatically on newline

        // Push the value back onto the stack for potential further use
        try vm.stack.push(value);
    }

    // Execute Peek instruction
    pub fn execPeek(vm: anytype, peek: anytype) !void {
        const value = try vm.stack.pop();

        if (peek.location) |location| {
            try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
        }

        // Print variable name before :: if available, then always show type
        const writer = std.io.getStdOut().writer();

        // If we have union member info, print union with '>' on active member; else fallback to concrete type
        if (peek.name) |name| {
            if (peek.union_members) |members| {
                if (members.len > 1) {
                    const active = PrintOps.getTypeString(vm, value.value);
                    try writer.print("{s} :: ", .{name});
                    for (members, 0..) |m, i| {
                        if (i > 0) try writer.print(" | ", .{});
                        if (std.mem.eql(u8, m, active)) try writer.print(">", .{});
                        try writer.print("{s}", .{m});
                    }
                    try writer.print(" is ", .{});
                } else {
                    // If a union was detected but only one member present, still print as that type
                    const type_string = PrintOps.getTypeString(vm, value.value);
                    try writer.print("{s} :: {s} is ", .{ name, type_string });
                }
            } else {
                const type_string = PrintOps.getTypeString(vm, value.value);
                try writer.print("{s} :: {s} is ", .{ name, type_string });
            }
        } else {
            if (peek.union_members) |members| {
                if (members.len > 1) {
                    const active = PrintOps.getTypeString(vm, value.value);
                    try writer.print(":: ", .{});
                    for (members, 0..) |m, i| {
                        if (i > 0) try writer.print(" | ", .{});
                        if (std.mem.eql(u8, m, active)) try writer.print(">", .{});
                        try writer.print("{s}", .{m});
                    }
                    try writer.print(" is ", .{});
                } else {
                    const type_string = PrintOps.getTypeString(vm, value.value);
                    try writer.print(":: {s} is ", .{type_string});
                }
            } else {
                const type_string = PrintOps.getTypeString(vm, value.value);
                try writer.print(":: {s} is ", .{type_string});
            }
        }

        // Format the value
        try PrintOps.formatHIRValue(vm, std.io.getStdOut().writer(), value.value);
        try std.io.getStdOut().writer().print("\n", .{});

        // Push the value back onto the stack for potential further use
        try vm.stack.push(value);
    }

    // Execute PeekStruct instruction
    pub fn execPeekStruct(vm: anytype, i: anytype) !void {
        const value = try vm.stack.pop();

        // Handle both struct instances and field values
        switch (value.value) {
            .struct_instance => |s| {
                // Print each field with proper location formatting
                for (s.fields) |field| {
                    // Format with location information like the regular Peek instruction
                    if (i.location) |location| {
                        try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
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
                    try std.io.getStdOut().writer().print(":: {s} is ", .{field_type_string});

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
                            try vm.executeInstruction(nested_peek);
                        },
                        else => {
                            try PrintOps.formatHIRValue(vm, std.io.getStdOut().writer(), field.value);
                            try std.io.getStdOut().writer().print("\n", .{});
                        },
                    }
                }
            },
            else => {
                // For non-struct values (like field access results), print as a single value with location
                if (i.field_names.len > 0) {
                    if (i.location) |location| {
                        try std.io.getStdOut().writer().print("[{s}:{d}:{d}] ", .{ location.file, location.range.start_line, location.range.start_col });
                    }

                    // Show type information for non-struct values
                    const value_type_string = PrintOps.getTypeString(vm, value.value);
                    try std.io.getStdOut().writer().print(":: {s} is ", .{value_type_string});
                    try PrintOps.formatHIRValue(vm, std.io.getStdOut().writer(), value.value);
                    try std.io.getStdOut().writer().print("\n", .{});
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
        var actual_format_parts = std.ArrayList([]const u8).init(vm.allocator);
        defer actual_format_parts.deinit();

        for (interp.format_part_ids) |id| {
            if (id < vm.program.constant_pool.len) {
                const constant = vm.program.constant_pool[id];
                if (constant == .string) {
                    try actual_format_parts.append(constant.string);
                } else {
                    try actual_format_parts.append(""); // Fallback
                }
            } else {
                try actual_format_parts.append(""); // Fallback
            }
        }

        // Build the final string by interleaving format parts and argument values
        var result = std.ArrayList(u8).init(vm.allocator);
        defer result.deinit();

        for (actual_format_parts.items, 0..) |part, i| {
            try result.appendSlice(part);

            // Add argument value if there is one for this part
            if (i < interp.placeholder_indices.len) {
                const arg_index = interp.placeholder_indices[i];
                if (arg_index < args.len) {
                    try PrintOps.formatHIRValueRaw(vm, result.writer(), args[arg_index]);
                }
            }
        }

        // Print the final interpolated string
        try std.io.getStdOut().writer().print("{s}", .{result.items});
    }

    // Print a HIR value for debugging
    pub fn printHIRValue(vm: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| std.debug.print("{}", .{i}),
            .float => |f| std.debug.print("{d}", .{f}),
            .string => |s| std.debug.print("\"{s}\"", .{s}),
            .tetra => |b| std.debug.print("{}", .{b}),
            .byte => |u| std.debug.print("{}", .{u}),
            .nothing => std.debug.print("nothing", .{}),
            // Complex types - show contents for arrays
            .array => |arr| {
                std.debug.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break;
                    if (!first) std.debug.print(", ", .{});
                    try PrintOps.printHIRValue(vm, elem);
                    first = false;
                }
                std.debug.print("]", .{});
            },
            .struct_instance => |s| {
                std.debug.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    std.debug.print("{s}: ", .{field.name});
                    try PrintOps.printHIRValue(vm, field.value);
                    if (i < s.fields.len - 1) std.debug.print(", ", .{});
                }
                std.debug.print(" }}", .{}); // Fix closing bracket
            },
            .map => std.debug.print("{{map}}", .{}),
            .enum_variant => |e| std.debug.print(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| std.debug.print("storage_id_ref({})", .{storage_id}),
        }
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
            .array => |arr| switch (arr.element_type) {
                .Int => "int[]",
                .Byte => "byte[]",
                .Float => "float[]",
                .String => "string[]",
                .Tetra => "tetra[]",
                .Array => "array[]", // Nested arrays
                .Struct => "struct[]",
                .Nothing => "nothing[]",
                .Map => "map[]",
                .Enum => "enum[]",
                .Function => "function[]",
                .Union => "union[]",
                .Unknown => "unknown[]",
            },
            .struct_instance => |s| s.type_name,
            .map => "map",
            .enum_variant => |e| e.type_name,
            .storage_id_ref => "storage_id_ref",
        };
    }

    // Format HIR value to a writer (for proper UTF-8 buffered output)
    pub fn formatHIRValue(vm: anytype, writer: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try writer.print("{}", .{i}),
            .byte => |u| try writer.print("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(writer, f),
            .string => |s| try writer.print("\"{s}\"", .{s}),
            .tetra => |t| try writer.print("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try writer.print("nothing", .{}),
            .array => |arr| {
                try writer.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try writer.print(", ", .{});
                    try PrintOps.formatHIRValue(vm, writer, elem);
                    first = false;
                }
                try writer.print("]", .{});
            },
            .struct_instance => |s| {
                try writer.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try writer.print("{s}: ", .{field.name});
                    try PrintOps.formatHIRValue(vm, writer, field.value);
                    if (i < s.fields.len - 1) try writer.print(", ", .{});
                }
                try writer.print(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try writer.print(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| try writer.print("storage_id_ref({})", .{storage_id}),
            else => try writer.print("{s}", .{@tagName(value)}),
        }
    }

    // Format HIR value to a writer without quotes or extra formatting (for Show instruction)
    pub fn formatHIRValueRaw(vm: anytype, writer: anytype, value: HIRValue) !void {
        switch (value) {
            .int => |i| try writer.print("{}", .{i}),
            .byte => |u| try writer.print("0x{X:0>2}", .{u}),
            .float => |f| try printFloat(writer, f),
            .string => |s| try writer.writeAll(s), // No quotes around strings, direct write
            .tetra => |t| try writer.print("{s}", .{switch (t) {
                0 => "false",
                1 => "true",
                2 => "both",
                3 => "neither",
                else => "invalid",
            }}),
            .nothing => try writer.print("nothing", .{}),
            .array => |arr| {
                try writer.print("[", .{});
                var first = true;
                for (arr.elements) |elem| {
                    if (std.meta.eql(elem, HIRValue.nothing)) break; // Stop at first nothing element
                    if (!first) try writer.print(", ", .{});
                    try PrintOps.formatHIRValueRaw(vm, writer, elem);
                    first = false;
                }
                try writer.print("]", .{});
            },
            .struct_instance => |s| {
                try writer.print("{{ ", .{}); // Add opening bracket
                for (s.fields, 0..) |field, i| {
                    try writer.print("{s}: ", .{field.name});
                    try PrintOps.formatHIRValueRaw(vm, writer, field.value);
                    if (i < s.fields.len - 1) try writer.print(", ", .{});
                }
                try writer.print(" }}", .{}); // Fix closing bracket
            },
            .enum_variant => |e| try writer.print(".{s}", .{e.variant_name}),
            .storage_id_ref => |storage_id| try writer.print("storage_id_ref({})", .{storage_id}),
            else => try writer.print("{s}", .{@tagName(value)}),
        }
    }

    fn printFloat(writer: anytype, f: f64) !void {
        const roundedDown = std.math.floor(f);
        if (f - roundedDown == 0) {
            try writer.print("{d}.0", .{f});
        } else {
            try writer.print("{d}", .{f});
        }
    }
};
