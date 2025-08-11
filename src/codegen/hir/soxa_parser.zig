const std = @import("std");
const Reporting = @import("../../utils/reporting.zig");

const SoxaValues = @import("soxa_values.zig");
const HIRValue = SoxaValues.HIRValue;
const HIREnum = SoxaValues.HIREnum;
const HIRMapEntry = SoxaValues.HIRMapEntry;

const SoxaInstructions = @import("soxa_instructions.zig");
const HIRInstruction = SoxaInstructions.HIRInstruction;
const ArithOp = SoxaInstructions.ArithOp;
const ResizeBehavior = SoxaInstructions.ResizeBehavior;
const CompareOp = SoxaInstructions.CompareOp;
const LogicalOpType = SoxaInstructions.LogicalOpType;
const StringOpType = SoxaInstructions.StringOpType;

const SoxaTypes = @import("soxa_types.zig");
const CallKind = SoxaTypes.CallKind;
const HIRProgram = SoxaTypes.HIRProgram;
const HIRType = SoxaTypes.HIRType;

// =================================================================================

pub const SoxaTextParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize = 0,
    line: u32 = 1,
    constants: std.ArrayList(HIRValue),
    functions: std.ArrayList(HIRProgram.HIRFunction),
    instructions: std.ArrayList(HIRInstruction),
    struct_context: ?StructContext = null,

    const StructContext = struct {
        type_name: []const u8, // Changed from struct_name to type_name to match usage
        field_count: u32,
        field_names: std.ArrayList([]const u8),
        field_types: std.ArrayList(HIRType),
        field_path: std.ArrayList([]const u8), // Added to track nested field access
    };

    pub fn init(allocator: std.mem.Allocator, source: []const u8) SoxaTextParser {
        return SoxaTextParser{
            .allocator = allocator,
            .source = source,
            .constants = std.ArrayList(HIRValue).init(allocator),
            .functions = std.ArrayList(HIRProgram.HIRFunction).init(allocator),
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
            .struct_context = null,
        };
    }

    pub fn deinit(self: *SoxaTextParser) void {
        if (self.struct_context) |*context| {
            context.field_names.deinit();
            context.field_types.deinit();
            context.field_path.deinit();
        }
    }

    pub fn parse(self: *SoxaTextParser) !HIRProgram {
        var lines = std.mem.splitScalar(u8, self.source, '\n');
        var line_count: u32 = 0;

        while (lines.next()) |line| {
            line_count += 1;
            self.line += 1;
            const trimmed_right = std.mem.trimRight(u8, line, " \t\r\n"); // Only trim right side

            // Skip empty lines and comments
            if (trimmed_right.len == 0 or trimmed_right[0] == ';') continue;

            // Handle sections and instructions - check for indented content
            if (std.mem.startsWith(u8, trimmed_right, ".constants")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, ".functions")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, ".code")) {
                continue; // Section header
            } else if (std.mem.startsWith(u8, trimmed_right, "    const_")) {
                try self.parseConstant(trimmed_right);
            } else if (std.mem.startsWith(u8, trimmed_right, "        entry:")) {
                // Function metadata - update the last function's entry point
                try self.updateFunctionEntry(trimmed_right);
                continue;
            } else if (std.mem.indexOf(u8, trimmed_right, ":")) |colon_pos| {
                // Check if this is an instruction with location info (like "Peek ... @file:line:column")
                const trimmed_line = std.mem.trim(u8, trimmed_right, " \t");
                const is_instruction_with_location = std.mem.startsWith(u8, trimmed_line, "Peek ") or
                    std.mem.startsWith(u8, trimmed_line, "Call ") or
                    std.mem.startsWith(u8, trimmed_line, "LoadVar ") or
                    std.mem.startsWith(u8, trimmed_line, "StoreVar ") or
                    std.mem.startsWith(u8, trimmed_line, "AssertFail ");

                if (is_instruction_with_location) {
                    // This is an instruction, not a label
                    if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                        try self.parseFunction(trimmed_right);
                    } else {
                        try self.parseInstruction(trimmed_right);
                    }
                } else {
                    // Label (can be either indented or not) - CHECK BEFORE INSTRUCTIONS!
                    const is_label = blk: {
                        // Check if this is a label by seeing if colon comes before semicolon (or is at end)
                        if (std.mem.indexOf(u8, trimmed_right, ";")) |semicolon_pos| {
                            break :blk colon_pos < semicolon_pos;
                        } else {
                            break :blk std.mem.endsWith(u8, trimmed_right, ":");
                        }
                    };

                    if (is_label) {
                        const label_line = std.mem.trim(u8, trimmed_right, " \t");
                        const label_colon_pos = std.mem.indexOf(u8, label_line, ":").?;
                        const label_name = try self.allocator.dupe(u8, label_line[0..label_colon_pos]);
                        try self.instructions.append(HIRInstruction{ .Label = .{ .name = label_name, .vm_address = 0 } });
                    } else {
                        // Not a label, fall through to instruction parsing
                        if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                            try self.parseFunction(trimmed_right);
                        } else {
                            try self.parseInstruction(trimmed_right);
                        }
                    }
                }
            } else if (std.mem.startsWith(u8, trimmed_right, "    ") and !std.mem.startsWith(u8, trimmed_right, "        ")) {
                // Function or instruction
                if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                    try self.parseFunction(trimmed_right);
                } else {
                    try self.parseInstruction(trimmed_right);
                }
            }
            // Silently ignore other lines (like function metadata)
        }

        return HIRProgram{
            .instructions = try self.instructions.toOwnedSlice(),
            .constant_pool = try self.constants.toOwnedSlice(),
            .string_pool = &[_][]const u8{}, // Empty for now
            .function_table = try self.functions.toOwnedSlice(),
            .module_map = std.StringHashMap(HIRProgram.ModuleInfo).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    fn parseConstant(self: *SoxaTextParser, line: []const u8) !void {
        // Expect: "    const_N: TYPE [payload]"
        const colon_pos = std.mem.indexOfScalar(u8, line, ':') orelse return error.InvalidCharacter;
        var rest = std.mem.trimLeft(u8, line[colon_pos + 1 ..], " \t");

        // Split TYPE and payload
        const space_idx = std.mem.indexOfScalar(u8, rest, ' ');
        const type_tok = if (space_idx) |idx| rest[0..idx] else rest;
        const payload = if (space_idx) |idx| std.mem.trimLeft(u8, rest[idx + 1 ..], " \t") else "";

        if (std.mem.eql(u8, type_tok, "int")) {
            const value = try std.fmt.parseInt(i32, payload, 10);
            try self.constants.append(HIRValue{ .int = value });
        } else if (std.mem.eql(u8, type_tok, "float")) {
            const value = try std.fmt.parseFloat(f64, payload);
            try self.constants.append(HIRValue{ .float = value });
        } else if (std.mem.eql(u8, type_tok, "string")) {
            const value = try self.parseQuotedString(payload);
            try self.constants.append(HIRValue{ .string = value });
        } else if (std.mem.eql(u8, type_tok, "tetra")) {
            const value = try std.fmt.parseInt(u8, payload, 10);
            try self.constants.append(HIRValue{ .tetra = value });
        } else if (std.mem.eql(u8, type_tok, "byte")) {
            const value = try std.fmt.parseInt(u8, payload, 10);
            try self.constants.append(HIRValue{ .byte = value });
        } else if (std.mem.eql(u8, type_tok, "enum_variant")) {
            const variant_str = std.mem.trim(u8, payload, " \t");
            if (std.mem.indexOfScalar(u8, variant_str, '.')) |dot_pos| {
                const type_name = try self.allocator.dupe(u8, variant_str[0..dot_pos]);
                const variant_name = try self.allocator.dupe(u8, variant_str[dot_pos + 1 ..]);
                try self.constants.append(HIRValue{
                    .enum_variant = HIREnum{
                        .type_name = type_name,
                        .variant_name = variant_name,
                        .variant_index = 0,
                        .path = null,
                    },
                });
            } else {
                try self.constants.append(HIRValue.nothing);
            }
        } else if (std.mem.eql(u8, type_tok, "nothing")) {
            try self.constants.append(HIRValue.nothing);
        } else {
            // Fallback for unhandled/unknown types
            try self.constants.append(HIRValue.nothing);
        }
    }

    fn parseFunction(self: *SoxaTextParser, line: []const u8) !void {
        // Parse: "    fibonacci(1 args) -> int"
        const trimmed = std.mem.trim(u8, line, " \t");
        if (std.mem.indexOf(u8, trimmed, "(")) |paren_pos| {
            const name = try self.allocator.dupe(u8, trimmed[0..paren_pos]);

            // Extract arity from "(N args)"
            var arity: u32 = 0;
            if (std.mem.indexOf(u8, trimmed, "(") != null and std.mem.indexOf(u8, trimmed, " args)") != null) {
                const args_start = std.mem.indexOf(u8, trimmed, "(").? + 1;
                const args_end = std.mem.indexOf(u8, trimmed, " args)").?;
                if (args_end > args_start) {
                    const arity_str = trimmed[args_start..args_end];
                    arity = std.fmt.parseInt(u32, arity_str, 10) catch 0;
                }
            }

            // Create function with placeholder start_label - will be updated when we see the entry line
            try self.functions.append(HIRProgram.HIRFunction{
                .name = name,
                .qualified_name = name,
                .arity = arity,
                .return_type = .Auto,
                .start_label = try self.allocator.dupe(u8, "unknown"), // Will be updated
                .local_var_count = 0,
                .is_entry = false,
            });
        }
    }

    fn updateFunctionEntry(self: *SoxaTextParser, entry_line: []const u8) !void {
        // Parse: "        entry: func_fibonacci_0"
        if (std.mem.indexOf(u8, entry_line, "entry:")) |entry_pos| {
            const label_start = entry_pos + 6; // Skip "entry:"
            const label = std.mem.trim(u8, entry_line[label_start..], " \t");

            // Update the last function we parsed
            if (self.functions.items.len > 0) {
                const last_func_idx = self.functions.items.len - 1;
                self.functions.items[last_func_idx].start_label = try self.allocator.dupe(u8, label);
            }
        }
    }

    fn parseInstruction(self: *SoxaTextParser, line: []const u8) !void {
        const trimmed = std.mem.trim(u8, line, " \t");

        // Handle struct peekion instructions
        if (std.mem.startsWith(u8, trimmed, "PeekStruct")) {
            // Parse: PeekStruct "Person" 2 ["name", "age"] [String, Int]
            const struct_name_start = std.mem.indexOf(u8, trimmed, "\"").? + 1;
            const struct_name_end = std.mem.indexOfPos(u8, trimmed, struct_name_start, "\"").?;
            const struct_name = try self.allocator.dupe(u8, trimmed[struct_name_start..struct_name_end]);

            // Get field count
            const count_start = struct_name_end + 2;
            const count_end = std.mem.indexOfAny(u8, trimmed[count_start..], " [").? + count_start;
            const field_count = try std.fmt.parseInt(u32, trimmed[count_start..count_end], 10);

            // Initialize new struct context
            var field_names = std.ArrayList([]const u8).init(self.allocator);
            var field_types = std.ArrayList(HIRType).init(self.allocator);
            const field_path = std.ArrayList([]const u8).init(self.allocator);

            // Parse field names and types
            var in_names = true;
            var current_pos = count_end;
            while (current_pos < trimmed.len) : (current_pos += 1) {
                if (trimmed[current_pos] == '[') {
                    continue;
                } else if (trimmed[current_pos] == ']') {
                    if (in_names) {
                        in_names = false;
                    } else {
                        break;
                    }
                } else if (trimmed[current_pos] == '"') {
                    const name_start = current_pos + 1;
                    const name_end = std.mem.indexOfPos(u8, trimmed, name_start, "\"").?;
                    const name = try self.allocator.dupe(u8, trimmed[name_start..name_end]);
                    try field_names.append(name);
                    current_pos = name_end;
                } else if (std.mem.indexOfPos(u8, trimmed, current_pos, "String")) |type_pos| {
                    if (type_pos == current_pos) {
                        try field_types.append(.String);
                        current_pos = type_pos + 5;
                    }
                } else if (std.mem.indexOfPos(u8, trimmed, current_pos, "Int")) |type_pos| {
                    if (type_pos == current_pos) {
                        try field_types.append(.Int);
                        current_pos = type_pos + 2;
                    }
                }
            }

            // Set up the new struct context
            self.struct_context = .{
                .type_name = struct_name,
                .field_count = field_count,
                .field_names = field_names,
                .field_types = field_types,
                .field_path = field_path,
            };

            try self.instructions.append(.{ .PeekStruct = .{
                .type_name = struct_name,
                .field_count = field_count,
                .field_names = try field_names.toOwnedSlice(),
                .field_types = try field_types.toOwnedSlice(),
                .location = null,
                .should_pop_after_peek = false,
            } });
            return;
        }
        var tokens = std.mem.splitScalar(u8, trimmed, ' ');
        const op = tokens.next() orelse return;

        if (std.mem.eql(u8, op, "Const")) {
            const id_str = tokens.next() orelse return;
            const id = std.fmt.parseInt(u32, id_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .Const = .{ .value = HIRValue.nothing, .constant_id = id } });
        } else if (std.mem.eql(u8, op, "LoadVar")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = .Local, .module_context = null } });
        } else if (std.mem.eql(u8, op, "StoreVar")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = .Local, .module_context = null, .expected_type = .Auto } });
        } else if (std.mem.eql(u8, op, "StoreConst")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .StoreConst = .{ .var_index = var_index, .var_name = var_name } });
        } else if (std.mem.eql(u8, op, "IntArith")) {
            const op_str = tokens.next() orelse return;
            const arith_op = if (std.mem.eql(u8, op_str, "Add")) ArithOp.Add else if (std.mem.eql(u8, op_str, "Sub")) ArithOp.Sub else if (std.mem.eql(u8, op_str, "Mul")) ArithOp.Mul else if (std.mem.eql(u8, op_str, "Div")) ArithOp.Div else if (std.mem.eql(u8, op_str, "Mod")) ArithOp.Mod else ArithOp.Add;
            try self.instructions.append(HIRInstruction{ .IntArith = .{ .op = arith_op, .overflow_behavior = .Wrap } });
        } else if (std.mem.eql(u8, op, "Compare")) {
            const op_str = tokens.next() orelse return;
            const comp_op = if (std.mem.eql(u8, op_str, "Eq")) CompareOp.Eq else if (std.mem.eql(u8, op_str, "Ne")) CompareOp.Ne else if (std.mem.eql(u8, op_str, "Lt")) CompareOp.Lt else if (std.mem.eql(u8, op_str, "Le")) CompareOp.Le else if (std.mem.eql(u8, op_str, "Gt")) CompareOp.Gt else if (std.mem.eql(u8, op_str, "Ge")) CompareOp.Ge else CompareOp.Eq;

            // Optional operand type (default Int if omitted)
            const maybe_type = tokens.next();
            var operand_type: HIRType = .Int;
            if (maybe_type) |type_str| {
                operand_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Byte")) HIRType.Byte else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Nothing")) HIRType.Nothing else if (std.mem.eql(u8, type_str, "Array")) HIRType.Array else if (std.mem.eql(u8, type_str, "Struct")) HIRType.Struct else if (std.mem.eql(u8, type_str, "Map")) HIRType.Map else if (std.mem.eql(u8, type_str, "Enum")) HIRType.Enum else if (std.mem.eql(u8, type_str, "Function")) HIRType.Function else if (std.mem.eql(u8, type_str, "Auto")) HIRType.Auto else HIRType.Int;
            }

            try self.instructions.append(HIRInstruction{ .Compare = .{ .op = comp_op, .operand_type = operand_type } });
        } else if (std.mem.eql(u8, op, "Jump")) {
            const label = tokens.next() orelse return;
            const label_name = try self.allocator.dupe(u8, label);
            try self.instructions.append(HIRInstruction{ .Jump = .{ .label = label_name, .vm_offset = 0 } });
        } else if (std.mem.eql(u8, op, "JumpCond")) {
            const true_label = tokens.next() orelse return;
            const false_label = tokens.next() orelse return;
            const true_name = try self.allocator.dupe(u8, true_label);
            const false_name = try self.allocator.dupe(u8, false_label);
            try self.instructions.append(HIRInstruction{ .JumpCond = .{ .label_true = true_name, .label_false = false_name, .vm_offset = 0, .condition_type = .Tetra } });
        } else if (std.mem.eql(u8, op, "Call")) {
            const func_idx_str = tokens.next() orelse return;
            const arg_count_str = tokens.next() orelse return;
            const name_quoted = tokens.next() orelse return;
            const kind_str = tokens.next() orelse return;

            const function_index = std.fmt.parseInt(u32, func_idx_str, 10) catch return;
            const arg_count = std.fmt.parseInt(u32, arg_count_str, 10) catch return;
            const qualified_name = try self.parseQuotedString(name_quoted);
            const call_kind = if (std.mem.eql(u8, kind_str, "LocalFunction")) CallKind.LocalFunction else if (std.mem.eql(u8, kind_str, "ModuleFunction")) CallKind.ModuleFunction else if (std.mem.eql(u8, kind_str, "BuiltinFunction")) CallKind.BuiltinFunction else CallKind.LocalFunction;

            try self.instructions.append(HIRInstruction{
                .Call = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = null,
                    .return_type = .String, // Default to String to prevent Auto leakage
                },
            });
        } else if (std.mem.eql(u8, op, "TailCall")) {
            const func_idx_str = tokens.next() orelse return;
            const arg_count_str = tokens.next() orelse return;
            const name_quoted = tokens.next() orelse return;
            const kind_str = tokens.next() orelse return;

            const function_index = std.fmt.parseInt(u32, func_idx_str, 10) catch return;
            const arg_count = std.fmt.parseInt(u32, arg_count_str, 10) catch return;
            const qualified_name = try self.parseQuotedString(name_quoted);
            const call_kind = if (std.mem.eql(u8, kind_str, "LocalFunction")) CallKind.LocalFunction else if (std.mem.eql(u8, kind_str, "ModuleFunction")) CallKind.ModuleFunction else if (std.mem.eql(u8, kind_str, "BuiltinFunction")) CallKind.BuiltinFunction else CallKind.LocalFunction;

            try self.instructions.append(HIRInstruction{
                .TailCall = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = null,
                    .return_type = .String, // Default to String to prevent Auto leakage
                },
            });
        } else if (std.mem.eql(u8, op, "Return")) {
            const has_val_str = tokens.next() orelse return;
            const has_value = std.mem.eql(u8, has_val_str, "true");
            try self.instructions.append(HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Nothing } }); // Default to Nothing to prevent Auto leakage
        } else if (std.mem.eql(u8, op, "Dup")) {
            try self.instructions.append(HIRInstruction.Dup);
        } else if (std.mem.eql(u8, op, "Pop")) {
            try self.instructions.append(HIRInstruction.Pop);
        } else if (std.mem.eql(u8, op, "Swap")) {
            try self.instructions.append(HIRInstruction.Swap);
        } else if (std.mem.eql(u8, op, "Halt")) {
            try self.instructions.append(HIRInstruction.Halt);
        } else if (std.mem.eql(u8, op, "Peek")) {
            const name_or_type = tokens.next() orelse return;
            var name: ?[]const u8 = null;
            var value_type: HIRType = .String;
            var location: ?Reporting.Reporter.Location = null;

            // For struct peekion
            var struct_name: ?[]const u8 = null;
            var field_names = std.ArrayList([]const u8).init(self.allocator);
            var field_types = std.ArrayList(HIRType).init(self.allocator);
            defer field_names.deinit();
            defer field_types.deinit();

            // Track the full path for struct fields
            var path_builder = std.ArrayList(u8).init(self.allocator);
            defer path_builder.deinit();

            // Build path from struct context if available
            if (self.struct_context) |context| {
                try path_builder.appendSlice(context.type_name);
                for (context.field_path.items) |field| {
                    try path_builder.appendSlice(".");
                    try path_builder.appendSlice(field);
                }
            }

            if (name_or_type.len > 2 and name_or_type[0] == '"' and name_or_type[name_or_type.len - 1] == '"') {
                // Has quoted name, get type from next token
                name = try self.parseQuotedString(name_or_type);
                const type_str = tokens.next() orelse "String";
                value_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Array")) HIRType.Array else if (std.mem.eql(u8, type_str, "Map")) HIRType.Map else if (std.mem.eql(u8, type_str, "Struct")) blk: {
                    // For structs, we need to collect field information
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else if (name) |n| n else "anonymous";
                    break :blk HIRType.Struct;
                } else if (std.mem.eql(u8, type_str, "Enum")) HIRType.Enum else HIRType.String;

                // Check for location info
                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            } else {
                // Similar logic for non-quoted case...
                value_type = if (std.mem.eql(u8, name_or_type, "Int")) HIRType.Int else if (std.mem.eql(u8, name_or_type, "Float")) HIRType.Float else if (std.mem.eql(u8, name_or_type, "String")) HIRType.String else if (std.mem.eql(u8, name_or_type, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, name_or_type, "Array")) HIRType.Map else if (std.mem.eql(u8, name_or_type, "Struct")) blk: {
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else
                        "anonymous";
                    break :blk HIRType.Struct;
                } else if (std.mem.eql(u8, name_or_type, "Enum")) HIRType.Enum else HIRType.String;

                // Check for location info after type
                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            }

            // If this is a struct peekion, gather field information from the stack
            if (value_type == .Struct) {
                // FIXED: Check if struct_context is available before trying to get struct info
                // When parsing pre-generated HIR files, struct_context may be null
                if (self.struct_context) |_| {
                    // Try to get struct info from the current context
                    const struct_info = try self.getCurrentStructInfo();
                    for (struct_info.fields) |field| {
                        try field_names.append(field.name);
                        try field_types.append(field.type);
                    }
                    // Free the allocated fields array
                    self.allocator.free(struct_info.fields);

                    try self.instructions.append(.{
                        .PeekStruct = .{
                            .type_name = struct_name.?,
                            .field_count = @intCast(field_names.items.len),
                            .field_names = try field_names.toOwnedSlice(),
                            .field_types = try field_types.toOwnedSlice(),
                            .location = location,
                            // parser should keep the peeked value on the stack
                            .should_pop_after_peek = false,
                        },
                    });
                } else {
                    // No struct context available - create a regular Peek instruction
                    // This happens when parsing pre-generated HIR files
                    try self.instructions.append(.{ .Peek = .{
                        .name = if (path_builder.items.len > 0)
                            try self.allocator.dupe(u8, path_builder.items)
                        else
                            name,
                        .value_type = value_type,
                        .location = location,
                    } });
                }
            } else {
                try self.instructions.append(.{ .Peek = .{
                    .name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else
                        name,
                    .value_type = value_type,
                    .location = location,
                } });
            }
        } else if (std.mem.eql(u8, op, "ArrayNew")) {
            const type_str = tokens.next() orelse return;
            const size_str = tokens.next() orelse return;
            const element_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Byte")) HIRType.Byte else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else HIRType.Auto;
            const size = std.fmt.parseInt(u32, size_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .ArrayNew = .{ .element_type = element_type, .size = size } });
        } else if (std.mem.eql(u8, op, "ArrayGet")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGet = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArraySet")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArraySet = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayPush")) {
            const behavior_str = tokens.next() orelse return;
            const resize_behavior = if (std.mem.eql(u8, behavior_str, "Double")) ResizeBehavior.Double else if (std.mem.eql(u8, behavior_str, "Fixed")) ResizeBehavior.Fixed else if (std.mem.eql(u8, behavior_str, "Exact")) ResizeBehavior.Exact else ResizeBehavior.Double;
            try self.instructions.append(HIRInstruction{ .ArrayPush = .{ .resize_behavior = resize_behavior } });
        } else if (std.mem.eql(u8, op, "ArrayPop")) {
            try self.instructions.append(HIRInstruction.ArrayPop);
        } else if (std.mem.eql(u8, op, "ArrayLen")) {
            try self.instructions.append(HIRInstruction.ArrayLen);
        } else if (std.mem.eql(u8, op, "ArrayConcat")) {
            try self.instructions.append(HIRInstruction.ArrayConcat);
        } else if (std.mem.eql(u8, op, "Map")) {
            const count_str = tokens.next() orelse return;
            const key_type_str = tokens.next() orelse return;
            const entry_count = std.fmt.parseInt(u32, count_str, 10) catch return;
            const key_type = if (std.mem.eql(u8, key_type_str, "String")) HIRType.String else if (std.mem.eql(u8, key_type_str, "Int")) HIRType.Int else HIRType.String;

            // Create dummy entries array with correct size
            const dummy_entries = try self.allocator.alloc(HIRMapEntry, entry_count);
            for (dummy_entries) |*entry| {
                entry.* = HIRMapEntry{
                    .key = HIRValue.nothing,
                    .value = HIRValue.nothing,
                };
            }

            try self.instructions.append(HIRInstruction{ .Map = .{
                .entries = dummy_entries,
                .key_type = key_type,
                .value_type = .Auto,
            } });
        } else if (std.mem.eql(u8, op, "MapGet")) {
            const key_type_str = tokens.next() orelse return;
            const key_type = if (std.mem.eql(u8, key_type_str, "String")) HIRType.String else if (std.mem.eql(u8, key_type_str, "Int")) HIRType.Int else HIRType.String;

            try self.instructions.append(HIRInstruction{ .MapGet = .{
                .key_type = key_type,
            } });
        } else if (std.mem.eql(u8, op, "EnterScope")) {
            const scope_id_str = tokens.next() orelse return;
            const var_count_str = tokens.next() orelse return;
            const scope_id = std.fmt.parseInt(u32, scope_id_str, 10) catch return;
            const var_count = std.fmt.parseInt(u32, var_count_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .EnterScope = .{ .scope_id = scope_id, .var_count = var_count } });
        } else if (std.mem.eql(u8, op, "ExitScope")) {
            const scope_id_str = tokens.next() orelse return;
            const scope_id = std.fmt.parseInt(u32, scope_id_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .ExitScope = .{ .scope_id = scope_id } });
        } else if (std.mem.eql(u8, op, "StructNew")) {
            const type_name_quoted = tokens.next() orelse return;
            const field_count_str = tokens.next() orelse return;

            const type_name = try self.parseQuotedString(type_name_quoted);
            const field_count = std.fmt.parseInt(u32, field_count_str, 10) catch return;

            // Create dummy field types - these will be populated by the VM at runtime
            // based on the actual values on the stack
            const field_types = try self.allocator.alloc(HIRType, field_count);
            for (field_types) |*field_type| {
                field_type.* = HIRType.Auto; // Default to Auto, will be resolved at runtime
            }

            try self.instructions.append(HIRInstruction{
                .StructNew = .{
                    .type_name = type_name,
                    .field_count = field_count,
                    .field_types = field_types,
                    .size_bytes = 0, // Size will be calculated at runtime
                },
            });
        } else if (std.mem.eql(u8, op, "GetField")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .GetField = .{
                    .field_name = field_name,
                    .container_type = HIRType.Struct,
                    .field_index = 0, // Will be resolved at runtime
                    .field_for_peek = false,
                },
            });
        } else if (std.mem.eql(u8, op, "SetField")) {
            const field_name_quoted = tokens.next() orelse return;
            // Parse format: SetField "age" Struct 0
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .SetField = .{
                    .field_name = field_name,
                    .container_type = HIRType.Struct,
                    .field_index = 0, // Will be resolved at runtime
                },
            });
        } else if (std.mem.eql(u8, op, "StoreFieldName")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{ .StoreFieldName = .{
                .field_name = field_name,
            } });
        } else if (std.mem.eql(u8, op, "StringOp")) {
            const op_str = tokens.next() orelse return;
            const string_op = if (std.mem.eql(u8, op_str, "Length"))
                StringOpType.Length
            else if (std.mem.eql(u8, op_str, "Bytes"))
                StringOpType.Bytes
            else if (std.mem.eql(u8, op_str, "Substring"))
                StringOpType.Substring
            else if (std.mem.eql(u8, op_str, "Concat"))
                StringOpType.Concat
            else
                StringOpType.Length; // Default fallback

            try self.instructions.append(HIRInstruction{ .StringOp = .{
                .op = string_op,
            } });
        } else if (std.mem.eql(u8, op, "LogicalOp")) {
            const op_str = tokens.next() orelse return;
            const logical_op = if (std.mem.eql(u8, op_str, "And"))
                LogicalOpType.And
            else if (std.mem.eql(u8, op_str, "Or"))
                LogicalOpType.Or
            else if (std.mem.eql(u8, op_str, "Not"))
                LogicalOpType.Not
            else if (std.mem.eql(u8, op_str, "Iff"))
                LogicalOpType.Iff
            else if (std.mem.eql(u8, op_str, "Xor"))
                LogicalOpType.Xor
            else if (std.mem.eql(u8, op_str, "Nand"))
                LogicalOpType.Nand
            else if (std.mem.eql(u8, op_str, "Nor"))
                LogicalOpType.Nor
            else if (std.mem.eql(u8, op_str, "Implies"))
                LogicalOpType.Implies
            else
                LogicalOpType.And; // Default fallback

            try self.instructions.append(HIRInstruction{ .LogicalOp = .{
                .op = logical_op,
            } });
        } else if (std.mem.eql(u8, op, "AssertFail")) {
            // Parse: AssertFail @file:line:column with message
            // or: AssertFail @file:line:column
            const rest_of_line = tokens.rest();
            var has_message = false;
            var location: ?Reporting.Reporter.Location = null;

            // Look for location string starting with @
            if (std.mem.indexOf(u8, rest_of_line, "@")) |at_pos| {
                const location_part = rest_of_line[at_pos..];

                // Find the end of the location (before " with message" if present)
                var location_end = location_part.len;
                if (std.mem.indexOf(u8, location_part, " with message")) |msg_pos| {
                    location_end = msg_pos;
                    has_message = true;
                }

                // Parse the location
                const location_str = location_part[0..location_end];
                location = self.parseLocationString(location_str) catch null;
            }

            try self.instructions.append(HIRInstruction{ .AssertFail = .{
                .location = location orelse Reporting.Reporter.Location{
                    .file = "unknown",
                    .line = 0,
                    .column = 0,
                },
                .has_message = has_message,
            } });
        }
        // Instructions not implemented yet are silently ignored for now
    }

    fn parseQuotedString(self: *SoxaTextParser, quoted: []const u8) ![]const u8 {
        if (quoted.len >= 2 and quoted[0] == '"' and quoted[quoted.len - 1] == '"') {
            return try self.allocator.dupe(u8, quoted[1 .. quoted.len - 1]);
        }
        return try self.allocator.dupe(u8, quoted);
    }

    const StructInfo = struct {
        fields: []StructField,

        const StructField = struct {
            name: []const u8,
            type: HIRType,
        };
    };

    fn getCurrentStructInfo(self: *SoxaTextParser) !StructInfo {
        if (self.struct_context) |context| {
            // Create array of fields with their types
            var fields = try self.allocator.alloc(StructInfo.StructField, context.field_count);
            for (0..context.field_count) |i| {
                fields[i] = .{
                    .name = context.field_names.items[i],
                    .type = context.field_types.items[i],
                };
            }
            return StructInfo{ .fields = fields };
        }
        return error.NoStructContext;
    }

    fn parseLocationString(self: *SoxaTextParser, location_str: []const u8) !Reporting.Reporter.Location {
        // Parse format: @file:line:column
        // Handle Windows paths by splitting from the right side
        if (!std.mem.startsWith(u8, location_str, "@")) {
            return error.InvalidLocationFormat;
        }

        const location_part = location_str[1..]; // Skip @

        // Find the last two colons to get line and column
        const last_colon = std.mem.lastIndexOfScalar(u8, location_part, ':') orelse return error.InvalidLocationFormat;
        const second_last_colon = std.mem.lastIndexOfScalar(u8, location_part[0..last_colon], ':') orelse return error.InvalidLocationFormat;

        const file_part = location_part[0..second_last_colon];
        const line_part = location_part[second_last_colon + 1 .. last_colon];
        const column_part = location_part[last_colon + 1 ..];

        const file = try self.allocator.dupe(u8, file_part);
        const line = try std.fmt.parseInt(i32, line_part, 10);
        const column = try std.fmt.parseInt(usize, column_part, 10);

        return Reporting.Reporter.Location{
            .file = file,
            .line = line,
            .column = column,
        };
    }
};
