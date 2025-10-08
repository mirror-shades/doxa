const std = @import("std");
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;

const SoxaValues = @import("soxa_values.zig");
const HIRValue = SoxaValues.HIRValue;
const HIREnum = SoxaValues.HIREnum;
const HIRMapEntry = SoxaValues.HIRMapEntry;

const SoxaInstructions = @import("soxa_instructions.zig");
const HIRInstruction = SoxaInstructions.HIRInstruction;
const ScopeKind = @import("soxa_types.zig").ScopeKind;
const ExceptionBehavior = SoxaInstructions.ExceptionBehavior;
const ArithOp = SoxaInstructions.ArithOp;
const ResizeBehavior = SoxaInstructions.ResizeBehavior;
const CompareOp = SoxaInstructions.CompareOp;
const LogicalOpType = SoxaInstructions.LogicalOpType;
const StringOpType = SoxaInstructions.StringOpType;

const SoxaTypes = @import("soxa_types.zig");
const CallKind = SoxaTypes.CallKind;
const HIRProgram = SoxaTypes.HIRProgram;
const HIRType = SoxaTypes.HIRType;

pub const SoxaTextParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize = 0,
    line: u32 = 1,
    constants: std.array_list.Managed(HIRValue),
    functions: std.array_list.Managed(HIRProgram.HIRFunction),
    instructions: std.array_list.Managed(HIRInstruction),
    struct_context: ?StructContext = null,
    label_to_function: std.StringHashMap(usize),
    current_function: ?usize = null,

    const StructContext = struct {
        type_name: []const u8,
        field_count: u32,
        field_names: std.array_list.Managed([]const u8),
        field_types: std.array_list.Managed(HIRType),
        field_path: std.array_list.Managed([]const u8),
    };

    pub fn init(allocator: std.mem.Allocator, source: []const u8) SoxaTextParser {
        return SoxaTextParser{
            .allocator = allocator,
            .source = source,
            .constants = std.array_list.Managed(HIRValue).init(allocator),
            .functions = std.array_list.Managed(HIRProgram.HIRFunction).init(allocator),
            .instructions = std.array_list.Managed(HIRInstruction).init(allocator),
            .struct_context = null,
            .label_to_function = std.StringHashMap(usize).init(allocator),
            .current_function = null,
        };
    }

    pub fn deinit(self: *SoxaTextParser) void {
        if (self.struct_context) |*context| {
            context.field_names.deinit();
            context.field_types.deinit();
            context.field_path.deinit();
        }
        self.label_to_function.deinit();
    }

    pub fn parse(self: *SoxaTextParser) !HIRProgram {
        var lines = std.mem.splitScalar(u8, self.source, '\n');
        var line_count: u32 = 0;

        while (lines.next()) |line| {
            line_count += 1;
            self.line += 1;
            const trimmed_right = std.mem.trimRight(u8, line, " \t\r\n");

            if (trimmed_right.len == 0 or trimmed_right[0] == ';') continue;

            if (std.mem.startsWith(u8, trimmed_right, ".constants")) {
                continue;
            } else if (std.mem.startsWith(u8, trimmed_right, ".functions")) {
                continue;
            } else if (std.mem.startsWith(u8, trimmed_right, ".code")) {
                continue;
            } else if (std.mem.startsWith(u8, trimmed_right, "    const_")) {
                try self.parseConstant(trimmed_right);
            } else if (std.mem.startsWith(u8, trimmed_right, "        entry:")) {
                try self.updateFunctionEntry(trimmed_right);
                continue;
            } else if (std.mem.startsWith(u8, trimmed_right, "        param[")) {
                try self.updateFunctionParam(trimmed_right);
                continue;
            } else if (std.mem.indexOf(u8, trimmed_right, ":")) |_| {
                const trimmed_line = std.mem.trim(u8, trimmed_right, " \t");
                const is_instruction_with_location = std.mem.startsWith(u8, trimmed_line, "Peek ") or
                    std.mem.startsWith(u8, trimmed_line, "Call ") or
                    std.mem.startsWith(u8, trimmed_line, "LoadVar ") or
                    std.mem.startsWith(u8, trimmed_line, "StoreVar ") or
                    std.mem.startsWith(u8, trimmed_line, "AssertFail ");

                if (is_instruction_with_location) {
                    if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                        try self.parseFunction(trimmed_right);
                    } else {
                        try self.parseInstruction(trimmed_right);
                    }
                } else {
                    const semicolon_pos_opt = std.mem.indexOf(u8, trimmed_line, ";");
                    const pre_comment = if (semicolon_pos_opt) |p| std.mem.trimRight(u8, trimmed_line[0..p], " \t") else trimmed_line;
                    const is_label = std.mem.endsWith(u8, pre_comment, ":");

                    if (is_label) {
                        const label_line = pre_comment;
                        const label_colon_pos = std.mem.indexOf(u8, label_line, ":").?;
                        const label_slice = label_line[0..label_colon_pos];
                        try self.handleLabel(label_slice);
                        continue;
                    } else {
                        if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                            try self.parseFunction(trimmed_right);
                        } else {
                            try self.parseInstruction(trimmed_right);
                        }
                    }
                }
            } else if (std.mem.startsWith(u8, trimmed_right, "    ") and !std.mem.startsWith(u8, trimmed_right, "        ")) {
                if (std.mem.indexOf(u8, trimmed_right, "(") != null and std.mem.indexOf(u8, trimmed_right, "args)") != null) {
                    try self.parseFunction(trimmed_right);
                } else {
                    try self.parseInstruction(trimmed_right);
                }
            }
        }

        return HIRProgram{
            .instructions = try self.instructions.toOwnedSlice(),
            .constant_pool = try self.constants.toOwnedSlice(),
            .string_pool = &[_][]const u8{},
            .function_table = try self.functions.toOwnedSlice(),
            .module_map = std.StringHashMap(HIRProgram.ModuleInfo).init(self.allocator),
            .allocator = self.allocator,
        };
    }

    fn parseConstant(self: *SoxaTextParser, line: []const u8) !void {
        const colon_pos = std.mem.indexOfScalar(u8, line, ':') orelse return error.InvalidCharacter;
        var rest = std.mem.trimLeft(u8, line[colon_pos + 1 ..], " \t");

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
        } else if (std.mem.eql(u8, type_tok, "enum")) {
            const variant_str = std.mem.trim(u8, payload, " \t");
            if (std.mem.indexOfScalar(u8, variant_str, '.')) |dot_pos| {
                const type_name = try self.allocator.dupe(u8, variant_str[0..dot_pos]);

                const after_dot = variant_str[dot_pos + 1 ..];
                if (std.mem.indexOfScalar(u8, after_dot, ' ')) |space_pos| {
                    const variant_name = try self.allocator.dupe(u8, after_dot[0..space_pos]);

                    const idx_start = std.mem.indexOf(u8, after_dot, "(idx ") orelse {
                        try self.constants.append(HIRValue.nothing);
                        return;
                    };
                    const idx_str = after_dot[idx_start + 5 ..];
                    const idx_end = std.mem.indexOfScalar(u8, idx_str, ')') orelse {
                        try self.constants.append(HIRValue.nothing);
                        return;
                    };
                    const variant_index = std.fmt.parseInt(u32, idx_str[0..idx_end], 10) catch 0;

                    try self.constants.append(HIRValue{
                        .enum_variant = HIREnum{
                            .type_name = type_name,
                            .variant_name = variant_name,
                            .variant_index = variant_index,
                            .path = null,
                        },
                    });
                } else {
                    try self.constants.append(HIRValue.nothing);
                }
            } else {
                try self.constants.append(HIRValue.nothing);
            }
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
            try self.constants.append(HIRValue.nothing);
        }
    }

    fn parseFunction(self: *SoxaTextParser, line: []const u8) !void {
        const trimmed = std.mem.trim(u8, line, " \t");
        if (std.mem.indexOf(u8, trimmed, "(")) |paren_pos| {
            const name = try self.allocator.dupe(u8, trimmed[0..paren_pos]);
            var arity: u32 = 0;
            if (std.mem.indexOf(u8, trimmed, "(") != null and std.mem.indexOf(u8, trimmed, " args)") != null) {
                const args_start = std.mem.indexOf(u8, trimmed, "(").? + 1;
                const args_end = std.mem.indexOf(u8, trimmed, " args)").?;
                if (args_end > args_start) {
                    const arity_str = trimmed[args_start..args_end];
                    arity = std.fmt.parseInt(u32, arity_str, 10) catch 0;
                }
            }

            var return_type: HIRType = .Unknown;
            if (std.mem.indexOf(u8, trimmed, "->")) |arrow_pos| {
                const type_start = arrow_pos + 2;
                const type_str = std.mem.trim(u8, trimmed[type_start..], " \t");
                if (type_str.len != 0) {
                    return_type = parseTypeToken(type_str);
                }
            }

            try self.functions.append(HIRProgram.HIRFunction{
                .name = name,
                .qualified_name = name,
                .arity = arity,
                .return_type = return_type,
                .start_label = try self.allocator.dupe(u8, "unknown"),
                .local_var_count = 0,
                .is_entry = false,
                .param_is_alias = blk: {
                    const arr = try self.allocator.alloc(bool, arity);
                    for (arr) |*item| item.* = false;
                    break :blk arr;
                },
                .param_types = blk: {
                    const arr = try self.allocator.alloc(HIRType, arity);
                    for (arr) |*item| item.* = .Unknown;
                    break :blk arr;
                },
            });
        }
    }

    fn updateFunctionEntry(self: *SoxaTextParser, entry_line: []const u8) !void {
        if (std.mem.indexOf(u8, entry_line, "entry:")) |entry_pos| {
            const label_start = entry_pos + 6;
            const label = std.mem.trim(u8, entry_line[label_start..], " \t");

            if (self.functions.items.len > 0) {
                const last_func_idx = self.functions.items.len - 1;
                const label_copy = try self.allocator.dupe(u8, label);
                self.functions.items[last_func_idx].start_label = label_copy;
                _ = try self.label_to_function.put(label_copy, last_func_idx);
            }
        }
    }

    fn updateFunctionParam(self: *SoxaTextParser, param_line: []const u8) !void {
        const trimmed = std.mem.trim(u8, param_line, " \t");
        const open_idx = std.mem.indexOfScalar(u8, trimmed, '[') orelse return;
        const close_idx = std.mem.indexOfScalarPos(u8, trimmed, open_idx + 1, ']') orelse return;
        const index_str = std.mem.trim(u8, trimmed[open_idx + 1 .. close_idx], " \t");
        const param_index = std.fmt.parseInt(usize, index_str, 10) catch return;

        const colon_idx = std.mem.indexOfScalarPos(u8, trimmed, close_idx + 1, ':') orelse return;
        const after_colon_full = std.mem.trimLeft(u8, trimmed[colon_idx + 1 ..], " \t");

        const alias_marker = std.mem.indexOf(u8, after_colon_full, "alias:");
        const type_slice = blk: {
            if (alias_marker) |pos| {
                break :blk std.mem.trimRight(u8, after_colon_full[0..pos], " \t");
            }
            break :blk std.mem.trimRight(u8, after_colon_full, " \t");
        };

        var alias_flag = false;
        if (alias_marker) |pos| {
            const alias_start = pos + "alias:".len;
            const alias_slice = std.mem.trim(u8, after_colon_full[alias_start..], " \t");
            alias_flag = std.mem.startsWith(u8, alias_slice, "true");
        }

        if (self.functions.items.len == 0) return;
        const last_func_idx = self.functions.items.len - 1;
        if (param_index >= self.functions.items[last_func_idx].param_types.len) return;

        self.functions.items[last_func_idx].param_types[param_index] = parseTypeToken(type_slice);
        if (param_index < self.functions.items[last_func_idx].param_is_alias.len) {
            self.functions.items[last_func_idx].param_is_alias[param_index] = alias_flag;
        }
    }

    fn parseTypeToken(token: []const u8) HIRType {
        if (std.mem.eql(u8, token, "Int")) return .Int;
        if (std.mem.eql(u8, token, "Float")) return .Float;
        if (std.mem.eql(u8, token, "String")) return .String;
        if (std.mem.eql(u8, token, "Byte")) return .Byte;
        if (std.mem.eql(u8, token, "Tetra")) return .Tetra;
        if (std.mem.eql(u8, token, "Nothing")) return .Nothing;
        if (std.mem.eql(u8, token, "Array")) return .Unknown;
        if (std.mem.eql(u8, token, "Struct")) return HIRType{ .Struct = 0 };
        if (std.mem.eql(u8, token, "Map")) return .Unknown;
        if (std.mem.eql(u8, token, "Enum")) return HIRType{ .Enum = 0 };
        if (std.mem.eql(u8, token, "Function")) return .Unknown;
        if (std.mem.eql(u8, token, "Union")) return .Unknown;
        if (std.mem.eql(u8, token, "Auto")) return .Unknown;
        return .Unknown;
    }

    fn inferTargetModule(self: *SoxaTextParser, qualified_name: []const u8, call_kind: CallKind) !?[]const u8 {
        if (call_kind != .ModuleFunction) {
            return null;
        }
        const dot_idx = std.mem.lastIndexOfScalar(u8, qualified_name, '.') orelse return null;
        if (dot_idx == 0) return null;
        return try self.allocator.dupe(u8, qualified_name[0..dot_idx]);
    }

    fn findFunctionIndexForLabel(self: *SoxaTextParser, label: []const u8) ?usize {
        if (self.label_to_function.get(label)) |idx| {
            return idx;
        }

        if (std.mem.startsWith(u8, label, "func_")) {
            const rest = label[5..];
            if (std.mem.indexOfScalar(u8, rest, '_')) |underscore| {
                const name_slice = rest[0..underscore];
                for (self.functions.items, 0..) |func, idx| {
                    if (std.mem.eql(u8, func.name, name_slice)) {
                        _ = self.label_to_function.put(label, idx) catch {};
                        return idx;
                    }
                }
            }
        }

        return null;
    }

    fn handleLabel(self: *SoxaTextParser, label_slice: []const u8) !void {
        const label_copy = try self.allocator.dupe(u8, label_slice);

        if (self.findFunctionIndexForLabel(label_copy)) |idx| {
            self.current_function = idx;
            var func = &self.functions.items[idx];
            if (std.mem.indexOf(u8, label_copy, "_body_") != null) {
                if (func.body_label == null) {
                    func.body_label = label_copy;
                }
                _ = self.label_to_function.put(label_copy, idx) catch {};
            } else {
                _ = self.label_to_function.put(label_copy, idx) catch {};
            }
        } else if (self.current_function == null) {
            self.current_function = null;
        }

        try self.instructions.append(HIRInstruction{ .Label = .{ .name = label_copy, .vm_address = 0 } });
    }
    fn parseInstruction(self: *SoxaTextParser, line: []const u8) !void {
        const trimmed = std.mem.trim(u8, line, " \t");

        if (std.mem.startsWith(u8, trimmed, "PeekStruct")) {
            const struct_name_start = std.mem.indexOf(u8, trimmed, "\"").? + 1;
            const struct_name_end = std.mem.indexOfPos(u8, trimmed, struct_name_start, "\"").?;
            const struct_name = try self.allocator.dupe(u8, trimmed[struct_name_start..struct_name_end]);

            const count_start = struct_name_end + 2;
            const count_end = std.mem.indexOfAny(u8, trimmed[count_start..], " [").? + count_start;
            const field_count = try std.fmt.parseInt(u32, trimmed[count_start..count_end], 10);

            var field_names = std.array_list.Managed([]const u8).init(self.allocator);
            var field_types = std.array_list.Managed(HIRType).init(self.allocator);
            const field_path = std.array_list.Managed([]const u8).init(self.allocator);

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
            const scope_str = tokens.next() orelse "Local";
            const scope_kind = if (std.mem.eql(u8, scope_str, "ModuleGlobal"))
                ScopeKind.ModuleGlobal
            else if (std.mem.eql(u8, scope_str, "ImportedModule"))
                ScopeKind.ImportedModule
            else if (std.mem.eql(u8, scope_str, "Builtin"))
                ScopeKind.Builtin
            else
                ScopeKind.Local;
            try self.instructions.append(HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = scope_kind, .module_context = null } });
        } else if (std.mem.eql(u8, op, "StoreVar")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            const scope_str = tokens.next() orelse "Local";
            const scope_kind = if (std.mem.eql(u8, scope_str, "ModuleGlobal"))
                ScopeKind.ModuleGlobal
            else if (std.mem.eql(u8, scope_str, "ImportedModule"))
                ScopeKind.ImportedModule
            else if (std.mem.eql(u8, scope_str, "Builtin"))
                ScopeKind.Builtin
            else
                ScopeKind.Local;
            // Optional trailing type token; default to Int if absent/unknown
            const maybe_type = tokens.next();
            const expected_type: HIRType = if (maybe_type) |t|
                (if (std.mem.eql(u8, t, "Int")) .Int else if (std.mem.eql(u8, t, "Byte")) .Byte else if (std.mem.eql(u8, t, "Float")) .Float else if (std.mem.eql(u8, t, "String")) .String else if (std.mem.eql(u8, t, "Tetra")) .Tetra else if (std.mem.eql(u8, t, "Nothing")) .Nothing else .Int)
            else
                .Int;
            try self.instructions.append(HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
        } else if (std.mem.eql(u8, op, "LoadAlias")) {
            const idx_str = tokens.next() orelse return;
            const slot_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .LoadAlias = .{ .slot_index = slot_index, .var_name = var_name } });
        } else if (std.mem.eql(u8, op, "StoreAlias")) {
            const idx_str = tokens.next() orelse return;
            const slot_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            const type_str = tokens.next() orelse return;
            const expected_type: HIRType = if (std.mem.eql(u8, type_str, "Int")) .Int else if (std.mem.eql(u8, type_str, "Byte")) .Byte else if (std.mem.eql(u8, type_str, "Float")) .Float else if (std.mem.eql(u8, type_str, "String")) .String else if (std.mem.eql(u8, type_str, "Tetra")) .Tetra else if (std.mem.eql(u8, type_str, "Nothing")) .Nothing else .Int;
            try self.instructions.append(HIRInstruction{ .StoreAlias = .{ .slot_index = slot_index, .var_name = var_name, .expected_type = expected_type } });
        } else if (std.mem.eql(u8, op, "StoreConst")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            const scope_str = tokens.next() orelse "Local";
            const scope_kind = if (std.mem.eql(u8, scope_str, "ModuleGlobal"))
                ScopeKind.ModuleGlobal
            else if (std.mem.eql(u8, scope_str, "ImportedModule"))
                ScopeKind.ImportedModule
            else if (std.mem.eql(u8, scope_str, "Builtin"))
                ScopeKind.Builtin
            else
                ScopeKind.Local;
            try self.instructions.append(HIRInstruction{ .StoreConst = .{ .var_index = var_index, .var_name = var_name, .scope_kind = scope_kind, .module_context = null } });
        } else if (std.mem.eql(u8, op, "StoreParamAlias")) {
            const name_quoted = tokens.next() orelse return;
            const param_name = try self.parseQuotedString(name_quoted);
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            // Optional trailing type token for alias param
            const maybe_type = tokens.next();
            const param_type: HIRType = if (maybe_type) |t|
                (if (std.mem.eql(u8, t, "Int")) .Int else if (std.mem.eql(u8, t, "Byte")) .Byte else if (std.mem.eql(u8, t, "Float")) .Float else if (std.mem.eql(u8, t, "String")) .String else if (std.mem.eql(u8, t, "Tetra")) .Tetra else if (std.mem.eql(u8, t, "Nothing")) .Nothing else .Int)
            else
                .Int;
            try self.instructions.append(HIRInstruction{ .StoreParamAlias = .{ .param_name = param_name, .param_type = param_type, .var_index = var_index } });
        } else if (std.mem.eql(u8, op, "PushStorageId")) {
            const idx_str = tokens.next() orelse return;
            const var_index = std.fmt.parseInt(u32, idx_str, 10) catch return;
            const name_quoted = tokens.next() orelse return;
            const var_name = try self.parseQuotedString(name_quoted);
            try self.instructions.append(HIRInstruction{ .PushStorageId = .{ .var_index = var_index, .var_name = var_name, .scope_kind = .Local } });
        } else if (std.mem.eql(u8, op, "Arith")) {
            const op_str = tokens.next() orelse return;
            const type_str = tokens.next() orelse return;
            const arith_op = if (std.mem.eql(u8, op_str, "Add")) ArithOp.Add else if (std.mem.eql(u8, op_str, "Sub")) ArithOp.Sub else if (std.mem.eql(u8, op_str, "Mul")) ArithOp.Mul else if (std.mem.eql(u8, op_str, "Div")) ArithOp.Div else if (std.mem.eql(u8, op_str, "Mod")) ArithOp.Mod else if (std.mem.eql(u8, op_str, "Pow")) ArithOp.Pow else unreachable;
            const operand_type: HIRType = if (std.mem.eql(u8, type_str, "Int")) .Int else if (std.mem.eql(u8, type_str, "Float")) .Float else if (std.mem.eql(u8, type_str, "Byte")) .Byte else .Int;
            try self.instructions.append(HIRInstruction{ .Arith = .{ .op = arith_op, .operand_type = operand_type } });
        } else if (std.mem.eql(u8, op, "Convert")) {
            const from_str = tokens.next() orelse return;
            const to_str = tokens.next() orelse return;
            const from_type: HIRType = if (std.mem.eql(u8, from_str, "Int")) .Int else if (std.mem.eql(u8, from_str, "Byte")) .Byte else if (std.mem.eql(u8, from_str, "Float")) .Float else if (std.mem.eql(u8, from_str, "String")) .String else if (std.mem.eql(u8, from_str, "Tetra")) .Tetra else if (std.mem.eql(u8, from_str, "Nothing")) .Nothing else .Unknown;
            const to_type: HIRType = if (std.mem.eql(u8, to_str, "Int")) .Int else if (std.mem.eql(u8, to_str, "Byte")) .Byte else if (std.mem.eql(u8, to_str, "Float")) .Float else if (std.mem.eql(u8, to_str, "String")) .String else if (std.mem.eql(u8, to_str, "Tetra")) .Tetra else if (std.mem.eql(u8, to_str, "Nothing")) .Nothing else .Unknown;
            try self.instructions.append(HIRInstruction{ .Convert = .{ .from_type = from_type, .to_type = to_type } });
        } else if (std.mem.eql(u8, op, "Compare")) {
            const op_str = tokens.next() orelse return;
            const comp_op = if (std.mem.eql(u8, op_str, "Eq")) CompareOp.Eq else if (std.mem.eql(u8, op_str, "Ne")) CompareOp.Ne else if (std.mem.eql(u8, op_str, "Lt")) CompareOp.Lt else if (std.mem.eql(u8, op_str, "Le")) CompareOp.Le else if (std.mem.eql(u8, op_str, "Gt")) CompareOp.Gt else if (std.mem.eql(u8, op_str, "Ge")) CompareOp.Ge else CompareOp.Eq;
            const maybe_type = tokens.next();
            var operand_type: HIRType = .Int;
            if (maybe_type) |type_str| {
                operand_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Byte")) HIRType.Byte else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Nothing")) HIRType.Nothing else HIRType.Int;
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

            var return_type: HIRType = .Unknown;
            if (call_kind == .LocalFunction and function_index < self.functions.items.len) {
                return_type = self.functions.items[function_index].return_type;
            }

            try self.instructions.append(HIRInstruction{
                .Call = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = try self.inferTargetModule(qualified_name, call_kind),
                    .return_type = return_type,
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

            var return_type: HIRType = .Unknown;
            if (call_kind == .LocalFunction and function_index < self.functions.items.len) {
                return_type = self.functions.items[function_index].return_type;
            }

            try self.instructions.append(HIRInstruction{
                .TailCall = .{
                    .function_index = function_index,
                    .qualified_name = qualified_name,
                    .arg_count = arg_count,
                    .call_kind = call_kind,
                    .target_module = try self.inferTargetModule(qualified_name, call_kind),
                    .return_type = return_type,
                },
            });
        } else if (std.mem.eql(u8, op, "Return")) {
            const has_val_str = tokens.next() orelse return;
            const has_value = std.mem.eql(u8, has_val_str, "true");
            var return_type: HIRType = .Nothing;
            if (has_value) {
                if (self.current_function) |idx| {
                    return_type = self.functions.items[idx].return_type;
                } else {
                    return_type = .Unknown;
                }
            }
            try self.instructions.append(HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = return_type } });
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
            var location: ?Location = null;
            var union_members: ?[][]const u8 = null;

            var struct_name: ?[]const u8 = null;
            var field_names = std.array_list.Managed([]const u8).init(self.allocator);
            var field_types = std.array_list.Managed(HIRType).init(self.allocator);
            defer field_names.deinit();
            defer field_types.deinit();

            var path_builder = std.array_list.Managed(u8).init(self.allocator);
            defer path_builder.deinit();

            if (self.struct_context) |context| {
                try path_builder.appendSlice(context.type_name);
                for (context.field_path.items) |field| {
                    try path_builder.appendSlice(".");
                    try path_builder.appendSlice(field);
                }
            }

            if (name_or_type.len > 2 and name_or_type[0] == '"' and name_or_type[name_or_type.len - 1] == '"') {
                name = try self.parseQuotedString(name_or_type);
                const type_str = tokens.next() orelse "String";
                value_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Array")) HIRType.Nothing else if (std.mem.eql(u8, type_str, "Map")) HIRType.Nothing else if (std.mem.eql(u8, type_str, "Struct")) blk: {
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else if (name) |n| n else "anonymous";
                    break :blk HIRType{ .Struct = 0 };
                } else if (std.mem.eql(u8, type_str, "Enum")) HIRType{ .Enum = 0 } else HIRType.String;

                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            } else {
                value_type = if (std.mem.eql(u8, name_or_type, "Int")) HIRType.Int else if (std.mem.eql(u8, name_or_type, "Float")) HIRType.Float else if (std.mem.eql(u8, name_or_type, "String")) HIRType.String else if (std.mem.eql(u8, name_or_type, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, name_or_type, "Array")) HIRType.Nothing else if (std.mem.eql(u8, name_or_type, "Struct")) blk: {
                    struct_name = if (path_builder.items.len > 0)
                        try self.allocator.dupe(u8, path_builder.items)
                    else
                        "anonymous";
                    break :blk HIRType{ .Struct = 0 };
                } else if (std.mem.eql(u8, name_or_type, "Enum")) HIRType{ .Enum = 0 } else HIRType.String;

                if (tokens.next()) |location_str| {
                    if (std.mem.startsWith(u8, location_str, "@")) {
                        location = try self.parseLocationString(location_str);
                    }
                }
            }

            const union_marker = std.mem.indexOf(u8, trimmed, "; union [");
            if (union_marker) |marker_pos| {
                const list_start = marker_pos + 9;
                if (std.mem.indexOfPos(u8, trimmed, list_start, "]")) |end_pos| {
                    const inner = std.mem.trim(u8, trimmed[list_start..end_pos], " \t");
                    var items = std.mem.splitScalar(u8, inner, ',');
                    var tmp = std.array_list.Managed([]const u8).init(self.allocator);
                    defer tmp.deinit();
                    while (items.next()) |it| {
                        const item_trim = std.mem.trim(u8, it, " \t");
                        if (item_trim.len > 0) {
                            const s = try self.allocator.dupe(u8, item_trim);
                            try tmp.append(s);
                        }
                    }
                    union_members = try tmp.toOwnedSlice();
                }
            }

            if (value_type == .Struct) {
                if (self.struct_context) |_| {
                    const struct_info = try self.getCurrentStructInfo();
                    for (struct_info.fields) |field| {
                        try field_names.append(field.name);
                        try field_types.append(field.type);
                    }
                    self.allocator.free(struct_info.fields);

                    try self.instructions.append(.{
                        .PeekStruct = .{
                            .type_name = struct_name.?,
                            .field_count = @intCast(field_names.items.len),
                            .field_names = try field_names.toOwnedSlice(),
                            .field_types = try field_types.toOwnedSlice(),
                            .location = location,
                            .should_pop_after_peek = false,
                        },
                    });
                } else {
                    try self.instructions.append(.{ .Peek = .{
                        .name = if (path_builder.items.len > 0)
                            try self.allocator.dupe(u8, path_builder.items)
                        else
                            name,
                        .value_type = value_type,
                        .location = location,
                        .union_members = union_members,
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
                    .union_members = union_members,
                } });
            }
        } else if (std.mem.eql(u8, op, "Print")) {
            try self.instructions.append(.{ .Print = .{} });
        } else if (std.mem.eql(u8, op, "PrintInterpolated")) {
            const format_parts_str = tokens.next() orelse return;
            const placeholder_indices_str = tokens.next() orelse return;
            const argument_count_str = tokens.next() orelse return;

            const format_parts_count = std.fmt.parseInt(u32, format_parts_str, 10) catch return;
            const placeholder_indices_count = std.fmt.parseInt(u32, placeholder_indices_str, 10) catch return;
            const argument_count = std.fmt.parseInt(u32, argument_count_str, 10) catch return;

            const format_part_ids_str = tokens.next() orelse return;
            if (!std.mem.startsWith(u8, format_part_ids_str, "[") or !std.mem.endsWith(u8, format_part_ids_str, "]")) {
                return error.InvalidFormatPartIds;
            }

            const ids_content = format_part_ids_str[1 .. format_part_ids_str.len - 1];
            var format_part_ids = std.array_list.Managed(u32).init(self.allocator);
            defer format_part_ids.deinit();

            if (ids_content.len > 0) {
                var id_tokens = std.mem.splitScalar(u8, ids_content, ',');
                while (id_tokens.next()) |id_str| {
                    const id = std.fmt.parseInt(u32, std.mem.trim(u8, id_str, " "), 10) catch return;
                    try format_part_ids.append(id);
                }
            }

            const format_parts = try self.allocator.alloc([]const u8, format_parts_count);
            const placeholder_indices = try self.allocator.alloc(u32, placeholder_indices_count);
            const format_part_ids_array = try self.allocator.alloc(u32, format_part_ids.items.len);

            for (format_parts) |*part| {
                part.* = "";
            }
            for (placeholder_indices, 0..) |*index, i| {
                index.* = @intCast(i);
            }
            for (format_part_ids.items, 0..) |id, i| {
                format_part_ids_array[i] = id;
            }

            try self.instructions.append(.{ .PrintInterpolated = .{
                .format_parts = format_parts,
                .placeholder_indices = placeholder_indices,
                .argument_count = argument_count,
                .format_part_ids = format_part_ids_array,
            } });
        } else if (std.mem.eql(u8, op, "ArrayNew")) {
            const type_str = tokens.next() orelse return;
            const size_str = tokens.next() orelse return;
            const element_type: HIRType = if (std.mem.eql(u8, type_str, "Int")) .Int else if (std.mem.eql(u8, type_str, "Float")) .Float else if (std.mem.eql(u8, type_str, "String")) .String else if (std.mem.eql(u8, type_str, "Byte")) .Byte else if (std.mem.eql(u8, type_str, "Tetra")) .Tetra else if (std.mem.eql(u8, type_str, "Array")) .Nothing else .Nothing;
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
        } else if (std.mem.eql(u8, op, "ArrayInsert")) {
            try self.instructions.append(HIRInstruction.ArrayInsert);
        } else if (std.mem.eql(u8, op, "ArrayRemove")) {
            try self.instructions.append(HIRInstruction.ArrayRemove);
        } else if (std.mem.eql(u8, op, "ArraySlice")) {
            try self.instructions.append(HIRInstruction.ArraySlice);
        } else if (std.mem.eql(u8, op, "ArrayLen")) {
            try self.instructions.append(HIRInstruction.ArrayLen);
        } else if (std.mem.eql(u8, op, "ArrayConcat")) {
            try self.instructions.append(HIRInstruction.ArrayConcat);
        } else if (std.mem.eql(u8, op, "ArrayGetAndAdd")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            std.debug.print("SOXA PARSER: Parsing ArrayGetAndAdd instruction with bounds_check={}\n", .{bounds_check});
            try self.instructions.append(HIRInstruction{ .ArrayGetAndAdd = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayGetAndSub")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGetAndSub = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayGetAndMul")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGetAndMul = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayGetAndDiv")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGetAndDiv = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayGetAndMod")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGetAndMod = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "ArrayGetAndPow")) {
            const bounds_str = tokens.next() orelse return;
            const bounds_check = std.mem.eql(u8, bounds_str, "true");
            try self.instructions.append(HIRInstruction{ .ArrayGetAndPow = .{ .bounds_check = bounds_check } });
        } else if (std.mem.eql(u8, op, "Range")) {
            const type_str = tokens.next() orelse return;
            const element_type: HIRType = if (std.mem.eql(u8, type_str, "Int")) .Int else if (std.mem.eql(u8, type_str, "Byte")) .Byte else if (std.mem.eql(u8, type_str, "Float")) .Float else .Int;
            try self.instructions.append(HIRInstruction{ .Range = .{ .element_type = element_type } });
        } else if (std.mem.eql(u8, op, "Map")) {
            const count_str = tokens.next() orelse return;
            const key_type_str = tokens.next() orelse return;
            const entry_count = std.fmt.parseInt(u32, count_str, 10) catch return;
            const key_type: HIRType = if (std.mem.eql(u8, key_type_str, "String")) .String else if (std.mem.eql(u8, key_type_str, "Int")) .Int else .String;

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
                .value_type = HIRType.Unknown,
            } });
        } else if (std.mem.eql(u8, op, "MapGet")) {
            const key_type_str = tokens.next() orelse return;
            const key_type: HIRType = if (std.mem.eql(u8, key_type_str, "String")) .String else if (std.mem.eql(u8, key_type_str, "Int")) .Int else .String;

            try self.instructions.append(HIRInstruction{ .MapGet = .{
                .key_type = key_type,
            } });
        } else if (std.mem.eql(u8, op, "MapSet")) {
            const key_type_str = tokens.next() orelse return;
            const key_type: HIRType = if (std.mem.eql(u8, key_type_str, "String")) .String else if (std.mem.eql(u8, key_type_str, "Int")) .Int else .String;

            try self.instructions.append(HIRInstruction{ .MapSet = .{
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

            const field_types = try self.allocator.alloc(HIRType, field_count);
            for (field_types) |*field_type| {
                field_type.* = HIRType.Nothing;
            }

            try self.instructions.append(HIRInstruction{
                .StructNew = .{
                    .type_name = type_name,
                    .field_count = field_count,
                    .field_types = field_types,
                    .size_bytes = 0,
                },
            });
        } else if (std.mem.eql(u8, op, "GetField")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .GetField = .{
                    .field_name = field_name,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = 0,
                    .field_for_peek = false,
                },
            });
        } else if (std.mem.eql(u8, op, "SetField")) {
            const field_name_quoted = tokens.next() orelse return;
            const field_name = try self.parseQuotedString(field_name_quoted);

            try self.instructions.append(HIRInstruction{
                .SetField = .{
                    .field_name = field_name,
                    .container_type = HIRType{ .Struct = 0 },
                    .field_index = 0,
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
            else if (std.mem.eql(u8, op_str, "Substring"))
                StringOpType.Substring
            else if (std.mem.eql(u8, op_str, "Concat"))
                StringOpType.Concat
            else if (std.mem.eql(u8, op_str, "ToInt"))
                StringOpType.ToInt
            else if (std.mem.eql(u8, op_str, "ToFloat"))
                StringOpType.ToFloat
            else if (std.mem.eql(u8, op_str, "ToByte"))
                StringOpType.ToByte
            else if (std.mem.eql(u8, op_str, "ToString"))
                StringOpType.ToString
            else if (std.mem.eql(u8, op_str, "Pop"))
                StringOpType.Pop
            else
                StringOpType.Length;

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
                LogicalOpType.And;

            try self.instructions.append(HIRInstruction{ .LogicalOp = .{
                .op = logical_op,
            } });
        } else if (std.mem.eql(u8, op, "TypeCheck")) {
            const target_type_quoted = tokens.next() orelse return;
            const target_type = try self.parseQuotedString(target_type_quoted);
            try self.instructions.append(HIRInstruction{ .TypeCheck = .{
                .target_type = target_type,
            } });
        } else if (std.mem.eql(u8, op, "AssertFail")) {
            const rest_of_line = tokens.rest();
            var has_message = false;
            var location: ?Location = null;

            if (std.mem.indexOf(u8, rest_of_line, "@")) |at_pos| {
                const location_part = rest_of_line[at_pos..];

                var location_end = location_part.len;
                if (std.mem.indexOf(u8, location_part, " with message")) |msg_pos| {
                    location_end = msg_pos;
                    has_message = true;
                }

                const location_str = location_part[0..location_end];
                location = self.parseLocationString(location_str) catch null;
            }

            try self.instructions.append(HIRInstruction{ .AssertFail = .{
                .location = location orelse Location{
                    .file = "unknown",
                    .range = .{
                        .start_line = 0,
                        .start_col = 0,
                        .end_line = 0,
                        .end_col = 0,
                    },
                },
                .has_message = has_message,
            } });
        }
    }

    fn parseQuotedString(self: *SoxaTextParser, quoted: []const u8) ![]const u8 {
        if (quoted.len >= 2 and quoted[0] == '"' and quoted[quoted.len - 1] == '"') {
            const content = quoted[1 .. quoted.len - 1];

            var result = std.array_list.Managed(u8).init(self.allocator);
            defer result.deinit();

            var i: usize = 0;
            while (i < content.len) {
                if (content[i] == '\\' and i + 1 < content.len) {
                    i += 1;
                    switch (content[i]) {
                        'n' => try result.append('\n'),
                        't' => try result.append('\t'),
                        '"' => try result.append('"'),
                        '\\' => try result.append('\\'),
                        else => {
                            try result.append('\\');
                            try result.append(content[i]);
                        },
                    }
                } else {
                    try result.append(content[i]);
                }
                i += 1;
            }

            return try result.toOwnedSlice();
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

    fn parseLocationString(self: *SoxaTextParser, location_str: []const u8) !Location {
        if (!std.mem.startsWith(u8, location_str, "@")) {
            return error.InvalidLocationFormat;
        }

        const location_part = location_str[1..];

        const last_colon = std.mem.lastIndexOfScalar(u8, location_part, ':') orelse return error.InvalidLocationFormat;
        const second_last_colon = std.mem.lastIndexOfScalar(u8, location_part[0..last_colon], ':') orelse return error.InvalidLocationFormat;

        const file_part = location_part[0..second_last_colon];
        const line_part = location_part[second_last_colon + 1 .. last_colon];
        const column_part = location_part[last_colon + 1 ..];

        const file = try self.allocator.dupe(u8, file_part);
        const line = try std.fmt.parseInt(usize, line_part, 10);
        const column = try std.fmt.parseInt(usize, column_part, 10);

        return Location{
            .file = file,
            .range = .{
                .start_line = line,
                .start_col = column,
                .end_line = line,
                .end_col = column,
            },
        };
    }
};
