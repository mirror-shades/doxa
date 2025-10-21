const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const instructions = @import("../../interpreter/instructions.zig");
const reporting = @import("../../utils/reporting.zig");
const Reporting = reporting;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const SoxaTypes = @import("soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const HIRProgram = SoxaTypes.HIRProgram;
const CallKind = SoxaTypes.CallKind;
const SoxaValues = @import("soxa_values.zig");
const HIRArray = SoxaValues.HIRArray;
const HIRMapEntry = SoxaValues.HIRMapEntry;
const HIRValue = SoxaValues.HIRValue;
const HIREnum = SoxaValues.HIREnum;
const SoxaInstructions = @import("soxa_instructions.zig");
const ArithOp = SoxaInstructions.ArithOp;
const CompareOp = SoxaInstructions.CompareOp;
const StringOpType = SoxaInstructions.StringOpType;
const LogicalOpType = SoxaInstructions.LogicalOpType;
const HIRInstruction = SoxaInstructions.HIRInstruction;
const ExceptionBehavior = SoxaInstructions.ExceptionBehavior;
const ResizeBehavior = SoxaInstructions.ResizeBehavior;
const HIRGenerator = @import("soxa_generator.zig").HIRGenerator;
const SoxaTextParser = @import("soxa_parser.zig").SoxaTextParser;

const HirTypeNames = [_][]const u8{
    "Int",
    "Byte",
    "Float",
    "String",
    "Tetra",
    "Nothing",
    "Array",
    "Struct",
    "Map",
    "Enum",
    "Function",
    "Union",
    "Unknown",
};

fn hirTypeNameSafe(value: HIRType) []const u8 {
    const raw = @intFromEnum(value);
    return if (raw < HirTypeNames.len) HirTypeNames[raw] else "Invalid";
}

pub fn translateToVMBytecode(program: *HIRProgram, allocator: std.mem.Allocator, reporter: *reporting.Reporter) ![]u8 {
    var bytecode = std.array_list.Managed(u8).init(allocator);
    defer bytecode.deinit();

    // First pass: resolve labels to addresses
    var label_addresses = std.StringHashMap(u32).init(allocator);
    defer label_addresses.deinit();

    var address: u32 = 0;
    for (program.instructions) |instruction| {
        switch (instruction) {
            .Label => |label| {
                try label_addresses.put(label.name, address);
            },
            else => {
                address += getBytecodeSize(instruction);
            },
        }
    }

    // Second pass: generate actual bytecode
    for (program.instructions) |instruction| {
        switch (instruction) {
            .Const => |c| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CONST));
                try bytecode.append(@intCast(c.constant_id));
            },
            .LoadVar => |v| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_VAR));
                try bytecode.append(@intCast(v.var_index));
            },
            .StoreVar => |v| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_SET_VAR));
                try bytecode.append(@intCast(v.var_index));
            },
            .StoreConst => |v| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_SET_CONST));
                try bytecode.append(@intCast(v.var_index));
            },
            .PushStorageId => |p| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CONST));
                try bytecode.append(@intCast(p.var_index));
            },
            .StoreParamAlias => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_SET_VAR));
                try bytecode.append(0);
            },
            .IntArith => |a| {
                const opcode = switch (a.op) {
                    .Add => instructions.OpCode.OP_IADD,
                    .Sub => instructions.OpCode.OP_ISUB,
                    .Mul => instructions.OpCode.OP_IMUL,
                    .Div => unreachable,
                    .Mod => instructions.OpCode.OP_IADD, // TODO: Add OP_IMOD to your VM
                };
                try bytecode.append(@intFromEnum(opcode));
            },
            .Compare => |c| {
                const opcode = switch (c.op) {
                    .Eq => instructions.OpCode.OP_EQUAL,
                    .Ne => instructions.OpCode.OP_NOTEQUAL,
                    .Lt => instructions.OpCode.OP_LESS,
                    .Gt => instructions.OpCode.OP_GREATER,
                    .Le => instructions.OpCode.OP_LESS, // TODO: Add OP_LESS_EQUAL
                    .Ge => instructions.OpCode.OP_GREATER, // TODO: Add OP_GREATER_EQUAL
                };
                try bytecode.append(@intFromEnum(opcode));
            },
            .Jump => |j| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_JUMP));
                const target_addr = label_addresses.get(j.label) orelse {
                    reporter.reportError("Undefined label: {s}", .{j.label});
                    return ErrorList.UndefinedVariable;
                };
                const current_addr = @as(u32, @intCast(bytecode.items.len - 1));
                const offset = target_addr - current_addr;
                try bytecode.append(@intCast(offset));
            },
            .JumpCond => |j| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_JUMP_IF_FALSE));
                const target_addr = label_addresses.get(j.label_false) orelse {
                    reporter.reportError("Undefined label: {s}", .{j.label_false});
                    return ErrorList.UndefinedVariable;
                };
                const current_addr = @as(u32, @intCast(bytecode.items.len - 1));
                const offset = target_addr - current_addr;
                try bytecode.append(@intCast(offset));
            },
            .Call => |c| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CALL));
                try bytecode.append(@intCast(c.function_index));
            },
            .Return => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_RETURN));
            },
            .Dup => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP));
            },
            .Pop => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_POP));
            },
            .Peek => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP));
            },
            .AssertFail => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_ASSERT_FAIL));
            },
            .Halt => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_HALT));
            },
            .Label => {},
            else => {
                reporter.reportError("Unhandled HIR instruction for VM translation: {}", .{instruction});
                return ErrorList.UnsupportedStatement;
            },
        }
    }

    const result = try bytecode.toOwnedSlice();
    return result;
}

fn getBytecodeSize(instruction: HIRInstruction) u32 {
    return switch (instruction) {
        .Const, .LoadVar, .StoreVar, .StoreConst, .Jump, .JumpCond, .Call, .TailCall => 2,
        .IntArith, .FloatArith, .Convert, .Compare, .Return, .Dup, .Pop, .Swap, .Peek, .Halt, .AssertFail => 1,
        .Label => 0,
        else => 1,
    };
}

pub fn convertHIRConstants(hir_constants: []HIRValue, allocator: std.mem.Allocator) ![]instructions.Value {
    var vm_constants = std.array_list.Managed(instructions.Value).init(allocator);
    defer vm_constants.deinit();

    for (hir_constants) |hir_const| {
        const vm_value = switch (hir_const) {
            .int => |i| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = i } },
            .float => |f| instructions.Value{ .type = .FLOAT, .nothing = false, .data = .{ .float = f } },
            .string => |s| instructions.Value{ .type = .STRING, .nothing = false, .data = .{ .string = s } },
            .tetra => |t| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, t) } },
            .byte => |u| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, u) } },
            .nothing => instructions.Value{ .type = .INT, .nothing = true, .data = .{ .int = 0 } },
        };
        try vm_constants.append(vm_value);
    }

    return vm_constants.toOwnedSlice();
}

const SOXA_MAGIC: u32 = 0x534F5841; // "SOXA" in ASCII
const SOXA_VERSION: u16 = 1;

pub const SoxaHeader = struct {
    magic: u32 = SOXA_MAGIC,
    version: u16 = SOXA_VERSION,
    instruction_count: u32,
    constant_count: u32,
    string_count: u32,
    reserved: [6]u8 = [_]u8{0} ** 6,
};

/// Computes a cache key for source file validation
/// TODO: this might be better handled earlier in the pipeline
/// possibly even in some sort of precompiler build step
fn computeCacheKey(source_path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    const source_content = try std.fs.cwd().readFileAlloc(allocator, source_path, 1024 * 1024);
    defer allocator.free(source_content);

    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(source_content);

    hasher.update("doxa-0.1.0-dev");
    hasher.update("debug-mode");

    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    return std.fmt.allocPrint(allocator, "{x}", .{hash[0..8]});
}

pub fn validateSoxaCache(soxa_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !bool {
    const file = std.fs.cwd().openFile(soxa_path, .{}) catch return false;
    defer file.close();

    var reader_buffer: [512]u8 = undefined;
    var file_reader = file.reader(&reader_buffer);
    const reader = &file_reader.interface;

    _ = reader.takeDelimiterExclusive('\n') catch return false;

    const line = reader.takeDelimiterExclusive('\n') catch return false;
    if (std.mem.indexOf(u8, line, "Source-Hash: ")) |start| {
        const cached_hash = std.mem.trim(u8, line[start + 13 ..], " \n\r");
        const current_hash = try computeCacheKey(source_path, allocator);
        defer allocator.free(current_hash);

        const is_valid = std.mem.eql(u8, cached_hash, current_hash);
        return is_valid;
    }

    return false;
}

pub fn writeSoxaFile(program: *HIRProgram, file_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !void {
    const file = std.fs.cwd().createFile(file_path, .{}) catch |err| switch (err) {
        error.AccessDenied => return ErrorList.AccessDenied,
        error.FileNotFound => return ErrorList.FileNotFound,
        else => return err,
    };
    defer file.close();

    var writer_buffer: [1024]u8 = undefined;
    var file_writer = file.writer(&writer_buffer);
    const writer = &file_writer.interface;

    const cache_key = try computeCacheKey(source_path, allocator);
    defer allocator.free(cache_key);
    try writer.print("; SOXA HIR v0.1.0\n", .{});
    try writer.print("; Source-Hash: {s}\n", .{cache_key});
    try writer.print("; Compiler-Version: 0.1.0-dev\n", .{});
    try writer.print("; Build-Flags: debug\n", .{});
    try writer.print("; Generated with {} instructions, {} constants, {} functions\n\n", .{ program.instructions.len, program.constant_pool.len, program.function_table.len });

    if (program.constant_pool.len > 0) {
        try writer.print(".constants\n", .{});
        for (program.constant_pool, 0..) |constant, i| {
            try writer.print("    const_{}: ", .{i});
            try writeHIRValueText(writer, constant);
            try writer.print("\n", .{});
        }
        try writer.print("\n", .{});
    }

    if (program.function_table.len > 0) {
        try writer.print(".functions\n", .{});
        for (program.function_table) |func| {
            try writer.print("    {s}({} args) -> {s}\n", .{ func.name, func.arity, @tagName(func.return_type) });
            try writer.print("        entry: {s}\n", .{func.start_label});

            for (func.param_types, 0..) |param_type, idx| {
                const alias_flag = if (idx < func.param_is_alias.len) func.param_is_alias[idx] else false;
                const alias_str = if (alias_flag) "true" else "false";
                try writer.print("        param[{}]:{s} alias:{s}\n", .{ idx, hirTypeNameSafe(param_type), alias_str });
            }

            if (func.is_entry) {
                try writer.print("        main: true\n", .{});
            }
        }
        try writer.print("\n", .{});
    }

    try writer.print(".code\n", .{});
    for (program.instructions) |instruction| {
        try writeHIRInstructionText(writer, instruction);
    }

    try writer.flush();
}

pub fn readSoxaFile(file_path: []const u8, allocator: std.mem.Allocator) !HIRProgram {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return ErrorList.FileNotFound,
        error.AccessDenied => return ErrorList.AccessDenied,
        else => return err,
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(source);

    var parser = SoxaTextParser.init(allocator, source);
    const program = try parser.parse();
    parser.deinit();

    return program;
}

/// Serializes a single HIR value to binary format
fn writeHIRValue(writer: anytype, value: HIRValue) !void {
    switch (value) {
        .int => |i| {
            try writer.writeByte(0);
            try writer.writeInt(i32, i, .little);
        },
        .float => |f| {
            try writer.writeByte(1);
            try writer.writeInt(u64, @bitCast(f), .little);
        },
        .string => |s| {
            try writer.writeByte(2);
            try writer.writeInt(u32, @as(u32, @intCast(s.len)), .little);
            try writer.writeAll(s);
        },
        .tetra => |t| {
            try writer.writeByte(3);
            try writer.writeByte(t);
        },
        .byte => |u| {
            try writer.writeByte(4);
            try writer.writeByte(u);
        },
        .nothing => {
            try writer.writeByte(5);
        },
        .array => |arr| {
            try writer.writeByte(6);
            try writer.writeInt(u32, @as(u32, @intCast(arr.elements.len)), .little);
            try writer.writeInt(u32, arr.capacity, .little);
            try writer.writeByte(@intFromEnum(arr.element_type));
        },
        .struct_instance => {
            try writer.writeByte(7);
            // TODO: Implement struct serialization when needed
        },
        .map => {
            try writer.writeByte(9);
            // TODO: Implement map serialization when needed
        },
        .enum_variant => |variant| {
            try writer.writeByte(10);
            try writer.writeInt(u32, @as(u32, @intCast(variant.type_name.len)), .little);
            try writer.writeAll(variant.type_name);
            try writer.writeInt(u32, @as(u32, @intCast(variant.variant_name.len)), .little);
            try writer.writeAll(variant.variant_name);
            try writer.writeInt(u32, variant.variant_index, .little);
        },
    }
}

/// Deserializes a single HIR value from binary format
fn readHIRValue(reader: anytype, allocator: std.mem.Allocator) !HIRValue {
    const type_tag = try reader.readByte();

    return switch (type_tag) {
        0 => HIRValue{ .int = try reader.readInt(i32, .little) },
        1 => HIRValue{ .float = @bitCast(try reader.readInt(u64, .little)) },
        2 => {
            const str_len = try reader.readInt(u32, .little);
            const str_data = try allocator.alloc(u8, str_len);
            _ = try reader.readAll(str_data);
            return HIRValue{ .string = str_data };
        },
        3 => HIRValue{ .tetra = try reader.readByte() },
        4 => HIRValue{ .byte = try reader.readByte() },
        5 => HIRValue.nothing,
        6 => {
            const array_len = try reader.readInt(u32, .little);
            const capacity = try reader.readInt(u32, .little);
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));

            const backing_memory = try allocator.alloc(HIRValue, capacity);
            for (backing_memory) |*element| {
                element.* = HIRValue.nothing;
            }
            const elements = backing_memory[0..array_len];

            return HIRValue{ .array = HIRArray{
                .elements = elements,
                .element_type = element_type,
                .capacity = capacity,
            } };
        },
        7 => {
            return HIRValue.nothing; // TODO: Implement proper struct deserialization
        },
        8 => {
            return HIRValue.nothing; // TODO: Implement proper map deserialization
        },
        9 => {
            const type_name_len = try reader.readInt(u32, .little);
            const type_name = try allocator.alloc(u8, type_name_len);
            _ = try reader.readAll(type_name);

            const variant_name_len = try reader.readInt(u32, .little);
            const variant_name = try allocator.alloc(u8, variant_name_len);
            _ = try reader.readAll(variant_name);

            const variant_index = try reader.readInt(u32, .little);

            return HIRValue{
                .enum_variant = HIREnum{
                    .type_name = type_name,
                    .variant_name = variant_name,
                    .variant_index = variant_index,
                    .path = null,
                },
            };
        },
        else => {
            return ErrorList.InvalidArgument;
        },
    };
}

/// Serializes a single HIR instruction to binary format
fn writeHIRInstruction(writer: anytype, instruction: HIRInstruction, allocator: std.mem.Allocator) !void {
    _ = allocator;

    switch (instruction) {
        .Const => |c| {
            try writer.writeByte(0);
            try writer.writeInt(u32, c.constant_id, .little);
        },
        .LoadVar => |v| {
            try writer.writeByte(1);
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .StoreVar => |v| {
            try writer.writeByte(2);
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .StoreConst => |v| {
            try writer.writeByte(23);
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .PushStorageId => |p| {
            try writer.writeByte(24);
            try writer.writeInt(u32, p.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(p.var_name.len)), .little);
            try writer.writeAll(p.var_name);
        },
        .StoreParamAlias => |s| {
            try writer.writeByte(25);
            try writer.writeInt(u32, @as(u32, @intCast(s.param_name.len)), .little);
            try writer.writeAll(s.param_name);
        },
        .IntArith => |a| {
            try writer.writeByte(3);
            try writer.writeByte(@intFromEnum(a.op));
        },
        .Compare => |c| {
            try writer.writeByte(4);
            try writer.writeByte(@intFromEnum(c.op));
            try writer.writeByte(@intFromEnum(c.operand_type));
        },
        .Jump => |j| {
            try writer.writeByte(5);
            try writer.writeInt(u32, @as(u32, @intCast(j.label.len)), .little);
            try writer.writeAll(j.label);
        },
        .JumpCond => |j| {
            try writer.writeByte(6);
            try writer.writeInt(u32, @as(u32, @intCast(j.label_true.len)), .little);
            try writer.writeAll(j.label_true);
            try writer.writeInt(u32, @as(u32, @intCast(j.label_false.len)), .little);
            try writer.writeAll(j.label_false);
        },
        .Call => |c| {
            try writer.writeByte(7);
            try writer.writeInt(u32, c.function_index, .little);
            try writer.writeInt(u32, c.arg_count, .little);
            try writer.writeInt(u32, @as(u32, @intCast(c.qualified_name.len)), .little);
            try writer.writeAll(c.qualified_name);
            try writer.writeByte(@intFromEnum(c.call_kind));
        },
        .Return => |r| {
            try writer.writeByte(8);
            try writer.writeByte(if (r.has_value) 1 else 0);
        },
        .Dup => {
            try writer.writeByte(9);
        },
        .Pop => {
            try writer.writeByte(10);
        },
        .Label => |l| {
            try writer.writeByte(11);
            try writer.writeInt(u32, @as(u32, @intCast(l.name.len)), .little);
            try writer.writeAll(l.name);
        },
        .Halt => {
            try writer.writeByte(12);
        },
        .Peek => |i| {
            try writer.writeByte(13);
            if (i.name) |name| {
                try writer.writeByte(1);
                try writer.writeInt(u32, @as(u32, @intCast(name.len)), .little);
                try writer.writeAll(name);
            } else {
                try writer.writeByte(0);
            }
            try writer.writeByte(@intFromEnum(i.value_type));
            if (i.location) |location| {
                try writer.writeByte(1);
                try writer.writeInt(u32, @as(u32, @intCast(location.file.len)), .little);
                try writer.writeAll(location.file);
                try writer.writeInt(u32, location.line, .little);
                try writer.writeInt(u32, location.column, .little);
            } else {
                try writer.writeByte(0);
            }
            if (i.union_members) |members| {
                try writer.writeByte(1);
                try writer.writeInt(u32, @as(u32, @intCast(members.len)), .little);
                for (members) |m| {
                    try writer.writeInt(u32, @as(u32, @intCast(m.len)), .little);
                    try writer.writeAll(m);
                }
            } else {
                try writer.writeByte(0);
            }
        },

        .ArrayNew => |a| {
            try writer.writeByte(14);
            try writer.writeByte(@intFromEnum(a.element_type));
            try writer.writeInt(u32, a.size, .little);
        },
        .ArrayGet => |a| {
            try writer.writeByte(15);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArraySet => |a| {
            try writer.writeByte(16);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayPush => |a| {
            try writer.writeByte(17);
            try writer.writeByte(@intFromEnum(a.resize_behavior));
        },
        .ArrayPop => {
            try writer.writeByte(18);
        },
        .ArrayLen => {
            try writer.writeByte(19);
        },
        .ArrayConcat => {
            try writer.writeByte(20);
        },

        .EnterScope => |s| {
            try writer.writeByte(21);
            try writer.writeInt(u32, s.scope_id, .little);
            try writer.writeInt(u32, s.var_count, .little);
        },
        .ExitScope => |s| {
            try writer.writeByte(22);
            try writer.writeInt(u32, s.scope_id, .little);
        },

        .ArrayGetAndAdd => |a| {
            try writer.writeByte(26);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayGetAndSub => |a| {
            try writer.writeByte(27);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayGetAndMul => |a| {
            try writer.writeByte(28);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayGetAndDiv => |a| {
            try writer.writeByte(29);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayGetAndMod => |a| {
            try writer.writeByte(30);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayGetAndPow => |a| {
            try writer.writeByte(31);
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },

        else => {
            return ErrorList.UnsupportedStatement;
        },
    }
}

/// Deserializes a single HIR instruction from binary format
fn readHIRInstruction(reader: anytype, allocator: std.mem.Allocator) !HIRInstruction {
    const instruction_tag = try reader.readByte();

    return switch (instruction_tag) {
        0 => {
            const constant_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .Const = .{ .value = HIRValue.nothing, .constant_id = constant_id } };
        },
        1 => {
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null } };
        },
        2 => {
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null, .expected_type = .Auto } };
        },
        23 => {
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .StoreConst = .{ .var_index = var_index, .var_name = name } };
        },
        3 => {
            const op_byte = try reader.readByte();
            const op = @as(ArithOp, @enumFromInt(op_byte));
            return HIRInstruction{ .IntArith = .{ .op = op, .overflow_behavior = .Wrap } };
        },
        4 => {
            const op_byte = try reader.readByte();
            const op = @as(CompareOp, @enumFromInt(op_byte));
            const type_byte = try reader.readByte();
            const operand_type = @as(HIRType, @enumFromInt(type_byte));
            return HIRInstruction{ .Compare = .{ .op = op, .operand_type = operand_type } };
        },
        5 => {
            const label_len = try reader.readInt(u32, .little);
            const label = try allocator.alloc(u8, label_len);
            _ = try reader.readAll(label);
            return HIRInstruction{ .Jump = .{ .label = label, .vm_offset = 0 } };
        },
        6 => {
            const true_len = try reader.readInt(u32, .little);
            const label_true = try allocator.alloc(u8, true_len);
            _ = try reader.readAll(label_true);

            const false_len = try reader.readInt(u32, .little);
            const label_false = try allocator.alloc(u8, false_len);
            _ = try reader.readAll(label_false);

            return HIRInstruction{ .JumpCond = .{ .label_true = label_true, .label_false = label_false, .vm_offset = 0, .condition_type = .Tetra } };
        },
        7 => {
            const function_index = try reader.readInt(u32, .little);
            const arg_count = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const qualified_name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(qualified_name);
            const call_kind_byte = try reader.readByte();
            const call_kind = @as(CallKind, @enumFromInt(call_kind_byte));
            return HIRInstruction{ .Call = .{
                .function_index = function_index,
                .qualified_name = qualified_name,
                .arg_count = arg_count,
                .call_kind = call_kind,
                .target_module = null,
                .return_type = .Auto,
            } };
        },
        8 => {
            const has_value = (try reader.readByte()) != 0;
            return HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Nothing } };
        },
        9 => HIRInstruction.Dup,
        10 => HIRInstruction.Pop,
        11 => {
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .Label = .{ .name = name, .vm_address = 0 } };
        },
        12 => HIRInstruction.Halt,
        13 => {
            const has_name = (try reader.readByte()) != 0;
            const name = if (has_name) blk: {
                const name_len = try reader.readInt(u32, .little);
                const name_str = try allocator.alloc(u8, name_len);
                _ = try reader.readAll(name_str);
                break :blk name_str;
            } else null;

            const value_type_byte = try reader.readByte();
            const value_type = @as(HIRType, @enumFromInt(value_type_byte));

            const has_location = (try reader.readByte()) != 0;
            const location = if (has_location) blk: {
                const file_len = try reader.readInt(u32, .little);
                const file = try allocator.alloc(u8, file_len);
                _ = try reader.readAll(file);
                const line = try reader.readInt(u32, .little);
                const column = try reader.readInt(u32, .little);
                break :blk Reporting.Reporter.Location{
                    .file = file,
                    .line = line,
                    .column = column,
                };
            } else null;

            const has_union = (try reader.readByte()) != 0;
            var union_members: ?[][]const u8 = null;
            if (has_union) {
                const count = try reader.readInt(u32, .little);
                const arr = try allocator.alloc([]const u8, count);
                for (arr) |*slot| {
                    const slen = try reader.readInt(u32, .little);
                    const s = try allocator.alloc(u8, slen);
                    _ = try reader.readAll(s);
                    slot.* = s;
                }
                union_members = arr;
            }

            return HIRInstruction{ .Peek = .{ .name = name, .value_type = value_type, .location = location, .union_members = union_members } };
        },

        14 => {
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));
            const size = try reader.readInt(u32, .little);
            return HIRInstruction{ .ArrayNew = .{ .element_type = element_type, .size = size } };
        },
        15 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGet = .{ .bounds_check = bounds_check } };
        },
        16 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArraySet = .{ .bounds_check = bounds_check } };
        },
        17 => {
            const resize_behavior_byte = try reader.readByte();
            const resize_behavior = @as(ResizeBehavior, @enumFromInt(resize_behavior_byte));
            return HIRInstruction{ .ArrayPush = .{ .resize_behavior = resize_behavior } };
        },
        18 => HIRInstruction.ArrayPop,
        19 => HIRInstruction.ArrayLen,
        20 => HIRInstruction.ArrayConcat,

        21 => {
            const scope_id = try reader.readInt(u32, .little);
            const var_count = try reader.readInt(u32, .little);
            return HIRInstruction{ .EnterScope = .{ .scope_id = scope_id, .var_count = var_count } };
        },
        22 => {
            const scope_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .ExitScope = .{ .scope_id = scope_id } };
        },

        26 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndAdd = .{ .bounds_check = bounds_check } };
        },
        27 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndSub = .{ .bounds_check = bounds_check } };
        },
        28 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndMul = .{ .bounds_check = bounds_check } };
        },
        29 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndDiv = .{ .bounds_check = bounds_check } };
        },
        30 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndMod = .{ .bounds_check = bounds_check } };
        },
        31 => {
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGetAndPow = .{ .bounds_check = bounds_check } };
        },

        else => {
            return ErrorList.UnsupportedStatement;
        },
    };
}

fn writeHIRValueText(writer: anytype, value: HIRValue) !void {
    switch (value) {
        .int => |i| try writer.print("int {}", .{i}),
        .float => |f| try writer.print("float {d}", .{f}),
        .string => |s| {
            try writer.print("string \"", .{});
            for (s) |c| {
                switch (c) {
                    '\n' => try writer.print("\\n", .{}),
                    '\t' => try writer.print("\\t", .{}),
                    '"' => try writer.print("\\\"", .{}),
                    '\\' => try writer.print("\\\\", .{}),
                    else => try writer.writeByte(c),
                }
            }
            try writer.print("\"", .{});
        },
        .tetra => |t| try writer.print("tetra {}", .{t}),
        .byte => |u| try writer.print("byte {}", .{u}),
        .nothing => try writer.print("nothing", .{}),
        .array => |arr| try writer.print("array[{s}] capacity:{}", .{ @tagName(arr.element_type), arr.capacity }),
        .struct_instance => try writer.print("struct", .{}),
        .map => try writer.print("map", .{}),
        .enum_variant => |variant| try writer.print("enum {s}.{s} (idx {})", .{ variant.type_name, variant.variant_name, variant.variant_index }),
        .storage_id_ref => |storage_id| try writer.print("storage_id_ref {}", .{storage_id}),
    }
}

fn writeHIRInstructionText(writer: anytype, instruction: HIRInstruction) !void {
    switch (instruction) {
        .Const => |c| try writer.print("    Const {}                    ; Push constant\n", .{c.constant_id}),

        .LoadVar => |v| try writer.print("    LoadVar {} \"{s}\" {s}           ; Load variable\n", .{ v.var_index, v.var_name, @tagName(v.scope_kind) }),

        .StoreVar => |v| try writer.print("    StoreVar {} \"{s}\" {s}          ; Store variable\n", .{ v.var_index, v.var_name, @tagName(v.scope_kind) }),
        .StoreConst => |v| try writer.print("    StoreConst {} \"{s}\" {s}        ; Store constant\n", .{ v.var_index, v.var_name, @tagName(v.scope_kind) }),
        .PushStorageId => |p| try writer.print("    PushStorageId {} \"{s}\"     ; Push storage ID for alias\n", .{ p.var_index, p.var_name }),
        .StoreParamAlias => |s| try writer.print("    StoreParamAlias \"{s}\" {}      ; Store alias parameter\n", .{ s.param_name, s.var_index }),

        .Arith => |a| {
            const op_name = switch (a.op) {
                .Add => "Add",
                .Sub => "Sub",
                .Mul => "Mul",
                .Div => "Div",
                .Mod => "Mod",
                .Pow => "Pow",
            };
            try writer.print("    Arith {s} {s}                ; {s} arithmetic\n", .{ op_name, @tagName(a.operand_type), @tagName(a.operand_type) });
        },

        .Convert => |c| try writer.print("    Convert {s} {s}            ; Type conversion\n", .{ @tagName(c.from_type), @tagName(c.to_type) }),

        .Compare => |c| try writer.print("    Compare {s} {s}                 ; Comparison\n", .{ @tagName(c.op), @tagName(c.operand_type) }),

        .Jump => |j| try writer.print("    Jump {s}                    ; Unconditional jump\n", .{j.label}),

        .JumpCond => |j| try writer.print("    JumpCond {s} {s}            ; Conditional jump\n", .{ j.label_true, j.label_false }),

        .Call => |c| try writer.print("    Call {} {} \"{s}\" {s}      ; Function call\n", .{ c.function_index, c.arg_count, c.qualified_name, @tagName(c.call_kind) }),
        .TailCall => |c| try writer.print("    TailCall {} {} \"{s}\" {s}      ; Tail call optimization\n", .{ c.function_index, c.arg_count, c.qualified_name, @tagName(c.call_kind) }),

        .Return => |r| try writer.print("    Return {}                   ; Return from function\n", .{r.has_value}),

        .Label => |l| try writer.print("{s}:                            ; Label\n", .{l.name}),

        .Dup => try writer.print("    Dup                         ; Duplicate top value\n", .{}),

        .Pop => try writer.print("    Pop                         ; Remove top value\n", .{}),
        .Swap => try writer.print("    Swap                        ; Swap top two values\n", .{}),

        .Peek => |i| {
            if (i.name) |name| {
                if (i.location) |location| {
                    try writer.print("    Peek \"{s}\" {s} @{s}:{}:{}", .{ name, @tagName(i.value_type), location.file, location.range.start_line, location.range.start_col });
                } else {
                    try writer.print("    Peek \"{s}\" {s}", .{ name, @tagName(i.value_type) });
                }
            } else {
                if (i.location) |location| {
                    try writer.print("    Peek {s} @{s}:{}:{}", .{ @tagName(i.value_type), location.file, location.range.start_line, location.range.start_col });
                } else {
                    try writer.print("    Peek {s}", .{@tagName(i.value_type)});
                }
            }
            if (i.union_members) |members| {
                try writer.print(" ; union [", .{});
                for (members, 0..) |m, idx| {
                    try writer.print("{s}", .{m});
                    if (idx < members.len - 1) try writer.writeByte(',');
                }
                try writer.writeByte(']');
            }
            try writer.print("         ; Debug print\n", .{});
        },

        .PeekStruct => |i| {
            try writer.print("    PeekStruct \"{s}\" {} [", .{ i.type_name, i.field_count });
            for (i.field_names, 0..) |name, idx| {
                try writer.print("\"{s}\"", .{name});
                if (idx < i.field_names.len - 1) try writer.writeByte(',');
            }
            try writer.writeAll("] [");
            for (i.field_types, 0..) |type_info, idx| {
                try writer.print("{s}", .{@tagName(type_info)});
                if (idx < i.field_types.len - 1) try writer.writeByte(',');
            }
            try writer.writeAll("]");
            if (i.location) |loc| {
                try writer.print(" @{s}:{d}:{d}", .{ loc.file, loc.range.start_line, loc.range.start_col });
            }
            try writer.writeAll("         ; Peek struct\n");
        },

        .Print => {
            try writer.print("    Print         ; Print value\n", .{});
        },

        .PrintInterpolated => |i| {
            try writer.print("    PrintInterpolated {} {} {} [", .{ i.format_parts.len, i.placeholder_indices.len, i.argument_count });
            for (i.format_part_ids, 0..) |id, idx| {
                try writer.print("{}", .{id});
                if (idx < i.format_part_ids.len - 1) try writer.writeByte(',');
            }
            try writer.print("]        ; Interpolated print\n", .{});
        },

        .AssertFail => |a| {
            try writer.print("    AssertFail @{s}:{}:{}{s}        ; Assertion failure\n", .{ a.location.file, a.location.range.start_line, a.location.range.start_col, if (a.has_message) " with message" else "" });
        },

        .Halt => try writer.print("    Halt                        ; Program termination\n", .{}),

        .ArrayNew => |a| try writer.print("    ArrayNew {s} {}             ; Create array\n", .{ @tagName(a.element_type), a.size }),
        .ArrayGet => |a| try writer.print("    ArrayGet {}                 ; Get array element\n", .{a.bounds_check}),
        .ArrayGetAndAdd => |a| try writer.print("    ArrayGetAndAdd {}           ; Compound assignment: array[index] += value\n", .{a.bounds_check}),
        .ArrayGetAndSub => |a| try writer.print("    ArrayGetAndSub {}           ; Compound assignment: array[index] -= value\n", .{a.bounds_check}),
        .ArrayGetAndMul => |a| try writer.print("    ArrayGetAndMul {}           ; Compound assignment: array[index] *= value\n", .{a.bounds_check}),
        .ArrayGetAndDiv => |a| try writer.print("    ArrayGetAndDiv {}           ; Compound assignment: array[index] /= value\n", .{a.bounds_check}),
        .ArrayGetAndMod => |a| try writer.print("    ArrayGetAndMod {}           ; Compound assignment: array[index] %= value\n", .{a.bounds_check}),
        .ArrayGetAndPow => |a| try writer.print("    ArrayGetAndPow {}           ; Compound assignment: array[index] **= value\n", .{a.bounds_check}),
        .ArraySet => |a| try writer.print("    ArraySet {}                 ; Set array element\n", .{a.bounds_check}),
        .ArrayPush => |a| try writer.print("    ArrayPush {s}               ; Push to array\n", .{@tagName(a.resize_behavior)}),
        .ArrayPop => try writer.print("    ArrayPop                    ; Pop from array\n", .{}),
        .ArrayInsert => try writer.print("    ArrayInsert                 ; Insert into array\n", .{}),
        .ArrayRemove => try writer.print("    ArrayRemove                 ; Remove from array\n", .{}),
        .ArraySlice => try writer.print("    ArraySlice                  ; Slice array/string\n", .{}),
        .ArrayLen => try writer.print("    ArrayLen                    ; Get array length\n", .{}),
        .ArrayConcat => try writer.print("    ArrayConcat                 ; Concatenate arrays\n", .{}),

        .Map => |m| try writer.print("    Map {} {s}                   ; Create map with {} entries\n", .{ m.entries.len, @tagName(m.key_type), m.entries.len }),
        .MapGet => |m| try writer.print("    MapGet {s}                   ; Get map value by key\n", .{@tagName(m.key_type)}),
        .MapSet => |m| try writer.print("    MapSet {s}                   ; Set map value by key\n", .{@tagName(m.key_type)}),

        .StructNew => |s| try writer.print("    StructNew \"{s}\" {}          ; Create struct with {} fields\n", .{ s.type_name, s.field_count, s.field_count }),
        .GetField => |f| try writer.print("    GetField \"{s}\"               ; Get field from struct\n", .{f.field_name}),
        .SetField => |f| try writer.print("    SetField \"{s}\" {s} {}        ; Set field in struct\n", .{ f.field_name, @tagName(f.container_type), f.field_index }),
        .StoreFieldName => |s| try writer.print("    StoreFieldName \"{s}\"          ; Store field name for struct\n", .{s.field_name}),

        .EnterScope => |s| try writer.print("    EnterScope {} {}            ; Enter new scope\n", .{ s.scope_id, s.var_count }),
        .ExitScope => |s| try writer.print("    ExitScope {}                ; Exit scope\n", .{s.scope_id}),

        .StringOp => |s| {
            const op_name = switch (s.op) {
                .Length => "Length",
                .Substring => "Substring",
                .Concat => "Concat",
                .ToInt => "ToInt",
                .ToFloat => "ToFloat",
                .ToByte => "ToByte",
                .ToString => "ToString",
                .Pop => "Pop",
            };
            try writer.print("    StringOp {s}                 ; String operation\n", .{op_name});
        },

        .LogicalOp => |l| {
            const op_name = switch (l.op) {
                .And => "And",
                .Or => "Or",
                .Not => "Not",
                .Iff => "Iff",
                .Xor => "Xor",
                .Nand => "Nand",
                .Nor => "Nor",
                .Implies => "Implies",
            };
            try writer.print("    LogicalOp {s}                ; Logical operation\n", .{op_name});
        },

        .TypeCheck => |tc| try writer.print("    TypeCheck \"{s}\"              ; Type check\n", .{tc.target_type}),

        .Range => |r| try writer.print("    Range {s}                    ; Create array from range\n", .{@tagName(r.element_type)}),

        .LoadAlias => |la| try writer.print("    LoadAlias {} \"{s}\"           ; Load from alias parameter\n", .{ la.slot_index, la.var_name }),
        .StoreAlias => |sa| try writer.print("    StoreAlias {} \"{s}\" {s}        ; Store to alias parameter\n", .{ sa.slot_index, sa.var_name, @tagName(sa.expected_type) }),

        else => try writer.print("    ; TODO: {s}\n", .{@tagName(instruction)}),
    }
}
