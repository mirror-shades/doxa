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

pub fn translateToVMBytecode(program: *HIRProgram, allocator: std.mem.Allocator, reporter: *reporting.Reporter) ![]u8 {
    var bytecode = std.ArrayList(u8).init(allocator);
    defer bytecode.deinit();

    // First pass: resolve labels to addresses
    var label_addresses = std.StringHashMap(u32).init(allocator);
    defer label_addresses.deinit();

    var address: u32 = 0;
    for (program.instructions) |instruction| {
        switch (instruction) {
            .Label => |label| {
                try label_addresses.put(label.name, address);
                // Labels don't generate bytecode
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
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_CONST)); // Use OP_CONST for now
                try bytecode.append(@intCast(p.var_index));
            },
            .StoreParamAlias => |_| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_SET_VAR)); // Use OP_SET_VAR for now
                try bytecode.append(0); // Placeholder index
            },
            .IntArith => |a| {
                const opcode = switch (a.op) {
                    .Add => instructions.OpCode.OP_IADD,
                    .Sub => instructions.OpCode.OP_ISUB,
                    .Mul => instructions.OpCode.OP_IMUL,
                    .Div => unreachable, // Division never maps to IntArith
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
                // For peek, we can use existing VM peekion mechanism
                // The VM will handle the printing based on the top stack value
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_DUP)); // Keep value on stack
                // Note: Your VM's peek handling is in the main execution loop
            },
            .AssertFail => |_| {
                // Generate proper AssertFail bytecode
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_ASSERT_FAIL));
            },
            .Halt => {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_HALT));
            },
            .Label => {
                // Labels don't generate bytecode - already handled in first pass
            },
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
        .Const, .LoadVar, .StoreVar, .StoreConst, .Jump, .JumpCond, .Call, .TailCall => 2, // opcode + operand
        .IntArith, .FloatArith, .Convert, .Compare, .Return, .Dup, .Pop, .Swap, .Peek, .Halt, .AssertFail => 1, // opcode only
        .Label => 0, // No bytecode generated
        else => 1, // Default to 1 byte
    };
}

pub fn convertHIRConstants(hir_constants: []HIRValue, allocator: std.mem.Allocator) ![]instructions.Value {
    var vm_constants = std.ArrayList(instructions.Value).init(allocator);
    defer vm_constants.deinit();

    for (hir_constants) |hir_const| {
        const vm_value = switch (hir_const) {
            .int => |i| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = i } },
            .float => |f| instructions.Value{ .type = .FLOAT, .nothing = false, .data = .{ .float = f } },
            .string => |s| instructions.Value{ .type = .STRING, .nothing = false, .data = .{ .string = s } },
            .tetra => |t| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, t) } },
            .byte => |u| instructions.Value{ .type = .INT, .nothing = false, .data = .{ .int = @as(i32, u) } }, // Convert u8 to int
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
    reserved: [6]u8 = [_]u8{0} ** 6, // For future expansion
};

/// Computes a cache key for source file validation
/// TODO: this might be better handled earlier in the pipeline
/// possibly even in some sort of precompiler build step
fn computeCacheKey(source_path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    // Read source file content
    const source_content = try std.fs.cwd().readFileAlloc(allocator, source_path, 1024 * 1024); // 1MB max
    defer allocator.free(source_content);

    // Create hasher
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(source_content);

    // Include compiler version to invalidate cache when compiler changes
    hasher.update("doxa-0.1.0-dev");

    // Include build configuration
    hasher.update("debug-mode");

    // Generate hash
    var hash: [32]u8 = undefined;
    hasher.final(&hash);

    // Return first 16 hex chars (8 bytes) as string
    return std.fmt.allocPrint(allocator, "{}", .{std.fmt.fmtSliceHexLower(hash[0..8])});
}

pub fn validateSoxaCache(soxa_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !bool {
    const file = std.fs.cwd().openFile(soxa_path, .{}) catch return false;
    defer file.close();

    var reader = file.reader();
    var buf: [512]u8 = undefined;

    // Skip first line ("; SOXA HIR v0.1.0")
    _ = reader.readUntilDelimiterOrEof(&buf, '\n') catch return false;

    // Read second line looking for Source-Hash
    if (try reader.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        if (std.mem.indexOf(u8, line, "Source-Hash: ")) |start| {
            const cached_hash = std.mem.trim(u8, line[start + 13 ..], " \n\r");
            const current_hash = try computeCacheKey(source_path, allocator);
            defer allocator.free(current_hash);

            const is_valid = std.mem.eql(u8, cached_hash, current_hash);
            if (!is_valid) {}
            return is_valid;
        }
    }

    return false; // No hash found = old format = invalid
}

pub fn writeSoxaFile(program: *HIRProgram, file_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !void {
    const file = std.fs.cwd().createFile(file_path, .{}) catch |err| switch (err) {
        error.AccessDenied => return ErrorList.AccessDenied,
        error.FileNotFound => return ErrorList.FileNotFound,
        else => return err,
    };
    defer file.close();

    var writer = file.writer();

    // Compute cache key for validation
    const cache_key = try computeCacheKey(source_path, allocator);
    defer allocator.free(cache_key);

    // Write enhanced header with cache validation
    try writer.print("; SOXA HIR v0.1.0\n", .{});
    try writer.print("; Source-Hash: {s}\n", .{cache_key});
    try writer.print("; Compiler-Version: 0.1.0-dev\n", .{});
    try writer.print("; Build-Flags: debug\n", .{});
    try writer.print("; Generated with {} instructions, {} constants, {} functions\n\n", .{ program.instructions.len, program.constant_pool.len, program.function_table.len });

    // Write constants section
    if (program.constant_pool.len > 0) {
        try writer.print(".constants\n", .{});
        for (program.constant_pool, 0..) |constant, i| {
            try writer.print("    const_{}: ", .{i});
            try writeHIRValueText(writer, constant);
            try writer.print("\n", .{});
        }
        try writer.print("\n", .{});
    }

    // Write functions section
    if (program.function_table.len > 0) {
        try writer.print(".functions\n", .{});
        for (program.function_table) |func| {
            try writer.print("    {s}({} args) -> {s}\n", .{ func.name, func.arity, @tagName(func.return_type) });
            try writer.print("        entry: {s}\n", .{func.start_label});
            if (func.is_entry) {
                try writer.print("        main: true\n", .{});
            }
        }
        try writer.print("\n", .{});
    }

    // Write instructions section
    try writer.print(".code\n", .{});
    for (program.instructions) |instruction| {
        try writeHIRInstructionText(writer, instruction);
    }
}

pub fn readSoxaFile(file_path: []const u8, allocator: std.mem.Allocator) !HIRProgram {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return ErrorList.FileNotFound,
        error.AccessDenied => return ErrorList.AccessDenied,
        else => return err,
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024); // 1MB max
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
            try writer.writeByte(0); // Type tag
            try writer.writeInt(i32, i, .little);
        },
        .float => |f| {
            try writer.writeByte(1); // Type tag
            try writer.writeInt(u64, @bitCast(f), .little);
        },
        .string => |s| {
            try writer.writeByte(2); // Type tag
            try writer.writeInt(u32, @as(u32, @intCast(s.len)), .little);
            try writer.writeAll(s);
        },
        .tetra => |t| {
            try writer.writeByte(3); // Type tag
            try writer.writeByte(t);
        },
        .byte => |u| {
            try writer.writeByte(4); // Type tag
            try writer.writeByte(u);
        },
        .nothing => {
            try writer.writeByte(5); // Type tag
        },
        .array => |arr| {
            try writer.writeByte(6); // Type tag for array
            try writer.writeInt(u32, @as(u32, @intCast(arr.elements.len)), .little);
            try writer.writeInt(u32, arr.capacity, .little); // CRITICAL FIX: Save capacity!
            try writer.writeByte(@intFromEnum(arr.element_type)); // Element type
            // For now, we'll serialize basic array structure
            // Full recursive serialization would require more complex handling
        },
        .struct_instance => {
            try writer.writeByte(7); // Type tag for struct
            // TODO: Implement struct serialization when needed
        },
        .map => {
            try writer.writeByte(9); // Type tag for map
            // TODO: Implement map serialization when needed
        },
        .enum_variant => |variant| {
            try writer.writeByte(10); // Type tag for enum
            // Serialize enum variant information
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
            // Array deserialization (basic structure)
            const array_len = try reader.readInt(u32, .little);
            const capacity = try reader.readInt(u32, .little); // CRITICAL FIX: Read capacity!
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));

            // Create array with proper capacity (allocate capacity, but slice to actual length)
            const backing_memory = try allocator.alloc(HIRValue, capacity);
            for (backing_memory) |*element| {
                element.* = HIRValue.nothing;
            }
            const elements = backing_memory[0..array_len]; // Slice to actual length

            return HIRValue{ .array = HIRArray{
                .elements = elements,
                .element_type = element_type,
                .capacity = capacity,
            } };
        },
        7 => {
            // Struct instance - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper struct deserialization
        },
        8 => {
            // Map - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper map deserialization
        },
        9 => {
            // Enum variant deserialization
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
    _ = allocator; // May be needed for complex instructions

    switch (instruction) {
        .Const => |c| {
            try writer.writeByte(0); // Instruction tag
            try writer.writeInt(u32, c.constant_id, .little);
        },
        .LoadVar => |v| {
            try writer.writeByte(1); // Instruction tag
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .StoreVar => |v| {
            try writer.writeByte(2); // Instruction tag
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .StoreConst => |v| {
            try writer.writeByte(23); // Instruction tag (new)
            try writer.writeInt(u32, v.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(v.var_name.len)), .little);
            try writer.writeAll(v.var_name);
        },
        .PushStorageId => |p| {
            try writer.writeByte(24); // Instruction tag (new)
            try writer.writeInt(u32, p.var_index, .little);
            try writer.writeInt(u32, @as(u32, @intCast(p.var_name.len)), .little);
            try writer.writeAll(p.var_name);
        },
        .StoreParamAlias => |s| {
            try writer.writeByte(25); // Instruction tag (new)
            try writer.writeInt(u32, @as(u32, @intCast(s.param_name.len)), .little);
            try writer.writeAll(s.param_name);
        },
        .IntArith => |a| {
            try writer.writeByte(3); // Instruction tag
            try writer.writeByte(@intFromEnum(a.op));
        },
        .Compare => |c| {
            try writer.writeByte(4); // Instruction tag
            try writer.writeByte(@intFromEnum(c.op));
            // Serialize operand_type to preserve intended comparison semantics (Int/Float/String/Enum/...)
            try writer.writeByte(@intFromEnum(c.operand_type));
        },
        .Jump => |j| {
            try writer.writeByte(5); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(j.label.len)), .little);
            try writer.writeAll(j.label);
        },
        .JumpCond => |j| {
            try writer.writeByte(6); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(j.label_true.len)), .little);
            try writer.writeAll(j.label_true);
            try writer.writeInt(u32, @as(u32, @intCast(j.label_false.len)), .little);
            try writer.writeAll(j.label_false);
        },
        .Call => |c| {
            try writer.writeByte(7); // Instruction tag
            try writer.writeInt(u32, c.function_index, .little);
            try writer.writeInt(u32, c.arg_count, .little);
            // Serialize qualified_name
            try writer.writeInt(u32, @as(u32, @intCast(c.qualified_name.len)), .little);
            try writer.writeAll(c.qualified_name);
            // CRITICAL FIX: Save call_kind!
            try writer.writeByte(@intFromEnum(c.call_kind));
        },
        .Return => |r| {
            try writer.writeByte(8); // Instruction tag
            try writer.writeByte(if (r.has_value) 1 else 0);
        },
        .Dup => {
            try writer.writeByte(9); // Instruction tag
        },
        .Pop => {
            try writer.writeByte(10); // Instruction tag
        },
        .Label => |l| {
            try writer.writeByte(11); // Instruction tag
            try writer.writeInt(u32, @as(u32, @intCast(l.name.len)), .little);
            try writer.writeAll(l.name);
        },
        .Halt => {
            try writer.writeByte(12); // Instruction tag
        },
        .Peek => |i| {
            try writer.writeByte(13); // Instruction tag
            // Write whether name is present
            if (i.name) |name| {
                try writer.writeByte(1); // Has name
                try writer.writeInt(u32, @as(u32, @intCast(name.len)), .little);
                try writer.writeAll(name);
            } else {
                try writer.writeByte(0); // No name
            }
            try writer.writeByte(@intFromEnum(i.value_type));
            // Write whether location is present
            if (i.location) |location| {
                try writer.writeByte(1); // Has location
                try writer.writeInt(u32, @as(u32, @intCast(location.file.len)), .little);
                try writer.writeAll(location.file);
                try writer.writeInt(u32, location.line, .little);
                try writer.writeInt(u32, location.column, .little);
            } else {
                try writer.writeByte(0); // No location
            }
            // Write union members if present
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

        // Array operations (Phase 1)
        .ArrayNew => |a| {
            try writer.writeByte(14); // Instruction tag
            try writer.writeByte(@intFromEnum(a.element_type));
            try writer.writeInt(u32, a.size, .little);
        },
        .ArrayGet => |a| {
            try writer.writeByte(15); // Instruction tag
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArraySet => |a| {
            try writer.writeByte(16); // Instruction tag
            try writer.writeByte(if (a.bounds_check) 1 else 0);
        },
        .ArrayPush => |a| {
            try writer.writeByte(17); // Instruction tag
            try writer.writeByte(@intFromEnum(a.resize_behavior));
        },
        .ArrayPop => {
            try writer.writeByte(18); // Instruction tag
        },
        .ArrayLen => {
            try writer.writeByte(19); // Instruction tag
        },
        .ArrayConcat => {
            try writer.writeByte(20); // Instruction tag
        },

        // Scope management
        .EnterScope => |s| {
            try writer.writeByte(21); // Instruction tag
            try writer.writeInt(u32, s.scope_id, .little);
            try writer.writeInt(u32, s.var_count, .little);
        },
        .ExitScope => |s| {
            try writer.writeByte(22); // Instruction tag
            try writer.writeInt(u32, s.scope_id, .little);
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
        0 => { // Const
            const constant_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .Const = .{ .value = HIRValue.nothing, .constant_id = constant_id } }; // Value will be resolved from constant pool
        },
        1 => { // LoadVar
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .LoadVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null } };
        },
        2 => { // StoreVar
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .StoreVar = .{ .var_index = var_index, .var_name = name, .scope_kind = .Local, .module_context = null, .expected_type = .Auto } };
        },
        23 => { // StoreConst
            const var_index = try reader.readInt(u32, .little);
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .StoreConst = .{ .var_index = var_index, .var_name = name } };
        },
        3 => { // IntArith
            const op_byte = try reader.readByte();
            const op = @as(ArithOp, @enumFromInt(op_byte));
            return HIRInstruction{ .IntArith = .{ .op = op, .overflow_behavior = .Wrap } };
        },
        4 => { // Compare
            const op_byte = try reader.readByte();
            const op = @as(CompareOp, @enumFromInt(op_byte));
            // Read operand_type that was serialized with Compare
            const type_byte = try reader.readByte();
            const operand_type = @as(HIRType, @enumFromInt(type_byte));
            return HIRInstruction{ .Compare = .{ .op = op, .operand_type = operand_type } };
        },
        5 => { // Jump
            const label_len = try reader.readInt(u32, .little);
            const label = try allocator.alloc(u8, label_len);
            _ = try reader.readAll(label);
            return HIRInstruction{ .Jump = .{ .label = label, .vm_offset = 0 } };
        },
        6 => { // JumpCond
            const true_len = try reader.readInt(u32, .little);
            const label_true = try allocator.alloc(u8, true_len);
            _ = try reader.readAll(label_true);

            const false_len = try reader.readInt(u32, .little);
            const label_false = try allocator.alloc(u8, false_len);
            _ = try reader.readAll(label_false);

            return HIRInstruction{ .JumpCond = .{ .label_true = label_true, .label_false = label_false, .vm_offset = 0, .condition_type = .Tetra } };
        },
        7 => { // Call
            const function_index = try reader.readInt(u32, .little);
            const arg_count = try reader.readInt(u32, .little);
            // Deserialize qualified_name
            const name_len = try reader.readInt(u32, .little);
            const qualified_name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(qualified_name);
            // CRITICAL FIX: Read call_kind!
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
        8 => { // Return
            const has_value = (try reader.readByte()) != 0;
            return HIRInstruction{ .Return = .{ .has_value = has_value, .return_type = .Nothing } }; // Default to Nothing to prevent Auto leakage
        },
        9 => HIRInstruction.Dup,
        10 => HIRInstruction.Pop,
        11 => { // Label
            const name_len = try reader.readInt(u32, .little);
            const name = try allocator.alloc(u8, name_len);
            _ = try reader.readAll(name);
            return HIRInstruction{ .Label = .{ .name = name, .vm_address = 0 } };
        },
        12 => HIRInstruction.Halt,
        13 => { // Peek
            const has_name = (try reader.readByte()) != 0;
            const name = if (has_name) blk: {
                const name_len = try reader.readInt(u32, .little);
                const name_str = try allocator.alloc(u8, name_len);
                _ = try reader.readAll(name_str);
                break :blk name_str;
            } else null;

            const value_type_byte = try reader.readByte();
            const value_type = @as(HIRType, @enumFromInt(value_type_byte));

            // Read whether location is present
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

            // Read union members if present
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

        // Array operations (Phase 1)
        14 => { // ArrayNew
            const element_type_byte = try reader.readByte();
            const element_type = @as(HIRType, @enumFromInt(element_type_byte));
            const size = try reader.readInt(u32, .little);
            return HIRInstruction{ .ArrayNew = .{ .element_type = element_type, .size = size } };
        },
        15 => { // ArrayGet
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArrayGet = .{ .bounds_check = bounds_check } };
        },
        16 => { // ArraySet
            const bounds_check = (try reader.readByte()) != 0;
            return HIRInstruction{ .ArraySet = .{ .bounds_check = bounds_check } };
        },
        17 => { // ArrayPush
            const resize_behavior_byte = try reader.readByte();
            const resize_behavior = @as(ResizeBehavior, @enumFromInt(resize_behavior_byte));
            return HIRInstruction{ .ArrayPush = .{ .resize_behavior = resize_behavior } };
        },
        18 => HIRInstruction.ArrayPop,
        19 => HIRInstruction.ArrayLen,
        20 => HIRInstruction.ArrayConcat,

        // Scope management
        21 => { // EnterScope
            const scope_id = try reader.readInt(u32, .little);
            const var_count = try reader.readInt(u32, .little);
            return HIRInstruction{ .EnterScope = .{ .scope_id = scope_id, .var_count = var_count } };
        },
        22 => { // ExitScope
            const scope_id = try reader.readInt(u32, .little);
            return HIRInstruction{ .ExitScope = .{ .scope_id = scope_id } };
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
            // Escape special characters when writing to HIR text format
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
        .enum_variant => |variant| try writer.print("enum_variant {s}.{s}", .{ variant.type_name, variant.variant_name }),
        .storage_id_ref => |storage_id| try writer.print("storage_id_ref {}", .{storage_id}),
    }
}

fn writeHIRInstructionText(writer: anytype, instruction: HIRInstruction) !void {
    switch (instruction) {
        .Const => |c| try writer.print("    Const {}                    ; Push constant\n", .{c.constant_id}),

        .LoadVar => |v| try writer.print("    LoadVar {} \"{s}\"           ; Load variable\n", .{ v.var_index, v.var_name }),

        .StoreVar => |v| try writer.print("    StoreVar {} \"{s}\"          ; Store variable\n", .{ v.var_index, v.var_name }),
        .StoreConst => |v| try writer.print("    StoreConst {} \"{s}\"        ; Store constant\n", .{ v.var_index, v.var_name }),
        .PushStorageId => |p| try writer.print("    PushStorageId {} \"{s}\"     ; Push storage ID for alias\n", .{ p.var_index, p.var_name }),
        .StoreParamAlias => |s| try writer.print("    StoreParamAlias \"{s}\"      ; Store alias parameter\n", .{s.param_name}),

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
            // Optionally include union member list in text form for debugging
            if (i.union_members) |members| {
                // Print as ; union [a,b,c]
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

        // Array operations
        .ArrayNew => |a| try writer.print("    ArrayNew {s} {}             ; Create array\n", .{ @tagName(a.element_type), a.size }),
        .ArrayGet => |a| try writer.print("    ArrayGet {}                 ; Get array element\n", .{a.bounds_check}),
        .ArraySet => |a| try writer.print("    ArraySet {}                 ; Set array element\n", .{a.bounds_check}),
        .ArrayPush => |a| try writer.print("    ArrayPush {s}               ; Push to array\n", .{@tagName(a.resize_behavior)}),
        .ArrayPop => try writer.print("    ArrayPop                    ; Pop from array\n", .{}),
        .ArrayLen => try writer.print("    ArrayLen                    ; Get array length\n", .{}),
        .ArrayConcat => try writer.print("    ArrayConcat                 ; Concatenate arrays\n", .{}),

        .Map => |m| try writer.print("    Map {} {s}                   ; Create map with {} entries\n", .{ m.entries.len, @tagName(m.key_type), m.entries.len }),
        .MapGet => |m| try writer.print("    MapGet {s}                   ; Get map value by key\n", .{@tagName(m.key_type)}),
        .MapSet => |m| try writer.print("    MapSet {s}                   ; Set map value by key\n", .{@tagName(m.key_type)}),

        // Struct operations
        .StructNew => |s| try writer.print("    StructNew \"{s}\" {}          ; Create struct with {} fields\n", .{ s.type_name, s.field_count, s.field_count }),
        .GetField => |f| try writer.print("    GetField \"{s}\"               ; Get field from struct\n", .{f.field_name}),
        .SetField => |f| try writer.print("    SetField \"{s}\" {s} {}        ; Set field in struct\n", .{ f.field_name, @tagName(f.container_type), f.field_index }),
        .StoreFieldName => |s| try writer.print("    StoreFieldName \"{s}\"          ; Store field name for struct\n", .{s.field_name}),

        // Scope management
        .EnterScope => |s| try writer.print("    EnterScope {} {}            ; Enter new scope\n", .{ s.scope_id, s.var_count }),
        .ExitScope => |s| try writer.print("    ExitScope {}                ; Exit scope\n", .{s.scope_id}),

        .StringOp => |s| {
            const op_name = switch (s.op) {
                .Length => "Length",
                .Bytes => "Bytes",
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

        else => try writer.print("    ; TODO: {s}\n", .{@tagName(instruction)}),
    }
}
