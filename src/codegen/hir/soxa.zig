const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../lexer/token.zig").Token;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const instructions = @import("../../interpreter/instructions.zig");
const reporting = @import("../../utils/reporting.zig");
const Reporting = reporting;
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
            .IntArith => |a| {
                const opcode = switch (a.op) {
                    .Add => instructions.OpCode.OP_IADD,
                    .Sub => instructions.OpCode.OP_ISUB,
                    .Mul => instructions.OpCode.OP_IMUL,
                    .Div => instructions.OpCode.OP_IADD, // TODO: Add OP_IDIV to your VM
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
                    return reporting.ErrorList.UndefinedVariable;
                };
                const current_addr = @as(u32, @intCast(bytecode.items.len - 1));
                const offset = target_addr - current_addr;
                try bytecode.append(@intCast(offset));
            },
            .JumpCond => |j| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_JUMP_IF_FALSE));
                const target_addr = label_addresses.get(j.label_false) orelse {
                    reporter.reportError("Undefined label: {s}", .{j.label_false});
                    return reporting.ErrorList.UndefinedVariable;
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
            .TupleGet => |t| {
                try bytecode.append(@intFromEnum(instructions.OpCode.OP_ARRAY_GET));
                try bytecode.append(@intCast(t.index));
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
                return reporting.ErrorList.UnsupportedStatement;
            },
        }
    }

    const result = try bytecode.toOwnedSlice();
    return result;
}

fn getBytecodeSize(instruction: HIRInstruction) u32 {
    return switch (instruction) {
        .Const, .LoadVar, .StoreVar, .Jump, .JumpCond, .Call, .TailCall => 2, // opcode + operand
        .TupleGet => 2, // opcode + index
        .IntArith, .Compare, .Return, .Dup, .Pop, .Swap, .Peek, .Halt, .AssertFail => 1, // opcode only
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
        error.AccessDenied => return reporting.ErrorList.AccessDenied,
        error.FileNotFound => return reporting.ErrorList.FileNotFound,
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
        error.FileNotFound => return reporting.ErrorList.FileNotFound,
        error.AccessDenied => return reporting.ErrorList.AccessDenied,
        else => return err,
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, 1024 * 1024); // 1MB max
    defer allocator.free(source);

    var parser = SoxaTextParser.init(allocator, source);
    const program = try parser.parse();

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
        .tuple => {
            try writer.writeByte(8); // Type tag for tuple
            // TODO: Implement tuple serialization when needed
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
            // Tuple - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper tuple deserialization
        },
        9 => {
            // Map - return placeholder for now
            return HIRValue.nothing; // TODO: Implement proper map deserialization
        },
        10 => {
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
            return reporting.ErrorList.InvalidArgument;
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
        .IntArith => |a| {
            try writer.writeByte(3); // Instruction tag
            try writer.writeByte(@intFromEnum(a.op));
        },
        .Compare => |c| {
            try writer.writeByte(4); // Instruction tag
            try writer.writeByte(@intFromEnum(c.op));
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
            return reporting.ErrorList.UnsupportedStatement;
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
        3 => { // IntArith
            const op_byte = try reader.readByte();
            const op = @as(ArithOp, @enumFromInt(op_byte));
            return HIRInstruction{ .IntArith = .{ .op = op, .overflow_behavior = .Wrap } };
        },
        4 => { // Compare
            const op_byte = try reader.readByte();
            const op = @as(CompareOp, @enumFromInt(op_byte));
            return HIRInstruction{ .Compare = .{ .op = op, .operand_type = .Int } };
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

            return HIRInstruction{ .Peek = .{ .name = name, .value_type = value_type, .location = location } };
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
            return reporting.ErrorList.UnsupportedStatement;
        },
    };
}

fn writeHIRValueText(writer: anytype, value: HIRValue) !void {
    switch (value) {
        .int => |i| try writer.print("int {}", .{i}),
        .float => |f| try writer.print("float {d}", .{f}),
        .string => |s| try writer.print("string \"{s}\"", .{s}),
        .tetra => |t| try writer.print("tetra {}", .{t}),
        .byte => |u| try writer.print("byte {}", .{u}),
        .nothing => try writer.print("nothing", .{}),
        .array => |arr| try writer.print("array[{s}] capacity:{}", .{ @tagName(arr.element_type), arr.capacity }),
        .struct_instance => try writer.print("struct", .{}),
        .tuple => try writer.print("tuple", .{}),
        .map => try writer.print("map", .{}),
        .enum_variant => |variant| try writer.print("enum_variant {s}.{s}", .{ variant.type_name, variant.variant_name }),
    }
}

fn writeHIRInstructionText(writer: anytype, instruction: HIRInstruction) !void {
    switch (instruction) {
        .Const => |c| try writer.print("    Const {}                    ; Push constant\n", .{c.constant_id}),

        .LoadVar => |v| try writer.print("    LoadVar {} \"{s}\"           ; Load variable\n", .{ v.var_index, v.var_name }),

        .StoreVar => |v| try writer.print("    StoreVar {} \"{s}\"          ; Store variable\n", .{ v.var_index, v.var_name }),

        .IntArith => |a| try writer.print("    IntArith {s}                ; Integer arithmetic\n", .{@tagName(a.op)}),

        .Compare => |c| try writer.print("    Compare {s}                 ; Comparison\n", .{@tagName(c.op)}),

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
                    try writer.print("    Peek \"{s}\" {s} @{s}:{}:{}         ; Debug print\n", .{ name, @tagName(i.value_type), location.file, location.line, location.column });
                } else {
                    try writer.print("    Peek \"{s}\" {s}         ; Debug print\n", .{ name, @tagName(i.value_type) });
                }
            } else {
                if (i.location) |location| {
                    try writer.print("    Peek {s} @{s}:{}:{}                 ; Debug print\n", .{ @tagName(i.value_type), location.file, location.line, location.column });
                } else {
                    try writer.print("    Peek {s}                 ; Debug print\n", .{@tagName(i.value_type)});
                }
            }
        },

        .AssertFail => |a| {
            try writer.print("    AssertFail @{s}:{}:{}{s}        ; Assertion failure\n", .{ a.location.file, a.location.line, a.location.column, if (a.has_message) " with message" else "" });
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

        .TupleNew => |t| try writer.print("    TupleNew {}                 ; Create tuple with {} elements\n", .{ t.element_count, t.element_count }),
        .TupleGet => |t| try writer.print("    TupleGet {}                 ; Get tuple element at index {}\n", .{ t.index, t.index }),
        .Map => |m| try writer.print("    Map {} {s}                   ; Create map with {} entries\n", .{ m.entries.len, @tagName(m.key_type), m.entries.len }),
        .MapGet => |m| try writer.print("    MapGet {s}                   ; Get map value by key\n", .{@tagName(m.key_type)}),

        // Struct operations
        .StructNew => |s| try writer.print("    StructNew \"{s}\" {}          ; Create struct with {} fields\n", .{ s.type_name, s.field_count, s.field_count }),
        .GetField => |f| try writer.print("    GetField \"{s}\"               ; Get field from struct\n", .{f.field_name}),
        .SetField => |f| try writer.print("    SetField \"{s}\" {s} {}        ; Set field in struct\n", .{ f.field_name, @tagName(f.container_type), f.field_index }),
        .StoreFieldName => |s| try writer.print("    StoreFieldName \"{s}\"          ; Store field name for struct\n", .{s.field_name}),

        // Scope management
        .EnterScope => |s| try writer.print("    EnterScope {} {}            ; Enter new scope\n", .{ s.scope_id, s.var_count }),
        .ExitScope => |s| try writer.print("    ExitScope {}                ; Exit scope\n", .{s.scope_id}),

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
                try writer.print(" @{s}:{d}:{d}", .{ loc.file, loc.line, loc.column });
            }
            try writer.writeAll("         ; Peek struct\n");
        },

        .StringOp => |s| {
            const op_name = switch (s.op) {
                .Length => "Length",
                .Bytes => "Bytes",
                .Substring => "Substring",
                .Concat => "Concat",
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

        else => try writer.print("    ; TODO: {s}\n", .{@tagName(instruction)}),
    }
}

const StructContext = struct {
    type_name: []const u8, // Changed from struct_name to type_name to match usage
    field_count: u32,
    field_names: std.ArrayList([]const u8),
    field_types: std.ArrayList(HIRType),
    field_path: std.ArrayList([]const u8), // Added to track nested field access
};

const SoxaTextParser = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    pos: usize = 0,
    line: u32 = 1,
    constants: std.ArrayList(HIRValue),
    functions: std.ArrayList(HIRProgram.HIRFunction),
    instructions: std.ArrayList(HIRInstruction),
    struct_context: ?StructContext = null,

    fn init(allocator: std.mem.Allocator, source: []const u8) SoxaTextParser {
        return SoxaTextParser{
            .allocator = allocator,
            .source = source,
            .constants = std.ArrayList(HIRValue).init(allocator),
            .functions = std.ArrayList(HIRProgram.HIRFunction).init(allocator),
            .instructions = std.ArrayList(HIRInstruction).init(allocator),
            .struct_context = null,
        };
    }

    fn deinit(self: *SoxaTextParser) void {
        if (self.struct_context) |*context| {
            context.field_names.deinit();
            context.field_types.deinit();
            context.field_path.deinit();
        }
    }

    fn parse(self: *SoxaTextParser) !HIRProgram {
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
        // Parse: "    const_0: int 42", "    const_1: string "hello"", etc.
        if (std.mem.indexOf(u8, line, "int ")) |int_pos| {
            const value_str = std.mem.trim(u8, line[int_pos + 4 ..], " \t");
            const value = try std.fmt.parseInt(i32, value_str, 10);
            try self.constants.append(HIRValue{ .int = value });
        } else if (std.mem.indexOf(u8, line, "float ")) |float_pos| {
            const value_str = std.mem.trim(u8, line[float_pos + 6 ..], " \t");
            const value = try std.fmt.parseFloat(f64, value_str);
            try self.constants.append(HIRValue{ .float = value });
        } else if (std.mem.indexOf(u8, line, "string ")) |string_pos| {
            const quoted_str = std.mem.trim(u8, line[string_pos + 7 ..], " \t");
            const value = try self.parseQuotedString(quoted_str);
            try self.constants.append(HIRValue{ .string = value });
        } else if (std.mem.indexOf(u8, line, "tetra ")) |tetra_pos| {
            const value_str = std.mem.trim(u8, line[tetra_pos + 6 ..], " \t");
            const value = try std.fmt.parseInt(u8, value_str, 10);
            try self.constants.append(HIRValue{ .tetra = value });
        } else if (std.mem.indexOf(u8, line, "byte ")) |byte_pos| {
            const value_str = std.mem.trim(u8, line[byte_pos + 5 ..], " \t");
            const value = try std.fmt.parseInt(u8, value_str, 10);
            try self.constants.append(HIRValue{ .byte = value });
        } else if (std.mem.indexOf(u8, line, "enum_variant ")) |enum_pos| {
            // Parse: "enum_variant Color.Red"
            const variant_str = std.mem.trim(u8, line[enum_pos + 13 ..], " \t");
            if (std.mem.indexOf(u8, variant_str, ".")) |dot_pos| {
                const type_name = try self.allocator.dupe(u8, variant_str[0..dot_pos]);
                const variant_name = try self.allocator.dupe(u8, variant_str[dot_pos + 1 ..]);
                try self.constants.append(HIRValue{
                    .enum_variant = HIREnum{
                        .type_name = type_name,
                        .variant_name = variant_name,
                        .variant_index = 0, // Default to 0, actual index will be resolved at runtime
                        .path = null,
                    },
                });
            } else {
                try self.constants.append(HIRValue.nothing);
            }
        } else if (std.mem.indexOf(u8, line, "nothing")) |_| {
            try self.constants.append(HIRValue.nothing);
        } else {
            // Default to nothing for unhandled types
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
        } else if (std.mem.eql(u8, op, "IntArith")) {
            const op_str = tokens.next() orelse return;
            const arith_op = if (std.mem.eql(u8, op_str, "Add")) ArithOp.Add else if (std.mem.eql(u8, op_str, "Sub")) ArithOp.Sub else if (std.mem.eql(u8, op_str, "Mul")) ArithOp.Mul else if (std.mem.eql(u8, op_str, "Div")) ArithOp.Div else if (std.mem.eql(u8, op_str, "Mod")) ArithOp.Mod else ArithOp.Add;
            try self.instructions.append(HIRInstruction{ .IntArith = .{ .op = arith_op, .overflow_behavior = .Wrap } });
        } else if (std.mem.eql(u8, op, "Compare")) {
            const op_str = tokens.next() orelse return;
            const comp_op = if (std.mem.eql(u8, op_str, "Eq")) CompareOp.Eq else if (std.mem.eql(u8, op_str, "Ne")) CompareOp.Ne else if (std.mem.eql(u8, op_str, "Lt")) CompareOp.Lt else if (std.mem.eql(u8, op_str, "Le")) CompareOp.Le else if (std.mem.eql(u8, op_str, "Gt")) CompareOp.Gt else if (std.mem.eql(u8, op_str, "Ge")) CompareOp.Ge else CompareOp.Eq;
            try self.instructions.append(HIRInstruction{ .Compare = .{ .op = comp_op, .operand_type = .Int } });
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
                value_type = if (std.mem.eql(u8, type_str, "Int")) HIRType.Int else if (std.mem.eql(u8, type_str, "Float")) HIRType.Float else if (std.mem.eql(u8, type_str, "String")) HIRType.String else if (std.mem.eql(u8, type_str, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, type_str, "Array")) HIRType.Array else if (std.mem.eql(u8, type_str, "Tuple")) HIRType.Tuple else if (std.mem.eql(u8, type_str, "Map")) HIRType.Map else if (std.mem.eql(u8, type_str, "Struct")) blk: {
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
                value_type = if (std.mem.eql(u8, name_or_type, "Int")) HIRType.Int else if (std.mem.eql(u8, name_or_type, "Float")) HIRType.Float else if (std.mem.eql(u8, name_or_type, "String")) HIRType.String else if (std.mem.eql(u8, name_or_type, "Tetra")) HIRType.Tetra else if (std.mem.eql(u8, name_or_type, "Array")) HIRType.Array else if (std.mem.eql(u8, name_or_type, "Tuple")) HIRType.Tuple else if (std.mem.eql(u8, name_or_type, "Map")) HIRType.Map else if (std.mem.eql(u8, name_or_type, "Struct")) blk: {
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
        } else if (std.mem.eql(u8, op, "TupleNew")) {
            const count_str = tokens.next() orelse return;
            const element_count = std.fmt.parseInt(u32, count_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .TupleNew = .{ .element_count = element_count } });
        } else if (std.mem.eql(u8, op, "TupleGet")) {
            const index_str = tokens.next() orelse return;
            const index = std.fmt.parseInt(u32, index_str, 10) catch return;
            try self.instructions.append(HIRInstruction{ .TupleGet = .{ .index = index } });
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
