const std = @import("std");
const reporting = @import("../../utils/reporting.zig");
const Reporting = reporting;
const Location = Reporting.Location;
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
const constants = @import("../../common/constants.zig");

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

/// Computes a cache key for source file validation
/// TODO: this might be better handled earlier in the pipeline
/// possibly even in some sort of precompiler build step
fn computeCacheKey(source_path: []const u8, allocator: std.mem.Allocator) ![]u8 {
    // Read source file content
    const source_content = try std.fs.cwd().readFileAlloc(allocator, source_path, constants.MAX_SOURCE_FILE_BYTES);
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
    return std.fmt.allocPrint(allocator, "{x}", .{hash[0..8]});
}

pub fn validateSoxaCache(soxa_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !bool {
    const file = std.fs.cwd().openFile(soxa_path, .{}) catch return false;
    defer file.close();

    var reader_buffer: [512]u8 = undefined;
    var file_reader = file.reader(&reader_buffer);
    const reader = &file_reader.interface;

    // Skip first line ("; SOXA HIR v0.1.0")
    _ = reader.takeDelimiterExclusive('\n') catch return false;

    // Read second line looking for Source-Hash
    const line = reader.takeDelimiterExclusive('\n') catch return false;
    if (std.mem.indexOf(u8, line, "Source-Hash: ")) |start| {
        const cached_hash = std.mem.trim(u8, line[start + 13 ..], " \n\r");
        const current_hash = try computeCacheKey(source_path, allocator);
        defer allocator.free(current_hash);

        const is_valid = std.mem.eql(u8, cached_hash, current_hash);
        return is_valid;
    }

    return false; // No hash found = old format = invalid
}

pub fn writeSoxaFile(program: *const HIRProgram, file_path: []const u8, source_path: []const u8, allocator: std.mem.Allocator) !void {
    const file = std.fs.cwd().createFile(file_path, .{}) catch |err| switch (err) {
        error.AccessDenied => return ErrorList.AccessDenied,
        error.FileNotFound => return ErrorList.FileNotFound,
        else => return err,
    };
    defer file.close();

    var writer_buffer: [1024]u8 = undefined;
    var file_writer = file.writer(&writer_buffer);
    const writer = &file_writer.interface;

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

    // Write instructions section
    try writer.print(".code\n", .{});
    for (program.instructions) |instruction| {
        try writeHIRInstructionText(writer, instruction);
    }

    // Flush the writer to ensure all data is written
    try writer.flush();
}

pub fn readSoxaFile(file_path: []const u8, allocator: std.mem.Allocator) !HIRProgram {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| switch (err) {
        error.FileNotFound => return ErrorList.FileNotFound,
        error.AccessDenied => return ErrorList.AccessDenied,
        else => return err,
    };
    defer file.close();

    const source = try file.readToEndAlloc(allocator, constants.MAX_SOURCE_FILE_BYTES);
    defer allocator.free(source);

    var parser = SoxaTextParser.init(allocator, source);
    const program = try parser.parse();
    parser.deinit();

    return program;
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
        .enum_variant => |variant| try writer.print("enum {s}.{s} (idx {})", .{ variant.type_name, variant.variant_name, variant.variant_index }),
        .group_instance => |group| try writer.print("group {s} member#{}", .{ group.type_name, group.member_index }),
        .storage_id_ref => |storage_id| try writer.print("storage_id_ref {}", .{storage_id}),
    }
}

fn writeHIRInstructionText(writer: anytype, instruction: HIRInstruction) !void {
    switch (instruction) {
        .Const => |c| try writer.print("    Const {}                    ; Push constant\n", .{c.constant_id}),

        .LoadVar => |v| try writer.print("    LoadVar {} \"{s}\" {s}           ; Load variable\n", .{ v.var_index, v.var_name, @tagName(v.scope_kind) }),

        .StoreVar => |v| try writer.print("    StoreVar {} \"{s}\" {s}          ; Store variable\n", .{ v.var_index, v.var_name, @tagName(v.scope_kind) }),
        .StoreDecl => |d| try writer.print("    StoreDecl {} \"{s}\" {s} {s} {s} ; Store declaration\n", .{ d.var_index, d.var_name, @tagName(d.scope_kind), @tagName(d.declared_type), if (d.is_const) "true" else "false" }),
        .PushStorageId => |p| try writer.print("    PushStorageId {} \"{s}\"     ; Push storage ID for alias\n", .{ p.var_index, p.var_name }),
        .BindAlias => |s| try writer.print("    BindAlias \"{s}\" {}      ; Bind alias parameter\n", .{ s.alias_name, s.alias_slot }),

        .Arith => |a| {
            const op_name = switch (a.op) {
                .Add => "Add",
                .Sub => "Sub",
                .Mul => "Mul",
                .Div => "Div",
                .IntDiv => "IntDiv",
                .Mod => "Mod",
                .Pow => "Pow",
            };
            try writer.print("    Arith {s} {s}                ; {s} arithmetic\n", .{ op_name, @tagName(a.operand_type), @tagName(a.operand_type) });
        },

        .Convert => |c| try writer.print("    Convert {s} {s}            ; Type conversion\n", .{ @tagName(c.from_type), @tagName(c.to_type) }),

        .Compare => |c| try writer.print("    Compare {s} {s}                 ; Comparison\n", .{ @tagName(c.op), @tagName(c.operand_type) }),

        .Jump => |j| try writer.print("    Jump {s}                    ; Unconditional jump\n", .{j.label}),

        .JumpCond => |j| try writer.print("    JumpCond {s} {s}            ; Conditional jump\n", .{ j.label_true, j.label_false }),

        .Call => |c| try writer.print("    Call {} {} \"{s}\" {s} tail={}     ; Function call\n", .{ c.function_index, c.arg_count, c.qualified_name, @tagName(c.call_kind), c.tail }),

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

        .AssertFail => |a| {
            try writer.print("    AssertFail @{s}:{}:{}{s}        ; Assertion failure\n", .{ a.location.file, a.location.range.start_line, a.location.range.start_col, if (a.has_message) " with message" else "" });
        },
        .Unreachable => |u| {
            try writer.print("    Unreachable @{s}:{}:{}         ; Reached unreachable code\n", .{ u.location.file, u.location.range.start_line, u.location.range.start_col });
        },

        .Halt => try writer.print("    Halt                        ; Program termination\n", .{}),

        // Array operations
        .ArrayNew => |a| try writer.print(
            "    ArrayNew {s} {} {s}             ; Create array\n",
            .{ @tagName(a.element_type), a.size, @tagName(a.storage_kind) },
        ),
        .ArrayGet => |a| try writer.print("    ArrayGet {}                 ; Get array element\n", .{a.bounds_check}),
        .ArrayCompoundAssign => |a| try writer.print("    ArrayCompoundAssign {} {s}   ; Compound array assignment\n", .{ a.bounds_check, @tagName(a.op) }),
        .ArraySet => |a| try writer.print("    ArraySet {}                 ; Set array element\n", .{a.bounds_check}),
        .ArrayPush => |a| try writer.print("    ArrayPush {s}               ; Push to array\n", .{@tagName(a.resize_behavior)}),
        .ArrayPop => try writer.print("    ArrayPop                    ; Pop from array\n", .{}),
        .ArrayInsert => try writer.print("    ArrayInsert                 ; Insert into array\n", .{}),
        .ArrayRemove => try writer.print("    ArrayRemove                 ; Remove from array\n", .{}),
        .ArraySlice => try writer.print("    ArraySlice                  ; Slice array/string\n", .{}),
        .ArrayLen => try writer.print("    ArrayLen                    ; Get array length\n", .{}),
        .ArrayConcat => try writer.print("    ArrayConcat                 ; Concatenate arrays\n", .{}),

        .Map => |m| try writer.print(
            "    Map {} {s} {s} else:{s}       ; Create map with {} entries\n",
            .{ m.entries.len, @tagName(m.key_type), @tagName(m.value_type), if (m.has_else_value) "true" else "false", m.entries.len },
        ),
        .MapGet => |m| try writer.print("    MapGet {s} {s}              ; Get map value by key\n", .{ @tagName(m.key_type), @tagName(m.value_type) }),
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
                .Substring => "Substring",
                .Concat => "Concat",
                .ToInt => "ToInt",
                .ToFloat => "ToFloat",
                .ToByte => "ToByte",
                .ToString => "ToString",
                .Pop => "Pop",
                .Pack => "Pack",
                .Unpack => "Unpack",
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
        .GroupCheck => |gc| try writer.print("    GroupCheck member_index:{}   ; Group member check\n", .{gc.member_index}),
        .GroupExtractPayload => try writer.print("    GroupExtractPayload          ; Extract payload from group\n", .{}),
        .UnionConstruct => |uc| try writer.print("    UnionConstruct member:{d}   ; union value construct\n", .{uc.member_index}),
        .LoadAlias => |la| try writer.print("    LoadAlias {} \"{s}\"           ; Load from alias parameter\n", .{ la.slot_index, la.var_name }),
        .StoreAlias => |sa| try writer.print("    StoreAlias {} \"{s}\" {s}        ; Store to alias parameter\n", .{ sa.slot_index, sa.var_name, @tagName(sa.expected_type) }),
        .LoadModule => |lm| try writer.print("    LoadModule \"{s}\"              ; Load module as struct\n", .{lm.module_name}),
    }
}
