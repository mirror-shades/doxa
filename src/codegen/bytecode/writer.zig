const std = @import("std");
const module = @import("module.zig");

pub fn writeBytecodeModuleToFile(bc: *const module.BytecodeModule, file_path: []const u8) !void {
    var file = try std.fs.cwd().createFile(file_path, .{});
    defer file.close();

    var buffer: [1024]u8 = undefined;
    var file_writer = file.writer(&buffer);
    const writer = &file_writer.interface;

    try writeHeader(bc, writer);
    try writeConsts(bc, writer);
    try writeStrings(bc, writer);
    try writeModules(bc, writer);
    try writeFunctions(bc, writer);
    try writeInstructions(bc, writer);
    try writer.flush();
}

fn writeHeader(bc: *const module.BytecodeModule, writer: anytype) !void {
    try writer.print("spec_version:{}\n", .{bc.spec_version});
    try writer.print("constants:{}\n", .{bc.constants.len});
    try writer.print("strings:{}\n", .{bc.string_pool.len});
    try writer.print("modules:{}\n", .{bc.modules.len});
    try writer.print("functions:{}\n", .{bc.functions.len});
    try writer.print("instructions:{}\n", .{bc.instructions.len});
}

fn writeConsts(bc: *const module.BytecodeModule, writer: anytype) !void {
    for (bc.constants, 0..) |c, idx| {
        try writeConstant(idx, c, writer);
    }
}

fn writeStrings(bc: *const module.BytecodeModule, writer: anytype) !void {
    for (bc.string_pool, 0..) |s, idx| {
        try writer.print("string[{d}]:{s}\n", .{ idx, s });
    }
}

fn writeModules(bc: *const module.BytecodeModule, writer: anytype) !void {
    for (bc.modules) |m| {
        try writer.print("module:{}:{s}:globals:{}\n", .{ m.id, m.name, m.global_var_count });
    }
}

fn writeFunctions(bc: *const module.BytecodeModule, writer: anytype) !void {
    for (bc.functions, 0..) |f, idx| {
        try writer.print("fn[{d}]\n", .{idx});
        try writer.print("  name:{s}\n", .{f.name});
        try writer.print("  qualified:{s}\n", .{f.qualified_name});
        try writer.print("  module_id:{}\n", .{f.module_id});
        try writer.print("  arity:{}\n", .{f.arity});
        try writer.print("  return_type:{s}\n", .{@tagName(f.return_type)});
        try writer.print("  start_ip:{}\n", .{f.start_ip});
        if (f.body_ip) |bp| try writer.print("  body_ip:{}\n", .{bp});
        try writer.print("  locals:{}\n", .{f.local_var_count});
        try writer.print("  entry:{}\n", .{f.is_entry});
        for (f.param_types, 0..) |pt, pidx| {
            try writer.print("  param[{d}]:{s} alias:{}\n", .{ pidx, @tagName(pt), f.param_is_alias[pidx] });
        }
    }
}

fn writeInstructions(bc: *const module.BytecodeModule, writer: anytype) !void {
    for (bc.instructions, 0..) |inst, idx| {
        try writeInstruction(idx, inst, writer);
    }
}

fn writeConstant(idx: usize, value: module.ConstantValue, writer: anytype) !void {
    switch (value) {
        .int => |v| try writer.print("const[{d}]: int {d}\n", .{ idx, v }),
        .byte => |v| try writer.print("const[{d}]: byte {d}\n", .{ idx, v }),
        .float => |v| try writer.print("const[{d}]: float {d}\n", .{ idx, v }),
        .string => |v| {
            try writer.print("const[{d}]: string \"", .{idx});
            try printEscapedString(writer, v);
            try writer.print("\"\n", .{});
        },
        .tetra => |v| try writer.print("const[{d}]: tetra {d}\n", .{ idx, v }),
        .nothing => try writer.print("const[{d}]: nothing\n", .{idx}),
        .storage_id_ref => |v| try writer.print("const[{d}]: storage {d}\n", .{ idx, v }),
        .array => |arr| try writer.print("const[{d}]: array len:{} elem_type:{s}\n", .{ idx, arr.elements.len, @tagName(arr.element_type) }),
        .struct_instance => |s| try writer.print("const[{d}]: struct {s} fields:{}\n", .{ idx, s.type_name, s.fields.len }),
        .map => |m| try writer.print("const[{d}]: map entries:{}\n", .{ idx, m.entries.len }),
        .enum_variant => |ev| try writer.print("const[{d}]: enum {s}.{s} (idx {})\n", .{ idx, ev.type_name, ev.variant_name, ev.variant_index }),
    }
}

fn writeInstruction(idx: usize, inst: module.Instruction, writer: anytype) !void {
    switch (inst) {
        .PushConst => |payload| try writer.print("ip[{d}] PushConst const:{}\n", .{ idx, payload.constant_index }),
        .LoadSlot => |operand| try writeSlotOperand(idx, "LoadSlot", operand, writer),
        .StoreSlot => |payload| {
            try writer.print("ip[{d}] StoreSlot type:{s}\n", .{ idx, @tagName(payload.type_tag) });
            try writeSlotOperandDetail("    target", payload.target, writer);
        },
        .StoreConstSlot => |operand| try writeSlotOperand(idx, "StoreConst", operand, writer),
        .PushStorageRef => |operand| try writeSlotOperand(idx, "PushStorageRef", operand, writer),
        .BindAlias => |payload| try writer.print("ip[{d}] BindAlias slot:{} type:{s}\n", .{ idx, payload.alias_slot, @tagName(payload.type_tag) }),
        .LoadAlias => |payload| try writer.print("ip[{d}] LoadAlias slot:{}\n", .{ idx, payload.slot_index }),
        .Arith => |payload| try writer.print("ip[{d}] Arith op:{s} type:{s}\n", .{ idx, @tagName(payload.op), @tagName(payload.type_tag) }),
        .Convert => |payload| try writer.print("ip[{d}] Convert {s}->{s}\n", .{ idx, @tagName(payload.from), @tagName(payload.to) }),
        .Compare => |payload| try writer.print("ip[{d}] Compare op:{s} type:{s}\n", .{ idx, @tagName(payload.op), @tagName(payload.type_tag) }),
        .LogicalOp => |payload| try writer.print("ip[{d}] LogicalOp op:{s}\n", .{ idx, @tagName(payload.op) }),
        .StringOp => |payload| try writer.print("ip[{d}] StringOp op:{s}\n", .{ idx, @tagName(payload.op) }),
        .TypeOf => |payload| try writer.print("ip[{d}] TypeOf value_type:{s}\n", .{ idx, @tagName(payload.value_type) }),
        .TypeCheck => |payload| try writer.print("ip[{d}] TypeCheck type:{s}\n", .{ idx, payload.type_name }),
        .Jump => |payload| try writer.print("ip[{d}] Jump label:{}\n", .{ idx, payload.label_id }),
        .JumpIfFalse => |payload| try writer.print("ip[{d}] JumpIfFalse label:{} type:{s}\n", .{ idx, payload.label_id, @tagName(payload.condition_type) }),
        .PrintBegin => try writer.print("ip[{d}] PrintBegin\n", .{idx}),
        .PrintStr => |payload| try writer.print("ip[{d}] PrintStr const:{}\n", .{ idx, payload.const_id }),
        .PrintVal => try writer.print("ip[{d}] PrintVal\n", .{idx}),
        .PrintNewline => try writer.print("ip[{d}] PrintNewline\n", .{idx}),
        .PrintEnd => try writer.print("ip[{d}] PrintEnd\n", .{idx}),
        .JumpIfTrue => |payload| try writer.print("ip[{d}] JumpIfTrue label:{} type:{s}\n", .{ idx, payload.label_id, @tagName(payload.condition_type) }),
        .Label => |payload| try writer.print("ip[{d}] Label {}\n", .{ idx, payload.id }),
        .Call => |payload| try writeCallInstruction(idx, "Call", payload, writer),
        .TailCall => |payload| try writeCallInstruction(idx, "TailCall", payload, writer),
        .Return => |payload| try writer.print("ip[{d}] Return has_value:{} type:{s}\n", .{ idx, payload.has_value, @tagName(payload.return_type) }),
        .TryBegin => |payload| try writer.print("ip[{d}] TryBegin label:{}\n", .{ idx, payload.label_id }),
        .TryCatch => |payload| {
            if (payload.exception_type) |et| {
                try writer.print("ip[{d}] TryCatch type:{s}\n", .{ idx, @tagName(et) });
            } else {
                try writer.print("ip[{d}] TryCatch type:any\n", .{idx});
            }
        },
        .Throw => |payload| try writer.print("ip[{d}] Throw type:{s}\n", .{ idx, @tagName(payload.exception_type) }),
        .EnterScope => |payload| try writer.print("ip[{d}] EnterScope id:{} vars:{}\n", .{ idx, payload.scope_id, payload.var_count }),
        .ExitScope => |payload| try writer.print("ip[{d}] ExitScope id:{}\n", .{ idx, payload.scope_id }),
        .ArrayNew => |payload| try writer.print("ip[{d}] ArrayNew elem:{s} size:{} nested:{}\n", .{ idx, @tagName(payload.element_type), payload.static_size, payload.nested_element_type != null }),
        .ArrayGet => |payload| try writer.print("ip[{d}] ArrayGet bounds_check:{}\n", .{ idx, payload.bounds_check }),
        .ArraySet => |payload| try writer.print("ip[{d}] ArraySet bounds_check:{}\n", .{ idx, payload.bounds_check }),
        .ArrayPush => |payload| try writer.print("ip[{d}] ArrayPush resize:{s}\n", .{ idx, @tagName(payload.resize) }),
        .ArrayPop => try writer.print("ip[{d}] ArrayPop\n", .{idx}),
        .ArrayInsert => try writer.print("ip[{d}] ArrayInsert\n", .{idx}),
        .ArrayRemove => try writer.print("ip[{d}] ArrayRemove\n", .{idx}),
        .ArraySlice => try writer.print("ip[{d}] ArraySlice\n", .{idx}),
        .ArrayLen => try writer.print("ip[{d}] ArrayLen\n", .{idx}),
        .ArrayConcat => try writer.print("ip[{d}] ArrayConcat\n", .{idx}),
        .Range => |payload| try writer.print("ip[{d}] Range elem:{s}\n", .{ idx, @tagName(payload.element_type) }),
        .Exists => |payload| try writer.print("ip[{d}] Exists predicate:{s}\n", .{ idx, @tagName(payload.predicate_type) }),
        .Forall => |payload| try writer.print("ip[{d}] Forall predicate:{s}\n", .{ idx, @tagName(payload.predicate_type) }),
        .StructNew => |payload| try writer.print("ip[{d}] StructNew {s} fields:{} bytes:{}\n", .{ idx, payload.type_name, payload.field_count, payload.size_bytes }),
        .EnumNew => |payload| try writer.print("ip[{d}] EnumNew {s}.{s} idx:{}\n", .{ idx, payload.enum_name, payload.variant_name, payload.variant_index }),
        .GetField => |payload| try writer.print("ip[{d}] GetField {s} idx:{} container:{s}\n", .{ idx, payload.field_name, payload.field_index, @tagName(payload.container_type) }),
        .SetField => |payload| try writer.print("ip[{d}] SetField {s} idx:{} container:{s}\n", .{ idx, payload.field_name, payload.field_index, @tagName(payload.container_type) }),
        .StoreFieldName => |payload| try writer.print("ip[{d}] StoreFieldName {s}\n", .{ idx, payload.field_name }),
        .Print => try writer.print("ip[{d}] Print\n", .{idx}),
        .PrintInterpolated => |payload| {
            try writer.print("ip[{d}] PrintInterpolated parts:{} placeholders:{} args:{}\n", .{ idx, payload.format_parts.len, payload.placeholder_indices.len, payload.argument_count });
            try writeStringSlice("    format_parts", payload.format_parts, writer);
            try writeU32Slice("    placeholder_indices", payload.placeholder_indices, writer);
            try writeU32Slice("    format_part_ids", payload.format_part_ids, writer);
        },
        .Peek => |payload| {
            const name = payload.name orelse "-";
            try writer.print("ip[{d}] Peek name:{s} type:{s}\n", .{ idx, name, @tagName(payload.value_type) });
        },
        .PeekStruct => |payload| try writer.print("ip[{d}] PeekStruct {s} fields:{}\n", .{ idx, payload.type_name, payload.field_count }),
        .PrintStruct => |payload| try writer.print("ip[{d}] PrintStruct {s} fields:{}\n", .{ idx, payload.type_name, payload.field_count }),
        .Map => |payload| try writer.print("ip[{d}] Map entries:{} key:{s} value:{s}\n", .{ idx, payload.entries.len, @tagName(payload.key_type), @tagName(payload.value_type) }),
        .MapGet => |payload| try writer.print("ip[{d}] MapGet key:{s}\n", .{ idx, @tagName(payload.key_type) }),
        .MapSet => |payload| try writer.print("ip[{d}] MapSet key:{s}\n", .{ idx, @tagName(payload.key_type) }),
        .AssertFail => |payload| try writer.print("ip[{d}] AssertFail file:{s} line:{} col:{} message:{}\n", .{ idx, payload.location.file, payload.location.range.start_line, payload.location.range.start_col, payload.has_message }),
        else => try writer.print("ip[{d}] {s}\n", .{ idx, @tagName(inst) }),
    }
}

fn writeSlotOperand(idx: usize, label: []const u8, operand: module.SlotOperand, writer: anytype) !void {
    try writer.print("ip[{d}] {s}\n", .{ idx, label });
    try writeSlotOperandDetail("    operand", operand, writer);
}

fn writeSlotOperandDetail(prefix: []const u8, operand: module.SlotOperand, writer: anytype) !void {
    if (operand.module_id) |module_id| {
        try writer.print("{s}: slot:{} kind:{s} module:{}\n", .{ prefix, operand.slot, @tagName(operand.kind), module_id });
    } else {
        try writer.print("{s}: slot:{} kind:{s}\n", .{ prefix, operand.slot, @tagName(operand.kind) });
    }
}

fn writeCallInstruction(idx: usize, name: []const u8, payload: anytype, writer: anytype) !void {
    const target = payload.target;
    const module_name = target.target_module orelse "-";
    if (target.target_module_id) |module_id| {
        try writer.print(
            "ip[{d}] {s} fn:{} qualified:{s} kind:{s} module:{s} module_id:{} args:{} return:{s}\n",
            .{ idx, name, target.function_index, target.qualified_name, @tagName(target.call_kind), module_name, module_id, payload.arg_count, @tagName(payload.return_type) },
        );
    } else {
        try writer.print(
            "ip[{d}] {s} fn:{} qualified:{s} kind:{s} module:{s} args:{} return:{s}\n",
            .{ idx, name, target.function_index, target.qualified_name, @tagName(target.call_kind), module_name, payload.arg_count, @tagName(payload.return_type) },
        );
    }
}

fn writeU32Slice(prefix: []const u8, slice: []const u32, writer: anytype) !void {
    try writer.print("{s}: [", .{prefix});
    for (slice, 0..) |value, i| {
        if (i != 0) try writer.print(", ", .{});
        try writer.print("{}", .{value});
    }
    try writer.print("]\n", .{});
}

fn writeStringSlice(prefix: []const u8, slice: []const []const u8, writer: anytype) !void {
    try writer.print("{s}: [", .{prefix});
    for (slice, 0..) |value, i| {
        if (i != 0) try writer.print(", ", .{});
        try writer.print("\"", .{});
        try printEscapedString(writer, value);
        try writer.print("\"", .{});
    }
    try writer.print("]\n", .{});
}

fn printEscapedString(writer: anytype, input: []const u8) !void {
    for (input) |ch| {
        switch (ch) {
            '\\' => try writer.writeAll("\\\\"),
            '"' => try writer.writeAll("\\\""),
            '\n' => try writer.writeAll("\\n"),
            '\t' => try writer.writeAll("\\t"),
            '\r' => try writer.writeAll("\\r"),
            else => try writer.writeByte(ch),
        }
    }
}
