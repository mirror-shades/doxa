const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;

const StructPeekInfo = HIRGenerator.StructPeekInfo;

    /// Handle I/O and debugging operations: peek, input
    pub const IOHandler = struct {
        generator: *HIRGenerator,

        pub fn init(generator: *HIRGenerator) IOHandler {
            return .{ .generator = generator };
        }

        /// Generate HIR for peek expressions
        pub fn generatePeek(self: *IOHandler, peek: ast.PeekExpr, preserve_result: bool) !void {
        // Set current peek expression for field access tracking
        self.generator.current_peek_expr = peek.expr;
        defer self.generator.current_peek_expr = null;

        // Generate the expression to peek (leaves value on stack)
        try self.generator.generateExpression(peek.expr, true, false);

        // Build the full path for the peek expression (handles field access)
        // Special case: Don't show variable name for enum member access like Color.Red
        const peek_path = if (peek.expr.data == .FieldAccess) blk: {
            const field = peek.expr.data.FieldAccess;
            const obj_type = self.generator.inferTypeFromExpression(field.object);
            // If this is enum member access (Color.Red), don't show variable name
            if (obj_type == .Enum and field.object.data == .Variable) {
                break :blk null; // No variable name for enum member access
            } else {
                break :blk try self.generator.buildPeekPath(peek.expr);
            }
        } else try self.generator.buildPeekPath(peek.expr);

        // NEW: Prefer expression inference; refine for array indexing
        var inferred_type: HIRType = self.generator.inferTypeFromExpression(peek.expr);
        var enum_type_name: ?[]const u8 = null;
        if (peek.expr.data == .Index and peek.expr.data.Index.array.data == .Variable) {
            if (self.generator.getTrackedArrayElementType(peek.expr.data.Index.array.data.Variable.lexeme)) |elem_type| {
                inferred_type = elem_type;
            }
        } else if (peek.expr.data == .Variable) {
            inferred_type = try self.generator.typeOf(peek.expr);
        } else if (peek.expr.data == .FieldAccess) {
            // For field accesses, try to recover the concrete enum type name
            // (e.g., "Species" for zoo[0].animal_type) so the LLVM backend
            // can print the enum nicely even when the value on the stack is
            // just an i64 discriminant.
            if (inferred_type == .Enum) {
                if (self.generator.resolveFieldAccessType(peek.expr)) |res| {
                    enum_type_name = res.custom_type_name;
                }
            }
        }

        // Without its enum the backend falls back to raw integers, which prints
        // `enum = <enum>`. A variable narrowed by `as` tracks the member type
        // itself, so the enum's key comes from the enum table rather than from
        // a field-access path.
        if (enum_type_name == null and inferred_type == .Enum) {
            enum_type_name = self.generator.semantic.enum_table.keyOf(inferred_type.Enum);
        }

        // New: include union member list for variables declared as unions or expressions that return unions
        var union_members: ?[][]const u8 = null;
        // Attach inline union info for selected builtins/internal calls
        if (peek.expr.data == .Variable) {
            const var_name = peek.expr.data.Variable.lexeme;
            // Scope-aware lookup: local and global variables can share a numeric
            // index, so the union-member key is keyed by (is_local, index).
            if (self.generator.symbol_table.getVariable(var_name)) |var_index| {
                const is_local = self.generator.symbol_table.isLocalVariable(var_name);
                // Inside a function, a global accessed by name should not surface
                // union member info; keep parameters/locals authoritative.
                const should_use_union_members = !(self.generator.symbol_table.current_function != null and !is_local);
                if (should_use_union_members) {
                    if (self.generator.symbol_table.getVariableUnionMembers(is_local, var_index)) |members2| {
                        union_members = members2;
                    }
                }
            }
        }

        if (union_members == null and inferred_type == .Union) {
            union_members = try self.generator.collectUnionMemberNamesFromHIRType(inferred_type);
        }

        if (union_members == null and inferred_type == .Group) {
            if (peek.expr.data == .Variable) {
                const var_name = peek.expr.data.Variable.lexeme;
                if (self.generator.symbol_table.getVariableCustomType(var_name)) |custom_name| {
                    union_members = try self.generator.type_system.getGroupMemberNames(custom_name);
                }
            }
            if (union_members == null) {
                if (self.generator.semantic.group_table.keyOf(inferred_type.Group)) |group_key| {
                    union_members = try self.generator.type_system.getGroupMemberNames(group_key);
                }
            }
        }

        // A union the source wrote with a group shows the group, not the
        // members it flattened into.
        var member_slots: ?[]const u32 = null;
        if (union_members) |names| {
            if (inferred_type == .Union) {
                if (try self.collapseWrittenGroups(peek.expr, inferred_type, names)) |collapsed| {
                    union_members = collapsed.names;
                    member_slots = collapsed.slots;
                }
            }
        }

        // Generate peek instruction with full path and correct type
        // B2: peeking a struct prints it through the descriptor registry.
        self.generator.markReflectedType(inferred_type);
        try self.generator.instructions.append(.{ .Peek = .{
            .name = peek_path,
            .value_type = inferred_type,
            .location = peek.location,
            .union_members = union_members,
            .member_slots = member_slots,
            .enum_type_name = enum_type_name,
        } });

        // Peek reads the top of the stack without popping it.
        // In statement context (!preserve_result), we must pop the value to prevent
        // it from polluting the compile-time stack in the LLVM IR backend.
        // Without this, stale values accumulate and get consumed by subsequent
        // operations (e.g. StoreVar after a void Call), causing memory corruption.
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
    }

    const CollapsedMembers = struct {
        names: [][]const u8,
        slots: []const u32,
    };

    /// The display list of a union whose written type names groups: each
    /// member flattened from a written group is shown as that group, once, at
    /// the place of its first member. Null when the written type names no group.
    fn collapseWrittenGroups(self: *IOHandler, expr: *ast.Expr, union_type: HIRType, member_names: [][]const u8) !?CollapsedMembers {
        const g = self.generator;
        const written = g.semantic.getCachedExprType(expr) orelse return null;
        if (written.base != .Union) return null;
        var groups: std.ArrayListUnmanaged(u32) = .empty;
        try self.collectWrittenGroups(written, &groups);
        if (groups.items.len == 0) return null;

        const members = union_type.Union.members;
        var names: std.ArrayListUnmanaged([]const u8) = .empty;
        const slots = try g.allocator.alloc(u32, members.len);
        var slot_groups: std.ArrayListUnmanaged(?u32) = .empty;
        defer slot_groups.deinit(g.allocator);
        for (members, member_names, slots) |member, member_name, *slot| {
            const group = self.writtenGroupOf(member.*, groups.items);
            const existing = if (group) |gid| for (slot_groups.items, 0..) |slot_group, idx| {
                if (slot_group == gid) break idx;
            } else null else null;
            if (existing) |idx| {
                slot.* = @intCast(idx);
                continue;
            }
            slot.* = @intCast(names.items.len);
            try names.append(g.allocator, if (group) |gid| g.semantic.group_table.displayName(gid).? else member_name);
            try slot_groups.append(g.allocator, group);
        }
        return .{ .names = try names.toOwnedSlice(g.allocator), .slots = slots };
    }

    fn collectWrittenGroups(self: *IOHandler, written: *const ast.TypeInfo, groups: *std.ArrayListUnmanaged(u32)) !void {
        const ut = written.union_type orelse return;
        for (ut.types) |member| {
            if (member.base == .Union) {
                try self.collectWrittenGroups(member, groups);
                continue;
            }
            const custom = member.custom_type orelse continue;
            const gid = self.generator.semantic.group_table.idOf(custom.resolved()) orelse continue;
            try groups.append(self.generator.allocator, gid);
        }
    }

    /// The first of `groups` that flattened into the union member `member`.
    fn writtenGroupOf(self: *IOHandler, member: HIRType, groups: []const u32) ?u32 {
        for (groups) |gid| {
            for (self.generator.semantic.group_table.members(gid) orelse &.{}) |group_member| {
                const holds = switch (group_member.kind) {
                    .Enum => member == .Enum and member.Enum == group_member.id,
                    .Struct => member == .Struct and member.Struct == group_member.id,
                    .Group => member == .Group and member.Group == group_member.id,
                };
                if (holds) return gid;
            }
        }
        return null;
    }

    /// Generate HIR for struct peek expressions
    pub fn generatePeekStruct(self: *IOHandler, peek: ast.Expr.Data, preserve_result: bool) !void {
        const peek_data = peek.PeekStruct;

        // Generate the expression to peek
        try self.generator.generateExpression(peek_data.expr, true, false);

        // Get struct info from the expression
        const struct_info = switch (peek_data.expr.data) {
            .StructLiteral => |struct_lit| blk: {
                const field_count: u32 = @truncate(struct_lit.fields.len);
                const field_names = try self.generator.allocator.alloc([]const u8, struct_lit.fields.len);
                const field_types = try self.generator.allocator.alloc(HIRType, struct_lit.fields.len);
                for (struct_lit.fields, 0..) |field_ptr, idx| {
                    field_names[idx] = field_ptr.name.lexeme;
                    field_types[idx] = self.generator.inferTypeFromExpression(field_ptr.value);
                }
                break :blk StructPeekInfo{
                    .name = struct_lit.name.lexeme,
                    .field_count = field_count,
                    .field_names = field_names,
                    .field_types = field_types,
                };
            },
            .Variable => |var_token| if (self.generator.getTrackedVariableType(var_token.lexeme)) |var_type| blk: {
                if (var_type != .Struct) {
                    return error.ExpectedStructType;
                }
                const field_names = try self.generator.allocator.alloc([]const u8, 0);
                const field_types = try self.generator.allocator.alloc(HIRType, 0);
                var info = StructPeekInfo{
                    .name = var_token.lexeme,
                    .field_count = 0,
                    .field_names = field_names,
                    .field_types = field_types,
                };
                try self.populateStructInfoFromType(&info, var_type);
                break :blk info;
            } else {
                return error.UnknownVariableType;
            },
            .FieldAccess => |field| blk: {
                // For field access, we need to generate the field access code first
                try self.generator.generateExpression(field.object, true, false);
                try self.generator.instructions.append(.{
                    .StoreFieldName = .{
                        .field_name = field.field.lexeme,
                    },
                });

                const container_type = self.generator.inferTypeFromExpression(field.object);
                const field_struct_id: u32 = if (container_type == .Struct) container_type.Struct else 0;

                // Generate GetField instruction to access the field
                try self.generator.instructions.append(.{
                    .GetField = .{
                        .field_name = field.field.lexeme,
                        .container_type = container_type,
                        .struct_id = field_struct_id,
                        .field_index = 0,
                        .field_type = .Unknown,
                        .field_for_peek = true,
                        .nested_struct_id = null,
                    },
                });

                // Create a single-field struct info
                const field_names = try self.generator.allocator.alloc([]const u8, 1);
                const field_types = try self.generator.allocator.alloc(HIRType, 1);
                field_names[0] = field.field.lexeme;
                field_types[0] = self.generator.inferTypeFromExpression(peek_data.expr);

                break :blk StructPeekInfo{
                    .name = field.field.lexeme,
                    .field_count = 1,
                    .field_names = field_names,
                    .field_types = field_types,
                };
            },
            else => {
                return error.ExpectedStructType;
            },
        };

        const peek_struct_type = if (peek_data.expr.data == .StructLiteral)
            try self.generator.typeOf(peek_data.expr)
        else
            self.generator.inferTypeFromExpression(peek_data.expr);
        const peek_sid: u32 = if (peek_struct_type == .Struct) peek_struct_type.Struct else 0;

        // B2: a struct peek prints through the descriptor registry.
        self.generator.markReflectedType(peek_struct_type);

        // Add the PeekStruct instruction with the gathered info
        try self.generator.instructions.append(.{ .PeekStruct = .{
            .type_name = struct_info.name,
            .struct_id = peek_sid,
            .field_count = struct_info.field_count,
            .field_names = struct_info.field_names,
            .field_types = struct_info.field_types,
            .location = peek_data.location,
            .should_pop_after_peek = !preserve_result,
        } });
    }

    fn populateStructInfoFromType(self: *IOHandler, info: *StructPeekInfo, hir_type: HIRType) !void {
        if (hir_type != .Struct) return;
        const struct_id = hir_type.Struct;
        const table = &self.generator.semantic.struct_table;
        const fields = table.fields(struct_id) orelse return;
        const names = try self.generator.allocator.alloc([]const u8, fields.len);
        const types_arr = try self.generator.allocator.alloc(HIRType, fields.len);
        for (fields, 0..) |field_info, idx| {
            names[idx] = field_info.name;
            types_arr[idx] = field_info.hir_type;
        }
        self.generator.allocator.free(info.field_names);
        self.generator.allocator.free(info.field_types);
        info.field_names = names;
        info.field_types = types_arr;
        info.field_count = @intCast(fields.len);
        info.name = table.keyOf(struct_id).?;
    }

    /// Generate HIR for input expressions
    pub fn generateInput(self: *IOHandler, input: ast.Expr.Data) !void {
        const input_data = input.Input;

        // Check if we have a non-empty prompt
        const prompt_str = input_data.prompt.literal.string;
        if (prompt_str.len > 0) {
            // Generate the prompt as a constant first
            const prompt_value = HIRValue{ .string = prompt_str };
            const prompt_idx = try self.generator.addConstant(prompt_value);

            // Push the prompt onto the stack as an argument
            try self.generator.instructions.append(.{ .Const = .{ .value = prompt_value, .constant_id = prompt_idx } });

            // Generate input call with the prompt as argument
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = null,
                    .qualified_name = "input",
                    .arg_count = 1, // Has 1 argument (the prompt)
                    .call_kind = .BuiltinFunction,
                    .return_type = .String,
                },
            });
        } else {
            // No prompt - call input with no arguments
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = null,
                    .qualified_name = "input",
                    .arg_count = 0, // No arguments
                    .call_kind = .BuiltinFunction,
                    .return_type = .String,
                },
            });
        }
    }
};
