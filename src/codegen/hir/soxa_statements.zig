const std = @import("std");
const HIRGenerator = @import("soxa_generator.zig").HIRGenerator;
const ast = @import("../../ast/ast.zig");
const Location = @import("../../utils/reporting.zig").Location;
const ErrorList = @import("../../utils/errors.zig").ErrorList;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;
const HIRType = @import("soxa_generator.zig").HIRType;
const HIRValue = @import("soxa_generator.zig").HIRValue;
const HIRInstruction = @import("soxa_generator.zig").HIRInstruction;
const HIRMapEntry = @import("soxa_generator.zig").HIRMapEntry;
const SoxaTypes = @import("soxa_types.zig");
const ScopeKind = SoxaTypes.ScopeKind;

const NestedSizes = struct {
    sizes: [4]u32 = [_]u32{0} ** 4,
    depth: u3 = 0,
    truncated: bool = false,
};

fn collectNestedSizes(array_type: ?*const ast.TypeInfo) NestedSizes {
    var result = NestedSizes{};
    var cursor = array_type;
    while (cursor) |info| {
        if (info.base == .Array and info.array_storage == .fixed) {
            if (info.array_size) |s| {
                if (result.depth < 4) {
                    result.sizes[result.depth] = @intCast(s);
                    result.depth += 1;
                } else {
                    result.truncated = true;
                }
            }
            cursor = info.array_type;
        } else {
            break;
        }
    }
    return result;
}

/// Push a declared array with no elements: zero-length when dynamic, its
/// declared dimensions zero-filled when fixed.
fn emitDeclaredArray(self: *HIRGenerator, stmt: ast.Stmt, declared: ast.TypeInfo, element_type: HIRType) !void {
    const storage_kind = self.storageKindFromTypeInfo(declared);
    const is_fixed = storage_kind == .fixed or storage_kind == .const_literal;
    const size: u32 = if (is_fixed) (if (declared.array_size) |n| @intCast(n) else 0) else 0;
    const nested = if (is_fixed) collectNestedSizes(declared.array_type) else NestedSizes{};
    if (nested.truncated) {
        self.reporter.reportCompileError(
            stmt.base.location(),
            ErrorCode.INVALID_ARRAY_TYPE,
            "nested arrays are limited to 4 levels",
            .{},
        );
    }
    try self.instructions.append(.{ .ArrayNew = .{
        .element_type = element_type,
        .size = size,
        .nested_element_type = SoxaTypes.arrayInnermostElementType(element_type),
        .storage_kind = storage_kind,
        .nested_sizes = nested.sizes,
        .nested_depth = nested.depth,
        .element_struct_field_types = self.elementStructFieldTypes(element_type),
        .element_struct_type_name = self.elementStructTypeName(element_type),
    } });
}

pub fn generateStatement(self: *HIRGenerator, stmt: ast.Stmt) (std.mem.Allocator.Error || ErrorList)!void {
    switch (stmt.data) {
        .Defer => |expr| {
            if (self.deferred_stack.items.len > 0) {
                try self.deferred_stack.items[self.deferred_stack.items.len - 1]
                    .actions.append(.{ .expr = expr });
            } else {
                try self.generateExpression(expr, false, false);
            }
        },
        .Lift => |lift_data| {
            // TODO: lift outside a block context should be a semantic error reported earlier
            try self.generateExpression(lift_data.value, false, true);
        },
        .ZigDecl => {
            // Inline zig modules are handled outside of normal statement lowering.
            // They only contribute external/module-callable functions.
        },
        // TODO: binary expressions don't pop their result when should_pop_after_use=true,
        // causing stack leaks when binary expressions appear as discarded statements in blocks
        .Expression => |expr| {
            if (expr) |e| {
                // Do not treat a trailing expression as an implicit function return.
                // Always generate as a regular expression statement here.
                try self.generateExpression(e, false, true);
            }
        },
        .Continue => {
            if (self.currentLoopContext()) |lc| {
                if (self.loop_deferred_boundaries.items.len > 0) {
                    const boundary = self.loop_deferred_boundaries.items[self.loop_deferred_boundaries.items.len - 1];
                    try self.emitDeferredToBoundary(boundary);
                }
                try self.instructions.append(.{ .Jump = .{ .label = lc.continue_label } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
                    .file_uri = stmt.base.location().file_uri,
                    .range = .{
                        .start_line = stmt.base.location().range.start_line,
                        .start_col = stmt.base.location().range.start_col,
                        .end_line = stmt.base.location().range.end_line,
                        .end_col = stmt.base.location().range.end_col,
                    },
                };
                self.reporter.reportCompileError(
                    location,
                    ErrorCode.CONTINUE_USED_OUTSIDE_OF_LOOP,
                    "'continue' used outside of a loop",
                    .{},
                );
            }
        },
        .Break => {
            if (self.currentLoopContext()) |lc| {
                if (self.loop_deferred_boundaries.items.len > 0) {
                    const boundary = self.loop_deferred_boundaries.items[self.loop_deferred_boundaries.items.len - 1];
                    try self.emitDeferredToBoundary(boundary);
                }
                try self.instructions.append(.{ .Jump = .{ .label = lc.break_label } });
            } else {
                const location = Location{
                    .file = stmt.base.location().file,
                    .file_uri = stmt.base.location().file_uri,
                    .range = .{
                        .start_line = stmt.base.location().range.start_line,
                        .start_col = stmt.base.location().range.start_col,
                        .end_line = stmt.base.location().range.end_line,
                        .end_col = stmt.base.location().range.end_col,
                    },
                };
                self.reporter.reportCompileError(
                    location,
                    ErrorCode.BREAK_USED_OUTSIDE_OF_LOOP,
                    "'break' used outside of a loop",
                    .{},
                );
            }
        },
        .VarDecl => |decl| {
            // The binding's type, as analysis declared it: its annotation
            // (completed from the initializer where incomplete), or else its
            // initializer's type.
            const var_type = try self.bindingTypeOf(&stmt.base);

            if (decl.initializer) |init_expr| {
                try self.generateExpression(init_expr, true, true);

                const empty_literal = init_expr.data == .Array and init_expr.data.Array.len == 0;
                if (empty_literal and decl.type_info.base == .Array) {
                    // A fixed-size array has an immutable length, so `is []`
                    // zero-fills the declared dimensions rather than producing
                    // a zero-length array (which downstream indexing would then
                    // read out of bounds of).
                    try self.instructions.append(.Pop);
                    try emitDeclaredArray(self, stmt, decl.type_info, var_type.Array.*);
                } else {
                    try self.convertValue(try self.typeOf(init_expr), var_type);
                }
            } else if (var_type == .Array) {
                try emitDeclaredArray(self, stmt, decl.type_info, var_type.Array.*);
            } else {
                const default_value: HIRValue = switch (var_type) {
                    .Int => .{ .int = 0 },
                    .Float => .{ .float = 0.0 },
                    .String => .{ .string = "" },
                    .Tetra => .{ .tetra = 0 },
                    .Byte => .{ .byte = 0 },
                    // TODO(struct default): this arm absorbs every type with
                    // no materialized default — `.Struct` included — and
                    // pushes `nothing`, which the store then rejects as
                    // malformed HIR, so `var p :: Point` fails to compile
                    // with an internal error instead of a Doxa diagnostic.
                    // What a struct local should hold (null, a synthesized
                    // default, or a rejection) is undecided: see
                    // plan/uninitialized-declarations.md. Whatever it becomes,
                    // this switch should be exhaustive so the next type
                    // without a default fails to compile rather than emit
                    // invalid IR.
                    else => .nothing,
                };
                const const_idx = try self.addConstant(default_value);
                try self.instructions.append(.{ .Const = .{ .value = default_value, .constant_id = const_idx } });
                try self.convertValue(if (default_value == .nothing) .Nothing else var_type, var_type);
            }

            const is_module_ctx = self.current_function == null and self.isModuleContext();
            if (self.current_function == null) {
                try self.instructions.append(.Dup);
            }
            const place = try self.placeOfName(&stmt.base, decl.name.lexeme);
            try self.instructions.append(.{ .StoreDecl = .{
                .slot = place.slot,
                .var_name = place.var_name,
                .scope_kind = if (is_module_ctx and place.scope_kind == .GlobalLocal) .ModuleGlobal else place.scope_kind,
                .module_context = null,
                .declared_type = var_type,
                .is_const = !decl.type_info.is_mutable,
            } });
            if (self.current_function == null) {
                try self.instructions.append(.Pop);
            }
        },
        .FunctionDecl => {},
        .Return => |ret| {
            if (ret.value) |value| {
                try self.generateReturnValue(value);
            } else {
                const nothing_idx = try self.addConstant(HIRValue.nothing);
                try self.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
            }

            try self.emitAllDeferredForReturn();

            try self.instructions.append(.{ .Return = .{ .has_value = ret.value != null, .return_type = self.current_function_return_type } });

            if (self.current_function_scope_id) |scope_id| {
                try self.instructions.append(.{ .ExitScope = .{ .scope_id = scope_id } });
            }
        },
        // Type declarations are compile time only: analysis registered them.
        .EnumDecl, .GroupDecl => {},
        .Assert => |assert_stmt| {
            try self.generateExpression(assert_stmt.condition, true, true);

            const success_label = try self.generateLabel("assert_success");
            const failure_label = try self.generateLabel("assert_failure");

            try self.instructions.append(.{
                .JumpCond = .{
                    .label_true = success_label,
                    .label_false = failure_label,
                    .condition_type = .Tetra,
                },
            });

            try self.instructions.append(.{ .Label = .{ .name = failure_label } });

            if (assert_stmt.message) |msg| {
                try self.generateExpression(msg, true, true);
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = true,
                } });
            } else {
                try self.instructions.append(.{ .AssertFail = .{
                    .location = assert_stmt.location,
                    .has_message = false,
                } });
            }
            try self.instructions.append(.{ .Label = .{ .name = success_label } });
        },
        .MapLiteral => |map_literal| {
            // Generate else value first if it exists
            if (map_literal.else_value) |else_expr| {
                try self.generateExpression(else_expr, true, false);
            }

            var reverse_i = map_literal.entries.len;
            while (reverse_i > 0) {
                reverse_i -= 1;
                const entry = map_literal.entries[reverse_i];
                try self.generateExpression(entry.key, true, false);
                try self.generateExpression(entry.value, true, false);
            }

            const dummy_entries = try self.allocator.alloc(HIRMapEntry, map_literal.entries.len);
            for (dummy_entries) |*entry| {
                const nothing_key = try self.allocator.create(HIRValue);
                nothing_key.* = .{ .nothing = .{} };
                const nothing_value = try self.allocator.create(HIRValue);
                nothing_value.* = .{ .nothing = .{} };
                entry.* = HIRMapEntry{
                    .key = nothing_key,
                    .value = nothing_value,
                };
            }

            const map_instruction = HIRInstruction{
                .Map = .{
                    .entries = dummy_entries,
                    .key_type = .String,
                    .value_type = .Unknown,
                    .has_else_value = map_literal.else_value != null,
                },
            };

            try self.instructions.append(map_instruction);
        },
        // Imports were bound by the module loader: compile time only.
        .Import => {},
    }
}
