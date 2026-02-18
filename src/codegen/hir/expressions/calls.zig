const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const SoxaStatements = @import("../soxa_statements.zig");
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const ScopeKind = @import("../soxa_types.zig").ScopeKind;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ArithOp = @import("../soxa_instructions.zig").ArithOp;
const CallKind = @import("../soxa_instructions.zig").CallKind;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;
const builtin_methods = @import("../../../runtime/builtin_methods.zig");

pub const CallsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) CallsHandler {
        return .{ .generator = generator };
    }

    pub fn generateFunctionCall(self: *CallsHandler, function_call: ast.Expr.Data, should_pop_after_use: bool) !void {
        const call_data = function_call.FunctionCall;

        var function_name: []const u8 = "unknown";
        var call_kind: CallKind = .LocalFunction;
        var function_index: u32 = 0;

        switch (call_data.callee.data) {
            .FieldAccess => |field_access| {
                var handled_module_call = false;
                if (try self.moduleNamespaceFromExpr(field_access.object)) |module_ns| {
                    function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ module_ns, field_access.field.lexeme });
                    call_kind = .ModuleFunction;
                    if (self.generator.getFunctionIndex(function_name)) |idx| {
                        function_index = idx;
                    }
                    handled_module_call = true;
                }

                if (field_access.object.data == .FieldAccess) {
                    if (self.generator.resolveFieldAccessType(field_access.object)) |resolved| {
                        if (resolved.custom_type_name != null) {
                            const module_ns = resolved.custom_type_name.?;
                            if (self.generator.isModuleNamespace(module_ns)) {
                                function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ module_ns, field_access.field.lexeme });
                                call_kind = .ModuleFunction;
                                if (self.generator.getFunctionIndex(function_name)) |idx| {
                                    function_index = idx;
                                }
                                handled_module_call = true;
                            }
                        }
                    }
                }

                if (handled_module_call) {
                    // handled below by common call emission path
                } else if (field_access.object.data == .Variable and self.generator.isModuleNamespace(field_access.object.data.Variable.lexeme)) {
                    const object_name = field_access.object.data.Variable.lexeme;

                    function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ object_name, field_access.field.lexeme });
                    call_kind = .ModuleFunction;
                    if (self.generator.getFunctionIndex(function_name)) |idx| {
                        function_index = idx;
                    }
                } else if (field_access.object.data == .Variable) {
                    const type_name = field_access.object.data.Variable.lexeme;
                    if (self.generator.isCustomType(type_name)) |ct| {
                        if (ct.kind == .Struct) {
                            function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ type_name, field_access.field.lexeme });
                            if (self.generator.getFunctionIndex(function_name)) |idx| {
                                function_index = idx;
                                call_kind = .LocalFunction;
                                for (call_data.arguments) |arg| {
                                    try self.generator.generateExpression(arg.expr, true, should_pop_after_use);
                                }
                                const return_type = self.generator.inferCallReturnType(function_name, .LocalFunction) catch .Nothing;
                                try self.generator.instructions.append(.{ .Call = .{
                                    .function_index = function_index,
                                    .qualified_name = function_name,
                                    .arg_count = @intCast(call_data.arguments.len),
                                    .call_kind = call_kind,
                                    .target_module = null,
                                    .return_type = return_type,
                                } });
                                return;
                            }
                        }
                    }

                    const type_name2 = field_access.object.data.Variable.lexeme;
                    if (self.generator.isCustomType(type_name2)) |ct2| {
                        if (ct2.kind == .Struct and std.mem.eql(u8, field_access.field.lexeme, "New")) {
                            try self.generateStructConstructorCall(type_name2, call_data.arguments);
                            return;
                        }
                    }

                    if (field_access.object.data == .Variable) {
                        const recv_var_name = field_access.object.data.Variable.lexeme;
                        const struct_name = blk: {
                            if (self.generator.symbol_table.getVariableCustomType(recv_var_name)) |ctype| break :blk ctype;
                            break :blk recv_var_name;
                        };

                        if (self.generator.struct_methods.get(struct_name)) |method_table| {
                            if (method_table.get(field_access.field.lexeme)) |mi| {
                                const qualified_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ struct_name, field_access.field.lexeme });

                                 if (!mi.is_static) {
                                     // For non-static methods, 'this' is an alias parameter
                                     // We need to push a storage reference, not the value
                                     if (field_access.object.data == .Variable) {
                                         const var_token = field_access.object.data.Variable;
                                         // Always push a storage reference for alias semantics, even for globals.
                                         // `getOrCreateVariable` is safe here because undefined variables should
                                         // already be rejected by semantic analysis.
                                         const var_idx = try self.generator.getOrCreateVariable(var_token.lexeme);
                                         const scope_kind = self.generator.symbol_table.determineVariableScope(var_token.lexeme);
                                         try self.generator.instructions.append(.{
                                             .PushStorageId = .{
                                                 .var_index = var_idx,
                                                 .var_name = var_token.lexeme,
                                                 .scope_kind = scope_kind,
                                             },
                                         });
                                     } else {
                                         // For non-variable receivers (like field access), generate normally
                                         try self.generator.generateExpression(field_access.object, true, false);
                                     }
                                 }
                                for (call_data.arguments) |arg| {
                                    try self.generator.generateExpression(arg.expr, true, false);
                                }

                                const ret_type: HIRType = self.generator.convertTypeInfo(mi.return_type.*);

                                const fn_index: u32 = blk: {
                                    if (self.generator.getFunctionIndex(qualified_name)) |idx| {
                                        break :blk idx;
                                    } else {
                                        break :blk 0;
                                    }
                                };

                                var arg_count: u32 = @intCast(call_data.arguments.len);
                                if (!mi.is_static) arg_count += 1;

                                try self.generator.instructions.append(.{
                                    .Call = .{
                                        .function_index = fn_index,
                                        .qualified_name = qualified_name,
                                        .arg_count = arg_count,
                                        .call_kind = .LocalFunction,
                                        .target_module = null,
                                        .return_type = ret_type,
                                    },
                                });
                                return;
                            }
                        }
                    }

                    try self.generator.generateInternalMethodCall(field_access.field, field_access.object, call_data.arguments, should_pop_after_use);
                    return;
                } else {
                    if (field_access.object.data == .Variable) {
                        const type_name = field_access.object.data.Variable.lexeme;
                        if (self.generator.isCustomType(type_name)) |ct| {
                            if (ct.kind == .Struct and std.mem.eql(u8, field_access.field.lexeme, "New")) {
                                try self.generateStructConstructorCall(type_name, call_data.arguments);
                                return;
                            }
                        }
                    }

                    try self.generator.generateInternalMethodCall(field_access.field, field_access.object, call_data.arguments, should_pop_after_use);
                    return;
                }
            },
            .Variable => |var_token| {
                function_name = var_token.lexeme;
                if (self.generator.getFunctionIndex(function_name)) |index| {
                    function_index = index;
                    call_kind = .LocalFunction;
                } else if (function_name.len > 0 and function_name[0] == '@') {
                    call_kind = .BuiltinFunction;
                } else {
                    call_kind = .ModuleFunction;
                }
            },
            else => {
                self.generator.reporter.reportCompileError(
                    call_data.callee.base.location(),
                    ErrorCode.UNSUPPORTED_FUNCTION_CALL_TYPE,
                    "Unsupported function call type",
                    .{},
                );
                return ErrorList.UnsupportedFunctionCallType;
            },
        }

        var arg_emitted_count: u32 = 0;
        for (call_data.arguments, 0..) |arg, arg_index| {
            if (arg.expr.data == .DefaultArgPlaceholder) {
                if (self.generator.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                    try self.generator.generateExpression(default_expr, true, false);
                    arg_emitted_count += 1;
                } else {
                    const location = if (call_data.callee.base.span) |span| span.location else Location{
                        .file = "",
                        .file_uri = null,
                        .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
                    };
                    self.generator.reporter.reportCompileError(location, ErrorCode.NO_DEFAULT_VALUE_FOR_PARAMETER, "No default value for parameter {} in function '{s}'", .{ arg_index, function_name });
                }
            } else {
                if (arg.is_alias) {
                    if (arg.expr.data == .Variable) {
                        const var_token = arg.expr.data.Variable;
                        const maybe_idx: ?u32 = self.generator.symbol_table.getVariable(var_token.lexeme);
                        if (maybe_idx) |var_idx| {
                            const scope_kind = self.generator.symbol_table.determineVariableScope(var_token.lexeme);
                            try self.generator.instructions.append(.{
                                .PushStorageId = .{
                                    .var_index = var_idx,
                                    .var_name = var_token.lexeme,
                                    .scope_kind = scope_kind,
                                },
                            });
                            arg_emitted_count += 1;
                        } else {
                            self.generator.reporter.reportCompileError(
                                arg.expr.base.location(),
                                ErrorCode.UNDEFINED_VARIABLE,
                                "Undefined variable used as alias argument: {s}",
                                .{var_token.lexeme},
                            );
                            return ErrorList.UndefinedVariable;
                        }
                    } else {
                        self.generator.reporter.reportCompileError(
                            arg.expr.base.location(),
                            ErrorCode.INVALID_ALIAS_ARGUMENT,
                            "Alias argument must be a variable (e.g., ^myVar)",
                            .{},
                        );
                        return ErrorList.InvalidAliasArgument;
                    }
                } else {
                    try self.generator.generateExpression(arg.expr, true, false);
                    arg_emitted_count += 1;
                }
            }
        }

        const return_type = self.generator.inferCallReturnType(function_name, call_kind) catch .String;

        if (call_kind == .LocalFunction) {
            if (try self.tryInlineFunction(function_name, call_kind)) {
                return;
            }
        }

        const target_module = try self.generator.computeTargetModule(function_name, call_kind);
        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = function_index,
                .qualified_name = function_name,
                .arg_count = arg_emitted_count,
                .call_kind = call_kind,
                .target_module = target_module,
                .return_type = return_type,
            },
        });
    }

    fn moduleNamespaceFromExpr(self: *CallsHandler, expr: *ast.Expr) !?[]const u8 {
        return switch (expr.data) {
            .Variable => |var_tok| blk: {
                if (self.generator.isModuleNamespace(var_tok.lexeme)) {
                    break :blk try self.generator.allocator.dupe(u8, var_tok.lexeme);
                }
                break :blk null;
            },
            .FieldAccess => |fa| blk: {
                const parent = try self.moduleNamespaceFromExpr(fa.object);
                if (parent) |p| {
                    break :blk try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ p, fa.field.lexeme });
                }
                break :blk null;
            },
            else => null,
        };
    }

    /// Helper function to convert AST type to HIR type
    fn astTypeToHIRType(self: *CallsHandler, ast_type: ast.Type) HIRType {
        _ = self; // self not used but kept for consistency
        return switch (ast_type) {
            .Int => .Int,
            .Byte => .Byte,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Nothing => .Nothing,
            else => .Unknown,
        };
    }

    /// Helper to validate argument count using centralized data structure
    fn validateBuiltinArgCount(self: *CallsHandler, name: []const u8, arg_count: usize) !void {
        _ = self; // self not used but kept for consistency
        if (builtin_methods.getArgCountRangeByName(name)) |range| {
            if (arg_count < range.min or arg_count > range.max) {
                return error.InvalidArgumentCount;
            }
        }
    }

    /// Helper to generate simple builtin calls that just need argument validation and a call instruction
    fn generateSimpleBuiltinCall(self: *CallsHandler, name: []const u8, arguments: []const *ast.Expr) !?HIRType {
        try self.validateBuiltinArgCount(name, arguments.len);

        // Generate all argument expressions
        for (arguments) |arg| {
            try self.generator.generateExpression(arg, true, false);
        }

        // Get return type from metadata
        if (builtin_methods.getMethodInfoByName(name)) |info| {
            const return_type = self.astTypeToHIRType(info.return_type);
            try self.generator.instructions.append(.{ .Call = .{
                .function_index = 0,
                .qualified_name = name,
                .arg_count = @intCast(arguments.len),
                .call_kind = .BuiltinFunction,
                .target_module = null,
                .return_type = return_type,
            } });
            return return_type;
        }
        return null;
    }

    pub fn generateBuiltinCall(self: *CallsHandler, bc: ast.Expr.Data, preserve_result: bool) !void {
        const builtin_data = bc.BuiltinCall;
        const name = builtin_data.function.lexeme;

        if (std.mem.eql(u8, name, "type")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            const arg = builtin_data.arguments[0];

            // For FieldAccess and EnumMember expressions, try to get the custom type name
            const inferred_type = self.generator.inferTypeFromExpression(arg);
            var custom_type_name: ?[]const u8 = null;

            if (arg.data == .FieldAccess) {
                if (self.generator.type_system.resolveFieldAccessType(arg, &self.generator.symbol_table)) |resolve_result| {
                    custom_type_name = resolve_result.custom_type_name;
                }
            } else if (arg.data == .EnumMember) {
                // For enum members, we need to find the parent enum type
                // This is a bit tricky because we don't have direct access to the parent enum name
                // We'll need to infer it from the context or use a different approach
                // For now, let's try to infer it from the inferred type
                if (inferred_type == .Enum) {
                    // Try to find the enum type that contains this variant
                    var enum_type_iter = self.generator.type_system.custom_types.iterator();
                    while (enum_type_iter.next()) |entry| {
                        if (entry.value_ptr.kind == .Enum) {
                            if (entry.value_ptr.enum_variants) |variants| {
                                for (variants) |variant| {
                                    if (std.mem.eql(u8, variant.name, arg.data.EnumMember.lexeme)) {
                                        custom_type_name = entry.key_ptr.*;
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            const type_name = switch (inferred_type) {
                .Int => "int",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Byte => "byte",
                .Nothing => "nothing",
                .Array => "array",
                .Union => "union",
                .Poison => "poison",
                .Struct => blk: {
                    if (arg.data == .Variable) {
                        const var_name = arg.data.Variable.lexeme;
                        if (self.generator.isCustomType(var_name)) |custom_type| {
                            if (custom_type.kind == .Struct) break :blk "struct";
                        }
                        if (self.generator.symbol_table.getVariableCustomType(var_name)) |var_custom_type_name| {
                            break :blk var_custom_type_name;
                        }
                    }
                    if (arg.data == .FieldAccess) {
                        const field_access = arg.data.FieldAccess;
                        if (field_access.object.data == .Variable) {
                            const obj_name = field_access.object.data.Variable.lexeme;
                            if (self.generator.symbol_table.getVariableCustomType(obj_name)) |var_custom_type_name| {
                                if (self.generator.isCustomType(var_custom_type_name)) |custom_type| {
                                    if (custom_type.kind == .Struct) {
                                        if (custom_type.struct_fields) |fields| {
                                            for (fields) |f| {
                                                if (std.mem.eql(u8, f.name, field_access.field.lexeme)) {
                                                    if (f.custom_type_name) |ctn| break :blk ctn;
                                                    // Fallback to the field's HIR type
                                                    const ft = f.field_type;
                                                    break :blk switch (ft) {
                                                        .Int => "int",
                                                        .Float => "float",
                                                        .String => "string",
                                                        .Tetra => "tetra",
                                                        .Byte => "byte",
                                                        .Nothing => "nothing",
                                                        .Array => "array",
                                                        .Poison => "poison",
                                                        .Union => "union",
                                                        .Struct => "struct",
                                                        .Map => "map",
                                                        .Enum => "enum",
                                                        .Function => "function",
                                                        .Unknown => "unknown",
                                                    };
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break :blk "struct";
                },
                .Map => "map",
                .Enum => blk: {
                    if (custom_type_name) |ctn| break :blk ctn;
                    if (arg.data == .Variable) {
                        const var_name = arg.data.Variable.lexeme;
                        if (self.generator.isCustomType(var_name)) |custom_type| {
                            if (custom_type.kind == .Enum) break :blk var_name;
                        }
                        if (self.generator.symbol_table.getVariableCustomType(var_name)) |var_custom_type_name| break :blk var_custom_type_name;
                    }
                    if (arg.data == .FieldAccess) {
                        const field_access = arg.data.FieldAccess;
                        if (field_access.object.data == .Variable) {
                            const obj_name = field_access.object.data.Variable.lexeme;
                            if (self.generator.isCustomType(obj_name)) |custom_type| {
                                if (custom_type.kind == .Enum) break :blk obj_name;
                            }
                            if (self.generator.symbol_table.getVariableCustomType(obj_name)) |var_custom_type_name| {
                                if (self.generator.isCustomType(var_custom_type_name)) |ct| {
                                    if (ct.kind == .Struct) {
                                        if (ct.struct_fields) |fields| {
                                            for (fields) |f| {
                                                if (std.mem.eql(u8, f.name, field_access.field.lexeme)) {
                                                    if (f.custom_type_name) |ctn| break :blk ctn;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    break :blk "enum";
                },
                .Function => "function",
                .Unknown => "unknown",
            };
            const type_value = HIRValue{ .string = type_name };
            const const_idx = try self.generator.addConstant(type_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = type_value, .constant_id = const_idx } });
        } else if (std.mem.eql(u8, name, "length")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            var t = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            if (t == .Unknown and builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                if (self.generator.getTrackedVariableType(var_name)) |tracked| {
                    t = tracked;
                }
            }
            switch (t) {
                .Array => try self.generator.instructions.append(.ArrayLen),
                else => try self.generator.instructions.append(.{ .StringOp = .{ .op = .Length } }),
            }
        } else if (std.mem.eql(u8, name, "int")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
        } else if (std.mem.eql(u8, name, "float")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
        } else if (std.mem.eql(u8, name, "string")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString } });
        } else if (std.mem.eql(u8, name, "byte")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            const t = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            if (t == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else if (std.mem.eql(u8, name, "push")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            const target_type = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            if (target_type == .String) {
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
            } else {
                try self.generator.instructions.append(.{ .ArrayPush = .{ .resize_behavior = .Double } });
            }
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;

                const scope_kind = self.generator.symbol_table.determineVariableScope(var_name);

                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
            }
            const nothing_const_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx } });

            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "pop")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            const target_type = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            if (target_type == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Pop } });
            } else {
                try self.generator.instructions.append(.ArrayPop);
            }
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;

                const scope_kind = self.generator.symbol_table.determineVariableScope(var_name);

                if (target_type == .String) {
                    try self.generator.instructions.append(.Swap);
                    try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
                } else {
                    try self.generator.instructions.append(.Swap);
                    try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
                }
            }
        } else if (std.mem.eql(u8, name, "insert")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.generateExpression(builtin_data.arguments[2], true, false);
            try self.generator.instructions.append(.ArrayInsert);
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;

                const scope_kind = self.generator.symbol_table.determineVariableScope(var_name);

                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
            } else {
                try self.generator.instructions.append(.Pop);
            }
            const nothing_const_idx2 = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx2 } });
            if (!preserve_result) try self.generator.instructions.append(.Pop);
        } else if (std.mem.eql(u8, name, "remove")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.instructions.append(.ArrayRemove);
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;

                const scope_kind = self.generator.symbol_table.determineVariableScope(var_name);

                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
            } else {
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "slice")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.generateExpression(builtin_data.arguments[2], true, false);
            try self.generator.instructions.append(.ArraySlice);
        } else if (std.mem.eql(u8, name, "clear")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            const target_type = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);

            if (target_type == .String) {
                // Strings are immutable - replace with empty string
                if (builtin_data.arguments[0].data == .Variable) {
                    const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                    const var_idx = try self.generator.getOrCreateVariable(var_name);
                    const expected_type = self.generator.getTrackedVariableType(var_name) orelse .String;
                    const scope_kind = self.generator.symbol_table.determineVariableScope(var_name);
                    const empty_str_value = HIRValue{ .string = "" };
                    const empty_str_idx = try self.generator.addConstant(empty_str_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = empty_str_value, .constant_id = empty_str_idx } });
                    try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = scope_kind, .module_context = null, .expected_type = expected_type } });
                }
            } else {
                // Arrays: doxa_clear modifies the ArrayHeader in-place (sets len=0).
                // No StoreVar needed since the pointer doesn't change.
                try self.generator.generateExpression(builtin_data.arguments[0], true, false);
                try self.generator.instructions.append(.{
                    .Call = .{
                        .function_index = 0,
                        .qualified_name = "clear",
                        .arg_count = 1,
                        .call_kind = .BuiltinFunction,
                        .target_module = null,
                        .return_type = .Nothing,
                    },
                });
            }
            // @clear returns nothing
            const nothing_const_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx } });

            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "find")) {
            try self.validateBuiltinArgCount(name, builtin_data.arguments.len);
            // Evaluate receiver/collection and search value
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = 0,
                    .qualified_name = "find",
                    .arg_count = 2,
                    .call_kind = .BuiltinFunction,
                    .target_module = null,
                    .return_type = .Int,
                },
            });
        } else if (std.mem.eql(u8, name, "os") or
            std.mem.eql(u8, name, "arch") or
            std.mem.eql(u8, name, "abi") or
            std.mem.eql(u8, name, "time") or
            std.mem.eql(u8, name, "tick") or
            std.mem.eql(u8, name, "random") or
            std.mem.eql(u8, name, "exit") or
            std.mem.eql(u8, name, "sleep") or
            std.mem.eql(u8, name, "build") or
            std.mem.eql(u8, name, "read"))
        {
            // Use centralized data structure for simple builtin calls
            _ = try self.generateSimpleBuiltinCall(name, builtin_data.arguments);
        } else {
            return error.NotImplemented;
        }
    }

    pub fn generateInternalCall(self: *CallsHandler, m: ast.Expr.Data) !void {
        const internal_data = m.InternalCall;

        const name = std.mem.trimLeft(u8, internal_data.method.lexeme, "@");

        if (std.mem.eql(u8, name, "substring")) {
            try self.generator.generateExpression(internal_data.arguments[0], true, false);
            try self.generator.generateExpression(internal_data.arguments[1], true, false);
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Substring } });
        } else if (std.mem.eql(u8, name, "string")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString } });
        } else if (std.mem.eql(u8, name, "length")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            var t = self.generator.inferTypeFromExpression(internal_data.receiver);
            if (t == .Unknown and internal_data.receiver.data == .Variable) {
                const var_name = internal_data.receiver.data.Variable.lexeme;
                if (self.generator.getTrackedVariableType(var_name)) |tracked| {
                    t = tracked;
                }
            }
            switch (t) {
                .Array => try self.generator.instructions.append(.ArrayLen),
                else => try self.generator.instructions.append(.{ .StringOp = .{ .op = .Length } }),
            }
        } else if (std.mem.eql(u8, name, "int")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
        } else if (std.mem.eql(u8, name, "float")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
        } else if (std.mem.eql(u8, name, "byte")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            const t = self.generator.inferTypeFromExpression(internal_data.receiver);
            if (t == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else if (std.mem.eql(u8, name, "pop")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            const target_type = self.generator.inferTypeFromExpression(internal_data.receiver);
            if (target_type == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Pop } });
            } else {
                try self.generator.instructions.append(.ArrayPop);
            }
        } else if (std.mem.eql(u8, name, "type")) {
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .TypeOf = .{ .value_type = .Unknown } });
        } else {
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }
    }

    fn generateStructConstructorCall(self: *CallsHandler, type_name: []const u8, arguments: []ast.CallArgument) !void {
        if (self.generator.isCustomType(type_name)) |ct| {
            const fields_info = ct.struct_fields orelse &[_]@import("../type_system.zig").TypeSystem.CustomTypeInfo.StructField{};
            const field_count: usize = fields_info.len;

            var field_types = try self.generator.allocator.alloc(HIRType, field_count);
            defer self.generator.allocator.free(field_types);
            var field_names = try self.generator.allocator.alloc([]const u8, field_count);
            defer self.generator.allocator.free(field_names);

            var i: usize = 0;
            while (i < field_count and i < arguments.len) : (i += 1) {
                const arg_expr = arguments[i].expr;
                try self.generator.generateExpression(arg_expr, true, false);
                const slot = field_count - 1 - i;
                field_types[slot] = self.generator.inferTypeFromExpression(arg_expr);
                const fname = fields_info[i].name;
                field_names[slot] = fname;
                const fname_const = try self.generator.addConstant(HIRValue{ .string = fname });
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = fname }, .constant_id = fname_const } });
            }

            while (i < field_count) : (i += 1) {
                const nothing_id = try self.generator.addConstant(HIRValue.nothing);
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_id } });
                const slot = field_count - 1 - i;
                field_types[slot] = .Unknown;
                const fname = fields_info[i].name;
                field_names[slot] = fname;
                const fname_const = try self.generator.addConstant(HIRValue{ .string = fname });
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = fname }, .constant_id = fname_const } });
            }

            const struct_t = self.generator.type_system.structTypeForName(type_name);
            const resolved_struct_id: u32 = if (struct_t == .Struct) struct_t.Struct else 0;

            try self.generator.instructions.append(.{
                .StructNew = .{
                    .type_name = type_name,
                    .struct_id = resolved_struct_id,
                    .field_count = @intCast(field_count),
                    .field_names = try self.generator.allocator.dupe([]const u8, field_names),
                    .field_types = try self.generator.allocator.dupe(HIRType, field_types),
                    .size_bytes = 0,
                },
            });
        }
    }

    fn tryInlineFunction(self: *CallsHandler, function_name: []const u8, call_kind: CallKind) !bool {
        if (call_kind != .LocalFunction) return false;
        const func_body = self.generator.findFunctionBody(function_name) orelse return false;
        if (!self.shouldInlineFunction(func_body)) return false;

        const scope_id = self.generator.nextScopeId();
        try self.generator.instructions.append(.{
            .EnterScope = .{ .scope_id = scope_id, .var_count = @intCast(func_body.function_info.arity) },
        });

        var i: usize = func_body.function_params.len;
        while (i > 0) {
            i -= 1;
            const param = func_body.function_params[i];
            const expected_t = func_body.param_types[i];

            if (func_body.param_is_alias[i]) {
                try self.generator.instructions.append(.{
                    .StoreParamAlias = .{
                        .param_name = param.name.lexeme,
                        .param_type = expected_t,
                        .var_index = @intCast(i + 1),
                    },
                });
            } else {
                const var_idx = try self.generator.getOrCreateVariable(param.name.lexeme);
                try self.generator.instructions.append(.{
                    .StoreVar = .{
                        .var_index = var_idx,
                        .var_name = param.name.lexeme,
                        .scope_kind = .Local,
                        .module_context = null,
                        .expected_type = expected_t,
                    },
                });
            }
        }

        try self.inlineBody(func_body);

        try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = scope_id } });

        return true;
    }

    fn inlineBody(self: *CallsHandler, func_body: *const HIRGenerator.FunctionBody) !void {
        if (func_body.statements.len == 1 and func_body.statements[0].data == .Return) {
            if (func_body.statements[0].data.Return.value) |val| {
                try self.generator.generateExpression(val, true, true);
            }
            return;
        }

        if (func_body.statements.len == 2 and
            func_body.statements[0].data == .Expression and
            func_body.statements[1].data == .Return)
        {
            const expr = func_body.statements[0].data.Expression orelse return;
            if (expr.data == .Binary) {
                try self.inlineSimpleBinary(expr.data);
                return;
            } else {
                try self.generator.generateExpression(expr, true, true);
                return;
            }
        }

        for (func_body.statements) |stmt| {
            try SoxaStatements.generateStatement(self.generator, stmt);
        }
    }

    fn inlineSimpleBinary(self: *CallsHandler, bin: ast.Expr.Data) !void {
        if (bin != .Binary) return;

        if (bin.Binary.left) |l| {
            try self.loadVarIfSimple(l);
        }
        if (bin.Binary.right) |r| {
            try self.loadVarIfSimple(r);
        }

        const op: ArithOp = switch (bin.Binary.operator.type) {
            .PLUS => .Add,
            .MINUS => .Sub,
            .ASTERISK => .Mul,
            .SLASH => .Div,
            .MODULO => .Mod,
            else => .Add,
        };

        try self.generator.instructions.append(.{ .Arith = .{ .op = op, .operand_type = .Int } });
    }

    fn loadVarIfSimple(self: *CallsHandler, expr: *ast.Expr) !void {
        if (expr.data == .Variable) {
            const v = expr.data.Variable;
            const var_idx = try self.generator.getOrCreateVariable(v.lexeme);
            try self.generator.instructions.append(.{
                .LoadVar = .{
                    .var_index = var_idx,
                    .var_name = v.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                },
            });
        } else {
            try self.generator.generateExpression(expr, true, true);
        }
    }

    fn shouldInlineFunction(self: *CallsHandler, func_body: *const HIRGenerator.FunctionBody) bool {
        if (func_body.statements.len > 3) return false; // Too complex

        // 1. Single return statement (like "return a + b")
        if (func_body.statements.len == 1 and func_body.statements[0].data == .Return) {
            return true;
        }

        // 2. Single expression statement (like "a + b" without explicit return)
        if (func_body.statements.len == 1 and func_body.statements[0].data == .Expression) {
            const expr = func_body.statements[0].data.Expression;
            if (expr) |e| {
                // Check if it's a simple binary operation
                return self.isSimpleArithmeticExpression(e);
            }
        }

        // 3. Expression + Return pattern (like "a + b; return")
        if (func_body.statements.len == 2 and
            func_body.statements[0].data == .Expression and
            func_body.statements[1].data == .Return)
        {
            const expr = func_body.statements[0].data.Expression;
            if (expr) |e| {
                return self.isSimpleArithmeticExpression(e);
            }
        }

        return false;
    }

    fn isSimpleArithmeticExpression(self: *CallsHandler, expr: *ast.Expr) bool {
        _ = self;
        if (expr.data == .Binary) {
            const binary = expr.data.Binary;
            const left_is_var = if (binary.left) |l| l.data == .Variable else false;
            const right_is_var = if (binary.right) |r| r.data == .Variable else false;
            return left_is_var and right_is_var;
        }
        return false;
    }
};
