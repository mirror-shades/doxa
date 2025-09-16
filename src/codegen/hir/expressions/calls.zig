const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const CallKind = @import("../soxa_instructions.zig").CallKind;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;

/// Handle function calls and method calls
pub const CallsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) CallsHandler {
        return .{ .generator = generator };
    }

    /// Generate HIR for function call expressions
    pub fn generateFunctionCall(self: *CallsHandler, function_call: ast.Expr.Data, should_pop_after_use: bool) !void {
        const call_data = function_call.FunctionCall;

        // Extract function name and determine call type
        var function_name: []const u8 = "unknown";
        var call_kind: CallKind = .LocalFunction;
        var function_index: u32 = 0;

        // Distinguish between module function, internal method, and regular function
        switch (call_data.callee.data) {
            .FieldAccess => |field_access| {
                // Module function call: namespace.func(...)
                if (field_access.object.data == .Variable and self.generator.isModuleNamespace(field_access.object.data.Variable.lexeme)) {
                    const object_name = field_access.object.data.Variable.lexeme;
                    function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ object_name, field_access.field.lexeme });

                    if (std.mem.eql(u8, function_name, "safeMath.safeAdd")) {
                        call_kind = .ModuleFunction;
                    } else if (self.generator.getFunctionIndex(function_name)) |idx| {
                        function_index = idx;
                        call_kind = .LocalFunction;
                    } else {
                        call_kind = .ModuleFunction;
                    }

                    // Emit only the arguments for module function
                    for (call_data.arguments) |arg| {
                        try self.generator.generateExpression(arg.expr, true, should_pop_after_use);
                    }
                } else if (field_access.object.data == .Variable) {
                    // Static struct method: TypeName.method(...)
                    const type_name = field_access.object.data.Variable.lexeme;
                    if (self.generator.isCustomType(type_name)) |ct| {
                        if (ct.kind == .Struct) {
                            // Qualify as TypeName.method for lookup
                            function_name = try std.fmt.allocPrint(self.generator.allocator, "{s}.{s}", .{ type_name, field_access.field.lexeme });
                            if (self.generator.getFunctionIndex(function_name)) |idx| {
                                function_index = idx;
                                call_kind = .LocalFunction;
                                // Emit only arguments (no receiver)
                                for (call_data.arguments) |arg| {
                                    try self.generator.generateExpression(arg.expr, true, should_pop_after_use);
                                }
                                // Emit the call now
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

                    // Static struct constructor sugar: Point.New(a, b) -> StructNew Point { x: a, y: b }
                    const type_name2 = field_access.object.data.Variable.lexeme;
                    if (self.generator.isCustomType(type_name2)) |ct2| {
                        if (ct2.kind == .Struct and std.mem.eql(u8, field_access.field.lexeme, "New")) {
                            try self.generateStructConstructorCall(type_name2, call_data.arguments);
                            return;
                        }
                    }

                    // Built-in/internal method on a receiver: delegate and return early
                    try self.generator.generateInternalMethodCall(field_access.field, field_access.object, call_data.arguments, should_pop_after_use);
                    return;
                } else {
                    // Static struct constructor sugar: Point.New(a, b) -> StructNew Point { x: a, y: b }
                    if (field_access.object.data == .Variable) {
                        const type_name = field_access.object.data.Variable.lexeme;
                        if (self.generator.isCustomType(type_name)) |ct| {
                            if (ct.kind == .Struct and std.mem.eql(u8, field_access.field.lexeme, "New")) {
                                try self.generateStructConstructorCall(type_name, call_data.arguments);
                                return;
                            }
                        }
                    }

                    // Built-in/internal method on a receiver: delegate and return early
                    try self.generator.generateInternalMethodCall(field_access.field, field_access.object, call_data.arguments, should_pop_after_use);
                    return;
                }
            },
            .Variable => |var_token| {
                function_name = var_token.lexeme;
                if (self.generator.getFunctionIndex(function_name)) |index| {
                    function_index = index;
                    call_kind = .LocalFunction;
                } else {
                    call_kind = .BuiltinFunction;
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

        // Generate arguments (default placeholders resolved)
        var arg_emitted_count: u32 = 0;
        for (call_data.arguments, 0..) |arg, arg_index| {
            if (arg.expr.data == .DefaultArgPlaceholder) {
                if (self.generator.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                    try self.generator.generateExpression(default_expr, true, false);
                    arg_emitted_count += 1;
                } else {
                    const location = if (call_data.callee.base.span) |span| span.location else Location{
                        .file = "",
                        .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 },
                    };
                    self.generator.reporter.reportCompileError(location, ErrorCode.NO_DEFAULT_VALUE_FOR_PARAMETER, "No default value for parameter {} in function '{s}'", .{ arg_index, function_name });
                }
            } else {
                if (arg.is_alias) {
                    // For alias arguments, we need to push the storage ID of the variable
                    if (arg.expr.data == .Variable) {
                        const var_token = arg.expr.data.Variable;
                        // Find the existing variable to get its storage ID
                        const maybe_idx: ?u32 = self.generator.symbol_table.getVariable(var_token.lexeme);
                        if (maybe_idx) |var_idx| {
                            try self.generator.instructions.append(.{
                                .PushStorageId = .{
                                    .var_index = var_idx,
                                    .var_name = var_token.lexeme,
                                    .scope_kind = .Local, // This will be resolved at runtime
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
                        // Alias argument must be a variable
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
        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = function_index,
                .qualified_name = function_name,
                .arg_count = arg_emitted_count,
                .call_kind = call_kind,
                .target_module = null,
                .return_type = return_type,
            },
        });
    }

    /// Generate HIR for builtin call expressions
    pub fn generateBuiltinCall(self: *CallsHandler, bc: ast.Expr.Data, preserve_result: bool) !void {
        const builtin_data = bc.BuiltinCall;
        const name = builtin_data.function.lexeme;

        // Implement minimal built-ins needed now: @type and @length
        if (std.mem.eql(u8, name, "type")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            const arg = builtin_data.arguments[0];
            const inferred_type = self.generator.inferTypeFromExpression(arg);
            const type_name = switch (inferred_type) {
                .Int => "int",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Byte => "byte",
                .Nothing => "nothing",
                .Array => "array",
                .Union => "union",
                .Struct => blk: {
                    if (arg.data == .Variable) {
                        const var_name = arg.data.Variable.lexeme;
                        if (self.generator.isCustomType(var_name)) |custom_type| {
                            if (custom_type.kind == .Struct) break :blk "struct";
                        }
                        if (self.generator.symbol_table.getVariableCustomType(var_name)) |custom_type_name| break :blk custom_type_name;
                    }
                    if (arg.data == .FieldAccess) {
                        const field_access = arg.data.FieldAccess;
                        if (field_access.object.data == .Variable) {
                            const obj_name = field_access.object.data.Variable.lexeme;
                            // If the object is a variable of a custom struct type, resolve the field type
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
                    if (arg.data == .Variable) {
                        const var_name = arg.data.Variable.lexeme;
                        if (self.generator.isCustomType(var_name)) |custom_type| {
                            if (custom_type.kind == .Enum) break :blk "enum";
                        }
                        if (self.generator.symbol_table.getVariableCustomType(var_name)) |custom_type_name| break :blk custom_type_name;
                    }
                    if (arg.data == .FieldAccess) {
                        const field_access = arg.data.FieldAccess;
                        if (field_access.object.data == .Variable) {
                            const obj_name = field_access.object.data.Variable.lexeme;
                            if (self.generator.isCustomType(obj_name)) |custom_type| {
                                if (custom_type.kind == .Enum) break :blk obj_name;
                            }
                            // If this is a struct field access whose field type is an enum,
                            // prefer returning the enum's custom type name (e.g., "Species").
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
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            // Evaluate argument to leave value on stack for ops
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            const t = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            if (t == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Length } });
            } else {
                // default to array length
                try self.generator.instructions.append(.ArrayLen);
            }
        } else if (std.mem.eql(u8, name, "int")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            // Evaluate argument and convert to int
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
        } else if (std.mem.eql(u8, name, "float")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            // Evaluate argument and convert to float
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
        } else if (std.mem.eql(u8, name, "string")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            // Evaluate argument and convert to string
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString } });
        } else if (std.mem.eql(u8, name, "byte")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            // Evaluate argument
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            const t = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            if (t == .String) {
                // string -> byte (single char/parsed) using ToByte
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                // numeric -> byte
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else if (std.mem.eql(u8, name, "push")) {
            if (builtin_data.arguments.len != 2) return error.InvalidArgumentCount;
            const target_type = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            if (target_type == .String) {
                // For strings: receiver is on stack first, then value; Concat expects second on top
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
            } else {
                try self.generator.instructions.append(.{ .ArrayPush = .{ .resize_behavior = .Double } });
            }
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                // Store the modified array back to the variable
                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = .Local, .module_context = null, .expected_type = expected_type } });
            }
            // Push nothing as the return value
            const nothing_const_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx } });

            // If the caller doesn't need the result, pop it
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "pop")) {
            if (builtin_data.arguments.len != 1) return error.InvalidArgumentCount;
            const target_type = self.generator.inferTypeFromExpression(builtin_data.arguments[0]);
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            if (target_type == .String) {
                // String pop: remove last character and return it as a string
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Pop } });
            } else {
                // Default: array pop
                try self.generator.instructions.append(.ArrayPop);
            }
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = .Local, .module_context = null, .expected_type = expected_type } });
            }
        } else if (std.mem.eql(u8, name, "insert")) {
            // @insert(container, index, value) -> returns nothing; stores updated container
            if (builtin_data.arguments.len != 3) return error.InvalidArgumentCount;
            // Evaluate receiver, index, value (stack: container, index, value)
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.generateExpression(builtin_data.arguments[2], true, false);
            try self.generator.instructions.append(.ArrayInsert);
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                // Store updated container back to the variable
                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = .Local, .module_context = null, .expected_type = expected_type } });
            } else {
                // Not a variable: just discard updated container
                try self.generator.instructions.append(.Pop);
            }
            // Insert returns nothing
            const nothing_const_idx2 = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx2 } });
            if (!preserve_result) try self.generator.instructions.append(.Pop);
        } else if (std.mem.eql(u8, name, "remove")) {
            // @remove(container, index) -> returns removed element or error union; stores updated container
            if (builtin_data.arguments.len != 2) return error.InvalidArgumentCount;
            // Evaluate receiver, index (stack: container, index)
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.instructions.append(.ArrayRemove);
            if (builtin_data.arguments[0].data == .Variable) {
                const var_name = builtin_data.arguments[0].data.Variable.lexeme;
                const var_idx = try self.generator.getOrCreateVariable(var_name);
                const expected_type = self.generator.getTrackedVariableType(var_name) orelse .Unknown;
                // After ArrayRemove: stack [updated_container, removed_value]
                // Swap so container is on top, store it, leaving removed_value as result
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StoreVar = .{ .var_index = var_idx, .var_name = var_name, .scope_kind = .Local, .module_context = null, .expected_type = expected_type } });
            } else {
                // Not a variable: discard updated container, leave removed as result
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "slice")) {
            // @slice(container, start, length) -> returns sliced container
            if (builtin_data.arguments.len != 3) return error.InvalidArgumentCount;
            // Evaluate receiver, start, length (stack: container, start, length)
            try self.generator.generateExpression(builtin_data.arguments[0], true, false);
            try self.generator.generateExpression(builtin_data.arguments[1], true, false);
            try self.generator.generateExpression(builtin_data.arguments[2], true, false);
            try self.generator.instructions.append(.ArraySlice);
            // Result is the sliced container
        } else if (std.mem.eql(u8, name, "os")) {
            // @os() -> returns OS name as string
            if (builtin_data.arguments.len != 0) return error.InvalidArgumentCount;
            try self.generator.instructions.append(.{ .Call = .{ .function_index = 0, .qualified_name = "os", .arg_count = 0, .call_kind = .BuiltinFunction, .target_module = null, .return_type = .String } });
        } else if (std.mem.eql(u8, name, "arch")) {
            // @arch() -> returns architecture name as string
            if (builtin_data.arguments.len != 0) return error.InvalidArgumentCount;
            try self.generator.instructions.append(.{ .Call = .{ .function_index = 0, .qualified_name = "arch", .arg_count = 0, .call_kind = .BuiltinFunction, .target_module = null, .return_type = .String } });
        } else if (std.mem.eql(u8, name, "time")) {
            // @time() -> returns Unix timestamp as int
            if (builtin_data.arguments.len != 0) return error.InvalidArgumentCount;
            try self.generator.instructions.append(.{ .Call = .{ .function_index = 0, .qualified_name = "time", .arg_count = 0, .call_kind = .BuiltinFunction, .target_module = null, .return_type = .Int } });
        } else {
            // Fallback: no-op or error until implemented
            return error.NotImplemented;
        }
    }

    /// Generate HIR for internal method calls
    pub fn generateInternalCall(self: *CallsHandler, m: ast.Expr.Data) !void {
        const internal_data = m.InternalCall;

        // Generate HIR for compiler methods like @string, @length, @substring
        const name = internal_data.method.lexeme;
        if (std.mem.eql(u8, name, "substring")) {
            // Evaluate in VM-expected order: start, length, then receiver on top
            try self.generator.generateExpression(internal_data.arguments[0], true, false);
            try self.generator.generateExpression(internal_data.arguments[1], true, false);
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Substring } });
        } else if (std.mem.eql(u8, name, "string")) {
            // Evaluate receiver (the value to convert to string)
            try self.generator.generateExpression(internal_data.receiver, true, false);
            // Generate StringOp.ToString instruction
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString } });
        } else if (std.mem.eql(u8, name, "length")) {
            // Evaluate receiver (the value to get length of)
            try self.generator.generateExpression(internal_data.receiver, true, false);
            // Generate StringOp.Length instruction
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Length } });
        } else if (std.mem.eql(u8, name, "int")) {
            // Evaluate receiver and convert to int
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
        } else if (std.mem.eql(u8, name, "float")) {
            // Evaluate receiver and convert to float
            try self.generator.generateExpression(internal_data.receiver, true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
        } else if (std.mem.eql(u8, name, "byte")) {
            // Evaluate receiver
            try self.generator.generateExpression(internal_data.receiver, true, false);
            const t = self.generator.inferTypeFromExpression(internal_data.receiver);
            if (t == .String) {
                // string -> byte
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                // numeric -> byte
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else {
            // Unknown method - fallback to nothing
            const nothing_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_idx } });
        }
    }

    // Private helper methods
    fn generateStructConstructorCall(self: *CallsHandler, type_name: []const u8, arguments: []ast.CallArgument) !void {
        if (self.generator.isCustomType(type_name)) |ct| {
            const fields_info = ct.struct_fields orelse &[_]@import("../type_system.zig").TypeSystem.CustomTypeInfo.StructField{};
            const field_count: usize = fields_info.len;

            // Generate argument expressions in forward order, mirroring StructLiteral path
            var field_types = try self.generator.allocator.alloc(HIRType, field_count);
            defer self.generator.allocator.free(field_types);

            if (arguments.len != field_count) {
                // Fallback: generate what we have; missing fields default to Unknown
            }

            var i: usize = 0;
            while (i < field_count and i < arguments.len) : (i += 1) {
                const arg_expr = arguments[i].expr;
                try self.generator.generateExpression(arg_expr, true, false);
                field_types[field_count - 1 - i] = self.generator.inferTypeFromExpression(arg_expr);
                const fname = fields_info[i].name;
                const fname_const = try self.generator.addConstant(HIRValue{ .string = fname });
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = fname }, .constant_id = fname_const } });
            }

            // If fewer args than fields, pad remaining names with empty values and Unknown types
            while (i < field_count) : (i += 1) {
                // Push default value (nothing) then field name
                const nothing_id = try self.generator.addConstant(HIRValue.nothing);
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_id } });
                field_types[field_count - 1 - i] = .Unknown;
                const fname = fields_info[i].name;
                const fname_const = try self.generator.addConstant(HIRValue{ .string = fname });
                try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue{ .string = fname }, .constant_id = fname_const } });
            }

            // Emit StructNew
            try self.generator.instructions.append(.{ .StructNew = .{
                .type_name = type_name,
                .field_count = @intCast(field_count),
                .field_types = try self.generator.allocator.dupe(HIRType, field_types),
                .size_bytes = 0,
            } });
        }
    }
};
