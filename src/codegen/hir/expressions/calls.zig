const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const module_graph = @import("../../../module/graph.zig");
const types = @import("../../../types/types.zig");
const Location = @import("../../../utils/reporting.zig").Location;
const HIRGenerator = @import("../soxa_generator.zig").HIRGenerator;
const SoxaStatements = @import("../soxa_statements.zig");
const HIRValue = @import("../soxa_values.zig").HIRValue;
const HIRType = @import("../soxa_types.zig").HIRType;
const HeapCopyKind = @import("../soxa_types.zig").HeapCopyKind;
const ScopeKind = @import("../soxa_types.zig").ScopeKind;
const Slot = @import("../soxa_types.zig").Slot;
const HIRInstruction = @import("../soxa_instructions.zig").HIRInstruction;
const ArithOp = @import("../soxa_instructions.zig").ArithOp;
const CallKind = @import("../soxa_instructions.zig").CallKind;
const ErrorCode = @import("../../../utils/errors.zig").ErrorCode;
const ErrorList = @import("../../../utils/errors.zig").ErrorList;
const builtin_methods = @import("../../../runtime/builtin_methods.zig");
const ModuleCall = @import("../module_call.zig");
const StructsHandler = @import("structs.zig").StructsHandler;

pub const CallsHandler = struct {
    generator: *HIRGenerator,

    pub fn init(generator: *HIRGenerator) CallsHandler {
        return .{ .generator = generator };
    }

    /// Store the value on top of the stack — `target` after an in-place
    /// change — back into the variable, alias parameter or `this` it names,
    /// converted to that storage's type.
    fn storeBack(self: *CallsHandler, target: *ast.Expr, heap_copy: HeapCopyKind) !void {
        const expected_type = try self.generator.convertForStoreBack(target);
        try self.generator.storeTo(target, expected_type, heap_copy);
    }

    pub fn generateFunctionCall(self: *CallsHandler, function_call: ast.Expr.Data, preserve_result: bool, should_pop_after_use: bool) !void {
        const call_data = function_call.FunctionCall;

        const target = ModuleCall.classifyCallTarget(self.generator, call_data.callee) catch {
            self.generator.reporter.reportCompileError(
                call_data.callee.base.location(),
                ErrorCode.INTERNAL_ERROR,
                "call target was not resolved by analysis. This is a compiler bug, not an error in the program",
                .{},
            );
            return ErrorList.UnsupportedFunctionCallType;
        };

        switch (target) {
            .function => |callee| try self.emitResolvedFunctionCall(callee, function_call, preserve_result, should_pop_after_use),
            .static_method => |callee| try self.emitResolvedFunctionCall(callee, function_call, preserve_result, should_pop_after_use),
            .method => |method| try self.emitMethodCall(method.callee, method.receiver, call_data.arguments, preserve_result),
        }
    }

    /// An instance method call: the receiver (as an alias) and then the
    /// arguments.
    fn emitMethodCall(
        self: *CallsHandler,
        callee: ModuleCall.Callee,
        receiver: *ast.Expr,
        arguments: []const ast.CallArgument,
        preserve_result: bool,
    ) !void {
        try self.generator.pushStructReceiver(receiver);
        for (arguments) |arg| {
            try self.generator.generateExpression(arg.expr, true, false);
        }
        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = callee.index,
                .qualified_name = callee.link_name,
                .arg_count = @intCast(arguments.len + 1),
                .call_kind = .DoxaFunction,
                .return_type = self.generator.functionInfoByLink(callee.link_name).?.return_type,
            },
        });
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
    }

    /// Convert the argument just pushed, of type `value_type`, to the type of
    /// the parameter it binds, so an inlined body's parameter store and a
    /// call's argument both receive a value of the parameter's type.
    fn convertArgument(self: *CallsHandler, info: ?HIRGenerator.FunctionInfo, arg_index: usize, value_type: HIRType) !void {
        const callee = info orelse return;
        if (arg_index >= callee.param_types.len) return;
        try self.generator.convertValue(value_type, callee.param_types[arg_index]);
    }

    fn emitResolvedFunctionCall(
        self: *CallsHandler,
        callee: ModuleCall.Callee,
        function_call: ast.Expr.Data,
        preserve_result: bool,
        should_pop_after_use: bool,
    ) !void {
        const call_data = function_call.FunctionCall;
        const function_name = callee.link_name;
        const call_kind = callee.kind;
        const function_index: ?u32 = callee.index;

        var arg_emitted_count: u32 = 0;

        // A concrete member variable passed to a union/group `^` parameter is
        // boxed into a temporary at the call site, the temporary is aliased,
        // and the member is written back after the call. Alias binding is by
        // reference, and a plain variable's storage is not a `%DoxaValue` box,
        // so the callee cannot read or write the union layout directly.
        const finfo_opt = switch (call_kind) {
            .DoxaFunction, .ZigFunction => self.generator.functionInfoByLink(function_name),
            .BuiltinFunction => null,
        };
        const AliasWriteback = struct {
            /// The call-site temporary the member was boxed into.
            box_slot: Slot,
            box_name: []const u8,
            /// The aliased argument, a name, the member is written back to.
            target: *ast.Expr,
            member_type: HIRType,
            /// The parameter type the temporary is boxed as.
            box_type: HIRType,
        };
        var alias_writebacks = std.array_list.Managed(AliasWriteback).init(self.generator.allocator);
        defer alias_writebacks.deinit();

        for (call_data.arguments, 0..) |arg, arg_index| {
            if (arg.expr.data == .DefaultArgPlaceholder) {
                if (self.generator.resolveDefaultArgument(function_name, arg_index)) |default_expr| {
                    try self.generator.generateExpression(default_expr, true, false);
                    try self.convertArgument(finfo_opt, arg_index, try self.generator.typeOf(default_expr));
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
                        // When the callee's parameter is a boxed (union/group)
                        // alias but the aliased storage is a concrete member,
                        // forwarding the storage would hand the callee a raw
                        // concrete layout it reads as a `%DoxaValue`. Box the
                        // member into a call-site temporary and write it back
                        // after the call; matching storage is passed directly.
                        if (finfo_opt) |info| {
                            if (arg_index < info.param_types.len) {
                                const param_type = info.param_types[arg_index];
                                const member_type = try self.generator.bindingTypeOf(&arg.expr.base);
                                if (param_type.isBoxed() and !member_type.eql(param_type)) {
                                    const box_name = try std.fmt.allocPrint(self.generator.allocator, "__doxa_alias_box_{d}", .{self.generator.instructions.items.len});
                                    const box_slot = self.generator.tempSlot();
                                    try self.generator.loadName(&arg.expr.base, var_token.lexeme);
                                    try self.generator.convertValue(try self.generator.typeOf(arg.expr), param_type);
                                    try self.generator.instructions.append(.{ .StoreVar = .{ .slot = box_slot, .var_name = box_name, .scope_kind = .Local, .module_context = null, .expected_type = param_type, .heap_copy = .keep } });
                                    try self.generator.instructions.append(.{ .PushStorageId = .{ .slot = box_slot, .var_name = box_name, .scope_kind = .Local } });
                                    try alias_writebacks.append(.{ .box_slot = box_slot, .box_name = box_name, .target = arg.expr, .member_type = member_type, .box_type = param_type });
                                    arg_emitted_count += 1;
                                    continue;
                                }
                            }
                        }
                        try self.generator.pushStorageOfName(&arg.expr.base, var_token.lexeme);
                        arg_emitted_count += 1;
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
                    try self.generator.generateExpression(arg.expr, true, should_pop_after_use);
                    try self.convertArgument(finfo_opt, arg_index, try self.generator.typeOf(arg.expr));
                    arg_emitted_count += 1;
                }
            }
        }

        const return_type = self.generator.calleeReturnType(callee);

        if (call_kind == .DoxaFunction) {
            if (try self.tryInlineFunction(function_name, call_kind)) {
                if (!preserve_result) {
                    try self.generator.instructions.append(.Pop);
                }
                return;
            }
        }

        try self.generator.instructions.append(.{
            .Call = .{
                .function_index = function_index,
                .qualified_name = function_name,
                .arg_count = arg_emitted_count,
                .call_kind = call_kind,
                .return_type = return_type,
            },
        });
        // Unbox each call-site temporary and store the member back into the
        // caller's plain variable. The call result (if any) stays on the stack
        // across these stores.
        for (alias_writebacks.items) |wb| {
            try self.generator.instructions.append(.{ .LoadVar = .{ .slot = wb.box_slot, .var_name = wb.box_name, .scope_kind = .Local, .module_context = null } });
            try self.generator.convertValue(wb.box_type, wb.member_type);
            // Written back through an alias, the member is re-homed into the
            // arena that owns the aliased variable; `.rehome` preserves array
            // identity and clones a fresh string out of the transient box
            // arena. A local of this frame keeps it.
            const slot = try self.generator.slotOf(&wb.target.base);
            const heap_copy: HeapCopyKind = if (self.generator.alias_params.contains(slot)) .rehome else .keep;
            try self.generator.storeName(&wb.target.base, wb.target.data.Variable.lexeme, wb.member_type, heap_copy);
        }
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
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
                .function_index = null,
                .qualified_name = name,
                .arg_count = @intCast(arguments.len),
                .call_kind = .BuiltinFunction,
                .return_type = return_type,
            } });
            return return_type;
        }
        return null;
    }

    pub fn generateInternalCall(self: *CallsHandler, expr: *ast.Expr, preserve_result: bool) !void {
        const call = expr.data.InternalCall;
        const name = call.method.lexeme;

        // Receiver-first argument vector: the shape every built-in method below
        // is written against. `@std` takes none of its own; the receiver the
        // parser synthesizes for its empty argument list is a placeholder.
        const args: []const *ast.Expr = if (std.mem.eql(u8, name, "std"))
            &[_]*ast.Expr{}
        else blk: {
            const vector = try self.generator.allocator.alloc(*ast.Expr, call.arguments.len + 1);
            vector[0] = call.receiver;
            @memcpy(vector[1..], call.arguments);
            break :blk vector;
        };
        defer self.generator.allocator.free(args);

        if (std.mem.eql(u8, name, "type")) {
            try self.validateBuiltinArgCount(name, args.len);
            const arg = args[0];

            // The analyzer's type for the operand, shown by its declared name.
            const type_info = self.generator.semantic.getCachedExprType(arg) orelse return ErrorList.MissingExpressionType;
            const type_name: []const u8 = if (type_info.custom_type) |custom| custom.displayName() else switch (type_info.base) {
                .Int => "int",
                .Float => "float",
                .String => "string",
                .Tetra => "tetra",
                .Byte => "byte",
                .Nothing => "nothing",
                .Array => "array",
                .Union => "union",
                .Map => "map",
                .Function => "function",
                .Struct => "struct",
                .Enum => "enum",
                .Custom => "custom",
            };
            const type_value = HIRValue{ .string = type_name };
            const const_idx = try self.generator.addConstant(type_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = type_value, .constant_id = const_idx } });
        } else if (std.mem.eql(u8, name, "length")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            var t = self.generator.inferTypeFromExpression(args[0]);
            var use_array_len = t == .Array;
            // A union narrowed by `as` to a single array member behaves like an
            // array for @length (e.g. `x as string then ... else @length(x)`).
            if (t == .Union and t.Union.members.len == 1 and t.Union.members[0].* == .Array) {
                use_array_len = true;
            }
            if (args[0].data == .Variable) {
                const var_name = args[0].data.Variable.lexeme;
                if (self.generator.getTrackedVariableType(var_name)) |tracked| {
                    if (t == .Unknown) t = tracked;
                    use_array_len = use_array_len or tracked == .Array;
                    if (tracked == .Union and tracked.Union.members.len == 1 and tracked.Union.members[0].* == .Array) {
                        use_array_len = true;
                    }
                }
                // Match VM: length() on arrays uses element count even when the static annotation is wrong.
                if (!use_array_len and self.generator.symbol_table.getTrackedArrayElementType(var_name) != null) {
                    use_array_len = true;
                }
            }
            if (use_array_len) {
                try self.generator.instructions.append(.ArrayLen);
            } else {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Length } });
            }
        } else if (std.mem.eql(u8, name, "int")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToInt } });
        } else if (std.mem.eql(u8, name, "float")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToFloat } });
        } else if (std.mem.eql(u8, name, "string")) {
            try self.validateBuiltinArgCount(name, args.len);
            // B2: @string(x) of a struct prints it, so its descriptor must stay.
            const value_type = self.generator.inferTypeFromExpression(args[0]);
            self.generator.markReflectedType(value_type);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToString, .value_type = value_type } });
        } else if (std.mem.eql(u8, name, "pack")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Pack } });
        } else if (std.mem.eql(u8, name, "unpack")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{ .StringOp = .{ .op = .Unpack } });
        } else if (std.mem.eql(u8, name, "byte")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            const t = self.generator.inferTypeFromExpression(args[0]);
            if (t == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else if (std.mem.eql(u8, name, "push")) {
            try self.validateBuiltinArgCount(name, args.len);
            if (args[0].data == .Variable) {
                const var_name = args[0].data.Variable.lexeme;
                const storage_kind = self.generator.getTrackedArrayStorageKind(var_name) orelse .dynamic;
                if (storage_kind == .fixed or storage_kind == .const_literal) {
                    const location = args[0].base.location();
                    self.generator.reporter.reportCompileError(location, ErrorCode.INVALID_ARRAY_TYPE, "cannot push to a fixed-size array", .{});
                    return ErrorList.UnsupportedArrayType;
                }
            }
            const target_type = self.generator.inferTypeFromExpression(args[0]);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.generateExpression(args[1], true, false);
            if (target_type == .String) {
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Concat } });
            } else {
                try self.generator.instructions.append(.{ .ArrayPush = .{ .resize_behavior = .Double } });
            }
            if (args[0].data == .Variable) {
                const heap_copy: HeapCopyKind = if (target_type == .String) .rehome else .keep;
                try self.storeBack(args[0], heap_copy);
            } else if (args[0].data == .FieldAccess) {
                const fa = args[0].data.FieldAccess;
                try self.generator.generateExpression(fa.object, true, false);
                try self.generator.instructions.append(.Swap);
                const container_type = self.generator.inferTypeFromExpression(fa.object);
                var structs_handler = StructsHandler.init(self.generator);
                const slot = try structs_handler.fieldSlot(fa.object, fa.field);
                try self.generator.instructions.append(.{
                    .SetField = .{
                        .field_name = fa.field.lexeme,
                        .container_type = container_type,
                        .struct_id = slot.struct_id,
                        .field_index = slot.index,
                        .field_type = .Unknown,
                        .nested_struct_id = null,
                    },
                });
                if (fa.object.data == .Variable or fa.object.data == .This) {
                    try self.storeBack(fa.object, .keep);
                }
            }
            const nothing_const_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx } });

            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "pop")) {
            try self.validateBuiltinArgCount(name, args.len);
            const target_type = self.generator.inferTypeFromExpression(args[0]);
            try self.generator.generateExpression(args[0], true, false);
            if (target_type == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .Pop } });
            } else {
                try self.generator.instructions.append(.ArrayPop);
            }
            if (args[0].data == .Variable) {

                if (target_type == .String) {
                    try self.generator.instructions.append(.Swap);
                    try self.storeBack(args[0], .rehome);
                } else {
                    try self.generator.instructions.append(.Swap);
                    try self.storeBack(args[0], .keep);
                }
            }
        } else if (std.mem.eql(u8, name, "insert")) {
            try self.validateBuiltinArgCount(name, args.len);
            const target_type = self.generator.inferTypeFromExpression(args[0]);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.generateExpression(args[1], true, false);
            try self.generator.generateExpression(args[2], true, false);
            try self.generator.instructions.append(.ArrayInsert);
            if (args[0].data == .Variable) {
                // A string insert produces a fresh immutable buffer, so an alias
                // store must re-home it into the caller's arena. An array insert
                // mutates in place and must keep identity.
                const heap_copy: HeapCopyKind = if (target_type == .String) .rehome else .keep;
                try self.storeBack(args[0], heap_copy);
            } else {
                try self.generator.instructions.append(.Pop);
            }
            const nothing_const_idx2 = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx2 } });
            if (!preserve_result) try self.generator.instructions.append(.Pop);
        } else if (std.mem.eql(u8, name, "remove")) {
            try self.validateBuiltinArgCount(name, args.len);
            const target_type = self.generator.inferTypeFromExpression(args[0]);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.generateExpression(args[1], true, false);
            try self.generator.instructions.append(.ArrayRemove);
            if (args[0].data == .Variable) {

                const heap_copy: HeapCopyKind = if (target_type == .String) .rehome else .keep;
                try self.generator.instructions.append(.Swap);
                try self.storeBack(args[0], heap_copy);
            } else {
                try self.generator.instructions.append(.Swap);
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "slice")) {
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.generateExpression(args[1], true, false);
            try self.generator.generateExpression(args[2], true, false);
            try self.generator.instructions.append(.ArraySlice);
        } else if (std.mem.eql(u8, name, "clear")) {
            try self.validateBuiltinArgCount(name, args.len);
            const target_type = self.generator.inferTypeFromExpression(args[0]);

            if (target_type == .String) {
                if (args[0].data == .Variable) {
                    const empty_str_value = HIRValue{ .string = "" };
                    const empty_str_idx = try self.generator.addConstant(empty_str_value);
                    try self.generator.instructions.append(.{ .Const = .{ .value = empty_str_value, .constant_id = empty_str_idx } });
                    try self.storeBack(args[0], .rehome);
                }
            } else {
                try self.generator.generateExpression(args[0], true, false);
                try self.generator.instructions.append(.{
                    .Call = .{
                        .function_index = null,
                        .qualified_name = "clear",
                        .arg_count = 1,
                        .call_kind = .BuiltinFunction,
                        .return_type = .Nothing,
                    },
                });
                // `doxa_clear` empties the collection in place; its `nothing`
                // result is not the collection, so nothing is stored back.
                try self.generator.instructions.append(.Pop);
            }
            const nothing_const_idx = try self.generator.addConstant(HIRValue.nothing);
            try self.generator.instructions.append(.{ .Const = .{ .value = HIRValue.nothing, .constant_id = nothing_const_idx } });

            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "find")) {
            try self.validateBuiltinArgCount(name, args.len);
            // Evaluate receiver/collection and search value
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.generateExpression(args[1], true, false);
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = null,
                    .qualified_name = "find",
                    .arg_count = 2,
                    .call_kind = .BuiltinFunction,
                    .return_type = .Int,
                },
            });
        } else if (std.mem.eql(u8, name, "exit")) {
            // Use centralized data structure for simple builtin calls
            _ = try self.generateSimpleBuiltinCall(name, args);
        } else if (std.mem.eql(u8, name, "panic")) {
            // Use centralized data structure for simple builtin calls
            _ = try self.generateSimpleBuiltinCall(name, args);
        } else if (std.mem.eql(u8, name, "print")) {
            // @print(string) - emits the string to stdout
            try self.validateBuiltinArgCount(name, args.len);
            try self.generator.generateExpression(args[0], true, false);
            try self.generator.instructions.append(.{
                .Call = .{
                    .function_index = null,
                    .qualified_name = "print",
                    .arg_count = 1,
                    .call_kind = .BuiltinFunction,
                    .return_type = .Nothing,
                },
            });
            // @print returns nothing (already pushed by the VM)
            if (!preserve_result) {
                try self.generator.instructions.append(.Pop);
            }
        } else if (std.mem.eql(u8, name, "std")) {
            // `@std()` is the standard library's specifier, importable like
            // any other; it names no install location.
            const path_value = HIRValue{ .string = module_graph.std_specifier };
            const const_idx = try self.generator.addConstant(path_value);
            try self.generator.instructions.append(.{ .Const = .{ .value = path_value, .constant_id = const_idx } });
        } else {
            self.generator.reporter.reportCompileError(
                expr.base.location(),
                ErrorCode.NOT_IMPLEMENTED,
                "unimplemented built-in method '@{s}'",
                .{name},
            );
            return error.NotImplemented;
        }
    }

    fn tryInlineFunction(self: *CallsHandler, function_name: []const u8, call_kind: CallKind) !bool {
        if (call_kind != .DoxaFunction) return false;
        const func_body = self.generator.findFunctionBody(function_name) orelse return false;
        if (!self.shouldInlineFunction(func_body)) return false;

        const scope_id = self.generator.nextScopeId();
        try self.generator.instructions.append(.{
            .EnterScope = .{ .scope_id = scope_id, .var_count = @intCast(func_body.function_info.arity) },
        });

        var i: usize = func_body.function_params.len;
        const is_method = func_body.function_info.arity > func_body.function_params.len;
        while (i > 0) {
            i -= 1;
            const param = func_body.function_params[i];
            // `shouldInlineFunction` refuses a `^` parameter.
            const expected_t = func_body.function_info.param_types[if (is_method) i + 1 else i];
            try self.generator.instructions.append(.{
                .StoreVar = .{
                    .slot = try self.generator.paramSlot(param),
                    .var_name = param.name.lexeme,
                    .scope_kind = .Local,
                    .module_context = null,
                    .expected_type = expected_t,
                },
            });
        }

        try self.inlineBody(func_body);

        try self.generator.instructions.append(.{ .ExitScope = .{ .scope_id = scope_id } });

        return true;
    }

    /// Whether a value of `return_type` is heap-allocated and therefore cannot
    /// be inlined (its result would be freed with the inline scope before the
    /// caller takes it). `Unknown` is treated as heap so an unresolved return
    /// type is never inlined into a dangling value.
    fn isHeapReturnType(return_type: HIRType) bool {
        return switch (return_type) {
            .String, .Array, .Map, .Struct, .Union, .Group, .Unknown => true,
            else => false,
        };
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
            try self.generator.loadName(&expr.base, expr.data.Variable.lexeme);
        } else {
            try self.generator.generateExpression(expr, true, true);
        }
    }

    fn shouldInlineFunction(self: *CallsHandler, func_body: *const HIRGenerator.FunctionBody) bool {
        if (func_body.statements.len > 3) return false; // Too complex

        // A `^` parameter is bound by reference to the caller's storage, which
        // the inliner does not bind. Call functions that take one instead.
        // TODO: bind an inlined `^` parameter's slot to the argument's storage.
        for (func_body.function_info.param_is_alias) |is_alias| {
            if (is_alias) return false;
        }

        // A heap-returning body constructs its result inside the inline scope,
        // which is torn down before the caller clones the value out. Such a
        // result must go through the ordinary call path, where clone-on-return
        // copies it into the caller's arena before the callee scope is freed.
        // Only scalar results can be inlined without a dangling value.
        if (isHeapReturnType(func_body.function_info.return_type)) return false;

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
