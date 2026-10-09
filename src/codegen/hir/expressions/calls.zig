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

        // A member-typed variable lent to a union `^` parameter is boxed into
        // a temporary at the call site, the temporary is lent, and the member
        // is read back out after the call. Analysis admits such a loan only
        // when the parameter preserves its member, so the box still holds the
        // member the variable's storage is typed as.
        const MemberLoan = struct {
            /// The call-site temporary the member is boxed into.
            box: HIRGenerator.Place,
            /// The variable lent, a name.
            target: *ast.Expr,
            member_type: HIRType,
            box_type: HIRType,
        };
        var member_loans = std.array_list.Managed(MemberLoan).init(self.generator.allocator);
        defer member_loans.deinit();
        const finfo_opt = switch (call_kind) {
            .DoxaFunction, .ZigFunction => self.generator.functionInfoByLink(function_name),
            .BuiltinFunction => null,
        };

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
                        const member_type = try self.generator.bindingTypeOf(&arg.expr.base);
                        const param_type = if (finfo_opt) |info| info.param_types[arg_index] else member_type;
                        if (member_type.eql(param_type)) {
                            try self.generator.pushStorageOfName(&arg.expr.base, var_token.lexeme);
                        } else {
                            const box_name = try std.fmt.allocPrint(self.generator.allocator, "__doxa_member_loan_{d}", .{self.generator.instructions.items.len});
                            const box_slot = self.generator.tempSlot();
                            try self.generator.loadName(&arg.expr.base, var_token.lexeme);
                            try self.generator.convertValue(try self.generator.typeOf(arg.expr), param_type);
                            const box_place = try self.generator.placeOf(box_slot, box_name, false);
                            try self.generator.instructions.append(.{ .StoreDecl = .{
                                .slot = box_slot,
                                .var_name = box_place.var_name,
                                .scope_kind = box_place.scope_kind,
                                .module_context = null,
                                .declared_type = param_type,
                                .is_const = false,
                            } });
                            try self.generator.instructions.append(.{ .PushStorageId = .{ .slot = box_slot, .var_name = box_place.var_name, .scope_kind = box_place.scope_kind } });
                            try member_loans.append(.{ .box = box_place, .target = arg.expr, .member_type = member_type, .box_type = param_type });
                        }
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

        const return_type = try self.generator.calleeReturnType(callee);

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
        // The call result, if any, stays on the stack across the read-backs.
        for (member_loans.items) |loan| {
            try self.generator.instructions.append(.{ .LoadVar = .{ .slot = loan.box.slot, .var_name = loan.box.var_name, .scope_kind = loan.box.scope_kind, .module_context = null } });
            try self.generator.convertValue(loan.box_type, loan.member_type);
            // Read back through an alias, the member is re-homed into the
            // arena that owns the aliased variable; a local of this frame
            // keeps it.
            const heap_copy: HeapCopyKind = if (self.generator.alias_params.contains(try self.generator.slotOf(&loan.target.base))) .rehome else .keep;
            try self.generator.storeName(&loan.target.base, loan.target.data.Variable.lexeme, loan.member_type, heap_copy);
        }
        if (!preserve_result) {
            try self.generator.instructions.append(.Pop);
        }
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

    /// A builtin that is one runtime call on its arguments, whose value is
    /// what analysis typed the call as.
    fn generateSimpleBuiltinCall(self: *CallsHandler, expr: *ast.Expr, name: []const u8, arguments: []const *ast.Expr) !void {
        try self.validateBuiltinArgCount(name, arguments.len);
        for (arguments) |arg| {
            try self.generator.generateExpression(arg, true, false);
        }
        try self.generator.instructions.append(.{ .Call = .{
            .function_index = null,
            .qualified_name = name,
            .arg_count = @intCast(arguments.len),
            .call_kind = .BuiltinFunction,
            .return_type = try self.generator.typeOf(expr),
        } });
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
            const type_info = try self.generator.typeInfoOf(arg);
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
            const use_array_len = try self.generator.typeOf(args[0]) == .Array;
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
            const value_type = try self.generator.typeOf(args[0]);
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
            const t = try self.generator.typeOf(args[0]);
            if (t == .String) {
                try self.generator.instructions.append(.{ .StringOp = .{ .op = .ToByte } });
            } else {
                try self.generator.instructions.append(.{ .Convert = .{ .from_type = t, .to_type = .Byte } });
            }
        } else if (std.mem.eql(u8, name, "push")) {
            try self.validateBuiltinArgCount(name, args.len);
            const target_type = try self.generator.typeOf(args[0]);
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
                const container_type = try self.generator.typeOf(fa.object);
                var structs_handler = StructsHandler.init(self.generator);
                const slot = try structs_handler.fieldSlot(fa.object, fa.field);
                try self.generator.instructions.append(.{
                    .SetField = .{
                        .field_name = fa.field.lexeme,
                        .container_type = container_type,
                        .struct_id = slot.struct_id,
                        .field_index = slot.index,
                        .field_type = slot.hir_type,
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
            const target_type = try self.generator.typeOf(args[0]);
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
            const target_type = try self.generator.typeOf(args[0]);
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
            const target_type = try self.generator.typeOf(args[0]);
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
            const target_type = try self.generator.typeOf(args[0]);

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
        } else if (std.mem.eql(u8, name, "exit") or std.mem.eql(u8, name, "panic")) {
            try self.generateSimpleBuiltinCall(expr, name, args);
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
