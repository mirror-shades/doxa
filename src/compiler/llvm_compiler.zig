const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../lexer/token.zig");
const Reporting = @import("../utils/reporting.zig");
const ErrorList = Reporting.ErrorList;
const c = @import("./llvm_c.zig").c;

// This module provides an LLVM backend for Doxa
// It requires the LLVM C API bindings

pub const LLVMCompiler = struct {
    allocator: std.mem.Allocator,
    context: *c.LLVMContext,
    module: *c.LLVMModule,
    builder: *c.LLVMBuilder,
    target_machine: *c.LLVMTargetMachine,
    named_values: std.StringHashMap(*c.LLVMValue),
    function_protos: std.StringHashMap(*c.LLVMValue),
    reporter: *Reporting.Reporter,
    types: TypeCache,
    current_function: ?*c.LLVMValue = null,

    // Type cache for commonly used LLVM types
    const TypeCache = struct {
        void_type: *c.LLVMType,
        i1_type: *c.LLVMType, // bool
        i2_type: *c.LLVMType, // tetra
        i8_type: *c.LLVMType, // char/byte
        i32_type: *c.LLVMType, // int
        i64_type: *c.LLVMType, // long
        f64_type: *c.LLVMType, // double
        string_type: *c.LLVMType, // char*
    };

    // Define a consistent struct type for call arguments
    const CallArgs = struct {
        callee: *ast.Expr,
        arguments: []const *ast.Expr,
    };

    pub fn init(allocator: std.mem.Allocator, module_name: []const u8, reporter: *Reporting.Reporter) !LLVMCompiler {
        // Initialize LLVM
        c.LLVMInitializeAllTargetInfos();
        c.LLVMInitializeAllTargets();
        c.LLVMInitializeAllTargetMCs();
        c.LLVMInitializeAllAsmParsers();
        c.LLVMInitializeAllAsmPrinters();

        // Create LLVM context, module and builder
        const context = c.LLVMContextCreate();
        errdefer c.LLVMContextDispose(context);

        // Create a null-terminated copy of the module name
        const module_name_z = try allocator.dupeZ(u8, module_name);
        defer allocator.free(module_name_z);

        const module = c.LLVMModuleCreateWithNameInContext(module_name_z.ptr, context);
        errdefer c.LLVMDisposeModule(module);

        const builder = c.LLVMCreateBuilderInContext(context);
        errdefer c.LLVMDisposeBuilder(builder);

        // Get native target
        var target: ?*c.LLVMTarget = null;
        var error_message: ?[*:0]u8 = null;
        const triple = c.LLVMGetDefaultTargetTriple();
        defer c.LLVMDisposeMessage(triple);

        if (c.LLVMGetTargetFromTriple(triple, &target, @ptrCast(&error_message)) != 0) {
            if (error_message) |msg| {
                const err_msg = std.mem.span(msg);
                reporter.reportError("Failed to get target: {s}", .{err_msg});
                c.LLVMDisposeMessage(msg);
            } else {
                reporter.reportError("Failed to get target: unknown error", .{});
            }
            return error.TargetInitFailed;
        }

        // Create target machine
        const cpu = c.LLVMGetHostCPUName();
        defer c.LLVMDisposeMessage(cpu);

        const features = c.LLVMGetHostCPUFeatures();
        defer c.LLVMDisposeMessage(features);

        const target_machine = c.LLVMCreateTargetMachine(target.?, triple, cpu, features, c.LLVMCodeGenLevelDefault, c.LLVMRelocDefault, c.LLVMCodeModelDefault);
        errdefer c.LLVMDisposeTargetMachine(target_machine);

        // Set up common types
        const types = TypeCache{
            .void_type = c.LLVMVoidTypeInContext(context),
            .i1_type = c.LLVMInt1TypeInContext(context),
            .i2_type = c.LLVMIntTypeInContext(context, 2), // 2-bit int for tetra
            .i8_type = c.LLVMInt8TypeInContext(context),
            .i32_type = c.LLVMInt32TypeInContext(context),
            .i64_type = c.LLVMInt64TypeInContext(context),
            .f64_type = c.LLVMDoubleTypeInContext(context),
            .string_type = c.LLVMPointerType(c.LLVMInt8TypeInContext(context), 0),
        };

        return LLVMCompiler{
            .allocator = allocator,
            .context = context,
            .module = module,
            .builder = builder,
            .target_machine = target_machine,
            .named_values = std.StringHashMap(*c.LLVMValue).init(allocator),
            .function_protos = std.StringHashMap(*c.LLVMValue).init(allocator),
            .reporter = reporter,
            .types = types,
        };
    }

    pub fn deinit(self: *LLVMCompiler) void {
        self.named_values.deinit();
        self.function_protos.deinit();
        c.LLVMDisposeTargetMachine(self.target_machine);
        c.LLVMDisposeBuilder(self.builder);
        c.LLVMDisposeModule(self.module);
        c.LLVMContextDispose(self.context);
    }

    pub fn compile(self: *LLVMCompiler, statements: []ast.Stmt) ErrorList!void {
        // First pass - collect all struct and function prototypes
        try self.declareStructs(statements);
        try self.declareFunctions(statements);

        // Second pass - compile all statements
        for (statements) |*stmt| {
            try self.compileStatement(stmt);
        }
    }

    fn declareStructs(self: *LLVMCompiler, statements: []ast.Stmt) ErrorList!void {
        for (statements) |*stmt| {
            switch (stmt.*) {
                .Expression => |expr| {
                    if (expr) |e| {
                        if (e.* == .StructDecl) {
                            try self.declareStruct(e.StructDecl);
                        }
                    }
                },
                else => {},
            }
        }
    }

    fn declareStruct(self: *LLVMCompiler, struct_decl: ast.StructDecl) ErrorList!void {
        const name = struct_decl.name.lexeme;

        // Create a null-terminated copy of the name
        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        // Create array of field types
        const field_count = struct_decl.fields.len;
        const field_types = try self.allocator.alloc(*c.LLVMType, field_count);
        defer self.allocator.free(field_types);

        for (struct_decl.fields, 0..) |field, i| {
            field_types[i] = try self.typeExprToLLVMType(field.type_expr);
        }

        // Create struct type
        const struct_type = c.LLVMStructCreateNamed(self.context, name_z.ptr);
        c.LLVMStructSetBody(struct_type, field_types.ptr, @as(c_uint, @intCast(field_count)), c.toBool(false));
    }

    fn declareFunctions(self: *LLVMCompiler, statements: []ast.Stmt) ErrorList!void {
        for (statements) |*stmt| {
            if (stmt.* == .Function) {
                try self.declareFunctionProto(stmt);
            }
        }
    }

    fn declareFunctionProto(self: *LLVMCompiler, function_stmt: *const ast.Stmt) ErrorList!void {
        const func = function_stmt.Function;
        const name = func.name.lexeme;

        // Create a null-terminated copy of the name
        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        // Create array of parameter types
        const param_count = func.params.len;
        const param_types = try self.allocator.alloc(*c.LLVMType, param_count);
        defer self.allocator.free(param_types);

        for (func.params, 0..) |param, i| {
            if (param.type_expr) |type_expr| {
                param_types[i] = try self.typeExprToLLVMType(type_expr);
            } else {
                // If no type is specified, default to int
                param_types[i] = self.types.i32_type;
            }
        }

        // Get return type
        const return_type = try self.typeInfoToLLVMType(func.return_type_info);

        // Create function type
        const func_type = c.LLVMFunctionType(return_type, param_types.ptr, @as(c_uint, @intCast(param_count)), c.toBool(false) // not vararg
        );

        // Create the function
        const function = c.LLVMAddFunction(self.module, name_z.ptr, func_type);

        // Store function prototype
        try self.function_protos.put(name, function);
    }

    fn compileStatement(self: *LLVMCompiler, stmt: *const ast.Stmt) ErrorList!void {
        switch (stmt.*) {
            .Expression => |expr| {
                if (expr) |e| {
                    _ = try self.compileExpression(e);
                }
            },
            .VarDecl => |var_decl| {
                // Convert from Stmt.VarDecl to VarDecl
                const converted_var_decl = ast.VarDecl{
                    .name = var_decl.name,
                    .initializer = var_decl.initializer,
                    .type_info = var_decl.type_info,
                    .is_public = var_decl.is_public,
                };
                try self.compileVarDecl(converted_var_decl);
            },
            .Function => try self.compileFunction(stmt),
            .Return => try self.compileReturn(stmt.*),
            .Block => |block| try self.compileBlock(block),
            else => {
                self.reporter.reportError("Unsupported statement type: {s}", .{@tagName(stmt.*)});
                return error.UnsupportedStatement;
            },
        }
    }

    fn compileExpression(self: *LLVMCompiler, expr: *const ast.Expr) ErrorList!*c.LLVMValue {
        switch (expr.*) {
            .Literal => |lit| return self.compileLiteral(lit),
            .Binary => |bin| return self.compileBinary(bin),
            .Unary => |un| return self.compileUnary(un),
            .Variable => |var_expr| return self.compileVariable(var_expr),
            .Call => |call| {
                const call_args = CallArgs{ .callee = call.callee, .arguments = call.arguments };
                return self.compileCall(call_args);
            },
            .If => |if_expr| return try self.compileIfExpr(if_expr),
            .While => |while_expr| return try self.compileWhileExpr(while_expr),
            .For => |for_expr| return try self.compileForExpr(for_expr),
            else => {
                self.reporter.reportError("Unsupported expression type: {s}", .{@tagName(expr.*)});
                return error.UnsupportedExpression;
            },
        }
    }

    // Helper function to create null-terminated strings for LLVM C API
    fn createNullTerminatedString(self: *LLVMCompiler, str: []const u8) ![:0]const u8 {
        const result = try self.allocator.dupeZ(u8, str);
        return result;
    }

    fn compileLiteral(self: *LLVMCompiler, lit: token.TokenLiteral) ErrorList!*c.LLVMValue {
        switch (lit) {
            .int => |v| return c.LLVMConstInt(self.types.i32_type, @as(c_ulonglong, @intCast(v)), c.toBool(false)),
            .u8 => |v| return c.LLVMConstInt(self.types.i8_type, v, c.toBool(false)),
            .float => |v| return c.LLVMConstReal(self.types.f64_type, v),
            .boolean => |b| return c.LLVMConstInt(self.types.i1_type, @intFromBool(b == .true), c.toBool(false)),
            .string => |s| {
                const s_z = try self.createNullTerminatedString(s);
                defer self.allocator.free(s_z);

                const name_z = try self.createNullTerminatedString("str");
                defer self.allocator.free(name_z);

                const global_str = c.LLVMBuildGlobalStringPtr(self.builder, s_z.ptr, name_z.ptr);
                return global_str;
            },
            .tetra => |t| {
                const value: c_ulonglong = switch (t) {
                    .neither => 0, // 00
                    .false => 1, // 01
                    .true => 2, // 10
                    .both => 3, // 11
                };
                return c.LLVMConstInt(self.types.i2_type, value, c.toBool(false));
            },
            else => {
                self.reporter.reportError("Unsupported literal type", .{});
                return error.UnsupportedLiteral;
            },
        }
    }

    fn compileBinary(self: *LLVMCompiler, bin: ast.Binary) ErrorList!*c.LLVMValue {
        const left = try self.compileExpression(bin.left orelse return error.InvalidExpression);
        const right = try self.compileExpression(bin.right orelse return error.InvalidExpression);

        // Determine if we're dealing with integers or floats
        const left_type = c.LLVMTypeOf(left);
        const is_float = c.LLVMGetTypeKind(left_type) == c.LLVMFloatTypeKind or
            c.LLVMGetTypeKind(left_type) == c.LLVMDoubleTypeKind;

        // Create temporary name
        const temp_name = switch (bin.operator.type) {
            .PLUS => "addtmp",
            .MINUS => "subtmp",
            .ASTERISK => "multmp",
            .SLASH => "divtmp",
            .LESS, .GREATER, .EQUALITY, .BANG_EQUAL => "cmptmp",
            else => "bintmp",
        };

        const temp_name_z = try self.createNullTerminatedString(temp_name);
        defer self.allocator.free(temp_name_z);

        switch (bin.operator.type) {
            .PLUS => {
                if (is_float) {
                    return c.LLVMBuildFAdd(self.builder, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildAdd(self.builder, left, right, temp_name_z.ptr);
                }
            },
            .MINUS => {
                if (is_float) {
                    return c.LLVMBuildFSub(self.builder, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildSub(self.builder, left, right, temp_name_z.ptr);
                }
            },
            .ASTERISK => {
                if (is_float) {
                    return c.LLVMBuildFMul(self.builder, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildMul(self.builder, left, right, temp_name_z.ptr);
                }
            },
            .SLASH => {
                if (is_float) {
                    return c.LLVMBuildFDiv(self.builder, left, right, temp_name_z.ptr);
                } else {
                    // For integer division, we use signed division
                    return c.LLVMBuildSDiv(self.builder, left, right, temp_name_z.ptr);
                }
            },
            .LESS => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, left, right, temp_name_z.ptr);
                }
            },
            .GREATER => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, left, right, temp_name_z.ptr);
                }
            },
            .EQUALITY => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, left, right, temp_name_z.ptr);
                }
            },
            .BANG_EQUAL => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, left, right, temp_name_z.ptr);
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, left, right, temp_name_z.ptr);
                }
            },
            else => {
                self.reporter.reportError("Unsupported binary operator: {s}", .{@tagName(bin.operator.type)});
                return error.UnsupportedOperator;
            },
        }
    }

    fn compileUnary(self: *LLVMCompiler, un: ast.Unary) ErrorList!*c.LLVMValue {
        const operand = try self.compileExpression(un.right orelse return error.InvalidExpression);

        const name_z = try self.createNullTerminatedString(switch (un.operator.type) {
            .MINUS => "negtmp",
            .BANG => "nottmp",
            else => "unary",
        });
        defer self.allocator.free(name_z);

        switch (un.operator.type) {
            .MINUS => {
                const operand_type = c.LLVMTypeOf(operand);
                const is_float = c.LLVMGetTypeKind(operand_type) == c.LLVMFloatTypeKind or
                    c.LLVMGetTypeKind(operand_type) == c.LLVMDoubleTypeKind;

                if (is_float) {
                    // For floating-point negation
                    return c.LLVMBuildFNeg(self.builder, operand, name_z.ptr);
                } else {
                    // For integer negation, we use 0 - operand
                    const zero = c.LLVMConstInt(c.LLVMTypeOf(operand), 0, c.toBool(false));
                    return c.LLVMBuildSub(self.builder, zero, operand, name_z.ptr);
                }
            },
            .BANG => {
                // Logical NOT - compare with false
                return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, operand, c.LLVMConstInt(self.types.i1_type, 0, c.toBool(false)), name_z.ptr);
            },
            else => {
                self.reporter.reportError("Unsupported unary operator: {s}", .{@tagName(un.operator.type)});
                return error.UnsupportedOperator;
            },
        }
    }

    fn compileVariable(self: *LLVMCompiler, var_expr: token.Token) ErrorList!*c.LLVMValue {
        const name = var_expr.lexeme;

        // Look up the variable in our named_values map
        if (self.named_values.get(name)) |value| {
            // Create a null-terminated copy for the name
            const name_z = try self.allocator.dupeZ(u8, name);
            defer self.allocator.free(name_z);

            // Load the value from the variable
            const var_type = c.LLVMTypeOf(value);
            const pointed_type = c.LLVMGetElementType(var_type);
            return c.LLVMBuildLoad2(self.builder, pointed_type, value, name_z.ptr);
        }

        self.reporter.reportError("Unknown variable name: {s}", .{name});
        return error.VariableNotFound;
    }

    fn compileCall(self: *LLVMCompiler, call: CallArgs) ErrorList!*c.LLVMValue {
        // Get the function
        const callee = call.callee;

        // Only support direct function calls by name for now
        if (callee.* != .Variable) {
            self.reporter.reportError("Only direct function calls are supported", .{});
            return error.UnsupportedCallType;
        }

        const func_name = callee.Variable.lexeme;

        // Look up the function in our function_protos map
        const function = self.function_protos.get(func_name) orelse {
            self.reporter.reportError("Unknown function: {s}", .{func_name});
            return error.FunctionNotFound;
        };

        // Compile the arguments
        const arg_count = call.arguments.len;
        const args = try self.allocator.alloc(*c.LLVMValue, arg_count);
        defer self.allocator.free(args);

        for (call.arguments, 0..) |arg, i| {
            args[i] = try self.compileExpression(arg);
        }

        // Get the function type
        const func_type = c.LLVMTypeOf(function);

        // Name for the call result
        const call_name_z = try self.createNullTerminatedString("calltmp");
        defer self.allocator.free(call_name_z);

        // Build the call
        return c.LLVMBuildCall2(self.builder, func_type, function, args.ptr, @intCast(arg_count), call_name_z.ptr);
    }

    fn compileVarDecl(self: *LLVMCompiler, var_decl: ast.VarDecl) ErrorList!void {
        const name = var_decl.name.lexeme;

        // Create a null-terminated copy of the name
        const name_z = try self.allocator.dupeZ(u8, name);
        defer self.allocator.free(name_z);

        // Create a variable type and allocation
        const type_info = var_decl.type_info;
        const var_type = try self.typeInfoToLLVMType(type_info);

        // In the entry block of the current function
        // Place new allocations at the beginning of the current function
        const current_block = c.LLVMGetInsertBlock(self.builder);
        const current_function = c.LLVMGetBasicBlockParent(current_block);
        const entry_block = c.LLVMGetEntryBasicBlock(current_function);
        const first_instr = c.LLVMGetFirstInstruction(entry_block);

        if (first_instr != null) {
            c.LLVMPositionBuilderBefore(self.builder, first_instr.?);
        } else {
            c.LLVMPositionBuilderAtEnd(self.builder, entry_block);
        }

        const alloca = c.LLVMBuildAlloca(self.builder, var_type, name_z.ptr);

        // Return to previous position
        c.LLVMPositionBuilderAtEnd(self.builder, current_block);

        // Store the variable in our map
        try self.named_values.put(name, alloca);

        // If there's an initializer, compile and store it
        if (var_decl.initializer) |initializer| {
            const init_val = try self.compileExpression(initializer);
            _ = c.LLVMBuildStore(self.builder, init_val, alloca);
        }
    }

    fn compileFunction(self: *LLVMCompiler, function_stmt: *const ast.Stmt) ErrorList!void {
        const func = function_stmt.Function;
        const name = func.name.lexeme;

        // Get function prototype from earlier declaration
        const function = self.function_protos.get(name) orelse {
            self.reporter.reportError("Function implementation doesn't match any declaration: {s}", .{name});
            return error.FunctionNotDeclared;
        };

        // Create entry block
        const entry_block_name = try self.allocator.dupeZ(u8, "entry");
        defer self.allocator.free(entry_block_name);

        const entry_block = c.LLVMAppendBasicBlockInContext(self.context, function, entry_block_name.ptr);
        c.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        // Clear previous variables
        self.named_values.clearRetainingCapacity();

        // Add the parameters to the variable map
        for (func.params, 0..) |param, i| {
            const param_name = param.name.lexeme;
            const param_name_z = try self.allocator.dupeZ(u8, param_name);
            defer self.allocator.free(param_name_z);

            const param_val = c.LLVMGetParam(function, @intCast(i));
            const param_type = c.LLVMTypeOf(param_val);

            // Allocate stack slot for parameter
            const alloca = c.LLVMBuildAlloca(self.builder, param_type, param_name_z.ptr);
            _ = c.LLVMBuildStore(self.builder, param_val, alloca);

            // Add to variable map
            try self.named_values.put(param_name, alloca);
        }

        // Set current function
        self.current_function = function;

        // Compile function body
        for (func.body) |*stmt| {
            try self.compileStatement(stmt);
        }

        // If function ends without a return and returns void, add a return
        if (func.return_type_info.base == .Nothing) {
            _ = c.LLVMBuildRetVoid(self.builder);
        }

        // Verify the function
        var error_msg: ?[*:0]u8 = null;
        if (c.LLVMVerifyFunction(function, c.LLVMPrintMessageAction, @ptrCast(&error_msg)) != 0) {
            if (error_msg) |msg| {
                const err_str = std.mem.span(msg);
                self.reporter.reportError("Function verification failed for {s}: {s}", .{ name, err_str });
                c.LLVMDisposeMessage(msg);
            } else {
                self.reporter.reportError("Function verification failed for {s}: unknown error", .{name});
            }
            return error.FunctionVerificationFailed;
        }

        // Reset current function
        self.current_function = null;
    }

    fn compileReturn(self: *LLVMCompiler, ret_stmt: ast.Stmt) ErrorList!void {
        const ret = ret_stmt.Return;
        if (self.current_function == null) {
            self.reporter.reportError("Return statement outside of function", .{});
            return error.ReturnOutsideFunction;
        }

        if (ret.value) |value| {
            const return_val = try self.compileExpression(value);
            _ = c.LLVMBuildRet(self.builder, return_val);
        } else {
            _ = c.LLVMBuildRetVoid(self.builder);
        }
    }

    fn compileBlock(self: *LLVMCompiler, block: []ast.Stmt) ErrorList!void {
        for (block) |*stmt| {
            try self.compileStatement(stmt);
        }
    }

    pub fn outputToFile(self: *LLVMCompiler, output_path: []const u8) ErrorList!void {
        // Verify the module
        var error_message: ?[*:0]u8 = null;
        if (c.LLVMVerifyModule(self.module, c.LLVMPrintMessageAction, @ptrCast(&error_message)) != 0) {
            if (error_message) |msg| {
                const err_msg = std.mem.span(msg);
                self.reporter.reportError("Module verification failed: {s}", .{err_msg});
                c.LLVMDisposeMessage(msg);
            } else {
                self.reporter.reportError("Module verification failed: unknown error", .{});
            }
            return error.ModuleVerificationFailed;
        }

        // Apply optimizations
        const pass_manager = c.LLVMCreatePassManager();
        defer c.LLVMDisposePassManager(pass_manager);

        // Add some standard optimization passes
        c.LLVMAddPromoteMemoryToRegisterPass(pass_manager);
        c.LLVMAddInstructionCombiningPass(pass_manager);
        c.LLVMAddReassociatePass(pass_manager);
        c.LLVMAddGVNPass(pass_manager);
        c.LLVMAddCFGSimplificationPass(pass_manager);

        // Run the optimizations
        _ = c.LLVMRunPassManager(pass_manager, self.module);

        // Create a null-terminated copy of the output path
        const output_path_z = try self.allocator.dupeZ(u8, output_path);
        defer self.allocator.free(output_path_z);

        // Output to object file
        error_message = null;
        if (c.LLVMTargetMachineEmitToFile(self.target_machine, self.module, output_path_z.ptr, c.LLVMObjectFile, @ptrCast(&error_message)) != 0) {
            if (error_message) |msg| {
                const err_msg = std.mem.span(msg);
                self.reporter.reportError("Failed to emit object file: {s}", .{err_msg});
                c.LLVMDisposeMessage(msg);
            } else {
                self.reporter.reportError("Failed to emit object file: unknown error", .{});
            }
            return error.EmitFailed;
        }
    }

    // Helper methods for type conversion, etc.
    fn typeInfoToLLVMType(self: *LLVMCompiler, type_info: ast.TypeInfo) ErrorList!*c.LLVMType {
        switch (type_info.base) {
            .Int => return self.types.i32_type,
            .U8 => return self.types.i8_type,
            .Float => return self.types.f64_type,
            .Boolean => return self.types.i1_type,
            .String => return self.types.string_type,
            .Nothing => return self.types.void_type,
            .Tetra => return self.types.i2_type,
            else => {
                self.reporter.reportError("Unsupported type: {s}", .{@tagName(type_info.base)});
                return error.UnsupportedType;
            },
        }
    }

    fn typeExprToLLVMType(self: *LLVMCompiler, type_expr: *ast.TypeExpr) ErrorList!*c.LLVMType {
        switch (type_expr.*) {
            .Basic => |basic| {
                switch (basic) {
                    .Integer => return self.types.i32_type,
                    .U8 => return self.types.i8_type,
                    .Float => return self.types.f64_type,
                    .Boolean => return self.types.i1_type,
                    .String => return self.types.string_type,
                    .Auto => {
                        self.reporter.reportError("Auto type not supported in compilation", .{});
                        return error.UnsupportedType;
                    },
                    .Tetra => return self.types.i2_type,
                }
            },
            .Custom => |type_name_token| {
                const type_name = type_name_token.lexeme;
                if (std.mem.eql(u8, type_name, "int")) {
                    return self.types.i32_type;
                } else if (std.mem.eql(u8, type_name, "u8")) {
                    return self.types.i8_type;
                } else if (std.mem.eql(u8, type_name, "float")) {
                    return self.types.f64_type;
                } else if (std.mem.eql(u8, type_name, "bool")) {
                    return self.types.i1_type;
                } else if (std.mem.eql(u8, type_name, "string")) {
                    return self.types.string_type;
                } else if (std.mem.eql(u8, type_name, "void") or
                    std.mem.eql(u8, type_name, "nothing"))
                {
                    return self.types.void_type;
                } else if (std.mem.eql(u8, type_name, "tetra")) {
                    return self.types.i2_type;
                } else {
                    // This could be a struct or other custom type
                    const type_name_z = try self.allocator.dupeZ(u8, type_name);
                    defer self.allocator.free(type_name_z);

                    const struct_type_opt = c.LLVMGetTypeByName(self.module, type_name_z.ptr);
                    if (struct_type_opt) |struct_type| {
                        return struct_type;
                    }
                    self.reporter.reportError("Unknown type: {s}", .{type_name});
                    return error.UnknownType;
                }
            },
            .Array => |array| {
                const element_type = try self.typeExprToLLVMType(array.element_type);

                // If size is specified, create a fixed-size array
                if (array.size) |size_expr| {
                    // For now, we only support constant sizes
                    if (size_expr.* == .Literal and size_expr.Literal == .int) {
                        const size: c_uint = @intCast(size_expr.Literal.int);
                        return c.LLVMArrayType(element_type, size);
                    }
                }

                // For dynamic arrays, use a pointer to element type
                return c.LLVMPointerType(element_type, 0);
            },
            .Struct => {
                self.reporter.reportError("Struct type definitions not supported yet in type expressions", .{});
                return error.UnsupportedTypeExpr;
            },
            .Enum => {
                self.reporter.reportError("Enum type definitions not supported yet in type expressions", .{});
                return error.UnsupportedTypeExpr;
            },
        }
    }

    fn compileIfExpr(self: *LLVMCompiler, if_expr: ast.If) ErrorList!*c.LLVMValue {
        // Create blocks for then, else, and merge
        const function = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(self.builder));

        const then_name_z = try self.createNullTerminatedString("then");
        defer self.allocator.free(then_name_z);
        const else_name_z = try self.createNullTerminatedString("else");
        defer self.allocator.free(else_name_z);
        const merge_name_z = try self.createNullTerminatedString("ifcont");
        defer self.allocator.free(merge_name_z);

        const then_block = c.LLVMAppendBasicBlockInContext(self.context, function, then_name_z.ptr);
        const else_block = c.LLVMAppendBasicBlockInContext(self.context, function, else_name_z.ptr);
        const merge_block = c.LLVMAppendBasicBlockInContext(self.context, function, merge_name_z.ptr);

        // Compile condition
        const condition = if_expr.condition orelse {
            self.reporter.reportError("Missing condition in if expression", .{});
            return error.MissingCondition;
        };

        const condition_val = try self.compileExpression(condition);

        // Convert condition to boolean if needed
        const cond_type = c.LLVMTypeOf(condition_val);

        const cond_name_z = try self.createNullTerminatedString("ifcond");
        defer self.allocator.free(cond_name_z);

        const bool_cond = if (c.LLVMGetTypeKind(cond_type) == c.LLVMIntegerTypeKind and
            c.LLVMGetIntTypeWidth(cond_type) == 1)
            condition_val
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntNE, condition_val, c.LLVMConstInt(cond_type, 0, c.toBool(false)), cond_name_z.ptr);

        // Create conditional branch
        _ = c.LLVMBuildCondBr(self.builder, bool_cond, then_block, else_block);

        // Emit then block
        c.LLVMPositionBuilderAtEnd(self.builder, then_block);
        const then_branch = if_expr.then_branch orelse {
            self.reporter.reportError("Missing then branch in if expression", .{});
            return error.MissingThenBranch;
        };

        const then_val = try self.compileExpression(then_branch);
        _ = c.LLVMBuildBr(self.builder, merge_block);

        // Remember where then block ends
        const then_end_block = c.LLVMGetInsertBlock(self.builder);

        // Emit else block
        c.LLVMPositionBuilderAtEnd(self.builder, else_block);
        var else_val: *c.LLVMValue = undefined;

        if (if_expr.else_branch) |else_branch| {
            else_val = try self.compileExpression(else_branch);
        } else {
            // Default value if no else branch
            else_val = c.LLVMConstInt(self.types.i32_type, 0, c.toBool(false));
        }
        _ = c.LLVMBuildBr(self.builder, merge_block);

        // Remember where else block ends
        const else_end_block = c.LLVMGetInsertBlock(self.builder);

        // Continue with merge block
        c.LLVMPositionBuilderAtEnd(self.builder, merge_block);

        // Create phi node to merge results
        const phi_name_z = try self.createNullTerminatedString("iftmp");
        defer self.allocator.free(phi_name_z);

        const result_type = c.LLVMTypeOf(then_val);
        const phi = c.LLVMBuildPhi(self.builder, result_type, phi_name_z.ptr);

        // Add incoming values
        var incoming_values = [_]*c.LLVMValue{ then_val, else_val };
        var incoming_blocks = [_]*c.LLVMBasicBlock{ then_end_block, else_end_block };

        // Cast array pointer to expected type
        c.LLVMAddIncoming(phi, @as([*]*c.LLVMValue, @ptrCast(&incoming_values)), @as([*]*c.LLVMBasicBlock, @ptrCast(&incoming_blocks)), 2);

        return phi;
    }

    fn compileWhileExpr(self: *LLVMCompiler, while_expr: ast.WhileExpr) ErrorList!*c.LLVMValue {
        // Create blocks for condition, body, and after
        const function = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(self.builder));

        const cond_name_z = try self.createNullTerminatedString("whilecond");
        defer self.allocator.free(cond_name_z);
        const body_name_z = try self.createNullTerminatedString("whilebody");
        defer self.allocator.free(body_name_z);
        const after_name_z = try self.createNullTerminatedString("whileafter");
        defer self.allocator.free(after_name_z);

        const cond_block = c.LLVMAppendBasicBlockInContext(self.context, function, cond_name_z.ptr);
        const body_block = c.LLVMAppendBasicBlockInContext(self.context, function, body_name_z.ptr);
        const after_block = c.LLVMAppendBasicBlockInContext(self.context, function, after_name_z.ptr);

        // Jump to condition block first
        _ = c.LLVMBuildBr(self.builder, cond_block);

        // Emit condition block
        c.LLVMPositionBuilderAtEnd(self.builder, cond_block);
        const condition_val = try self.compileExpression(while_expr.condition);

        // Convert condition to boolean if needed
        const cond_type = c.LLVMTypeOf(condition_val);
        const bool_cond = if (c.LLVMGetTypeKind(cond_type) == c.LLVMIntegerTypeKind and
            c.LLVMGetIntTypeWidth(cond_type) == 1)
            condition_val
        else
            c.LLVMBuildICmp(self.builder, c.LLVMIntNE, condition_val, c.LLVMConstInt(cond_type, 0, c.toBool(false)), "whilecond");

        // Create conditional branch
        _ = c.LLVMBuildCondBr(self.builder, bool_cond, body_block, after_block);

        // Emit body block
        c.LLVMPositionBuilderAtEnd(self.builder, body_block);
        _ = try self.compileExpression(while_expr.body);

        // Jump back to condition block
        _ = c.LLVMBuildBr(self.builder, cond_block);

        // Continue with after block
        c.LLVMPositionBuilderAtEnd(self.builder, after_block);

        // While expressions in Doxa always evaluate to "nothing" (void)
        return c.LLVMConstInt(self.types.i32_type, 0, c.toBool(false));
    }

    fn compileForExpr(self: *LLVMCompiler, for_expr: ast.ForExpr) ErrorList!*c.LLVMValue {
        // Create blocks for condition, body, increment, and after
        const function = c.LLVMGetBasicBlockParent(c.LLVMGetInsertBlock(self.builder));

        const cond_name_z = try self.createNullTerminatedString("forcond");
        defer self.allocator.free(cond_name_z);
        const body_name_z = try self.createNullTerminatedString("forbody");
        defer self.allocator.free(body_name_z);
        const incr_name_z = try self.createNullTerminatedString("forincr");
        defer self.allocator.free(incr_name_z);
        const after_name_z = try self.createNullTerminatedString("forafter");
        defer self.allocator.free(after_name_z);

        const cond_block = c.LLVMAppendBasicBlockInContext(self.context, function, cond_name_z.ptr);
        const body_block = c.LLVMAppendBasicBlockInContext(self.context, function, body_name_z.ptr);
        const incr_block = c.LLVMAppendBasicBlockInContext(self.context, function, incr_name_z.ptr);
        const after_block = c.LLVMAppendBasicBlockInContext(self.context, function, after_name_z.ptr);

        // Execute initializer if present
        if (for_expr.initializer) |initi| {
            // For now, we don't have a way to compile statements directly,
            // so we handle special case of expression statements
            if (initi.* == .Expression) {
                // Check if Expression field is not null before using it
                if (initi.Expression) |expr| {
                    _ = try self.compileExpression(expr);
                }
            } else {
                self.reporter.reportError("Only expression initializers are supported in for loops", .{});
                return error.UnsupportedInitializer;
            }
        }

        // Jump to condition block
        _ = c.LLVMBuildBr(self.builder, cond_block);

        // Emit condition block
        c.LLVMPositionBuilderAtEnd(self.builder, cond_block);

        // If there's no condition, we always branch to body
        if (for_expr.condition) |cond| {
            const condition_val = try self.compileExpression(cond);
            // Convert condition to boolean if needed
            const cond_type = c.LLVMTypeOf(condition_val);
            const bool_cond = if (c.LLVMGetTypeKind(cond_type) == c.LLVMIntegerTypeKind and
                c.LLVMGetIntTypeWidth(cond_type) == 1)
                condition_val
            else
                c.LLVMBuildICmp(self.builder, c.LLVMIntNE, condition_val, c.LLVMConstInt(cond_type, 0, c.toBool(false)), "forcond");

            // Create conditional branch
            _ = c.LLVMBuildCondBr(self.builder, bool_cond, body_block, after_block);
        } else {
            // No condition means always execute body
            _ = c.LLVMBuildBr(self.builder, body_block);
        }

        // Emit body block
        c.LLVMPositionBuilderAtEnd(self.builder, body_block);
        _ = try self.compileExpression(for_expr.body);

        // Jump to increment block
        _ = c.LLVMBuildBr(self.builder, incr_block);

        // Emit increment block
        c.LLVMPositionBuilderAtEnd(self.builder, incr_block);
        if (for_expr.increment) |incr| {
            _ = try self.compileExpression(incr);
        }

        // Jump back to condition block
        _ = c.LLVMBuildBr(self.builder, cond_block);

        // Continue with after block
        c.LLVMPositionBuilderAtEnd(self.builder, after_block);

        // For expressions in Doxa always evaluate to "nothing" (void)
        return c.LLVMConstInt(self.types.i32_type, 0, c.toBool(false));
    }
};
