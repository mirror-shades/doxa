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
    reporter: *Reporting,
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

    pub fn init(allocator: std.mem.Allocator, module_name: []const u8, reporter: *Reporting) !LLVMCompiler {
        // Initialize LLVM
        c.LLVMInitializeAllTargetInfos();
        c.LLVMInitializeAllTargets();
        c.LLVMInitializeAllTargetMCs();
        c.LLVMInitializeAllAsmParsers();
        c.LLVMInitializeAllAsmPrinters();

        // Create LLVM context, module and builder
        const context = c.LLVMContextCreate();
        errdefer c.LLVMContextDispose(context);

        const module = c.LLVMModuleCreateWithNameInContext(module_name.ptr, context);
        errdefer c.LLVMDisposeModule(module);

        const builder = c.LLVMCreateBuilderInContext(context);
        errdefer c.LLVMDisposeBuilder(builder);

        // Get native target
        var target: ?*c.LLVMTarget = null;
        var error_message: [*c]u8 = null;
        const triple = c.LLVMGetDefaultTargetTriple();
        defer c.LLVMDisposeMessage(triple);

        if (c.LLVMGetTargetFromTriple(triple, &target, &error_message) != 0) {
            const err_msg = std.mem.span(error_message);
            reporter.reportError("Failed to get target: {s}", .{err_msg});
            c.LLVMDisposeMessage(error_message);
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

    pub fn compile(self: *LLVMCompiler, statements: []ast.Stmt) !void {
        // First pass - collect all struct and function prototypes
        try self.declareStructs(statements);
        try self.declareFunctions(statements);

        // Second pass - compile all statements
        for (statements) |*stmt| {
            try self.compileStatement(stmt);
        }
    }

    fn declareStructs(self: *LLVMCompiler, statements: []ast.Stmt) !void {
        for (statements) |*stmt| {
            switch (stmt.*) {
                .Expression => |expr| {
                    if (expr.* == .StructDecl) {
                        try self.declareStruct(expr.StructDecl);
                    }
                },
                else => {},
            }
        }
    }

    fn declareStruct(self: *LLVMCompiler, struct_decl: ast.StructDecl) !void {
        const name = struct_decl.name.lexeme;

        // Create array of field types
        const field_count = struct_decl.fields.len;
        const field_types = try self.allocator.alloc(*c.LLVMType, field_count);
        defer self.allocator.free(field_types);

        for (struct_decl.fields, 0..) |field, i| {
            field_types[i] = try self.typeExprToLLVMType(field.type_expr);
        }

        // Create struct type
        const struct_type = c.LLVMStructCreateNamed(self.context, name.ptr);
        c.LLVMStructSetBody(struct_type, field_types.ptr, @as(c_uint, @intCast(field_count)), c.LLVMBool(0));
    }

    fn declareFunctions(self: *LLVMCompiler, statements: []ast.Stmt) !void {
        for (statements) |*stmt| {
            if (stmt.* == .Function) {
                try self.declareFunctionProto(stmt.Function);
            }
        }
    }

    fn declareFunctionProto(self: *LLVMCompiler, func: ast.FunctionStmt) !void {
        const name = func.name.lexeme;

        // Create array of parameter types
        const param_count = func.params.len;
        const param_types = try self.allocator.alloc(*c.LLVMType, param_count);
        defer self.allocator.free(param_types);

        for (func.params, 0..) |param, i| {
            param_types[i] = try self.typeExprToLLVMType(param.type_expr);
        }

        // Get return type
        const return_type = try self.typeInfoToLLVMType(func.return_type_info);

        // Create function type
        const func_type = c.LLVMFunctionType(return_type, param_types.ptr, @as(c_uint, @intCast(param_count)), c.LLVMBool(0) // not vararg
        );

        // Create the function
        const function = c.LLVMAddFunction(self.module, name.ptr, func_type);

        // Store function prototype
        try self.function_protos.put(name, function);
    }

    fn compileStatement(self: *LLVMCompiler, stmt: *const ast.Stmt) !void {
        switch (stmt.*) {
            .Expression => |expr| try self.compileExpression(expr),
            .VarDecl => |var_decl| try self.compileVarDecl(var_decl),
            .Function => |func| try self.compileFunction(func),
            .Return => |ret| try self.compileReturn(ret),
            .Block => |block| try self.compileBlock(block),
            .If => |if_stmt| try self.compileIf(if_stmt),
            .While => |while_stmt| try self.compileWhile(while_stmt),
            .For => |for_stmt| try self.compileFor(for_stmt),
            else => {
                self.reporter.reportError("Unsupported statement type: {s}", .{@tagName(stmt.*)});
                return error.UnsupportedStatement;
            },
        }
    }

    fn compileExpression(self: *LLVMCompiler, expr: *const ast.Expr) !*c.LLVMValue {
        switch (expr.*) {
            .Literal => |lit| return self.compileLiteral(lit),
            .Binary => |bin| return self.compileBinary(bin),
            .Unary => |un| return self.compileUnary(un),
            .Variable => |var_expr| return self.compileVariable(var_expr),
            .Call => |call| return self.compileCall(call),
            else => {
                self.reporter.reportError("Unsupported expression type: {s}", .{@tagName(expr.*)});
                return error.UnsupportedExpression;
            },
        }
    }

    fn compileLiteral(self: *LLVMCompiler, lit: token.TokenLiteral) !*c.LLVMValue {
        switch (lit) {
            .int => |v| return c.LLVMConstInt(self.types.i32_type, @as(c_ulonglong, @intCast(v)), c.LLVMBool(0)),
            .u8 => |v| return c.LLVMConstInt(self.types.i8_type, v, c.LLVMBool(0)),
            .float => |v| return c.LLVMConstReal(self.types.f64_type, v),
            .boolean => |b| return c.LLVMConstInt(self.types.i1_type, @intFromBool(b == .true), c.LLVMBool(0)),
            .string => |s| {
                const global_str = c.LLVMBuildGlobalStringPtr(self.builder, s.ptr, "str");
                return global_str;
            },
            .tetra => |t| {
                const value: c_ulonglong = switch (t) {
                    .neither => 0, // 00
                    .false => 1, // 01
                    .true => 2, // 10
                    .both => 3, // 11
                };
                return c.LLVMConstInt(self.types.i2_type, value, c.LLVMBool(0));
            },
            else => {
                self.reporter.reportError("Unsupported literal type", .{});
                return error.UnsupportedLiteral;
            },
        }
    }

    fn compileBinary(self: *LLVMCompiler, bin: ast.BinaryExpr) !*c.LLVMValue {
        const left = try self.compileExpression(bin.left orelse return error.InvalidExpression);
        const right = try self.compileExpression(bin.right orelse return error.InvalidExpression);

        // Determine if we're dealing with integers or floats
        const left_type = c.LLVMTypeOf(left);
        const is_float = c.LLVMGetTypeKind(left_type) == c.LLVMFloatTypeKind or
            c.LLVMGetTypeKind(left_type) == c.LLVMDoubleTypeKind;

        switch (bin.operator.type) {
            .PLUS => {
                if (is_float) {
                    return c.LLVMBuildFAdd(self.builder, left, right, "addtmp");
                } else {
                    return c.LLVMBuildAdd(self.builder, left, right, "addtmp");
                }
            },
            .MINUS => {
                if (is_float) {
                    return c.LLVMBuildFSub(self.builder, left, right, "subtmp");
                } else {
                    return c.LLVMBuildSub(self.builder, left, right, "subtmp");
                }
            },
            .ASTERISK => {
                if (is_float) {
                    return c.LLVMBuildFMul(self.builder, left, right, "multmp");
                } else {
                    return c.LLVMBuildMul(self.builder, left, right, "multmp");
                }
            },
            .SLASH => {
                if (is_float) {
                    return c.LLVMBuildFDiv(self.builder, left, right, "divtmp");
                } else {
                    // For integer division, we use signed division
                    return c.LLVMBuildSDiv(self.builder, left, right, "divtmp");
                }
            },
            .LESS => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, left, right, "cmptmp");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntSLT, left, right, "cmptmp");
                }
            },
            .GREATER => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, left, right, "cmptmp");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntSGT, left, right, "cmptmp");
                }
            },
            .EQUAL_EQUAL => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, left, right, "cmptmp");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, left, right, "cmptmp");
                }
            },
            .BANG_EQUAL => {
                if (is_float) {
                    return c.LLVMBuildFCmp(self.builder, c.LLVMRealONE, left, right, "cmptmp");
                } else {
                    return c.LLVMBuildICmp(self.builder, c.LLVMIntNE, left, right, "cmptmp");
                }
            },
            else => {
                self.reporter.reportError("Unsupported binary operator: {s}", .{@tagName(bin.operator.type)});
                return error.UnsupportedOperator;
            },
        }
    }

    // Additional methods for compiling various node types will follow...
    // ...

    pub fn outputToFile(self: *LLVMCompiler, output_path: []const u8) !void {
        // Verify the module
        var error_message: [*c]u8 = null;
        if (c.LLVMVerifyModule(self.module, c.LLVMPrintMessageAction, &error_message) != 0) {
            const err_msg = std.mem.span(error_message);
            self.reporter.reportError("Module verification failed: {s}", .{err_msg});
            c.LLVMDisposeMessage(error_message);
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

        // Output to object file
        if (c.LLVMTargetMachineEmitToFile(self.target_machine, self.module, output_path.ptr, c.LLVMObjectFile, &error_message) != 0) {
            const err_msg = std.mem.span(error_message);
            self.reporter.reportError("Failed to emit object file: {s}", .{err_msg});
            c.LLVMDisposeMessage(error_message);
            return error.EmitFailed;
        }
    }

    // Helper methods for type conversion, etc.
    fn typeInfoToLLVMType(self: *LLVMCompiler, type_info: ast.TypeInfo) !*c.LLVMType {
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

    fn typeExprToLLVMType(self: *LLVMCompiler, type_expr: *ast.Expr) !*c.LLVMType {
        if (type_expr.* == .Variable) {
            const type_name = type_expr.Variable.lexeme;
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
                const struct_type = c.LLVMGetTypeByName(self.module, type_name.ptr);
                if (struct_type != null) {
                    return struct_type;
                }
                self.reporter.reportError("Unknown type: {s}", .{type_name});
                return error.UnknownType;
            }
        } else if (type_expr.* == .ArrayType) {
            const element_type = try self.typeExprToLLVMType(type_expr.ArrayType.element_type);

            // If size is specified, create a fixed-size array
            if (type_expr.ArrayType.size) |size_expr| {
                // For now, we only support constant sizes
                if (size_expr.* == .Literal and size_expr.Literal == .int) {
                    const size: c_uint = @intCast(size_expr.Literal.int);
                    return c.LLVMArrayType(element_type, size);
                }
            }

            // For dynamic arrays, use a pointer to element type
            return c.LLVMPointerType(element_type, 0);
        }

        self.reporter.reportError("Unsupported type expression", .{});
        return error.UnsupportedTypeExpr;
    }
};
