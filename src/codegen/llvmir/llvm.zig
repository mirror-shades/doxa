const std = @import("std");
const llvm = @import("llvm");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMTargetMachine = llvm.target_machine;
const LLVMTarget = llvm.target;

pub const LLVMGenError = error{
    UnsupportedExpressionType,
    UnsupportedStatementType,
    UnsupportedBinaryOperator,
    UnsupportedLogicalOperator,
    UnsupportedType,
    UnsupportedPrintType,
    UnsupportedLiteralType,
    UndefinedVariable,
    NameTooLong,
    IRGenerationFailed,
    EmitObjectCodeFailed,
    OutOfMemory,
    NoSpaceLeft,
    InvalidUtf8,
    FileTooBig,
    DeviceBusy,
    AccessDenied,
    SystemResources,
    WouldBlock,
    NoDevice,
    Unexpected,
    SharingViolation,
    PathAlreadyExists,
    FileNotFound,
    PipeBusy,
    InvalidWtf8,
    BadPathName,
    NetworkNotFound,
    AntivirusInterference,
    SymLinkLoop,
    ProcessFdQuotaExceeded,
    SystemFdQuotaExceeded,
    IsDir,
    NotDir,
    FileLocksNotSupported,
    FileBusy,
    DiskQuota,
    InputOutput,
    InvalidArgument,
    BrokenPipe,
    OperationAborted,
    NotOpenForWriting,
    LockViolation,
    ConnectionResetByPeer,
    ProcessNotFound,
    FunctionTypeNotFound,
};

pub const LLVMGenerator = struct {
    context: LLVMTypes.LLVMContextRef,
    module: LLVMTypes.LLVMModuleRef,
    builder: LLVMTypes.LLVMBuilderRef,
    target_machine: LLVMTypes.LLVMTargetMachineRef,

    // Symbol tables for tracking variables and types
    variables: std.StringHashMap(LLVMTypes.LLVMValueRef),
    types: std.StringHashMap(LLVMTypes.LLVMTypeRef),
    allocator: std.mem.Allocator,

    // Current function being generated
    current_function: ?LLVMTypes.LLVMValueRef,

    pub fn init(allocator: std.mem.Allocator) !*LLVMGenerator {
        // Create context, module, and builder
        const context = LLVMCore.LLVMContextCreate();
        errdefer LLVMCore.LLVMContextDispose(context);

        const module = LLVMCore.LLVMModuleCreateWithNameInContext("main", context);
        errdefer LLVMCore.LLVMDisposeModule(module);

        const builder = LLVMCore.LLVMCreateBuilderInContext(context);
        errdefer LLVMCore.LLVMDisposeBuilder(builder);

        // Get target machine (reusing code from main.zig)
        const target_triple = if (@import("builtin").os.tag == .windows)
            @as([*:0]const u8, "x86_64-w64-mingw32") // MinGW target for Windows
        else
            LLVMTargetMachine.LLVMGetDefaultTargetTriple();

        // No need to check for null since we handle Windows case separately
        defer if (@import("builtin").os.tag != .windows) LLVMCore.LLVMDisposeMessage(target_triple);

        var target: LLVMTypes.LLVMTargetRef = undefined;
        var error_message: [*c]u8 = undefined;
        if (LLVMTargetMachine.LLVMGetTargetFromTriple(target_triple, &target, &error_message) != 0) {
            std.debug.print("Error getting target: {s}\n", .{error_message});
            LLVMCore.LLVMDisposeMessage(error_message);
            return error.TargetInitializationFailed;
        }

        const cpu = LLVMTargetMachine.LLVMGetHostCPUName();
        if (cpu == null) {
            return error.CPUNameInitializationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(cpu);

        const features = LLVMTargetMachine.LLVMGetHostCPUFeatures();
        if (features == null) {
            return error.CPUFeaturesInitializationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(features);

        // Use large code model for Windows to handle relocations properly
        const code_model = if (@import("builtin").os.tag == .windows)
            LLVMTypes.LLVMCodeModel.LLVMCodeModelLarge
        else
            LLVMTypes.LLVMCodeModel.LLVMCodeModelDefault;

        // Use PIC relocation model for better compatibility
        const reloc_mode = if (@import("builtin").os.tag == .windows)
            LLVMTypes.LLVMRelocMode.LLVMRelocPIC
        else
            LLVMTypes.LLVMRelocMode.LLVMRelocDefault;

        const target_machine = LLVMTargetMachine.LLVMCreateTargetMachine(
            target,
            target_triple,
            cpu,
            features,
            LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault,
            reloc_mode,
            code_model,
        );
        if (target_machine == null) {
            return error.TargetMachineCreationFailed;
        }

        // Set target triple and data layout
        LLVMCore.LLVMSetTarget(module, target_triple);
        const data_layout = LLVMTargetMachine.LLVMCreateTargetDataLayout(target_machine);
        defer LLVMTarget.LLVMDisposeTargetData(data_layout);
        const data_layout_str = LLVMTarget.LLVMCopyStringRepOfTargetData(data_layout);
        defer LLVMCore.LLVMDisposeMessage(data_layout_str);
        LLVMCore.LLVMSetDataLayout(module, data_layout_str);

        // Create the generator instance
        const generator = try allocator.create(LLVMGenerator);
        generator.* = .{
            .context = context,
            .module = module,
            .builder = builder,
            .target_machine = target_machine,
            .variables = std.StringHashMap(LLVMTypes.LLVMValueRef).init(allocator),
            .types = std.StringHashMap(LLVMTypes.LLVMTypeRef).init(allocator),
            .allocator = allocator,
            .current_function = null,
        };

        return generator;
    }

    pub fn deinit(self: *LLVMGenerator) void {
        self.variables.deinit();
        self.types.deinit();
        LLVMCore.LLVMDisposeBuilder(self.builder);
        LLVMCore.LLVMDisposeModule(self.module);
        LLVMCore.LLVMContextDispose(self.context);
        LLVMTargetMachine.LLVMDisposeTargetMachine(self.target_machine);
        self.allocator.destroy(self);
    }

    pub fn generateAST(self: *LLVMGenerator, expr: *ast.Expr) LLVMGenError!void {
        std.debug.print("Generating AST\n", .{});
        _ = try self.generateExpr(expr);
    }

    pub fn emitLLVMIR(self: *LLVMGenerator, output_path: []const u8) LLVMGenError![]u8 {
        const ir_string = LLVMCore.LLVMPrintModuleToString(self.module);
        if (ir_string == null) {
            return LLVMGenError.IRGenerationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(ir_string);

        // Write to file
        const file = try std.fs.cwd().createFile(output_path, .{});
        defer file.close();

        const ir_content = std.mem.span(ir_string);
        try file.writeAll(ir_content);

        // Return a copy of the IR
        return self.allocator.dupe(u8, ir_content);
    }

    pub fn emitObjectCode(self: *LLVMGenerator, output_path: []const u8) LLVMGenError!void {
        // Set the correct data layout and triple again just to be safe
        const target_triple = if (@import("builtin").os.tag == .windows)
            "x86_64-w64-mingw32"
        else
            LLVMTargetMachine.LLVMGetDefaultTargetTriple();

        LLVMCore.LLVMSetTarget(self.module, target_triple);

        const data_layout = LLVMTargetMachine.LLVMCreateTargetDataLayout(self.target_machine);
        defer LLVMTarget.LLVMDisposeTargetData(data_layout);
        const data_layout_str = LLVMTarget.LLVMCopyStringRepOfTargetData(data_layout);
        defer LLVMCore.LLVMDisposeMessage(data_layout_str);
        LLVMCore.LLVMSetDataLayout(self.module, data_layout_str);

        // Emit the object code
        var error_message: [*c]u8 = undefined;
        if (LLVMTargetMachine.LLVMTargetMachineEmitToFile(
            self.target_machine,
            self.module,
            output_path.ptr,
            LLVMTypes.LLVMCodeGenFileType.LLVMObjectFile,
            &error_message,
        ) != 0) {
            std.debug.print("Error emitting object code: {s}\n", .{error_message});
            LLVMCore.LLVMDisposeMessage(error_message);
            return LLVMGenError.EmitObjectCodeFailed;
        }
    }

    fn generateExpr(self: *LLVMGenerator, expr: *const ast.Expr) LLVMGenError!LLVMTypes.LLVMValueRef {
        std.debug.print("Generating expression\n", .{});
        switch (expr.data) {
            .Literal => |lit| {
                std.debug.print("Generating literal\n", .{});
                return switch (lit) {
                    .int => |i| {
                        std.debug.print("Generating integer literal: {}\n", .{i});
                        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt32TypeInContext(self.context), @intCast(i), @intFromBool(true));
                    },
                    .string => |s| {
                        std.debug.print("Generating string literal: {s}\n", .{s});
                        return try self.createStringConstant(s);
                    },
                    .u8 => |b| {
                        std.debug.print("Generating u8 literal: {}\n", .{b});
                        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(self.context), b, @intFromBool(false));
                    },
                    else => error.UnsupportedLiteralType,
                };
            },

            .Binary => |bin| {
                const lhs = try self.generateExpr(bin.left.?);
                const rhs = try self.generateExpr(bin.right.?);

                return switch (bin.operator.type) {
                    .PLUS => LLVMCore.LLVMBuildAdd(self.builder, lhs, rhs, "add"),
                    .MINUS => LLVMCore.LLVMBuildSub(self.builder, lhs, rhs, "sub"),
                    .ASTERISK => LLVMCore.LLVMBuildMul(self.builder, lhs, rhs, "mul"),
                    .SLASH => LLVMCore.LLVMBuildSDiv(self.builder, lhs, rhs, "div"),
                    .MODULO => LLVMCore.LLVMBuildSRem(self.builder, lhs, rhs, "mod"),
                    else => error.UnsupportedBinaryOperator,
                };
            },

            .Logical => |logical| {
                const lhs = try self.generateExpr(logical.left);
                const rhs = try self.generateExpr(logical.right);

                return switch (logical.operator.type) {
                    .AND => LLVMCore.LLVMBuildAnd(self.builder, lhs, rhs, "and"),
                    .OR => LLVMCore.LLVMBuildOr(self.builder, lhs, rhs, "or"),
                    else => error.UnsupportedLogicalOperator,
                };
            },

            .If => |if_expr| {
                const condition = try self.generateExpr(if_expr.condition.?);

                // Create basic blocks
                const then_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "then");
                const else_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "else");
                const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "merge");

                // Create conditional branch
                _ = LLVMCore.LLVMBuildCondBr(self.builder, condition, then_block, else_block);

                // Generate then branch
                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, then_block);
                const then_value = try self.generateExpr(if_expr.then_branch.?);
                _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                // Generate else branch
                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, else_block);
                const else_value = if (if_expr.else_branch) |else_branch|
                    try self.generateExpr(else_branch)
                else
                    null;
                _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                // Create phi node if needed
                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                if (then_value != null and else_value != null) {
                    const phi = LLVMCore.LLVMBuildPhi(self.builder, LLVMCore.LLVMTypeOf(then_value), "if.result");
                    var values = [_]LLVMTypes.LLVMValueRef{ then_value, else_value.? };
                    var blocks = [_]LLVMTypes.LLVMBasicBlockRef{ then_block, else_block };
                    LLVMCore.LLVMAddIncoming(phi, &values, &blocks, 2);
                    return phi;
                }

                return null;
            },

            .Match => |match_expr| {
                const value = try self.generateExpr(match_expr.value);

                // Create basic blocks for each case
                var case_blocks = std.ArrayList(LLVMTypes.LLVMBasicBlockRef).init(self.allocator);
                defer case_blocks.deinit();

                const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.merge");

                // Generate code for each case
                for (match_expr.cases) |case| {
                    const case_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.case");
                    try case_blocks.append(case_block);

                    // Generate pattern comparison
                    const pattern_value = try self.generatePatternValue(case.pattern);
                    const cmp = LLVMCore.LLVMBuildICmp(self.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, value, pattern_value, "match.cmp");

                    _ = LLVMCore.LLVMBuildCondBr(self.builder, cmp, case_block, merge_block);

                    // Generate case body
                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case_block);
                    _ = try self.generateExpr(case.body);
                    _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);
                }

                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                return null;
            },

            .Function => |func| {
                // Create function type
                var param_types = std.ArrayList(LLVMTypes.LLVMTypeRef).init(self.allocator);
                defer param_types.deinit();

                for (func.params) |param| {
                    const type_info = try ast.typeInfoFromExpr(self.allocator, param.type_expr);
                    defer self.allocator.destroy(type_info);
                    const param_type = try self.getLLVMTypeFromTypeInfo(type_info.*);
                    try param_types.append(param_type);
                }

                const return_type = try self.getLLVMTypeFromTypeInfo(func.return_type_info);
                const func_type = LLVMCore.LLVMFunctionType(return_type, param_types.items.ptr, @intCast(param_types.items.len), @intFromBool(false));

                // Create function
                var name_buffer: [256]u8 = undefined;
                const name_with_null = std.fmt.bufPrintZ(&name_buffer, "{s}", .{func.name.lexeme}) catch return error.NameTooLong;
                const function = LLVMCore.LLVMAddFunction(self.module, name_with_null.ptr, func_type);

                // Generate function body
                const entry_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, function, "entry");
                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, entry_block);

                // Save current function and create new scope
                const prev_function = self.current_function;
                self.current_function = function;
                defer self.current_function = prev_function;

                // Add parameters to symbol table
                for (func.params, 0..) |param, i| {
                    const param_value = LLVMCore.LLVMGetParam(function, @intCast(i));
                    try self.variables.put(param.name.lexeme, param_value);
                }

                // Generate function body
                for (func.body) |stmt| {
                    try self.generateStmt(&stmt);
                }

                return function;
            },

            .Variable => |var_token| {
                if (self.variables.get(var_token.lexeme)) |value| {
                    // If it's a pointer (alloca), load it
                    if (LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(value)) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        return LLVMCore.LLVMBuildLoad2(self.builder, LLVMCore.LLVMGetElementType(LLVMCore.LLVMTypeOf(value)), value, "load");
                    }
                    return value;
                }
                return error.UndefinedVariable;
            },

            .Assignment => |assign| {
                const value = try self.generateExpr(assign.value.?);
                if (self.variables.get(assign.name.lexeme)) |ptr| {
                    _ = LLVMCore.LLVMBuildStore(self.builder, value, ptr);
                    return value;
                }
                return error.UndefinedVariable;
            },

            .Block => |block| {
                var last_value: ?LLVMTypes.LLVMValueRef = null;
                for (block.statements) |stmt| {
                    try self.generateStmt(&stmt);
                }
                if (block.value) |value| {
                    last_value = try self.generateExpr(value);
                }
                return last_value orelse null;
            },

            .Peek => |peek| {
                std.debug.print("Generating peek expression\n", .{});
                const value = try self.generateExpr(peek.expr);
                const type_kind = LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(value));
                std.debug.print("Peek value type kind: {}\n", .{type_kind});

                // Call appropriate print function based on type
                switch (type_kind) {
                    LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
                        std.debug.print("Calling print int\n", .{});
                        return self.buildPrintInt(value);
                    },
                    LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
                        std.debug.print("Calling print string\n", .{});
                        return self.buildPrintString(value);
                    },
                    else => {
                        std.debug.print("Unsupported print type: {}\n", .{type_kind});
                        return error.UnsupportedPrintType;
                    },
                }
            },

            .ReturnExpr => |return_expr| {
                if (return_expr.value) |value| {
                    const ret_val = try self.generateExpr(value);
                    return LLVMCore.LLVMBuildRet(self.builder, ret_val);
                } else {
                    return LLVMCore.LLVMBuildRetVoid(self.builder);
                }
            },

            else => return error.UnsupportedExpressionType,
        }
    }

    fn generateStmt(self: *LLVMGenerator, stmt: *const ast.Stmt) LLVMGenError!void {
        switch (stmt.*) {
            .Expression => |expr| {
                if (expr) |e| {
                    _ = try self.generateExpr(e);
                }
            },

            .VarDecl => |var_decl| {
                const value = if (var_decl.initializer) |init_expr|
                    try self.generateExpr(init_expr)
                else
                    try self.getDefaultValue(var_decl.type_info);

                // Create alloca for local variables
                const var_type = try self.getLLVMTypeFromTypeInfo(var_decl.type_info);
                var name_buffer: [256]u8 = undefined;
                const name_with_null = std.fmt.bufPrintZ(&name_buffer, "{s}", .{var_decl.name.lexeme}) catch return error.NameTooLong;
                const alloca = LLVMCore.LLVMBuildAlloca(self.builder, var_type, name_with_null.ptr);

                _ = LLVMCore.LLVMBuildStore(self.builder, value, alloca);
                try self.variables.put(var_decl.name.lexeme, alloca);
            },

            .Return => |ret| {
                if (ret.value) |value| {
                    const ret_val = try self.generateExpr(value);
                    _ = LLVMCore.LLVMBuildRet(self.builder, ret_val);
                } else {
                    _ = LLVMCore.LLVMBuildRetVoid(self.builder);
                }
            },

            else => return error.UnsupportedStatementType,
        }
    }

    fn getLLVMTypeFromTypeInfo(self: *LLVMGenerator, type_info: ast.TypeInfo) LLVMGenError!LLVMTypes.LLVMTypeRef {
        return switch (type_info.base) {
            .Int => LLVMCore.LLVMInt32TypeInContext(self.context),
            .U8 => LLVMCore.LLVMInt8TypeInContext(self.context),
            .Float => LLVMCore.LLVMDoubleTypeInContext(self.context),
            .String => LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0),
            .Nothing => LLVMCore.LLVMVoidTypeInContext(self.context),
            else => error.UnsupportedType,
        };
    }

    fn getDefaultValue(self: *LLVMGenerator, type_info: ast.TypeInfo) LLVMGenError!LLVMTypes.LLVMValueRef {
        const llvm_type = try self.getLLVMTypeFromTypeInfo(type_info);
        return LLVMCore.LLVMConstNull(llvm_type);
    }

    fn generatePatternValue(self: *LLVMGenerator, pattern: Token) LLVMGenError!LLVMTypes.LLVMValueRef {
        // For now, assume patterns are just enum variants stored as integers
        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt32TypeInContext(self.context), @intFromEnum(pattern.type), @intFromBool(false));
    }

    fn buildPrintInt(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef) !LLVMTypes.LLVMValueRef {
        // Create printf function if not already created
        const printf_fn = if (self.variables.get("printf")) |fn_val|
            fn_val
        else blk: {
            const i8_ptr_type = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0);
            var param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_type};
            const printf_type = LLVMCore.LLVMFunctionType(
                LLVMCore.LLVMInt32TypeInContext(self.context),
                &param_types,
                1,
                @intFromBool(true),
            );
            const fn_val = LLVMCore.LLVMAddFunction(self.module, "printf", printf_type);
            LLVMCore.LLVMSetLinkage(fn_val, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
            try self.variables.put("printf", fn_val);
            break :blk fn_val;
        };

        // Create format string
        const format_str = try self.createStringConstant("%d\n");

        // Create arguments array
        var args = [_]LLVMTypes.LLVMValueRef{ format_str, value };

        return LLVMCore.LLVMBuildCall2(
            self.builder,
            LLVMCore.LLVMTypeOf(printf_fn),
            printf_fn,
            &args,
            2,
            "",
        );
    }

    fn buildPrintString(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef) !LLVMTypes.LLVMValueRef {
        std.debug.print("\n=== Building Print String Call ===\n", .{});

        // Debug input value
        const value_type = LLVMCore.LLVMTypeOf(value);
        const value_type_kind = LLVMCore.LLVMGetTypeKind(value_type);
        std.debug.print("Input value: {*}\n", .{value});
        std.debug.print("Input value type: {}\n", .{value_type_kind});

        // Create puts function if not already created
        const puts_fn = if (self.variables.get("puts")) |fn_val| blk: {
            std.debug.print("Using existing puts function\n", .{});
            break :blk fn_val;
        } else blk: {
            std.debug.print("Creating new puts function\n", .{});
            const i8_ptr_type = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0);
            var param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_type};
            const puts_type = LLVMCore.LLVMFunctionType(
                LLVMCore.LLVMInt32TypeInContext(self.context),
                &param_types,
                1,
                @intFromBool(false),
            );
            const fn_val = LLVMCore.LLVMAddFunction(self.module, "puts", puts_type);
            LLVMCore.LLVMSetLinkage(fn_val, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
            try self.variables.put("puts", fn_val);

            // Debug puts function
            const fn_type = LLVMCore.LLVMTypeOf(fn_val);
            const fn_type_kind = LLVMCore.LLVMGetTypeKind(fn_type);
            std.debug.print("Puts function: {*}\n", .{fn_val});
            std.debug.print("Puts type: {}\n", .{fn_type_kind});
            break :blk fn_val;
        };

        // Debug arguments
        std.debug.print("\nPreparing call arguments...\n", .{});
        var args = [_]LLVMTypes.LLVMValueRef{value};
        std.debug.print("Argument (string): {*}\n", .{args[0]});

        // Create the function type for the call
        const i8_ptr_type = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0);
        var param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_type};
        const puts_type = LLVMCore.LLVMFunctionType(
            LLVMCore.LLVMInt32TypeInContext(self.context),
            &param_types,
            1,
            @intFromBool(false),
        );

        // Make the call
        std.debug.print("\nMaking puts call...\n", .{});
        const result = LLVMCore.LLVMBuildCall2(
            self.builder,
            puts_type,
            puts_fn,
            &args,
            1,
            "",
        );

        // Debug result
        if (result != null) {
            std.debug.print("Call instruction created successfully\n", .{});
            const result_type = LLVMCore.LLVMTypeOf(result);
            const result_type_kind = LLVMCore.LLVMGetTypeKind(result_type);
            std.debug.print("Call result type: {}\n", .{result_type_kind});
        } else {
            std.debug.print("Call instruction creation failed\n", .{});
        }

        std.debug.print("=== Print String Call Done ===\n\n", .{});
        return result;
    }

    fn createStringConstant(self: *LLVMGenerator, string: []const u8) !LLVMTypes.LLVMValueRef {
        std.debug.print("\n=== Creating String Constant ===\n", .{});
        std.debug.print("Input string: '{s}'\n", .{string});

        // Create a null-terminated copy of the string
        var buffer = try self.allocator.alloc(u8, string.len + 1);
        defer self.allocator.free(buffer);
        @memcpy(buffer[0..string.len], string);
        buffer[string.len] = 0;

        // Create a global string constant
        const str_val = LLVMCore.LLVMConstStringInContext(
            self.context,
            @ptrCast(buffer.ptr),
            @intCast(buffer.len),
            @intFromBool(false), // Don't add another null terminator
        );
        std.debug.print("String constant created: {*}\n", .{str_val});

        // Create a unique name for the global
        var name_buffer: [32]u8 = undefined;
        const unique_name = std.fmt.bufPrintZ(&name_buffer, "str.{d}", .{@intFromPtr(string.ptr)}) catch "str";

        // Create a global constant
        const global_str = LLVMCore.LLVMAddGlobal(
            self.module,
            LLVMCore.LLVMTypeOf(str_val),
            unique_name.ptr,
        );
        LLVMCore.LLVMSetInitializer(global_str, str_val);
        LLVMCore.LLVMSetGlobalConstant(global_str, @intFromBool(true));
        LLVMCore.LLVMSetLinkage(global_str, LLVMTypes.LLVMLinkage.LLVMPrivateLinkage);
        std.debug.print("Global variable created: {*}\n", .{global_str});

        // Get pointer to first element using GEP
        const zero = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt32TypeInContext(self.context), 0, @intFromBool(false));
        var indices = [_]LLVMTypes.LLVMValueRef{ zero, zero };
        const ptr = LLVMCore.LLVMBuildGEP2(
            self.builder,
            LLVMCore.LLVMTypeOf(str_val),
            global_str,
            &indices,
            2,
            "str_ptr",
        );
        std.debug.print("GEP result: {*}\n", .{ptr});

        // Cast to i8*
        const i8_ptr_type = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0);
        const result = LLVMCore.LLVMBuildBitCast(self.builder, ptr, i8_ptr_type, "str_ptr_cast");
        std.debug.print("Final string pointer: {*}\n", .{result});

        std.debug.print("=== String Constant Creation Done ===\n\n", .{});
        return result;
    }
};
