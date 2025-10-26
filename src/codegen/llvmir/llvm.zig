const std = @import("std");
const llvm = @import("llvm");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const TokenType = @import("../../types/token.zig").TokenType;
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMTargetMachine = llvm.target_machine;
const LLVMTarget = llvm.target;
const LLVMAnalysis = llvm.analysis;
const LLVMTransform = llvm.transform;
const externs = @import("common/externs.zig");
const _doxa_runtime_force_link = @import("../../runtime/mod.zig");
const strings = @import("common/strings.zig");

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

var next_string_id: usize = 0;

const FunctionSymbol = struct {
    value: LLVMTypes.LLVMValueRef,
    param_types: []LLVMTypes.LLVMTypeRef,
    param_alias_flags: []bool,
    return_type: LLVMTypes.LLVMTypeRef,
};

pub const LLVMGenerator = struct {
    context: LLVMTypes.LLVMContextRef,
    module: LLVMTypes.LLVMModuleRef,
    builder: LLVMTypes.LLVMBuilderRef,
    target_machine: LLVMTypes.LLVMTargetMachineRef,

    // Symbol tables for tracking variables and types
    variables: std.StringHashMap(LLVMTypes.LLVMValueRef),
    types: std.StringHashMap(LLVMTypes.LLVMTypeRef),
    externs: std.StringHashMap(LLVMTypes.LLVMValueRef),
    functions: std.StringHashMap(FunctionSymbol),
    string_literals: std.StringHashMap(LLVMTypes.LLVMValueRef),
    allocator: std.mem.Allocator,

    // Current function being generated
    current_function: ?LLVMTypes.LLVMValueRef,

    // Debug/opt controls
    debug_peek: bool,
    opt_level: i32,

    pub fn init(allocator: std.mem.Allocator, opt_level: i32) !*LLVMGenerator {
        // Ensure targets are registered before querying triple
        // Initialize native target and also register all targets/printers for portability
        LLVMTarget.LLVMInitializeAllTargetInfos();
        LLVMTarget.LLVMInitializeAllTargets();
        LLVMTarget.LLVMInitializeAllTargetMCs();
        LLVMTarget.LLVMInitializeAllAsmPrinters();
        LLVMTarget.LLVMInitializeAllAsmParsers();
        LLVMTarget.LLVMInitializeAllDisassemblers();

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

        // Map opt_level to LLVM codegen opt level
        const llvm_opt: LLVMTypes.LLVMCodeGenOptLevel = blk: {
            if (opt_level <= -1 or opt_level == 0) break :blk LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelNone;
            if (opt_level == 1) break :blk LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelLess;
            if (opt_level >= 3) break :blk LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelAggressive;
            break :blk LLVMTypes.LLVMCodeGenOptLevel.LLVMCodeGenLevelDefault; // opt_level == 2
        };

        const target_machine = LLVMTargetMachine.LLVMCreateTargetMachine(
            target,
            target_triple,
            cpu,
            features,
            llvm_opt,
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
            .externs = std.StringHashMap(LLVMTypes.LLVMValueRef).init(allocator),
            .functions = std.StringHashMap(FunctionSymbol).init(allocator),
            .string_literals = std.StringHashMap(LLVMTypes.LLVMValueRef).init(allocator),
            .allocator = allocator,
            .current_function = null,
            .debug_peek = false,
            .opt_level = opt_level,
        };

        return generator;
    }

    pub fn deinit(self: *LLVMGenerator) void {
        self.variables.deinit();
        self.types.deinit();
        self.externs.deinit();
        var func_it = self.functions.iterator();
        while (func_it.next()) |entry| {
            self.allocator.free(entry.value_ptr.param_types);
            self.allocator.free(entry.value_ptr.param_alias_flags);
        }
        self.functions.deinit();
        var str_it = self.string_literals.iterator();
        while (str_it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
        }
        self.string_literals.deinit();
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
        // Optionally dump IR before verification to help diagnose verifier failures
        if (self.debug_peek) {
            const pre_ir = LLVMCore.LLVMPrintModuleToString(self.module);
            if (pre_ir != null) {
                defer LLVMCore.LLVMDisposeMessage(pre_ir);
                const pre_path = "out/ir_before.ll";
                const pre_file = std.fs.cwd().createFile(pre_path, .{}) catch null;
                if (pre_file) |f| {
                    defer f.close();
                    _ = f.writeAll(std.mem.span(pre_ir)) catch {};
                }
            }
        }

        // Verify module before emission
        try self.verify();
        // Run optimization pipeline
        self.optimize();
        // Verify again after optimization
        try self.verify();
        const ir_string = LLVMCore.LLVMPrintModuleToString(self.module);
        if (ir_string == null) {
            return LLVMGenError.IRGenerationFailed;
        }
        defer LLVMCore.LLVMDisposeMessage(ir_string);

        // Write to file
        const file = std.fs.cwd().createFile(output_path, .{}) catch |err| switch (err) {
            error.AccessDenied => return LLVMGenError.AccessDenied,
            error.AntivirusInterference => return LLVMGenError.AntivirusInterference,
            error.BadPathName => return LLVMGenError.BadPathName,
            error.DeviceBusy => return LLVMGenError.DeviceBusy,
            error.FileBusy => return LLVMGenError.FileBusy,
            error.FileLocksNotSupported => return LLVMGenError.FileLocksNotSupported,
            error.FileNotFound => return LLVMGenError.FileNotFound,
            error.FileTooBig => return LLVMGenError.FileTooBig,
            error.InvalidUtf8 => return LLVMGenError.InvalidUtf8,
            error.InvalidWtf8 => return LLVMGenError.InvalidWtf8,
            error.IsDir => return LLVMGenError.IsDir,
            error.NameTooLong => return LLVMGenError.NameTooLong,
            error.NetworkNotFound => return LLVMGenError.NetworkNotFound,
            error.NoDevice => return LLVMGenError.NoDevice,
            error.NoSpaceLeft => return LLVMGenError.NoSpaceLeft,
            error.NotDir => return LLVMGenError.NotDir,
            error.PathAlreadyExists => return LLVMGenError.PathAlreadyExists,
            error.PermissionDenied => return LLVMGenError.AccessDenied,
            error.PipeBusy => return LLVMGenError.PipeBusy,
            error.ProcessFdQuotaExceeded => return LLVMGenError.ProcessFdQuotaExceeded,
            error.ProcessNotFound => return LLVMGenError.ProcessNotFound,
            error.SharingViolation => return LLVMGenError.SharingViolation,
            error.SymLinkLoop => return LLVMGenError.SymLinkLoop,
            error.SystemFdQuotaExceeded => return LLVMGenError.SystemFdQuotaExceeded,
            error.SystemResources => return LLVMGenError.SystemResources,
            error.Unexpected => return LLVMGenError.Unexpected,
            error.WouldBlock => return LLVMGenError.WouldBlock,
        };
        defer file.close();

        const ir_content = std.mem.span(ir_string);
        file.writeAll(ir_content) catch |err| switch (err) {
            error.AccessDenied => return LLVMGenError.AccessDenied,
            error.BrokenPipe => return LLVMGenError.BrokenPipe,
            error.ConnectionResetByPeer => return LLVMGenError.ConnectionResetByPeer,
            error.DeviceBusy => return LLVMGenError.DeviceBusy,
            error.DiskQuota => return LLVMGenError.DiskQuota,
            error.FileTooBig => return LLVMGenError.FileTooBig,
            error.InputOutput => return LLVMGenError.InputOutput,
            error.InvalidArgument => return LLVMGenError.InvalidArgument,
            error.LockViolation => return LLVMGenError.LockViolation,
            error.NoDevice => return LLVMGenError.NoDevice,
            error.NoSpaceLeft => return LLVMGenError.NoSpaceLeft,
            error.NotOpenForWriting => return LLVMGenError.NotOpenForWriting,
            error.OperationAborted => return LLVMGenError.OperationAborted,
            error.PermissionDenied => return LLVMGenError.AccessDenied,
            error.ProcessNotFound => return LLVMGenError.ProcessNotFound,
            error.SystemResources => return LLVMGenError.SystemResources,
            error.Unexpected => return LLVMGenError.Unexpected,
            error.WouldBlock => return LLVMGenError.WouldBlock,
            else => return LLVMGenError.Unexpected,
        };

        // Return a copy of the IR
        return self.allocator.dupe(u8, ir_content);
    }

    pub fn emitObjectCode(self: *LLVMGenerator, output_path: []const u8) LLVMGenError!void {
        // Set the correct data layout and triple again just to be safe
        const target_triple = if (@import("builtin").os.tag == .windows)
            "x86_64-w64-mingw32"
        else
            LLVMTargetMachine.LLVMGetDefaultTargetTriple();
        defer if (@import("builtin").os.tag != .windows) LLVMCore.LLVMDisposeMessage(target_triple);

        LLVMCore.LLVMSetTarget(self.module, target_triple);

        const data_layout = LLVMTargetMachine.LLVMCreateTargetDataLayout(self.target_machine);
        defer LLVMTarget.LLVMDisposeTargetData(data_layout);
        const data_layout_str = LLVMTarget.LLVMCopyStringRepOfTargetData(data_layout);
        defer LLVMCore.LLVMDisposeMessage(data_layout_str);
        LLVMCore.LLVMSetDataLayout(self.module, data_layout_str);

        // Verify and optimize before object emission
        try self.verify();
        self.optimize();
        try self.verify();

        // Ensure output path is NUL-terminated
        var path_buf: [256]u8 = undefined;
        const zpath = std.fmt.bufPrintZ(&path_buf, "{s}", .{output_path}) catch return LLVMGenError.InvalidArgument;

        // Emit the object code
        var error_message: [*c]u8 = undefined;
        if (LLVMTargetMachine.LLVMTargetMachineEmitToFile(
            self.target_machine,
            self.module,
            zpath.ptr,
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
                        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt64TypeInContext(self.context), @intCast(i), @intFromBool(true));
                    },
                    .float => |f| {
                        return LLVMCore.LLVMConstReal(LLVMCore.LLVMDoubleTypeInContext(self.context), f);
                    },
                    .tetra => |t| {
                        const ty_i2 = LLVMCore.LLVMIntTypeInContext(self.context, 2);
                        const enc: u64 = switch (t) {
                            .true => 1, // 01
                            .false => 2, // 10
                            .both => 3, // 11
                            .neither => 0, // 00
                        };
                        return LLVMCore.LLVMConstInt(ty_i2, enc, @intFromBool(false));
                    },
                    .string => |s| {
                        std.debug.print("Generating string literal: {s}\n", .{s});
                        return try strings.createStringPtr(self, s, &next_string_id);
                    },
                    .byte => |b| {
                        std.debug.print("Generating byte literal: {}\n", .{b});
                        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(self.context), b, @intFromBool(false));
                    },
                    else => error.UnsupportedLiteralType,
                };
            },

            .Binary => |bin| {
                const lhs = try self.generateExpr(bin.left.?);
                const rhs = try self.generateExpr(bin.right.?);

                return switch (bin.operator.type) {
                    .PLUS => blk: {
                        // Promote operands to common type
                        const L = try self.toF64(lhs);
                        const R = try self.toF64(rhs);
                        break :blk LLVMCore.LLVMBuildFAdd(self.builder, L, R, "fadd");
                    },
                    .MINUS => blk: {
                        // Promote operands to common type
                        const L = try self.toF64(lhs);
                        const R = try self.toF64(rhs);
                        break :blk LLVMCore.LLVMBuildFSub(self.builder, L, R, "fsub");
                    },
                    .ASTERISK => blk: {
                        // Promote both operands to f64; perform floating multiplication
                        const L = try self.toF64(lhs);
                        const R = try self.toF64(rhs);
                        break :blk LLVMCore.LLVMBuildFMul(self.builder, L, R, "fmul");
                    },
                    .SLASH => blk: {
                        // Promote both operands to f64; perform floating division only
                        const L = try self.toF64(lhs);
                        const R = try self.toF64(rhs);
                        break :blk LLVMCore.LLVMBuildFDiv(self.builder, L, R, "fdiv");
                    },
                    .MODULO => LLVMCore.LLVMBuildSRem(self.builder, lhs, rhs, "mod"),
                    else => error.UnsupportedBinaryOperator,
                };
            },

            .Unary => |un| {
                const operand = try self.generateExpr(un.right.?);
                switch (un.operator.type) {
                    .NOT => {
                        const ty = LLVMCore.LLVMTypeOf(operand);
                        if (LLVMCore.LLVMGetTypeKind(ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind and LLVMCore.LLVMGetIntTypeWidth(ty) == 2) {
                            // Tetra NOT: swap low/high bits, preserve both/neither
                            const one2 = LLVMCore.LLVMConstInt(ty, 1, @intFromBool(false));
                            const two2 = LLVMCore.LLVMConstInt(ty, 2, @intFromBool(false));
                            const low = LLVMCore.LLVMBuildAnd(self.builder, operand, one2, "tetra.low");
                            const high = LLVMCore.LLVMBuildAnd(self.builder, operand, two2, "tetra.high");
                            const low_to_high = LLVMCore.LLVMBuildShl(self.builder, low, LLVMCore.LLVMConstInt(ty, 1, @intFromBool(false)), "tetra.l2h");
                            const high_to_low = LLVMCore.LLVMBuildLShr(self.builder, high, LLVMCore.LLVMConstInt(ty, 1, @intFromBool(false)), "tetra.h2l");
                            return LLVMCore.LLVMBuildOr(self.builder, low_to_high, high_to_low, "tetra.not");
                        }
                        return error.UnsupportedLogicalOperator;
                    },
                    else => return error.UnsupportedLogicalOperator,
                }
            },

            .Logical => |logical| {
                const lhs = try self.generateExpr(logical.left);

                // Short-circuit logical operators with boolean i1 semantics
                return switch (logical.operator.type) {
                    .AND => blk_and: {
                        const start_block = LLVMCore.LLVMGetInsertBlock(self.builder);
                        const lhs_i1 = try self.toBoolI1(lhs);
                        const rhs_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "and.rhs");
                        const end_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "and.end");
                        _ = LLVMCore.LLVMBuildCondBr(self.builder, lhs_i1, rhs_block, end_block);

                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, rhs_block);
                        const rhs_val_raw = try self.generateExpr(logical.right);
                        const rhs_i1 = try self.toBoolI1(rhs_val_raw);
                        _ = LLVMCore.LLVMBuildBr(self.builder, end_block);

                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, end_block);
                        const bool_ty = LLVMCore.LLVMInt1TypeInContext(self.context);
                        const phi = LLVMCore.LLVMBuildPhi(self.builder, bool_ty, "and.result");
                        const false_const = LLVMCore.LLVMConstInt(bool_ty, 0, @intFromBool(false));
                        var incoming_vals = [_]LLVMTypes.LLVMValueRef{ rhs_i1, false_const };
                        var incoming_blocks = [_]LLVMTypes.LLVMBasicBlockRef{ rhs_block, start_block };
                        LLVMCore.LLVMAddIncoming(phi, &incoming_vals, &incoming_blocks, 2);
                        break :blk_and phi;
                    },
                    .OR => blk_or: {
                        const start_block = LLVMCore.LLVMGetInsertBlock(self.builder);
                        const lhs_i1 = try self.toBoolI1(lhs);
                        const rhs_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "or.rhs");
                        const end_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "or.end");
                        _ = LLVMCore.LLVMBuildCondBr(self.builder, lhs_i1, end_block, rhs_block);

                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, rhs_block);
                        const rhs_val_raw = try self.generateExpr(logical.right);
                        const rhs_i1 = try self.toBoolI1(rhs_val_raw);
                        _ = LLVMCore.LLVMBuildBr(self.builder, end_block);

                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, end_block);
                        const bool_ty = LLVMCore.LLVMInt1TypeInContext(self.context);
                        const phi = LLVMCore.LLVMBuildPhi(self.builder, bool_ty, "or.result");
                        const true_const = LLVMCore.LLVMConstInt(bool_ty, 1, @intFromBool(false));
                        var incoming_vals = [_]LLVMTypes.LLVMValueRef{ true_const, rhs_i1 };
                        var incoming_blocks = [_]LLVMTypes.LLVMBasicBlockRef{ start_block, rhs_block };
                        LLVMCore.LLVMAddIncoming(phi, &incoming_vals, &incoming_blocks, 2);
                        break :blk_or phi;
                    },
                    else => error.UnsupportedLogicalOperator,
                };
            },

            .If => |if_expr| {
                var condition = try self.generateExpr(if_expr.condition.?);
                condition = try self.toBoolI1(condition);

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
                var cond_val = try self.generateExpr(match_expr.value);
                const cond_ty = LLVMCore.LLVMTypeOf(cond_val);

                // Handle string matching with if-else chains instead of switch
                if (LLVMCore.LLVMGetTypeKind(cond_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                    // String matching - use if-else chain
                    const result_type = try self.getLLVMTypeFromTypeInfo(match_expr.type_info);
                    const result_alloca = self.createEntryAlloca(LLVMCore.LLVMGetInsertBlock(self.builder), result_type, "match.result");

                    const default_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.default");
                    const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.merge");

                    // Generate if-else chain for string matching
                    var current_block = LLVMCore.LLVMGetInsertBlock(self.builder);

                    for (match_expr.cases, 0..) |case, i| {
                        const case_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.case");
                        const next_block = if (i < match_expr.cases.len - 1)
                            LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.next")
                        else
                            default_block;

                        // Generate comparisons for each pattern in the patterns array
                        var pattern_matched_block: LLVMCore.LLVMValueRef = undefined;
                        var pattern_check_block = current_block;
                        
                        for (case.patterns, 0..) |pattern, pattern_idx| {
                            if (pattern_idx > 0) {
                                // Create intermediate block for pattern checking
                                pattern_check_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "pattern.check");
                                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, current_block);
                                _ = LLVMCore.LLVMBuildBr(self.builder, pattern_check_block);
                            }
                            
                            // Generate string comparison for this pattern
                            const pattern_val = try self.generateExpr(pattern);
                            const cmp_result = LLVMCore.LLVMBuildICmp(self.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, cond_val, pattern_val, "str.cmp");
                            
                            LLVMCore.LLVMPositionBuilderAtEnd(self.builder, pattern_check_block);
                            
                            if (pattern_idx == case.patterns.len - 1) {
                                // Last pattern - if it doesn't match, go to next case
                                _ = LLVMCore.LLVMBuildCondBr(self.builder, cmp_result, case_block, next_block);
                            } else {
                                // Not last pattern - if it doesn't match, check next pattern
                                const next_pattern_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "next.pattern");
                                _ = LLVMCore.LLVMBuildCondBr(self.builder, cmp_result, case_block, next_pattern_block);
                                current_block = next_pattern_block;
                            }
                        }

                        // Generate case body
                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case_block);
                        const case_result = try self.generateExpr(case.body);
                        _ = LLVMCore.LLVMBuildStore(self.builder, case_result, result_alloca);
                        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                        current_block = next_block;
                    }

                    // Default path
                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, default_block);
                    const default_value = try self.getDefaultValue(match_expr.type_info);
                    _ = LLVMCore.LLVMBuildStore(self.builder, default_value, result_alloca);
                    _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                    return LLVMCore.LLVMBuildLoad2(self.builder, result_type, result_alloca, "match.load");
                } else {
                    // Integer matching - use switch statement
                    if (LLVMCore.LLVMGetTypeKind(cond_ty) != LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                        return error.UnsupportedExpressionType;
                    }
                    // Normalize to i64 for switch lowering if possible (including i2 tetra)
                    if (LLVMCore.LLVMGetIntTypeWidth(cond_ty) < 64) {
                        const i64_ty = LLVMCore.LLVMInt64TypeInContext(self.context);
                        cond_val = LLVMCore.LLVMBuildZExt(self.builder, cond_val, i64_ty, "match.zext");
                    } else if (LLVMCore.LLVMGetIntTypeWidth(cond_ty) > 64) {
                        const i64_ty = LLVMCore.LLVMInt64TypeInContext(self.context);
                        cond_val = LLVMCore.LLVMBuildTrunc(self.builder, cond_val, i64_ty, "match.trunc");
                    }

                    const default_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.default");
                    const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.merge");

                    // Create a result variable to store the match result
                    const result_type = try self.getLLVMTypeFromTypeInfo(match_expr.type_info);
                    const result_alloca = self.createEntryAlloca(LLVMCore.LLVMGetInsertBlock(self.builder), result_type, "match.result");

                    const switch_inst = LLVMCore.LLVMBuildSwitch(self.builder, cond_val, default_block, @intCast(match_expr.cases.len));

                    // Generate code for each case
                    for (match_expr.cases) |case| {
                        const case_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.case");
                        
                        // Generate cases for each pattern in the patterns array
                        for (case.patterns) |pattern| {
                            const pattern_value = try self.generatePatternValue(pattern);
                            LLVMCore.LLVMAddCase(switch_inst, pattern_value, case_block);
                        }

                        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case_block);
                        const case_result = try self.generateExpr(case.body);
                        _ = LLVMCore.LLVMBuildStore(self.builder, case_result, result_alloca);
                        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);
                    }

                    // Default path - store default value
                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, default_block);
                    const default_value = try self.getDefaultValue(match_expr.type_info);
                    _ = LLVMCore.LLVMBuildStore(self.builder, default_value, result_alloca);
                    _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                    return LLVMCore.LLVMBuildLoad2(self.builder, result_type, result_alloca, "match.load");
                }
            },

            .Function => |func| {
                // Create function type
                var param_types = std.array_list.Managed(LLVMTypes.LLVMTypeRef).init(self.allocator);
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

                // If function returns Nothing (void) and current block has no terminator, add implicit ret void
                const insert_block = LLVMCore.LLVMGetInsertBlock(self.builder);
                if (insert_block != null and LLVMCore.LLVMGetTypeKind(return_type) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
                    _ = LLVMCore.LLVMBuildRetVoid(self.builder);
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
                if (self.functions.get(var_token.lexeme)) |func_symbol| {
                    return func_symbol.value;
                }
                if (self.externs.get(var_token.lexeme)) |extern_value| {
                    return extern_value;
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

            .CompoundAssign => |compound| {
                const ptr = self.variables.get(compound.name.lexeme) orelse return error.UndefinedVariable;
                const ptr_ty = LLVMCore.LLVMTypeOf(ptr);
                if (LLVMCore.LLVMGetTypeKind(ptr_ty) != LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                    return LLVMGenError.UnsupportedExpressionType;
                }

                const elem_ty = LLVMCore.LLVMGetElementType(ptr_ty);
                const elem_kind = LLVMCore.LLVMGetTypeKind(elem_ty);

                const current_value = LLVMCore.LLVMBuildLoad2(self.builder, elem_ty, ptr, "compound.load");
                var rhs_value = try self.generateExpr(compound.value.?);
                rhs_value = try self.coerceValueToType(rhs_value, elem_ty);

                const result: LLVMTypes.LLVMValueRef = switch (compound.operator.type) {
                    TokenType.PLUS_EQUAL => switch (elem_kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => LLVMCore.LLVMBuildAdd(self.builder, current_value, rhs_value, "compound.add"),
                        LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind, LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => LLVMCore.LLVMBuildFAdd(self.builder, current_value, rhs_value, "compound.fadd"),
                        else => return LLVMGenError.UnsupportedExpressionType,
                    },
                    TokenType.MINUS_EQUAL => switch (elem_kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => LLVMCore.LLVMBuildSub(self.builder, current_value, rhs_value, "compound.sub"),
                        LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind, LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => LLVMCore.LLVMBuildFSub(self.builder, current_value, rhs_value, "compound.fsub"),
                        else => return LLVMGenError.UnsupportedExpressionType,
                    },
                    TokenType.ASTERISK_EQUAL => switch (elem_kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => LLVMCore.LLVMBuildMul(self.builder, current_value, rhs_value, "compound.mul"),
                        LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind, LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => LLVMCore.LLVMBuildFMul(self.builder, current_value, rhs_value, "compound.fmul"),
                        else => return LLVMGenError.UnsupportedExpressionType,
                    },
                    TokenType.SLASH_EQUAL => switch (elem_kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => LLVMCore.LLVMBuildSDiv(self.builder, current_value, rhs_value, "compound.sdiv"),
                        LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind, LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => LLVMCore.LLVMBuildFDiv(self.builder, current_value, rhs_value, "compound.fdiv"),
                        else => return LLVMGenError.UnsupportedExpressionType,
                    },
                    TokenType.POWER_EQUAL => blk: {
                        const f64_ty = LLVMCore.LLVMDoubleTypeInContext(self.context);
                        var base = current_value;
                        var exponent = rhs_value;

                        switch (elem_kind) {
                            LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
                                base = LLVMCore.LLVMBuildSIToFP(self.builder, current_value, f64_ty, "compound.pow.base.i2f");
                                exponent = LLVMCore.LLVMBuildSIToFP(self.builder, rhs_value, f64_ty, "compound.pow.exp.i2f");
                            },
                            LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
                                base = LLVMCore.LLVMBuildFPExt(self.builder, current_value, f64_ty, "compound.pow.base.ext");
                                exponent = LLVMCore.LLVMBuildFPExt(self.builder, rhs_value, f64_ty, "compound.pow.exp.ext");
                            },
                            LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => {},
                            else => return LLVMGenError.UnsupportedExpressionType,
                        }

                        const pow_fn = externs.getOrCreatePow(self);
                        var params = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                        const pow_ty = LLVMCore.LLVMFunctionType(f64_ty, &params, 2, @intFromBool(false));
                        var args = [_]LLVMTypes.LLVMValueRef{ base, exponent };
                        const pow_res = LLVMCore.LLVMBuildCall2(self.builder, pow_ty, pow_fn, &args, 2, "compound.pow");

                        switch (elem_kind) {
                            LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => break :blk LLVMCore.LLVMBuildFPToSI(self.builder, pow_res, elem_ty, "compound.pow.to.int"),
                            LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => break :blk LLVMCore.LLVMBuildFPTrunc(self.builder, pow_res, elem_ty, "compound.pow.to.float"),
                            LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => break :blk pow_res,
                            else => unreachable,
                        }
                    },
                    else => return LLVMGenError.UnsupportedExpressionType,
                };

                _ = LLVMCore.LLVMBuildStore(self.builder, result, ptr);
                return result;
            },

            .FunctionCall => |call| {
                const callee_value = try self.generateExpr(call.callee);

                var symbol_info: ?FunctionSymbol = null;
                if (call.callee.data == .Variable) {
                    const var_token = call.callee.data.Variable;
                    if (self.functions.get(var_token.lexeme)) |fn_symbol| {
                        symbol_info = fn_symbol;
                    }
                }

                var function_type = LLVMCore.LLVMTypeOf(callee_value);
                if (symbol_info) |info| {
                    const param_ptr = if (info.param_types.len == 0) null else info.param_types.ptr;
                    function_type = LLVMCore.LLVMFunctionType(info.return_type, param_ptr, @intCast(info.param_types.len), @intFromBool(false));
                } else {
                    if (LLVMCore.LLVMGetTypeKind(function_type) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        function_type = LLVMCore.LLVMGetElementType(function_type);
                    }
                }

                if (LLVMCore.LLVMGetTypeKind(function_type) != LLVMTypes.LLVMTypeKind.LLVMFunctionTypeKind) {
                    return LLVMGenError.UnsupportedExpressionType;
                }

                const arg_count = call.arguments.len;
                var arg_values = try self.allocator.alloc(LLVMTypes.LLVMValueRef, arg_count);
                defer self.allocator.free(arg_values);

                if (symbol_info) |info| {
                    if (info.param_types.len != arg_count) {
                        return LLVMGenError.UnsupportedExpressionType;
                    }

                    for (call.arguments, 0..) |argument, i| {
                        const expect_alias = info.param_alias_flags[i];
                        const expected_type = info.param_types[i];

                        var arg_value: LLVMTypes.LLVMValueRef = undefined;
                        if (expect_alias) {
                            arg_value = try self.getPointerForExpr(argument.expr);
                            const arg_ty = LLVMCore.LLVMTypeOf(arg_value);
                            if (arg_ty != expected_type and LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                                arg_value = LLVMCore.LLVMBuildBitCast(self.builder, arg_value, expected_type, "alias.cast");
                            }
                        } else {
                            arg_value = try self.generateExpr(argument.expr);
                            const arg_ty = LLVMCore.LLVMTypeOf(arg_value);
                            if (arg_ty != expected_type) {
                                if (LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind and LLVMCore.LLVMGetTypeKind(expected_type) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                                    arg_value = LLVMCore.LLVMBuildIntCast2(self.builder, arg_value, expected_type, "arg.intcast", @intFromBool(false));
                                } else if ((LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind or LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind) and (LLVMCore.LLVMGetTypeKind(expected_type) == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or LLVMCore.LLVMGetTypeKind(expected_type) == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind)) {
                                    arg_value = LLVMCore.LLVMBuildFPCast(self.builder, arg_value, expected_type, "arg.fpcast");
                                } else if (LLVMCore.LLVMGetTypeKind(expected_type) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind and LLVMCore.LLVMGetTypeKind(arg_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                                    arg_value = LLVMCore.LLVMBuildBitCast(self.builder, arg_value, expected_type, "arg.ptrcast");
                                }
                            }
                        }

                        arg_values[i] = arg_value;
                    }
                } else {
                    for (call.arguments, 0..) |argument, i| {
                        if (argument.is_alias) {
                            arg_values[i] = try self.getPointerForExpr(argument.expr);
                        } else {
                            arg_values[i] = try self.generateExpr(argument.expr);
                        }
                    }
                }

                return LLVMCore.LLVMBuildCall2(
                    self.builder,
                    function_type,
                    callee_value,
                    if (arg_count == 0) null else arg_values.ptr,
                    @intCast(arg_count),
                    "calltmp",
                );
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
                    LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => blk: {
                        const ty = LLVMCore.LLVMTypeOf(value);
                        if (LLVMCore.LLVMGetIntTypeWidth(ty) == 2) {
                            return self.buildPrintTetra(value);
                        }
                        std.debug.print("Calling print int\n", .{});
                        break :blk self.buildPrintInt(value);
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
                const alloca = self.createEntryAlloca(LLVMCore.LLVMGetInsertBlock(self.builder), var_type, name_with_null.ptr);

                _ = LLVMCore.LLVMBuildStore(self.builder, value, alloca);
                try self.variables.put(var_decl.name.lexeme, alloca);
            },

            .FunctionDecl => |func_decl| try self.generateFunctionDecl(&func_decl),

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

    fn generateFunctionDecl(self: *LLVMGenerator, func_decl: *const ast.Stmt.Data.FunctionDecl) LLVMGenError!void {
        const previous_block = LLVMCore.LLVMGetInsertBlock(self.builder);
        defer if (previous_block != null) LLVMCore.LLVMPositionBuilderAtEnd(self.builder, previous_block);

        var name_buffer: [256]u8 = undefined;
        const name_with_null = std.fmt.bufPrintZ(&name_buffer, "{s}", .{func_decl.name.lexeme}) catch return error.NameTooLong;

        if (self.functions.get(func_decl.name.lexeme) != null) {
            return LLVMGenError.Unexpected;
        }

        const param_count = func_decl.params.len;
        var param_types = std.array_list.Managed(LLVMTypes.LLVMTypeRef).init(self.allocator);
        defer param_types.deinit();
        var value_types = std.array_list.Managed(LLVMTypes.LLVMTypeRef).init(self.allocator);
        defer value_types.deinit();
        var alias_flags = std.array_list.Managed(bool).init(self.allocator);
        defer alias_flags.deinit();

        try param_types.ensureTotalCapacity(param_count);
        try value_types.ensureTotalCapacity(param_count);

        for (func_decl.params) |param| {
            var type_info_storage = ast.TypeInfo{ .base = .Int };
            if (param.type_expr) |type_expr| {
                const type_info_ptr = try ast.typeInfoFromExpr(self.allocator, type_expr);
                defer self.allocator.destroy(type_info_ptr);
                type_info_storage = type_info_ptr.*;
            }

            const llvm_base_type = try self.getLLVMTypeFromTypeInfo(type_info_storage);
            try value_types.append(llvm_base_type);
            try alias_flags.append(param.is_alias);

            if (param.is_alias) {
                try param_types.append(LLVMCore.LLVMPointerType(llvm_base_type, 0));
            } else {
                try param_types.append(llvm_base_type);
            }
        }

        const return_type = try self.getLLVMTypeFromTypeInfo(func_decl.return_type_info);
        const func_type = LLVMCore.LLVMFunctionType(
            return_type,
            if (param_types.items.len == 0) null else param_types.items.ptr,
            @intCast(param_types.items.len),
            @intFromBool(false),
        );

        const function = LLVMCore.LLVMAddFunction(self.module, name_with_null.ptr, func_type);
        const stored_param_types = try self.allocator.dupe(LLVMTypes.LLVMTypeRef, param_types.items);
        errdefer self.allocator.free(stored_param_types);
        const stored_alias_flags = try self.allocator.dupe(bool, alias_flags.items);
        errdefer self.allocator.free(stored_alias_flags);
        try self.functions.put(func_decl.name.lexeme, FunctionSymbol{
            .value = function,
            .param_types = stored_param_types,
            .param_alias_flags = stored_alias_flags,
            .return_type = return_type,
        });

        const previous_function = self.current_function;
        self.current_function = function;
        defer self.current_function = previous_function;

        const prev_scope = self.pushVariableScope();
        defer self.popVariableScope(prev_scope);

        const entry_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, function, "entry");
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, entry_block);

        for (func_decl.params, 0..) |param, i| {
            const param_value = LLVMCore.LLVMGetParam(function, @intCast(i));

            var param_name_buffer: [256]u8 = undefined;
            const param_name_z = std.fmt.bufPrintZ(&param_name_buffer, "{s}", .{param.name.lexeme}) catch return error.NameTooLong;

            if (param.is_alias) {
                try self.variables.put(param.name.lexeme, param_value);
            } else {
                const alloca = self.createEntryAlloca(entry_block, value_types.items[i], param_name_z.ptr);
                _ = LLVMCore.LLVMBuildStore(self.builder, param_value, alloca);
                try self.variables.put(param.name.lexeme, alloca);
            }
        }

        for (func_decl.body) |*stmt| {
            try self.generateStmt(stmt);
        }

        const insert_block = LLVMCore.LLVMGetInsertBlock(self.builder);
        if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
            if (LLVMCore.LLVMGetTypeKind(return_type) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind) {
                _ = LLVMCore.LLVMBuildRetVoid(self.builder);
            } else {
                const default_ret = try self.getDefaultValue(func_decl.return_type_info);
                _ = LLVMCore.LLVMBuildRet(self.builder, default_ret);
            }
        }
    }

    fn getPointerForExpr(self: *LLVMGenerator, expr: *const ast.Expr) LLVMGenError!LLVMTypes.LLVMValueRef {
        return switch (expr.data) {
            .Variable => |token| {
                if (self.variables.get(token.lexeme)) |ptr| {
                    return ptr;
                }
                return LLVMGenError.UndefinedVariable;
            },
            else => LLVMGenError.UnsupportedExpressionType,
        };
    }

    fn pushVariableScope(self: *LLVMGenerator) std.StringHashMap(LLVMTypes.LLVMValueRef) {
        const previous = self.variables;
        self.variables = std.StringHashMap(LLVMTypes.LLVMValueRef).init(self.allocator);
        return previous;
    }

    fn popVariableScope(self: *LLVMGenerator, previous: std.StringHashMap(LLVMTypes.LLVMValueRef)) void {
        self.variables.deinit();
        self.variables = previous;
    }

    fn getLLVMTypeFromTypeInfo(self: *LLVMGenerator, type_info: ast.TypeInfo) LLVMGenError!LLVMTypes.LLVMTypeRef {
        return switch (type_info.base) {
            .Int => LLVMCore.LLVMInt64TypeInContext(self.context),
            .Byte => LLVMCore.LLVMInt8TypeInContext(self.context),
            .Float => LLVMCore.LLVMDoubleTypeInContext(self.context),
            .String => LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(self.context), 0),
            .Tetra => LLVMCore.LLVMIntTypeInContext(self.context, 2),
            .Nothing => LLVMCore.LLVMVoidTypeInContext(self.context),
            else => error.UnsupportedType,
        };
    }

    fn getDefaultValue(self: *LLVMGenerator, type_info: ast.TypeInfo) LLVMGenError!LLVMTypes.LLVMValueRef {
        const llvm_type = try self.getLLVMTypeFromTypeInfo(type_info);
        return LLVMCore.LLVMConstNull(llvm_type);
    }

    fn coerceValueToType(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef, target_ty: LLVMTypes.LLVMTypeRef) LLVMGenError!LLVMTypes.LLVMValueRef {
        const target_kind = LLVMCore.LLVMGetTypeKind(target_ty);
        const value_ty = LLVMCore.LLVMTypeOf(value);
        const value_kind = LLVMCore.LLVMGetTypeKind(value_ty);

        switch (target_kind) {
            LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
                if (value_kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                    const target_width = LLVMCore.LLVMGetIntTypeWidth(target_ty);
                    const value_width = LLVMCore.LLVMGetIntTypeWidth(value_ty);
                    if (target_width == value_width) return value;
                    if (value_width > target_width) {
                        return LLVMCore.LLVMBuildTrunc(self.builder, value, target_ty, "compound.trunc");
                    }
                    return LLVMCore.LLVMBuildSExt(self.builder, value, target_ty, "compound.sext");
                } else if (value_kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or value_kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
                    return LLVMCore.LLVMBuildFPToSI(self.builder, value, target_ty, "compound.fptosi");
                } else {
                    return LLVMGenError.UnsupportedExpressionType;
                }
            },
            LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
                if (value_kind == LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind or value_kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind) {
                    if (value_ty == target_ty) return value;
                    return LLVMCore.LLVMBuildFPCast(self.builder, value, target_ty, "compound.fpcast");
                } else if (value_kind == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                    return LLVMCore.LLVMBuildSIToFP(self.builder, value, target_ty, "compound.sitofp");
                } else {
                    return LLVMGenError.UnsupportedExpressionType;
                }
            },
            else => return LLVMGenError.UnsupportedExpressionType,
        }
    }

    fn toF64(self: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMGenError!LLVMTypes.LLVMValueRef {
        const ty = LLVMCore.LLVMTypeOf(v);
        const kind = LLVMCore.LLVMGetTypeKind(ty);
        switch (kind) {
            LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
                const double_ty = LLVMCore.LLVMDoubleTypeInContext(self.context);
                return LLVMCore.LLVMBuildSIToFP(self.builder, v, double_ty, "si2fp");
            },
            LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => {
                const double_ty = LLVMCore.LLVMDoubleTypeInContext(self.context);
                return LLVMCore.LLVMBuildFPExt(self.builder, v, double_ty, "fpext");
            },
            LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => return v,
            else => return LLVMGenError.UnsupportedBinaryOperator,
        }
    }

    fn createEntryAlloca(self: *LLVMGenerator, current_block: LLVMTypes.LLVMBasicBlockRef, ty: LLVMTypes.LLVMTypeRef, name: [*:0]const u8) LLVMTypes.LLVMValueRef {
        const function = LLVMCore.LLVMGetBasicBlockParent(current_block);
        const entry = LLVMCore.LLVMGetEntryBasicBlock(function);
        const first_inst = LLVMCore.LLVMGetFirstInstruction(entry);
        if (first_inst != null) {
            const tmp_builder = LLVMCore.LLVMCreateBuilderInContext(self.context);
            defer LLVMCore.LLVMDisposeBuilder(tmp_builder);
            LLVMCore.LLVMPositionBuilderBefore(tmp_builder, first_inst);
            return LLVMCore.LLVMBuildAlloca(tmp_builder, ty, name);
        } else {
            const tmp_builder = LLVMCore.LLVMCreateBuilderInContext(self.context);
            defer LLVMCore.LLVMDisposeBuilder(tmp_builder);
            LLVMCore.LLVMPositionBuilderAtEnd(tmp_builder, entry);
            return LLVMCore.LLVMBuildAlloca(tmp_builder, ty, name);
        }
    }

    fn toBoolI1(self: *LLVMGenerator, v: LLVMTypes.LLVMValueRef) LLVMGenError!LLVMTypes.LLVMValueRef {
        const ty = LLVMCore.LLVMTypeOf(v);
        const kind = LLVMCore.LLVMGetTypeKind(ty);
        switch (kind) {
            LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
                const width = LLVMCore.LLVMGetIntTypeWidth(ty);
                if (width == 1) return v;
                if (width == 2) {
                    // Tetra: then-branch triggers when low bit is set
                    const ty_i2 = ty;
                    const one = LLVMCore.LLVMConstInt(ty_i2, 1, @intFromBool(false));
                    const andv = LLVMCore.LLVMBuildAnd(self.builder, v, one, "tetra.lowbit");
                    const ty_i1 = LLVMCore.LLVMInt1TypeInContext(self.context);
                    const zero1 = LLVMCore.LLVMConstInt(ty_i1, 0, @intFromBool(false));
                    return LLVMCore.LLVMBuildICmp(self.builder, LLVMTypes.LLVMIntPredicate.LLVMIntNE, LLVMCore.LLVMBuildTrunc(self.builder, andv, ty_i1, "tetra.bit"), zero1, "tobool.tetra");
                }
                const zero = LLVMCore.LLVMConstNull(ty);
                return LLVMCore.LLVMBuildICmp(self.builder, LLVMTypes.LLVMIntPredicate.LLVMIntNE, v, zero, "tobool");
            },
            LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
                const null_ptr = LLVMCore.LLVMConstNull(ty);
                return LLVMCore.LLVMBuildICmp(self.builder, LLVMTypes.LLVMIntPredicate.LLVMIntNE, v, null_ptr, "tobool.ptr");
            },
            LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind, LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => {
                const zero = if (kind == LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind)
                    LLVMCore.LLVMConstReal(ty, 0.0)
                else
                    LLVMCore.LLVMConstReal(ty, 0.0);
                return LLVMCore.LLVMBuildFCmp(self.builder, LLVMTypes.LLVMRealPredicate.LLVMRealONE, v, zero, "tobool.fp");
            },
            else => return LLVMGenError.UnsupportedLogicalOperator,
        }
    }

    pub fn verify(self: *LLVMGenerator) LLVMGenError!void {
        var msg: [*c]u8 = null;
        const failed = LLVMAnalysis.LLVMVerifyModule(self.module, LLVMTypes.LLVMVerifierFailureAction.LLVMReturnStatusAction, &msg);
        if (failed != 0) {
            if (msg != null) {
                std.debug.print("IR verification failed: {s}\n", .{msg});
                LLVMCore.LLVMDisposeMessage(msg);
            }
            return LLVMGenError.IRGenerationFailed;
        }
        if (msg != null) LLVMCore.LLVMDisposeMessage(msg);
    }

    fn optimize(self: *LLVMGenerator) void {
        // Prototype: keep simple mapping; real pass managers can be added later
        _ = self;
    }

    fn generatePatternValue(self: *LLVMGenerator, pattern: Token) LLVMGenError!LLVMTypes.LLVMValueRef {
        // For now, assume patterns are just enum variants stored as integers (i64 for consistency)
        return LLVMCore.LLVMConstInt(LLVMCore.LLVMInt64TypeInContext(self.context), @intFromEnum(pattern.type), @intFromBool(false));
    }

    fn buildPrintInt(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef) !LLVMTypes.LLVMValueRef {
        const printf_fn = externs.getOrCreatePrintf(self);
        const format_str = try strings.createStringPtr(self, "%d\n", &next_string_id);

        // Create arguments array
        var args = [_]LLVMTypes.LLVMValueRef{ format_str, value };

        const fnty = LLVMCore.LLVMGetElementType(LLVMCore.LLVMTypeOf(printf_fn));
        return LLVMCore.LLVMBuildCall2(self.builder, fnty, printf_fn, &args, 2, "");
    }

    fn buildPrintString(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef) !LLVMTypes.LLVMValueRef {
        const puts_fn = externs.getOrCreatePuts(self);
        var args = [_]LLVMTypes.LLVMValueRef{value};
        const fnty = LLVMCore.LLVMGetElementType(LLVMCore.LLVMTypeOf(puts_fn));
        return LLVMCore.LLVMBuildCall2(self.builder, fnty, puts_fn, &args, 1, "");
    }

    fn buildPrintTetra(self: *LLVMGenerator, value: LLVMTypes.LLVMValueRef) !LLVMTypes.LLVMValueRef {
        // Map i2 to string labels via small switch: 0->"neither", 1->"true", 2->"false", 3->"both"
        const i64_ty = LLVMCore.LLVMInt64TypeInContext(self.context);
        const casted = LLVMCore.LLVMBuildZExt(self.builder, value, i64_ty, "tetra.zext");

        const default_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.default");
        const merge_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.merge");

        const switch_inst = LLVMCore.LLVMBuildSwitch(self.builder, casted, default_block, 4);

        const str_neither = try strings.createStringPtr(self, "neither\n", &next_string_id);
        const str_true = try strings.createStringPtr(self, "true\n", &next_string_id);
        const str_false = try strings.createStringPtr(self, "false\n", &next_string_id);
        const str_both = try strings.createStringPtr(self, "both\n", &next_string_id);

        const case0 = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.case0");
        const case1 = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.case1");
        const case2 = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.case2");
        const case3 = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "tetra.case3");

        LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 0, @intFromBool(false)), case0);
        LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 1, @intFromBool(false)), case1);
        LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 2, @intFromBool(false)), case2);
        LLVMCore.LLVMAddCase(switch_inst, LLVMCore.LLVMConstInt(i64_ty, 3, @intFromBool(false)), case3);

        // printf("%s", str)
        const printf_fn = externs.getOrCreatePrintf(self);
        const printf_type = LLVMCore.LLVMTypeOf(printf_fn);

        // case 0
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case0);
        var args0 = [_]LLVMTypes.LLVMValueRef{str_neither};
        _ = LLVMCore.LLVMBuildCall2(self.builder, printf_type, printf_fn, &args0, 1, "");
        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

        // case 1
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case1);
        var args1 = [_]LLVMTypes.LLVMValueRef{str_true};
        _ = LLVMCore.LLVMBuildCall2(self.builder, printf_type, printf_fn, &args1, 1, "");
        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

        // case 2
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case2);
        var args2 = [_]LLVMTypes.LLVMValueRef{str_false};
        _ = LLVMCore.LLVMBuildCall2(self.builder, printf_type, printf_fn, &args2, 1, "");
        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

        // case 3
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case3);
        var args3 = [_]LLVMTypes.LLVMValueRef{str_both};
        _ = LLVMCore.LLVMBuildCall2(self.builder, printf_type, printf_fn, &args3, 1, "");
        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

        // default
        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, default_block);
        _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

        LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
        return null;
    }

    // createStringConstant removed in favor of common/strings.createStringPtr
};
