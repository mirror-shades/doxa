const std = @import("std");
const llvm = @import("llvm");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMTargetMachine = llvm.target_machine;
const LLVMTarget = llvm.target;
const LLVMAnalysis = llvm.analysis;
const LLVMTransform = llvm.transform;
const externs = @import("common/externs.zig");
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

pub const LLVMGenerator = struct {
    context: LLVMTypes.LLVMContextRef,
    module: LLVMTypes.LLVMModuleRef,
    builder: LLVMTypes.LLVMBuilderRef,
    target_machine: LLVMTypes.LLVMTargetMachineRef,

    // Symbol tables for tracking variables and types
    variables: std.StringHashMap(LLVMTypes.LLVMValueRef),
    types: std.StringHashMap(LLVMTypes.LLVMTypeRef),
    externs: std.StringHashMap(LLVMTypes.LLVMValueRef),
    allocator: std.mem.Allocator,

    // Current function being generated
    current_function: ?LLVMTypes.LLVMValueRef,

    // Debug controls
    debug_peek: bool,

    pub fn init(allocator: std.mem.Allocator) !*LLVMGenerator {
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
            .externs = std.StringHashMap(LLVMTypes.LLVMValueRef).init(allocator),
            .allocator = allocator,
            .current_function = null,
            .debug_peek = false,
        };

        return generator;
    }

    pub fn deinit(self: *LLVMGenerator) void {
        self.variables.deinit();
        self.types.deinit();
        self.externs.deinit();
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
        // Dump IR before verification to help diagnose verifier failures
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
                    .PLUS => LLVMCore.LLVMBuildAdd(self.builder, lhs, rhs, "add"),
                    .MINUS => LLVMCore.LLVMBuildSub(self.builder, lhs, rhs, "sub"),
                    .ASTERISK => LLVMCore.LLVMBuildMul(self.builder, lhs, rhs, "mul"),
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

                const switch_inst = LLVMCore.LLVMBuildSwitch(self.builder, cond_val, default_block, @intCast(match_expr.cases.len));

                // Generate code for each case
                for (match_expr.cases) |case| {
                    const case_block = LLVMCore.LLVMAppendBasicBlockInContext(self.context, self.current_function.?, "match.case");
                    const pattern_value = try self.generatePatternValue(case.pattern);
                    LLVMCore.LLVMAddCase(switch_inst, pattern_value, case_block);

                    LLVMCore.LLVMPositionBuilderAtEnd(self.builder, case_block);
                    _ = try self.generateExpr(case.body);
                    _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);
                }

                // Default path just goes to merge
                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, default_block);
                _ = LLVMCore.LLVMBuildBr(self.builder, merge_block);

                LLVMCore.LLVMPositionBuilderAtEnd(self.builder, merge_block);
                return null;
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
        // Prototype: skip adding transform passes to avoid extra link deps
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
