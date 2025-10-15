const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;

const HIR = @import("../hir/soxa_types.zig");
const HIRValue = @import("../hir/soxa_values.zig").HIRValue;
const HIRInstruction = @import("../hir/soxa_instructions.zig").HIRInstruction;
const LLVMGenerator = @import("llvm.zig").LLVMGenerator;

var next_string_id: usize = 0; // unique IDs for string globals within this module

fn mapHIRTypeToLLVM(gen: *LLVMGenerator, ty: HIR.HIRType) LLVMTypes.LLVMTypeRef {
    return switch (ty) {
        .Int => LLVMCore.LLVMInt64TypeInContext(gen.context),
        .Byte => LLVMCore.LLVMInt8TypeInContext(gen.context),
        .Float => LLVMCore.LLVMDoubleTypeInContext(gen.context),
        .String => LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0),
        .Tetra => LLVMCore.LLVMIntTypeInContext(gen.context, 2),
        .Nothing => LLVMCore.LLVMVoidTypeInContext(gen.context),
        else => LLVMCore.LLVMInt64TypeInContext(gen.context),
    };
}

fn getOrCreatePuts(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "puts");
    if (existing != null) return existing;
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const puts_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(false));
    return LLVMCore.LLVMAddFunction(gen.module, "puts", puts_ty);
}

fn getOrCreatePrintf(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const existing = LLVMCore.LLVMGetNamedFunction(gen.module, "printf");
    if (existing != null) return existing;
    var params = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &params, 1, @intFromBool(true));
    return LLVMCore.LLVMAddFunction(gen.module, "printf", printf_ty);
}

fn createStringPtr(gen: *LLVMGenerator, s: []const u8) !LLVMTypes.LLVMValueRef {
    // Create a null-terminated copy to satisfy [*:0]const u8
    var buf = try gen.allocator.alloc(u8, s.len + 1);
    defer gen.allocator.free(buf);
    @memcpy(buf[0..s.len], s);
    buf[s.len] = 0;

    const str_val = LLVMCore.LLVMConstStringInContext(gen.context, @ptrCast(buf.ptr), @intCast(buf.len), @intFromBool(false));

    // Generate a unique name per call to avoid collisions between multiple literals
    var name_buf: [32]u8 = undefined;
    const id = next_string_id;
    next_string_id += 1;
    const zname = std.fmt.bufPrintZ(&name_buf, "str.lit.{d}", .{id}) catch "str.lit";

    const global = LLVMCore.LLVMAddGlobal(gen.module, LLVMCore.LLVMTypeOf(str_val), zname.ptr);
    LLVMCore.LLVMSetInitializer(global, str_val);
    LLVMCore.LLVMSetGlobalConstant(global, @intFromBool(true));
    LLVMCore.LLVMSetLinkage(global, LLVMTypes.LLVMLinkage.LLVMPrivateLinkage);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const zero = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
    var idx = [_]LLVMTypes.LLVMValueRef{ zero, zero };
    const gep = LLVMCore.LLVMBuildGEP2(gen.builder, LLVMCore.LLVMTypeOf(str_val), global, &idx, 2, "str.gep");
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    return LLVMCore.LLVMBuildBitCast(gen.builder, gep, i8_ptr_ty, "str.cast");
}

pub fn translateToLLVM(hir: *const HIR.HIRProgram, generator: *LLVMGenerator) !void {
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(generator.context);
    const fn_ty = LLVMCore.LLVMFunctionType(i32_ty, null, 0, @intFromBool(false));
    const main_fn = LLVMCore.LLVMAddFunction(generator.module, "main", fn_ty);
    const entry = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, main_fn, "entry");
    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, entry);

    var stack = std.array_list.Managed(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer stack.deinit();

    for (hir.instructions) |inst| {
        switch (inst) {
            .Const => |c| {
                const idx = c.constant_id;
                if (idx >= hir.constant_pool.len) continue;
                const hv = hir.constant_pool[idx];
                switch (hv) {
                    .string => |s| {
                        const ptr = try createStringPtr(generator, s);
                        try stack.append(ptr);
                    },
                    else => {},
                }
            },
            .Print => {
                if (stack.items.len > 0) {
                    const val = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    // Use printf("%s", str) to avoid implicit newline from puts
                    const printf_fn = getOrCreatePrintf(generator);
                    const fmt = try createStringPtr(generator, "%s");
                    var args = [_]LLVMTypes.LLVMValueRef{ fmt, val };
                    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                    var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                    _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                }
            },
            .PrintBegin => {},
            .PrintStr => |p| {
                if (p.const_id < hir.constant_pool.len) {
                    const hv = hir.constant_pool[p.const_id];
                    if (hv == .string) {
                        const str_ptr = try createStringPtr(generator, hv.string);
                        const printf_fn = getOrCreatePrintf(generator);
                        const fmt = try createStringPtr(generator, "%s");
                        var args = [_]LLVMTypes.LLVMValueRef{ fmt, str_ptr };
                        const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                        var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                        const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                        _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                    }
                }
            },
            .PrintVal => {
                if (stack.items.len > 0) {
                    const val = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    const printf_fn = getOrCreatePrintf(generator);
                    const fmt = try createStringPtr(generator, "%s");
                    // Currently, only strings are supported; future: numeric formats
                    var args = [_]LLVMTypes.LLVMValueRef{ fmt, val };
                    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                    var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                    const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                    _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 2, "printf");
                }
            },
            .PrintNewline => {
                const printf_fn = getOrCreatePrintf(generator);
                const nl = try createStringPtr(generator, "\n");
                var args = [_]LLVMTypes.LLVMValueRef{nl};
                const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(generator.context), 0);
                var printf_param_types = [_]LLVMTypes.LLVMTypeRef{i8_ptr_ty};
                const printf_ty = LLVMCore.LLVMFunctionType(i32_ty, &printf_param_types, 1, @intFromBool(true));
                _ = LLVMCore.LLVMBuildCall2(generator.builder, printf_ty, printf_fn, &args, 1, "printf");
            },
            .PrintEnd => {},
            .Halt => {
                const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
                _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
                break;
            },
            else => {},
        }
    }

    const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
    if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
        const zero_ret = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
        _ = LLVMCore.LLVMBuildRet(generator.builder, zero_ret);
    }
}
