const std = @import("std");
const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub fn createStringPtr(gen: *LLVMGenerator, s: []const u8, next_string_id: *usize) !LLVMTypes.LLVMValueRef {
    if (gen.string_literals.get(s)) |existing| {
        return existing;
    }

    const key_copy = try gen.allocator.dupe(u8, s);
    errdefer gen.allocator.free(key_copy);
    var buf = try gen.allocator.alloc(u8, s.len + 1);
    defer gen.allocator.free(buf);
    @memcpy(buf[0..s.len], s);
    buf[s.len] = 0;

    const str_val = LLVMCore.LLVMConstStringInContext(gen.context, @ptrCast(buf.ptr), @intCast(buf.len), @intFromBool(false));

    var name_buf: [32]u8 = undefined;
    const id = next_string_id.*;
    next_string_id.* += 1;
    const zname = std.fmt.bufPrintZ(&name_buf, "str.lit.{d}", .{id}) catch "str.lit";

    const array_ty = LLVMCore.LLVMTypeOf(str_val);
    const global = LLVMCore.LLVMAddGlobal(gen.module, array_ty, zname.ptr);
    LLVMCore.LLVMSetInitializer(global, str_val);
    LLVMCore.LLVMSetGlobalConstant(global, @intFromBool(true));
    LLVMCore.LLVMSetLinkage(global, LLVMTypes.LLVMLinkage.LLVMPrivateLinkage);
    LLVMCore.LLVMSetUnnamedAddr(global, LLVMTypes.LLVMUnnamedAddr.LLVMGlobalUnnamedAddr);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const zero = LLVMCore.LLVMConstInt(i32_ty, 0, @intFromBool(false));
    var idx = [_]LLVMTypes.LLVMValueRef{ zero, zero };
    const gep = LLVMCore.LLVMConstInBoundsGEP2(array_ty, global, &idx, 2);
    const i8_ptr_ty = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
    const ptr_const = LLVMCore.LLVMConstBitCast(gep, i8_ptr_ty);
    try gen.string_literals.put(key_copy, ptr_const);
    return ptr_const;
}
