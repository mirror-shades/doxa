const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const HIR = @import("../../hir/soxa_types.zig");
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub const ArrayFields = struct {
    pub const data: u32 = 0; // i8*
    pub const len: u32 = 1; // i64
    pub const cap: u32 = 2; // i64
    pub const elem_size: u32 = 3; // i64
    pub const elm_type: u32 = 4; // i64

};

pub fn getArrayHeaderType(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    const existing = LLVMCore.LLVMGetTypeByName(gen.module, "ArrayHeader");
    if (existing != null) return existing;
    // Create named struct; body is set in ensureArrayHeaderBody
    return LLVMCore.LLVMStructCreateNamed(gen.context, "ArrayHeader");
}

pub fn ensureArrayHeaderBody(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    const ty = getArrayHeaderType(gen);
    // If body is not set, set it now
    if (LLVMCore.LLVMIsOpaqueStruct(ty) != 0) {
        const i8_ptr = LLVMCore.LLVMPointerType(LLVMCore.LLVMInt8TypeInContext(gen.context), 0);
        const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
        var fields = [_]LLVMTypes.LLVMTypeRef{ i8_ptr, i64_ty, i64_ty, i64_ty, i64_ty };
        LLVMCore.LLVMStructSetBody(ty, &fields, fields.len, 0);
    }
    return ty;
}

pub fn getArrayHeaderPtrType(gen: *LLVMGenerator) LLVMTypes.LLVMTypeRef {
    const hdr = ensureArrayHeaderBody(gen);
    return LLVMCore.LLVMPointerType(hdr, 0);
}

pub fn elemSizeConst(gen: *LLVMGenerator, elem: HIR.HIRType) LLVMTypes.LLVMValueRef {
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    const sz: u64 = switch (elem) {
        .Int => 8,
        .Byte => 1,
        .Float => 8,
        .String => 8, // pointer size on 64-bit
        .Tetra => 1, // store tetra in 1 byte
        .Nothing => 0,
        else => 8,
    };
    return LLVMCore.LLVMConstInt(i64_ty, sz, 0);
}

pub fn headerSizeConst(gen: *LLVMGenerator) LLVMTypes.LLVMValueRef {
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    // sizeof(ArrayHeader) = i8* (8) + i64 (8) + i64 (8) + i64 (8) + i64 (8) = 40 on 64-bit
    return LLVMCore.LLVMConstInt(i64_ty, 40, 0);
}

pub fn elemTagConst(gen: *LLVMGenerator, elem: HIR.HIRType) LLVMTypes.LLVMValueRef {
    const i64_ty = LLVMCore.LLVMInt64TypeInContext(gen.context);
    const tag: u64 = switch (elem) {
        .Int => 0,
        .Byte => 1,
        .Float => 2,
        .String => 3,
        .Tetra => 4,
        .Nothing => 5,
        .Array => 6,
        .Struct => 7,
        .Enum => 8,
        else => 255,
    };
    return LLVMCore.LLVMConstInt(i64_ty, tag, 0);
}
