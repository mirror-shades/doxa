const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const HIR = @import("../../hir/soxa_types.zig");
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub fn mapHIRTypeToLLVM(gen: *LLVMGenerator, ty: HIR.HIRType) LLVMTypes.LLVMTypeRef {
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
