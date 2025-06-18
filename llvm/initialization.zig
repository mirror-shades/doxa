const LLVMtype = @import("types.zig");

pub extern fn LLVMGetGlobalPassRegistry() LLVMtype.LLVMPassRegistryRef;
pub extern fn LLVMInitializeCore(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeTransformUtils(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeScalarOpts(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeVectorization(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeInstCombine(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeIPO(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeAnalysis(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeIPA(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeCodeGen(R: LLVMtype.LLVMPassRegistryRef) void;
pub extern fn LLVMInitializeTarget(R: LLVMtype.LLVMPassRegistryRef) void;

// We only need these functions as they are the ones available in LLVM-C.dll
pub extern fn LLVMInitializeNativeTarget() c_int;
pub extern fn LLVMInitializeNativeAsmParser() c_int;
pub extern fn LLVMInitializeNativeAsmPrinter() c_int;
pub extern fn LLVMInitializeNativeDisassembler() c_int;
