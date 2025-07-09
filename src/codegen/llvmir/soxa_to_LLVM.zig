// pub fn translateToLLVM(hir: *HIRProgram, generator: *LLVMGenerator) !void {
//     // Pre-create all functions and global variables
//     for (hir.function_table) |func| {
//         const llvm_func = try generator.createFunction(func.qualified_name, func.arity, func.return_type);
//         try generator.function_map.put(func.qualified_name, llvm_func);
//     }

//     // Generate basic blocks for all labels
//     var label_map = std.StringHashMap(LLVMTypes.LLVMBasicBlockRef).init(generator.allocator);
//     for (hir.instructions) |instruction| {
//         if (instruction == .Label) {
//             const block = LLVMCore.LLVMAppendBasicBlock(generator.current_function, instruction.Label.name.ptr);
//             try label_map.put(instruction.Label.name, block);
//         }
//     }

//     // Generate instructions
//     for (hir.instructions) |instruction| {
//         switch (instruction) {
//             .Const => |c| {
//                 const llvm_const = try generator.createConstant(c.value);
//                 try generator.pushValue(llvm_const);
//             },
//             .LoadVar => |v| {
//                 const llvm_var = generator.symbol_table.get(v.var_name) orelse return error.UndefinedVariable;
//                 const loaded = LLVMCore.LLVMBuildLoad2(generator.builder, LLVMCore.LLVMTypeOf(llvm_var), llvm_var, "load");
//                 try generator.pushValue(loaded);
//             },
//             .IntArith => |a| {
//                 const rhs = generator.popValue();
//                 const lhs = generator.popValue();
//                 const result = switch (a.op) {
//                     .Add => LLVMCore.LLVMBuildAdd(generator.builder, lhs, rhs, "add"),
//                     .Sub => LLVMCore.LLVMBuildSub(generator.builder, lhs, rhs, "sub"),
//                     .Mul => LLVMCore.LLVMBuildMul(generator.builder, lhs, rhs, "mul"),
//                     else => unreachable,
//                 };
//                 try generator.pushValue(result);
//             },
//             .Jump => |j| {
//                 const target_block = label_map.get(j.label) orelse return error.UndefinedLabel;
//                 _ = LLVMCore.LLVMBuildBr(generator.builder, target_block);
//             },
//             .Call => |c| {
//                 const function = generator.function_map.get(c.qualified_name) orelse return error.UndefinedFunction;
//                 var args = try generator.popValues(c.arg_count);
//                 const result = LLVMCore.LLVMBuildCall2(generator.builder, LLVMCore.LLVMTypeOf(function), function, args.ptr, @intCast(args.len), "call");
//                 if (c.return_type != .Nothing) {
//                     try generator.pushValue(result);
//                 }
//             },
//             // ... other instructions with full semantic context
//         }
//     }
// }
