# Bytecode lowering (draft)

The bytecode layer sits between Soxa HIR and the runtime. Use
`BytecodeGenerator` to translate a `HIRProgram` into a `BytecodeModule`
that records canonical opcodes, preserved metadata, and function bounds.

```zig
const bc = @import("../src/codegen/bytecode/generator.zig");
const writer = @import("../src/codegen/bytecode/writer.zig");

var generator = bc.BytecodeGenerator.init(allocator, "out", "test");
var module = try generator.generate(hir_program);
defer module.deinit();

if (module.artifact_path) |path| {
    try writer.writeBytecodeModuleToFile(&module, path);
}
```

Key points:
- The generator mirrors the current HIR instruction set while
  normalising operands (variable slots, type tags, label ids).
- Jump targets are resolved to stable label ids; actual instruction
  offsets can be computed when the VM loads the module.
- `BytecodeModule` borrows the HIR constant and string pools—keep the
  HIR program alive while the bytecode module is in use.
- Function metadata (`start_ip`, `body_ip`, aliases, parameter types)
  is carried over so the VM can size frames without re-walking HIR.

This implementation intentionally returns `UnsupportedHIRInstruction`
for opcodes that are not yet lowered. Add handling in
`Context.lowerInstruction` as new VM features land.
