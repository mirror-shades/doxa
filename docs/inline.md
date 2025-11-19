
`zig` blocks should be limited in the following ways. 
1) they can only define functions, no global state (other than imports)
2) they must have valid doxa types (int - i64, float - f64, byte - u8, string - []const u8, tetra(flattened) - boolean, nothing - void) for all parameters and it return values


``` 
zig ZigOps {
    fn fast_hash(data: []const u8) -> u64 {
        return @llvm_external_call("ZigOps.fast_hash", data.ptr, data.len);
    }
    
    fn sqrt_approx(x: f64) -> f64 {
        return @llvm_external_call("ZigOps.sqrt_approx", x);
    }
}

const zigSqrt is ZigOps.sqrt_approx(10.5)
```

the zig within the block is compiled to an object file and caches after HIR gen and before the pipeline splits between VM and compiler. the object can then be both linked in compilation or accessed jit in vm execution. this adds a small amount of overhead for VM execution but only for the first run due to cacheing.

with some clairity on rules, molecular types could be provided as well. structs would need to have a matching zig counterpart, and arrays would need to be handled as slices, etc. This can come later when the implementation is mature.

