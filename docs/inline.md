

zig ZigOps {
    fn fast_hash(data: []const u8) -> u64 {
        return @llvm_external_call("ZigOps.fast_hash", data.ptr, data.len);
    }
    
    fn sqrt_approx(x: f64) -> f64 {
        return @llvm_external_call("ZigOps.sqrt_approx", x);
    }
}