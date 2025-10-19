const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub const BlockManager = struct {
    gen: *LLVMGenerator,
    function: LLVMTypes.LLVMValueRef,
    map: std.StringHashMap(LLVMTypes.LLVMBasicBlockRef),
    next_block_id: *usize,

    pub fn init(allocator: std.mem.Allocator, gen: *LLVMGenerator, function: LLVMTypes.LLVMValueRef, next_block_id: *usize) BlockManager {
        return .{
            .gen = gen,
            .function = function,
            .map = std.StringHashMap(LLVMTypes.LLVMBasicBlockRef).init(allocator),
            .next_block_id = next_block_id,
        };
    }

    pub fn deinit(self: *BlockManager) void {
        self.map.deinit();
    }

    pub fn getOrCreate(self: *BlockManager, name: []const u8) !LLVMTypes.LLVMBasicBlockRef {
        if (self.map.get(name)) |bb| return bb;
        const zname = try self.gen.allocator.dupeZ(u8, name);
        const bb = LLVMCore.LLVMAppendBasicBlockInContext(self.gen.context, self.function, zname.ptr);
        try self.map.put(name, bb);
        return bb;
    }

    pub fn bind(self: *BlockManager, name: []const u8, bb: LLVMTypes.LLVMBasicBlockRef) !void {
        try self.map.put(name, bb);
    }

    pub fn branchTo(self: *BlockManager, label: []const u8) !void {
        const target = try self.getOrCreate(label);
        _ = LLVMCore.LLVMBuildBr(self.gen.builder, target);
        self.positionAtContinuation();
    }

    pub fn branchCond(self: *BlockManager, cond_i1: LLVMTypes.LLVMValueRef, true_label: []const u8, false_label: []const u8) !void {
        const t = try self.getOrCreate(true_label);
        const f = try self.getOrCreate(false_label);
        _ = LLVMCore.LLVMBuildCondBr(self.gen.builder, cond_i1, t, f);
        self.positionAtContinuation();
    }

    pub fn enterLabel(self: *BlockManager, label: []const u8) !void {
        const insert_block = LLVMCore.LLVMGetInsertBlock(self.gen.builder);
        const bb = try self.getOrCreate(label);
        const need_branch = insert_block != null and insert_block != bb and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null;
        if (need_branch) _ = LLVMCore.LLVMBuildBr(self.gen.builder, bb);
        LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, bb);
    }

    fn positionAtContinuation(self: *BlockManager) void {
        var name_buf: [32]u8 = undefined;
        const cont_z = std.fmt.bufPrintZ(&name_buf, "cont.{d}", .{self.next_block_id.*}) catch "cont";
        self.next_block_id.* += 1;
        const cont_bb = LLVMCore.LLVMAppendBasicBlockInContext(self.gen.context, self.function, cont_z.ptr);
        LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, cont_bb);
    }
};
