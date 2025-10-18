const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

pub const VarPtrEntry = struct {
    ptr: LLVMTypes.LLVMValueRef,
    elem_ty: LLVMTypes.LLVMTypeRef,
};

pub const VarStore = struct {
    gen: *LLVMGenerator,
    entry_block: LLVMTypes.LLVMBasicBlockRef,
    map: std.StringHashMap(VarPtrEntry),

    pub fn init(allocator: std.mem.Allocator, gen: *LLVMGenerator, entry_block: LLVMTypes.LLVMBasicBlockRef) VarStore {
        return .{ .gen = gen, .entry_block = entry_block, .map = std.StringHashMap(VarPtrEntry).init(allocator) };
    }

    pub fn deinit(self: *VarStore) void {
        self.map.deinit();
    }

    pub fn store(self: *VarStore, key: []const u8, value: LLVMTypes.LLVMValueRef, elem_ty: LLVMTypes.LLVMTypeRef, is_local: bool, var_name: []const u8) !void {
        if (is_local) {
            const existing = self.map.get(key);
            const entry: VarPtrEntry = blk: {
                if (existing) |e| break :blk e;
                const curr_bb = LLVMCore.LLVMGetInsertBlock(self.gen.builder);
                LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, self.entry_block);
                const ptr = LLVMCore.LLVMBuildAlloca(self.gen.builder, elem_ty, (try self.gen.allocator.dupeZ(u8, var_name)).ptr);
                LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, curr_bb);
                const e = VarPtrEntry{ .ptr = ptr, .elem_ty = elem_ty };
                try self.map.put(key, e);
                break :blk e;
            };
            _ = LLVMCore.LLVMBuildStore(self.gen.builder, value, entry.ptr);
        } else {
            const existing = self.map.get(key);
            const entry: VarPtrEntry = blk2: {
                if (existing) |e| break :blk2 e;
                const zsym = try self.gen.allocator.dupeZ(u8, key);
                const g = LLVMCore.LLVMAddGlobal(self.gen.module, elem_ty, zsym.ptr);
                LLVMCore.LLVMSetLinkage(g, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
                _ = LLVMCore.LLVMSetInitializer(g, LLVMCore.LLVMConstNull(elem_ty));
                const e = VarPtrEntry{ .ptr = g, .elem_ty = elem_ty };
                try self.map.put(key, e);
                break :blk2 e;
            };
            _ = LLVMCore.LLVMBuildStore(self.gen.builder, value, entry.ptr);
        }
    }

    pub fn load(self: *VarStore, key: []const u8, default_ty: LLVMTypes.LLVMTypeRef, default_name: []const u8) !LLVMTypes.LLVMValueRef {
        if (self.map.get(key)) |entry| {
            return LLVMCore.LLVMBuildLoad2(self.gen.builder, entry.elem_ty, entry.ptr, "load");
        }
        const curr_bb = LLVMCore.LLVMGetInsertBlock(self.gen.builder);
        LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, self.entry_block);
        const ptr = LLVMCore.LLVMBuildAlloca(self.gen.builder, default_ty, (try self.gen.allocator.dupeZ(u8, default_name)).ptr);
        _ = LLVMCore.LLVMBuildStore(self.gen.builder, LLVMCore.LLVMConstNull(default_ty), ptr);
        LLVMCore.LLVMPositionBuilderAtEnd(self.gen.builder, curr_bb);
        const entry = VarPtrEntry{ .ptr = ptr, .elem_ty = default_ty };
        try self.map.put(key, entry);
        return LLVMCore.LLVMBuildLoad2(self.gen.builder, default_ty, ptr, "load");
    }

    pub fn defineConstGlobal(self: *VarStore, key: []const u8, value: LLVMTypes.LLVMValueRef) !void {
        const val_ty = LLVMCore.LLVMTypeOf(value);
        const zsym = try self.gen.allocator.dupeZ(u8, key);
        const g = LLVMCore.LLVMAddGlobal(self.gen.module, val_ty, zsym.ptr);
        LLVMCore.LLVMSetLinkage(g, LLVMTypes.LLVMLinkage.LLVMExternalLinkage);
        if (LLVMCore.LLVMIsConstant(value) != 0) {
            LLVMCore.LLVMSetInitializer(g, value);
            LLVMCore.LLVMSetGlobalConstant(g, @intFromBool(true));
        } else {
            _ = LLVMCore.LLVMSetInitializer(g, LLVMCore.LLVMConstNull(val_ty));
            _ = LLVMCore.LLVMBuildStore(self.gen.builder, value, g);
        }
        try self.map.put(key, .{ .ptr = g, .elem_ty = val_ty });
    }
};
