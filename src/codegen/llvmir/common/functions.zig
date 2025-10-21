const std = @import("std");
const llvm = @import("llvm");
const LLVMCore = llvm.core;
const LLVMTypes = llvm.types;

const HIR = @import("../../hir/soxa_types.zig");
const HIRInstruction = @import("../../hir/soxa_instructions.zig").HIRInstruction;
const LLVMGenerator = @import("../llvm.zig").LLVMGenerator;

const type_map = @import("types.zig");
const blocks = @import("blocks.zig");
const vars = @import("vars.zig");
const cast = @import("cast.zig");
const externs = @import("externs.zig");
const strings = @import("strings.zig");
const arrays = @import("arrays.zig");
const print = @import("print.zig");

var next_string_id: usize = 0;

fn findLabelIndex(hir: *const HIR.HIRProgram, label: []const u8) ?usize {
    for (hir.instructions, 0..) |inst, idx| {
        if (inst == .Label and std.mem.eql(u8, inst.Label.name, label)) return idx;
    }
    return null;
}

fn coerceToType(gen: *LLVMGenerator, v: LLVMTypes.LLVMValueRef, target_ty: LLVMTypes.LLVMTypeRef) LLVMTypes.LLVMValueRef {
    const kind = LLVMCore.LLVMGetTypeKind(target_ty);
    switch (kind) {
        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => {
            const w = LLVMCore.LLVMGetIntTypeWidth(target_ty);
            const src_ty = LLVMCore.LLVMTypeOf(v);
            if (LLVMCore.LLVMGetTypeKind(src_ty) == LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind) {
                const sw = LLVMCore.LLVMGetIntTypeWidth(src_ty);
                if (sw == w) return v;
                if (sw > w) return LLVMCore.LLVMBuildTrunc(gen.builder, v, target_ty, "trunc.int");
                return LLVMCore.LLVMBuildZExt(gen.builder, v, target_ty, "zext.int");
            }
            // Don't coerce pointers to integers - this is likely an error
            if (LLVMCore.LLVMGetTypeKind(src_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                return v; // Return as-is, let the type system handle it
            }
            const as64 = cast.ensureI64(gen, v);
            if (w == 64) return as64;
            return LLVMCore.LLVMBuildTrunc(gen.builder, as64, target_ty, "trunc.from.i64");
        },
        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind => return cast.ensureF64(gen, v),
        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => {
            const src_ty = LLVMCore.LLVMTypeOf(v);
            if (LLVMCore.LLVMGetTypeKind(src_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                // Both are pointers - check if they're the same type
                if (LLVMCore.LLVMTypeOf(v) == target_ty) {
                    return v;
                }
                // Different pointer types - perform bitcast
                return LLVMCore.LLVMBuildBitCast(gen.builder, v, target_ty, "bitcast.ptr");
            }
            return v; // Can't coerce non-pointer to pointer
        },
        else => return v,
    }
}

fn byteSaturatingAdd(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i16_ty = LLVMCore.LLVMInt16TypeInContext(gen.context);
    const a16 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, a), i16_ty, "a.z16");
    const b16 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, b), i16_ty, "b.z16");
    const sum16 = LLVMCore.LLVMBuildAdd(gen.builder, a16, b16, "add16");
    const max255 = LLVMCore.LLVMConstInt(i16_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, sum16, max255, "gt255");
    const clamped = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, sum16, "sel");
    return LLVMCore.LLVMBuildTrunc(gen.builder, clamped, i8_ty, "to.i8");
}

fn byteSaturatingSub(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const a8 = cast.ensureI8(gen, a);
    const b8 = cast.ensureI8(gen, b);
    const ge = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGE, a8, b8, "ge");
    const diff = LLVMCore.LLVMBuildSub(gen.builder, a8, b8, "sub8");
    const zero = LLVMCore.LLVMConstInt(i8_ty, 0, @intFromBool(false));
    return LLVMCore.LLVMBuildSelect(gen.builder, ge, diff, zero, "sel");
}

fn byteSaturatingMul(gen: *LLVMGenerator, a: LLVMTypes.LLVMValueRef, b: LLVMTypes.LLVMValueRef) LLVMTypes.LLVMValueRef {
    const i8_ty = LLVMCore.LLVMInt8TypeInContext(gen.context);
    const i32_ty = LLVMCore.LLVMInt32TypeInContext(gen.context);
    const a32 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, a), i32_ty, "a.z32");
    const b32 = LLVMCore.LLVMBuildZExt(gen.builder, cast.ensureI8(gen, b), i32_ty, "b.z32");
    const prod32 = LLVMCore.LLVMBuildMul(gen.builder, a32, b32, "mul32");
    const max255 = LLVMCore.LLVMConstInt(i32_ty, 255, @intFromBool(false));
    const gt = LLVMCore.LLVMBuildICmp(gen.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, prod32, max255, "gt255");
    const sel = LLVMCore.LLVMBuildSelect(gen.builder, gt, max255, prod32, "sel");
    return LLVMCore.LLVMBuildTrunc(gen.builder, sel, i8_ty, "to.i8");
}

pub fn predeclareFunctions(hir: *const HIR.HIRProgram, generator: *LLVMGenerator, function_map: *std.StringHashMap(LLVMTypes.LLVMValueRef), func_start_labels: *std.StringHashMap(bool)) !void {
    for (hir.function_table) |f| {
        const ret_ty = type_map.mapHIRTypeToLLVM(generator, f.return_type);
        var pbuf = try generator.allocator.alloc(LLVMTypes.LLVMTypeRef, f.param_types.len);
        defer generator.allocator.free(pbuf);
        for (f.param_types, 0..) |pt, i| {
            const base = type_map.mapHIRTypeToLLVM(generator, pt);
            pbuf[i] = if (f.param_is_alias[i]) LLVMCore.LLVMPointerType(base, 0) else base;
        }
        const fnty = LLVMCore.LLVMFunctionType(ret_ty, if (pbuf.len == 0) null else pbuf.ptr, @intCast(pbuf.len), @intFromBool(false));
        const zname = try generator.allocator.dupeZ(u8, f.qualified_name);
        const fnref = LLVMCore.LLVMAddFunction(generator.module, zname.ptr, fnty);
        try function_map.put(f.qualified_name, fnref);
        try func_start_labels.put(f.start_label, true);
    }
}

pub fn findFunctionsSectionStart(hir: *const HIR.HIRProgram, func_start_labels: *std.StringHashMap(bool)) usize {
    for (hir.instructions, 0..) |inst, idx| {
        if (inst == .Label) {
            const lbl = inst.Label.name;
            if (func_start_labels.get(lbl) != null) return idx;
        }
    }
    return hir.instructions.len;
}

pub fn emitFunctionBody(hir: *const HIR.HIRProgram, generator: *LLVMGenerator, function_map: *std.StringHashMap(LLVMTypes.LLVMValueRef), func: HIR.HIRProgram.HIRFunction, func_start_labels: *std.StringHashMap(bool), next_block_id_ptr: *usize) !void {
    const f_opt = function_map.get(func.qualified_name) orelse return;
    const fnref = f_opt.?;
    const fn_entry = LLVMCore.LLVMAppendBasicBlockInContext(generator.context, fnref, "entry");
    LLVMCore.LLVMPositionBuilderAtEnd(generator.builder, fn_entry);

    var stack = std.array_list.Managed(LLVMTypes.LLVMValueRef).init(generator.allocator);
    defer stack.deinit();

    var bm = blocks.BlockManager.init(generator.allocator, generator, fnref, next_block_id_ptr);
    defer bm.deinit();

    var var_store = vars.VarStore.init(generator.allocator, generator, fn_entry);
    defer var_store.deinit();

    const AliasEntry = struct { ptr: LLVMTypes.LLVMValueRef, elem_ty: LLVMTypes.LLVMTypeRef };
    var alias_slots = std.AutoHashMap(u32, AliasEntry).init(generator.allocator);
    defer alias_slots.deinit();

    const param_count: u32 = LLVMCore.LLVMCountParams(fnref);
    var p: u32 = 0;
    while (p < param_count) : (p += 1) {
        const pv = LLVMCore.LLVMGetParam(fnref, @intCast(p));
        try stack.append(pv);
    }

    const start_idx_opt = findLabelIndex(hir, func.start_label) orelse {
        const rt_missing = type_map.mapHIRTypeToLLVM(generator, func.return_type);
        if (LLVMCore.LLVMGetTypeKind(rt_missing) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind) {
            _ = LLVMCore.LLVMBuildRetVoid(generator.builder);
        } else {
            _ = LLVMCore.LLVMBuildRet(generator.builder, LLVMCore.LLVMConstNull(rt_missing));
        }
        return;
    };
    var end_idx: usize = hir.instructions.len;
    var i: usize = start_idx_opt + 1;
    while (i < hir.instructions.len) : (i += 1) {
        const ins = hir.instructions[i];
        if (ins == .Label) {
            const name = ins.Label.name;
            if (func_start_labels.get(name) != null) {
                end_idx = i;
                break;
            }
        }
    }

    // Bind entry block to start label name (so entering start label reuses it)
    try bm.bind(func.start_label, fn_entry);
    // We are already positioned at the start label block
    var pre_entered_start: bool = false;

    var in_dead_code: bool = false;
    for (hir.instructions[start_idx_opt..end_idx]) |inst| {
        if (in_dead_code) {
            switch (inst) {
                .Label => |lbl_dc| {
                    try bm.enterLabel(lbl_dc.name);
                    in_dead_code = false;
                },
                else => continue,
            }
        }
        switch (inst) {
            .ArrayGet => |ag| {
                _ = ag;
                if (stack.items.len < 2) continue;
                const idx_v = stack.items[stack.items.len - 1];
                const hdr = stack.items[stack.items.len - 2];
                stack.items.len -= 2;
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                const i8_ptr = LLVMCore.LLVMPointerType(i8_ty, 0);
                const hdr_ty = arrays.ensureArrayHeaderBody(generator);
                const zero = LLVMCore.LLVMConstInt(i64_ty, 0, 0);
                const idx_data = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.data, 0);
                const idx_len = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.len, 0);
                const idx_es = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.elem_size, 0);

                var gep_params = [_]LLVMTypes.LLVMValueRef{ zero, idx_len };
                const len_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.len");
                _ = len_ptr; // length unused here yet (bounds check TBD)
                gep_params[1] = idx_es;
                const es_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.es");
                const es_val = LLVMCore.LLVMBuildLoad2(generator.builder, i64_ty, es_ptr, "elem_size");

                const idx64 = cast.ensureI64(generator, idx_v);
                gep_params[1] = idx_data;
                const data_ptr_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.data.ptr");
                const data_ptr = LLVMCore.LLVMBuildLoad2(generator.builder, i8_ptr, data_ptr_ptr, "data.ptr");

                const byte_off = LLVMCore.LLVMBuildMul(generator.builder, idx64, es_val, "byte.off");
                var idxs = [_]LLVMTypes.LLVMValueRef{byte_off};
                const elem_ptr_i8 = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, i8_ty, data_ptr, &idxs, 1, "elem.i8p");
                const is_one = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, es_val, LLVMCore.LLVMConstInt(i64_ty, 1, 0), "es.is1");
                const i64_ptr = LLVMCore.LLVMPointerType(i64_ty, 0);
                const elem_ptr_i64 = LLVMCore.LLVMBuildBitCast(generator.builder, elem_ptr_i8, i64_ptr, "elem.i64p");
                const load_i64 = LLVMCore.LLVMBuildLoad2(generator.builder, i64_ty, elem_ptr_i64, "ld.i64");
                const load_i8 = LLVMCore.LLVMBuildLoad2(generator.builder, i8_ty, elem_ptr_i8, "ld.i8");
                const load_i8_z = LLVMCore.LLVMBuildZExt(generator.builder, load_i8, i64_ty, "i8.zext.i64");
                const sel = LLVMCore.LLVMBuildSelect(generator.builder, is_one, load_i8_z, load_i64, "elem.sel");
                try stack.append(sel);
            },
            .ArraySet => |as| {
                _ = as;
                if (stack.items.len < 3) continue;
                const val = stack.items[stack.items.len - 1];
                const idx_v = stack.items[stack.items.len - 2];
                const hdr = stack.items[stack.items.len - 3];
                stack.items.len -= 3;
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                const i8_ptr = LLVMCore.LLVMPointerType(i8_ty, 0);
                const hdr_ty = arrays.ensureArrayHeaderBody(generator);
                const zero = LLVMCore.LLVMConstInt(i64_ty, 0, 0);
                const one = LLVMCore.LLVMConstInt(i64_ty, 1, 0);
                const idx_data = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.data, 0);
                const idx_len = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.len, 0);
                const idx_es = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.elem_size, 0);
                const idx_cap = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.cap, 0);

                var gep_params = [_]LLVMTypes.LLVMValueRef{ zero, idx_len };
                const len_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.len");
                const curr_len = LLVMCore.LLVMBuildLoad2(generator.builder, i64_ty, len_ptr, "len");
                gep_params[1] = idx_es;
                const es_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.es");
                const es_val = LLVMCore.LLVMBuildLoad2(generator.builder, i64_ty, es_ptr, "elem_size");
                gep_params[1] = idx_cap;
                const cap_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.cap");
                _ = cap_ptr; // capacity unused (resize TBD)

                const idx64 = cast.ensureI64(generator, idx_v);
                gep_params[1] = idx_data;
                const data_ptr_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.data.ptr");
                const data_ptr = LLVMCore.LLVMBuildLoad2(generator.builder, i8_ptr, data_ptr_ptr, "data.ptr");

                const byte_off = LLVMCore.LLVMBuildMul(generator.builder, idx64, es_val, "byte.off");
                var idxs = [_]LLVMTypes.LLVMValueRef{byte_off};
                const elem_ptr_i8 = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, i8_ty, data_ptr, &idxs, 1, "elem.i8p");
                _ = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, es_val, LLVMCore.LLVMConstInt(i64_ty, 1, 0), "es.is1");
                const i64_ptr = LLVMCore.LLVMPointerType(i64_ty, 0);
                const elem_ptr_i64 = LLVMCore.LLVMBuildBitCast(generator.builder, elem_ptr_i8, i64_ptr, "elem.i64p");
                const val64 = cast.ensureI64(generator, val);
                const val8 = LLVMCore.LLVMBuildTrunc(generator.builder, val64, i8_ty, "val.trunc.i8");
                _ = LLVMCore.LLVMBuildStore(generator.builder, val8, elem_ptr_i8);
                _ = LLVMCore.LLVMBuildStore(generator.builder, val64, elem_ptr_i64);

                const ge_len = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntSGE, idx64, curr_len, "ge.len");
                const idxp1 = LLVMCore.LLVMBuildAdd(generator.builder, idx64, one, "idx.plus1");
                const new_len = LLVMCore.LLVMBuildSelect(generator.builder, ge_len, idxp1, curr_len, "len.sel");
                _ = LLVMCore.LLVMBuildStore(generator.builder, new_len, len_ptr);

                try stack.append(hdr);
            },
            .ArrayNew => |a| {
                const hdr_ptr_ty = arrays.getArrayHeaderPtrType(generator);
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                // i8_ptr not needed directly here; keep allocation via malloc
                const malloc_fn = externs.getOrCreateMalloc(generator);
                var m_params = [_]LLVMTypes.LLVMValueRef{arrays.headerSizeConst(generator)};
                const raw_hdr = LLVMCore.LLVMBuildCall2(generator.builder, LLVMCore.LLVMTypeOf(malloc_fn), malloc_fn, &m_params, 1, "malloc");
                const hdr = LLVMCore.LLVMBuildBitCast(generator.builder, raw_hdr, hdr_ptr_ty, "hdr");

                const cap_const = LLVMCore.LLVMConstInt(i64_ty, if (a.size == 0) 8 else a.size, @intFromBool(false));
                const len_const = LLVMCore.LLVMConstInt(i64_ty, if (a.size == 0) 0 else a.size, @intFromBool(false));
                const elem_sz = arrays.elemSizeConst(generator, a.element_type);
                const data_bytes = LLVMCore.LLVMBuildMul(generator.builder, cap_const, elem_sz, "data.bytes");
                var d_params = [_]LLVMTypes.LLVMValueRef{data_bytes};
                const raw_data = LLVMCore.LLVMBuildCall2(generator.builder, LLVMCore.LLVMTypeOf(malloc_fn), malloc_fn, &d_params, 1, "malloc");

                const zero = LLVMCore.LLVMConstInt(i64_ty, 0, @intFromBool(false));
                const idx_data = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.data, @intFromBool(false));
                const idx_len = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.len, @intFromBool(false));
                const idx_cap = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.cap, @intFromBool(false));
                const idx_es = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.elem_size, @intFromBool(false));
                const idx_tag = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.elm_type, @intFromBool(false));
                const hdr_ty = arrays.ensureArrayHeaderBody(generator);

                var gep_params = [_]LLVMTypes.LLVMValueRef{ zero, idx_data };
                const data_ptr_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.data.ptr");
                _ = LLVMCore.LLVMBuildStore(generator.builder, raw_data, data_ptr_ptr);
                gep_params[1] = idx_len;
                const len_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.len");
                _ = LLVMCore.LLVMBuildStore(generator.builder, len_const, len_ptr);
                gep_params[1] = idx_cap;
                const cap_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.cap");
                _ = LLVMCore.LLVMBuildStore(generator.builder, cap_const, cap_ptr);
                gep_params[1] = idx_es;
                const es_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.es");
                _ = LLVMCore.LLVMBuildStore(generator.builder, elem_sz, es_ptr);

                // Store element tag
                gep_params[1] = idx_tag;
                const tag_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, hdr, &gep_params, 2, "hdr.tag");
                const tag_val = arrays.elemTagConst(generator, a.element_type);
                _ = LLVMCore.LLVMBuildStore(generator.builder, tag_val, tag_ptr);

                try stack.append(hdr);
                // Also keep the header pointer on stack for immediate StoreDecl following var initialization
                try stack.append(hdr);
            },
            .ArrayLen => {
                if (stack.items.len < 1) continue;
                const arr_hdr = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const zero = LLVMCore.LLVMConstInt(i64_ty, 0, @intFromBool(false));
                const idx_len = LLVMCore.LLVMConstInt(i64_ty, arrays.ArrayFields.len, @intFromBool(false));
                const hdr_ty = arrays.ensureArrayHeaderBody(generator);
                var gep_params = [_]LLVMTypes.LLVMValueRef{ zero, idx_len };
                const len_ptr = LLVMCore.LLVMBuildInBoundsGEP2(generator.builder, hdr_ty, arr_hdr, &gep_params, 2, "hdr.len");
                const len_val = LLVMCore.LLVMBuildLoad2(generator.builder, i64_ty, len_ptr, "len");
                try stack.append(len_val);
            },
            .Const => |c| {
                const idx = c.constant_id;
                if (idx >= hir.constant_pool.len) continue;
                const hv = hir.constant_pool[idx];
                switch (hv) {
                    .string => |s| {
                        const ptr = try strings.createStringPtr(generator, s, &next_string_id);
                        try stack.append(ptr);
                    },
                    .int => |ival| {
                        const as_u64: u64 = @bitCast(ival);
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt64TypeInContext(generator.context), as_u64, @intFromBool(true));
                        try stack.append(v);
                    },
                    .byte => |b| {
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), b, @intFromBool(false));
                        try stack.append(v);
                    },
                    .float => |f| {
                        const v = LLVMCore.LLVMConstReal(LLVMCore.LLVMDoubleTypeInContext(generator.context), f);
                        try stack.append(v);
                    },
                    .tetra => |t| {
                        const v = LLVMCore.LLVMConstInt(LLVMCore.LLVMIntTypeInContext(generator.context, 2), t, @intFromBool(false));
                        try stack.append(v);
                    },
                    else => {},
                }
            },
            .Arith => |a| {
                if (stack.items.len < 2) continue;
                const right = stack.items[stack.items.len - 1];
                const left = stack.items[stack.items.len - 2];
                stack.items.len -= 2;
                var result: LLVMTypes.LLVMValueRef = null;
                switch (a.operand_type) {
                    .Float => {
                        const lf = cast.ensureF64(generator, left);
                        const rf = cast.ensureF64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildFAdd(generator.builder, lf, rf, "fadd"),
                            .Sub => LLVMCore.LLVMBuildFSub(generator.builder, lf, rf, "fsub"),
                            .Mul => LLVMCore.LLVMBuildFMul(generator.builder, lf, rf, "fmul"),
                            .Div => LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "fdiv"),
                            .Mod => LLVMCore.LLVMBuildFRem(generator.builder, lf, rf, "frem"),
                            .Pow => blk: {
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ lf, rf };
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                break :blk LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                            },
                        };
                    },
                    .Int => {
                        const li64 = cast.ensureI64(generator, left);
                        const ri64 = cast.ensureI64(generator, right);
                        result = switch (a.op) {
                            .Add => LLVMCore.LLVMBuildAdd(generator.builder, li64, ri64, "add"),
                            .Sub => LLVMCore.LLVMBuildSub(generator.builder, li64, ri64, "sub"),
                            .Mul => LLVMCore.LLVMBuildMul(generator.builder, li64, ri64, "mul"),
                            .Div => blk_div: {
                                const lf = cast.ensureF64(generator, li64);
                                const rf = cast.ensureF64(generator, ri64);
                                break :blk_div LLVMCore.LLVMBuildFDiv(generator.builder, lf, rf, "idiv.f");
                            },
                            .Mod => LLVMCore.LLVMBuildSRem(generator.builder, li64, ri64, "srem"),
                            .Pow => blk_pow: {
                                const lf = cast.ensureF64(generator, li64);
                                const rf = cast.ensureF64(generator, ri64);
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ lf, rf };
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                const dres = LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                                break :blk_pow LLVMCore.LLVMBuildFPToSI(generator.builder, dres, LLVMCore.LLVMInt64TypeInContext(generator.context), "pow.to.i64");
                            },
                        };
                    },
                    .Byte => {
                        result = switch (a.op) {
                            .Add => byteSaturatingAdd(generator, left, right),
                            .Sub => byteSaturatingSub(generator, left, right),
                            .Mul => byteSaturatingMul(generator, left, right),
                            .Div => blk_bdiv: {
                                const a8 = cast.ensureI8(generator, left);
                                const b8 = cast.ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const quot = LLVMCore.LLVMBuildUDiv(generator.builder, a8, b8, "udiv8");
                                break :blk_bdiv LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, quot, "sel");
                            },
                            .Mod => blk_bmod: {
                                const a8 = cast.ensureI8(generator, left);
                                const b8 = cast.ensureI8(generator, right);
                                const zero8 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt8TypeInContext(generator.context), 0, @intFromBool(false));
                                const is_zero = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b8, zero8, "is0");
                                const rem = LLVMCore.LLVMBuildURem(generator.builder, a8, b8, "urem8");
                                break :blk_bmod LLVMCore.LLVMBuildSelect(generator.builder, is_zero, zero8, rem, "sel");
                            },
                            .Pow => blk_bpow: {
                                const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                                const a_f = cast.ensureF64(generator, cast.ensureI8(generator, left));
                                const b_f = cast.ensureF64(generator, cast.ensureI8(generator, right));
                                const pow_fn = externs.getOrCreatePow(generator);
                                var args = [_]LLVMTypes.LLVMValueRef{ a_f, b_f };
                                var ptys = [_]LLVMTypes.LLVMTypeRef{ f64_ty, f64_ty };
                                const pty = LLVMCore.LLVMFunctionType(f64_ty, &ptys, 2, @intFromBool(false));
                                const dres = LLVMCore.LLVMBuildCall2(generator.builder, pty, pow_fn, &args, 2, "pow");
                                const as_u32 = LLVMCore.LLVMBuildFPToUI(generator.builder, dres, LLVMCore.LLVMInt32TypeInContext(generator.context), "fp2u32");
                                const max255 = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt32TypeInContext(generator.context), 255, @intFromBool(false));
                                const gt = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntUGT, as_u32, max255, "gt255");
                                const sel = LLVMCore.LLVMBuildSelect(generator.builder, gt, max255, as_u32, "sel");
                                const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                                break :blk_bpow LLVMCore.LLVMBuildTrunc(generator.builder, sel, i8_ty, "to.i8");
                            },
                        };
                    },
                    else => {
                        result = left;
                    },
                }
                try stack.append(result);
            },
            .Convert => |conv| {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;

                var out: LLVMTypes.LLVMValueRef = v;
                switch (conv.to_type) {
                    .Int => {
                        switch (conv.from_type) {
                            .Int => {
                                out = cast.ensureI64(generator, v);
                            },
                            .Byte => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildZExt(generator.builder, cast.ensureI8(generator, v), i64_ty, "zext.i64");
                            },
                            .Float => {
                                const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                                out = LLVMCore.LLVMBuildFPToSI(generator.builder, cast.ensureF64(generator, v), i64_ty, "fp2si.i64");
                            },
                            else => {},
                        }
                    },
                    .Byte => {
                        const i8_ty = LLVMCore.LLVMInt8TypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, v), i8_ty, "trunc.i8");
                            },
                            .Byte => {
                                out = cast.ensureI8(generator, v);
                            },
                            .Float => {
                                out = LLVMCore.LLVMBuildFPToUI(generator.builder, cast.ensureF64(generator, v), i8_ty, "fp2ui.i8");
                            },
                            else => {},
                        }
                    },
                    .Float => {
                        const f64_ty = LLVMCore.LLVMDoubleTypeInContext(generator.context);
                        switch (conv.from_type) {
                            .Int => {
                                out = LLVMCore.LLVMBuildSIToFP(generator.builder, cast.ensureI64(generator, v), f64_ty, "si2fp.f64");
                            },
                            .Byte => {
                                const i8v = cast.ensureI8(generator, v);
                                out = LLVMCore.LLVMBuildUIToFP(generator.builder, i8v, f64_ty, "ui2fp.f64");
                            },
                            .Float => {
                                out = cast.ensureF64(generator, v);
                            },
                            else => {},
                        }
                    },
                    else => {},
                }

                try stack.append(out);
            },
            .Compare => |cmp| {
                if (stack.items.len < 2) continue;
                const right = stack.items[stack.items.len - 1];
                const left = stack.items[stack.items.len - 2];
                stack.items.len -= 2;
                var result: LLVMTypes.LLVMValueRef = null;
                switch (cmp.operand_type) {
                    .Int => {
                        const li = cast.ensureI64(generator, left);
                        const ri = cast.ensureI64(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMIntPredicate.LLVMIntEQ,
                            .Ne => LLVMTypes.LLVMIntPredicate.LLVMIntNE,
                            .Lt => LLVMTypes.LLVMIntPredicate.LLVMIntSLT,
                            .Le => LLVMTypes.LLVMIntPredicate.LLVMIntSLE,
                            .Gt => LLVMTypes.LLVMIntPredicate.LLVMIntSGT,
                            .Ge => LLVMTypes.LLVMIntPredicate.LLVMIntSGE,
                        };
                        result = LLVMCore.LLVMBuildICmp(generator.builder, pred, li, ri, "icmp");
                    },
                    .Byte => {
                        const li8 = cast.ensureI8(generator, left);
                        const ri8 = cast.ensureI8(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMIntPredicate.LLVMIntEQ,
                            .Ne => LLVMTypes.LLVMIntPredicate.LLVMIntNE,
                            .Lt => LLVMTypes.LLVMIntPredicate.LLVMIntULT,
                            .Le => LLVMTypes.LLVMIntPredicate.LLVMIntULE,
                            .Gt => LLVMTypes.LLVMIntPredicate.LLVMIntUGT,
                            .Ge => LLVMTypes.LLVMIntPredicate.LLVMIntUGE,
                        };
                        result = LLVMCore.LLVMBuildICmp(generator.builder, pred, li8, ri8, "icmp");
                    },
                    .Float => {
                        const lf = cast.ensureF64(generator, left);
                        const rf = cast.ensureF64(generator, right);
                        const pred = switch (cmp.op) {
                            .Eq => LLVMTypes.LLVMRealPredicate.LLVMRealOEQ,
                            .Ne => LLVMTypes.LLVMRealPredicate.LLVMRealONE,
                            .Lt => LLVMTypes.LLVMRealPredicate.LLVMRealOLT,
                            .Le => LLVMTypes.LLVMRealPredicate.LLVMRealOLE,
                            .Gt => LLVMTypes.LLVMRealPredicate.LLVMRealOGT,
                            .Ge => LLVMTypes.LLVMRealPredicate.LLVMRealOGE,
                        };
                        result = LLVMCore.LLVMBuildFCmp(generator.builder, pred, lf, rf, "fcmp");
                    },
                    else => {
                        result = LLVMCore.LLVMConstInt(LLVMCore.LLVMInt1TypeInContext(generator.context), 0, @intFromBool(false));
                    },
                }
                try stack.append(result);
            },
            .LogicalOp => |lop| {
                // Operates on Tetra encoded as i2 on the stack
                const i2_ty = LLVMCore.LLVMIntTypeInContext(generator.context, 2);
                switch (lop.op) {
                    .Not => {
                        if (stack.items.len < 1) continue;
                        const a = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        const a2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, a), i2_ty, "to.i2");
                        const zero = LLVMCore.LLVMConstInt(i2_ty, 0, 0);
                        const one = LLVMCore.LLVMConstInt(i2_ty, 1, 0);
                        const both = LLVMCore.LLVMConstInt(i2_ty, 2, 0);
                        const neither = LLVMCore.LLVMConstInt(i2_ty, 3, 0);
                        const is_true = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, one, "is.true");
                        const is_both = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, both, "is.both");
                        const is_neither = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, neither, "is.neither");
                        const sel_tf = LLVMCore.LLVMBuildSelect(generator.builder, is_true, zero, one, "not.t_or_f");
                        const sel_both = LLVMCore.LLVMBuildSelect(generator.builder, is_both, both, sel_tf, "not.both");
                        const out = LLVMCore.LLVMBuildSelect(generator.builder, is_neither, neither, sel_both, "not.out");
                        try stack.append(out);
                    },
                    else => {
                        if (stack.items.len < 2) continue;
                        const b = stack.items[stack.items.len - 1];
                        const a = stack.items[stack.items.len - 2];
                        stack.items.len -= 2;
                        const a2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, a), i2_ty, "a.i2");
                        const b2 = LLVMCore.LLVMBuildTrunc(generator.builder, cast.ensureI64(generator, b), i2_ty, "b.i2");
                        const one = LLVMCore.LLVMConstInt(i2_ty, 1, @intFromBool(false));
                        _ = LLVMCore.LLVMConstInt(i2_ty, 2, @intFromBool(false));
                        // classical truth: true if low bit set (1 or both(2->10b has no low bit), we need (val==1 or val==2)
                        // Represent classical bit as i1 then zext back to i2 {00,false} or {01,true}
                        const two = LLVMCore.LLVMConstInt(i2_ty, 2, @intFromBool(false));
                        const a_is_true = blk_a: {
                            const is_one = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, one, "a.eq1");
                            const is_two = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, a2, two, "a.eq2");
                            break :blk_a LLVMCore.LLVMBuildOr(generator.builder, is_one, is_two, "a.true");
                        };
                        const b_is_true = blk_b: {
                            const is_one = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b2, one, "b.eq1");
                            const is_two = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, b2, two, "b.eq2");
                            break :blk_b LLVMCore.LLVMBuildOr(generator.builder, is_one, is_two, "b.true");
                        };
                        const i1_ty = LLVMCore.LLVMInt1TypeInContext(generator.context);
                        const and_i1 = LLVMCore.LLVMBuildAnd(generator.builder, a_is_true, b_is_true, "and");
                        const or_i1 = LLVMCore.LLVMBuildOr(generator.builder, a_is_true, b_is_true, "or");
                        const xor_i1 = LLVMCore.LLVMBuildXor(generator.builder, a_is_true, b_is_true, "xor");
                        const not_b = LLVMCore.LLVMBuildXor(generator.builder, b_is_true, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "not.b");
                        const implies_i1 = LLVMCore.LLVMBuildOr(generator.builder, not_b, a_is_true, "impl");
                        const iff_i1 = LLVMCore.LLVMBuildXor(generator.builder, xor_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "iff");
                        const nand_i1 = LLVMCore.LLVMBuildXor(generator.builder, and_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "nand");
                        const nor_i1 = LLVMCore.LLVMBuildXor(generator.builder, or_i1, LLVMCore.LLVMConstInt(i1_ty, 1, @intFromBool(false)), "nor");

                        const sel_i1 = switch (lop.op) {
                            .And => and_i1,
                            .Or => or_i1,
                            .Xor => xor_i1,
                            .Iff => iff_i1,
                            .Nand => nand_i1,
                            .Nor => nor_i1,
                            .Implies => implies_i1,
                            else => LLVMCore.LLVMConstInt(i1_ty, 0, @intFromBool(false)),
                        };
                        const sel_i2 = LLVMCore.LLVMBuildZExt(generator.builder, sel_i1, i2_ty, "i1.to.i2");
                        try stack.append(sel_i2);
                    },
                }
            },
            .Dup => {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                try stack.append(v);
            },
            .Pop => {
                if (stack.items.len > 0) {
                    stack.items.len -= 1;
                }
            },
            .Swap => {
                if (stack.items.len >= 2) {
                    const a = stack.items[stack.items.len - 1];
                    const b = stack.items[stack.items.len - 2];
                    stack.items[stack.items.len - 2] = a;
                    stack.items[stack.items.len - 1] = b;
                }
            },
            .Print => {
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    print.printValue(generator, v, &next_string_id);
                }
            },
            .PrintBegin => {},
            .PrintStr => |pstr| {
                if (pstr.const_id < hir.constant_pool.len) {
                    const hv = hir.constant_pool[pstr.const_id];
                    if (hv == .string) {
                        print.printLiteralString(generator, hv.string, &next_string_id);
                    }
                }
            },
            .PrintVal => {
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    print.printValue(generator, v, &next_string_id);
                }
            },
            .Peek => |pk| {
                if (!generator.debug_peek) {
                    if (stack.items.len > 0) {
                        const v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        try stack.append(v);
                    }
                    continue;
                }
                if (stack.items.len > 0) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;

                    var prefix_buf: [256]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&prefix_buf);
                    var w = fbs.writer();
                    if (pk.location) |loc| {
                        const file = loc.file;
                        const line = loc.range.start_line;
                        const col = loc.range.start_col;
                        try w.print("[{s}:{d}:{d}] ", .{ file, line, col });
                    }
                    if (pk.name) |nm| {
                        try w.print("{s} :: ", .{nm});
                    }

                    const kind = LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(v));
                    const type_tag = switch (kind) {
                        LLVMTypes.LLVMTypeKind.LLVMIntegerTypeKind => "int",
                        LLVMTypes.LLVMTypeKind.LLVMDoubleTypeKind, LLVMTypes.LLVMTypeKind.LLVMFloatTypeKind => "float",
                        LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind => "string",
                        else => "value",
                    };
                    try w.print("{s} is ", .{type_tag});
                    const prefix_len = fbs.pos;
                    const prefix_slice = prefix_buf[0..prefix_len];

                    print.printLiteralString(generator, prefix_slice, &next_string_id);
                    print.printValue(generator, v, &next_string_id);
                    print.printNewline(generator, &next_string_id);

                    try stack.append(v);
                }
            },
            .PrintNewline => {
                print.printNewline(generator, &next_string_id);
            },
            .Label => |lbl| {
                if (pre_entered_start and std.mem.eql(u8, lbl.name, func.start_label)) {
                    // Already positioned at start label; do not create a self-branch
                    pre_entered_start = false;
                } else {
                    try bm.enterLabel(lbl.name);
                }
            },
            .Jump => |j| {
                try bm.branchTo(j.label);
            },
            .JumpCond => |jc| {
                if (stack.items.len < 1) continue;
                const cond_v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                var cond_i1 = cast.ensureI1(generator, cond_v);
                // Use HIR type when available: tetra truth (1 and 2 => true)
                if (jc.condition_type == .Tetra) {
                    const i64_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                    const c64 = cast.ensureI64(generator, cond_v);
                    const eq1 = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, c64, LLVMCore.LLVMConstInt(i64_ty, 1, 0), "eq1");
                    const eq2 = LLVMCore.LLVMBuildICmp(generator.builder, LLVMTypes.LLVMIntPredicate.LLVMIntEQ, c64, LLVMCore.LLVMConstInt(i64_ty, 2, 0), "eq2");
                    cond_i1 = LLVMCore.LLVMBuildOr(generator.builder, eq1, eq2, "tetra.true");
                }
                try bm.branchCond(cond_i1, jc.label_true, jc.label_false);
            },
            .StoreVar => |sv| {
                if (stack.items.len < 1) continue;
                var v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const key = switch (sv.scope_kind) {
                    .Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{sv.var_name}),
                    .GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sv.var_name}),
                    .ModuleGlobal => blk: {
                        const mod = sv.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, sv.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{sv.var_name}),
                };
                const target_ty = type_map.mapHIRTypeToLLVM(generator, sv.expected_type);
                v = coerceToType(generator, v, target_ty);
                try var_store.store(key, v, target_ty, sv.scope_kind == HIR.ScopeKind.Local, sv.var_name);
            },
            .LoadVar => |lv| {
                const key = switch (lv.scope_kind) {
                    .Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{lv.var_name}),
                    .GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{lv.var_name}),
                    .ModuleGlobal => blk: {
                        const mod = lv.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, lv.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{lv.var_name}),
                };

                // Determine the correct type to load by checking the stored variable type
                const target_ty = if (var_store.map.get(key)) |entry| blk: {
                    // Check if it's an ArrayHeader pointer
                    const arrays_mod = @import("arrays.zig");
                    if (entry.elem_ty == arrays_mod.getArrayHeaderPtrType(generator)) {
                        break :blk arrays_mod.getArrayHeaderPtrType(generator);
                    }
                    // For other types, use the stored element type
                    break :blk entry.elem_ty;
                } else LLVMCore.LLVMInt64TypeInContext(generator.context);

                const loaded = try var_store.load(key, target_ty, lv.var_name);
                try stack.append(loaded);
            },
            .StoreConst => |sc| {
                if (stack.items.len < 1) continue;
                const v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const key = switch (sc.scope_kind) {
                    .GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sc.var_name}),
                    .ModuleGlobal => blk: {
                        const mod = sc.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, sc.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sc.var_name}),
                };
                try var_store.defineConstGlobal(key, v);
            },
            .StoreDecl => |sd| {
                if (stack.items.len < 1) continue;
                var v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const key = switch (sd.scope_kind) {
                    .Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{sd.var_name}),
                    .GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{sd.var_name}),
                    .ModuleGlobal => blk: {
                        const mod = sd.module_context orelse "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, sd.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{sd.var_name}),
                };
                const target_ty = type_map.mapHIRTypeToLLVM(generator, sd.declared_type);
                // If expecting a pointer (e.g., ArrayHeader*) but we accidentally have a non-pointer on top,
                // try to use the previous stack item if it is a pointer (common after ArrayNew patterns).
                if (LLVMCore.LLVMGetTypeKind(target_ty) == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                    const v_kind = LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(v));
                    if (v_kind != LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                        if (stack.items.len > 0) {
                            const alt = stack.items[stack.items.len - 1];
                            const alt_kind = LLVMCore.LLVMGetTypeKind(LLVMCore.LLVMTypeOf(alt));
                            if (alt_kind == LLVMTypes.LLVMTypeKind.LLVMPointerTypeKind) {
                                // Use previous pointer value for the declaration
                                v = alt;
                                stack.items.len -= 1; // consume the pointer we just used
                                std.debug.print("WARN: StoreDecl {s} used previous stack item pointer due to non-pointer on top\n", .{sd.var_name});
                            } else {
                                std.debug.print("ERROR: StoreDecl for {s} expected pointer, got {s}\n", .{ sd.var_name, @tagName(v_kind) });
                                continue;
                            }
                        } else {
                            std.debug.print("ERROR: StoreDecl for {s} expected pointer, stack empty after non-pointer top\n", .{sd.var_name});
                            continue;
                        }
                    }
                    if (LLVMCore.LLVMTypeOf(v) != target_ty) {
                        v = LLVMCore.LLVMBuildBitCast(generator.builder, v, target_ty, "decl.ptr.cast");
                    }
                }
                // For declarations, we don't coerce scalars; types should match or be handled above
                try var_store.store(key, v, target_ty, sd.scope_kind == HIR.ScopeKind.Local, sd.var_name);
            },
            .PushStorageId => |pstid| {
                const key = switch (pstid.scope_kind) {
                    .Local => try std.fmt.allocPrint(generator.allocator, "l:{s}", .{pstid.var_name}),
                    .GlobalLocal => try std.fmt.allocPrint(generator.allocator, "g:{s}", .{pstid.var_name}),
                    .ModuleGlobal => blk: {
                        const mod = "";
                        break :blk try std.fmt.allocPrint(generator.allocator, "m:{s}.{s}", .{ mod, pstid.var_name });
                    },
                    else => try std.fmt.allocPrint(generator.allocator, "u:{s}", .{pstid.var_name}),
                };
                const default_ty = LLVMCore.LLVMInt64TypeInContext(generator.context);
                const entry_ptr = try var_store.getOrCreatePtr(key, default_ty, pstid.scope_kind == HIR.ScopeKind.Local, pstid.var_name);
                try stack.append(entry_ptr.ptr);
            },
            .StoreParamAlias => |spa| {
                if (stack.items.len < 1) continue;
                const raw_ptr = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const elem = type_map.mapHIRTypeToLLVM(generator, spa.param_type);
                const want_ptr_ty = LLVMCore.LLVMPointerType(elem, 0);
                const cast_ptr = if (LLVMCore.LLVMTypeOf(raw_ptr) == want_ptr_ty) raw_ptr else LLVMCore.LLVMBuildBitCast(generator.builder, raw_ptr, want_ptr_ty, "alias.ptr.cast");
                try alias_slots.put(spa.var_index, .{ .ptr = cast_ptr, .elem_ty = elem });
            },
            .LoadAlias => |la| {
                const entry = alias_slots.get(la.slot_index) orelse continue;
                const loaded = LLVMCore.LLVMBuildLoad2(generator.builder, entry.elem_ty, entry.ptr, "alias.load");
                try stack.append(loaded);
            },
            .StoreAlias => |sa| {
                if (stack.items.len < 1) continue;
                var v = stack.items[stack.items.len - 1];
                stack.items.len -= 1;
                const entry = alias_slots.get(sa.slot_index) orelse continue;
                v = coerceToType(generator, v, entry.elem_ty);
                _ = LLVMCore.LLVMBuildStore(generator.builder, v, entry.ptr);
            },
            .Call => |c| {
                const f_entry = function_map.get(c.qualified_name) orelse null;
                if (f_entry == null) continue;
                const callee = f_entry.?;
                const argc: usize = @intCast(c.arg_count);
                if (stack.items.len < argc) continue;
                var args = try generator.allocator.alloc(LLVMTypes.LLVMValueRef, argc);
                defer generator.allocator.free(args);
                var ai: usize = 0;
                while (ai < argc) : (ai += 1) {
                    const v2 = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    args[argc - 1 - ai] = v2;
                }
                var callee_info: ?HIR.HIRProgram.HIRFunction = null;
                for (hir.function_table) |finfo| {
                    if (std.mem.eql(u8, finfo.qualified_name, c.qualified_name)) {
                        callee_info = finfo;
                        break;
                    }
                }
                if (callee_info) |fi| {
                    var idx: usize = 0;
                    while (idx < args.len and idx < fi.param_types.len) : (idx += 1) {
                        const pt = fi.param_types[idx];
                        if (fi.param_is_alias[idx]) {
                            const elem = type_map.mapHIRTypeToLLVM(generator, pt);
                            const want_ptr_ty = LLVMCore.LLVMPointerType(elem, 0);
                            if (LLVMCore.LLVMTypeOf(args[idx]) != want_ptr_ty) args[idx] = LLVMCore.LLVMBuildBitCast(generator.builder, args[idx], want_ptr_ty, "arg.ptr.cast");
                        } else {
                            const tgt = type_map.mapHIRTypeToLLVM(generator, pt);
                            args[idx] = coerceToType(generator, args[idx], tgt);
                        }
                    }
                }
                const fnty_ptr = LLVMCore.LLVMTypeOf(callee);
                const fnty = LLVMCore.LLVMGetElementType(fnty_ptr);
                const res = LLVMCore.LLVMBuildCall2(generator.builder, fnty, callee, args.ptr, @intCast(argc), "");
                if (c.return_type != .Nothing) {
                    try stack.append(res);
                }
            },
            .TailCall => |tc| {
                const f_entry = function_map.get(tc.qualified_name) orelse null;
                if (f_entry == null) continue;
                const callee = f_entry.?;
                const argc: usize = @intCast(tc.arg_count);
                if (stack.items.len < argc) continue;
                var args = try generator.allocator.alloc(LLVMTypes.LLVMValueRef, argc);
                defer generator.allocator.free(args);
                var ai: usize = 0;
                while (ai < argc) : (ai += 1) {
                    const v = stack.items[stack.items.len - 1];
                    stack.items.len -= 1;
                    args[argc - 1 - ai] = v;
                }
                const fnty_ptr = LLVMCore.LLVMTypeOf(callee);
                const fnty = LLVMCore.LLVMGetElementType(fnty_ptr);
                const res = LLVMCore.LLVMBuildCall2(generator.builder, fnty, callee, args.ptr, @intCast(argc), "");
                const rt = type_map.mapHIRTypeToLLVM(generator, func.return_type);
                if (LLVMCore.LLVMGetTypeKind(rt) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind) {
                    _ = LLVMCore.LLVMBuildRetVoid(generator.builder);
                } else {
                    const coerced = coerceToType(generator, res, rt);
                    _ = LLVMCore.LLVMBuildRet(generator.builder, coerced);
                }
                in_dead_code = true;
            },
            .Return => |r| {
                const rt = type_map.mapHIRTypeToLLVM(generator, func.return_type);
                const is_void = LLVMCore.LLVMGetTypeKind(rt) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind;
                if (!is_void) {
                    var v: LLVMTypes.LLVMValueRef = LLVMCore.LLVMConstNull(rt);
                    if (r.has_value and stack.items.len > 0) {
                        v = stack.items[stack.items.len - 1];
                        stack.items.len -= 1;
                        v = coerceToType(generator, v, rt);
                    }
                    _ = LLVMCore.LLVMBuildRet(generator.builder, v);
                } else {
                    _ = LLVMCore.LLVMBuildRetVoid(generator.builder);
                }
                in_dead_code = true;
            },
            .EnterScope, .ExitScope => {},
            else => {},
        }
    }

    const insert_block = LLVMCore.LLVMGetInsertBlock(generator.builder);
    const rt = type_map.mapHIRTypeToLLVM(generator, func.return_type);
    if (insert_block != null and LLVMCore.LLVMGetBasicBlockTerminator(insert_block) == null) {
        if (LLVMCore.LLVMGetTypeKind(rt) == LLVMTypes.LLVMTypeKind.LLVMVoidTypeKind) {
            _ = LLVMCore.LLVMBuildRetVoid(generator.builder);
        } else {
            _ = LLVMCore.LLVMBuildRet(generator.builder, LLVMCore.LLVMConstNull(rt));
        }
    }
}
