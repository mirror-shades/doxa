declare void @doxa_write_cstr(ptr, i64)
declare void @doxa_write_raw(ptr)
declare void @doxa_write_stderr(ptr, i64)
declare void @doxa_exit(i64) noreturn
declare void @doxa_panic(ptr, i64)
declare void @doxa_print_i64(i64)
declare void @doxa_print_u64(i64)
declare void @doxa_print_f64(double)
declare void @doxa_print_byte(i64)
declare i64 @doxa_str_len(ptr, i64)
declare void @doxa_str_concat(ptr, i64, ptr, i64, ptr, ptr)
declare void @doxa_str_clone_at(i64, ptr, i64, ptr, ptr)
declare void @doxa_str_from_cstr(ptr, ptr, ptr)
declare ptr @doxa_str_clone_raw(ptr, i64)
declare void @doxa_substring(ptr, i64, i64, i64, ptr, ptr)
declare i8 @doxa_str_pop(ptr, i64, ptr, ptr)
declare void @doxa_str_insert(ptr, i64, i64, ptr, i64, ptr, ptr)
declare i8 @doxa_str_remove(ptr, i64, i64, ptr, ptr)
declare void @doxa_char_to_string(i8, ptr, ptr)
declare i64 @doxa_int_from_string(ptr, i64)
declare double @doxa_float_from_string(ptr, i64)
declare i64 @doxa_byte_from_string(ptr, i64)
declare i64 @doxa_byte_from_f64(double)
declare void @doxa_int_to_string(i64, ptr, ptr)
declare void @doxa_float_to_string(double, ptr, ptr)
declare void @doxa_byte_to_string(i64, ptr, ptr)
declare void @doxa_tetra_to_string(i64, ptr, ptr)
declare void @doxa_nothing_to_string(ptr, ptr)
declare void @doxa_enum_to_string(ptr, i64, i64, ptr, ptr)
declare void @doxa_struct_to_string(ptr, ptr, ptr)
declare void @doxa_array_to_string(ptr, ptr, ptr)
declare void @doxa_pack_bytes(ptr, ptr, ptr)
declare ptr @doxa_unpack_bytes(ptr, i64)
declare void @doxa_debug_peek(ptr)
declare void @doxa_peek_string(ptr, i64)
declare void @doxa_print_array_hdr(ptr)
declare i1 @doxa_str_eq(ptr, i64, ptr, i64)
declare ptr @doxa_array_new(i64, i64, i64)
declare ptr @doxa_array_new_nested(i64, i64, i64, ptr, i64, i64, i64)
declare ptr @doxa_array_clone(ptr)
declare ptr @doxa_array_clone_at(i64, ptr)
declare i64 @doxa_array_len(ptr)
declare i64 @doxa_array_get_i64(ptr, i64)
declare void @doxa_array_get_str(ptr, i64, ptr, ptr)
declare void @doxa_array_set_i64(ptr, i64, i64)
declare void @doxa_array_set_str(ptr, i64, ptr, i64)
declare ptr @doxa_array_concat(ptr, ptr, i64, i64)
declare ptr @doxa_array_insert(ptr, i64, i64)
declare ptr @doxa_array_insert_str(ptr, i64, ptr, i64)
declare ptr @doxa_array_remove(ptr, i64, ptr)
declare ptr @doxa_array_remove_str(ptr, i64, ptr, ptr)
declare ptr @doxa_array_slice(ptr, i64, i64)
declare ptr @doxa_map_new(i64, i64, i64)
declare void @doxa_map_set_i64(ptr, i64, i64)
declare void @doxa_map_set_else_i64(ptr, i64)
declare i64 @doxa_map_get_i64(ptr, i64)
declare i8 @doxa_map_try_get_i64(ptr, i64, ptr)
declare double @llvm.pow.f64(double, double)
declare void @doxa_set_args(i32, ptr)
declare i64 @doxa_int(double)
declare i64 @doxa_type_check(i64, i64, ptr)
declare void @doxa_print_value(ptr)
declare void @doxa_clone_doxa_value_at(i64, ptr)
declare i64 @doxa_find_array(ptr, i64)
declare i64 @doxa_find_array_str(ptr, ptr, i64)
declare i64 @doxa_find_str(ptr, i64, ptr, i64)
declare void @doxa_struct_register(ptr, ptr)
declare ptr @doxa_struct_clone_at(i64, ptr)
declare void @doxa_enum_register(ptr)
declare ptr @doxa_scope_alloc(i64, i64)
declare void @doxa_scope_enter()
declare void @doxa_scope_exit()
declare i8 @doxa_exists_quantifier_gt(ptr, ptr, i64)
declare i8 @doxa_exists_quantifier_eq(ptr, ptr, i64)
declare i8 @doxa_forall_quantifier_gt(ptr, ptr, i64)
declare i8 @doxa_forall_quantifier_eq(ptr, ptr, i64)
declare void @doxa_clear(ptr)
declare ptr @doxa_array_range(i64, i64)
declare void @doxa_trap_unreachable()
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)
@.str.2 = private constant [3 x i8] c"hi "
@.str.4 = private constant [3 x i8] c"age"
@.str.5 = private constant [5 x i8] c"Alice"
@.str.6 = private constant [4 x i8] c"name"
@.str.7 = private constant [1 x i8] c"\0A"
@.str.8 = private constant [1 x i8] c" "
@.str.10 = private constant [3 x i8] c"Bob"
@.str.11 = private constant [1 x i8] c"b"
@.str.12 = private constant [1 x i8] c"a"
%DoxaPeekInfo = type { ptr, ptr, ptr, ptr, i32, i32, i32, i32, i32 }
%DoxaValue = type { i32, i32, i64, i64 }
%DoxaString = type { ptr, i64 }
%ArrayHeader = type { ptr, i64, i64, i64, i64, ptr }

@.doxa.nl = private constant [2 x i8] c"\0A\00"
@.doxa.empty = private constant [1 x i8] c"\00"
@.doxa.arr_open = private constant [2 x i8] c"[\00"
@.doxa.arr_close = private constant [2 x i8] c"]\00"
@.doxa.arr_sep = private constant [3 x i8] c", \00"
@tetra_not_lut = private constant [4 x i8] [i8 1, i8 0, i8 2, i8 3]
@tetra_and_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 0, i8 0, i8 0, i8 0],
  [4 x i8] [i8 0, i8 1, i8 2, i8 3],
  [4 x i8] [i8 0, i8 2, i8 2, i8 0],
  [4 x i8] [i8 0, i8 3, i8 0, i8 3]
]
@tetra_or_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 0, i8 1, i8 2, i8 3],
  [4 x i8] [i8 1, i8 1, i8 1, i8 1],
  [4 x i8] [i8 2, i8 1, i8 2, i8 2],
  [4 x i8] [i8 3, i8 1, i8 2, i8 3]
]
@tetra_iff_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 1, i8 0, i8 3, i8 2],
  [4 x i8] [i8 0, i8 1, i8 2, i8 3],
  [4 x i8] [i8 3, i8 2, i8 2, i8 3],
  [4 x i8] [i8 2, i8 3, i8 3, i8 2]
]
@tetra_xor_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 0, i8 1, i8 2, i8 3],
  [4 x i8] [i8 1, i8 0, i8 3, i8 2],
  [4 x i8] [i8 2, i8 3, i8 2, i8 3],
  [4 x i8] [i8 3, i8 2, i8 3, i8 2]
]
@tetra_nand_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 1, i8 1, i8 1, i8 1],
  [4 x i8] [i8 1, i8 0, i8 3, i8 2],
  [4 x i8] [i8 1, i8 3, i8 3, i8 1],
  [4 x i8] [i8 1, i8 2, i8 1, i8 2]
]
@tetra_nor_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 1, i8 0, i8 3, i8 2],
  [4 x i8] [i8 0, i8 0, i8 0, i8 0],
  [4 x i8] [i8 3, i8 0, i8 3, i8 3],
  [4 x i8] [i8 2, i8 0, i8 3, i8 2]
]
@tetra_implies_lut = private constant [4 x [4 x i8]] [
  [4 x i8] [i8 1, i8 1, i8 1, i8 1],
  [4 x i8] [i8 0, i8 1, i8 2, i8 3],
  [4 x i8] [i8 2, i8 1, i8 2, i8 2],
  [4 x i8] [i8 3, i8 1, i8 2, i8 3]
]

define i2 @exists_quantifier_gt(ptr %hdr, ptr %value_ptr, i64 %value_len) {
entry:
  %res = call i8 @doxa_exists_quantifier_gt(ptr %hdr, ptr %value_ptr, i64 %value_len)
  %cast = trunc i8 %res to i2
  ret i2 %cast
}

define i2 @exists_quantifier_eq(ptr %hdr, ptr %value_ptr, i64 %value_len) {
entry:
  %res = call i8 @doxa_exists_quantifier_eq(ptr %hdr, ptr %value_ptr, i64 %value_len)
  %cast = trunc i8 %res to i2
  ret i2 %cast
}

define i2 @forall_quantifier_gt(ptr %hdr, ptr %value_ptr, i64 %value_len) {
entry:
  %res = call i8 @doxa_forall_quantifier_gt(ptr %hdr, ptr %value_ptr, i64 %value_len)
  %cast = trunc i8 %res to i2
  ret i2 %cast
}

define i2 @forall_quantifier_eq(ptr %hdr, ptr %value_ptr, i64 %value_len) {
entry:
  %res = call i8 @doxa_forall_quantifier_eq(ptr %hdr, ptr %value_ptr, i64 %value_len)
  %cast = trunc i8 %res to i2
  ret i2 %cast
}

define void @doxa_program_main() {
entry:
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  call void @doxa_user_main()
  ret void
}
define %DoxaString @Person.greet(ptr %0) {
entry:
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %1 = getelementptr inbounds [3 x i8], ptr @.str.2, i64 0, i64 0
  %2 = insertvalue %DoxaString undef, ptr %1, 0
  %3 = insertvalue %DoxaString %2, i64 3, 1
  %4 = load ptr, ptr %0
  %5 = getelementptr inbounds { i64, i64, i64 }, ptr %4, i32 0, i32 0
  %6 = load i64, ptr %5
  %7 = getelementptr inbounds { i64, i64, i64 }, ptr %4, i32 0, i32 1
  %8 = load i64, ptr %7
  %9 = inttoptr i64 %6 to ptr
  %10 = insertvalue %DoxaString undef, ptr %9, 0
  %11 = insertvalue %DoxaString %10, i64 %8, 1
  %12 = extractvalue %DoxaString %3, 0
  %13 = extractvalue %DoxaString %3, 1
  %14 = extractvalue %DoxaString %11, 0
  %15 = extractvalue %DoxaString %11, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %12, i64 %13, ptr %14, i64 %15, ptr %str_out_ptr, ptr %str_out_len)
  %16 = load ptr, ptr %str_out_ptr
  %17 = load i64, ptr %str_out_len
  %18 = insertvalue %DoxaString undef, ptr %16, 0
  %19 = insertvalue %DoxaString %18, i64 %17, 1
  %20 = extractvalue %DoxaString %19, 0
  %21 = extractvalue %DoxaString %19, 1
  %22 = alloca ptr
  %23 = alloca i64
  store ptr null, ptr %22
  store i64 0, ptr %23
  call void @doxa_str_clone_at(i64 1, ptr %20, i64 %21, ptr %22, ptr %23)
  %24 = load ptr, ptr %22
  %25 = load i64, ptr %23
  %26 = insertvalue %DoxaString undef, ptr %24, 0
  %27 = insertvalue %DoxaString %26, i64 %25, 1
  call void @doxa_scope_exit()
  ret %DoxaString %27
}

define void @doxa_user_main() {
entry:
  %var.c = alloca ptr
  %var.p = alloca ptr
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %0 = add i64 0, 30
  %1 = getelementptr inbounds [3 x i8], ptr @.str.4, i64 0, i64 0
  %2 = insertvalue %DoxaString undef, ptr %1, 0
  %3 = insertvalue %DoxaString %2, i64 3, 1
  %4 = getelementptr inbounds [5 x i8], ptr @.str.5, i64 0, i64 0
  %5 = insertvalue %DoxaString undef, ptr %4, 0
  %6 = insertvalue %DoxaString %5, i64 5, 1
  %7 = getelementptr inbounds [4 x i8], ptr @.str.6, i64 0, i64 0
  %8 = insertvalue %DoxaString undef, ptr %7, 0
  %9 = insertvalue %DoxaString %8, i64 4, 1
  %10 = add i64 0, 24
  %11 = call ptr @doxa_scope_alloc(i64 %10, i64 8)
  %12 = bitcast ptr %11 to ptr
  %13 = extractvalue %DoxaString %6, 0
  %14 = extractvalue %DoxaString %6, 1
  %15 = alloca ptr
  %16 = alloca i64
  store ptr null, ptr %15
  store i64 0, ptr %16
  call void @doxa_str_clone_at(i64 0, ptr %13, i64 %14, ptr %15, ptr %16)
  %17 = load ptr, ptr %15
  %18 = load i64, ptr %16
  %19 = ptrtoint ptr %17 to i64
  %20 = getelementptr inbounds { i64, i64, i64 }, ptr %12, i32 0, i32 0
  store i64 %19, ptr %20
  %21 = getelementptr inbounds { i64, i64, i64 }, ptr %12, i32 0, i32 1
  store i64 %18, ptr %21
  %22 = getelementptr inbounds { i64, i64, i64 }, ptr %12, i32 0, i32 2
  store i64 %0, ptr %22
  call void @doxa_struct_register(ptr %12, ptr @.doxa.struct.desc.5)
  store ptr %12, ptr %var.p
  %23 = call %DoxaString @Person.greet(ptr %var.p)
  %24 = getelementptr inbounds [1 x i8], ptr @.str.7, i64 0, i64 0
  %25 = insertvalue %DoxaString undef, ptr %24, 0
  %26 = insertvalue %DoxaString %25, i64 1, 1
  %27 = extractvalue %DoxaString %23, 0
  %28 = extractvalue %DoxaString %23, 1
  %29 = extractvalue %DoxaString %26, 0
  %30 = extractvalue %DoxaString %26, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %27, i64 %28, ptr %29, i64 %30, ptr %str_out_ptr, ptr %str_out_len)
  %31 = load ptr, ptr %str_out_ptr
  %32 = load i64, ptr %str_out_len
  %33 = insertvalue %DoxaString undef, ptr %31, 0
  %34 = insertvalue %DoxaString %33, i64 %32, 1
  %35 = extractvalue %DoxaString %34, 0
  %36 = extractvalue %DoxaString %34, 1
  call void @doxa_write_cstr(ptr %35, i64 %36)
  %37 = load ptr, ptr %var.p
  %38 = getelementptr inbounds { i64, i64, i64 }, ptr %37, i32 0, i32 0
  %39 = load i64, ptr %38
  %40 = getelementptr inbounds { i64, i64, i64 }, ptr %37, i32 0, i32 1
  %41 = load i64, ptr %40
  %42 = inttoptr i64 %39 to ptr
  %43 = insertvalue %DoxaString undef, ptr %42, 0
  %44 = insertvalue %DoxaString %43, i64 %41, 1
  %45 = getelementptr inbounds [1 x i8], ptr @.str.8, i64 0, i64 0
  %46 = insertvalue %DoxaString undef, ptr %45, 0
  %47 = insertvalue %DoxaString %46, i64 1, 1
  %48 = extractvalue %DoxaString %44, 0
  %49 = extractvalue %DoxaString %44, 1
  %50 = extractvalue %DoxaString %47, 0
  %51 = extractvalue %DoxaString %47, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %48, i64 %49, ptr %50, i64 %51, ptr %str_out_ptr, ptr %str_out_len)
  %52 = load ptr, ptr %str_out_ptr
  %53 = load i64, ptr %str_out_len
  %54 = insertvalue %DoxaString undef, ptr %52, 0
  %55 = insertvalue %DoxaString %54, i64 %53, 1
  %56 = load ptr, ptr %var.p
  %57 = getelementptr inbounds { i64, i64, i64 }, ptr %56, i32 0, i32 2
  %58 = load i64, ptr %57
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_int_to_string(i64 %58, ptr %str_out_ptr, ptr %str_out_len)
  %59 = load ptr, ptr %str_out_ptr
  %60 = load i64, ptr %str_out_len
  %61 = insertvalue %DoxaString undef, ptr %59, 0
  %62 = insertvalue %DoxaString %61, i64 %60, 1
  %63 = extractvalue %DoxaString %55, 0
  %64 = extractvalue %DoxaString %55, 1
  %65 = extractvalue %DoxaString %62, 0
  %66 = extractvalue %DoxaString %62, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %63, i64 %64, ptr %65, i64 %66, ptr %str_out_ptr, ptr %str_out_len)
  %67 = load ptr, ptr %str_out_ptr
  %68 = load i64, ptr %str_out_len
  %69 = insertvalue %DoxaString undef, ptr %67, 0
  %70 = insertvalue %DoxaString %69, i64 %68, 1
  %71 = getelementptr inbounds [1 x i8], ptr @.str.7, i64 0, i64 0
  %72 = insertvalue %DoxaString undef, ptr %71, 0
  %73 = insertvalue %DoxaString %72, i64 1, 1
  %74 = extractvalue %DoxaString %70, 0
  %75 = extractvalue %DoxaString %70, 1
  %76 = extractvalue %DoxaString %73, 0
  %77 = extractvalue %DoxaString %73, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %74, i64 %75, ptr %76, i64 %77, ptr %str_out_ptr, ptr %str_out_len)
  %78 = load ptr, ptr %str_out_ptr
  %79 = load i64, ptr %str_out_len
  %80 = insertvalue %DoxaString undef, ptr %78, 0
  %81 = insertvalue %DoxaString %80, i64 %79, 1
  %82 = extractvalue %DoxaString %81, 0
  %83 = extractvalue %DoxaString %81, 1
  call void @doxa_write_cstr(ptr %82, i64 %83)
  %84 = add i64 0, 25
  %85 = getelementptr inbounds [3 x i8], ptr @.str.4, i64 0, i64 0
  %86 = insertvalue %DoxaString undef, ptr %85, 0
  %87 = insertvalue %DoxaString %86, i64 3, 1
  %88 = getelementptr inbounds [3 x i8], ptr @.str.10, i64 0, i64 0
  %89 = insertvalue %DoxaString undef, ptr %88, 0
  %90 = insertvalue %DoxaString %89, i64 3, 1
  %91 = getelementptr inbounds [4 x i8], ptr @.str.6, i64 0, i64 0
  %92 = insertvalue %DoxaString undef, ptr %91, 0
  %93 = insertvalue %DoxaString %92, i64 4, 1
  %94 = add i64 0, 24
  %95 = call ptr @doxa_scope_alloc(i64 %94, i64 8)
  %96 = bitcast ptr %95 to ptr
  %97 = extractvalue %DoxaString %90, 0
  %98 = extractvalue %DoxaString %90, 1
  %99 = alloca ptr
  %100 = alloca i64
  store ptr null, ptr %99
  store i64 0, ptr %100
  call void @doxa_str_clone_at(i64 0, ptr %97, i64 %98, ptr %99, ptr %100)
  %101 = load ptr, ptr %99
  %102 = load i64, ptr %100
  %103 = ptrtoint ptr %101 to i64
  %104 = getelementptr inbounds { i64, i64, i64 }, ptr %96, i32 0, i32 0
  store i64 %103, ptr %104
  %105 = getelementptr inbounds { i64, i64, i64 }, ptr %96, i32 0, i32 1
  store i64 %102, ptr %105
  %106 = getelementptr inbounds { i64, i64, i64 }, ptr %96, i32 0, i32 2
  store i64 %84, ptr %106
  call void @doxa_struct_register(ptr %96, ptr @.doxa.struct.desc.5)
  %107 = getelementptr inbounds [1 x i8], ptr @.str.11, i64 0, i64 0
  %108 = insertvalue %DoxaString undef, ptr %107, 0
  %109 = insertvalue %DoxaString %108, i64 1, 1
  %110 = add i64 0, 30
  %111 = getelementptr inbounds [3 x i8], ptr @.str.4, i64 0, i64 0
  %112 = insertvalue %DoxaString undef, ptr %111, 0
  %113 = insertvalue %DoxaString %112, i64 3, 1
  %114 = getelementptr inbounds [5 x i8], ptr @.str.5, i64 0, i64 0
  %115 = insertvalue %DoxaString undef, ptr %114, 0
  %116 = insertvalue %DoxaString %115, i64 5, 1
  %117 = getelementptr inbounds [4 x i8], ptr @.str.6, i64 0, i64 0
  %118 = insertvalue %DoxaString undef, ptr %117, 0
  %119 = insertvalue %DoxaString %118, i64 4, 1
  %120 = add i64 0, 24
  %121 = call ptr @doxa_scope_alloc(i64 %120, i64 8)
  %122 = bitcast ptr %121 to ptr
  %123 = extractvalue %DoxaString %116, 0
  %124 = extractvalue %DoxaString %116, 1
  %125 = alloca ptr
  %126 = alloca i64
  store ptr null, ptr %125
  store i64 0, ptr %126
  call void @doxa_str_clone_at(i64 0, ptr %123, i64 %124, ptr %125, ptr %126)
  %127 = load ptr, ptr %125
  %128 = load i64, ptr %126
  %129 = ptrtoint ptr %127 to i64
  %130 = getelementptr inbounds { i64, i64, i64 }, ptr %122, i32 0, i32 0
  store i64 %129, ptr %130
  %131 = getelementptr inbounds { i64, i64, i64 }, ptr %122, i32 0, i32 1
  store i64 %128, ptr %131
  %132 = getelementptr inbounds { i64, i64, i64 }, ptr %122, i32 0, i32 2
  store i64 %110, ptr %132
  call void @doxa_struct_register(ptr %122, ptr @.doxa.struct.desc.5)
  %133 = getelementptr inbounds [1 x i8], ptr @.str.12, i64 0, i64 0
  %134 = insertvalue %DoxaString undef, ptr %133, 0
  %135 = insertvalue %DoxaString %134, i64 1, 1
  %136 = add i64 0, 16
  %137 = call ptr @doxa_scope_alloc(i64 %136, i64 8)
  %138 = bitcast ptr %137 to ptr
  %139 = ptrtoint ptr %122 to i64
  %140 = getelementptr inbounds { i64, i64 }, ptr %138, i32 0, i32 0
  store i64 %139, ptr %140
  %141 = ptrtoint ptr %96 to i64
  %142 = getelementptr inbounds { i64, i64 }, ptr %138, i32 0, i32 1
  store i64 %141, ptr %142
  call void @doxa_struct_register(ptr %138, ptr @.doxa.struct.desc.15)
  store ptr %138, ptr %var.c
  %143 = load ptr, ptr %var.c
  %144 = getelementptr inbounds { i64, i64 }, ptr %143, i32 0, i32 0
  %145 = load i64, ptr %144
  %146 = inttoptr i64 %145 to ptr
  %147 = getelementptr inbounds { i64, i64, i64 }, ptr %146, i32 0, i32 0
  %148 = load i64, ptr %147
  %149 = getelementptr inbounds { i64, i64, i64 }, ptr %146, i32 0, i32 1
  %150 = load i64, ptr %149
  %151 = inttoptr i64 %148 to ptr
  %152 = insertvalue %DoxaString undef, ptr %151, 0
  %153 = insertvalue %DoxaString %152, i64 %150, 1
  %154 = getelementptr inbounds [1 x i8], ptr @.str.8, i64 0, i64 0
  %155 = insertvalue %DoxaString undef, ptr %154, 0
  %156 = insertvalue %DoxaString %155, i64 1, 1
  %157 = extractvalue %DoxaString %153, 0
  %158 = extractvalue %DoxaString %153, 1
  %159 = extractvalue %DoxaString %156, 0
  %160 = extractvalue %DoxaString %156, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %157, i64 %158, ptr %159, i64 %160, ptr %str_out_ptr, ptr %str_out_len)
  %161 = load ptr, ptr %str_out_ptr
  %162 = load i64, ptr %str_out_len
  %163 = insertvalue %DoxaString undef, ptr %161, 0
  %164 = insertvalue %DoxaString %163, i64 %162, 1
  %165 = load ptr, ptr %var.c
  %166 = getelementptr inbounds { i64, i64 }, ptr %165, i32 0, i32 0
  %167 = load i64, ptr %166
  %168 = inttoptr i64 %167 to ptr
  %169 = getelementptr inbounds { i64, i64, i64 }, ptr %168, i32 0, i32 2
  %170 = load i64, ptr %169
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_int_to_string(i64 %170, ptr %str_out_ptr, ptr %str_out_len)
  %171 = load ptr, ptr %str_out_ptr
  %172 = load i64, ptr %str_out_len
  %173 = insertvalue %DoxaString undef, ptr %171, 0
  %174 = insertvalue %DoxaString %173, i64 %172, 1
  %175 = extractvalue %DoxaString %164, 0
  %176 = extractvalue %DoxaString %164, 1
  %177 = extractvalue %DoxaString %174, 0
  %178 = extractvalue %DoxaString %174, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %175, i64 %176, ptr %177, i64 %178, ptr %str_out_ptr, ptr %str_out_len)
  %179 = load ptr, ptr %str_out_ptr
  %180 = load i64, ptr %str_out_len
  %181 = insertvalue %DoxaString undef, ptr %179, 0
  %182 = insertvalue %DoxaString %181, i64 %180, 1
  %183 = getelementptr inbounds [1 x i8], ptr @.str.7, i64 0, i64 0
  %184 = insertvalue %DoxaString undef, ptr %183, 0
  %185 = insertvalue %DoxaString %184, i64 1, 1
  %186 = extractvalue %DoxaString %182, 0
  %187 = extractvalue %DoxaString %182, 1
  %188 = extractvalue %DoxaString %185, 0
  %189 = extractvalue %DoxaString %185, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %186, i64 %187, ptr %188, i64 %189, ptr %str_out_ptr, ptr %str_out_len)
  %190 = load ptr, ptr %str_out_ptr
  %191 = load i64, ptr %str_out_len
  %192 = insertvalue %DoxaString undef, ptr %190, 0
  %193 = insertvalue %DoxaString %192, i64 %191, 1
  %194 = extractvalue %DoxaString %193, 0
  %195 = extractvalue %DoxaString %193, 1
  call void @doxa_write_cstr(ptr %194, i64 %195)
  %196 = load ptr, ptr %var.c
  %197 = getelementptr inbounds { i64, i64 }, ptr %196, i32 0, i32 1
  %198 = load i64, ptr %197
  %199 = inttoptr i64 %198 to ptr
  %200 = getelementptr inbounds { i64, i64, i64 }, ptr %199, i32 0, i32 0
  %201 = load i64, ptr %200
  %202 = getelementptr inbounds { i64, i64, i64 }, ptr %199, i32 0, i32 1
  %203 = load i64, ptr %202
  %204 = inttoptr i64 %201 to ptr
  %205 = insertvalue %DoxaString undef, ptr %204, 0
  %206 = insertvalue %DoxaString %205, i64 %203, 1
  %207 = getelementptr inbounds [1 x i8], ptr @.str.8, i64 0, i64 0
  %208 = insertvalue %DoxaString undef, ptr %207, 0
  %209 = insertvalue %DoxaString %208, i64 1, 1
  %210 = extractvalue %DoxaString %206, 0
  %211 = extractvalue %DoxaString %206, 1
  %212 = extractvalue %DoxaString %209, 0
  %213 = extractvalue %DoxaString %209, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %210, i64 %211, ptr %212, i64 %213, ptr %str_out_ptr, ptr %str_out_len)
  %214 = load ptr, ptr %str_out_ptr
  %215 = load i64, ptr %str_out_len
  %216 = insertvalue %DoxaString undef, ptr %214, 0
  %217 = insertvalue %DoxaString %216, i64 %215, 1
  %218 = load ptr, ptr %var.c
  %219 = getelementptr inbounds { i64, i64 }, ptr %218, i32 0, i32 1
  %220 = load i64, ptr %219
  %221 = inttoptr i64 %220 to ptr
  %222 = getelementptr inbounds { i64, i64, i64 }, ptr %221, i32 0, i32 2
  %223 = load i64, ptr %222
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_int_to_string(i64 %223, ptr %str_out_ptr, ptr %str_out_len)
  %224 = load ptr, ptr %str_out_ptr
  %225 = load i64, ptr %str_out_len
  %226 = insertvalue %DoxaString undef, ptr %224, 0
  %227 = insertvalue %DoxaString %226, i64 %225, 1
  %228 = extractvalue %DoxaString %217, 0
  %229 = extractvalue %DoxaString %217, 1
  %230 = extractvalue %DoxaString %227, 0
  %231 = extractvalue %DoxaString %227, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %228, i64 %229, ptr %230, i64 %231, ptr %str_out_ptr, ptr %str_out_len)
  %232 = load ptr, ptr %str_out_ptr
  %233 = load i64, ptr %str_out_len
  %234 = insertvalue %DoxaString undef, ptr %232, 0
  %235 = insertvalue %DoxaString %234, i64 %233, 1
  %236 = getelementptr inbounds [1 x i8], ptr @.str.7, i64 0, i64 0
  %237 = insertvalue %DoxaString undef, ptr %236, 0
  %238 = insertvalue %DoxaString %237, i64 1, 1
  %239 = extractvalue %DoxaString %235, 0
  %240 = extractvalue %DoxaString %235, 1
  %241 = extractvalue %DoxaString %238, 0
  %242 = extractvalue %DoxaString %238, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %239, i64 %240, ptr %241, i64 %242, ptr %str_out_ptr, ptr %str_out_len)
  %243 = load ptr, ptr %str_out_ptr
  %244 = load i64, ptr %str_out_len
  %245 = insertvalue %DoxaString undef, ptr %243, 0
  %246 = insertvalue %DoxaString %245, i64 %244, 1
  %247 = extractvalue %DoxaString %246, 0
  %248 = extractvalue %DoxaString %246, 1
  call void @doxa_write_cstr(ptr %247, i64 %248)
  call void @doxa_scope_exit()
  ret void
}


@.peek.str.0 = private unnamed_addr constant [7 x i8] c"Person\00"
@.peek.str.len.1 = constant i64 6
@.peek.str.6 = private unnamed_addr constant [5 x i8] c"name\00"
@.peek.str.len.7 = constant i64 4
@.peek.str.8 = private unnamed_addr constant [4 x i8] c"age\00"
@.peek.str.len.9 = constant i64 3
@.doxa.struct.names.2 = private constant [2 x ptr] [ptr getelementptr inbounds ([4 x i8], ptr @.peek.str.6, i64 0, i64 0), ptr getelementptr inbounds ([3 x i8], ptr @.peek.str.8, i64 0, i64 0)]
@.doxa.struct.tags.3 = private constant [2 x i64] [i64 3, i64 0]
@.doxa.struct.enumtys.4 = private constant [2 x ptr] [ptr null, ptr null]
@.doxa.struct.desc.5 = private constant { ptr, i64, ptr, ptr, ptr } { ptr getelementptr inbounds ([6 x i8], ptr @.peek.str.0, i64 0, i64 0), i64 2, ptr getelementptr inbounds ([2 x ptr], ptr @.doxa.struct.names.2, i64 0, i64 0), ptr getelementptr inbounds ([2 x i64], ptr @.doxa.struct.tags.3, i64 0, i64 0), ptr getelementptr inbounds ([2 x ptr], ptr @.doxa.struct.enumtys.4, i64 0, i64 0) }
@.peek.str.10 = private unnamed_addr constant [7 x i8] c"Couple\00"
@.peek.str.len.11 = constant i64 6
@.peek.str.16 = private unnamed_addr constant [2 x i8] c"a\00"
@.peek.str.len.17 = constant i64 1
@.peek.str.18 = private unnamed_addr constant [2 x i8] c"b\00"
@.peek.str.len.19 = constant i64 1
@.doxa.struct.names.12 = private constant [2 x ptr] [ptr getelementptr inbounds ([1 x i8], ptr @.peek.str.16, i64 0, i64 0), ptr getelementptr inbounds ([1 x i8], ptr @.peek.str.18, i64 0, i64 0)]
@.doxa.struct.tags.13 = private constant [2 x i64] [i64 7, i64 7]
@.doxa.struct.enumtys.14 = private constant [2 x ptr] [ptr null, ptr null]
@.doxa.struct.desc.15 = private constant { ptr, i64, ptr, ptr, ptr } { ptr getelementptr inbounds ([6 x i8], ptr @.peek.str.10, i64 0, i64 0), i64 2, ptr getelementptr inbounds ([2 x ptr], ptr @.doxa.struct.names.12, i64 0, i64 0), ptr getelementptr inbounds ([2 x i64], ptr @.doxa.struct.tags.13, i64 0, i64 0), ptr getelementptr inbounds ([2 x ptr], ptr @.doxa.struct.enumtys.14, i64 0, i64 0) }
