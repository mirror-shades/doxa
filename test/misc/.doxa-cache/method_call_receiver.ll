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
@.str.3 = private constant [1 x i8] c"!"
@.str.5 = private constant [3 x i8] c"age"
@.str.6 = private constant [4 x i8] c"name"
@.str.8 = private constant [3 x i8] c"Bob"
@.str.9 = private constant [1 x i8] c"b"
@.str.11 = private constant [5 x i8] c"Alice"
@.str.12 = private constant [1 x i8] c"a"
@.str.13 = private constant [1 x i8] c"\0A"
@.str.14 = private constant [3 x i8] c"Eve"
@.str.15 = private constant [5 x i8] c"Frank"
@.str.17 = private constant [1 x i8] c"A"
@.str.19 = private constant [1 x i8] c"B"
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

define %DoxaString @Person.introduce(ptr %0) {
entry:
  %var.__recv_0 = alloca ptr
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %1 = load ptr, ptr %0
  %2 = call ptr @doxa_struct_clone_at(i64 0, ptr %1)
  store ptr %2, ptr %var.__recv_0
  %3 = call %DoxaString @Person.greet(ptr %var.__recv_0)
  %4 = getelementptr inbounds [1 x i8], ptr @.str.3, i64 0, i64 0
  %5 = insertvalue %DoxaString undef, ptr %4, 0
  %6 = insertvalue %DoxaString %5, i64 1, 1
  %7 = extractvalue %DoxaString %3, 0
  %8 = extractvalue %DoxaString %3, 1
  %9 = extractvalue %DoxaString %6, 0
  %10 = extractvalue %DoxaString %6, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %7, i64 %8, ptr %9, i64 %10, ptr %str_out_ptr, ptr %str_out_len)
  %11 = load ptr, ptr %str_out_ptr
  %12 = load i64, ptr %str_out_len
  %13 = insertvalue %DoxaString undef, ptr %11, 0
  %14 = insertvalue %DoxaString %13, i64 %12, 1
  %15 = extractvalue %DoxaString %14, 0
  %16 = extractvalue %DoxaString %14, 1
  %17 = alloca ptr
  %18 = alloca i64
  store ptr null, ptr %17
  store i64 0, ptr %18
  call void @doxa_str_clone_at(i64 1, ptr %15, i64 %16, ptr %17, ptr %18)
  %19 = load ptr, ptr %17
  %20 = load i64, ptr %18
  %21 = insertvalue %DoxaString undef, ptr %19, 0
  %22 = insertvalue %DoxaString %21, i64 %20, 1
  call void @doxa_scope_exit()
  ret %DoxaString %22
}

define ptr @makePerson(%DoxaString %0) {
entry:
  %var.name = alloca %DoxaString
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %1 = extractvalue %DoxaString %0, 0
  %2 = extractvalue %DoxaString %0, 1
  %3 = alloca ptr
  %4 = alloca i64
  store ptr null, ptr %3
  store i64 0, ptr %4
  call void @doxa_str_clone_at(i64 0, ptr %1, i64 %2, ptr %3, ptr %4)
  %5 = load ptr, ptr %3
  %6 = load i64, ptr %4
  %7 = insertvalue %DoxaString undef, ptr %5, 0
  %8 = insertvalue %DoxaString %7, i64 %6, 1
  store %DoxaString %8, ptr %var.name
  %9 = add i64 0, 20
  %10 = getelementptr inbounds [3 x i8], ptr @.str.5, i64 0, i64 0
  %11 = insertvalue %DoxaString undef, ptr %10, 0
  %12 = insertvalue %DoxaString %11, i64 3, 1
  %13 = load %DoxaString, ptr %var.name
  %14 = getelementptr inbounds [4 x i8], ptr @.str.6, i64 0, i64 0
  %15 = insertvalue %DoxaString undef, ptr %14, 0
  %16 = insertvalue %DoxaString %15, i64 4, 1
  %17 = add i64 0, 24
  %18 = call ptr @doxa_scope_alloc(i64 %17, i64 8)
  %19 = bitcast ptr %18 to ptr
  %20 = extractvalue %DoxaString %13, 0
  %21 = extractvalue %DoxaString %13, 1
  %22 = alloca ptr
  %23 = alloca i64
  store ptr null, ptr %22
  store i64 0, ptr %23
  call void @doxa_str_clone_at(i64 0, ptr %20, i64 %21, ptr %22, ptr %23)
  %24 = load ptr, ptr %22
  %25 = load i64, ptr %23
  %26 = ptrtoint ptr %24 to i64
  %27 = getelementptr inbounds { i64, i64, i64 }, ptr %19, i32 0, i32 0
  store i64 %26, ptr %27
  %28 = getelementptr inbounds { i64, i64, i64 }, ptr %19, i32 0, i32 1
  store i64 %25, ptr %28
  %29 = getelementptr inbounds { i64, i64, i64 }, ptr %19, i32 0, i32 2
  store i64 %9, ptr %29
  call void @doxa_struct_register(ptr %19, ptr @.doxa.struct.desc.5)
  %30 = call ptr @doxa_struct_clone_at(i64 1, ptr %19)
  call void @doxa_scope_exit()
  ret ptr %30
}

define ptr @makeCouple() {
entry:
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %0 = add i64 0, 25
  %1 = getelementptr inbounds [3 x i8], ptr @.str.5, i64 0, i64 0
  %2 = insertvalue %DoxaString undef, ptr %1, 0
  %3 = insertvalue %DoxaString %2, i64 3, 1
  %4 = getelementptr inbounds [3 x i8], ptr @.str.8, i64 0, i64 0
  %5 = insertvalue %DoxaString undef, ptr %4, 0
  %6 = insertvalue %DoxaString %5, i64 3, 1
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
  %23 = getelementptr inbounds [1 x i8], ptr @.str.9, i64 0, i64 0
  %24 = insertvalue %DoxaString undef, ptr %23, 0
  %25 = insertvalue %DoxaString %24, i64 1, 1
  %26 = add i64 0, 30
  %27 = getelementptr inbounds [3 x i8], ptr @.str.5, i64 0, i64 0
  %28 = insertvalue %DoxaString undef, ptr %27, 0
  %29 = insertvalue %DoxaString %28, i64 3, 1
  %30 = getelementptr inbounds [5 x i8], ptr @.str.11, i64 0, i64 0
  %31 = insertvalue %DoxaString undef, ptr %30, 0
  %32 = insertvalue %DoxaString %31, i64 5, 1
  %33 = getelementptr inbounds [4 x i8], ptr @.str.6, i64 0, i64 0
  %34 = insertvalue %DoxaString undef, ptr %33, 0
  %35 = insertvalue %DoxaString %34, i64 4, 1
  %36 = add i64 0, 24
  %37 = call ptr @doxa_scope_alloc(i64 %36, i64 8)
  %38 = bitcast ptr %37 to ptr
  %39 = extractvalue %DoxaString %32, 0
  %40 = extractvalue %DoxaString %32, 1
  %41 = alloca ptr
  %42 = alloca i64
  store ptr null, ptr %41
  store i64 0, ptr %42
  call void @doxa_str_clone_at(i64 0, ptr %39, i64 %40, ptr %41, ptr %42)
  %43 = load ptr, ptr %41
  %44 = load i64, ptr %42
  %45 = ptrtoint ptr %43 to i64
  %46 = getelementptr inbounds { i64, i64, i64 }, ptr %38, i32 0, i32 0
  store i64 %45, ptr %46
  %47 = getelementptr inbounds { i64, i64, i64 }, ptr %38, i32 0, i32 1
  store i64 %44, ptr %47
  %48 = getelementptr inbounds { i64, i64, i64 }, ptr %38, i32 0, i32 2
  store i64 %26, ptr %48
  call void @doxa_struct_register(ptr %38, ptr @.doxa.struct.desc.5)
  %49 = getelementptr inbounds [1 x i8], ptr @.str.12, i64 0, i64 0
  %50 = insertvalue %DoxaString undef, ptr %49, 0
  %51 = insertvalue %DoxaString %50, i64 1, 1
  %52 = add i64 0, 16
  %53 = call ptr @doxa_scope_alloc(i64 %52, i64 8)
  %54 = bitcast ptr %53 to ptr
  %55 = ptrtoint ptr %38 to i64
  %56 = getelementptr inbounds { i64, i64 }, ptr %54, i32 0, i32 0
  store i64 %55, ptr %56
  %57 = ptrtoint ptr %12 to i64
  %58 = getelementptr inbounds { i64, i64 }, ptr %54, i32 0, i32 1
  store i64 %57, ptr %58
  call void @doxa_struct_register(ptr %54, ptr @.doxa.struct.desc.15)
  %59 = call ptr @doxa_struct_clone_at(i64 1, ptr %54)
  call void @doxa_scope_exit()
  ret ptr %59
}

define void @doxa_user_main() {
entry:
  %var.__recv_5 = alloca ptr
  %var.__recv_6 = alloca ptr
  %var.p = alloca ptr
  %var.__recv_2 = alloca ptr
  %var.__recv_1 = alloca ptr
  %var.__recv_3 = alloca ptr
  %var.c = alloca ptr
  %var.people = alloca ptr
  %var.__recv_4 = alloca ptr
  %str_out_ptr = alloca ptr
  %str_out_len = alloca i64
  call void @doxa_scope_enter()
  %0 = getelementptr inbounds [5 x i8], ptr @.str.11, i64 0, i64 0
  %1 = insertvalue %DoxaString undef, ptr %0, 0
  %2 = insertvalue %DoxaString %1, i64 5, 1
  %3 = call ptr @makePerson(%DoxaString %2)
  store ptr %3, ptr %var.p
  %4 = call %DoxaString @Person.greet(ptr %var.p)
  %5 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %6 = insertvalue %DoxaString undef, ptr %5, 0
  %7 = insertvalue %DoxaString %6, i64 1, 1
  %8 = extractvalue %DoxaString %4, 0
  %9 = extractvalue %DoxaString %4, 1
  %10 = extractvalue %DoxaString %7, 0
  %11 = extractvalue %DoxaString %7, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %8, i64 %9, ptr %10, i64 %11, ptr %str_out_ptr, ptr %str_out_len)
  %12 = load ptr, ptr %str_out_ptr
  %13 = load i64, ptr %str_out_len
  %14 = insertvalue %DoxaString undef, ptr %12, 0
  %15 = insertvalue %DoxaString %14, i64 %13, 1
  %16 = extractvalue %DoxaString %15, 0
  %17 = extractvalue %DoxaString %15, 1
  call void @doxa_write_cstr(ptr %16, i64 %17)
  %18 = call ptr @makeCouple()
  store ptr %18, ptr %var.c
  %19 = load ptr, ptr %var.c
  %20 = getelementptr inbounds { i64, i64 }, ptr %19, i32 0, i32 0
  %21 = load i64, ptr %20
  %22 = inttoptr i64 %21 to ptr
  %23 = call ptr @doxa_struct_clone_at(i64 0, ptr %22)
  store ptr %23, ptr %var.__recv_1
  %24 = call %DoxaString @Person.greet(ptr %var.__recv_1)
  %25 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %26 = insertvalue %DoxaString undef, ptr %25, 0
  %27 = insertvalue %DoxaString %26, i64 1, 1
  %28 = extractvalue %DoxaString %24, 0
  %29 = extractvalue %DoxaString %24, 1
  %30 = extractvalue %DoxaString %27, 0
  %31 = extractvalue %DoxaString %27, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %28, i64 %29, ptr %30, i64 %31, ptr %str_out_ptr, ptr %str_out_len)
  %32 = load ptr, ptr %str_out_ptr
  %33 = load i64, ptr %str_out_len
  %34 = insertvalue %DoxaString undef, ptr %32, 0
  %35 = insertvalue %DoxaString %34, i64 %33, 1
  %36 = extractvalue %DoxaString %35, 0
  %37 = extractvalue %DoxaString %35, 1
  call void @doxa_write_cstr(ptr %36, i64 %37)
  %38 = load ptr, ptr %var.c
  %39 = getelementptr inbounds { i64, i64 }, ptr %38, i32 0, i32 1
  %40 = load i64, ptr %39
  %41 = inttoptr i64 %40 to ptr
  %42 = call ptr @doxa_struct_clone_at(i64 0, ptr %41)
  store ptr %42, ptr %var.__recv_2
  %43 = call %DoxaString @Person.introduce(ptr %var.__recv_2)
  %44 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %45 = insertvalue %DoxaString undef, ptr %44, 0
  %46 = insertvalue %DoxaString %45, i64 1, 1
  %47 = extractvalue %DoxaString %43, 0
  %48 = extractvalue %DoxaString %43, 1
  %49 = extractvalue %DoxaString %46, 0
  %50 = extractvalue %DoxaString %46, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %47, i64 %48, ptr %49, i64 %50, ptr %str_out_ptr, ptr %str_out_len)
  %51 = load ptr, ptr %str_out_ptr
  %52 = load i64, ptr %str_out_len
  %53 = insertvalue %DoxaString undef, ptr %51, 0
  %54 = insertvalue %DoxaString %53, i64 %52, 1
  %55 = extractvalue %DoxaString %54, 0
  %56 = extractvalue %DoxaString %54, 1
  call void @doxa_write_cstr(ptr %55, i64 %56)
  %57 = getelementptr inbounds [3 x i8], ptr @.str.14, i64 0, i64 0
  %58 = insertvalue %DoxaString undef, ptr %57, 0
  %59 = insertvalue %DoxaString %58, i64 3, 1
  %60 = call ptr @makePerson(%DoxaString %59)
  %61 = call ptr @doxa_struct_clone_at(i64 0, ptr %60)
  store ptr %61, ptr %var.__recv_3
  %62 = call %DoxaString @Person.greet(ptr %var.__recv_3)
  %63 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %64 = insertvalue %DoxaString undef, ptr %63, 0
  %65 = insertvalue %DoxaString %64, i64 1, 1
  %66 = extractvalue %DoxaString %62, 0
  %67 = extractvalue %DoxaString %62, 1
  %68 = extractvalue %DoxaString %65, 0
  %69 = extractvalue %DoxaString %65, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %66, i64 %67, ptr %68, i64 %69, ptr %str_out_ptr, ptr %str_out_len)
  %70 = load ptr, ptr %str_out_ptr
  %71 = load i64, ptr %str_out_len
  %72 = insertvalue %DoxaString undef, ptr %70, 0
  %73 = insertvalue %DoxaString %72, i64 %71, 1
  %74 = extractvalue %DoxaString %73, 0
  %75 = extractvalue %DoxaString %73, 1
  call void @doxa_write_cstr(ptr %74, i64 %75)
  %76 = getelementptr inbounds [5 x i8], ptr @.str.15, i64 0, i64 0
  %77 = insertvalue %DoxaString undef, ptr %76, 0
  %78 = insertvalue %DoxaString %77, i64 5, 1
  %79 = call ptr @makePerson(%DoxaString %78)
  %80 = call ptr @doxa_struct_clone_at(i64 0, ptr %79)
  store ptr %80, ptr %var.__recv_4
  %81 = call %DoxaString @Person.introduce(ptr %var.__recv_4)
  %82 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %83 = insertvalue %DoxaString undef, ptr %82, 0
  %84 = insertvalue %DoxaString %83, i64 1, 1
  %85 = extractvalue %DoxaString %81, 0
  %86 = extractvalue %DoxaString %81, 1
  %87 = extractvalue %DoxaString %84, 0
  %88 = extractvalue %DoxaString %84, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %85, i64 %86, ptr %87, i64 %88, ptr %str_out_ptr, ptr %str_out_len)
  %89 = load ptr, ptr %str_out_ptr
  %90 = load i64, ptr %str_out_len
  %91 = insertvalue %DoxaString undef, ptr %89, 0
  %92 = insertvalue %DoxaString %91, i64 %90, 1
  %93 = extractvalue %DoxaString %92, 0
  %94 = extractvalue %DoxaString %92, 1
  call void @doxa_write_cstr(ptr %93, i64 %94)
  %95 = call ptr @doxa_array_new(i64 8, i64 7, i64 2)
  %96 = add i64 0, 0
  %97 = getelementptr inbounds [1 x i8], ptr @.str.17, i64 0, i64 0
  %98 = insertvalue %DoxaString undef, ptr %97, 0
  %99 = insertvalue %DoxaString %98, i64 1, 1
  %100 = call ptr @makePerson(%DoxaString %99)
  %101 = ptrtoint ptr %100 to i64
  call void @doxa_array_set_i64(ptr %95, i64 %96, i64 %101)
  %102 = add i64 0, 1
  %103 = getelementptr inbounds [1 x i8], ptr @.str.19, i64 0, i64 0
  %104 = insertvalue %DoxaString undef, ptr %103, 0
  %105 = insertvalue %DoxaString %104, i64 1, 1
  %106 = call ptr @makePerson(%DoxaString %105)
  %107 = ptrtoint ptr %106 to i64
  call void @doxa_array_set_i64(ptr %95, i64 %102, i64 %107)
  %108 = call ptr @doxa_array_clone_at(i64 0, ptr %95)
  store ptr %108, ptr %var.people
  %109 = load ptr, ptr %var.people
  %110 = add i64 0, 0
  %111 = call i64 @doxa_array_get_i64(ptr %109, i64 %110)
  %112 = inttoptr i64 %111 to ptr
  %113 = call ptr @doxa_struct_clone_at(i64 0, ptr %112)
  store ptr %113, ptr %var.__recv_5
  %114 = call %DoxaString @Person.greet(ptr %var.__recv_5)
  %115 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %116 = insertvalue %DoxaString undef, ptr %115, 0
  %117 = insertvalue %DoxaString %116, i64 1, 1
  %118 = extractvalue %DoxaString %114, 0
  %119 = extractvalue %DoxaString %114, 1
  %120 = extractvalue %DoxaString %117, 0
  %121 = extractvalue %DoxaString %117, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %118, i64 %119, ptr %120, i64 %121, ptr %str_out_ptr, ptr %str_out_len)
  %122 = load ptr, ptr %str_out_ptr
  %123 = load i64, ptr %str_out_len
  %124 = insertvalue %DoxaString undef, ptr %122, 0
  %125 = insertvalue %DoxaString %124, i64 %123, 1
  %126 = extractvalue %DoxaString %125, 0
  %127 = extractvalue %DoxaString %125, 1
  call void @doxa_write_cstr(ptr %126, i64 %127)
  %128 = load ptr, ptr %var.people
  %129 = add i64 0, 1
  %130 = call i64 @doxa_array_get_i64(ptr %128, i64 %129)
  %131 = inttoptr i64 %130 to ptr
  %132 = call ptr @doxa_struct_clone_at(i64 0, ptr %131)
  store ptr %132, ptr %var.__recv_6
  %133 = call %DoxaString @Person.introduce(ptr %var.__recv_6)
  %134 = getelementptr inbounds [1 x i8], ptr @.str.13, i64 0, i64 0
  %135 = insertvalue %DoxaString undef, ptr %134, 0
  %136 = insertvalue %DoxaString %135, i64 1, 1
  %137 = extractvalue %DoxaString %133, 0
  %138 = extractvalue %DoxaString %133, 1
  %139 = extractvalue %DoxaString %136, 0
  %140 = extractvalue %DoxaString %136, 1
  store ptr null, ptr %str_out_ptr
  store i64 0, ptr %str_out_len
  call void @doxa_str_concat(ptr %137, i64 %138, ptr %139, i64 %140, ptr %str_out_ptr, ptr %str_out_len)
  %141 = load ptr, ptr %str_out_ptr
  %142 = load i64, ptr %str_out_len
  %143 = insertvalue %DoxaString undef, ptr %141, 0
  %144 = insertvalue %DoxaString %143, i64 %142, 1
  %145 = extractvalue %DoxaString %144, 0
  %146 = extractvalue %DoxaString %144, 1
  call void @doxa_write_cstr(ptr %145, i64 %146)
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
