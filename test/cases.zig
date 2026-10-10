const std = @import("std");
const answers = @import("answers");

pub const print_result = answers.print_result;
pub const peek_result = answers.peek_result;

/// Which execution pipeline a case is exercised through. Most cases run under
/// both `doxa run` and `doxa compile`; a case opts out of one only when it
/// tests something pipeline-specific, and that opt-out is stated here rather
/// than being an accident of whichever suite happened to be edited.
pub const Pipeline = enum { run, compile };

/// How a case's result is judged.
pub const Mode = enum {
    /// The program exits 0 and its stdout is `expected_print`, line for line.
    print,
    /// The program exits 0 and its stderr peek rows are `expected_peek`.
    /// Besides peeks, stderr may carry only compile warnings.
    peek,
    /// The program compiles, runs, and exits with `expect_code`, writing
    /// `expect_stderr` (when given) to stderr. A compile error never satisfies
    /// it, whatever its exit status or text.
    terminate,
    /// The compiler rejects the program: one compile error carries
    /// `expect_error` as its code and `expect_stderr` in its text.
    reject,
};

pub const Case = struct {
    name: []const u8,
    /// Source file, relative to the repository root.
    path: []const u8,
    mode: Mode = .print,
    input: ?[]const u8 = null,
    expected_print: ?[]const print_result = null,
    expected_peek: ?[]const peek_result = null,
    extra_args: []const []const u8 = &.{},
    pipelines: []const Pipeline = &.{ .run, .compile },
    /// `.terminate`: the program's exit status.
    expect_code: ?u8 = null,
    /// `.terminate`: text the program writes to stderr. `.reject`: text of the
    /// expected compile error.
    expect_stderr: ?[]const u8 = null,
    /// `.reject`: the expected compile error's code, e.g. `E1003`.
    expect_error: ?[]const u8 = null,

    pub fn runsOn(self: Case, pipeline: Pipeline) bool {
        for (self.pipelines) |candidate| {
            if (candidate == pipeline) return true;
        }
        return false;
    }

    /// Why this case's fields contradict its mode, or null when they agree.
    /// Checked for every case at compile time, so a judge never meets a case it
    /// cannot judge.
    fn contradiction(self: Case) ?[]const u8 {
        const prints = self.expected_print != null;
        const peeks = self.expected_peek != null;
        const runs = self.input != null or self.extra_args.len != 0;
        return switch (self.mode) {
            .print => if (!prints or peeks) "a print case needs expected_print and nothing else" else null,
            .peek => if (!peeks or prints) "a peek case needs expected_peek and nothing else" else null,
            .terminate => if (prints or peeks or self.expect_error != null)
                "a terminate case has no expected output or compile error"
            else if ((self.expect_code orelse 0) == 0)
                "a terminate case needs a non-zero expect_code"
            else
                null,
            .reject => if (prints or peeks or runs or self.expect_code != null)
                "a reject case never runs, so it has no output, input, or exit code"
            else if (self.expect_error == null or self.expect_stderr == null)
                "a reject case needs expect_error and expect_stderr"
            else
                null,
        };
    }
};

/// Source basename without its extension. A local implementation (rather than
/// `std.fs.path.stem`) because the comptime uniqueness guard below evaluates it
/// at compile time and the std path parser blows the branch quota.
fn sourceStem(path: []const u8) []const u8 {
    var start: usize = path.len;
    while (start > 0 and path[start - 1] != '/' and path[start - 1] != '\\') start -= 1;
    const base = path[start..];
    if (std.mem.lastIndexOfScalar(u8, base, '.')) |dot| return base[0..dot];
    return base;
}

/// Compiled artifacts live at `test/out/<stem>` (the CLI appends the platform
/// executable suffix). Every source stem in the table is unique, so the output
/// path is a pure function of the source path.
pub fn outputPathFor(allocator: std.mem.Allocator, source_path: []const u8) ![]u8 {
    return std.fmt.allocPrint(allocator, "test/out/{s}", .{sourceStem(source_path)});
}

const shared_cases = [_]Case{
    .{
        .name = "big file",
        .path = "./test/misc/bigfile.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_bigfile_results[0..],
    },
    .{
        .name = "brainfuck",
        .path = "./test/examples/brainfuck.doxa",
        .expected_print = answers.expected_brainfuck_results[0..],
    },
    .{
        .name = "complex print",
        .path = "./test/misc/complex_print.doxa",
        .expected_print = answers.expected_complex_print_results[0..],
    },
    .{
        .name = "array storage migration",
        .path = "./test/misc/array_storage_migration.doxa",
        .expected_print = answers.expected_array_storage_results[0..],
    },
    .{
        .name = "alias arrays",
        .path = "./test/misc/alias_arrays.doxa",
        .expected_print = answers.expected_alias_arrays_results[0..],
    },
    .{
        .name = "alias string mutation",
        .path = "./test/misc/alias_string_mutation.doxa",
        .expected_print = answers.expected_alias_string_mutation_results[0..],
    },
    .{
        .name = "alias re-pass to boxed union parameter",
        .path = "./test/misc/alias_repush_union.doxa",
        .expected_print = answers.expected_alias_repush_union_results[0..],
    },
    .{
        .name = "stdlib methods",
        .path = "./test/misc/stdlib_methods.doxa",
        .expected_print = answers.expected_stdlib_methods_results[0..],
    },
    .{
        .name = "slice heap string",
        .path = "./test/misc/slice_heap_string.doxa",
        .expected_print = answers.expected_slice_heap_string_results[0..],
    },
    .{
        .name = "var seeded from a const reference",
        .path = "./test/misc/var_from_const_ref.doxa",
        .expected_print = answers.expected_var_from_const_ref_results[0..],
    },
    .{
        .name = "union narrow",
        .path = "./test/misc/union_narrow.doxa",
        .expected_print = answers.expected_union_narrow_results[0..],
    },
    .{
        .name = "uninitialized union holds its first member's default",
        .path = "./test/misc/union_default.doxa",
        .expected_print = answers.expected_union_default_results[0..],
    },
    .{
        .name = "union stringify before narrowing",
        .path = "./test/misc/union_stringify.doxa",
        .expected_print = answers.expected_union_stringify_results[0..],
    },
    .{
        .name = "groups end to end",
        .path = "./test/misc/group_test.doxa",
        .expected_print = answers.expected_group_test_results[0..],
    },
    .{
        .name = "union narrowing reads as the member",
        .path = "./test/misc/union_narrow_read.doxa",
        .expected_print = answers.expected_union_narrow_read_results[0..],
    },
    .{
        .name = "match on a struct subject",
        .path = "./test/misc/match_struct.doxa",
        .expected_print = answers.expected_match_struct_results[0..],
    },
    .{
        .name = "match on an enum subject of every expression form",
        .path = "./test/misc/match_enum_subject_forms.doxa",
        .expected_print = answers.expected_match_enum_subject_forms_results[0..],
    },
    .{
        .name = "match with a nothing arm over a union",
        .path = "./test/misc/match_union_nothing_arm.doxa",
        .expected_print = answers.expected_match_union_nothing_arm_results[0..],
    },
    .{
        .name = "as fallback block that diverges",
        .path = "./test/misc/as_fallback_diverges.doxa",
        .expected_print = answers.expected_as_fallback_diverges_results[0..],
    },
    .{
        .name = "group value equals a member value",
        .path = "./test/misc/group_equals_member.doxa",
        .expected_print = answers.expected_group_equals_member_results[0..],
    },
    .{
        .name = "match on a union subject",
        .path = "./test/misc/match_union_struct.doxa",
        .expected_print = answers.expected_match_union_struct_results[0..],
    },
    .{
        .name = "match arm ruled out statically",
        .path = "./test/misc/match_dead_arm.doxa",
        .expected_print = answers.expected_match_dead_arm_results[0..],
    },
    .{
        .name = "match arm narrows a union subject",
        .path = "./test/misc/match_union_narrow.doxa",
        .expected_print = answers.expected_match_union_narrow_results[0..],
    },
    .{
        .name = "group re-assigned inside a narrowed branch",
        .path = "./test/misc/group_narrow_store.doxa",
        .expected_print = answers.expected_group_narrow_store_results[0..],
    },
    .{
        .name = "group copied across return and globals",
        .path = "./test/misc/group_global_copy.doxa",
        .expected_print = answers.expected_group_global_copy_results[0..],
    },
    .{
        .name = "match on a union global subject",
        .path = "./test/misc/match_union_global.doxa",
        .expected_print = answers.expected_match_union_global_results[0..],
    },
    .{
        .name = "multi-pattern arm binds without narrowing",
        .path = "./test/misc/match_union_multi.doxa",
        .expected_print = answers.expected_match_union_multi_results[0..],
    },
    .{
        .name = "loop variable and diverging cast fallback",
        .path = "./test/misc/loop_cast_fallback.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "found" },
            .{ .value = "missing" },
            .{ .value = "hey" },
        },
    },
    .{
        .name = "bare control statement as a branch body",
        .path = "./test/misc/branch_control_body.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "sum       = 10" },
            .{ .value = "first     = 5" },
            .{ .value = "firstMiss = -1" },
            .{ .value = "blanks    = 1" },
            .{ .value = "nonBlank  = 2" },
            .{ .value = "firstStr  = a" },
            .{ .value = "firstNone = none" },
            .{ .value = "tagZ      = 0" },
            .{ .value = "tagA      = 11" },
            .{ .value = "scanA     = 11" },
            .{ .value = "scanZ     = 0" },
        },
    },
    .{
        .name = "diverging branch keeps the opposite branch",
        .path = "./test/misc/loop_branch_jump.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "thenContinue      = 2" },
            .{ .value = "thenContinueBare  = 2" },
            .{ .value = "thenBreak         = 1" },
            .{ .value = "thenContinueWork  = 102" },
            .{ .value = "thenContinueNoElse= 2" },
            .{ .value = "elseContinue      = 2" },
            .{ .value = "thenContElseBreak = 0" },
            .{ .value = "nestedInner       = 24" },
            .{ .value = "asThenContinue    = 1" },
        },
    },
    .{
        .name = "nested struct return",
        .path = "./test/misc/nested_struct_return.doxa",
        .expected_print = answers.expected_nested_struct_return_results[0..],
    },
    .{
        .name = "reordered struct literal layout",
        .path = "./test/misc/struct_literal_reordered.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "hi Alice" },
            .{ .value = "Alice 30" },
            .{ .value = "Alice 30" },
            .{ .value = "Bob 25" },
        },
    },
    .{
        .name = "struct method call receivers",
        .path = "./test/misc/method_call_receiver.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "hi Alice" },
            .{ .value = "hi Alice" },
            .{ .value = "hi Bob!" },
            .{ .value = "hi Eve" },
            .{ .value = "hi Frank!" },
            .{ .value = "hi A" },
            .{ .value = "hi B!" },
        },
    },
    .{
        .name = "method call on a copied struct local",
        .path = "./test/misc/dropped_method_on_copied_struct.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "original 7" },
            .{ .value = "copied 7" },
            .{ .value = "after bump 12" },
            .{ .value = "field 12" },
        },
    },
    .{
        .name = "same-scope struct assignment aliases",
        .path = "./test/misc/same_scope_struct_alias.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "before 1 1" },
            .{ .value = "after 42 42" },
        },
    },
    .{
        .name = "tetra parameter from a comparison",
        .path = "./test/misc/tetra_param_comparison.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "int eq true" },
            .{ .value = "int lt false" },
            .{ .value = "mod eq false" },
            .{ .value = "not false" },
            .{ .value = "and true" },
            .{ .value = "byte true" },
            .{ .value = "float true" },
            .{ .value = "string true" },
            .{ .value = "place 14 false" },
            .{ .value = "stored true" },
            .{ .value = "stored not false" },
        },
    },
    .{
        .name = "runtime const if",
        .path = "./test/misc/runtime_const_if.doxa",
        .extra_args = &[_][]const u8{"hello"},
        .expected_print = answers.expected_runtime_const_if_results[0..],
    },
    .{
        .name = "global array push",
        .path = "./test/misc/global_array_push.doxa",
        .expected_print = answers.expected_global_array_push_results[0..],
    },
    .{
        .name = "expression branch merge",
        .path = "./test/misc/expression_branch_merge.doxa",
        .expected_print = answers.expected_expression_branch_merge_results[0..],
    },
    .{
        .name = "copy free return",
        .path = "./test/misc/copy_free_return.doxa",
        .expected_print = answers.expected_copy_free_return_results[0..],
    },
    .{
        .name = "fixed struct array",
        .path = "./test/misc/fixed_struct_array.doxa",
        .expected_print = answers.expected_fixed_struct_array_results[0..],
    },
    .{
        .name = "fixed struct field array",
        .path = "./test/misc/fixed_struct_field_array.doxa",
        .expected_print = answers.expected_fixed_struct_field_array_results[0..],
    },
    .{
        .name = "fixed struct array params",
        .path = "./test/misc/fixed_struct_array_params.doxa",
        .expected_print = answers.expected_fixed_struct_array_params_results[0..],
    },
    .{
        .name = "union struct field",
        .path = "./test/misc/union_struct_field.doxa",
        .expected_print = answers.expected_union_struct_field_results[0..],
    },
    .{
        .name = "union nothing match",
        .path = "./test/misc/union_nothing_match.doxa",
        .expected_print = answers.expected_union_nothing_match_results[0..],
    },
    .{
        .name = "floored arithmetic",
        .path = "./test/misc/floored_arith.doxa",
        .expected_print = answers.expected_floored_arith_results[0..],
    },
    .{
        .name = "a group member keeps its identity through a union",
        .path = "./test/misc/group_through_union.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "str io parse file " },
            .{ .value = "str io parse file " },
            .{ .value = "true true true false false false" },
            .{ .value = "true true" },
            .{ .value = "true false" },
            .{ .value = "false true true" },
            .{ .value = "io parse io parse" },
        },
    },
    .{
        .name = "a group array keeps each element's member",
        .path = "./test/misc/group_arrays.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "built:   parse file:tmp/x io " },
            .{ .value = "set:     io file:tmp/x io " },
            .{ .value = "insert:  io parse file:tmp/x io " },
            .{ .value = "remove:  file:tmp/x | io parse io " },
            .{ .value = "pop:     io | io parse " },
            .{ .value = "concat:  io parse parse file:tmp/x io " },
            .{ .value = "slice:   parse parse file:tmp/x " },
            .{ .value = "index:   io len 5" },
            .{ .value = "nested:  parse file:tmp/x io " },
            .{ .value = "local:   parse io  true false" },
            .{ .value = "field:   parse io" },
            .{ .value = "fixed:   parse io" },
        },
    },
    .{
        .name = "every field and element store receives its slot's type",
        .path = "./test/misc/store_conversions.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "3.0 3.0 3.0" },
            .{ .value = "0.0 3.0" },
            .{ .value = "true false" },
            .{ .value = "1.5 3" },
            .{ .value = "end" },
            .{ .value = "sound sound light" },
            .{ .value = "1.0 3.0 1 3" },
        },
    },
    .{
        .name = "a boxed enum is named through the box registry",
        .path = "./test/misc/box_enum_names.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "ParseError.Eof" },
            .{ .value = "IOError.Denied" },
            .{ .value = "[IOError.Denied, ParseError.BadToken]" },
            .{ .value = "IOError.NotFound" },
            .{ .value = "[[ParseError.Eof], [IOError.Denied]]" },
            .{ .value = "ParseError.Eof" },
        },
    },
    .{
        .name = "a union peek names the group it was written with",
        .path = "./test/misc/group_union_peek.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = ">string | Error", .value = "\"fine\"" },
            .{ .type = "string | >Error", .value = "{ code: 13, path: \"/\" }" },
            .{ .type = "IOError | >FileError", .value = "{ code: 2, path: \"/tmp\" }" },
        },
    },
    .{
        .name = "a name is reused only where the earlier binding is not visible",
        .path = "./test/misc/name_reuse.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = "int | >string", .value = "\"b\"" },
            .{ .type = "float", .value = "2.5" },
            .{ .type = "string", .value = "\"s\"" },
            .{ .type = "int", .value = "-4" },
            .{ .type = "int", .value = "1" },
            .{ .type = "string", .value = "\"two\"" },
            .{ .type = "int", .value = "-9" },
            .{ .type = "string", .value = "\"fallback\"" },
        },
    },
    .{
        .name = "strings order byte-wise lexicographically",
        .path = "./test/misc/string_order.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "true false true false" },
            .{ .value = "true false true true false" },
            .{ .value = "true false true true" },
        },
    },
    .{
        .name = "a union is one type however it is spelled or widened",
        .path = "./test/misc/union_identity.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = "int", .value = "7" },
            .{ .type = ">int | string", .value = "3" },
            .{ .type = "int | float | >string", .value = "\"hi\"" },
            .{ .type = "int | float | >string", .value = "\"local\"" },
            .{ .type = "int | float | >string", .value = "\"local\"" },
            .{ .type = "string", .value = "\"string\"" },
        },
    },
    .{
        .name = "a store converts its value to the type of the storage it writes",
        .path = "./test/misc/store_shapes.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = "int | >string", .value = "\"s\"" },
            .{ .type = "Piece | >nothing", .value = "nothing" },
            .{ .type = "int | >string", .value = "\"s\"" },
            .{ .type = "Piece | >nothing", .value = "nothing" },
            .{ .type = "int | >string", .value = "\"p\"" },
            .{ .type = ">int | string", .value = "3" },
            .{ .type = "string | >int[]", .value = "[5, 6]" },
            .{ .type = "int | >string", .value = "\"f\"" },
            .{ .type = "Piece | >nothing", .value = "nothing" },
            .{ .type = ">int | string", .value = "1" },
            .{ .type = ">int | string", .value = "2" },
            .{ .type = ">int | float", .value = "1" },
            .{ .type = ">int | string", .value = "7" },
            .{ .type = "int | >string", .value = "\"values\"" },
            .{ .type = ">int | string", .value = "7" },
            .{ .type = "byte", .value = "0xC8" },
            .{ .type = "nothing", .value = "nothing" },
        },
    },
    .{
        .name = "a read takes the analyzer's type whatever the operand's form",
        .path = "./test/misc/read_shapes.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "3 5 5 h" },
            .{ .value = "2 3 box! box 2" },
            .{ .value = "cde 3 e 1 ab?" },
            .{ .value = "5" },
            .{ .value = "8 values! 14 6" },
            .{ .value = "then 2 9" },
            .{ .value = "else 6 narrow?" },
            .{ .value = "array 2 15 string 3 abc!" },
            .{ .value = "3 2" },
        },
    },
    .{
        .name = "a compound assignment is the operation it spells",
        .path = "./test/misc/compound_sugar.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "0x14 9 1.25 ab [11, 2, 3] 5 c! 5" },
        },
    },
    .{
        .name = "an array literal is typed by the array its context expects",
        .path = "./test/misc/array_literal_context.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "[\"a\"] [1.0, 2.0, -3.0] [7, 8, 9] Hi [0.5] [4, 5] [[1.0], [2.0, 3.0], [4.0]] 3.0" },
            .{ .value = "ABC ABC" },
        },
    },
    .{
        .name = "a peeked initializer keeps its operand's type",
        .path = "./test/misc/peek_initializer.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "0.75 true spire" },
            .{ .value = "8" },
            .{ .value = "5.0 float" },
        },
    },
    .{
        .name = "float comparisons follow IEEE 754",
        .path = "./test/misc/float_compare.doxa",
        .expected_print = answers.expected_float_compare_results[0..],
    },
    .{
        .name = "float promotion in operators, value positions, and array literals",
        .path = "./test/misc/float_promotion.doxa",
        .expected_print = answers.expected_float_promotion_results[0..],
    },
    .{
        .name = "float extremes, infinities, and negative zero",
        .path = "./test/misc/float_edge.doxa",
        .expected_print = answers.expected_float_edge_results[0..],
    },
    .{
        .name = "float conversions",
        .path = "./test/misc/float_convert.doxa",
        .expected_print = answers.expected_float_convert_results[0..],
    },
    .{
        .name = "descriptor skip",
        .path = "./test/misc/descriptor_skip.doxa",
        .expected_print = answers.expected_descriptor_skip_results[0..],
    },
    .{
        .name = "module string interp",
        .path = "./test/misc/module_string_interp.doxa",
        .expected_print = answers.expected_module_string_interp_results[0..],
    },
    .{
        .name = "module method calls",
        .path = "./test/misc/module_method_calls.doxa",
        .expected_print = answers.expected_module_method_calls_results[0..],
    },
    .{
        .name = "module alias owner scope",
        .path = "./test/misc/module_alias_owner.doxa",
        .expected_print = answers.expected_module_alias_owner_results[0..],
    },
    .{
        .name = "module qualified collision",
        .path = "./test/misc/module_qualified_collision.doxa",
        .expected_print = answers.expected_module_qualified_collision_results[0..],
    },
    .{
        .name = "enum variant shorthand takes its enum from context",
        .path = "./test/misc/enum_variant_context.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "status result true true true true true true broken" },
        },
    },
    .{
        .name = "std specifier imports like any root-qualified path",
        .path = "./test/misc/std_specifier.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "std//std.doxa" },
            .{ .value = "rooted" },
        },
    },
    .{
        .name = "intrinsics called postfix",
        .path = "./test/misc/intrinsic_postfix.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "3 2 2" },
            .{ .value = "3 ab" },
        },
    },
    .{
        .name = "module type identity",
        .path = "./test/misc/module_type_identity.doxa",
        .expected_print = answers.expected_module_type_identity_results[0..],
    },
    .{
        .name = "module qualified group",
        .path = "./test/misc/module_qualified_group.doxa",
        .expected_print = answers.expected_module_qualified_group_results[0..],
    },
    .{
        .name = "module imported struct members",
        .path = "./test/misc/module_imported_struct_members.doxa",
        .expected_print = answers.expected_module_imported_struct_members_results[0..],
    },
    .{
        .name = "inline zig same block name in two files",
        .path = "./test/misc/inline_zig_same_name.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "1 2" },
        },
    },
    .{
        .name = "inline zig string",
        .path = "./test/misc/inline_zig_string.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "abc" },
            .{ .value = "hi" },
        },
    },
    .{
        .name = "inline zig test",
        .path = "./test/misc/inline_zig_test.doxa",
        .expected_print = answers.expected_inline_zig_test_results[0..],
    },
    .{
        .name = "inline zig arrays",
        .path = "./test/misc/inline_zig_arrays.doxa",
        .expected_print = answers.expected_inline_zig_arrays_results[0..],
    },
    .{
        .name = "inline zig nested arrays",
        .path = "./test/misc/inline_zig_nested_arrays.doxa",
        .expected_print = answers.expected_inline_zig_nested_arrays_results[0..],
    },
    .{
        .name = "inline zig enums",
        .path = "./test/misc/inline_zig_enums.doxa",
        .expected_print = answers.expected_inline_zig_enums_results[0..],
    },
    .{
        .name = "inline zig qualified enum",
        .path = "./test/misc/inline_zig_qualified_enum.doxa",
        .expected_print = answers.expected_inline_zig_qualified_enum_results[0..],
    },
    .{
        .name = "inline zig error union",
        .path = "./test/misc/inline_zig_errors.doxa",
        .expected_print = answers.expected_inline_zig_errors_results[0..],
    },
    .{
        .name = "union nothing narrow",
        .path = "./test/misc/union_nothing_narrow.doxa",
        .expected_print = answers.expected_union_nothing_narrow_results[0..],
    },
    .{
        .name = "std file list",
        .path = "./test/misc/std_file_list.doxa",
        .expected_print = answers.expected_std_file_list_results[0..],
    },
    .{
        .name = "host path join",
        .path = "./test/misc/host_path.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "a/b" },
            .{ .value = "a\\b" },
            .{ .value = "/b/c" },
            .{ .value = "/etc/hosts" },
            .{ .value = "C:\\Users\\x" },
            .{ .value = "a/b/c" },
            .{ .value = "a/b" },
            .{ .value = "host ok" },
        },
    },
    .{
        .name = "inline zig string escape",
        .path = "./test/misc/inline_zig_string_escape.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "root" },
            .{ .value = "block" },
            .{ .value = "returned" },
            .{ .value = "zzz" },
            .{ .value = "[]" },
        },
    },
    .{
        .name = "zig import test",
        .path = "./test/misc/zig_import_test.doxa",
        .expected_print = answers.expected_zig_import_test_results[0..],
    },
    .{
        .name = "zig specific import",
        .path = "./test/misc/zig_specific_import.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "42" },
        },
    },
    .{
        .name = "nested import",
        .path = "./test/misc/nested_import.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "clock=true" },
        },
    },
    .{
        .name = "struct method std import",
        .path = "./test/misc/struct_method_std.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "dump={\"k\":\"hi\"}" },
        },
    },
    .{
        .name = "json module import",
        .path = "./test/misc/json_module_import.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "plan=hello" },
        },
    },
    .{
        .name = "match expression value",
        .path = "./test/misc/match_expression_value.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "coord" },
            .{ .value = "plan" },
            .{ .value = "many" },
        },
    },
    .{
        .name = "method call concat",
        .path = "./test/misc/method_call_concat.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "greet=hi world" },
            .{ .value = "hi world suffix" },
            .{ .value = "hi worldhi world" },
        },
    },
    .{
        .name = "inline heap return",
        .path = "./test/misc/inline_heap_return.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "free world" },
            .{ .value = "x=free world" },
        },
    },
    .{
        .name = "peek escapes",
        .path = "./test/misc/peek_escapes.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_peek_escapes_results[0..],
    },
    .{
        .name = "expressions",
        .path = "./test/misc/expressions.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_expressions_results[0..],
    },
    .{
        .name = "methods",
        .path = "./test/misc/methods.doxa",
        .mode = .peek,
        .input = "f\n",
        .expected_peek = answers.expected_methods_results[0..],
    },
    .{
        .name = "union enum return",
        .path = "./test/misc/union_enum_return.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_union_enum_return_results[0..],
    },
    .{
        .name = "module private call",
        .path = "./test/misc/module_private_call.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_module_private_call_results[0..],
    },
    .{
        .name = "import submodule",
        .path = "./test/misc/import_submodule.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "submodule import works" },
            .{ .value = "true" },
        },
    },
    .{
        .name = "json",
        .path = "./test/misc/json.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "scalars ok" },
            .{ .value = "object read ok" },
            .{ .value = "array iterate ok" },
            .{ .value = "misses ok" },
            .{ .value = "parse error ok" },
            .{ .value = "stale nodes ok" },
            .{ .value = "writer exact ok" },
            .{ .value = "writer escapes ok" },
            .{ .value = "writer misuse ok" },
            .{ .value = "tetra reject ok" },
            .{ .value = "inf reject ok" },
            .{ .value = "round trip ok" },
            .{ .value = "bulk array ok" },
        },
    },
    .{
        .name = "http router",
        .path = "./test/misc/http_router.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "root 1" },
            .{ .value = "user 2 42" },
            .{ .value = "post 3 7 99" },
            .{ .value = "absent miss" },
            .{ .value = "verb miss" },
            .{ .value = "extra miss" },
        },
    },
    // Compile-only: a link smoke test that pulls in the HTTP runtime. It emits
    // no output; the assertion is that the artifact links and starts.
    .{
        .name = "http link test",
        .path = "./test/misc/http_link_test.doxa",
        .expected_print = &[_]print_result{},
        .pipelines = &.{.compile},
    },
    .{
        .name = "http loopback",
        .path = "./test/misc/http_loopback.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "primitives ok" },
            .{ .value = "check-status rejected" },
            .{ .value = "404 not found 2 a=1 b=2 text/plain true" },
            .{ .value = "getText not found" },
            .{ .value = "timeout ok" },
            .{ .value = "verbs ok" },
            .{ .value = "redirect credentials ok" },
            .{ .value = "redirect limit ok" },
            .{ .value = "download streaming ok" },
            .{ .value = "download filename ok" },
            .{ .value = "download status ok" },
        },
    },
    .{
        .name = "http server",
        .path = "./test/misc/http_server.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "accepts 1" },
            .{ .value = "keep-alive requests ok" },
            .{ .value = "accepts 3" },
            .{ .value = "multiplex ok" },
            .{ .value = "expect continue ok" },
            .{ .value = "accepts 70" },
            .{ .value = "many connections ok" },
        },
    },
    .{
        .name = "http websocket",
        .path = "./test/misc/http_websocket.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "echo 1" },
            .{ .value = "fragmented 1" },
            .{ .value = "ping 1" },
            .{ .value = "close 1" },
            .{ .value = "served 4" },
            .{ .value = "oversize 1" },
            .{ .value = "oversize served 0" },
            .{ .value = "large frame 1" },
            .{ .value = "large served 2" },
            .{ .value = "large fragment 1" },
            .{ .value = "fragseq served 2" },
            .{ .value = "overflow 1 0" },
            .{ .value = "bad utf8 1 0" },
            .{ .value = "close len1 1 0" },
            .{ .value = "close code 1 0" },
            .{ .value = "close reason 1 0" },
            .{ .value = "partial stalled 1" },
            .{ .value = "partial fast 1" },
            .{ .value = "partial served 4" },
            .{ .value = "sugar echo 1" },
            .{ .value = "sugar fragmented 1" },
            .{ .value = "sugar ping 1" },
            .{ .value = "sugar close 1" },
            .{ .value = "sugar served 4" },
            .{ .value = "plain 426 1 true" },
            .{ .value = "idle closed 1 true" },
            .{ .value = "burst 1 most 1" },
        },
    },
    .{
        .name = "logic",
        .path = "./test/misc/logic.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_logic_results[0..],
    },
    .{
        .name = "standard logic",
        .path = "./test/misc/standard_logic.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_standard_logic_results[0..],
    },
    .{
        .name = "angel",
        .path = "./test/misc/angel.doxa",
        .expected_print = answers.expected_angel_results[0..],
    },
    .{
        .name = "import test",
        .path = "./test/misc/import_test.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_import_test_results[0..],
    },
    .{
        .name = "basic test",
        .path = "./test/misc/basic_test.doxa",
        .expected_print = answers.expected_basic_test_results[0..],
    },
    .{
        .name = "alias test",
        .path = "./test/misc/alias_test.doxa",
        .expected_print = answers.expected_alias_test_results[0..],
    },
    .{
        .name = "fixed array struct field",
        .path = "./test/misc/fixed_field_array.doxa",
        .expected_print = answers.expected_fixed_field_array_results[0..],
    },
    .{
        .name = "list",
        .path = "./test/misc/list.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_list_results[0..],
    },
    .{
        .name = "panic",
        .path = "./test/misc/panic.doxa",
        .mode = .terminate,
        .expect_code = 1,
        .expect_stderr = "panic test message",
    },
    .{
        .name = "exit",
        .path = "./test/misc/exit.doxa",
        .mode = .terminate,
        .expect_code = 7,
    },
    .{
        .name = "assert fail",
        .path = "./test/misc/assert_fail.doxa",
        .mode = .terminate,
        .expect_code = 1,
        .expect_stderr = "assert test message",
    },
    .{
        .name = "unreachable keyword",
        .path = "./test/misc/unreachable.doxa",
        .mode = .terminate,
        .expect_code = 2,
        .expect_stderr = "Reached unreachable code",
    },
};

/// Programs the compiler must reject, each pinned to the code and text of the
/// error it reports.
const rejected_cases = [_]Case{
    .{
        .name = "a map entry store is checked against the map's value type",
        .path = "./test/misc/map_store_type_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "String is not assignable to type Float",
    },
    .{
        .name = "syntax error",
        .path = "./test/syntax/equals_for_assign.doxa",
        .mode = .reject,
        .expect_error = "E2004",
        .expect_stderr = "equals sign '=' is not used for variable declarations",
    },
    .{
        .name = "syntax error in imported module",
        .path = "./test/misc/module_syntax_error.doxa",
        .mode = .reject,
        .expect_error = "E2001",
        .expect_stderr = "expected an expression",
    },
    .{
        .name = "alias argument on by-value parameter",
        .path = "./test/misc/alias_argument_not_needed.doxa",
        .mode = .reject,
        .expect_error = "E1028",
        .expect_stderr = "does not require an alias argument",
    },
    .{
        .name = "alias argument on specifically imported by-value parameter",
        .path = "./test/misc/alias_specific_import.doxa",
        .mode = .reject,
        .expect_error = "E1028",
        .expect_stderr = "does not require an alias argument",
    },
    .{
        .name = "undefined variable",
        .path = "./test/misc/error_test.doxa",
        .mode = .reject,
        .expect_error = "E1001",
        .expect_stderr = "Undefined variable",
    },
    .{
        .name = "const seeded from a const reference stays immutable",
        .path = "./test/syntax/const_reassign_error.doxa",
        .mode = .reject,
        .expect_error = "E1015",
        .expect_stderr = "Cannot assign to immutable variable",
    },
    .{
        .name = "method call with too few arguments",
        .path = "./test/misc/method_too_few_args.doxa",
        .mode = .reject,
        .expect_error = "E5006",
        .expect_stderr = "Too few arguments: expected 2, got 1",
    },
    .{
        .name = "method call argument type mismatch",
        .path = "./test/misc/method_argument_type_mismatch.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "String is not assignable to type Coord",
    },
    .{
        .name = "unknown method on a struct-initialised local",
        .path = "./test/misc/unknown_method_on_copied_struct.doxa",
        .mode = .reject,
        .expect_error = "E1012",
        .expect_stderr = "Unknown method 'nope' on struct 'Counter'",
    },
    .{
        .name = "undefined variable suggestion",
        .path = "./test/misc/undefined_variable_suggestion.doxa",
        .mode = .reject,
        .expect_error = "E1020",
        .expect_stderr = "Did you mean 'total'?",
    },
    .{
        .name = "private field read outside this",
        .path = "./test/syntax/private_field_access_error.doxa",
        .mode = .reject,
        .expect_error = "E6016",
        .expect_stderr = "Cannot access private field 'balance' of struct 'Account'",
    },
    .{
        .name = "private method call outside the struct",
        .path = "./test/syntax/private_method_call_error.doxa",
        .mode = .reject,
        .expect_error = "E6016",
        .expect_stderr = "Cannot call private method 'reset' of struct 'Counter' outside the struct",
    },
    .{
        .name = "private field set by a literal outside the struct",
        .path = "./test/syntax/private_literal_field_error.doxa",
        .mode = .reject,
        .expect_error = "E6016",
        .expect_stderr = "Cannot set private field 'secret' of struct 'Token' outside the struct",
    },
    .{
        .name = "this in a struct function",
        .path = "./test/syntax/this_in_function_error.doxa",
        .mode = .reject,
        .expect_error = "E2030",
        .expect_stderr = "`this` is only available inside a method",
    },
    .{
        .name = "return value without returns",
        .path = "./test/syntax/return_without_returns_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "'half' declares no `returns`, so it cannot return a value",
    },
    .{
        .name = "nested return value of the wrong type",
        .path = "./test/syntax/nested_return_type_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "nested_return_type_error.doxa:5:9: String is not assignable to type Int",
    },
    .{
        .name = "an inner block may not reuse an enclosing local's name",
        .path = "./test/syntax/shadow_local_error.doxa",
        .mode = .reject,
        .expect_error = "E1035",
        .expect_stderr = "shadow_local_error.doxa:5:13: 'x' is already declared where this declaration is visible",
    },
    .{
        .name = "a local may not reuse a parameter's name",
        .path = "./test/syntax/shadow_parameter_error.doxa",
        .mode = .reject,
        .expect_error = "E1035",
        .expect_stderr = "shadow_parameter_error.doxa:4:13: 'n' is already declared where this declaration is visible",
    },
    .{
        .name = "a loop variable may not reuse a global's name",
        .path = "./test/syntax/shadow_loop_variable_error.doxa",
        .mode = .reject,
        .expect_error = "E1035",
        .expect_stderr = "shadow_loop_variable_error.doxa:5:10: 'c' is already declared where this declaration is visible",
    },
    .{
        .name = "a parameter may not reuse a top-level name declared later",
        .path = "./test/syntax/shadow_later_global_error.doxa",
        .mode = .reject,
        .expect_error = "E1035",
        .expect_stderr = "shadow_later_global_error.doxa:3:16: 'b' is already declared where this declaration is visible",
    },
    .{
        .name = "a destructured field may not reuse a visible name",
        .path = "./test/syntax/shadow_destructure_error.doxa",
        .mode = .reject,
        .expect_error = "E1035",
        .expect_stderr = "shadow_destructure_error.doxa:11:17: 'x' is already declared where this declaration is visible",
    },
    .{
        .name = "a struct method may not share a field's name",
        .path = "./test/syntax/struct_method_field_clash_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "struct_method_field_clash_error.doxa:5:19: struct 'Counter' already has a member named 'count'",
    },
    .{
        .name = "struct methods are not overloaded",
        .path = "./test/syntax/struct_duplicate_method_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "struct_duplicate_method_error.doxa:10:19: struct 'Pair' already has a member named 'sum'",
    },
    .{
        .name = "two struct fields may not share a name",
        .path = "./test/syntax/struct_duplicate_field_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "struct_duplicate_field_error.doxa:4:12: struct 'Point' already has a member named 'x'",
    },
    .{
        .name = "a member-preserving union alias parameter accepts one member's storage",
        .path = "./test/misc/alias_member_loan.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = "string", .value = "\"hi!\"" },
            .{ .type = "int", .value = "42" },
            .{ .type = ">int | string", .value = "0" },
        },
    },
    .{
        .name = "a union alias parameter that changes member rejects one member's storage",
        .path = "./test/syntax/alias_member_change_error.doxa",
        .mode = .reject,
        .expect_error = "E1025",
        .expect_stderr = "alias_member_change_error.doxa:8:8: An alias argument lends its storage",
    },
    .{
        .name = "lending a union alias parameter on to one that changes member changes it",
        .path = "./test/syntax/alias_member_change_relay_error.doxa",
        .mode = .reject,
        .expect_error = "E1025",
        .expect_stderr = "alias_member_change_relay_error.doxa:12:8: An alias argument lends its storage",
    },
    .{
        .name = "narrowing a union alias parameter to a smaller union does not pin its member",
        .path = "./test/syntax/alias_member_subunion_error.doxa",
        .mode = .reject,
        .expect_error = "E1025",
        .expect_stderr = "alias_member_subunion_error.doxa:12:9: An alias argument lends its storage",
    },
    .{
        .name = "a member-typed variable may not be lent through a function value",
        .path = "./test/syntax/alias_member_function_value_error.doxa",
        .mode = .reject,
        .expect_error = "E1025",
        .expect_stderr = "alias_member_function_value_error.doxa:11:4: An alias argument lends its storage, so its type must be exactly the parameter's: this call's body is not known",
    },
    .{
        .name = "a builtin cannot reach an un-narrowed union alias parameter",
        .path = "./test/syntax/alias_union_builtin_error.doxa",
        .mode = .reject,
        .expect_error = "E6001",
        .expect_stderr = "alias_union_builtin_error.doxa:3:11: @push requires array or string, got Union",
    },
    .{
        .name = "an empty array literal takes its element type from its context",
        .path = "./test/misc/empty_literal_context.doxa",
        .mode = .peek,
        .expected_peek = &[_]peek_result{
            .{ .type = "int[][]", .value = "[[], [1]]" },
            .{ .type = "int[][][]", .value = "[[[]], [[2]]]" },
            .{ .type = "int", .value = "0" },
            .{ .type = "string", .value = "\"\"" },
        },
    },
    .{
        .name = "a nested empty array literal with no context is rejected",
        .path = "./test/syntax/nested_empty_array_error.doxa",
        .mode = .reject,
        .expect_error = "E6019",
        .expect_stderr = "nested_empty_array_error.doxa:2:5: cannot infer element type of empty array literal",
    },
    .{
        .name = "an array literal of a group's members is typed by its context",
        .path = "./test/misc/group_array_literal.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "io parse" },
            .{ .value = "parse io 2" },
            .{ .value = "parse 2 parse parse" },
        },
    },
    .{
        .name = "an initializer the compiler cannot know is not a constant",
        .path = "./test/syntax/constant_unknown_operands.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "7 233 100" },
        },
    },
    .{
        .name = "constant expressions fold with the program's arithmetic",
        .path = "./test/syntax/constant_expressions.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "-4 -1 1024 6" },
            .{ .value = "0x2C" },
            .{ .value = "true true both" },
        },
    },
    .{
        .name = "an int overflow in a constant expression is rejected",
        .path = "./test/syntax/constant_overflow_error.doxa",
        .mode = .reject,
        .expect_error = "E1024",
        .expect_stderr = "constant_overflow_error.doxa:2:34: integer overflow in a constant expression",
    },
    .{
        .name = "a division by a constant zero is rejected",
        .path = "./test/syntax/constant_division_by_zero_error.doxa",
        .mode = .reject,
        .expect_error = "E1036",
        .expect_stderr = "constant_division_by_zero_error.doxa:4:16: division by zero in a constant expression",
    },
    .{
        .name = "a constant @int of a float with no int value is rejected",
        .path = "./test/syntax/constant_int_conversion_error.doxa",
        .mode = .reject,
        .expect_error = "E1032",
        .expect_stderr = "@int of a float with no int value",
    },
    .{
        .name = "a constant conversion of text that names no number is rejected",
        .path = "./test/syntax/constant_unparsable_error.doxa",
        .mode = .reject,
        .expect_error = "E1037",
        .expect_stderr = "constant_unparsable_error.doxa:2:15: the text names no number",
    },
    .{
        .name = "a conversion of text that names no number traps",
        .path = "./test/syntax/unparsable_conversion_trap.doxa",
        .mode = .terminate,
        .expect_code = 1,
        .expect_stderr = "@int: \"abc1\" is not a valid int",
    },
    .{
        .name = "an array inferred from int literals is an int[], which @pack rejects",
        .path = "./test/syntax/pack_inferred_int_array_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "pack_inferred_int_array_error.doxa:2:18: int is not implicitly assignable to byte",
    },
    .{
        .name = "a byte literal must fit a byte",
        .path = "./test/syntax/byte_literal_range_error.doxa",
        .mode = .reject,
        .expect_error = "E1031",
        .expect_stderr = "byte value out of range (must be 0-255)",
    },
    .{
        .name = "a function declared inside a body is rejected",
        .path = "./test/syntax/nested_function_error.doxa",
        .mode = .reject,
        .expect_error = "E2029",
        .expect_stderr = "nested_function_error.doxa:7:14: Function 'helper' must be declared at file scope",
    },
    .{
        .name = "two imports of one symbol name bind it twice",
        .path = "./test/syntax/duplicate_import_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "duplicate_import_error.doxa:4:8: Duplicate binding name 'collide'",
    },
    .{
        .name = "a top-level declaration may not reuse an imported name",
        .path = "./test/syntax/import_declaration_clash_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "import_declaration_clash_error.doxa:4:10: Duplicate binding name 'collide'",
    },
    .{
        .name = "two module aliases may not share a name",
        .path = "./test/syntax/duplicate_module_alias_error.doxa",
        .mode = .reject,
        .expect_error = "E1002",
        .expect_stderr = "duplicate_module_alias_error.doxa:3:8: Duplicate binding name 'm'",
    },
    .{
        .name = "imported function returns the wrong type",
        .path = "./test/syntax/imported_return_type_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "bad_return_mod.doxa:2:5: String is not assignable to type Int",
    },
    .{
        .name = "enum variant shorthand without a context",
        .path = "./test/syntax/enum_variant_without_context_error.doxa",
        .mode = .reject,
        .expect_error = "E2025",
        .expect_stderr = "enum_variant_without_context_error.doxa:6:13: '.Fail' needs its enum from context",
    },
    .{
        .name = "enum variant shorthand its enum does not declare",
        .path = "./test/syntax/enum_variant_undeclared_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "'Status' has no variant 'Nope'",
    },
    .{
        .name = "a missing namespace member names the module by its stable key",
        .path = "./test/syntax/namespace_member_missing_error.doxa",
        .mode = .reject,
        .expect_error = "E6009",
        .expect_stderr = "'std//std.doxa' has no public declaration 'nope'",
    },
    .{
        .name = "a built-in type has no un-prefixed methods",
        .path = "./test/syntax/builtin_method_without_at_error.doxa",
        .mode = .reject,
        .expect_error = "E1012",
        .expect_stderr = "An array has no method 'push'; compiler methods are `@`-prefixed",
    },
    .{
        .name = "a lazy module's syntax error is reported after an unrelated error",
        .path = "./test/syntax/lazy_module_parse_error_after_error.doxa",
        .mode = .reject,
        .expect_error = "E2001",
        .expect_stderr = "broken_syntax.doxa:2:1: ExpectedExpression",
    },
    .{
        .name = "an import cycle is reported at the import that closes it",
        .path = "./test/misc/lazy/icycle_a.doxa",
        .mode = .reject,
        .expect_error = "E7002",
        .expect_stderr = "icycle_b.doxa:1:8: Circular import detected",
    },
    .{
        .name = "same-named types from two modules are distinct",
        .path = "./test/syntax/module_type_identity_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Node (from inc0//test/misc/moddata/node_mod_a.doxa) is not assignable to type Node (from inc0//test/misc/moddata/node_mod_b.doxa)",
    },
    .{
        .name = "integer division rejects a float operand",
        .path = "./test/syntax/float_integer_division_error.doxa",
        .mode = .reject,
        .expect_error = "E1006",
        .expect_stderr = "Integer division requires integer or byte operands",
    },
    .{
        .name = "modulo rejects a float operand",
        .path = "./test/syntax/float_modulo_error.doxa",
        .mode = .reject,
        .expect_error = "E1006",
        .expect_stderr = "Modulo requires integer or byte operands",
    },
    .{
        .name = "integer division assignment rejects a float",
        .path = "./test/syntax/float_floor_assign_error.doxa",
        .mode = .reject,
        .expect_error = "E1006",
        .expect_stderr = "Integer division requires integer or byte operands",
    },
    .{
        .name = "float division assignment into an int",
        .path = "./test/syntax/float_divide_assign_int_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Float is not assignable to type Int",
    },
    .{
        .name = "a runtime int does not widen to float",
        .path = "./test/syntax/float_runtime_widen_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "int is not implicitly assignable to float; use @float() to widen",
    },
    .{
        .name = "a named int const does not widen to a float argument",
        .path = "./test/syntax/float_named_const_arg_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "int is not implicitly assignable to float; use @float() to widen",
    },
    .{
        .name = "a bare return needs nothing in the declared return type",
        .path = "./test/syntax/bare_return_without_nothing_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "'fallible' returns Err, so a bare `return` has no value to give; declare `returns nothing | Err` to return nothing",
    },
    .{
        .name = "a value that can be nothing is not returned through a type that cannot",
        .path = "./test/syntax/forwarded_nothing_return_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "this return can be `nothing`, which Err does not admit; declare `returns nothing | Err`",
    },
    .{
        .name = "+ between a number and a string is rejected",
        .path = "./test/syntax/plus_mixed_operands_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Cannot use + operator between Int and String",
    },
    .{
        .name = "a compound assignment on an un-narrowed union is rejected",
        .path = "./test/syntax/compound_union_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Cannot use + operator on union type",
    },
    .{
        .name = "a byte[] literal element must fit a byte",
        .path = "./test/syntax/byte_array_element_range_error.doxa",
        .mode = .reject,
        .expect_error = "E1031",
        .expect_stderr = "byte value out of range (must be 0-255)",
    },
    .{
        .name = "a parameter default is typed against its parameter",
        .path = "./test/syntax/default_argument_type_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "String is not assignable to type Int",
    },
    .{
        .name = "a float does not narrow to int",
        .path = "./test/syntax/float_narrow_decl_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Float is not assignable to type Int",
    },
    .{
        .name = "an int literal without an exact float value does not widen",
        .path = "./test/syntax/float_inexact_literal_error.doxa",
        .mode = .reject,
        .expect_error = "E1034",
        .expect_stderr = "int literal 9007199254740993 has no exact float value (the nearest float is 9007199254740992.0)",
    },
    .{
        .name = "as fallback type mismatch",
        .path = "./test/syntax/as_fallback_type_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Fallback for 'as' must produce type float, got int",
    },
    .{
        .name = "fixed array push requires dynamic storage",
        .path = "./test/syntax/fixed_array_push_error.doxa",
        .mode = .reject,
        .expect_error = "E6018",
        .expect_stderr = "cannot be used with @push",
    },
    .{
        .name = "const literal array push requires dynamic storage",
        .path = "./test/syntax/const_literal_push_error.doxa",
        .mode = .reject,
        .expect_error = "E6018",
        .expect_stderr = "cannot be used with @push",
    },
    .{
        .name = "nested const literal array push requires dynamic storage",
        .path = "./test/syntax/nested_const_literal_push_error.doxa",
        .mode = .reject,
        .expect_error = "E6018",
        .expect_stderr = "cannot be used with @push",
    },
    .{
        .name = "const alias array push requires dynamic storage",
        .path = "./test/syntax/const_alias_push_error.doxa",
        .mode = .reject,
        .expect_error = "E6018",
        .expect_stderr = "cannot be used with @push",
    },
    .{
        .name = "struct literal undeclared field",
        .path = "./test/misc/struct_literal_bad_field.doxa",
        .mode = .reject,
        .expect_error = "E1011",
        .expect_stderr = "struct 'Person' has no field 'kind'; declared fields: name, age",
    },
    .{
        .name = "struct literal undeclared field in imported module",
        .path = "./test/misc/module_bad_struct_import.doxa",
        .mode = .reject,
        .expect_error = "E1011",
        .expect_stderr = "struct 'Item' has no field 'kind'; declared fields: name, count, tags",
    },
    .{
        .name = "group match not exhaustive",
        .path = "./test/syntax/group_non_exhaustive.doxa",
        .mode = .reject,
        .expect_error = "E1033",
        .expect_stderr = "Match on group 'Palette' is not exhaustive: 'FileError' not covered",
    },
    .{
        .name = "group cycle",
        .path = "./test/syntax/group_cycle.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Cycle detected in group: 'A' includes itself transitively",
    },
    .{
        .name = "union arithmetic must be narrowed first",
        .path = "./test/misc/union_arith_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "Cannot use + operator on union type; narrow it with 'as' or match first",
    },
    .{
        .name = "@pack requires byte[]",
        .path = "./test/syntax/pack_requires_byte_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "is not implicitly assignable to byte",
    },
    .{
        .name = "@push on string requires string value",
        .path = "./test/syntax/push_string_value_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "@push on string requires string value",
    },
    .{
        .name = "@insert on string requires string value",
        .path = "./test/syntax/insert_string_value_error.doxa",
        .mode = .reject,
        .expect_error = "E1003",
        .expect_stderr = "@insert on string requires string value",
    },
};

/// Each calculator interaction is an independent stdin/stdout case against the
/// same program, so it folds into the table rather than needing a bespoke
/// batch runner in both suites.
const calculator_cases = blk: {
    var built: [answers.calculator_io_tests.len]Case = undefined;
    for (answers.calculator_io_tests, 0..) |io, index| {
        built[index] = .{
            .name = std.fmt.comptimePrint("calculator {d}", .{index + 1}),
            .path = "./test/examples/calculator.doxa",
            .input = io.input,
            .expected_print = &.{.{ .value = io.expected_output }},
        };
    }
    break :blk built;
};

pub const cases = shared_cases ++ calculator_cases ++ rejected_cases;

/// For each case, the position of its program among the table's distinct
/// programs, in table order. The suites schedule by it, so a program reaches
/// the same worker, and that worker's warm cache, on every run.
pub const program_of = blk: {
    @setEvalBranchQuota(cases.len * cases.len * 16);
    var ordinals: [cases.len]usize = undefined;
    var distinct: usize = 0;
    for (cases, 0..) |case, index| {
        ordinals[index] = for (cases[0..index], 0..) |earlier, earlier_index| {
            if (std.mem.eql(u8, earlier.path, case.path)) break ordinals[earlier_index];
        } else fresh: {
            distinct += 1;
            break :fresh distinct - 1;
        };
    }
    break :blk ordinals;
};

comptime {
    // The pairwise scan is quadratic in the case count; size the quota from it
    // so adding a case never trips a fixed limit.
    @setEvalBranchQuota(cases.len * cases.len * 16);
    for (cases) |case| {
        if (case.contradiction()) |why| @compileError(std.fmt.comptimePrint("case '{s}': {s}", .{ case.name, why }));
    }
    // Compiled artifacts are keyed by source stem, so a collision would make
    // one program's binary shadow another's. Fail the build instead.
    var stems: [cases.len][]const u8 = undefined;
    for (cases, 0..) |case, index| stems[index] = sourceStem(case.path);
    for (stems, 0..) |stem, index| {
        for (stems[index + 1 ..], index + 1..) |other, other_index| {
            if (std.mem.eql(u8, stem, other) and
                !std.mem.eql(u8, cases[index].path, cases[other_index].path))
            {
                @compileError(std.fmt.comptimePrint(
                    "duplicate source stem: {s} and {s}",
                    .{ cases[index].path, cases[other_index].path },
                ));
            }
        }
    }
}
