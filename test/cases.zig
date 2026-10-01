const std = @import("std");
const answers = @import("answers");

pub const print_result = answers.print_result;
pub const peek_result = answers.peek_result;

/// Which execution pipeline a case is exercised through. Most cases run under
/// both `doxa run` and `doxa compile`; a case opts out of one only when it
/// tests something pipeline-specific, and that opt-out is stated here rather
/// than being an accident of whichever suite happened to be edited.
pub const Pipeline = enum { run, compile };

/// How a case's result is validated.
pub const Mode = enum {
    /// Compare stdout against `expected_print`.
    print,
    /// Compare stderr peek output against `expected_peek`.
    peek,
    /// The program must exit non-zero; `expect_code` and `expect_stderr`
    /// optionally narrow that to a specific code and/or stderr substring.
    terminate,
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
    expect_code: ?u8 = null,
    expect_stderr: ?[]const u8 = null,

    pub fn runsOn(self: Case, pipeline: Pipeline) bool {
        for (self.pipelines) |candidate| {
            if (candidate == pipeline) return true;
        }
        return false;
    }

    /// Assertions this case contributes when it cannot be executed, so the
    /// suites can report an accurate `untested` count.
    pub fn expectedCount(self: Case) usize {
        return switch (self.mode) {
            .print => if (self.expected_print) |expected| expected.len else 1,
            .peek => if (self.expected_peek) |expected| expected.len else 1,
            .terminate => 1,
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
        .name = "union narrow",
        .path = "./test/misc/union_narrow.doxa",
        .expected_print = answers.expected_union_narrow_results[0..],
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
        .name = "loop shadow and diverging cast fallback",
        .path = "./test/misc/loop_shadow.doxa",
        .expected_print = &[_]print_result{
            .{ .value = "found" },
            .{ .value = "missing" },
            .{ .value = "hey" },
            .{ .value = "outer" },
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
        .name = "floored arithmetic",
        .path = "./test/misc/floored_arith.doxa",
        .expected_print = answers.expected_floored_arith_results[0..],
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
        .name = "std file list",
        .path = "./test/misc/std_file_list.doxa",
        .expected_print = answers.expected_std_file_list_results[0..],
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
        .name = "list",
        .path = "./test/misc/list.doxa",
        .mode = .peek,
        .expected_peek = answers.expected_list_results[0..],
    },
    .{
        .name = "panic",
        .path = "./test/misc/panic.doxa",
        .mode = .terminate,
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
        .expect_stderr = "assert test message",
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

pub const cases = blk: {
    var all: [shared_cases.len + calculator_cases.len]Case = undefined;
    for (shared_cases, 0..) |case, index| all[index] = case;
    for (calculator_cases, 0..) |case, index| all[shared_cases.len + index] = case;
    break :blk all;
};

comptime {
    @setEvalBranchQuota(20000);
    // Compiled artifacts are keyed by source stem, so a collision would make
    // one program's binary shadow another's. Fail the build instead.
    var stems: [cases.len][]const u8 = undefined;
    for (cases, 0..) |case, index| stems[index] = sourceStem(case.path);
    for (stems, 0..) |stem, index| {
        for (stems[index + 1 ..], index + 1..) |other, other_index| {
            const same_source = std.mem.eql(u8, cases[index].path, cases[other_index].path);
            if (!same_source and std.mem.eql(u8, stem, other)) {
                @compileError(std.fmt.comptimePrint(
                    "duplicate source stem: {s} and {s}",
                    .{ cases[index].path, cases[other_index].path },
                ));
            }
        }
    }
}
