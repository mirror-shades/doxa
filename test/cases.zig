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
        .name = "union narrow",
        .path = "./test/misc/union_narrow.doxa",
        .expected_print = answers.expected_union_narrow_results[0..],
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
        .name = "module string interp",
        .path = "./test/misc/module_string_interp.doxa",
        .expected_print = answers.expected_module_string_interp_results[0..],
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
        .name = "zig import test",
        .path = "./test/misc/zig_import_test.doxa",
        .expected_print = answers.expected_zig_import_test_results[0..],
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
    // Compile-only: a link smoke test that pulls in the HTTP runtime. It emits
    // no output; the assertion is that the artifact links and starts.
    .{
        .name = "http link test",
        .path = "./test/misc/http_link_test.doxa",
        .expected_print = &[_]print_result{},
        .pipelines = &.{.compile},
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
