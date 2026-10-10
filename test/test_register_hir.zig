//! The register HIR's builder, textual form and verifier, on functions built
//! by hand (`plan/register-hir.md`). Each well-formed function is pinned by
//! its exact text; each verifier rule by a function that breaks it.

const std = @import("std");
const testing = std.testing;

const ir = @import("../src/codegen/hir/register/ir.zig");
const builder_mod = @import("../src/codegen/hir/register/builder.zig");
const print = @import("../src/codegen/hir/register/print.zig");
const verify = @import("../src/codegen/hir/register/verify.zig");

const FunctionBuilder = builder_mod.FunctionBuilder;
const Type = ir.Type;

const no_program = ir.Program{
    .functions = &.{},
    .function_names = &.{},
    .zig_functions = &.{},
    .zig_function_names = &.{},
    .globals = &.{},
    .struct_fields = &.{},
    .group_members = &.{},
};

fn text(alloc: std.mem.Allocator, program: *const ir.Program, f: *const ir.Function) ![]const u8 {
    var out = std.Io.Writer.Allocating.init(alloc);
    try print.writeFunction(&out.writer, program, f);
    return out.written();
}

fn expectValid(alloc: std.mem.Allocator, program: *const ir.Program, f: *const ir.Function) !void {
    if (try verify.verify(alloc, program, f)) |fault| {
        var out = std.Io.Writer.Allocating.init(alloc);
        try verify.writeFault(&out.writer, program, f, fault);
        std.debug.print("{s}", .{out.written()});
        return error.UnexpectedFault;
    }
}

fn expectFault(alloc: std.mem.Allocator, program: *const ir.Program, f: *const ir.Function, needle: []const u8) !void {
    const fault = (try verify.verify(alloc, program, f)) orelse return error.ExpectedFault;
    if (std.mem.indexOf(u8, fault.message, needle) == null) {
        std.debug.print("fault: {s}\nexpected it to contain: {s}\n", .{ fault.message, needle });
        return error.WrongFault;
    }
}

const int: Type = .of(.Int);
const string: Type = .of(.String);

test "register hir: straight-line code" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "add", .Int);
    const x = try b.addParam(int, .value);
    const y = try b.addParam(int, .value);
    const sum = try b.define(.{ .arith = .{ .op = .Add, .lhs = x, .rhs = y } }, int);
    try b.ret(sum);
    const f = try b.finish();

    try expectValid(a, &no_program, &f);
    try testing.expectEqualStrings(
        \\fn add(%0: int, %1: int) -> int
        \\b0(%0: int, %1: int):
        \\    %2 = arith.Add %0, %1
        \\    return %2
        \\
    , try text(a, &no_program, &f));
}

test "register hir: an if value is a block parameter" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "max", .Int);
    const x = try b.addParam(int, .value);
    const y = try b.addParam(int, .value);
    const then = try b.createBlock();
    const @"else" = try b.createBlock();
    const join = try b.createBlock();
    const result = try b.blockParam(join, int);

    const greater = try b.define(.{ .cmp = .{ .op = .Gt, .lhs = x, .rhs = y } }, .cond);
    try b.branch(greater, .{ .block = then }, .{ .block = @"else" });
    try b.seal(then);
    try b.seal(@"else");
    b.switchTo(then);
    try b.jump(.{ .block = join, .args = &.{x} });
    b.switchTo(@"else");
    try b.jump(.{ .block = join, .args = &.{y} });
    try b.seal(join);
    b.switchTo(join);
    try b.ret(result);
    const f = try b.finish();

    try expectValid(a, &no_program, &f);
    try testing.expectEqualStrings(
        \\fn max(%0: int, %1: int) -> int
        \\b0(%0: int, %1: int):
        \\    %2 = cmp.Gt %0, %1
        \\    branch %2, b1, b2
        \\b1:
        \\    jump b3(%0)
        \\b2:
        \\    jump b3(%1)
        \\b3(%3: int):
        \\    return %3
        \\
    , try text(a, &no_program, &f));
}

/// `sum(n)`: `i` and `acc` start at 0; while `i < n`, `acc += i` and `i++`;
/// `k` is written once before the loop and read in it.
fn buildSum(a: std.mem.Allocator, with_invariant: bool) !ir.Function {
    var b = try FunctionBuilder.init(a, "sum", .Int);
    const n = try b.addParam(int, .value);
    const i = try b.declareVar(int);
    const acc = try b.declareVar(int);
    const k = try b.declareVar(int);
    const header = try b.createBlock();
    const body = try b.createBlock();
    const exit = try b.createBlock();

    const zero = try b.define(.{ .constant = .{ .int = 0 } }, int);
    try b.defVar(i, zero);
    try b.defVar(acc, zero);
    if (with_invariant) try b.defVar(k, n);
    try b.jump(.{ .block = header });

    b.switchTo(header);
    const keep_going = try b.define(.{ .cmp = .{ .op = .Lt, .lhs = try b.useVar(i), .rhs = n } }, .cond);
    try b.branch(keep_going, .{ .block = body }, .{ .block = exit });
    try b.seal(body);
    try b.seal(exit);

    b.switchTo(body);
    const step = if (with_invariant) try b.useVar(k) else try b.useVar(i);
    try b.defVar(acc, try b.define(.{ .arith = .{ .op = .Add, .lhs = try b.useVar(acc), .rhs = step } }, int));
    const one = try b.define(.{ .constant = .{ .int = 1 } }, int);
    try b.defVar(i, try b.define(.{ .arith = .{ .op = .Add, .lhs = try b.useVar(i), .rhs = one } }, int));
    try b.jump(.{ .block = header });
    try b.seal(header);

    b.switchTo(exit);
    try b.ret(try b.useVar(acc));
    return b.finish();
}

test "register hir: loop-carried locals become header parameters" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const f = try buildSum(a, false);
    try expectValid(a, &no_program, &f);
    try testing.expectEqualStrings(
        \\fn sum(%0: int) -> int
        \\b0(%0: int):
        \\    %1 = const int 0
        \\    jump b1(%1, %1)
        \\b1(%2: int, %3: int):
        \\    %4 = cmp.Lt %2, %0
        \\    branch %4, b2, b3
        \\b2:
        \\    %5 = arith.Add %3, %2
        \\    %6 = const int 1
        \\    %7 = arith.Add %2, %6
        \\    jump b1(%7, %5)
        \\b3:
        \\    return %3
        \\
    , try text(a, &no_program, &f));
}

test "register hir: a local the loop never writes gets no parameter" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    // `k` is read in the body before the header is sealed, so the builder
    // adds a parameter for it; both edges pass `%0`, so `finish` removes it.
    const f = try buildSum(a, true);
    try expectValid(a, &no_program, &f);
    try testing.expectEqualStrings(
        \\fn sum(%0: int) -> int
        \\b0(%0: int):
        \\    %1 = const int 0
        \\    jump b1(%1, %1)
        \\b1(%2: int, %3: int):
        \\    %4 = cmp.Lt %2, %0
        \\    branch %4, b2, b3
        \\b2:
        \\    %5 = arith.Add %3, %0
        \\    %6 = const int 1
        \\    %7 = arith.Add %2, %6
        \\    jump b1(%7, %5)
        \\b3:
        \\    return %3
        \\
    , try text(a, &no_program, &f));
}

test "register hir: code after a return is dropped" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "early", .Int);
    const x = try b.addParam(int, .value);
    const dead = try b.createBlock();
    try b.ret(x);
    try b.seal(dead);
    b.switchTo(dead);
    try testing.expect(b.isDead(dead));
    try testing.expectError(error.DeadBlock, b.ret(x));
    const f = try b.finish();
    try testing.expectEqual(@as(usize, 1), f.blocks.len);
}

test "register hir: reading a local never written is an error" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "undefined_read", .Int);
    const v = try b.declareVar(int);
    try testing.expectError(error.UndefinedVariable, b.useVar(v));
}

// ── Verifier rules ──

test "register hir verifier: operand types" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "mixed", .Int);
    const x = try b.addParam(int, .value);
    const y = try b.addParam(.of(.Float), .value);
    const sum = try b.define(.{ .arith = .{ .op = .Add, .lhs = x, .rhs = y } }, int);
    try b.ret(sum);
    const f = try b.finish();
    try expectFault(a, &no_program, &f, "wrong type");
}

test "register hir verifier: a definition dominates every use" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var b = try FunctionBuilder.init(a, "crossed", .Int);
    const x = try b.addParam(int, .value);
    const then = try b.createBlock();
    const @"else" = try b.createBlock();
    const cond = try b.define(.{ .cmp = .{ .op = .Gt, .lhs = x, .rhs = x } }, .cond);
    try b.branch(cond, .{ .block = then }, .{ .block = @"else" });
    try b.seal(then);
    try b.seal(@"else");
    b.switchTo(then);
    const doubled = try b.define(.{ .arith = .{ .op = .Add, .lhs = x, .rhs = x } }, int);
    try b.ret(doubled);
    b.switchTo(@"else");
    try b.ret(doubled);
    const f = try b.finish();
    try expectFault(a, &no_program, &f, "does not dominate");
}

/// A function taking `@caller` that opens its body arena under it.
const ArenaFixture = struct {
    b: FunctionBuilder,
    caller: ir.ValueId,
    body: ir.ValueId,

    fn init(a: std.mem.Allocator, name: []const u8, ret: ir.HIRType) !ArenaFixture {
        var b = try FunctionBuilder.init(a, name, ret);
        const caller = try b.addParam(.arena, .caller_arena);
        const body = try b.define(.{ .scope_enter = .{ .parent = caller } }, .arena);
        return .{ .b = b, .caller = caller, .body = body };
    }

    fn greeting(self: *ArenaFixture, arena: ir.ValueId) !ir.ValueId {
        const n = try self.b.define(.{ .constant = .{ .int = 7 } }, int);
        return self.b.define(.{ .to_string = .{ .arena = arena, .operand = n } }, string);
    }
};

test "register hir verifier: scopes are closed before a return" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var fx = try ArenaFixture.init(a, "leaky", .Nothing);
    try fx.b.ret(null);
    const f = try fx.b.finish();
    try expectFault(a, &no_program, &f, "still open");
}

test "register hir verifier: nothing is used after its arena closes" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    var fx = try ArenaFixture.init(a, "dangling", .Nothing);
    const s = try fx.greeting(fx.body);
    try fx.b.effect(.{ .scope_exit = .{ .arena = fx.body } });
    try fx.b.effect(.{ .print = .{ .operand = s } });
    try fx.b.ret(null);
    const f = try fx.b.finish();
    try expectFault(a, &no_program, &f, "is closed");
}

test "register hir verifier: a returned heap value outlives @caller" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    {
        var fx = try ArenaFixture.init(a, "escapes", .String);
        const s = try fx.greeting(fx.body);
        try fx.b.effect(.{ .scope_exit = .{ .arena = fx.body } });
        try fx.b.ret(s);
        const f = try fx.b.finish();
        // Closing the body arena already kills `s`; the use at the return
        // is what reports it.
        try expectFault(a, &no_program, &f, "is closed");
    }
    {
        var fx = try ArenaFixture.init(a, "placed", .String);
        const s = try fx.greeting(fx.caller);
        try fx.b.effect(.{ .scope_exit = .{ .arena = fx.body } });
        try fx.b.ret(s);
        const f = try fx.b.finish();
        try expectValid(a, &no_program, &f);
    }
    {
        var b = try FunctionBuilder.init(a, "no_caller", .String);
        const s = try b.define(.{ .constant = .{ .string = "hi" } }, string);
        try b.ret(s);
        const f = try b.finish();
        try expectFault(a, &no_program, &f, "takes no @caller arena");
    }
}

test "register hir verifier: a slot outlives what is stored in it" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    for ([_]bool{ false, true }) |rehomed| {
        var fx = try ArenaFixture.init(a, "kept", .Nothing);
        // The slot belongs to the body arena; the value to an inner scope.
        const slot = try fx.b.addSlot(.String, fx.body, "kept");
        const inner = try fx.b.define(.{ .scope_enter = .{ .parent = fx.body } }, .arena);
        var s = try fx.greeting(inner);
        if (rehomed) s = try fx.b.define(.{ .rehome = .{ .arena = fx.body, .operand = s } }, string);
        const ref = try fx.b.define(.{ .slot_addr = slot }, .{ .ref = .String });
        try fx.b.effect(.{ .store = .{ .ref = ref, .value = s } });
        try fx.b.effect(.{ .scope_exit = .{ .arena = inner } });
        try fx.b.effect(.{ .scope_exit = .{ .arena = fx.body } });
        try fx.b.ret(null);
        const f = try fx.b.finish();
        if (rehomed) {
            try expectValid(a, &no_program, &f);
        } else {
            try expectFault(a, &no_program, &f, "rehome or clone");
        }
    }
}

test "register hir verifier: a loop arena's values do not survive its reset" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    for ([_]bool{ false, true }) |carried| {
        var fx = try ArenaFixture.init(a, "loop", .Nothing);
        const b = &fx.b;
        const header = try b.createBlock();
        const body = try b.createBlock();
        const done = try b.createBlock();
        const loop_arena = try b.blockParam(header, .arena);
        const last = try b.blockParam(header, string);

        const first = try b.define(.{ .scope_enter = .{ .parent = fx.body } }, .arena);
        const seed = try b.define(.{ .constant = .{ .string = "" } }, string);
        try b.jump(.{ .block = header, .args = &.{ first, seed } });

        b.switchTo(header);
        const go = try b.define(.{ .constant = .{ .tetra = .true } }, .of(.Tetra));
        try b.branch(try b.define(.{ .tetra_holds = .{ .operand = go } }, .cond), .{ .block = body }, .{ .block = done });
        try b.seal(body);
        try b.seal(done);

        b.switchTo(body);
        const n = try b.define(.{ .constant = .{ .int = 1 } }, int);
        const s = try b.define(.{ .to_string = .{ .arena = loop_arena, .operand = n } }, string);
        const next = try b.define(.{ .scope_reset = .{ .arena = loop_arena } }, .arena);
        // Carrying `s` into the next iteration uses it after the reset.
        try b.jump(.{ .block = header, .args = &.{ next, if (carried) s else seed } });
        try b.seal(header);

        b.switchTo(done);
        try b.effect(.{ .print = .{ .operand = last } });
        try b.effect(.{ .scope_exit = .{ .arena = loop_arena } });
        try b.effect(.{ .scope_exit = .{ .arena = fx.body } });
        try b.ret(null);
        const f = try b.finish();
        if (carried) {
            try expectFault(a, &no_program, &f, "is closed");
        } else {
            try expectValid(a, &no_program, &f);
        }
    }
}

test "register hir verifier: one slot is never lent twice to one call" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();

    const swap = ir.Signature{
        .params = &.{ .{ .ref = .Int }, .arena, .{ .ref = .Int }, .arena },
        .roles = &.{ .{ .alias = .{ .arena = 1 } }, .alias_arena, .{ .alias = .{ .arena = 3 } }, .alias_arena },
        .ret = .Nothing,
    };
    var program = no_program;
    program.functions = &.{swap};
    program.function_names = &.{"swap"};

    for ([_]bool{ false, true }) |same| {
        var fx = try ArenaFixture.init(a, "caller", .Nothing);
        const left = try fx.b.addSlot(.Int, fx.body, "left");
        const right = try fx.b.addSlot(.Int, fx.body, "right");
        const l = try fx.b.define(.{ .slot_addr = left }, .{ .ref = .Int });
        const r = try fx.b.define(.{ .slot_addr = if (same) left else right }, .{ .ref = .Int });
        try fx.b.effect(.{ .call = .{ .callee = @enumFromInt(0), .args = &.{ l, fx.body, r, fx.body } } });
        try fx.b.effect(.{ .scope_exit = .{ .arena = fx.body } });
        try fx.b.ret(null);
        const f = try fx.b.finish();
        if (same) {
            try expectFault(a, &program, &f, "two ^ parameters");
        } else {
            try expectValid(a, &program, &f);
        }
    }
}
