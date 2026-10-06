//! Call-target classification for HIR lowering.
//!
//! Semantic analysis resolved every callee (`resolution.zig`); this module only
//! reads what it recorded. A Doxa function or method is identified by its
//! defining record's `SymbolKey` and called by its link name; a Zig function by
//! its wrapper's link name. Nothing here walks bindings or matches a spelling.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const HIRGenerator = @import("soxa_generator.zig").HIRGenerator;
const CallKind = @import("soxa_types.zig").CallKind;
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;

/// A resolved callee: what the `Call` instruction names and how.
pub const Callee = struct {
    link_name: []const u8,
    kind: CallKind,
    /// Function-table index of a Doxa function; null for a Zig function.
    index: ?u32,
    /// The callee's resolved parameter types, for typing enum-literal
    /// arguments against their parameter.
    params: []const ast.TypeInfo,
};

pub const CallTarget = union(enum) {
    /// A top-level Doxa function or a Zig function.
    function: Callee,
    /// `T.m(...)`: a static method.
    static_method: Callee,
    /// `x.m(...)`: an instance method; the receiver is pushed first.
    method: struct {
        callee: Callee,
        receiver: *ast.Expr,
    },
};

/// Classify a call's callee from its recorded resolution.
pub fn classifyCallTarget(generator: *HIRGenerator, callee: *ast.Expr) !CallTarget {
    const resolution = generator.resolutionOf(callee) orelse return error.UnresolvedCallee;
    return switch (resolution) {
        .function => |symbol| .{ .function = try generator.doxaCallee(.{ .module = symbol.module, .name = symbol.name }) },
        .zig_function => |symbol| .{ .function = try generator.zigCallee(symbol) },
        .static_method => |method| .{ .static_method = try generator.methodCallee(method) },
        .method => |method| .{ .method = .{
            .callee = try generator.methodCallee(method),
            .receiver = callee.data.FieldAccess.object,
        } },
        .namespace, .global, .type => error.UnresolvedCallee,
    };
}

/// Whether a tail call can be emitted for this function-call expression.
pub fn tryEmitTailCall(generator: *HIRGenerator, expr: *ast.Expr) bool {
    const call = expr.data.FunctionCall;
    const target = classifyCallTarget(generator, call.callee) catch return false;
    const callee = switch (target) {
        .function => |callee| callee,
        else => return false,
    };
    const function_index = callee.index orelse return false;

    for (call.arguments) |arg| {
        generator.generateExpression(arg.expr, true, true) catch return false;
    }
    const return_type = generator.typeOf(expr) catch return false;

    generator.instructions.append(HIRInstruction{ .Call = .{
        .function_index = function_index,
        .qualified_name = callee.link_name,
        .arg_count = @intCast(call.arguments.len),
        .call_kind = callee.kind,
        .return_type = return_type,
        .tail = true,
    } }) catch return false;
    return true;
}
