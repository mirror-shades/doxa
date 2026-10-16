//! The borrow `each n in ^path` takes (`docs/loops.md`, plan/plain-store-copies.md
//! step 3). The body may change the elements through `n`, so nothing else in
//! it may write or lend the path, a prefix of it, or anything under it.
//!
//! One lexical walk: analysis visits every write and loan in the body while
//! the borrow is live, and each is compared with it by path. Comparing
//! spellings is complete because plain stores copy (two names never share
//! storage) and a visible name is never redeclared (E1035), so within the
//! body a name has one binding.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const ErrorCode = @import("../../utils/errors.zig").ErrorCode;

/// A live `^` borrow of an `each` loop.
pub const Borrow = struct {
    /// The borrowed path, root first: `game.players` is `{game, players}`.
    path: []const []const u8,
    source: *ast.Expr,
};

/// What a write or loan does, for its diagnostic.
pub const Access = enum {
    write,
    resize,
    lend,
    borrow,

    fn verb(self: Access) []const u8 {
        return switch (self) {
            .write => "writes",
            .resize => "resizes",
            .lend => "lends",
            .borrow => "borrows",
        };
    }
};

/// Rule 1: the source of a borrow is a variable, or `this`, followed by
/// struct fields only. Null for any other expression.
pub fn sourcePath(alloc: std.mem.Allocator, e: *ast.Expr) !?[]const []const u8 {
    var segments: std.ArrayListUnmanaged([]const u8) = .empty;
    var at = e;
    while (true) switch (at.data) {
        .FieldAccess => |fa| {
            try segments.append(alloc, fa.field.lexeme);
            at = fa.object;
        },
        .Variable => |name| {
            try segments.append(alloc, name.lexeme);
            break;
        },
        .This => {
            try segments.append(alloc, "this");
            break;
        },
        else => {
            segments.deinit(alloc);
            return null;
        },
    };
    std.mem.reverse([]const u8, segments.items);
    return segments.items;
}

/// The storage a write to `target` reaches, as a path, root first: the
/// fields of `target` up to its first index (a write to an element is a write
/// under the array). Null for a target no binding holds: a call's result is
/// a fresh value.
fn writtenPath(alloc: std.mem.Allocator, target: *ast.Expr) !?[]const []const u8 {
    var segments: std.ArrayListUnmanaged([]const u8) = .empty;
    var at = target;
    while (true) switch (at.data) {
        .FieldAccess => |fa| {
            try segments.append(alloc, fa.field.lexeme);
            at = fa.object;
        },
        .Index => |idx| {
            segments.clearRetainingCapacity();
            at = idx.array;
        },
        .Grouping => |g| at = g orelse break,
        .Variable => |name| {
            try segments.append(alloc, name.lexeme);
            std.mem.reverse([]const u8, segments.items);
            return segments.items;
        },
        .This => {
            try segments.append(alloc, "this");
            std.mem.reverse([]const u8, segments.items);
            return segments.items;
        },
        else => break,
    };
    segments.deinit(alloc);
    return null;
}

/// Whether one path is a prefix of the other: the same storage, or one
/// holding the other.
fn overlaps(a: []const []const u8, b: []const []const u8) bool {
    for (a[0..@min(a.len, b.len)], b[0..@min(a.len, b.len)]) |x, y| {
        if (!std.mem.eql(u8, x, y)) return false;
    }
    return true;
}

fn render(alloc: std.mem.Allocator, path: []const []const u8) ![]const u8 {
    return std.mem.join(alloc, ".", path);
}

/// Rule 2: `access` to `target` (at `at`) overlaps no live borrow.
pub fn check(self: *SemanticAnalyzer, target: *ast.Expr, at: *ast.Expr, access: Access) !void {
    if (self.borrows.items.len == 0) return;
    const path = try writtenPath(self.allocator, target) orelse return;
    defer self.allocator.free(path);
    try checkPath(self, path, at, access);
}

/// Rule 2 for a path already known: a nested borrow's.
pub fn checkPath(self: *SemanticAnalyzer, path: []const []const u8, at: *ast.Expr, access: Access) !void {
    for (self.borrows.items) |borrow| {
        if (!overlaps(path, borrow.path)) continue;
        const written = try render(self.allocator, path);
        defer self.allocator.free(written);
        const borrowed = try render(self.allocator, borrow.path);
        defer self.allocator.free(borrowed);
        self.reporter.reportCompileError(
            at.base.location(),
            ErrorCode.BORROW_CONFLICT,
            "this {s} '{s}', which an enclosing 'each' borrows as '^{s}': change the elements through the loop's binding",
            .{ access.verb(), written, borrowed },
        );
        self.fatal_error = true;
        return;
    }
}
