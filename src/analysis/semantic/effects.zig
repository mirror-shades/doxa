//! What a function's body does to the storage it is handed, decided once by
//! analysis and read by codegen (plan/type-authority.md, Phase E).
//!
//! Two facts:
//!
//! - **Written parameters.** A by-value heap parameter is copied on entry
//!   unless its body never writes through it (`HeapCopyKind.keep`). A body
//!   writes through a parameter, or through its method's receiver, when a
//!   path rooted at it is assigned into (`p.f is …`, `p[i] is …`), handed to
//!   an in-place intrinsic (`@push(p, …)`), borrowed (`each … in ^p`), lent
//!   to a `^` parameter its callee writes through, or the receiver of a
//!   method that writes its own receiver. Rebinding a parameter (`p is
//!   other`) replaces what its slot holds and writes no object, so it is not
//!   a write: not for a by-value parameter, and not for a `^` one, whose
//!   rebinding changes the lender's slot but not the object it held. The last
//!   two routes are edges between bodies, possibly in two records, so they
//!   are settled once every body is analyzed: the least fixpoint, so a cycle
//!   of bodies that only hand the storage to each other writes nothing. A
//!   callee whose body analysis never saw (a function value, a Zig function)
//!   is taken to write.
//! - **Lent locals.** A local lent as `^` is addressable storage (a slot);
//!   every other local is an SSA variable. Storage ids are unique per binding,
//!   so the set is the program's, not a body's.
//!
//! Every root is identified by its storage (`StoreTarget.storage`, the binding
//! beneath any narrowing view) or by `this`, never by its spelling.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;

/// A parameter of an analyzed signature, or (`index` null) its method's
/// receiver.
pub const Param = struct {
    signature: *const ast.FunctionType,
    index: ?u32,
};

pub const Effects = struct {
    /// The parameters and receivers some body writes through.
    written: std.AutoHashMapUnmanaged(Param, void) = .empty,
    /// `to` writes its storage, so `from` does: `from` is a path rooted at
    /// the receiver of a call to the method whose receiver is `to`, or the
    /// root of an argument lent to the `^` parameter `to`.
    relays: std.ArrayListUnmanaged(struct { from: Param, to: Param }) = .empty,
    /// The local storages lent as `^`.
    lent: std.AutoHashMapUnmanaged(u32, void) = .empty,
    /// The body being analyzed: its parameters (by value and `^`) by storage,
    /// and its receiver.
    params: std.AutoHashMapUnmanaged(u32, Param) = .empty,
    receiver: ?Param = null,

    pub fn deinit(self: *Effects, alloc: std.mem.Allocator) void {
        self.written.deinit(alloc);
        self.relays.deinit(alloc);
        self.lent.deinit(alloc);
        self.params.deinit(alloc);
    }

    /// Every body is analyzed: a call whose callee writes its receiver or a
    /// `^` parameter writes the path handed to it. A callee in no `analyzed`
    /// signature has no body analysis saw, so it is taken to write.
    pub fn settle(self: *Effects, alloc: std.mem.Allocator, analyzed: *const std.AutoHashMap(*const ast.FunctionType, void)) !void {
        var changed = true;
        while (changed) {
            changed = false;
            for (self.relays.items) |relay| {
                if (self.written.contains(relay.from)) continue;
                if (analyzed.contains(relay.to.signature) and !self.written.contains(relay.to)) continue;
                try self.written.put(alloc, relay.from, {});
                changed = true;
            }
        }
    }

    pub fn paramWritten(self: *const Effects, signature: *const ast.FunctionType, index: u32) bool {
        return self.written.contains(.{ .signature = signature, .index = index });
    }

    pub fn isLent(self: *const Effects, storage: u32) bool {
        return self.lent.contains(storage);
    }
};

/// The parameter or receiver of the body being analyzed that a path is rooted
/// at: the variable or `this` beneath its fields, elements and groupings.
/// Null for a path rooted anywhere else, such as a call's fresh result.
fn rootParam(self: *SemanticAnalyzer, path: *const ast.Expr) ?Param {
    var at = path;
    while (true) switch (at.data) {
        .FieldAccess => |fa| at = fa.object,
        .Index => |idx| at = idx.array,
        .Grouping => |g| at = g orelse return null,
        .This => return self.effects.receiver,
        .Variable => {
            const target = self.getStoreTarget(at.base.id) orelse return null;
            if (target.global) return null;
            return self.effects.params.get(target.storage);
        },
        else => return null,
    };
}

/// The body writes through `path`. Call once `path` is analyzed, so its root
/// has a store target.
pub fn write(self: *SemanticAnalyzer, path: *const ast.Expr) !void {
    const param = rootParam(self, path) orelse return;
    try self.effects.written.put(self.allocator, param, {});
}

/// The body lends the analyzed `arg` to the `^` parameter `index` of
/// `signature`: a write if the callee writes through it, and a local the
/// lowering makes addressable.
pub fn lend(self: *SemanticAnalyzer, arg: *const ast.Expr, signature: *const ast.FunctionType, index: u32) !void {
    if (rootParam(self, arg)) |from| {
        try self.effects.relays.append(self.allocator, .{ .from = from, .to = .{ .signature = signature, .index = index } });
    }
    if (arg.data != .Variable) return;
    const target = self.getStoreTarget(arg.base.id) orelse return;
    if (!target.global) try self.effects.lent.put(self.allocator, target.storage, {});
}

/// The body calls the method `signature` on the analyzed `receiver`, which it
/// writes if the method writes its own receiver.
pub fn receiverCall(self: *SemanticAnalyzer, receiver: *const ast.Expr, signature: *const ast.FunctionType) !void {
    const from = rootParam(self, receiver) orelse return;
    try self.effects.relays.append(self.allocator, .{ .from = from, .to = .{ .signature = signature, .index = null } });
}
