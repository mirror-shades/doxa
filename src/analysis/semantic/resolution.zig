//! What a name-bearing expression denotes, decided once by semantic analysis.
//!
//! Analysis records a `Resolution` for every expression that names a
//! module-level entity: a bare or qualified function, a module global reached
//! from another record, a type in value position, a namespace on the way to a
//! member, and the method a call dispatches to. Codegen reads these and never
//! walks bindings or matches a spelling; an unresolved name reaching lowering
//! is a compiler bug, not a fallback.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const ids = @import("../../module/ids.zig");

pub const Resolution = union(enum) {
    /// A module path (`std`, `std.io`) on the way to a member. Not a value.
    namespace: ids.ModuleId,
    /// A Doxa function, by its defining record.
    function: ids.SymbolRef,
    /// A function of an inline `zig` block or an imported `.zig` file.
    zig_function: ids.SymbolRef,
    /// A module-level variable or constant of another record, reached through
    /// an import or a namespace.
    global: ids.SymbolRef,
    /// A type named in value position: an enum qualifier (`Color` in
    /// `Color.Red`), a struct literal's type, a constructor's receiver.
    type: ast.TypeRef,
    /// `T.m(...)`: a static method of `T`, or `T.New` (the constructor).
    static_method: Method,
    /// `x.m(...)`: an instance method of `x`'s struct type.
    method: Method,

    pub const Method = struct {
        owner: ast.TypeRef,
        name: []const u8,
    };
};

pub const ResolutionMap = std.AutoHashMap(ast.NodeId, Resolution);

/// A place in the AST that declares or names a module-level global. Once the
/// module graph is final, each site is given the global's link name
/// (`ModuleGraph.mangle(.global)`), so every later stage — whose storage is
/// keyed by a variable's name — tells two modules' same-named globals apart
/// by construction.
pub const GlobalSite = struct {
    symbol: ids.SymbolRef,
    place: union(enum) {
        /// A declaration's name, a bare reference, or an assignment target.
        name: *ast.Token,
        /// A namespace member access (`ns.global`), which becomes a plain
        /// reference to the global.
        member: *ast.Expr,
    },

    /// The AST place this site rewrites. A place is recorded once however
    /// often analysis resolves it.
    pub fn address(self: GlobalSite) usize {
        return switch (self.place) {
            .name => |token| @intFromPtr(token),
            .member => |expr| @intFromPtr(expr),
        };
    }
};

/// Every global site of the program, by place.
pub const GlobalSites = std.array_hash_map.Auto(usize, GlobalSite);
