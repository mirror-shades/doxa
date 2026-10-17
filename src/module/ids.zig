//! Compilation-local identities shared by the AST, the module graph, semantic
//! analysis, and codegen.
//!
//! A `ModuleId` is a handle into one compilation's module graph: assigned at
//! record creation, immutable, and never persisted. Anything that leaves the
//! compilation (caches, LSP indices, emitted names) is keyed by the record's
//! `stable_module_key` instead.

const std = @import("std");

pub const ModuleId = u32;

fn hashKey(module: ModuleId, name: []const u8) u64 {
    var h = std.hash.Wyhash.init(0);
    h.update(std.mem.asBytes(&module));
    h.update(name);
    return h.final();
}

fn eqlKey(a_module: ModuleId, a_name: []const u8, b_module: ModuleId, b_name: []const u8) bool {
    return a_module == b_module and std.mem.eql(u8, a_name, b_name);
}

/// Semantic type identity: the *defining* module plus the declared name. Two
/// modules' `Node` are distinct types because `module` differs; importing,
/// re-exporting, or qualifying a type never mints a new identity.
pub const TypeRef = struct {
    module: ModuleId,
    name: []const u8,

    pub fn eql(a: TypeRef, b: TypeRef) bool {
        return eqlKey(a.module, a.name, b.module, b.name);
    }

    pub fn hash(self: TypeRef) u64 {
        return hashKey(self.module, self.name);
    }
};

pub const TypeRefContext = struct {
    pub fn hash(_: TypeRefContext, key: TypeRef) u64 {
        return key.hash();
    }

    pub fn eql(_: TypeRefContext, a: TypeRef, b: TypeRef) bool {
        return a.eql(b);
    }
};

pub fn TypeRefHashMap(comptime V: type) type {
    return std.HashMap(TypeRef, V, TypeRefContext, std.hash_map.default_max_load_percentage);
}

pub const SymbolKind = enum { Function, Type, Variable, Constant };

/// A reference to a declared entity, naming its *defining* module, never the
/// importer.
pub const SymbolRef = struct {
    module: ModuleId,
    name: []const u8,
    kind: SymbolKind,

    pub fn typeRef(self: SymbolRef) TypeRef {
        std.debug.assert(self.kind == .Type);
        return .{ .module = self.module, .name = self.name };
    }
};

/// The internal codegen identity of a symbol owned by a module: the defining
/// record plus the declared local name (`fn` for a top-level function,
/// `Struct.method` for a method — `.` cannot occur in an identifier, so the two
/// never meet).
pub const SymbolKey = struct {
    module: ModuleId,
    name: []const u8,
};

pub const SymbolKeyContext = struct {
    pub fn hash(_: SymbolKeyContext, key: SymbolKey) u64 {
        return hashKey(key.module, key.name);
    }

    pub fn eql(_: SymbolKeyContext, a: SymbolKey, b: SymbolKey) bool {
        return eqlKey(a.module, a.name, b.module, b.name);
    }
};
