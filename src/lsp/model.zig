//! Shared data for the language server: open documents, the symbol index it
//! builds during analysis, and the small context each request handler hands to
//! the feature modules.

const std = @import("std");
const Types = @import("../types/types.zig");
const stdlib = @import("../stdlib/catalog.zig");

pub const JsonValue = std.json.Value;

pub const Document = struct {
    path: []const u8,
    text: []u8,
};

pub const CachedField = struct {
    name: []const u8,
    type_name: []const u8,
    is_public: bool,
};

pub const CachedParam = struct {
    name: []const u8,
    type_text: []const u8,
    alias: bool,
};

pub const CachedMethod = struct {
    name: []const u8,
    is_public: bool,
    is_static: bool,
    params: []CachedParam = &.{},
    return_type: []const u8 = "",
};

/// A user-defined callable (top-level function or struct method) with the
/// signature data needed for hover, signature help, and snippets.
pub const CachedCallable = struct {
    name: []const u8,
    /// `foo` for a function, `Type.method` for a method.
    qualified: []const u8,
    params: []CachedParam,
    return_type: []const u8,
};

pub const CachedType = struct {
    name: []const u8,
    kind: Types.CustomTypeKind,
    fields: []CachedField,
    methods: []CachedMethod,
    enum_variants: [][]const u8,
};

/// A rendered type hint (`:: int`) anchored after an inferred declaration.
pub const CachedInlayHint = struct {
    line: usize,
    character: usize,
    label: []const u8,
};

pub const SymbolEntry = struct {
    name: []const u8,
    kind: u32,
    start_line: usize,
    start_character: usize,
    end_line: usize,
    end_character: usize,
};

pub const SymbolIndex = struct {
    types: std.StringHashMap(CachedType),
    variables: std.StringHashMap([]const u8),
    modules: std.StringHashMap(void),
    symbols: std.array_list.Managed(SymbolEntry),
    module_members: std.StringHashMap(std.array_list.Managed([]const u8)),
    /// User-defined callables, keyed by qualified name: `foo` for a top-level
    /// function, `Type.method` for a struct method.
    callables: std.StringHashMap(*CachedCallable),
    inlay_hints: std.array_list.Managed(CachedInlayHint),
    arena: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator) SymbolIndex {
        return .{
            .types = std.StringHashMap(CachedType).init(allocator),
            .variables = std.StringHashMap([]const u8).init(allocator),
            .modules = std.StringHashMap(void).init(allocator),
            .symbols = std.array_list.Managed(SymbolEntry).init(allocator),
            .module_members = std.StringHashMap(std.array_list.Managed([]const u8)).init(allocator),
            .callables = std.StringHashMap(*CachedCallable).init(allocator),
            .inlay_hints = std.array_list.Managed(CachedInlayHint).init(allocator),
            .arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *SymbolIndex) void {
        self.types.deinit();
        self.variables.deinit();
        self.modules.deinit();
        {
            var it = self.module_members.iterator();
            while (it.next()) |entry| entry.value_ptr.deinit();
        }
        self.module_members.deinit();
        self.symbols.deinit();
        self.callables.deinit();
        self.inlay_hints.deinit();
        self.arena.deinit();
    }

    pub fn clear(self: *SymbolIndex) void {
        self.types.clearRetainingCapacity();
        self.variables.clearRetainingCapacity();
        self.modules.clearRetainingCapacity();
        {
            var it = self.module_members.iterator();
            while (it.next()) |entry| entry.value_ptr.clearRetainingCapacity();
        }
        self.module_members.clearRetainingCapacity();
        self.symbols.clearRetainingCapacity();
        self.callables.clearRetainingCapacity();
        self.inlay_hints.clearRetainingCapacity();
        _ = self.arena.reset(.retain_capacity);
    }

    pub fn findCallable(self: *const SymbolIndex, qualified: []const u8) ?*const CachedCallable {
        return self.callables.get(qualified);
    }

    /// The struct method table for a type, keyed by the type's cached name.
    pub fn findMethod(self: *const SymbolIndex, type_name: []const u8, method_name: []const u8) ?*const CachedMethod {
        const ct = self.types.get(type_name) orelse return null;
        for (ct.methods) |*method| {
            if (std.mem.eql(u8, method.name, method_name)) return method;
        }
        return null;
    }

    pub fn addVariable(self: *SymbolIndex, name: []const u8, type_str: []const u8) !void {
        const alloc = self.arena.allocator();
        try self.variables.put(try alloc.dupe(u8, name), try alloc.dupe(u8, type_str));
    }

    pub fn addModule(self: *SymbolIndex, name: []const u8) !void {
        const alloc = self.arena.allocator();
        try self.modules.put(try alloc.dupe(u8, name), {});
    }
};

/// Everything a feature needs from the session: an allocator, the open
/// documents, the symbol index, and the standard-library catalog when loaded.
pub const FeatureContext = struct {
    allocator: std.mem.Allocator,
    documents: *const std.StringHashMap(Document),
    index: *const SymbolIndex,
    stdlib: ?*const stdlib.Catalog,
};

pub const CompletionKind = enum { Intrinsic, Dot, Word, None };

pub const CompletionContext = struct {
    prefix: []const u8,
    kind: CompletionKind,
    object_name: ?[]const u8,
    /// Byte offsets of the text a completion item replaces. Equal to the
    /// cursor for an empty prefix.
    replace_start: usize = 0,
    replace_end: usize = 0,
    start_pos: DocumentPosition = .{ .line = 0, .character = 0 },
    end_pos: DocumentPosition = .{ .line = 0, .character = 0 },

    pub fn edit(self: CompletionContext) CompletionEdit {
        return .{ .start = self.start_pos, .end = self.end_pos };
    }
};

/// The document range a completion item replaces. Supplying this explicitly
/// (rather than relying on the client's word-boundary heuristics) keeps `@`
/// prefixed items from being re-inserted with a doubled sigil.
pub const CompletionEdit = struct {
    start: DocumentPosition,
    end: DocumentPosition,
};

pub const DocumentContext = struct {
    text: []const u8,
    offset: usize,
};

pub const DocumentPosition = struct {
    line: usize,
    character: usize,
};

/// Finds a standard-library type (struct, enum, or group) by the name as
/// written: `Response`, `http.Response`, or `std.http.Response`.
pub fn findCatalogType(cat: *const stdlib.Catalog, name: []const u8) ?*const stdlib.Decl {
    if (std.mem.lastIndexOfScalar(u8, name, '.')) |dot| {
        const module = cat.findModule(name[0..dot]) orelse return null;
        const decl = module.find(name[dot + 1 ..]) orelse return null;
        return if (isCatalogTypeKind(decl.kind)) decl else null;
    }
    for (cat.modules) |*module| {
        if (module.find(name)) |decl| {
            if (isCatalogTypeKind(decl.kind)) return decl;
        }
    }
    return null;
}

pub fn isCatalogTypeKind(kind: stdlib.Kind) bool {
    return kind == .structure or kind == .enumeration or kind == .group;
}

pub fn resolveObjectType(index: *const SymbolIndex, name: []const u8) ?[]const u8 {
    if (index.variables.get(name)) |type_str| {
        return type_str;
    }
    if (index.types.contains(name)) {
        return name;
    }
    if (index.modules.contains(name)) {
        return name;
    }
    return null;
}
