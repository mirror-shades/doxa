//! Module identity: the single authoritative identity for a module.
//!
//! A module carries two distinct identifiers, and conflating them is what lets
//! absolute checkout paths leak into emitted `.ll`, caches, and diagnostics:
//!
//! - `physical_key` is the resolved real path. It is used for loading and
//!   deduplication and is filesystem identity, not a spelling.
//! - `stable_module_key` is `<root-tag>//<real-path-relative>`. It is used for
//!   mangling, deterministic ids, cache keys, and diagnostics, and is never an
//!   absolute path.
//!
//! `RootRegistry` owns the ordered, uniquely-tagged roots that turn a real path
//! into a stable key. Matching is physical (both sides are `realpath`d once)
//! and component-wise, so a root `/pkg/foo` does not contain `/pkg/foobar`.
//!
//! This file is the identity layer of the module graph; the record store and
//! the `ensure*` state machine land on top of it.

const std = @import("std");
const ast = @import("../ast/ast.zig");

pub const ModuleId = u32;
pub const TypeId = u32;

/// Semantic type identity: the defining module plus the declared name. Two
/// modules' `Node` are distinct because `module` differs. Never a numeric id.
pub const TypeRef = struct {
    module: ModuleId,
    name: []const u8,
};

pub const SymbolKind = enum { Function, Type, Variable, Constant };

/// A reference to a symbol, naming its *defining* module, never the importer.
pub const SymbolRef = struct {
    module: ModuleId,
    name: []const u8,
    kind: SymbolKind,
};

pub const Visibility = enum { Private, Public };

/// A bare name is either a symbol or a namespace. A namespace is always a
/// `ModuleId`: inline `zig Name { … }` and `.zig` imports are synthetic
/// records, so there is no second namespace species.
pub const Binding = union(enum) {
    symbol: SymbolRef,
    namespace: ModuleId,
};

/// Stable key for the synthetic `builtin` module namespace.
pub const builtin_key = "builtin";

/// Generated identities derive from their owner, never a bare `gen:<name>`.
/// Each inline `zig Name { … }` is declared in exactly one file and duplicate
/// block names in one file are already an error, so (owner, block name) is a
/// total identity.
pub fn generatedKey(allocator: std.mem.Allocator, owner_key: []const u8, decl_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(allocator, "{s}//zig/{s}", .{ owner_key, decl_name });
}

fn isSeparator(c: u8) bool {
    return c == '/' or c == '\\';
}

/// Yield the next path component, skipping any run of separators. `index` is an
/// in/out byte cursor. Returns null once the path is exhausted.
fn nextComponent(path: []const u8, index: *usize) ?[]const u8 {
    var i = index.*;
    while (i < path.len and isSeparator(path[i])) i += 1;
    if (i >= path.len) {
        index.* = i;
        return null;
    }
    const start = i;
    while (i < path.len and !isSeparator(path[i])) i += 1;
    index.* = i;
    return path[start..i];
}

/// The part of `candidate` below `root`, or null when `candidate` is not `root`
/// or a descendant of it. Comparison is by path components, never a textual
/// prefix, so `/pkg/foo` does not contain `/pkg/foobar`. The returned slice
/// borrows `candidate` and keeps its host separators; the empty slice means
/// `candidate == root`.
pub fn relativePart(root: []const u8, candidate: []const u8) ?[]const u8 {
    var root_index: usize = 0;
    var candidate_index: usize = 0;
    while (nextComponent(root, &root_index)) |root_component| {
        const candidate_component = nextComponent(candidate, &candidate_index) orelse return null;
        if (!std.mem.eql(u8, root_component, candidate_component)) return null;
    }
    while (candidate_index < candidate.len and isSeparator(candidate[candidate_index])) {
        candidate_index += 1;
    }
    return candidate[candidate_index..];
}

/// `<tag>//<relative>` with `/` separators regardless of host separator.
pub fn stableKey(allocator: std.mem.Allocator, tag: []const u8, relative: []const u8) ![]const u8 {
    var out = std.array_list.Managed(u8).init(allocator);
    errdefer out.deinit();
    try out.appendSlice(tag);
    try out.appendSlice("//");
    for (relative) |c| try out.append(if (isSeparator(c)) '/' else c);
    return out.toOwnedSlice();
}

/// The canonical identity of a file: its real path once resolved, and the
/// root-relative stable key derived from that path.
pub const FileIdentity = struct {
    physical_key: []const u8,
    stable_module_key: []const u8,
};

pub const Root = struct {
    tag: []const u8,
    path: []const u8,
};

pub const RootRegistryError = error{
    DuplicateRootTag,
    InvalidRootPath,
    OutOfMemory,
};

pub const RootMatch = struct {
    tag: []const u8,
    /// Relative path with `/` separators, empty when the candidate is the root
    /// itself.
    relative: []const u8,
};

/// An ordered list of uniquely-tagged roots. A file is matched against the
/// roots in priority order; the first match wins, so overlapping roots cannot
/// yield route-dependent keys. All importable files must live under a declared
/// root: a file that matches none is a hard error, because any path- or
/// content-derived fallback either reintroduces machine dependence or fails to
/// distinguish two identical files at different locations.
///
/// Root paths are `realpath`d once at construction, and callers pass a real
/// path to `classify`, so both sides of a comparison are physical.
pub const RootRegistry = struct {
    arena: std.heap.ArenaAllocator,
    roots: []Root,

    pub fn init(io: std.Io, allocator: std.mem.Allocator, roots: []const Root) RootRegistryError!RootRegistry {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();
        const arena_allocator = arena.allocator();

        const owned = try arena_allocator.alloc(Root, roots.len);
        var seen = std.StringHashMap(void).init(arena_allocator);
        for (roots, 0..) |root, i| {
            const tag = try arena_allocator.dupe(u8, root.tag);
            if (seen.contains(tag)) return error.DuplicateRootTag;
            try seen.put(tag, {});

            const path = physicalPath(io, arena_allocator, root.path) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.InvalidRootPath,
            };
            owned[i] = .{ .tag = tag, .path = path };
        }

        return .{ .arena = arena, .roots = owned };
    }

    pub fn deinit(self: *RootRegistry) void {
        self.arena.deinit();
    }

    /// Classify a real path against the roots in priority order.
    pub fn classify(self: *const RootRegistry, real_path: []const u8) ?RootMatch {
        for (self.roots) |root| {
            if (relativePart(root.path, real_path)) |relative| {
                return .{ .tag = root.tag, .relative = relative };
            }
        }
        return null;
    }

    /// The stable key for a real path, or null when it matches no root.
    pub fn stableKeyFor(self: *const RootRegistry, allocator: std.mem.Allocator, real_path: []const u8) !?[]const u8 {
        const match = self.classify(real_path) orelse return null;
        return try stableKey(allocator, match.tag, match.relative);
    }

    /// The full identity for a real path, or null when it matches no root.
    pub fn identityFor(self: *const RootRegistry, allocator: std.mem.Allocator, real_path: []const u8) !?FileIdentity {
        const stable = (try self.stableKeyFor(allocator, real_path)) orelse return null;
        return .{
            .physical_key = try allocator.dupe(u8, real_path),
            .stable_module_key = stable,
        };
    }
};

/// Resolve `path` to its physical (real) path. This establishes filesystem
/// identity: `./a.doxa`, `dir/../a.doxa`, and a symlink to the same file all
/// collapse to one result, so they dedup to one module record.
pub fn physicalPath(io: std.Io, allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    // `realPath*Alloc` returns a sentinel-terminated slice; free that exact
    // allocation, then hand back a plain slice the caller frees normally.
    const resolved = if (std.fs.path.isAbsolute(path))
        try std.Io.Dir.realPathFileAbsoluteAlloc(io, path, allocator)
    else
        try std.Io.Dir.cwd().realPathFileAlloc(io, path, allocator);
    defer allocator.free(resolved);
    return try allocator.dupe(u8, resolved);
}

pub const DiagnosticIndex = u32;

/// One stage of a module's lazy lifecycle. Each stage is a transition
/// `NotLoaded → X → X_done`; a re-entry while the stage is active is a cycle.
pub const ModuleStage = enum { Parse, Declarations, Types, Analysis };

pub const ModuleStatus = enum {
    NotLoaded,
    Parsing,
    Parsed,
    CollectingDeclarations,
    DeclarationsCollected,
    RegisteringTypes,
    TypesRegistered,
    Analyzing,
    Analyzed,
    /// Sticky; a stage reported an error. No later stage runs.
    Failed,
};

/// Every `ensure*` returns this, so an active stage is never mistaken for a
/// completed one: no `ensure*` returns `ready` while its own stage is active.
pub const EnsureResult = union(enum) {
    ready: *ModuleRecord,
    in_progress: *ModuleRecord,
    failed: *ModuleRecord,
};

/// The outcome of a stage body invoked by an `ensure*`. `failed` carries the
/// reporter's diagnostic index when the body emitted one; the body keeps the
/// original error with its caller, since `EnsureResult` is infallible.
pub const StageOutcome = union(enum) {
    ok,
    failed: ?DiagnosticIndex,
};

/// A stage body. It performs the stage's work and records the result on
/// `record`; `ctx` carries the caller's own state (parser, source, error).
pub const StageRunner = *const fn (ctx: *anyopaque, record: *ModuleRecord) StageOutcome;

pub const ModuleRecord = struct {
    id: ModuleId,
    /// Real path; null for generated/inline-Zig records.
    physical_key: ?[]const u8,
    /// Mangled/deterministic identity; never an absolute path.
    stable_module_key: []const u8,
    status: ModuleStatus = .NotLoaded,
    /// Set iff `status == .Failed`.
    failed_stage: ?ModuleStage = null,
    /// First diagnostic; reporter-owned.
    failure: ?DiagnosticIndex = null,
    /// MIGRATION (Phase 1 → removed once records own their AST/bindings):
    /// the parsed module payload, keyed by this record's physical identity.
    module_info: ?ast.ModuleInfo = null,
    /// Owned source text and parsed body of this module. They are allocated in
    /// the compilation's analysis arena for now, which outlives every record,
    /// so the destruction order (lexer strings transferred before the lexer is
    /// freed; AST never outlives its arena; graph arena torn down last) holds
    /// without the record owning an arena yet. Per-record arenas land with
    /// per-record analysis (Phase 5).
    source: ?[]const u8 = null,
    ast: ?*ast.Expr = null,
};

pub const ModuleGraphError = error{
    /// Two distinct records claimed the same stable key. A compiler error, not
    /// a silent merge.
    DuplicateStableKey,
    /// A file matched no declared root. There is no silent fallback: any path-
    /// or content-derived alternative either reintroduces machine dependence or
    /// cannot distinguish two identical files at different locations.
    ModuleRootUnknown,
    OutOfMemory,
};

/// The single authoritative store of module records for one compilation. Every
/// parser shares one graph; there is no per-parser map copying and no
/// child→parent merge asymmetry.
///
/// Records are individually allocated in `arena` and `records` holds pointers.
/// Appending a record never relocates an existing one, because recursive
/// resolution retains `*ModuleRecord` across `ensure*` calls.
pub const ModuleGraph = struct {
    arena: std.heap.ArenaAllocator,
    roots: RootRegistry,
    records: std.ArrayListUnmanaged(*ModuleRecord) = .empty,
    by_physical_key: std.StringHashMapUnmanaged(ModuleId) = .empty,
    by_stable_key: std.StringHashMapUnmanaged(ModuleId) = .empty,
    import_stack: std.ArrayListUnmanaged(ModuleId) = .empty,

    pub fn init(io: std.Io, allocator: std.mem.Allocator, roots: []const Root) RootRegistryError!ModuleGraph {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .roots = try RootRegistry.init(io, allocator, roots),
        };
    }

    pub fn deinit(self: *ModuleGraph) void {
        // Every map and list backing store is arena-owned; the arena frees the
        // lot, including the individual records.
        self.roots.deinit();
        self.arena.deinit();
    }

    /// Stable key for a physical path, or null when it matches no root.
    pub fn stableKeyFor(self: *const ModuleGraph, allocator: std.mem.Allocator, physical: []const u8) !?[]const u8 {
        return self.roots.stableKeyFor(allocator, physical);
    }

    /// Ensure a record for a loaded file's physical identity. A second spelling,
    /// alias, or symlink to the same file returns the same record rather than a
    /// duplicate; a file matching no root is `ModuleRootUnknown`. The caller
    /// fills `module_info` once parsing completes.
    pub fn ensureRecord(self: *ModuleGraph, physical: []const u8) ModuleGraphError!*ModuleRecord {
        if (self.findPhysical(physical)) |existing| return existing;

        const allocator = self.arena.child_allocator;
        const stable = (self.roots.stableKeyFor(allocator, physical) catch return error.OutOfMemory) orelse
            return error.ModuleRootUnknown;
        defer allocator.free(stable);

        return self.addRecord(physical, stable);
    }

    /// Allocate a record and assign it an immutable id. Ids are compilation
    /// local, assigned at creation (discovery order), and never renumbered, so
    /// lazy discovery cannot invalidate references. Output order comes from
    /// `stable_module_key` + declaration order, never from these values.
    ///
    /// A generated record (`physical_key == null`) is keyed by stable key
    /// alone; a physical record is additionally indexed by its real path.
    pub fn addRecord(self: *ModuleGraph, physical_key: ?[]const u8, stable_key: []const u8) ModuleGraphError!*ModuleRecord {
        const allocator = self.arena.allocator();

        if (self.by_stable_key.contains(stable_key)) return error.DuplicateStableKey;

        const new_record = try allocator.create(ModuleRecord);
        new_record.* = .{
            .id = @intCast(self.records.items.len),
            .physical_key = if (physical_key) |key| try allocator.dupe(u8, key) else null,
            .stable_module_key = try allocator.dupe(u8, stable_key),
        };

        try self.records.append(allocator, new_record);
        try self.by_stable_key.put(allocator, new_record.stable_module_key, new_record.id);
        if (new_record.physical_key) |key| {
            try self.by_physical_key.put(allocator, key, new_record.id);
        }
        return new_record;
    }

    /// Register the synthetic record for an inline `zig Name { … }` block owned
    /// by `owner`. Its stable key is `<owner-stable-key>//zig/<name>`, and since a
    /// block name is unique within its file and each file is exactly one owner,
    /// `(owner, name)` is a total identity. A second registration of the same
    /// pair is a compiler bug and is reported as `DuplicateStableKey` rather than
    /// silently merged. `allocator` is only for the transient key; the record's
    /// copy lives in the graph arena.
    pub fn addGeneratedRecord(self: *ModuleGraph, allocator: std.mem.Allocator, owner: *const ModuleRecord, decl_name: []const u8) ModuleGraphError!*ModuleRecord {
        const key = try generatedKey(allocator, owner.stable_module_key, decl_name);
        defer allocator.free(key);
        return self.addRecord(null, key);
    }

    pub fn record(self: *const ModuleGraph, id: ModuleId) *ModuleRecord {
        return self.records.items[id];
    }

    /// Drive the Parse stage: `NotLoaded → Parsing → Parsed`. A re-entry while
    /// `Parsing` returns `in_progress`, so a parse cycle is never mistaken for a
    /// parsed record; the caller reports the cycle where it needs the body. A
    /// `Failed` record returns `failed` without re-running `run`, so a second
    /// `ensure*` emits no duplicate diagnostic. The record is pointer-stable
    /// across `run`, which may append records for its own dependencies.
    pub fn ensureParsed(self: *ModuleGraph, ctx: *anyopaque, run: StageRunner, target: *ModuleRecord) EnsureResult {
        _ = self;
        switch (target.status) {
            .Parsing => return .{ .in_progress = target },
            .Failed => return .{ .failed = target },
            .NotLoaded => {
                target.status = .Parsing;
                switch (run(ctx, target)) {
                    .ok => {
                        target.status = .Parsed;
                        return .{ .ready = target };
                    },
                    .failed => |diagnostic| {
                        target.status = .Failed;
                        target.failed_stage = .Parse;
                        if (diagnostic) |index| target.failure = index;
                        return .{ .failed = target };
                    },
                }
            },
            else => return .{ .ready = target },
        }
    }

    /// Complete the externally-driven Parse stage for the entry record. The
    /// entry file is parsed by the pipeline rather than `ensureParsed`, so the
    /// pipeline supplies the record's own source, AST, and parsed payload and
    /// this performs the `NotLoaded → Parsed` transition. `Parsed` never means
    /// `ast == null`; a record already marked `Failed` is left failed.
    pub fn completeExternalParse(
        self: *ModuleGraph,
        target: *ModuleRecord,
        source: []const u8,
        module_ast: *ast.Expr,
        module_info: ast.ModuleInfo,
    ) void {
        _ = self;
        if (target.status == .Failed) return;
        target.status = .Parsed;
        target.source = source;
        target.ast = module_ast;
        target.module_info = module_info;
    }

    pub fn findPhysical(self: *const ModuleGraph, physical_key: []const u8) ?*ModuleRecord {
        const id = self.by_physical_key.get(physical_key) orelse return null;
        return self.records.items[id];
    }

    pub fn findStable(self: *const ModuleGraph, stable_key: []const u8) ?*ModuleRecord {
        const id = self.by_stable_key.get(stable_key) orelse return null;
        return self.records.items[id];
    }

    pub fn count(self: *const ModuleGraph) usize {
        return self.records.items.len;
    }
};
