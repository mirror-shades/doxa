//! The module graph: the single authoritative store of modules for one
//! compilation, and the identity of each.
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
//! Every file is a `ModuleRecord` — the entry file (record 0), each imported
//! `.doxa` file, each inline `zig Name { … }` block, and each imported `.zig`
//! file. A record owns its namespace (`bindings`), its public surface, and its
//! declarations, and moves through the lazy stages Parse → Declarations →
//! Types → Analysis. The graph drives the stage machine; the bodies come from
//! the layers that own each stage (`loader.zig`, semantic analysis).

const std = @import("std");
const ast = @import("../ast/ast.zig");
const hashing = @import("../utils/hashing.zig");
const ids = @import("ids.zig");
const Reporting = @import("../utils/reporting.zig");
const Location = Reporting.Location;
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;

pub const ModuleId = ids.ModuleId;
pub const TypeRef = ids.TypeRef;
pub const TypeRefContext = ids.TypeRefContext;
pub const TypeRefHashMap = ids.TypeRefHashMap;
pub const SymbolKind = ids.SymbolKind;
pub const SymbolRef = ids.SymbolRef;
pub const SymbolKey = ids.SymbolKey;
pub const SymbolKeyContext = ids.SymbolKeyContext;

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

/// A specifier that names a file by its root, `<tag>//<relative>`: the
/// spelling of a stable module key, so every module's key is also a way to
/// import it. A tag is an identifier; anything else is an ordinary path.
pub const RootSpecifier = struct {
    tag: []const u8,
    relative: []const u8,
};

pub fn rootSpecifier(specifier: []const u8) ?RootSpecifier {
    const separator = std.mem.indexOf(u8, specifier, "//") orelse return null;
    const tag = specifier[0..separator];
    if (tag.len == 0) return null;
    for (tag) |c| if (!std.ascii.isAlphanumeric(c) and c != '_') return null;
    return .{ .tag = tag, .relative = specifier[separator + 2 ..] };
}

/// What `@std()` denotes: the standard library's entry file, named by its
/// root. It imports like any other specifier.
pub const std_specifier = "std//std.doxa";

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

    /// The real path of the root tagged `tag`, or null when none is.
    pub fn pathOf(self: *const RootRegistry, tag: []const u8) ?[]const u8 {
        for (self.roots) |root| {
            if (std.mem.eql(u8, root.tag, tag)) return root.path;
        }
        return null;
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
};

/// Resolve `path` to its physical (real) path. This establishes filesystem
/// identity: `./a.doxa`, `dir/../a.doxa`, and a symlink to the same file all
/// collapse to one result, so they dedup to one module record. A host without
/// `realpath` is unsupported; there is no lexical fallback.
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

/// One stage of a record's lazy lifecycle. Each stage is a transition
/// `X → X_active → X_done`; a re-entry while the stage is active is reported
/// as `in_progress` carrying the stage, never as ready.
pub const ModuleStage = enum { Parse, Declarations, Types, Analysis };

/// Ordered: a status compares greater than every status of an earlier stage,
/// so "is this stage done" is one comparison. `Failed` is sticky and is checked
/// before the order is consulted.
pub const ModuleStatus = enum(u8) {
    NotLoaded,
    Parsing,
    Parsed,
    CollectingDeclarations,
    DeclarationsCollected,
    RegisteringTypes,
    TypesRegistered,
    Analyzing,
    Analyzed,
    Failed,

    pub fn atLeast(self: ModuleStatus, other: ModuleStatus) bool {
        return self != .Failed and @intFromEnum(self) >= @intFromEnum(other);
    }
};

fn activeStatus(stage: ModuleStage) ModuleStatus {
    return switch (stage) {
        .Parse => .Parsing,
        .Declarations => .CollectingDeclarations,
        .Types => .RegisteringTypes,
        .Analysis => .Analyzing,
    };
}

fn doneStatus(stage: ModuleStage) ModuleStatus {
    return switch (stage) {
        .Parse => .Parsed,
        .Declarations => .DeclarationsCollected,
        .Types => .TypesRegistered,
        .Analysis => .Analyzed,
    };
}

/// The status a record must have reached before `stage` may run.
fn requiredStatus(stage: ModuleStage) ModuleStatus {
    return switch (stage) {
        .Parse => .NotLoaded,
        .Declarations => .Parsed,
        .Types => .DeclarationsCollected,
        .Analysis => .TypesRegistered,
    };
}

/// Every `ensure*` returns this, so an active stage is never mistaken for a
/// completed one.
pub const EnsureResult = union(enum) {
    ready: *ModuleRecord,
    /// The requested stage is active on this record — a cycle when reached
    /// from its own body. The record's placeholders (declarations bound, types
    /// allocated) are readable; its completed results are not.
    in_progress: InProgress,
    /// Sticky. `failed_stage` and `failure` on the record identify the first
    /// diagnostic; a caller propagates it and never reports again.
    failed: *ModuleRecord,

    pub const InProgress = struct {
        record: *ModuleRecord,
        stage: ModuleStage,
    };
};

/// A stage body. It performs the stage's work on `record`; `ctx` carries the
/// owning layer's state (the loader for Parse/Declarations, the analyzer for
/// Types/Analysis). Whether the stage failed is decided by `runStage`, from
/// what the body reported, not by the body.
pub const StageRunner = *const fn (ctx: *anyopaque, record: *ModuleRecord) anyerror!void;

pub const Visibility = enum { Private, Public };

/// A bare name is either a symbol or a namespace. A namespace is always a
/// `ModuleId`: inline `zig Name { … }` and `.zig` imports are records too, so
/// there is no second namespace species.
pub const Binding = union(enum) {
    symbol: SymbolRef,
    namespace: ModuleId,
};

/// A name in a record's own namespace: what it binds, its visibility, and the
/// span of the declaration that introduced it, for duplicate-binding
/// diagnostics. A `.zig` file's functions have no Doxa declaration site.
pub const BoundName = struct {
    binding: Binding,
    visibility: Visibility,
    span: ?ast.SourceSpan = null,
};

/// One declared entity, pointing at its declaration in the defining record's
/// own AST (or, for a Zig function, at its extracted signature). Cross-record
/// references never hold a `Decl`; they hold a `SymbolRef` and look the decl up
/// in its defining record.
pub const Decl = union(enum) {
    function: *ast.Stmt,
    variable: *ast.Stmt,
    @"struct": *ast.StructDecl,
    @"enum": *ast.EnumDecl,
    group: *ast.GroupDecl,
    zig_function: *ast.ZigFnSig,

    pub fn kind(self: Decl) SymbolKind {
        return switch (self) {
            .function, .zig_function => .Function,
            .@"struct", .@"enum", .group => .Type,
            .variable => |stmt| if (stmt.data.VarDecl.type_info.is_mutable) .Variable else .Constant,
        };
    }
};

pub const RecordKind = enum {
    /// A `.doxa` source file, including the entry file.
    doxa,
    /// An inline `zig Name { … }` block, owned by the file that declares it.
    inline_zig,
    /// An imported `.zig` file.
    zig_file,
};

/// The Zig source and extracted signatures behind an inline block or a `.zig`
/// file. `name` is the block's declared name, or the file stem for a `.zig`
/// file; it labels diagnostics and generated wrapper sources only — identity is
/// the record.
pub const ZigUnit = struct {
    name: []const u8,
    source: []const u8,
    sigs: []ast.ZigFnSig,
    location: ?Location = null,
    /// One entry per distinct `DoxaError_<path>` a signature returns, filled in
    /// by the analyzer's Types stage once the enum each path names is resolved.
    /// The wrapper generator turns each into a Zig error set with these variant
    /// names, so a shim body can `return error.<Variant>`.
    error_sets: []const ZigErrorSet = &.{},
};

/// A fallible return's error set, resolved: the enum path exactly as written
/// after `DoxaError_` (which names the generated Zig error set), the resolved
/// enum identity (which lets each signature find its own set), and the enum's
/// variant names in declaration order (which is the discriminant).
pub const ZigErrorSet = struct {
    path: []const u8,
    ref: ast.TypeRef,
    variants: []const []const u8,
};

pub const ModuleRecord = struct {
    id: ModuleId,
    kind: RecordKind,
    /// Real path; null for an inline `zig` block.
    physical_key: ?[]const u8,
    /// `<root-tag>//<relative>` or `<owner>//zig/<name>`; never an absolute path.
    stable_module_key: []const u8,
    /// The declaring file of an inline `zig` block.
    owner: ?ModuleId = null,
    status: ModuleStatus = .NotLoaded,
    /// Set iff `status == .Failed`.
    failed_stage: ?ModuleStage = null,
    /// The first error the failed stage reported; reporter-owned. Null unless
    /// `status == .Failed`, and null then only when the reporter was at its
    /// diagnostic limit and recorded nothing.
    failure: ?DiagnosticIndex = null,
    /// Source text and parsed body of a `.doxa` record (`ast` is a block). Both
    /// live in the compilation's analysis arena, which outlives every record.
    source: ?[]const u8 = null,
    ast: ?*ast.Expr = null,
    /// The Zig behind an `inline_zig` or `zig_file` record.
    zig: ?ZigUnit = null,
    /// This file's own namespace: bare name → binding. Owner-scoped, so two
    /// files may bind the same name to different entities. Keys and symbol
    /// names are owned by the graph arena.
    bindings: std.StringHashMapUnmanaged(BoundName) = .empty,
    /// The subset of `bindings` visible to importers. `import n from S` reads
    /// here.
    public_bindings: std.StringHashMapUnmanaged(BoundName) = .empty,
    /// Declarations by name, in source order.
    decls: std.StringArrayHashMapUnmanaged(Decl) = .empty,
    /// The mangling tag (16 hex, collision-extended); set by
    /// `finalizeMangling`.
    mangle_tag: ?[]const u8 = null,

    /// The statements of a `.doxa` record's body.
    pub fn statements(self: *const ModuleRecord) []ast.Stmt {
        const body = self.ast orelse return &.{};
        return body.data.Block.statements;
    }
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

/// The kind tag of a mangled symbol. A top-level function `S__m` and a method
/// `S.m` differ by tag, so they can never coincide.
pub const MangleKind = enum(u8) {
    function = 'f',
    type = 't',
    global = 'g',
    method = 'm',
};

const mangle_prefix = "__doxa_m1_";

/// The single authoritative store of module records for one compilation. Every
/// parser and the loader share one graph.
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
    /// Records whose Declarations stage is active, innermost last. A
    /// declaration-collection re-entry is an unresolvable import cycle, and
    /// this is its diagnostic trail.
    import_stack: std.ArrayListUnmanaged(ModuleId) = .empty,
    /// Set by `finalizeMangling`; no record may join the graph afterwards.
    mangling_final: bool = false,

    pub fn init(io: std.Io, allocator: std.mem.Allocator, roots: []const Root) RootRegistryError!ModuleGraph {
        return .{
            .arena = std.heap.ArenaAllocator.init(allocator),
            .roots = try RootRegistry.init(io, allocator, roots),
        };
    }

    /// The graph for a compilation entered at `entry_path`, with its declared
    /// roots in priority order: the entry file's directory (`pkg`), the
    /// standard library (`std`, normally `install.stdDir`), then each include
    /// directory (`inc0`, `inc1`, …). The compiler and the language server both
    /// build their graph here, so a document resolves exactly as its
    /// compilation would.
    pub fn initForEntry(
        io: std.Io,
        allocator: std.mem.Allocator,
        entry_path: []const u8,
        std_dir: []const u8,
        include_dirs: []const []const u8,
    ) RootRegistryError!ModuleGraph {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const scratch = arena.allocator();

        const roots = try scratch.alloc(Root, 2 + include_dirs.len);
        roots[0] = .{ .tag = "pkg", .path = std.fs.path.dirname(entry_path) orelse "." };
        roots[1] = .{ .tag = "std", .path = std_dir };
        for (include_dirs, roots[2..], 0..) |dir, *root, i| {
            root.* = .{ .tag = try std.fmt.allocPrint(scratch, "inc{d}", .{i}), .path = dir };
        }
        return init(io, allocator, roots);
    }

    pub fn deinit(self: *ModuleGraph) void {
        // Every map and list backing store is arena-owned; the arena frees the
        // lot, including the individual records.
        self.roots.deinit();
        self.arena.deinit();
    }

    /// Ensure a record for a loaded file's physical identity. A second spelling,
    /// alias, or symlink to the same file returns the same record; a file
    /// matching no root is `ModuleRootUnknown`.
    pub fn ensureRecord(self: *ModuleGraph, physical: []const u8, kind: RecordKind) ModuleGraphError!*ModuleRecord {
        if (self.findPhysical(physical)) |existing| return existing;

        const child = self.arena.child_allocator;
        const stable = (self.roots.stableKeyFor(child, physical) catch return error.OutOfMemory) orelse
            return error.ModuleRootUnknown;
        defer child.free(stable);

        return self.addRecord(physical, stable, kind);
    }

    /// Allocate a record and assign it an immutable id. Ids are compilation
    /// local, assigned at creation (discovery order), and never renumbered.
    /// Output order comes from `stable_module_key` + declaration order, never
    /// from these values.
    pub fn addRecord(self: *ModuleGraph, physical_key: ?[]const u8, stable_key: []const u8, kind: RecordKind) ModuleGraphError!*ModuleRecord {
        std.debug.assert(!self.mangling_final);
        const arena = self.arena.allocator();

        if (self.by_stable_key.contains(stable_key)) return error.DuplicateStableKey;

        const new_record = try arena.create(ModuleRecord);
        new_record.* = .{
            .id = @intCast(self.records.items.len),
            .kind = kind,
            .physical_key = if (physical_key) |key| try arena.dupe(u8, key) else null,
            .stable_module_key = try arena.dupe(u8, stable_key),
        };

        try self.records.append(arena, new_record);
        try self.by_stable_key.put(arena, new_record.stable_module_key, new_record.id);
        if (new_record.physical_key) |key| {
            try self.by_physical_key.put(arena, key, new_record.id);
        }
        return new_record;
    }

    /// Register the record for an inline `zig Name { … }` block owned by
    /// `owner`. Its key is `<owner-stable-key>//zig/<name>`; a block name is
    /// unique within its file, so `(owner, name)` is a total identity and a
    /// second registration is `DuplicateStableKey`.
    pub fn addGeneratedRecord(self: *ModuleGraph, allocator: std.mem.Allocator, owner: *const ModuleRecord, decl_name: []const u8) ModuleGraphError!*ModuleRecord {
        const key = try generatedKey(allocator, owner.stable_module_key, decl_name);
        defer allocator.free(key);
        const generated = try self.addRecord(null, key, .inline_zig);
        generated.owner = owner.id;
        return generated;
    }

    pub fn record(self: *const ModuleGraph, id: ModuleId) *ModuleRecord {
        return self.records.items[id];
    }

    /// The file a record's specifiers resolve against and its diagnostics
    /// point into: its own real path, or its owner's for an inline block.
    pub fn filePath(self: *const ModuleGraph, id: ModuleId) []const u8 {
        const rec = self.record(id);
        if (rec.physical_key) |key| return key;
        return self.filePath(rec.owner.?);
    }

    /// How diagnostics name a module: its stable key, never a path on this
    /// machine.
    pub fn moduleName(self: *const ModuleGraph, id: ModuleId) []const u8 {
        return self.record(id).stable_module_key;
    }

    /// Introduce a binding in `target`'s own namespace. The caller has already
    /// rejected a duplicate (reporting both spans); binding one name twice is a
    /// compiler bug. Names are copied into the graph arena, so a binding never
    /// borrows a lexer buffer. A public binding is mirrored into the record's
    /// public surface.
    pub fn bindName(
        self: *ModuleGraph,
        target: *ModuleRecord,
        name: []const u8,
        binding: Binding,
        visibility: Visibility,
        span: ?ast.SourceSpan,
    ) !void {
        std.debug.assert(!target.bindings.contains(name));
        const arena = self.arena.allocator();
        const owned_name = try arena.dupe(u8, name);
        var owned_binding = binding;
        switch (binding) {
            .symbol => |symbol| owned_binding = .{ .symbol = .{
                .module = symbol.module,
                .name = try arena.dupe(u8, symbol.name),
                .kind = symbol.kind,
            } },
            .namespace => {},
        }
        const bound = BoundName{ .binding = owned_binding, .visibility = visibility, .span = span };
        try target.bindings.put(arena, owned_name, bound);
        if (visibility == .Public) {
            try target.public_bindings.put(arena, owned_name, bound);
        }
    }

    /// Record a declaration of `target` itself and bind its name, naming the
    /// record as the defining module.
    pub fn declare(
        self: *ModuleGraph,
        target: *ModuleRecord,
        name: []const u8,
        decl: Decl,
        visibility: Visibility,
        span: ?ast.SourceSpan,
    ) !void {
        try self.bindName(target, name, .{ .symbol = .{
            .module = target.id,
            .name = name,
            .kind = decl.kind(),
        } }, visibility, span);
        // The binding's key is the graph-owned copy of the name.
        try target.decls.put(self.arena.allocator(), target.bindings.getKey(name).?, decl);
    }

    /// The declaration a symbol reference names, in its defining record.
    pub fn declOf(self: *const ModuleGraph, symbol: SymbolRef) ?Decl {
        return self.record(symbol.module).decls.get(symbol.name);
    }

    /// Drive `stage` on `target`. The previous stage must be done (the owning
    /// layer's `ensure*` chains them). An active stage returns `in_progress`; a
    /// done stage returns `ready` without running `run`; a failed record
    /// returns `failed` without running `run`, so a second `ensure*` emits no
    /// duplicate diagnostic. `target` is pointer-stable across `run`, which may
    /// append records for its own dependencies.
    ///
    /// A stage fails when it reported an error — whether or not its body
    /// returned one — and `failure` is the first. A body that returns an error
    /// without reporting one is a compiler bug, surfaced as one internal
    /// diagnostic rather than a silent failure.
    pub fn runStage(self: *ModuleGraph, target: *ModuleRecord, stage: ModuleStage, reporter: *Reporter, ctx: *anyopaque, run: StageRunner) EnsureResult {
        _ = self;
        if (target.status == .Failed) return .{ .failed = target };
        if (target.status == activeStatus(stage)) return .{ .in_progress = .{ .record = target, .stage = stage } };
        if (target.status.atLeast(doneStatus(stage))) return .{ .ready = target };
        std.debug.assert(target.status == requiredStatus(stage));

        target.status = activeStatus(stage);
        const start = reporter.diagnostics.items.len;
        var errored = false;
        run(ctx, target) catch |err| {
            errored = true;
            if (reporter.firstErrorSince(start) == null) {
                reporter.reportCompileError(
                    null,
                    ErrorCode.INTERNAL_ERROR,
                    "the {s} stage of '{s}' failed without a diagnostic ({s}). This is a compiler bug, not an error in the program",
                    .{ @tagName(stage), target.stable_module_key, @errorName(err) },
                );
            }
        };
        const failure = reporter.firstErrorSince(start);
        if (!errored and failure == null) {
            target.status = doneStatus(stage);
            return .{ .ready = target };
        }
        target.status = .Failed;
        target.failed_stage = stage;
        target.failure = if (failure) |index| @intCast(index) else null;
        return .{ .failed = target };
    }

    /// Complete the externally-driven Parse stage for the entry record. The
    /// pipeline parses the entry file itself, then hands its source and body
    /// here for the `NotLoaded → Parsed` transition, so `Parsed` never means
    /// `ast == null`.
    pub fn completeExternalParse(self: *ModuleGraph, target: *ModuleRecord, source: []const u8, body: *ast.Expr) void {
        _ = self;
        std.debug.assert(target.status == .NotLoaded);
        target.status = .Parsed;
        target.source = source;
        target.ast = body;
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

    /// Fix every record's mangling tag. The tag is the first 64 bits of a
    /// domain-separated SHA-256 of the stable key; colliding tags are then
    /// extended (`disperseMangleTags`). Deterministic and path-independent.
    /// Called once, by lowering (`SemanticAnalyzer.finalizeLinkIdentities`),
    /// after analysis has materialized every record the program needs.
    pub fn finalizeMangling(self: *ModuleGraph) !void {
        std.debug.assert(!self.mangling_final);
        const arena = self.arena.allocator();
        for (self.records.items) |rec| {
            rec.mangle_tag = try hexTag(arena, "doxa.mangle.m1.mod", rec.stable_module_key);
        }
        try self.disperseMangleTags();
        self.mangling_final = true;
    }

    /// Extend every tag that two or more records share with `_<h2>`, a second,
    /// independently domain-separated hash of each one's stable key. A unique
    /// tag is left as it is; an extended tag is longer than any base tag, so it
    /// cannot collide with one.
    pub fn disperseMangleTags(self: *ModuleGraph) !void {
        std.debug.assert(!self.mangling_final);
        const arena = self.arena.allocator();

        var tag_counts = std.StringHashMapUnmanaged(usize).empty;
        for (self.records.items) |rec| {
            const entry = try tag_counts.getOrPut(arena, rec.mangle_tag.?);
            entry.value_ptr.* = if (entry.found_existing) entry.value_ptr.* + 1 else 1;
        }
        for (self.records.items) |rec| {
            if (tag_counts.get(rec.mangle_tag.?).? < 2) continue;
            const disperse = try hexTag(arena, "doxa.mangle.m1.disperse", rec.stable_module_key);
            rec.mangle_tag = try std.fmt.allocPrint(arena, "{s}_{s}", .{ rec.mangle_tag.?, disperse });
        }
    }

    /// The emitted spelling of a symbol owned by `module`:
    /// `__doxa_m1_<tag>__<K><len>$<component>[<len>$<component>...]`.
    ///
    /// Every component is length-prefixed on its byte count, so a component can
    /// never forge a delimiter: for a fixed kind the encoding is reversible,
    /// hence injective within a module, and distinct (collision-extended) tags
    /// make it injective across modules. Doxa identifiers are ASCII
    /// `[A-Za-z0-9_]` and `$` is an LLVM identifier character, so a Doxa
    /// symbol needs no quoting; a component from a quoted Zig identifier may
    /// carry any bytes and is framed by its byte count like any other.
    pub fn mangle(self: *const ModuleGraph, allocator: std.mem.Allocator, module: ModuleId, kind: MangleKind, components: []const []const u8) ![]u8 {
        std.debug.assert(self.mangling_final);
        var out = std.array_list.Managed(u8).init(allocator);
        errdefer out.deinit();
        try out.appendSlice(mangle_prefix);
        try out.appendSlice(self.record(module).mangle_tag.?);
        try out.appendSlice("__");
        try out.append(@intFromEnum(kind));
        for (components) |component| {
            var len_buf: [20]u8 = undefined;
            try out.appendSlice(std.fmt.bufPrint(&len_buf, "{d}", .{component.len}) catch unreachable);
            try out.append('$');
            try out.appendSlice(component);
        }
        return out.toOwnedSlice();
    }

    /// The canonical codegen key of a type: its `t`-kind mangled symbol.
    pub fn typeKey(self: *const ModuleGraph, allocator: std.mem.Allocator, ref: TypeRef) ![]u8 {
        return self.mangle(allocator, ref.module, .type, &.{ref.name});
    }
};

fn hexTag(allocator: std.mem.Allocator, domain: []const u8, stable_key: []const u8) ![]const u8 {
    var hasher = hashing.Sha256.init(.{});
    hasher.update(domain);
    hasher.update(&[_]u8{0});
    hasher.update(stable_key);
    var digest: hashing.Digest = undefined;
    hasher.final(&digest);
    const hex = hashing.hexOf(digest);
    return allocator.dupe(u8, hex[0..16]);
}

/// The declared name a mangled symbol carries: its last component. Display
/// sites (diagnostics, `peek`, reflection) show this; identity never does. A
/// string that is not a mangled symbol is returned unchanged.
pub fn displayName(name: []const u8) []const u8 {
    if (!std.mem.startsWith(u8, name, mangle_prefix)) return name;
    const tag_end = std.mem.indexOfPos(u8, name, mangle_prefix.len, "__") orelse return name;
    var i = tag_end + 3; // "__" plus the kind tag
    var last: []const u8 = name;
    while (i < name.len) {
        const dollar = std.mem.indexOfScalarPos(u8, name, i, '$') orelse return name;
        const len = std.fmt.parseInt(usize, name[i..dollar], 10) catch return name;
        const start = dollar + 1;
        if (start + len > name.len) return name;
        last = name[start .. start + len];
        i = start + len;
    }
    return last;
}
