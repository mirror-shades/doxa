//! The module loader: the owner of the Parse and Declarations stages.
//!
//! A specifier is only a probe. `resolveSpecifier` finds the candidate file,
//! takes its real path, and interns the record — it parses nothing. A record is
//! parsed and its declarations collected only when a name in it is resolved
//! (`member`), or when an `import n from S` must classify `n` against `S`'s
//! public surface. So `module std from @std()` followed by `std.io.println`
//! materializes `std` and `std.io` and leaves `std.http` interned but unloaded.
//!
//! Declaration collection turns a file's top-level statements into its
//! namespace: each declaration is a `Decl` bound under its own name, `module X
//! from S` binds `X` to `S`'s record, `import n from S` binds `n` to whatever
//! `S` publicly binds under that name (a symbol or a namespace, kind
//! preserved), and an inline `zig Name { … }` becomes a record of its own bound
//! as a namespace. A name bound twice in one file is a located error pointing
//! at both sites.

const std = @import("std");
const ast = @import("../ast/ast.zig");
const token = @import("../types/token.zig");
const LexicalAnalyzer = @import("../analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../parser/parser_types.zig").Parser;
const inline_zig = @import("../parser/inline_zig.zig");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const graph_mod = @import("graph.zig");
const ModuleGraph = graph_mod.ModuleGraph;
const ModuleRecord = graph_mod.ModuleRecord;
const ModuleId = graph_mod.ModuleId;
const BoundName = graph_mod.BoundName;
const Binding = graph_mod.Binding;
const Visibility = graph_mod.Visibility;

pub const ModuleLoader = struct {
    io: std.Io,
    /// The compilation's analysis allocator. Sources, tokens, and ASTs live
    /// here; it outlives every record.
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    graph: *ModuleGraph,

    pub fn init(io: std.Io, allocator: std.mem.Allocator, reporter: *Reporter, graph: *ModuleGraph) ModuleLoader {
        return .{ .io = io, .allocator = allocator, .reporter = reporter, .graph = graph };
    }

    /// Register the entry file as record 0 and complete its Parse stage from
    /// the pipeline's own parse. It is registered before any other file is
    /// interned, so a module that imports the entry file resolves this record
    /// instead of parsing the root a second time.
    pub fn registerEntry(self: *ModuleLoader, path: []const u8, source: []const u8, statements: []ast.Stmt) ErrorList!*ModuleRecord {
        std.debug.assert(self.graph.count() == 0);
        const physical = graph_mod.physicalPath(self.io, self.allocator, path) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.reporter.reportCompileError(null, ErrorCode.MODULE_NOT_FOUND, "Entry file '{s}' could not be resolved", .{path});
                return error.ModuleNotFound;
            },
        };
        const record = try self.intern(physical, .doxa, path, null);
        self.graph.completeExternalParse(record, source, try blockOf(self.allocator, statements));
        return record;
    }

    /// Drive a record through the Parse stage. Parsing is pure syntax and never
    /// reaches another file, so it cannot re-enter.
    pub fn ensureParsed(self: *ModuleLoader, record: *ModuleRecord) ErrorList!*ModuleRecord {
        return switch (self.graph.runStage(record, .Parse, self.reporter, self, parseStage)) {
            .ready => |ready| ready,
            .failed => error.ModuleParseError,
            .in_progress => unreachable,
        };
    }

    /// Drive a record through the Declarations stage (parsing it first).
    pub fn ensureDeclarations(self: *ModuleLoader, record: *ModuleRecord) ErrorList!*ModuleRecord {
        return self.declarationsOf(record, null);
    }

    /// `record`'s declarations, needed at `site`. A re-entry is an import
    /// cycle no lazy resolution can break: two files whose public surfaces
    /// each need the other's to be classified. Only an `import` collecting
    /// declarations can re-enter, so the cycle is reported at that `import`.
    fn declarationsOf(self: *ModuleLoader, record: *ModuleRecord, site: ?Location) ErrorList!*ModuleRecord {
        _ = try self.ensureParsed(record);
        return switch (self.graph.runStage(record, .Declarations, self.reporter, self, declarationsStage)) {
            .ready => |ready| ready,
            .failed => error.ModuleParseError,
            .in_progress => |cycle| {
                self.reportImportCycle(cycle.record, site);
                return error.CircularImport;
            },
        };
    }

    /// What `namespace` publicly binds under `name`, collecting the namespace
    /// record's declarations first. Null when it binds nothing public by that
    /// name.
    pub fn member(self: *ModuleLoader, namespace: ModuleId, name: []const u8) ErrorList!?BoundName {
        const record = try self.ensureDeclarations(self.graph.record(namespace));
        return record.public_bindings.get(name);
    }

    /// Resolve `specifier` as written in `importer` to a record, interning it
    /// without parsing. The specifier is a probe for a candidate file; identity
    /// is the candidate's real path.
    pub fn resolveSpecifier(self: *ModuleLoader, importer: ModuleId, specifier: []const u8, site: ?Location) ErrorList!*ModuleRecord {
        const importer_path = self.graph.filePath(importer);
        const located = if (graph_mod.rootSpecifier(specifier)) |rooted| blk: {
            const root = self.graph.roots.pathOf(rooted.tag) orelse {
                self.reporter.reportCompileError(
                    site,
                    ErrorCode.MODULE_NOT_FOUND,
                    "Module '{s}' names the root '{s}', which is not declared",
                    .{ specifier, rooted.tag },
                );
                return error.ModuleNotFound;
            };
            break :blk try self.underRoot(root, rooted.relative);
        } else try self.probe(importer_path, specifier);
        const candidate = located orelse {
            self.reporter.reportCompileError(
                site,
                ErrorCode.MODULE_NOT_FOUND,
                "Module '{s}' could not be found (imported from '{s}')",
                .{ specifier, self.graph.moduleName(importer) },
            );
            return error.ModuleNotFound;
        };
        defer self.allocator.free(candidate);

        const physical = graph_mod.physicalPath(self.io, self.allocator, candidate) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.reporter.reportCompileError(site, ErrorCode.MODULE_NOT_FOUND, "Module '{s}' could not be resolved", .{specifier});
                return error.ModuleNotFound;
            },
        };
        const kind: graph_mod.RecordKind = if (std.mem.endsWith(u8, physical, ".zig")) .zig_file else .doxa;
        return self.intern(physical, kind, specifier, site);
    }

    fn intern(self: *ModuleLoader, physical: []const u8, kind: graph_mod.RecordKind, spelling: []const u8, site: ?Location) ErrorList!*ModuleRecord {
        return self.graph.ensureRecord(physical, kind) catch |err| {
            switch (err) {
                error.ModuleRootUnknown => self.reporter.reportCompileError(
                    site,
                    ErrorCode.MODULE_ROOT_UNKNOWN,
                    "Module '{s}' resolves to '{s}', which is outside every declared root",
                    .{ spelling, physical },
                ),
                error.DuplicateStableKey => self.reporter.reportCompileError(
                    site,
                    ErrorCode.DUPLICATE_STABLE_KEY,
                    "Two distinct modules claim the stable key of '{s}'",
                    .{physical},
                ),
                error.OutOfMemory => {},
            }
            return err;
        };
    }

    /// The file a root-qualified specifier names: `relative` under `root`,
    /// searched nowhere else.
    fn underRoot(self: *ModuleLoader, root: []const u8, relative: []const u8) ErrorList!?[]u8 {
        const file = try self.fileName(relative);
        defer self.allocator.free(file);
        return self.existingJoin(&.{ root, file });
    }

    /// The first existing candidate for `specifier`, in priority order: an
    /// absolute path as written; then, relative to the importing file's
    /// directory, the file itself, `modules/<file>`, and each ancestor
    /// directory. Never the working directory: a program resolves the same
    /// files wherever `doxa` is run from.
    fn probe(self: *ModuleLoader, importer_path: []const u8, specifier: []const u8) ErrorList!?[]u8 {
        const file = try self.fileName(if (std.mem.startsWith(u8, specifier, "./")) specifier[2..] else specifier);
        defer self.allocator.free(file);

        if (std.fs.path.isAbsolute(file)) {
            return if (self.exists(file)) try self.allocator.dupe(u8, file) else null;
        }

        const importer_dir = std.fs.path.dirname(importer_path) orelse ".";
        if (try self.existingJoin(&.{ importer_dir, file })) |found| return found;
        if (try self.existingJoin(&.{ importer_dir, "modules", file })) |found| return found;

        var dir = std.fs.path.dirname(importer_dir);
        while (dir) |ancestor| : (dir = std.fs.path.dirname(ancestor)) {
            if (try self.existingJoin(&.{ ancestor, file })) |found| return found;
        }
        return null;
    }

    /// A specifier's file name: one without an extension names a `.doxa` file.
    fn fileName(self: *ModuleLoader, spelled: []const u8) ErrorList![]u8 {
        const has_extension = std.mem.endsWith(u8, spelled, ".doxa") or std.mem.endsWith(u8, spelled, ".zig");
        return if (has_extension) try self.allocator.dupe(u8, spelled) else try std.fmt.allocPrint(self.allocator, "{s}.doxa", .{spelled});
    }

    fn existingJoin(self: *ModuleLoader, parts: []const []const u8) ErrorList!?[]u8 {
        const joined = try std.fs.path.join(self.allocator, parts);
        if (self.exists(joined)) return joined;
        self.allocator.free(joined);
        return null;
    }

    fn exists(self: *ModuleLoader, path: []const u8) bool {
        const file = (if (std.fs.path.isAbsolute(path))
            std.Io.Dir.openFileAbsolute(self.io, path, .{})
        else
            std.Io.Dir.cwd().openFile(self.io, path, .{})) catch return false;
        file.close(self.io);
        return true;
    }

    fn readFile(self: *ModuleLoader, path: []const u8) ErrorList![]const u8 {
        return std.Io.Dir.cwd().readFileAlloc(self.io, path, self.allocator, .unlimited) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.reporter.reportCompileError(try self.fileLocation(path), ErrorCode.MODULE_NOT_FOUND, "Module file '{s}' could not be read", .{path});
                return error.ModuleLoadError;
            },
        };
    }

    /// The start of the file at `path`: where a problem with the whole file is
    /// reported.
    fn fileLocation(self: *ModuleLoader, path: []const u8) ErrorList!Location {
        return .{
            .file = path,
            .file_uri = try self.reporter.ensureFileUri(self.io, path),
            .range = .{ .start_line = 1, .start_col = 1, .end_line = 1, .end_col = 1 },
        };
    }

    // ── Stage bodies ────────────────────────────────────────────────────────

    fn parseStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
        const self: *ModuleLoader = @ptrCast(@alignCast(ctx));
        return switch (record.kind) {
            .doxa => self.parseDoxa(record),
            .zig_file => self.parseZigFile(record),
            // An inline block is born with its owner's declarations.
            .inline_zig => unreachable,
        };
    }

    fn parseDoxa(self: *ModuleLoader, record: *ModuleRecord) ErrorList!void {
        const path = record.physical_key.?;
        const source = try self.readFile(path);
        if (self.reporter.source_cache) |cache| cache.load(path, source) catch {};

        var lexer = try LexicalAnalyzer.init(self.io, self.allocator, source, path, self.reporter);
        // The AST borrows the lexer's string buffers; they move to the analysis
        // arena before the lexer is freed (`docs/memory.md`).
        defer {
            lexer.takeOwnershipOfStrings();
            lexer.deinit();
        }
        try lexer.initKeywords();
        const tokens = try lexer.lexTokens();

        var parser = Parser.init(self.io, self.allocator, tokens.items, path, try self.reporter.ensureFileUri(self.io, path), self.reporter);
        const parse_start = self.reporter.diagnostics.items.len;
        const statements = parser.execute() catch |err| {
            // The parser reports most errors where it finds them; one it only
            // returned is reported here, located in this file.
            if (self.reporter.firstErrorSince(parse_start) == null) parser.reportParseError(err);
            return err;
        };
        record.source = source;
        record.ast = try blockOf(self.allocator, statements);
    }

    fn parseZigFile(self: *ModuleLoader, record: *ModuleRecord) ErrorList!void {
        const path = record.physical_key.?;
        const source = try self.readFile(path);
        if (self.reporter.source_cache) |cache| cache.load(path, source) catch {};

        const sigs = inline_zig.sanitizeAndExtract(self.allocator, source, true) catch {
            self.reporter.reportCompileError(
                try self.fileLocation(path),
                ErrorCode.INVALID_IMPORT,
                "Invalid Zig module '{s}': top-level may contain only `const X = @import(...)` and functions with Doxa-compatible signatures",
                .{path},
            );
            return error.InvalidZigModule;
        };
        record.zig = .{ .name = std.fs.path.stem(path), .source = source, .sigs = sigs };
    }

    fn declarationsStage(ctx: *anyopaque, record: *ModuleRecord) anyerror!void {
        const self: *ModuleLoader = @ptrCast(@alignCast(ctx));
        try self.graph.import_stack.append(self.graph.arena.allocator(), record.id);
        defer _ = self.graph.import_stack.pop();
        return switch (record.kind) {
            .doxa => self.collectDoxa(record),
            .zig_file => self.collectZigFunctions(record, record.zig.?.sigs, null),
            .inline_zig => unreachable,
        };
    }

    fn collectDoxa(self: *ModuleLoader, record: *ModuleRecord) ErrorList!void {
        for (record.statements()) |*stmt| {
            switch (stmt.data) {
                .FunctionDecl => |func| try self.declare(record, func.name, .{ .function = stmt }, func.is_public),
                .VarDecl => |decl| try self.declare(record, decl.name, .{ .variable = stmt }, decl.is_public),
                .EnumDecl => |*decl| try self.declare(record, decl.name, .{ .@"enum" = decl }, decl.is_public),
                .GroupDecl => |*decl| try self.declare(record, decl.name, .{ .group = decl }, decl.is_public),
                .Expression => |maybe_expr| {
                    const expr = maybe_expr orelse continue;
                    switch (expr.data) {
                        .StructDecl => |*decl| try self.declare(record, decl.name, .{ .@"struct" = decl }, decl.is_public),
                        .EnumDecl => |*decl| try self.declare(record, decl.name, .{ .@"enum" = decl }, decl.is_public),
                        .GroupDecl => |*decl| try self.declare(record, decl.name, .{ .group = decl }, decl.is_public),
                        else => {},
                    }
                },
                .ZigDecl => |zig| try self.collectInlineZig(record, stmt, zig.name, zig.source, zig.sigs),
                .Import => |import_info| try self.collectImport(record, import_info),
                else => {},
            }
        }
    }

    /// An inline `zig Name { … }` is a record of its own, owned by the file
    /// that declares it and bound there as a private namespace. Its functions
    /// are its public surface: `Name.fn` reaches them through the owner's
    /// binding.
    fn collectInlineZig(self: *ModuleLoader, owner: *ModuleRecord, stmt: *ast.Stmt, name: token.Token, source: []const u8, sigs: []ast.ZigFnSig) ErrorList!void {
        try self.checkUnbound(owner, name);
        const block = self.graph.addGeneratedRecord(self.allocator, owner, name.lexeme) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // The owner's own duplicate check already rejected a second block
            // of this name; reaching here is a compiler bug.
            error.DuplicateStableKey, error.ModuleRootUnknown => unreachable,
        };
        block.zig = .{ .name = name.lexeme, .source = source, .sigs = sigs, .location = stmt.base.location() };
        try self.collectZigFunctions(block, sigs, ast.SourceSpan.fromToken(name));
        // An inline block is parsed as part of its owner and has no imports, so
        // it is born with its declarations collected.
        block.status = .DeclarationsCollected;
        try self.graph.bindName(owner, name.lexeme, .{ .namespace = block.id }, .Private, ast.SourceSpan.fromToken(name));
    }

    fn collectZigFunctions(self: *ModuleLoader, record: *ModuleRecord, sigs: []ast.ZigFnSig, span: ?ast.SourceSpan) ErrorList!void {
        for (sigs) |*sig| {
            if (record.bindings.contains(sig.name)) {
                self.reporter.reportCompileError(
                    if (span) |s| s.location else null,
                    ErrorCode.DUPLICATE_VARIABLE,
                    "Duplicate zig function '{s}' in '{s}'",
                    .{ sig.name, record.zig.?.name },
                );
                return error.DuplicateVariableName;
            }
            try self.graph.declare(record, sig.name, .{ .zig_function = sig }, .Public, span);
        }
    }

    fn collectImport(self: *ModuleLoader, record: *ModuleRecord, import_info: ast.ImportInfo) ErrorList!void {
        const visibility: Visibility = if (import_info.is_public) .Public else .Private;
        const site = import_info.names[0];
        const site_location = ast.SourceSpan.fromToken(site).location;
        const target = try self.resolveSpecifier(record.id, import_info.module_path, site_location);

        switch (import_info.import_type) {
            // `module X from S` binds `X` to `S` without collecting anything
            // from it: the namespace materializes when a name in it is used.
            .Module => try self.bindChecked(record, site, .{ .namespace = target.id }, visibility),
            // `import n from S` binds what `S` publicly binds under `n`,
            // which requires `S`'s declarations.
            .Specific => {
                const source = try self.declarationsOf(target, site_location);
                for (import_info.names) |name| {
                    const bound = source.public_bindings.get(name.lexeme) orelse {
                        self.reporter.reportCompileError(
                            ast.SourceSpan.fromToken(name).location,
                            ErrorCode.SYMBOL_NOT_FOUND_IN_IMPORT_MODULE,
                            "Module '{s}' has no public declaration '{s}'",
                            .{ import_info.module_path, name.lexeme },
                        );
                        return error.ImportedNameNotFound;
                    };
                    try self.bindChecked(record, name, bound.binding, visibility);
                }
            },
        }
    }

    fn declare(self: *ModuleLoader, record: *ModuleRecord, name: token.Token, decl: graph_mod.Decl, is_public: bool) ErrorList!void {
        try self.checkUnbound(record, name);
        try self.graph.declare(record, name.lexeme, decl, if (is_public) .Public else .Private, ast.SourceSpan.fromToken(name));
    }

    fn bindChecked(self: *ModuleLoader, record: *ModuleRecord, name: token.Token, binding: Binding, visibility: Visibility) ErrorList!void {
        try self.checkUnbound(record, name);
        try self.graph.bindName(record, name.lexeme, binding, visibility, ast.SourceSpan.fromToken(name));
    }

    /// A file binds each name exactly once. A second binding — declaration,
    /// `module` alias, `import`, or `zig` block — is a located error with the
    /// previous site attached.
    fn checkUnbound(self: *ModuleLoader, record: *ModuleRecord, name: token.Token) ErrorList!void {
        const previous = record.bindings.get(name.lexeme) orelse return;
        const location = ast.SourceSpan.fromToken(name).location;
        if (previous.span) |previous_span| {
            const related = [_]Reporting.RelatedInformation{.{
                .message = "previous declaration here",
                .location = previous_span.location,
            }};
            self.reporter.reportWithRelated(.CompileTime, .Error, location, ErrorCode.DUPLICATE_VARIABLE, &related, "Duplicate binding name '{s}'", .{name.lexeme});
        } else {
            self.reporter.reportCompileError(location, ErrorCode.DUPLICATE_VARIABLE, "Duplicate binding name '{s}'", .{name.lexeme});
        }
        return error.DuplicateVariableName;
    }

    /// Report an import cycle ending at `reentered`, at the `import` that
    /// closes it, listing the files whose declarations were being collected.
    fn reportImportCycle(self: *ModuleLoader, reentered: *ModuleRecord, site: ?Location) void {
        var trail = std.array_list.Managed(u8).init(self.allocator);
        defer trail.deinit();
        const stack = self.graph.import_stack.items;
        const start = std.mem.indexOfScalar(ModuleId, stack, reentered.id) orelse 0;
        for (stack[start..]) |id| {
            trail.print("  {s} imports\n", .{self.graph.moduleName(id)}) catch return;
        }
        trail.print("  {s} (circular dependency)", .{self.graph.moduleName(reentered.id)}) catch return;
        self.reporter.reportCompileError(site, ErrorCode.CIRCULAR_IMPORT, "Circular import detected:\n{s}", .{trail.items});
    }
};

/// Wrap a file's statements in the block a record's `ast` holds.
fn blockOf(allocator: std.mem.Allocator, statements: []ast.Stmt) ErrorList!*ast.Expr {
    const body = try allocator.create(ast.Expr);
    body.* = .{
        .base = .{ .id = ast.generateNodeId(), .span = null },
        .data = .{ .Block = .{ .statements = statements, .value = null } },
    };
    return body;
}
