//! The standard-library catalog: the public surface of `std/`, extracted from
//! the Doxa sources without running the compiler's lexer/parser.
//!
//! This is the single source of truth shared by `scripts/gen_stdlib_docs.zig`
//! (the `docs/stdlib-api.md` generator) and the language server, which uses it
//! to complete and describe `std.*` symbols. Keeping one extractor means the
//! generated reference and editor completions can never disagree.

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Kind = enum {
    function,
    method,
    structure,
    enumeration,
    group,
    map,
    constant,
    variable,
    module,

    /// The keyword as written in the source.
    pub fn keyword(self: Kind) []const u8 {
        return switch (self) {
            .function => "function",
            .method => "method",
            .structure => "struct",
            .enumeration => "enum",
            .group => "group",
            .map => "map",
            .constant => "const",
            .variable => "var",
            .module => "module",
        };
    }
};

pub const Param = struct {
    name: []const u8,
    type_text: []const u8,
    alias: bool,
};

pub const Decl = struct {
    kind: Kind,
    /// Fully qualified name: `Node.kind` for a struct member, otherwise the
    /// declared name.
    name: []const u8,
    /// Leaf name: `kind`.
    short_name: []const u8,
    params: []const Param = &.{},
    return_type: ?[]const u8 = null,
    /// The declaration's signature text (without the body).
    signature: []const u8,
    doc: ?[]const u8 = null,
    /// The complete declaration source.
    source: []const u8,
    /// Public members of a struct (methods and associated functions).
    members: []const Decl = &.{},
    /// Variant names of an enum.
    variants: []const []const u8 = &.{},

    pub fn findMember(self: *const Decl, short_name: []const u8) ?*const Decl {
        for (self.members) |*member| {
            if (std.mem.eql(u8, member.short_name, short_name)) return member;
        }
        return null;
    }
};

pub const Module = struct {
    name: []const u8,
    decls: []const Decl,

    pub fn find(self: *const Module, short_name: []const u8) ?*const Decl {
        for (self.decls) |*decl| {
            if (std.mem.eql(u8, decl.short_name, short_name)) return decl;
        }
        return null;
    }
};

pub const Catalog = struct {
    modules: []const Module,

    /// Resolves a module name as the user would write it: `io` or `std.io`.
    pub fn findModule(self: *const Catalog, name: []const u8) ?*const Module {
        var leaf = name;
        if (std.mem.startsWith(u8, leaf, "std.")) leaf = leaf["std.".len..];
        if (std.mem.eql(u8, leaf, "std")) return null;
        for (self.modules) |*module| {
            if (std.mem.eql(u8, module.name, leaf)) return module;
        }
        return null;
    }
};

/// Reads every module reachable from `<std_dir>/std.doxa` (in re-export order),
/// then any remaining `.doxa` file directly under `<std_dir>`. All returned
/// memory is owned by `allocator`.
pub fn load(allocator: Allocator, io: std.Io, std_dir: []const u8) !Catalog {
    const root_file = try std.fs.path.join(allocator, &.{ std_dir, "std.doxa" });
    const root_src = try readNormalized(io, allocator, root_file);

    var modules = std.array_list.Managed(Module).init(allocator);
    var seen = std.StringHashMap(void).init(allocator);

    const reexports = try parseReExports(allocator, root_src);
    for (reexports) |re| {
        const file = try resolveModulePath(allocator, std_dir, re.path);
        const src = try readNormalized(io, allocator, file);
        try modules.append(.{ .name = re.name, .decls = try scanSource(allocator, src) });
        try seen.put(re.name, {});
    }

    var dir = try std.Io.Dir.cwd().openDir(io, std_dir, .{ .iterate = true });
    defer dir.close(io);
    var extra = std.array_list.Managed([]const u8).init(allocator);
    var it = dir.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .file) continue;
        if (!std.mem.endsWith(u8, entry.name, ".doxa")) continue;
        if (std.mem.eql(u8, entry.name, "std.doxa")) continue;
        const stem = entry.name[0 .. entry.name.len - ".doxa".len];
        if (seen.contains(stem)) continue;
        try extra.append(try allocator.dupe(u8, stem));
    }
    std.mem.sort([]const u8, extra.items, {}, lessStr);
    for (extra.items) |stem| {
        const name = try std.fmt.allocPrint(allocator, "{s}.doxa", .{stem});
        const file = try std.fs.path.join(allocator, &.{ std_dir, name });
        const src = try readNormalized(io, allocator, file);
        try modules.append(.{ .name = stem, .decls = try scanSource(allocator, src) });
        try seen.put(stem, {});
    }

    return .{ .modules = try modules.toOwnedSlice() };
}

/// Renders one parameter as the editor shows it: `^name: type` for an alias
/// parameter, `name: type` otherwise, and the bare type when it is unnamed.
pub fn parameterLabel(allocator: Allocator, param: *const Param) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    if (param.alias) try w.writeAll("^");
    if (param.name.len > 0) try w.print("{s}: ", .{param.name});
    try w.writeAll(param.type_text);
    return out.toOwnedSlice();
}

/// A snippet body that inserts the callable's name and a tab stop per
/// parameter: `get($1, $2)`. Intended for `function`/`method` declarations.
pub fn callSnippet(allocator: Allocator, decl: *const Decl) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    try w.print("{s}(", .{decl.short_name});
    for (decl.params, 0..) |_, i| {
        if (i > 0) try w.writeAll(", ");
        try w.print("${d}", .{i + 1});
    }
    try w.writeAll(")");
    return out.toOwnedSlice();
}

/// A one-line human signature: `get(url: string, timeout: int) -> Response | StdError`.
pub fn signatureDetail(allocator: Allocator, decl: *const Decl) ![]u8 {
    var out = std.Io.Writer.Allocating.init(allocator);
    errdefer out.deinit();
    const w = &out.writer;

    if (decl.kind == .function or decl.kind == .method) {
        try w.print("{s}(", .{decl.short_name});
        for (decl.params, 0..) |param, i| {
            if (i > 0) try w.writeAll(", ");
            if (param.alias) try w.writeAll("^");
            if (param.name.len > 0) try w.print("{s}: ", .{param.name});
            try w.writeAll(param.type_text);
        }
        try w.writeAll(")");
        if (decl.return_type) |ret| try w.print(" -> {s}", .{ret});
    } else {
        try w.print("{s} {s}", .{ decl.kind.keyword(), decl.short_name });
    }

    return out.toOwnedSlice();
}

fn lessStr(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.lessThan(u8, a, b);
}

fn readNormalized(io: std.Io, allocator: Allocator, path: []const u8) ![]u8 {
    const raw = try std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(64 * 1024 * 1024));
    var out = try allocator.alloc(u8, raw.len);
    var n: usize = 0;
    for (raw) |c| {
        if (c == '\r') continue;
        out[n] = c;
        n += 1;
    }
    return out[0..n];
}

fn resolveModulePath(allocator: Allocator, std_dir: []const u8, rel: []const u8) ![]u8 {
    var p = rel;
    if (std.mem.startsWith(u8, p, "./")) p = p[2..];
    return std.fs.path.join(allocator, &.{ std_dir, p });
}

const ReExport = struct {
    name: []const u8,
    path: []const u8,
};

fn parseReExports(allocator: Allocator, content: []const u8) ![]ReExport {
    var list = std.array_list.Managed(ReExport).init(allocator);
    var it = std.mem.splitScalar(u8, content, '\n');
    while (it.next()) |line| {
        const t = std.mem.trim(u8, line, " \t");
        const prefix = "public module ";
        if (!std.mem.startsWith(u8, t, prefix)) continue;
        const rest = t[prefix.len..];
        const sp = std.mem.indexOfScalar(u8, rest, ' ') orelse continue;
        const name = rest[0..sp];
        const q1 = std.mem.indexOfScalar(u8, rest, '"') orelse continue;
        const q2 = std.mem.lastIndexOfScalar(u8, rest, '"') orelse continue;
        if (q2 <= q1) continue;
        try list.append(.{
            .name = try allocator.dupe(u8, name),
            .path = try allocator.dupe(u8, rest[q1 + 1 .. q2]),
        });
    }
    return list.toOwnedSlice();
}

/// Extracts the public declarations from one module's source text. Exposed so
/// tests (and any future tooling) can work without touching the filesystem.
pub fn parseSource(allocator: Allocator, src: []const u8) Allocator.Error![]Decl {
    return scanSource(allocator, src);
}

fn scanSource(allocator: Allocator, src: []const u8) Allocator.Error![]Decl {
    var scan = Scan{
        .src = src,
        .allocator = allocator,
        .lines = try splitLines(allocator, src),
    };
    return scan.scanRange(0, src.len, null);
}

fn splitLines(allocator: Allocator, src: []const u8) Allocator.Error![]const []const u8 {
    var list = std.array_list.Managed([]const u8).init(allocator);
    var it = std.mem.splitScalar(u8, src, '\n');
    while (it.next()) |line| try list.append(line);
    return list.toOwnedSlice();
}

const Scan = struct {
    src: []const u8,
    allocator: Allocator,
    lines: []const []const u8,

    const Parsed = struct {
        decl: Decl,
        after: usize,
    };

    fn scanRange(self: *Scan, start: usize, end: usize, prefix: ?[]const u8) Allocator.Error![]Decl {
        var decls = std.array_list.Managed(Decl).init(self.allocator);
        var i = start;
        var depth: usize = 0;
        while (i < end) {
            const c = self.src[i];
            if (c == '"') {
                i = skipString(self.src, i, end);
                continue;
            }
            if (c == '#') {
                i = skipLineComment(self.src, i, end);
                continue;
            }
            if (c == '/' and i + 1 < end and self.src[i + 1] == '*') {
                i = skipBlockComment(self.src, i, end);
                continue;
            }
            if (c == '{') {
                depth += 1;
                i += 1;
                continue;
            }
            if (c == '}') {
                if (depth > 0) depth -= 1;
                i += 1;
                continue;
            }
            if (isIdentStart(c)) {
                const word_start = i;
                i = identEnd(self.src, i, end);
                // Inline `zig Name { ... }` blocks carry raw Zig, whose string
                // and brace rules differ from Doxa's. Skip them entirely so the
                // Doxa scan never interprets Zig source.
                if (skipZigBlock(self.src, word_start, end)) |after| {
                    i = after;
                    continue;
                }
                if (depth == 0 and std.mem.eql(u8, self.src[word_start..i], "public")) {
                    if (try self.parseDecl(i, end, prefix)) |parsed| {
                        try decls.append(parsed.decl);
                        i = parsed.after;
                        continue;
                    }
                }
                continue;
            }
            i += 1;
        }
        return decls.toOwnedSlice();
    }

    // `after_public` is the index just past the `public` keyword.
    fn parseDecl(self: *Scan, after_public: usize, end: usize, prefix: ?[]const u8) Allocator.Error!?Parsed {
        const src = self.src;
        const alloc = self.allocator;
        var j = skipWs(src, after_public, end);
        if (j >= end or !isIdentStart(src[j])) return null;
        const kind_start = j;
        j = identEnd(src, j, end);
        const kind = kindFromKeyword(src[kind_start..j]) orelse return null;

        j = skipWs(src, j, end);
        if (j >= end or !isIdentStart(src[j])) return null;
        const name_start = j;
        j = identEnd(src, j, end);
        const raw_name = src[name_start..j];

        const decl_start = after_public - "public".len;

        var source_end: usize = undefined;
        var sig_end: usize = undefined;
        var body_open: usize = 0;
        var body_close: usize = 0;
        var has_body = false;

        if (isBraceBodied(kind)) {
            const ob = findOpenBrace(src, j, end) orelse return null;
            const cb = matchBrace(src, ob, end);
            body_open = ob;
            body_close = cb;
            has_body = true;
            source_end = if (cb >= end) end else cb + 1;
            sig_end = ob;
        } else {
            const e = findValueEnd(src, j, end);
            source_end = e;
            sig_end = e;
        }

        const signature = std.mem.trim(u8, src[decl_start..sig_end], " \t\n");
        const name = if (prefix) |p|
            try std.fmt.allocPrint(alloc, "{s}.{s}", .{ p, raw_name })
        else
            try alloc.dupe(u8, raw_name);

        const is_callable = kind == .function or kind == .method;
        const params = if (is_callable) try parseParams(alloc, signature) else &.{};
        const return_type = if (is_callable) parseReturn(signature) else null;

        const decl_line = std.mem.count(u8, src[0..decl_start], "\n");
        const doc = try docBefore(alloc, self.lines, decl_line);

        var members: []Decl = &.{};
        var variants: []const []const u8 = &.{};
        if (has_body and kind == .structure) {
            members = try self.scanRange(body_open + 1, body_close, name);
        } else if (has_body and kind == .enumeration) {
            variants = try parseEnumVariants(alloc, src[body_open + 1 .. body_close]);
        }

        return .{
            .decl = .{
                .kind = kind,
                .name = name,
                .short_name = try alloc.dupe(u8, raw_name),
                .params = params,
                .return_type = return_type,
                .signature = signature,
                .doc = doc,
                .source = std.mem.trim(u8, src[decl_start..source_end], " \t\n"),
                .members = members,
                .variants = variants,
            },
            .after = source_end,
        };
    }
};

fn kindFromKeyword(keyword: []const u8) ?Kind {
    const kinds = [_]struct { text: []const u8, kind: Kind }{
        .{ .text = "function", .kind = .function },
        .{ .text = "method", .kind = .method },
        .{ .text = "struct", .kind = .structure },
        .{ .text = "enum", .kind = .enumeration },
        .{ .text = "group", .kind = .group },
        .{ .text = "map", .kind = .map },
        .{ .text = "const", .kind = .constant },
        .{ .text = "var", .kind = .variable },
        .{ .text = "module", .kind = .module },
    };
    for (kinds) |entry| {
        if (std.mem.eql(u8, entry.text, keyword)) return entry.kind;
    }
    return null;
}

fn isBraceBodied(kind: Kind) bool {
    return switch (kind) {
        .function, .method, .structure, .enumeration, .group, .map => true,
        .constant, .variable, .module => false,
    };
}

// Parses `name :: type` / `^name :: type` parameter lists.
fn parseParams(allocator: Allocator, signature: []const u8) Allocator.Error![]Param {
    const open = std.mem.indexOfScalar(u8, signature, '(') orelse return &.{};
    var depth: usize = 0;
    var close: usize = signature.len;
    var i = open;
    while (i < signature.len) : (i += 1) {
        const c = signature[i];
        if (c == '(') {
            depth += 1;
        } else if (c == ')') {
            depth -= 1;
            if (depth == 0) {
                close = i;
                break;
            }
        }
    }

    const inner = signature[open + 1 .. close];
    var list = std.array_list.Managed(Param).init(allocator);
    var start: usize = 0;
    var bracket_depth: usize = 0;
    var j: usize = 0;
    while (j <= inner.len) : (j += 1) {
        const at_end = j == inner.len;
        const c: u8 = if (at_end) ',' else inner[j];
        switch (c) {
            '(', '[', '{' => bracket_depth += 1,
            ')', ']', '}' => {
                if (bracket_depth > 0) bracket_depth -= 1;
            },
            ',' => {
                if (bracket_depth != 0) continue;
                const part = std.mem.trim(u8, inner[start..j], " \t\n");
                start = j + 1;
                if (part.len == 0) continue;
                try list.append(try parseParam(allocator, part));
            },
            else => {},
        }
    }
    return list.toOwnedSlice();
}

fn parseParam(allocator: Allocator, part: []const u8) Allocator.Error!Param {
    var alias = false;
    var name: []const u8 = "";
    var type_text: []const u8 = part;

    if (std.mem.indexOf(u8, part, "::")) |sep| {
        name = std.mem.trim(u8, part[0..sep], " \t");
        type_text = std.mem.trim(u8, part[sep + 2 ..], " \t");
    }
    if (name.len > 0 and name[0] == '^') {
        alias = true;
        name = std.mem.trim(u8, name[1..], " \t");
    }
    // The catalog borrows from its input buffers; the trimmed slices are stable
    // views into the same source, so duplicating them is unnecessary.
    _ = allocator;
    return .{ .name = name, .type_text = type_text, .alias = alias };
}

fn parseReturn(signature: []const u8) ?[]const u8 {
    const open = std.mem.indexOfScalar(u8, signature, '(') orelse return null;
    var depth: usize = 0;
    var close: usize = signature.len;
    var i = open;
    while (i < signature.len) : (i += 1) {
        const c = signature[i];
        if (c == '(') {
            depth += 1;
        } else if (c == ')') {
            depth -= 1;
            if (depth == 0) {
                close = i;
                break;
            }
        }
    }

    var rest = std.mem.trim(u8, signature[close + 1 ..], " \t\n");
    const keyword = "returns";
    if (!std.mem.startsWith(u8, rest, keyword)) return null;
    if (rest.len > keyword.len and isIdentChar(rest[keyword.len])) return null;
    rest = std.mem.trim(u8, rest[keyword.len..], " \t\n");
    if (rest.len == 0) return null;
    return rest;
}

fn parseEnumVariants(allocator: Allocator, body: []const u8) Allocator.Error![]const []const u8 {
    var list = std.array_list.Managed([]const u8).init(allocator);
    var i: usize = 0;
    while (i < body.len) {
        const c = body[i];
        if (c == '#') {
            i = skipLineComment(body, i, body.len);
            continue;
        }
        if (c == '/' and i + 1 < body.len and body[i + 1] == '*') {
            i = skipBlockComment(body, i, body.len);
            continue;
        }
        if (isIdentStart(c)) {
            const start = i;
            i = identEnd(body, i, body.len);
            try list.append(body[start..i]);
            continue;
        }
        i += 1;
    }
    return list.toOwnedSlice();
}

fn isIdentStart(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isIdentChar(c: u8) bool {
    return isIdentStart(c) or (c >= '0' and c <= '9');
}

fn identEnd(src: []const u8, start: usize, end: usize) usize {
    var i = start;
    while (i < end and isIdentChar(src[i])) i += 1;
    return i;
}

fn skipWs(src: []const u8, start: usize, end: usize) usize {
    var i = start;
    while (i < end and (src[i] == ' ' or src[i] == '\t' or src[i] == '\r' or src[i] == '\n')) i += 1;
    return i;
}

fn skipLineComment(src: []const u8, start: usize, end: usize) usize {
    var i = start;
    while (i < end and src[i] != '\n') i += 1;
    return i;
}

fn skipBlockComment(src: []const u8, start: usize, end: usize) usize {
    var i = start + 2;
    while (i + 1 < end) : (i += 1) {
        if (src[i] == '*' and src[i + 1] == '/') return i + 2;
    }
    return end;
}

fn skipString(src: []const u8, start: usize, end: usize) usize {
    var i = start + 1;
    while (i < end) {
        const c = src[i];
        if (c == '\\') {
            i += 2;
            continue;
        }
        if (c == '"') return i + 1;
        if (c == '{') {
            i = skipInterpolation(src, i, end);
            continue;
        }
        i += 1;
    }
    return end;
}

fn skipInterpolation(src: []const u8, start: usize, end: usize) usize {
    var depth: usize = 0;
    var i = start;
    while (i < end) {
        const c = src[i];
        if (c == '"') {
            i = skipString(src, i, end);
            continue;
        }
        if (c == '#') {
            i = skipLineComment(src, i, end);
            continue;
        }
        if (c == '/' and i + 1 < end and src[i + 1] == '*') {
            i = skipBlockComment(src, i, end);
            continue;
        }
        if (c == '{') {
            depth += 1;
        } else if (c == '}') {
            depth -= 1;
            if (depth == 0) return i + 1;
        }
        i += 1;
    }
    return end;
}

fn skipZigBlock(src: []const u8, start: usize, end: usize) ?usize {
    if (!isIdentStart(src[start])) return null;
    var i = identEnd(src, start, end);
    if (!std.mem.eql(u8, src[start..i], "zig")) return null;
    i = skipWs(src, i, end);
    if (i >= end or !isIdentStart(src[i])) return null;
    i = identEnd(src, i, end);
    i = skipWs(src, i, end);
    if (i >= end or src[i] != '{') return null;
    return matchZigBrace(src, i, end);
}

fn matchZigBrace(src: []const u8, open: usize, end: usize) usize {
    var depth: usize = 0;
    var i = open;
    while (i < end) {
        const c = src[i];
        if (c == '"') {
            i = skipZigString(src, i, end);
            continue;
        }
        if (c == '\'') {
            i = skipZigChar(src, i, end);
            continue;
        }
        if (c == '/' and i + 1 < end and src[i + 1] == '/') {
            i = skipLineComment(src, i, end);
            continue;
        }
        // Zig multiline string literal: the rest of the line is raw text.
        if (c == '\\' and i + 1 < end and src[i + 1] == '\\') {
            i = skipLineComment(src, i, end);
            continue;
        }
        if (c == '{') {
            depth += 1;
        } else if (c == '}') {
            depth -= 1;
            if (depth == 0) return i + 1;
        }
        i += 1;
    }
    return end;
}

fn skipZigString(src: []const u8, start: usize, end: usize) usize {
    var i = start + 1;
    while (i < end) {
        if (src[i] == '\\') {
            i += 2;
            continue;
        }
        if (src[i] == '"') return i + 1;
        i += 1;
    }
    return end;
}

fn skipZigChar(src: []const u8, start: usize, end: usize) usize {
    var i = start + 1;
    while (i < end) {
        if (src[i] == '\\') {
            i += 2;
            continue;
        }
        if (src[i] == '\'') return i + 1;
        i += 1;
    }
    return end;
}

fn findOpenBrace(src: []const u8, start: usize, end: usize) ?usize {
    var i = start;
    while (i < end) {
        const c = src[i];
        if (c == '"') {
            i = skipString(src, i, end);
            continue;
        }
        if (c == '#') {
            i = skipLineComment(src, i, end);
            continue;
        }
        if (c == '/' and i + 1 < end and src[i + 1] == '*') {
            i = skipBlockComment(src, i, end);
            continue;
        }
        if (c == '{') return i;
        i += 1;
    }
    return null;
}

fn matchBrace(src: []const u8, open: usize, end: usize) usize {
    var depth: usize = 0;
    var i = open;
    while (i < end) {
        const c = src[i];
        if (c == '"') {
            i = skipString(src, i, end);
            continue;
        }
        if (c == '#') {
            i = skipLineComment(src, i, end);
            continue;
        }
        if (c == '/' and i + 1 < end and src[i + 1] == '*') {
            i = skipBlockComment(src, i, end);
            continue;
        }
        if (c == '{') {
            depth += 1;
        } else if (c == '}') {
            depth -= 1;
            if (depth == 0) return i;
        }
        i += 1;
    }
    return end;
}

fn findValueEnd(src: []const u8, start: usize, end: usize) usize {
    var paren: usize = 0;
    var bracket: usize = 0;
    var brace: usize = 0;
    var i = start;
    while (i < end) {
        const c = src[i];
        if (c == '"') {
            i = skipString(src, i, end);
            continue;
        }
        if (c == '#') {
            i = skipLineComment(src, i, end);
            continue;
        }
        if (c == '/' and i + 1 < end and src[i + 1] == '*') {
            i = skipBlockComment(src, i, end);
            continue;
        }
        if (c == '\n' and paren == 0 and bracket == 0 and brace == 0) return i;
        switch (c) {
            '(' => paren += 1,
            ')' => {
                if (paren > 0) paren -= 1;
            },
            '[' => bracket += 1,
            ']' => {
                if (bracket > 0) bracket -= 1;
            },
            '{' => brace += 1,
            '}' => {
                if (brace > 0) brace -= 1;
            },
            else => {},
        }
        i += 1;
    }
    return end;
}

// The comment block immediately above a declaration, if any. Both `#` line
// runs and `/* ... */` blocks are recognized; a blank line breaks the block.
fn docBefore(allocator: Allocator, lines: []const []const u8, decl_line: usize) Allocator.Error!?[]const u8 {
    if (decl_line == 0) return null;
    const prev = std.mem.trim(u8, lines[decl_line - 1], " \t");
    if (prev.len == 0) return null;

    if (prev[0] == '#') {
        var start = decl_line - 1;
        while (start > 0) {
            const t = std.mem.trim(u8, lines[start - 1], " \t");
            if (t.len > 0 and t[0] == '#') start -= 1 else break;
        }
        var buf = std.array_list.Managed(u8).init(allocator);
        for (lines[start..decl_line], 0..) |ln, idx| {
            var t = std.mem.trim(u8, ln, " \t");
            if (t.len > 0 and t[0] == '#') t = t[1..];
            if (t.len > 0 and t[0] == ' ') t = t[1..];
            if (idx > 0) try buf.append('\n');
            try buf.appendSlice(t);
        }
        return try buf.toOwnedSlice();
    }

    if (std.mem.endsWith(u8, prev, "*/")) {
        var start = decl_line - 1;
        while (start > 0) {
            const t = std.mem.trim(u8, lines[start], " \t");
            if (std.mem.startsWith(u8, t, "/*")) break;
            start -= 1;
        }
        const first = std.mem.trim(u8, lines[start], " \t");
        if (!std.mem.startsWith(u8, first, "/*")) return null;

        var combined = std.array_list.Managed(u8).init(allocator);
        for (lines[start..decl_line], 0..) |ln, idx| {
            if (idx > 0) try combined.append('\n');
            try combined.appendSlice(ln);
        }
        var s = combined.items;
        const open = std.mem.indexOf(u8, s, "/*") orelse return null;
        s = s[open + 2 ..];
        const close = std.mem.lastIndexOf(u8, s, "*/") orelse s.len;
        s = s[0..close];

        var cleaned = std.array_list.Managed([]const u8).init(allocator);
        var line_it = std.mem.splitScalar(u8, s, '\n');
        while (line_it.next()) |ln| {
            var t = std.mem.trim(u8, ln, " \t");
            if (t.len > 0 and t[0] == '*') {
                t = t[1..];
                if (t.len > 0 and t[0] == ' ') t = t[1..];
            }
            try cleaned.append(std.mem.trim(u8, t, " \t"));
        }
        var lo: usize = 0;
        var hi: usize = cleaned.items.len;
        while (lo < hi and cleaned.items[lo].len == 0) lo += 1;
        while (hi > lo and cleaned.items[hi - 1].len == 0) hi -= 1;

        var buf = std.array_list.Managed(u8).init(allocator);
        for (cleaned.items[lo..hi], 0..) |t, idx| {
            if (idx > 0) try buf.append('\n');
            try buf.appendSlice(t);
        }
        return try buf.toOwnedSlice();
    }

    return null;
}
