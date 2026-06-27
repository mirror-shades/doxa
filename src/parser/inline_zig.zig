const std = @import("std");
const ast = @import("../ast/ast.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

const TokenKind = enum {
    ident,
    string_lit,
    symbol,
};

const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
};

const Tokenizer = struct {
    input: []const u8,
    i: usize = 0,
    block_comment_depth: usize = 0,
    peeked: ?Token = null,

    fn init(input: []const u8) Tokenizer {
        return .{ .input = input };
    }

    const Snapshot = struct { i: usize, block_comment_depth: usize, peeked: ?Token };

    fn save(self: *Tokenizer) Snapshot {
        return .{ .i = self.i, .block_comment_depth = self.block_comment_depth, .peeked = self.peeked };
    }

    fn restore(self: *Tokenizer, snap: Snapshot) void {
        self.i = snap.i;
        self.block_comment_depth = snap.block_comment_depth;
        self.peeked = snap.peeked;
    }

    fn peek(self: *Tokenizer) ErrorList!?Token {
        if (self.peeked == null) {
            self.peeked = try self.nextInternal();
        }
        return self.peeked;
    }

    fn next(self: *Tokenizer) ErrorList!?Token {
        if (self.peeked) |t| {
            self.peeked = null;
            return t;
        }
        return try self.nextInternal();
    }

    fn nextInternal(self: *Tokenizer) ErrorList!?Token {
        const s = self.input;
        while (true) {
            if (self.i >= s.len) return null;

            const ch = s[self.i];
            const next_ch = if (self.i + 1 < s.len) s[self.i + 1] else 0;

            // Whitespace
            if (std.ascii.isWhitespace(ch)) {
                self.i += 1;
                continue;
            }

            // Line comments
            if (ch == '/' and next_ch == '/') {
                self.i += 2;
                while (self.i < s.len and s[self.i] != '\n') : (self.i += 1) {}
                continue;
            }

            // Block comments (nested)
            if (ch == '/' and next_ch == '*') {
                self.i += 2;
                self.block_comment_depth += 1;
                while (self.block_comment_depth > 0) {
                    if (self.i >= s.len) return error.InlineZigNotValid;
                    const a = s[self.i];
                    const b = if (self.i + 1 < s.len) s[self.i + 1] else 0;
                    if (a == '/' and b == '*') {
                        self.block_comment_depth += 1;
                        self.i += 2;
                        continue;
                    }
                    if (a == '*' and b == '/') {
                        self.block_comment_depth -= 1;
                        self.i += 2;
                        continue;
                    }
                    self.i += 1;
                }
                continue;
            }

            // String literal
            if (ch == '"') {
                const start = self.i;
                self.i += 1;
                var escaped = false;
                while (self.i < s.len) : (self.i += 1) {
                    const c = s[self.i];
                    if (escaped) {
                        escaped = false;
                        continue;
                    }
                    if (c == '\\') {
                        escaped = true;
                        continue;
                    }
                    if (c == '"') {
                        self.i += 1;
                        return Token{ .kind = .string_lit, .lexeme = s[start..self.i] };
                    }
                }
                return error.InlineZigNotValid;
            }

            // Character literal (ignore as a single token)
            if (ch == '\'') {
                const start = self.i;
                self.i += 1;
                var escaped = false;
                while (self.i < s.len) : (self.i += 1) {
                    const c = s[self.i];
                    if (escaped) {
                        escaped = false;
                        continue;
                    }
                    if (c == '\\') {
                        escaped = true;
                        continue;
                    }
                    if (c == '\'') {
                        self.i += 1;
                        return Token{ .kind = .string_lit, .lexeme = s[start..self.i] };
                    }
                }
                return error.InlineZigNotValid;
            }

            // Two-char symbol
            if (ch == '-' and next_ch == '>') {
                const start = self.i;
                self.i += 2;
                return Token{ .kind = .symbol, .lexeme = s[start..self.i] };
            }

            // Single-char symbols
            if (ch == '(' or ch == ')' or ch == '{' or ch == '}' or ch == '[' or ch == ']' or ch == ',' or ch == ':' or ch == ';' or ch == '=') {
                const start = self.i;
                self.i += 1;
                return Token{ .kind = .symbol, .lexeme = s[start..self.i] };
            }

            // Identifier (including builtins like @import)
            if (ch == '@' or ch == '_' or std.ascii.isAlphabetic(ch)) {
                const start = self.i;
                self.i += 1;
                while (self.i < s.len) : (self.i += 1) {
                    const c = s[self.i];
                    if (c == '_' or std.ascii.isAlphanumeric(c)) continue;
                    break;
                }
                return Token{ .kind = .ident, .lexeme = s[start..self.i] };
            }

            // Number literal (only needed so function bodies can be lexed permissively)
            if (std.ascii.isDigit(ch)) {
                const start = self.i;
                self.i += 1;
                while (self.i < s.len) : (self.i += 1) {
                    const c = s[self.i];
                    if (std.ascii.isAlphanumeric(c) or c == '_' or c == '.') continue;
                    break;
                }
                return Token{ .kind = .ident, .lexeme = s[start..self.i] };
            }

            // Fallback: return a single-byte symbol token so we can skip over arbitrary Zig bodies.
            const start = self.i;
            self.i += 1;
            return Token{ .kind = .symbol, .lexeme = s[start..self.i] };
        }
    }
};

fn tokenIs(tok: Token, kind: TokenKind, lexeme: []const u8) bool {
    return tok.kind == kind and std.mem.eql(u8, tok.lexeme, lexeme);
}

fn parseAllowedType(ts: *Tokenizer, which: enum { param, ret }) ErrorList!ast.TypeInfo {
    const tok_opt = try ts.next();
    if (tok_opt == null) switch (which) {
        .param => return error.InvalidParamType,
        .ret => return error.InvalidReturnType,
    };
    const tok = tok_opt.?;

    if (tok.kind == .ident) {
        if (std.mem.eql(u8, tok.lexeme, "i64")) return .{ .base = .Int, .is_mutable = false };
        if (std.mem.eql(u8, tok.lexeme, "f64")) return .{ .base = .Float, .is_mutable = false };
        if (std.mem.eql(u8, tok.lexeme, "u8")) return .{ .base = .Byte, .is_mutable = false };
        if (std.mem.eql(u8, tok.lexeme, "bool")) return .{ .base = .Tetra, .is_mutable = false };
        if (std.mem.eql(u8, tok.lexeme, "void")) return .{ .base = .Nothing, .is_mutable = false };
    }

    // `string` in Doxa is represented as `[]const u8` in Zig.
    if (tok.kind == .symbol and std.mem.eql(u8, tok.lexeme, "[")) {
        const close = (try ts.next()) orelse return error.InlineZigNotValid;
        if (!tokenIs(close, .symbol, "]")) return error.InlineZigNotValid;
        const kw_const = (try ts.next()) orelse return error.InlineZigNotValid;
        if (!tokenIs(kw_const, .ident, "const")) return error.InlineZigNotValid;
        const u8_tok = (try ts.next()) orelse return error.InlineZigNotValid;
        if (!tokenIs(u8_tok, .ident, "u8")) return error.InlineZigNotValid;
        return .{ .base = .String, .is_mutable = false };
    }

    return switch (which) {
        .param => error.InvalidParamType,
        .ret => error.InvalidReturnType,
    };
}

fn isFnQualifier(tok: Token) bool {
    return tok.kind == .ident and (std.mem.eql(u8, tok.lexeme, "pub") or
        std.mem.eql(u8, tok.lexeme, "public") or
        std.mem.eql(u8, tok.lexeme, "export") or
        std.mem.eql(u8, tok.lexeme, "inline") or
        std.mem.eql(u8, tok.lexeme, "noinline") or
        std.mem.eql(u8, tok.lexeme, "extern"));
}

fn parseTopLevelImportConst(ts: *Tokenizer) ErrorList!void {
    // Grammar (strict):
    //   const IDENT = @import("...") ;
    const kw = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(kw, .ident, "const")) return error.InlineZigNotValid;

    const name = (try ts.next()) orelse return error.InlineZigNotValid;
    if (name.kind != .ident) return error.InlineZigNotValid;

    const eq = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(eq, .symbol, "=")) return error.InlineZigNotValid;

    const at_import = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(at_import, .ident, "@import")) return error.InlineZigNotValid;

    const lparen = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(lparen, .symbol, "(")) return error.InlineZigNotValid;

    const path = (try ts.next()) orelse return error.InlineZigNotValid;
    if (path.kind != .string_lit or !std.mem.startsWith(u8, path.lexeme, "\"")) return error.InlineZigNotValid;

    const rparen = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(rparen, .symbol, ")")) return error.InlineZigNotValid;

    const semi = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(semi, .symbol, ";")) return error.InlineZigNotValid;
}

fn parseTopLevelFnSig(allocator: std.mem.Allocator, ts: *Tokenizer) ErrorList!ast.ZigFnSig {
    // Grammar (restricted):
    //   (pub|export|inline|noinline|extern)* fn IDENT "(" (IDENT ":" AllowedType ("," IDENT ":" AllowedType)*)? ")" AllowedType "{"
    // Note: requires a body (`{ ... }`) per docs/inline.md.
    while (true) {
        const t = (try ts.peek()) orelse return error.InlineZigNotValid;
        if (!isFnQualifier(t)) break;
        _ = try ts.next();
    }

    const kw_fn = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(kw_fn, .ident, "fn")) return error.InlineZigNotValid;

    const name = (try ts.next()) orelse return error.InlineZigNotValid;
    if (name.kind != .ident) return error.InlineZigNotValid;

    const lparen = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(lparen, .symbol, "(")) return error.InlineZigNotValid;

    var param_types = std.array_list.Managed(ast.TypeInfo).init(allocator);
    errdefer param_types.deinit();

    while (true) {
        const t = (try ts.peek()) orelse return error.InlineZigNotValid;
        if (tokenIs(t, .symbol, ")")) {
            _ = try ts.next();
            break;
        }

        const param_name = (try ts.next()) orelse return error.InlineZigNotValid;
        if (param_name.kind != .ident) return error.InlineZigNotValid;

        const colon = (try ts.next()) orelse return error.InlineZigNotValid;
        if (!tokenIs(colon, .symbol, ":")) return error.InlineZigNotValid;

        const ti = try parseAllowedType(ts, .param);
        try param_types.append(ti);

        // TODO: support defaults and extra param modifiers
        const after = (try ts.peek()) orelse return error.InlineZigNotValid;
        if (tokenIs(after, .symbol, "=")) return error.InlineZigNotValid;

        if (tokenIs(after, .symbol, ",")) {
            _ = try ts.next();
            continue; // allow trailing comma before ')'
        }
        if (tokenIs(after, .symbol, ")")) {
            _ = try ts.next();
            break;
        }
        return error.InlineZigNotValid;
    }

    const ret_ti = try parseAllowedType(ts, .ret);

    // Require function body
    const body = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(body, .symbol, "{")) return error.InlineZigNotValid;

    return ast.ZigFnSig{
        .name = try allocator.dupe(u8, name.lexeme),
        .param_types = try param_types.toOwnedSlice(),
        .return_type = ret_ti,
    };
}

fn skipTopLevelFnHeader(ts: *Tokenizer) ErrorList!void {
    // Consume remaining qualifiers (public, inline, noinline, extern)
    while (true) {
        const t = (try ts.peek()) orelse return error.InlineZigNotValid;
        if (!isFnQualifier(t)) break;
        _ = try ts.next();
    }

    // fn
    const kw_fn = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(kw_fn, .ident, "fn")) return error.InlineZigNotValid;

    // name
    _ = (try ts.next()) orelse return error.InlineZigNotValid;

    // (
    const open = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(open, .symbol, "(")) return error.InlineZigNotValid;

    // skip parameter list
    var parens: usize = 1;
    while (parens > 0) {
        const t = (try ts.next()) orelse return error.InlineZigNotValid;
        if (t.kind == .symbol) {
            if (std.mem.eql(u8, t.lexeme, "(")) parens += 1;
            if (std.mem.eql(u8, t.lexeme, ")")) parens -= 1;
        }
    }

    // consume return type tokens and opening {
    while (true) {
        const t = (try ts.next()) orelse return error.InlineZigNotValid;
        if (tokenIs(t, .symbol, "{")) break;
    }
}

const TopLevelKind = enum { other, fn_exported, fn_plain };

// Classify the upcoming top-level declaration without consuming input.
fn classifyTopLevel(ts: *Tokenizer) ErrorList!TopLevelKind {
    const snap = ts.save();
    defer ts.restore(snap);
    var exported = false;
    while (try ts.peek()) |t| {
        if (isFnQualifier(t)) {
            if (std.mem.eql(u8, t.lexeme, "pub") or std.mem.eql(u8, t.lexeme, "public") or std.mem.eql(u8, t.lexeme, "export")) {
                exported = true;
            }
            _ = try ts.next();
            continue;
        }
        if (tokenIs(t, .ident, "fn")) return if (exported) .fn_exported else .fn_plain;
        return .other;
    }
    return .other;
}

// Skip a single top-level declaration of any form: a brace-bodied block
// (`fn ... {}`, `comptime {}`, `pub const S = struct {}`, ...) or a
// `;`-terminated declaration (`const x = ...;`, `extern fn f(...) T;`).
// Braces always balance in valid Zig, so terminating at a depth-0 close or a
// depth-0 `;` never desyncs; any leftover tokens are absorbed by the next call.
fn skipDeclLenient(ts: *Tokenizer) ErrorList!void {
    var brace: usize = 0;
    var saw_brace = false;
    while (try ts.next()) |t| {
        if (t.kind == .symbol and t.lexeme.len == 1) {
            switch (t.lexeme[0]) {
                '{' => {
                    brace += 1;
                    saw_brace = true;
                },
                '}' => {
                    if (brace > 0) brace -= 1;
                    if (brace == 0 and saw_brace) return;
                },
                ';' => {
                    if (brace == 0) return;
                },
                else => {},
            }
        }
    }
}

fn validateAndExtract(allocator: std.mem.Allocator, input: []const u8, lenient: bool) ErrorList![]ast.ZigFnSig {
    const trimmed = std.mem.trim(u8, input, " \t\n");
    if (trimmed.len == 0) return error.EmptyInput;

    var ts = Tokenizer.init(trimmed);

    // Strict (inline blocks, docs/zig.md):
    // - Top-level: only `const X = @import("...");` and `fn ... { ... }` (plus comments/blank lines).
    // - Function param/return types must be from the allowed scalar set.
    //
    // Lenient (file imports): tolerate any valid Zig top-level (`@cImport`, helper
    // consts/structs, non-Doxa functions); extract only the exported functions whose
    // signatures are Doxa-compatible, and skip everything else. `zig build-lib`
    // validates the remainder of the file.
    var depth: usize = 0;

    var out = std.array_list.Managed(ast.ZigFnSig).init(allocator);
    errdefer {
        for (out.items) |sig| {
            allocator.free(sig.name);
            allocator.free(sig.param_types);
        }
        out.deinit();
    }

    while (try ts.peek()) |tok| {
        if (tok.kind == .symbol and std.mem.eql(u8, tok.lexeme, "{")) {
            depth += 1;
            _ = try ts.next();
            continue;
        }
        if (tok.kind == .symbol and std.mem.eql(u8, tok.lexeme, "}")) {
            if (depth == 0) {
                if (lenient) {
                    _ = try ts.next();
                    continue;
                }
                return error.InlineZigNotValid;
            }
            depth -= 1;
            _ = try ts.next();
            continue;
        }

        if (depth != 0) {
            _ = try ts.next();
            continue;
        }

        // depth == 0
        if (lenient) {
            switch (try classifyTopLevel(&ts)) {
                .fn_exported => {
                    const snap = ts.save();
                    if (parseTopLevelFnSig(allocator, &ts)) |sig| {
                        try out.append(sig);
                        depth = 1; // opening `{` of the body was consumed
                    } else |_| {
                        // Signature isn't Doxa-compatible: skip the whole function.
                        ts.restore(snap);
                        try skipDeclLenient(&ts);
                    }
                },
                .fn_plain, .other => try skipDeclLenient(&ts),
            }
            continue;
        }

        // strict: enforce the restricted top-level set.
        if (tokenIs(tok, .ident, "const")) {
            try parseTopLevelImportConst(&ts);
            continue;
        }

        if (tokenIs(tok, .ident, "fn") or isFnQualifier(tok)) {
            if (std.mem.eql(u8, tok.lexeme, "pub") or std.mem.eql(u8, tok.lexeme, "public") or std.mem.eql(u8, tok.lexeme, "export")) {
                const sig = try parseTopLevelFnSig(allocator, &ts);
                try out.append(sig);
            } else {
                try skipTopLevelFnHeader(&ts);
            }
            depth = 1;
            continue;
        }

        return error.InlineZigNotValid;
    }

    if (depth != 0 and !lenient) return error.InlineZigNotValid;

    return try out.toOwnedSlice();
}

pub fn sanitizeAndExtract(allocator: std.mem.Allocator, input: []const u8, lenient: bool) ErrorList![]ast.ZigFnSig {
    return try validateAndExtract(allocator, input, lenient);
}
