const std = @import("std");
const ast = @import("../ast/ast.zig");
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

pub const ZigFnSig = struct {
    name: []const u8,
    param_types: []ast.TypeInfo,
    return_type: ast.TypeInfo,
};

pub fn deinitSigs(allocator: std.mem.Allocator, sigs: []ZigFnSig) void {
    for (sigs) |sig| {
        allocator.free(@constCast(sig.name));
        allocator.free(sig.param_types);
    }
    allocator.free(sigs);
}

pub fn deinitSigsShallow(allocator: std.mem.Allocator, sigs: []ZigFnSig) void {
    // Use when the caller has transferred ownership of `name`/`param_types` elsewhere.
    allocator.free(sigs);
}

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
    if (tok_opt == null) return switch (which) {
        .param => error.InvalidParamType,
        .ret => error.InvalidReturnType,
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

fn parseTopLevelFnSig(allocator: std.mem.Allocator, ts: *Tokenizer) ErrorList!ZigFnSig {
    // Grammar (restricted):
    //   (pub|export|inline|noinline|extern)* fn IDENT "(" (IDENT ":" AllowedType ("," IDENT ":" AllowedType)*)? ")" ("->")? AllowedType "{"
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

        // Disallow defaults / extra param modifiers for now.
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

    // Optional arrow (doc style): `->`
    if (try ts.peek()) |t| {
        if (tokenIs(t, .symbol, "->")) {
            _ = try ts.next();
        }
    }

    const ret_ti = try parseAllowedType(ts, .ret);

    // Require function body
    const body = (try ts.next()) orelse return error.InlineZigNotValid;
    if (!tokenIs(body, .symbol, "{")) return error.InlineZigNotValid;

    return ZigFnSig{
        .name = try allocator.dupe(u8, name.lexeme),
        .param_types = try param_types.toOwnedSlice(),
        .return_type = ret_ti,
    };
}

fn validateAndExtract(allocator: std.mem.Allocator, input: []const u8) ErrorList![]ZigFnSig {
    const trimmed = std.mem.trim(u8, input, " \t\n");
    if (trimmed.len == 0) return error.EmptyInput;

    var ts = Tokenizer.init(trimmed);

    // Rules (docs/inline.md):
    // - Top-level: only `const X = @import("...");` and `fn ... { ... }` declarations (and comments/blank lines)
    // - No other global statements/declarations.
    // - Function param/return types must be from allowed set.
    var depth: usize = 0;

    var out = std.array_list.Managed(ZigFnSig).init(allocator);
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
            if (depth == 0) return error.InlineZigNotValid;
            depth -= 1;
            _ = try ts.next();
            continue;
        }

        if (depth != 0) {
            _ = try ts.next();
            continue;
        }

        // depth == 0: enforce the restricted top-level set.
        if (tokenIs(tok, .ident, "const")) {
            try parseTopLevelImportConst(&ts);
            continue;
        }

        if (tokenIs(tok, .ident, "fn") or isFnQualifier(tok)) {
            const sig = try parseTopLevelFnSig(allocator, &ts);
            try out.append(sig);
            depth = 1; // parseTopLevelFnSig consumed the '{'
            continue;
        }

        return error.InlineZigNotValid;
    }

    if (depth != 0) return error.InlineZigNotValid;

    return try out.toOwnedSlice();
}

pub fn parseZig(allocator: std.mem.Allocator, input: []const u8) ErrorList!void {
    const sigs = try validateAndExtract(allocator, input);
    deinitSigs(allocator, sigs);
}

pub fn sanitizeAndExtract(allocator: std.mem.Allocator, input: []const u8) ErrorList![]ZigFnSig {
    return try validateAndExtract(allocator, input);
}
