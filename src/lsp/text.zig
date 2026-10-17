//! Turning LSP positions into byte offsets, and scanning the text around a
//! position for the construct a request refers to.

const std = @import("std");
const model = @import("model.zig");
const transport = @import("transport.zig");

pub const MethodRange = struct {
    start: usize,
    end: usize,
};

pub const DottedPath = struct {
    text: []const u8,
    start: usize,
    end: usize,
};

pub const CallContext = struct {
    callee: []const u8,
    active_parameter: usize,
};

pub fn computeCompletionContext(env: model.FeatureContext, params: ?model.JsonValue) model.CompletionContext {
    if (params == null) return model.CompletionContext{ .prefix = "", .kind = .None, .object_name = null };
    if (extractDocumentContext(env, params)) |ctx| {
        if (ctx.offset <= ctx.text.len) {
            return computeCompletionAtOffset(ctx.text, ctx.offset);
        }
    }
    return model.CompletionContext{ .prefix = "", .kind = .None, .object_name = null };
}

fn makeCompletionContext(
    text: []const u8,
    kind: model.CompletionKind,
    prefix: []const u8,
    object_name: ?[]const u8,
    start: usize,
    end: usize,
) model.CompletionContext {
    return .{
        .prefix = prefix,
        .kind = kind,
        .object_name = object_name,
        .replace_start = start,
        .replace_end = end,
        .start_pos = offsetToPosition(text, start),
        .end_pos = offsetToPosition(text, end),
    };
}

pub fn computeCompletionAtOffset(text: []const u8, offset: usize) model.CompletionContext {
    const end = if (offset > text.len) text.len else offset;
    if (end == 0) return makeCompletionContext(text, .None, "", null, 0, 0);

    var pos: usize = end;
    while (pos > 0) : (pos -= 1) {
        const c = text[pos - 1];
        if (c == '@') {
            return makeCompletionContext(text, .Intrinsic, text[pos - 1 .. end], null, pos - 1, end);
        }
        if (!isIdentChar(c)) break;
    }

    if (pos > 0 and text[pos - 1] == '.') {
        const member_prefix = text[pos..end];

        const obj_end: usize = pos - 1;
        if (obj_end == 0) return makeCompletionContext(text, .Dot, member_prefix, "", pos, end);

        // Walk back over a dotted path so `std.io.` yields `std.io` rather than
        // just `io`, which is what standard-library completion keys on.
        var obj_start = obj_end;
        while (obj_start > 0) {
            const prev = text[obj_start - 1];
            if (isIdentChar(prev)) {
                obj_start -= 1;
            } else if (prev == '.' and obj_start >= 2 and isIdentChar(text[obj_start - 2])) {
                obj_start -= 1;
            } else break;
        }

        return makeCompletionContext(text, .Dot, member_prefix, text[obj_start..obj_end], pos, end);
    }

    // A bare, partial identifier: complete it against the symbols in scope.
    return makeCompletionContext(text, .Word, text[pos..end], null, pos, end);
}

pub fn extractDocumentContext(env: model.FeatureContext, params: ?model.JsonValue) ?model.DocumentContext {
    const params_value = params orelse return null;
    if (params_value != .object) return null;
    const params_obj = params_value.object;

    const doc_value = params_obj.get("textDocument") orelse return null;
    if (doc_value != .object) return null;
    const doc_obj = doc_value.object;
    const uri_value = doc_obj.get("uri") orelse return null;
    if (uri_value != .string) return null;
    const document = env.documents.get(uri_value.string) orelse return null;

    const position_value = params_obj.get("position") orelse return null;
    if (position_value != .object) return null;
    const position_obj = position_value.object;

    const line_value = position_obj.get("line") orelse return null;
    const char_value = position_obj.get("character") orelse return null;
    const line = parsePositionComponent(env.allocator, line_value) orelse return null;
    const character = parsePositionComponent(env.allocator, char_value) orelse return null;

    const offset = computeOffsetFromPosition(document.text, line, character);
    return .{
        .text = document.text,
        .offset = offset,
    };
}

fn parsePositionComponent(allocator: std.mem.Allocator, value: model.JsonValue) ?usize {
    const repr = transport.jsonStringifyAlloc(allocator, value, .{}) catch return null;
    defer allocator.free(repr);
    const parsed = std.fmt.parseInt(usize, repr, 10) catch return null;
    return parsed;
}

fn computeOffsetFromPosition(text: []const u8, line: usize, character: usize) usize {
    var offset: usize = 0;
    var current_line: usize = 0;
    while (offset < text.len and current_line < line) {
        if (text[offset] == '\n') {
            current_line += 1;
        }
        offset += 1;
    }
    var current_character: usize = 0;
    while (offset < text.len and current_character < character) {
        const c = text[offset];
        if (c == '\n') break;
        if (c == '\r') {
            offset += 1;
            if (offset < text.len and text[offset] == '\n') {
                // Treat CRLF as a single newline.
            }
            break;
        }
        offset += 1;
        current_character += 1;
    }
    if (offset > text.len) offset = text.len;
    return offset;
}

pub fn findMethodRange(text: []const u8, offset: usize) ?MethodRange {
    if (findMethodStart(text, offset)) |start| {
        var end = start;
        while (end < text.len and isMethodChar(text[end])) {
            end += 1;
        }
        if (end == start) return null;
        return MethodRange{ .start = start, .end = end };
    }
    return null;
}

pub fn offsetToPosition(text: []const u8, offset: usize) model.DocumentPosition {
    var line: usize = 0;
    var character: usize = 0;
    var idx: usize = 0;
    while (idx < offset and idx < text.len) {
        const c = text[idx];
        if (c == '\n') {
            line += 1;
            character = 0;
        } else if (c == '\r') {
            line += 1;
            character = 0;
            if (idx + 1 < text.len and text[idx + 1] == '\n') {
                idx += 1;
            }
        } else {
            character += 1;
        }
        idx += 1;
    }
    return model.DocumentPosition{ .line = line, .character = character };
}

fn isMethodChar(c: u8) bool {
    return c == '@' or isIdentChar(c);
}

fn findMethodStart(text: []const u8, offset: usize) ?usize {
    if (text.len == 0) return null;
    var current = if (offset > text.len) text.len else offset;
    while (current > 0) {
        const prev = text[current - 1];
        if (prev == '@') return current - 1;
        if (!isIdentChar(prev)) break;
        current -= 1;
    }
    if (current < text.len and text[current] == '@') return current;
    return null;
}

pub fn isIdentChar(c: u8) bool {
    return std.ascii.isAlphabetic(c) or std.ascii.isDigit(c) or c == '_';
}

/// The dotted identifier (`std.http.get`) surrounding `offset`, expanded in
/// both directions so hover works mid-symbol as well as at its end.
pub fn findDottedPathAtOffset(text: []const u8, offset: usize) ?DottedPath {
    if (text.len == 0) return null;
    const clamped = @min(offset, text.len);

    var start = clamped;
    while (start > 0) {
        const c = text[start - 1];
        if (isIdentChar(c)) {
            start -= 1;
        } else if (c == '.' and start >= 2 and isIdentChar(text[start - 2])) {
            start -= 1;
        } else break;
    }

    var end = clamped;
    while (end < text.len) {
        const c = text[end];
        if (isIdentChar(c)) {
            end += 1;
        } else if (c == '.' and end + 1 < text.len and isIdentChar(text[end + 1])) {
            end += 1;
        } else break;
    }

    while (start < end and text[start] == '.') start += 1;
    while (end > start and text[end - 1] == '.') end -= 1;
    if (start >= end) return null;

    return .{ .text = text[start..end], .start = start, .end = end };
}

/// Finds the innermost call whose argument list encloses `offset`, together
/// with the zero-based index of the argument the cursor sits in. Returns null
/// when the cursor is not inside a call. A single level of string skipping
/// keeps commas and parens inside literals from being counted.
pub fn computeCallAtOffset(text: []const u8, offset: usize) ?CallContext {
    if (text.len == 0) return null;
    const end = if (offset > text.len) text.len else offset;

    var i = end;
    var depth: usize = 0;
    var active: usize = 0;
    while (i > 0) {
        i -= 1;
        const c = text[i];
        if (c == '"') {
            i = skipStringBackward(text, i);
            continue;
        }
        switch (c) {
            ')' => depth += 1,
            '(' => {
                if (depth == 0) {
                    const callee = calleeBefore(text, i) orelse return null;
                    return .{ .callee = callee, .active_parameter = active };
                }
                depth -= 1;
            },
            ',' => {
                if (depth == 0) active += 1;
            },
            else => {},
        }
    }
    return null;
}

fn skipStringBackward(text: []const u8, quote_index: usize) usize {
    var j = quote_index;
    while (j > 0) {
        j -= 1;
        if (text[j] == '"' and !isEscapedAt(text, j)) return j;
    }
    return 0;
}

fn isEscapedAt(text: []const u8, index: usize) bool {
    var backslashes: usize = 0;
    var k = index;
    while (k > 0 and text[k - 1] == '\\') : (k -= 1) backslashes += 1;
    return backslashes % 2 == 1;
}

fn calleeBefore(text: []const u8, open_paren: usize) ?[]const u8 {
    var end = open_paren;
    while (end > 0 and (text[end - 1] == ' ' or text[end - 1] == '\t')) end -= 1;

    var start = end;
    while (start > 0) {
        const c = text[start - 1];
        if (isIdentChar(c) or c == '.' or c == '@') {
            start -= 1;
        } else break;
    }

    while (start < end and text[start] == '.') start += 1;
    if (start >= end) return null;
    return text[start..end];
}
