const std = @import("std");

fn isValidType(token: []const u8, next_token: []const u8) bool {
    return std.mem.eql(u8, token, "i64") or
        std.mem.eql(u8, token, "f64") or
        std.mem.eql(u8, token, "u8") or
        (std.mem.eql(u8, token, "[]const") and std.mem.eql(u8, next_token, "u8")) or
        std.mem.eql(u8, token, "bool") or
        std.mem.eql(u8, token, "void");
}

fn isValidFunctionDeclaration(tokens: []const []const u8) bool {
    if (tokens.len == 0) return false;
    // Accept: fn ..., pub fn ..., export fn ..., inline fn ..., combinations like pub inline fn ...
    var idx: usize = 0;
    // Skip known qualifiers if present
    while (idx < tokens.len) : (idx += 1) {
        const t = tokens[idx];
        if (std.mem.eql(u8, t, "pub") or std.mem.eql(u8, t, "export") or std.mem.eql(u8, t, "inline") or std.mem.eql(u8, t, "noinline") or std.mem.eql(u8, t, "extern")) {
            continue;
        }
        break;
    }
    if (idx >= tokens.len) return false;
    return std.mem.eql(u8, tokens[idx], "fn");
}

pub fn parseZig(input: []const u8) !void {
    const trimmed = std.mem.trim(u8, input, " \t\n");
    if (trimmed.len == 0) {
        return error.EmptyInput;
    }

    var block_count: usize = 0;
    var inside_block = false;
    var inside_decl = false;

    var split = std.mem.splitScalar(u8, trimmed, '\n');
    while (split.next()) |line| {
        var tokens = std.array_list.Managed([]const u8).init(std.heap.page_allocator);
        defer tokens.deinit();

        var split_line = std.mem.splitSequence(u8, line, " ");
        while (split_line.next()) |token| {
            try tokens.append(token);
        }

        if (isValidFunctionDeclaration(tokens.items)) inside_decl = true;

        if (inside_decl) {
            for (tokens.items, 0..) |token, i| {
                const next_token = if (i + 1 < tokens.items.len) tokens.items[i + 1] else "";
                const next_next_token = if (i + 2 < tokens.items.len) tokens.items[i + 2] else "";
                if (std.mem.eql(u8, token, ":") or std.mem.endsWith(u8, token, ":")) {
                    if (!isValidType(next_token, next_next_token)) {
                        return error.InvalidParamType;
                    }
                }
                if (std.mem.eql(u8, token, ")") or std.mem.endsWith(u8, token, ")")) {
                    if (!isValidType(next_token, next_next_token)) {
                        return error.InvalidReturnType;
                    }
                    inside_decl = false;
                    inside_block = true;
                    break;
                }
            }
        }

        if (inside_block) {
            for (tokens.items) |token| {
                if (std.mem.eql(u8, token, "{")) {
                    block_count += 1;
                }
                if (std.mem.eql(u8, token, "}")) {
                    block_count -= 1;
                    if (block_count == 0) {
                        inside_block = false;
                        break;
                    }
                }
            }
        }

        if (tokens.items.len >= 1 and std.mem.eql(u8, tokens.items[0], "const")) {
            for (tokens.items) |token| {
                if (std.mem.startsWith(u8, token, "@import")) {
                    break;
                }
                if (std.mem.eql(u8, token, ";")) {
                    return error.InlineZigNotValid;
                }
            }
        }
    }
}
