const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TokenType = @import("../../types/token.zig").TokenType;

fn nameDistanceScore(a: []const u8, b: []const u8) usize {
    const min_len = @min(a.len, b.len);
    var mismatches: usize = 0;
    var i: usize = 0;
    while (i < min_len) : (i += 1) {
        const ac = std.ascii.toLower(a[i]);
        const bc = std.ascii.toLower(b[i]);
        if (ac != bc) mismatches += 1;
    }

    const len_penalty = if (a.len > b.len) a.len - b.len else b.len - a.len;
    return mismatches + (len_penalty * 2);
}

pub fn updateBestSuggestion(name: []const u8, candidate: []const u8, best: *?[]const u8, best_score: *usize) void {
    if (candidate.len == 0) return;
    if (std.mem.eql(u8, name, candidate)) return;

    const score = nameDistanceScore(name, candidate);
    if (score < best_score.*) {
        best_score.* = score;
        best.* = candidate;
        return;
    }

    if (score == best_score.* and best.* != null) {
        if (std.mem.lessThan(u8, candidate, best.*.?)) {
            best.* = candidate;
        }
    }
}

pub fn convertTypeToTokenType(base_type: ast.Type) TokenType {
    return switch (base_type) {
        .Int => .INT,
        .Float => .FLOAT,
        .String => .STRING,
        .Tetra => .TETRA,
        .Byte => .BYTE,
        .Array => .ARRAY,
        .Function => .FUNCTION,
        .Struct => .STRUCT,
        .Nothing => .NOTHING,
        .Map => .MAP,
        .Custom => .CUSTOM,
        .Enum => .ENUM,
        .Union => .UNION,
    };
}

pub fn isEnumTypeRequiringInitializer(type_info: *const TypeInfo, analyzer: anytype) bool {
    if (type_info.base == .Enum) return true;
    if (type_info.base == .Custom and type_info.custom_type != null) {
        if (analyzer.custom_types.get(type_info.custom_type.?.resolved())) |custom_type| {
            return custom_type.kind == .Enum;
        }
    }
    return false;
}
