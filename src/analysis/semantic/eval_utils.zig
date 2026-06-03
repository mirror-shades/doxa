const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Types = @import("../../types/types.zig");
const TokenLiteral = Types.TokenLiteral;
const TokenType = @import("../../types/token.zig").TokenType;
const Token = @import("../../types/token.zig").Token;

pub fn evaluateBinaryOp(left: TokenLiteral, operator: Token, right: TokenLiteral) TokenLiteral {
    return switch (operator.type) {
        .PLUS => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l + r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                .byte => |r| TokenLiteral{ .int = l + r },
                else => TokenLiteral{ .nothing = {} },
            },
            .float => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = l + r },
                .byte => |r| TokenLiteral{ .float = l + @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l + r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) + r },
                .byte => |r| TokenLiteral{ .byte = l + r },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .MINUS => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l - r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                .byte => |r| TokenLiteral{ .int = l - r },
                else => TokenLiteral{ .nothing = {} },
            },
            .float => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = l - r },
                .byte => |r| TokenLiteral{ .float = l - @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l - r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) - r },
                .byte => |r| TokenLiteral{ .byte = l - r },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .ASTERISK => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l * r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                .byte => |r| TokenLiteral{ .int = l * r },
                else => TokenLiteral{ .nothing = {} },
            },
            .float => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = l * r },
                .byte => |r| TokenLiteral{ .float = l * @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = l * r },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) * r },
                .byte => |r| TokenLiteral{ .byte = l * r },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .SLASH => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .float => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = l / r },
                .byte => |r| TokenLiteral{ .float = l / @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                .float => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / r },
                .byte => |r| TokenLiteral{ .float = @as(f64, @floatFromInt(l)) / @as(f64, @floatFromInt(r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .MODULO => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = @mod(l, r) },
                .byte => |r| TokenLiteral{ .int = @mod(l, r) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = @mod(l, r) },
                .byte => |r| TokenLiteral{ .byte = l % r },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .DOUBLE_SLASH => switch (left) {
            .int => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = @divTrunc(l, r) },
                .byte => |r| TokenLiteral{ .int = @divTrunc(l, @as(i64, r)) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .int = @divTrunc(@as(i64, l), r) },
                .byte => |r| TokenLiteral{ .byte = l / r },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        .POWER => switch (left) {
            .int => |l| switch (right) {
                .int => |r| blk: {
                    if (r >= 0) {
                        break :blk TokenLiteral{ .int = std.math.pow(i64, l, @as(i64, @intCast(r))) };
                    }
                    break :blk TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) };
                },
                .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                else => TokenLiteral{ .nothing = {} },
            },
            .float => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                .float => |r| TokenLiteral{ .float = std.math.pow(f64, l, r) },
                .byte => |r| TokenLiteral{ .float = std.math.pow(f64, l, @as(f64, @floatFromInt(r))) },
                else => TokenLiteral{ .nothing = {} },
            },
            .byte => |l| switch (right) {
                .int => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                .float => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), r) },
                .byte => |r| TokenLiteral{ .float = std.math.pow(f64, @as(f64, @floatFromInt(l)), @as(f64, @floatFromInt(r))) },
                else => TokenLiteral{ .nothing = {} },
            },
            else => TokenLiteral{ .nothing = {} },
        },
        else => TokenLiteral{ .nothing = {} },
    };
}

pub fn evaluateUnaryOp(operator: Token, operand: TokenLiteral) TokenLiteral {
    return switch (operator.type) {
        .MINUS => switch (operand) {
            .int => |i| TokenLiteral{ .int = -i },
            .float => |f| TokenLiteral{ .float = -f },
            .byte => |b| TokenLiteral{ .int = -@as(i32, b) },
            else => TokenLiteral{ .nothing = {} },
        },
        .BANG => switch (operand) {
            .tetra => |t| TokenLiteral{ .tetra = switch (t) {
                .true => .false,
                .false => .true,
                .both => .neither,
                .neither => .both,
            } },
            else => TokenLiteral{ .nothing = {} },
        },
        else => TokenLiteral{ .nothing = {} },
    };
}

pub fn convertValueToType(value: TokenLiteral, expected_type: ast.Type) !TokenLiteral {
    return switch (expected_type) {
        .Int => switch (value) {
            .int => value,
            .byte => |b| TokenLiteral{ .int = b },
            .float => |f| TokenLiteral{ .int = @intFromFloat(f) },
            else => value,
        },
        .Byte => switch (value) {
            .int => |i| {
                if (i >= 0 and i <= 255) {
                    return TokenLiteral{ .byte = @intCast(i) };
                } else if (i < 0) {
                    return error.byteUnderflow;
                } else {
                    return error.byteOverflow;
                }
            },
            .byte => value,
            .float => |f| {
                const int_val = @as(i32, @intFromFloat(f));
                if (int_val >= 0 and int_val <= 255) {
                    return TokenLiteral{ .byte = @intCast(int_val) };
                } else if (int_val < 0) {
                    return error.byteUnderflow;
                } else {
                    return error.byteOverflow;
                }
            },
            else => value,
        },
        .Float => switch (value) {
            .int => |i| TokenLiteral{ .float = @floatFromInt(i) },
            .byte => |b| TokenLiteral{ .float = @floatFromInt(b) },
            .float => value,
            else => value,
        },
        else => value,
    };
}

pub fn defaultTypeLiteral(allocator: std.mem.Allocator, type_info: *const TypeInfo) !TokenLiteral {
    return switch (type_info.base) {
        .Int => TokenLiteral{ .int = 0 },
        .Float => TokenLiteral{ .float = 0.0 },
        .String => TokenLiteral{ .string = "" },
        .Tetra => TokenLiteral{ .tetra = .false },
        .Byte => TokenLiteral{ .byte = 0 },
        .Array => blk: {
            if (type_info.array_type) |element_type| {
                if (type_info.array_size) |size| {
                    const elements = try allocator.alloc(TokenLiteral, size);
                    for (0..size) |i| {
                        elements[i] = try defaultTypeLiteral(allocator, element_type);
                    }
                    break :blk TokenLiteral{ .array = elements };
                }
            }
            break :blk TokenLiteral{ .array = &.{} };
        },
        else => TokenLiteral{ .nothing = {} },
    };
}

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

pub fn isEnumTypeRequiringInitializer(
    type_info: *const TypeInfo,
    custom_types: std.StringHashMap(Types.CustomTypeInfo),
) bool {
    if (type_info.base == .Enum) return true;
    if (type_info.base == .Custom and type_info.custom_type != null) {
        if (custom_types.get(type_info.custom_type.?)) |custom_type| {
            return custom_type.kind == .Enum;
        }
    }
    return false;
}
