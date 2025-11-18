const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;

/// Flatten union types to ensure no nested unions
pub fn flattenUnionType(allocator: std.mem.Allocator, union_type: *ast.UnionType) !*ast.UnionType {
    var flat_types = std.array_list.Managed(*ast.TypeInfo).init(allocator);
    defer flat_types.deinit();

    for (union_type.types) |member_type| {
        if (member_type.base == .Union) {
            if (member_type.union_type) |nested_union| {
                // Recursively flatten nested union
                const flattened_nested = try flattenUnionType(allocator, nested_union);
                for (flattened_nested.types) |nested_member| {
                    // Check for duplicates before adding
                    var is_duplicate = false;
                    for (flat_types.items) |existing_type| {
                        if (existing_type.base == nested_member.base) {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if (!is_duplicate) {
                        try flat_types.append(nested_member);
                    }
                }
            }
        } else {
            // Check for duplicates before adding
            var is_duplicate = false;
            for (flat_types.items) |existing_type| {
                if (existing_type.base == member_type.base) {
                    is_duplicate = true;
                    break;
                }
            }
            if (!is_duplicate) {
                try flat_types.append(member_type);
            }
        }
    }

    const flattened_union = try allocator.create(ast.UnionType);
    flattened_union.* = .{
        .types = try flat_types.toOwnedSlice(),
        .current_type_index = union_type.current_type_index,
    };

    return flattened_union;
}

/// Create a union type from multiple types
pub fn createUnionType(allocator: std.mem.Allocator, types: []*ast.TypeInfo) !*ast.TypeInfo {
    // Flatten any existing unions in the input types
    var flat_types = std.array_list.Managed(*ast.TypeInfo).init(allocator);
    defer flat_types.deinit();

    for (types) |type_info| {
        if (type_info.base == .Union) {
            if (type_info.union_type) |union_type| {
                const flattened = try flattenUnionType(allocator, union_type);
                for (flattened.types) |member| {
                    // Check for duplicates
                    var is_duplicate = false;
                    for (flat_types.items) |existing| {
                        if (existing.base == member.base) {
                            is_duplicate = true;
                            break;
                        }
                    }
                    if (!is_duplicate) {
                        try flat_types.append(member);
                    }
                }
            }
        } else {
            // Check for duplicates
            var is_duplicate = false;
            for (flat_types.items) |existing| {
                if (existing.base == type_info.base) {
                    is_duplicate = true;
                    break;
                }
            }
            if (!is_duplicate) {
                try flat_types.append(type_info);
            }
        }
    }

    // If only one type remains, return it directly (no need for union)
    if (flat_types.items.len == 1) {
        return flat_types.items[0];
    }

    // Create union type
    const union_type = try allocator.create(ast.UnionType);
    union_type.* = .{
        .types = try flat_types.toOwnedSlice(),
        .current_type_index = 0,
    };

    const type_info = try ast.TypeInfo.createDefault(allocator);
    type_info.* = .{
        .base = .Union,
        .union_type = union_type,
        .is_mutable = false,
    };

    return type_info;
}

/// Check if a union type contains Nothing as one of its members
pub fn unionContainsNothing(union_type_info: ast.TypeInfo) bool {
    if (union_type_info.base != .Union) return false;
    if (union_type_info.union_type) |union_type| {
        for (union_type.types) |member_type| {
            if (member_type.base == .Nothing) return true;
        }
    }
    return false;
}

/// Default value for unions: use the first member's default
pub fn getUnionDefaultValue(union_type: *ast.UnionType) TokenLiteral {
    if (union_type.types.len == 0) {
        return TokenLiteral{ .nothing = {} };
    }

    const first_type = union_type.types[0];
    return switch (first_type.base) {
        .Int => TokenLiteral{ .int = 0 },
        .Float => TokenLiteral{ .float = 0.0 },
        .String => TokenLiteral{ .string = "" },
        .Tetra => TokenLiteral{ .tetra = .false },
        .Byte => TokenLiteral{ .byte = 0 },
        .Nothing => TokenLiteral{ .nothing = {} },
        else => TokenLiteral{ .nothing = {} },
    };
}

/// Check if a type is compatible with a union type
pub fn isTypeCompatibleWithUnion(
    actual: *ast.TypeInfo,
    expected_union: *ast.UnionType,
    allocator: std.mem.Allocator,
    reporter: *Reporter,
    span: ast.SourceSpan,
) !bool {
    if (actual.base == .Union) {
        // Union-to-union compatibility: every member of actual must be allowed by expected
        if (actual.union_type) |act_union| {
            for (act_union.types) |act_member| {
                var member_allowed = false;
                for (expected_union.types) |exp_member| {
                    if (exp_member.base == act_member.base) {
                        member_allowed = true;
                        break;
                    }
                }
                if (!member_allowed) {
                    // Build expected list for error message
                    var type_list = std.array_list.Managed(u8).init(allocator);
                    defer type_list.deinit();
                    for (expected_union.types, 0..) |m, i| {
                        if (i > 0) try type_list.appendSlice(" | ");
                        try type_list.appendSlice(@tagName(m.base));
                    }
                    reporter.reportCompileError(
                        span.location,
                        ErrorCode.TYPE_MISMATCH,
                        "Type mismatch: expected union ({s}), got {s}",
                        .{ type_list.items, @tagName(act_member.base) },
                    );
                    return false;
                }
            }
            return true;
        }
    } else {
        // Single type - check if it's a member of the expected union
        for (expected_union.types) |member_type| {
            if (member_type.base == actual.base) {
                return true;
            }
            // Allow Custom and Struct types to be compatible when they refer to the same user-defined struct
            if ((member_type.base == .Custom and actual.base == .Struct) or
                (member_type.base == .Struct and actual.base == .Custom))
            {
                // Check if they refer to the same custom type
                if (member_type.custom_type != null and actual.custom_type != null and
                    std.mem.eql(u8, member_type.custom_type.?, actual.custom_type.?))
                {
                    return true;
                }
            }
        }
    }
    return false;
}
