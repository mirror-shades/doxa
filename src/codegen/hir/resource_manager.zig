const std = @import("std");
const HIRValue = @import("soxa_values.zig").HIRValue;

/// Manages label generation for HIR instructions
pub const LabelGenerator = struct {
    label_count: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) LabelGenerator {
        return LabelGenerator{
            .label_count = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *LabelGenerator) void {
        _ = self; // No cleanup needed for now
    }

    /// Generate a unique label with the given prefix
    pub fn generateLabel(self: *LabelGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.label_count });
        self.label_count += 1;
        return label;
    }
};

/// Manages constant values and deduplication for HIR
pub const ConstantManager = struct {
    constants: std.array_list.Managed(HIRValue),
    constant_map: std.StringHashMap(u32), // For deduplication
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ConstantManager {
        return ConstantManager{
            .constants = std.array_list.Managed(HIRValue).init(allocator),
            .constant_map = std.StringHashMap(u32).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ConstantManager) void {
        self.constants.deinit();
        self.constant_map.deinit();
    }

    /// Add a constant value and return its index
    /// TODO: Implement deduplication for identical constants
    pub fn addConstant(self: *ConstantManager, value: HIRValue) std.mem.Allocator.Error!u32 {
        const index = @as(u32, @intCast(self.constants.items.len));
        try self.constants.append(value);
        return index;
    }

    /// Get the index of an existing constant (for future deduplication)
    pub fn getConstantIndex(self: *ConstantManager, value: HIRValue) ?u32 {
        _ = self; // TODO: Implement deduplication lookup
        _ = value; // TODO: Implement deduplication lookup
        // For now, always return null to force new constant creation
        return null;
    }

    /// Get all constants as a slice (for HIR program generation)
    pub fn getConstants(self: *ConstantManager) []HIRValue {
        return self.constants.items;
    }

    /// Transfer ownership of constants to caller
    pub fn toOwnedSlice(self: *ConstantManager) std.mem.Allocator.Error![]HIRValue {
        return self.constants.toOwnedSlice();
    }
};
