const std = @import("std");
const HIRValue = @import("soxa_values.zig").HIRValue;
pub const LabelGenerator = struct {
    label_count: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) LabelGenerator {
        return LabelGenerator{
            .label_count = 0,
            .allocator = allocator,
        };
    }

    pub fn generateLabel(self: *LabelGenerator, prefix: []const u8) ![]const u8 {
        const label = try std.fmt.allocPrint(self.allocator, "{s}_{d}", .{ prefix, self.label_count });
        self.label_count += 1;
        return label;
    }
};

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
    pub fn addConstant(self: *ConstantManager, value: HIRValue) std.mem.Allocator.Error!u32 {
        if (self.getConstantIndex(value)) |existing| {
            return existing;
        }

        const index = @as(u32, @intCast(self.constants.items.len));
        try self.constants.append(value);

        if (makeKey(self.allocator, value)) |key| {
            // Store key -> index; keep key allocated for map lifetime
            _ = try self.constant_map.put(key, index);
        } else |_| {}

        return index;
    }

    pub fn getConstantIndex(self: *ConstantManager, value: HIRValue) ?u32 {
        if (makeKey(self.allocator, value)) |key| {
            defer self.allocator.free(key);
            if (self.constant_map.get(key)) |idx| return idx;
        } else |_| {}
        return null;
    }

    pub fn getConstants(self: *ConstantManager) []HIRValue {
        return self.constants.items;
    }

    pub fn toOwnedSlice(self: *ConstantManager) std.mem.Allocator.Error![]HIRValue {
        return self.constants.toOwnedSlice();
    }
};

fn makeKey(alloc: std.mem.Allocator, value: HIRValue) ![]u8 {
    return switch (value) {
        .int => |i| try std.fmt.allocPrint(alloc, "i:{d}", .{i}),
        .byte => |b| try std.fmt.allocPrint(alloc, "b:{d}", .{b}),
        .float => |f| blk: {
            const bits: u64 = @bitCast(f);
            break :blk try std.fmt.allocPrint(alloc, "f:{x}", .{bits});
        },
        .string => |s| try std.fmt.allocPrint(alloc, "s:{s}", .{s}),
        .tetra => |t| try std.fmt.allocPrint(alloc, "t:{d}", .{t}),
        else => error.Unsupported, // Skip dedup for complex values
    };
}
