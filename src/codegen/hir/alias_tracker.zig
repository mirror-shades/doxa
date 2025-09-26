const std = @import("std");
const HIRType = @import("soxa_types.zig").HIRType;

/// Tracks alias relationships between parameter names and target variables
pub const AliasTracker = struct {
    allocator: std.mem.Allocator,
    alias_map: std.StringHashMap(AliasInfo),
    
    pub const AliasInfo = struct {
        alias_name: []const u8,
        target_variable_name: []const u8,
        target_slot: u32,
        alias_slot: u32,
        target_type: HIRType,
        is_bound: bool,
    };

    pub fn init(allocator: std.mem.Allocator) AliasTracker {
        return AliasTracker{
            .allocator = allocator,
            .alias_map = std.StringHashMap(AliasInfo).init(allocator),
        };
    }

    pub fn deinit(self: *AliasTracker) void {
        var it = self.alias_map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.alias_name);
            self.allocator.free(entry.value_ptr.target_variable_name);
        }
        self.alias_map.deinit();
    }

    /// Register a new alias relationship
    pub fn registerAlias(self: *AliasTracker, alias_name: []const u8, target_variable_name: []const u8, target_slot: u32, alias_slot: u32, target_type: HIRType) !void {
        const alias_info = AliasInfo{
            .alias_name = try self.allocator.dupe(u8, alias_name),
            .target_variable_name = try self.allocator.dupe(u8, target_variable_name),
            .target_slot = target_slot,
            .alias_slot = alias_slot,
            .target_type = target_type,
            .is_bound = false,
        };
        
        try self.alias_map.put(alias_name, alias_info);
    }

    /// Bind an alias to its target (called when the alias parameter is set up)
    pub fn bindAlias(self: *AliasTracker, alias_name: []const u8) !void {
        if (self.alias_map.getPtr(alias_name)) |alias_info| {
            alias_info.is_bound = true;
        } else {
            return error.AliasNotFound;
        }
    }

    /// Get alias information by name
    pub fn getAlias(self: *AliasTracker, alias_name: []const u8) ?AliasInfo {
        return self.alias_map.get(alias_name);
    }

    /// Check if an alias is bound
    pub fn isAliasBound(self: *AliasTracker, alias_name: []const u8) bool {
        if (self.alias_map.get(alias_name)) |alias_info| {
            return alias_info.is_bound;
        }
        return false;
    }

    /// Get the target slot for an alias
    pub fn getTargetSlot(self: *AliasTracker, alias_name: []const u8) ?u32 {
        if (self.alias_map.get(alias_name)) |alias_info| {
            return alias_info.target_slot;
        }
        return null;
    }

    /// Get the alias slot for an alias
    pub fn getAliasSlot(self: *AliasTracker, alias_name: []const u8) ?u32 {
        if (self.alias_map.get(alias_name)) |alias_info| {
            return alias_info.alias_slot;
        }
        return null;
    }

    /// Get the target variable name for an alias
    pub fn getTargetVariable(self: *AliasTracker, alias_name: []const u8) ?[]const u8 {
        if (self.alias_map.get(alias_name)) |alias_info| {
            return alias_info.target_variable_name;
        }
        return null;
    }

    /// Get the target type for an alias
    pub fn getTargetType(self: *AliasTracker, alias_name: []const u8) ?HIRType {
        if (self.alias_map.get(alias_name)) |alias_info| {
            return alias_info.target_type;
        }
        return null;
    }

    /// Clear all aliases (useful for function scope cleanup)
    pub fn clear(self: *AliasTracker) void {
        var it = self.alias_map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.alias_name);
            self.allocator.free(entry.value_ptr.target_variable_name);
        }
        self.alias_map.clearRetainingCapacity();
    }
};
