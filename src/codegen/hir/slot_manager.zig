const std = @import("std");
const HIRType = @import("soxa_types.zig").HIRType;

/// Manages slot allocation to prevent conflicts between regular variables and aliases
pub const SlotManager = struct {
    allocator: std.mem.Allocator,
    next_regular_slot: u32,
    next_alias_slot: u32,
    slot_assignments: std.AutoHashMap(u32, SlotInfo),
    variable_slots: std.StringHashMap(u32),
    alias_slots: std.StringHashMap(u32),
    
    pub const SlotInfo = struct {
        slot_id: u32,
        variable_name: []const u8,
        slot_type: SlotType,
        hir_type: HIRType,
        is_allocated: bool,
    };

    pub const SlotType = enum {
        regular,
        alias,
        temporary,
    };

    pub fn init(allocator: std.mem.Allocator) SlotManager {
        return SlotManager{
            .allocator = allocator,
            .next_regular_slot = 0,
            .next_alias_slot = 1000, // Start alias slots at 1000 to avoid conflicts
            .slot_assignments = std.AutoHashMap(u32, SlotInfo).init(allocator),
            .variable_slots = std.StringHashMap(u32).init(allocator),
            .alias_slots = std.StringHashMap(u32).init(allocator),
        };
    }

    pub fn deinit(self: *SlotManager) void {
        var it = self.slot_assignments.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.variable_name);
        }
        self.slot_assignments.deinit();
        self.variable_slots.deinit();
        self.alias_slots.deinit();
    }

    /// Allocate a slot for a regular variable
    pub fn allocateRegularSlot(self: *SlotManager, variable_name: []const u8, hir_type: HIRType) !u32 {
        const slot_id = self.next_regular_slot;
        self.next_regular_slot += 1;

        const slot_info = SlotInfo{
            .slot_id = slot_id,
            .variable_name = try self.allocator.dupe(u8, variable_name),
            .slot_type = .regular,
            .hir_type = hir_type,
            .is_allocated = true,
        };

        try self.slot_assignments.put(slot_id, slot_info);
        try self.variable_slots.put(variable_name, slot_id);
        return slot_id;
    }

    /// Allocate a slot for an alias
    pub fn allocateAliasSlot(self: *SlotManager, alias_name: []const u8, hir_type: HIRType) !u32 {
        const slot_id = self.next_alias_slot;
        self.next_alias_slot += 1;

        const slot_info = SlotInfo{
            .slot_id = slot_id,
            .variable_name = try self.allocator.dupe(u8, alias_name),
            .slot_type = .alias,
            .hir_type = hir_type,
            .is_allocated = true,
        };

        try self.slot_assignments.put(slot_id, slot_info);
        try self.alias_slots.put(alias_name, slot_id);
        return slot_id;
    }

    /// Allocate a temporary slot (for intermediate calculations)
    pub fn allocateTemporarySlot(self: *SlotManager, hir_type: HIRType) !u32 {
        const slot_id = self.next_regular_slot;
        self.next_regular_slot += 1;

        const slot_info = SlotInfo{
            .slot_id = slot_id,
            .variable_name = try std.fmt.allocPrint(self.allocator, "__temp_{d}", .{slot_id}),
            .slot_type = .temporary,
            .hir_type = hir_type,
            .is_allocated = true,
        };

        try self.slot_assignments.put(slot_id, slot_info);
        return slot_id;
    }

    /// Get the slot ID for a variable
    pub fn getVariableSlot(self: *SlotManager, variable_name: []const u8) ?u32 {
        return self.variable_slots.get(variable_name);
    }

    /// Get the slot ID for an alias
    pub fn getAliasSlot(self: *SlotManager, alias_name: []const u8) ?u32 {
        return self.alias_slots.get(alias_name);
    }

    /// Get slot information by slot ID
    pub fn getSlotInfo(self: *SlotManager, slot_id: u32) ?SlotInfo {
        return self.slot_assignments.get(slot_id);
    }

    /// Check if a slot is allocated
    pub fn isSlotAllocated(self: *SlotManager, slot_id: u32) bool {
        if (self.slot_assignments.get(slot_id)) |slot_info| {
            return slot_info.is_allocated;
        }
        return false;
    }

    /// Check if a variable has a slot
    pub fn hasVariableSlot(self: *SlotManager, variable_name: []const u8) bool {
        return self.variable_slots.contains(variable_name);
    }

    /// Check if an alias has a slot
    pub fn hasAliasSlot(self: *SlotManager, alias_name: []const u8) bool {
        return self.alias_slots.contains(alias_name);
    }

    /// Deallocate a slot (for cleanup)
    pub fn deallocateSlot(self: *SlotManager, slot_id: u32) void {
        if (self.slot_assignments.getPtr(slot_id)) |slot_info| {
            slot_info.is_allocated = false;
            // Note: We don't remove from the map to avoid reusing slot IDs
            // This ensures that slot IDs remain unique throughout the program
        }
    }

    /// Clear all slot assignments (useful for function scope cleanup)
    pub fn clear(self: *SlotManager) void {
        var it = self.slot_assignments.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.variable_name);
        }
        self.slot_assignments.clearRetainingCapacity();
        self.variable_slots.clearRetainingCapacity();
        self.alias_slots.clearRetainingCapacity();
        self.next_regular_slot = 0;
        self.next_alias_slot = 1000;
    }

    /// Get statistics about slot usage
    pub fn getStats(self: *SlotManager) SlotStats {
        var regular_count: u32 = 0;
        var alias_count: u32 = 0;
        var temp_count: u32 = 0;

        var it = self.slot_assignments.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.is_allocated) {
                switch (entry.value_ptr.slot_type) {
                    .regular => regular_count += 1,
                    .alias => alias_count += 1,
                    .temporary => temp_count += 1,
                }
            }
        }

        return SlotStats{
            .regular_slots_used = regular_count,
            .alias_slots_used = alias_count,
            .temporary_slots_used = temp_count,
            .total_slots_used = regular_count + alias_count + temp_count,
        };
    }

    pub const SlotStats = struct {
        regular_slots_used: u32,
        alias_slots_used: u32,
        temporary_slots_used: u32,
        total_slots_used: u32,
    };
};
