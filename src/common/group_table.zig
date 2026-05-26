const std = @import("std");
const HIRTypes = @import("../codegen/hir/soxa_types.zig");

const GroupId = HIRTypes.GroupId;

pub const GroupTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .{},
    name_to_id: std.StringHashMapUnmanaged(GroupId) = .{},

    pub const MemberKind = enum {
        Enum,
        Struct,
        Group,
    };

    pub const Member = struct {
        qualifier: []const u8,
        kind: MemberKind,
        id: u32,
    };

    pub const Entry = struct {
        id: GroupId,
        qualified_name: []const u8,
        members: []Member,
    };

    pub fn init(allocator: std.mem.Allocator) GroupTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *GroupTable) void {
        for (self.entries.items) |entry| {
            self.allocator.free(entry.qualified_name);
            for (entry.members) |member| {
                self.allocator.free(member.qualifier);
            }
            self.allocator.free(entry.members);
        }
        self.entries.deinit(self.allocator);
        self.name_to_id.deinit(self.allocator);
    }

    pub fn registerGroup(self: *GroupTable, qualified_name: []const u8, group_members: []Member) !GroupId {
        if (self.name_to_id.get(qualified_name)) |existing| {
            return existing;
        }

        const owned_name = try self.allocator.dupe(u8, qualified_name);
        const id: GroupId = @intCast(self.entries.items.len);

        var stored_members = try self.allocator.alloc(Member, group_members.len);
        for (group_members, 0..) |member, i| {
            stored_members[i] = .{
                .qualifier = try self.allocator.dupe(u8, member.qualifier),
                .kind = member.kind,
                .id = member.id,
            };
        }

        try self.entries.append(self.allocator, Entry{
            .id = id,
            .qualified_name = owned_name,
            .members = stored_members,
        });

        try self.name_to_id.put(self.allocator, owned_name, id);
        return id;
    }

    pub fn getEntryById(self: *GroupTable, id: GroupId) ?*Entry {
        if (id >= self.entries.items.len) return null;
        return &self.entries.items[id];
    }

    pub fn getIdByName(self: *const GroupTable, qualified_name: []const u8) ?GroupId {
        return self.name_to_id.get(qualified_name);
    }

    pub fn members(self: *const GroupTable, id: GroupId) ?[]const Member {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].members;
    }

    pub fn getName(self: *const GroupTable, id: GroupId) ?[]const u8 {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].qualified_name;
    }
};
