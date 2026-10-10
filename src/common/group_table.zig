const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/types.zig");
const TypeIndex = @import("type_index.zig").TypeIndex;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const GroupId = HIRTypes.GroupId;
const TypeRef = ast.TypeRef;

/// Every group of the compilation, by identity, with its flattened members.
pub const GroupTable = struct {
    allocator: std.mem.Allocator,
    entries: std.ArrayListUnmanaged(Entry) = .empty,
    index: TypeIndex(GroupId) = .{},

    pub const MemberKind = enum {
        Enum,
        Struct,
        Group,
    };

    /// A flattened member: the qualifier the group writes for it, and the
    /// member type's identity and table id.
    pub const Member = struct {
        qualifier: []const u8,
        kind: MemberKind,
        ref: TypeRef,
        id: u32,
    };

    pub const Entry = struct {
        id: GroupId,
        ref: TypeRef,
        /// Canonical codegen key (`ModuleGraph.typeKey`); set by `assignKeys`.
        key: ?[]const u8 = null,
        declared: bool = false,
        members: []Member,
    };

    pub fn init(allocator: std.mem.Allocator) GroupTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *GroupTable) void {
        for (self.entries.items) |entry| {
            for (entry.members) |member| self.allocator.free(member.qualifier);
            self.allocator.free(entry.members);
        }
        self.entries.deinit(self.allocator);
        self.index.deinit(self.allocator);
    }

    /// The id of `ref`, allocating a placeholder the first time it is seen.
    pub fn idFor(self: *GroupTable, ref: TypeRef) !GroupId {
        if (self.index.get(ref)) |id| return id;
        const id: GroupId = @intCast(self.entries.items.len);
        try self.entries.append(self.allocator, .{ .id = id, .ref = ref, .members = &.{} });
        try self.index.put(self.allocator, ref, id);
        return id;
    }

    /// Record the flattened members of `ref`. A group's members are fixed by
    /// its one declaration, so a second registration leaves them unchanged.
    pub fn registerGroup(self: *GroupTable, ref: TypeRef, group_members: []const Member) !GroupId {
        const id = try self.idFor(ref);
        const entry = &self.entries.items[id];
        if (entry.declared) return id;

        const stored = try self.allocator.alloc(Member, group_members.len);
        for (group_members, 0..) |member, i| {
            stored[i] = member;
            stored[i].qualifier = try self.allocator.dupe(u8, member.qualifier);
        }
        entry.members = stored;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *GroupTable, graph: *const ModuleGraph) !void {
        for (self.entries.items) |*entry| {
            const key = try graph.typeKey(self.allocator, entry.ref);
            entry.key = key;
            try self.index.putKey(self.allocator, key, entry.id);
        }
    }

    pub fn getEntryById(self: *GroupTable, id: GroupId) ?*Entry {
        if (id >= self.entries.items.len) return null;
        return &self.entries.items[id];
    }

    pub fn idOf(self: *const GroupTable, ref: TypeRef) ?GroupId {
        return self.index.get(ref);
    }

    pub fn idByKey(self: *const GroupTable, key: []const u8) ?GroupId {
        return self.index.getByKey(key);
    }

    /// Whether `id`'s members are recorded: its declaration has been registered.
    pub fn isDeclared(self: *const GroupTable, id: GroupId) bool {
        if (id >= self.entries.items.len) return false;
        return self.entries.items[id].declared;
    }

    pub fn members(self: *const GroupTable, id: GroupId) ?[]const Member {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].members;
    }

    pub fn refOf(self: *const GroupTable, id: GroupId) ?TypeRef {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].ref;
    }

    pub fn keyOf(self: *const GroupTable, id: GroupId) ?[]const u8 {
        if (id >= self.entries.items.len) return null;
        return self.entries.items[id].key;
    }

    pub fn displayName(self: *const GroupTable, id: GroupId) ?[]const u8 {
        const ref = self.refOf(id) orelse return null;
        return ref.name;
    }
};
