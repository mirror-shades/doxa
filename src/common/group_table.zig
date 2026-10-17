const std = @import("std");
const ast = @import("../ast/ast.zig");
const HIRTypes = @import("../codegen/hir/types.zig");
const TypeTable = @import("type_index.zig").TypeTable;
const ModuleGraph = @import("../module/graph.zig").ModuleGraph;

const GroupId = HIRTypes.GroupId;
const TypeRef = ast.TypeRef;

/// Every group of the compilation, by identity, with its flattened members.
pub const GroupTable = struct {
    table: TypeTable(GroupId, Entry, 0),

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
        members: []Member = &.{},

        pub fn deinit(self: *Entry, allocator: std.mem.Allocator) void {
            for (self.members) |member| allocator.free(member.qualifier);
            allocator.free(self.members);
        }
    };

    pub fn init(allocator: std.mem.Allocator) GroupTable {
        return .{ .table = .init(allocator) };
    }

    pub fn deinit(self: *GroupTable) void {
        self.table.deinit();
    }

    /// The id of `ref`, allocating a placeholder the first time it is seen.
    pub fn idFor(self: *GroupTable, ref: TypeRef) !GroupId {
        return self.table.idFor(ref);
    }

    /// Record the flattened members of `ref`. A group's members are fixed by
    /// its one declaration, so a second registration leaves them unchanged.
    pub fn registerGroup(self: *GroupTable, ref: TypeRef, group_members: []const Member) !GroupId {
        const id = try self.table.idFor(ref);
        const entry = self.table.getEntryById(id).?;
        if (entry.declared) return id;

        const stored = try self.table.allocator.alloc(Member, group_members.len);
        for (group_members, 0..) |member, i| {
            stored[i] = member;
            stored[i].qualifier = try self.table.allocator.dupe(u8, member.qualifier);
        }
        entry.members = stored;
        entry.declared = true;
        return id;
    }

    /// Give every entry its canonical key. Called once the graph is final.
    pub fn assignKeys(self: *GroupTable, graph: *const ModuleGraph) !void {
        return self.table.assignKeys(graph);
    }

    pub fn getEntryById(self: *GroupTable, id: GroupId) ?*Entry {
        return self.table.getEntryById(id);
    }

    pub fn idOf(self: *const GroupTable, ref: TypeRef) ?GroupId {
        return self.table.idOf(ref);
    }

    pub fn idByKey(self: *const GroupTable, key: []const u8) ?GroupId {
        return self.table.idByKey(key);
    }

    /// Whether `id`'s members are recorded: its declaration has been registered.
    pub fn isDeclared(self: *const GroupTable, id: GroupId) bool {
        return self.table.isDeclared(id);
    }

    pub fn members(self: *const GroupTable, id: GroupId) ?[]const Member {
        const entry = self.table.entry(id) orelse return null;
        return entry.members;
    }

    pub fn refOf(self: *const GroupTable, id: GroupId) ?TypeRef {
        return self.table.refOf(id);
    }

    pub fn keyOf(self: *const GroupTable, id: GroupId) ?[]const u8 {
        return self.table.keyOf(id);
    }

    pub fn displayName(self: *const GroupTable, id: GroupId) ?[]const u8 {
        return self.table.displayName(id);
    }
};
