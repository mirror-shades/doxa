//! Union identity. A union is a set of member types: `A | B`, `B | A`, and a
//! union spelled with a nested union or a repeated member are one type, with
//! one id and one member order. `UnionTable.intern` is the only constructor of
//! a union `HIRType`. A boxed value records its member as an index into that
//! order, so every reader of a union agrees on what the index means.
//!
//! A group member is flattened into the group's own members: `string | Error`
//! is `string | IOError | ParseError | FileError`, and "is an `Error`" is a
//! test against a set of members. A union therefore never has a group member,
//! and a box always names the concrete type it holds.

const std = @import("std");
const HIRTypes = @import("../codegen/hir/types.zig");
const StructTable = @import("struct_table.zig").StructTable;
const EnumTable = @import("enum_table.zig").EnumTable;
const GroupTable = @import("group_table.zig").GroupTable;

const HIRType = HIRTypes.HIRType;
const UnionId = HIRTypes.UnionId;

pub const UnionTable = struct {
    allocator: std.mem.Allocator,
    /// Canonical member key → the union's identity.
    by_key: std.StringHashMapUnmanaged(Entry) = .empty,
    /// Member types made by flattening a group, owned by the table.
    leaves: std.ArrayListUnmanaged(*HIRType) = .empty,
    next_id: UnionId = 1,

    pub const Error = std.mem.Allocator.Error || error{
        /// A union names a group whose declaration is not registered yet, so
        /// its members are unknown.
        UndeclaredGroup,
    };

    const Entry = struct {
        id: UnionId,
        members: []const *const HIRType,
    };

    /// The tables that name a union's named members, so they order by name.
    pub const Names = struct {
        structs: *const StructTable,
        enums: *const EnumTable,
        groups: *const GroupTable,
    };

    pub fn init(allocator: std.mem.Allocator) UnionTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *UnionTable) void {
        var it = self.by_key.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.members);
        }
        self.by_key.deinit(self.allocator);
        for (self.leaves.items) |leaf| self.allocator.destroy(leaf);
        self.leaves.deinit(self.allocator);
    }

    /// The union of `members`: nested unions and groups spliced in, duplicates removed,
    /// members in canonical order — primitives, then arrays, maps and
    /// functions, then named types by name, then `nothing`. A set of one
    /// member is that member.
    ///
    /// Member types must outlive the table; the returned member slice is the
    /// table's and is the same slice for every spelling of the union.
    pub fn intern(self: *UnionTable, names: Names, members: []const *const HIRType) Error!HIRType {
        var set: std.ArrayListUnmanaged(Member) = .empty;
        defer {
            for (set.items) |member| self.allocator.free(member.key);
            set.deinit(self.allocator);
        }
        try self.collect(names, &set, members);

        std.sort.pdq(Member, set.items, names, Member.lessThan);
        var unique: usize = 0;
        for (set.items) |member| {
            if (unique > 0 and std.mem.eql(u8, set.items[unique - 1].key, member.key)) {
                self.allocator.free(member.key);
                continue;
            }
            set.items[unique] = member;
            unique += 1;
        }
        set.shrinkRetainingCapacity(unique);

        if (set.items.len == 1) return set.items[0].ty.*;

        var key: std.ArrayListUnmanaged(u8) = .empty;
        defer key.deinit(self.allocator);
        for (set.items, 0..) |member, i| {
            if (i > 0) try key.append(self.allocator, '|');
            try key.appendSlice(self.allocator, member.key);
        }

        const entry = try self.by_key.getOrPut(self.allocator, key.items);
        if (!entry.found_existing) {
            errdefer _ = self.by_key.remove(key.items);
            const stored_members = try self.allocator.alloc(*const HIRType, set.items.len);
            errdefer self.allocator.free(stored_members);
            for (set.items, stored_members) |member, *slot| slot.* = member.ty;
            entry.key_ptr.* = try self.allocator.dupe(u8, key.items);
            entry.value_ptr.* = .{ .id = self.next_id, .members = stored_members };
            self.next_id += 1;
        }
        return .{ .Union = .{ .id = entry.value_ptr.id, .members = entry.value_ptr.members } };
    }

    fn collect(self: *UnionTable, names: Names, set: *std.ArrayListUnmanaged(Member), members: []const *const HIRType) Error!void {
        for (members) |member| switch (member.*) {
            .Union => |nested| try self.collect(names, set, nested.members),
            .Group => |id| try self.collectGroup(names, set, id),
            else => {
                var key: std.ArrayListUnmanaged(u8) = .empty;
                errdefer key.deinit(self.allocator);
                try appendKey(self.allocator, &key, member.*);
                try set.append(self.allocator, .{ .ty = member, .key = try key.toOwnedSlice(self.allocator) });
            },
        };
    }

    fn collectGroup(self: *UnionTable, names: Names, set: *std.ArrayListUnmanaged(Member), id: HIRTypes.GroupId) Error!void {
        if (!names.groups.isDeclared(id)) return error.UndeclaredGroup;
        for (names.groups.members(id).?) |member| {
            const leaf: HIRType = switch (member.kind) {
                .Enum => .{ .Enum = member.id },
                .Struct => .{ .Struct = member.id },
                .Group => .{ .Group = member.id },
            };
            const leaf_ptr = try self.allocator.create(HIRType);
            errdefer self.allocator.destroy(leaf_ptr);
            leaf_ptr.* = leaf;
            try self.leaves.append(self.allocator, leaf_ptr);
            try self.collect(names, set, &.{leaf_ptr});
        }
    }

    const Member = struct {
        ty: *const HIRType,
        /// Structural identity: equal exactly when the member types are.
        key: []u8,

        fn lessThan(names: Names, a: Member, b: Member) bool {
            const rank_a = rank(a.ty.*);
            const rank_b = rank(b.ty.*);
            if (rank_a != rank_b) return rank_a < rank_b;
            if (namedName(names, a.ty.*)) |name_a| {
                const name_b = namedName(names, b.ty.*).?;
                switch (std.mem.order(u8, name_a, name_b)) {
                    .lt => return true,
                    .gt => return false,
                    .eq => {},
                }
            }
            return std.mem.lessThan(u8, a.key, b.key);
        }
    };

    fn rank(ty: HIRType) u8 {
        return switch (ty) {
            .Int => 0,
            .Byte => 1,
            .Float => 2,
            .String => 3,
            .Tetra => 4,
            .Array => 5,
            .Map => 6,
            .Function => 7,
            .Struct, .Enum, .Group => 8,
            .Union => 9,
            .Nothing => 10,
            .Unknown, .Poison => 11,
        };
    }

    fn namedName(names: Names, ty: HIRType) ?[]const u8 {
        return switch (ty) {
            .Struct => |id| names.structs.displayName(id),
            .Enum => |id| names.enums.displayName(id),
            .Group => |id| names.groups.displayName(id),
            else => null,
        };
    }

    fn appendKey(allocator: std.mem.Allocator, key: *std.ArrayListUnmanaged(u8), ty: HIRType) !void {
        switch (ty) {
            .Int => try key.append(allocator, 'i'),
            .Byte => try key.append(allocator, 'b'),
            .Float => try key.append(allocator, 'f'),
            .String => try key.append(allocator, 's'),
            .Tetra => try key.append(allocator, 't'),
            .Nothing => try key.append(allocator, 'n'),
            .Unknown => try key.append(allocator, '?'),
            .Poison => try key.append(allocator, '!'),
            .Array => |array| {
                try key.appendSlice(allocator, "A(");
                try appendKey(allocator, key, array.element.*);
                if (array.size) |size| try key.print(allocator, ";{d}", .{size});
                try key.append(allocator, ')');
            },
            .Map => |map| {
                try key.appendSlice(allocator, "M(");
                try appendKey(allocator, key, map.key.*);
                try key.append(allocator, ':');
                try appendKey(allocator, key, map.value.*);
                try key.append(allocator, ')');
            },
            .Function => |function| {
                try key.appendSlice(allocator, "F(");
                for (function.params, 0..) |param, i| {
                    if (i > 0) try key.append(allocator, ',');
                    try appendKey(allocator, key, param.*);
                }
                try key.append(allocator, ')');
                try appendKey(allocator, key, function.ret.*);
            },
            .Struct => |id| try key.print(allocator, "S{d}", .{id}),
            .Enum => |id| try key.print(allocator, "E{d}", .{id}),
            .Group => |id| try key.print(allocator, "G{d}", .{id}),
            .Union => |u| try key.print(allocator, "U{d}", .{u.id}),
        }
    }
};
