const std = @import("std");
const SoxaTypes = @import("soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const ArrayStorageKind = SoxaTypes.ArrayStorageKind;

pub const HIRValue = union(enum) {
    int: i64,
    byte: u8,
    float: f64,
    string: []const u8,
    tetra: u8, // 0=false, 1=true, 2=both, 3=neither
    array: HIRArray,
    struct_instance: HIRStruct,
    map: HIRMap,
    enum_variant: HIREnum,
    group_instance: HIRGroup,
    union_instance: HIRUnion,
    nothing: struct {},
    storage_id_ref: u32, // Represents a storage ID for aliases
};

pub const nothing_value = HIRValue{ .nothing = .{} };

pub const ValueOwner = union(enum) {
    Runtime,
    Scope,
    Module: u16,
};

pub const HIRArray = struct {
    /// Element storage. Homogeneous arrays of scalar elements use a packed
    /// backing (one machine word/byte per element) so large fixed-size arrays
    /// (e.g. `byte[100000001]`) do not pay the cost of a full `HIRValue` tagged
    /// union per element. Everything else (strings, structs, nested arrays,
    /// mixed/unknown element types) uses the `boxed` backing.
    backing: Backing,
    element_type: HIRType,
    capacity: u32,
    length: u32 = 0,
    path: ?[]const u8 = @as(?[]const u8, null),
    nested_element_type: ?HIRType = null, // For nested arrays: what type are the nested elements?
    storage_kind: ArrayStorageKind = .dynamic,
    owner: ValueOwner = .Runtime,
    scope_id: u32 = 0,

    pub const Backing = union(enum) {
        boxed: []HIRValue,
        bytes: []u8,
        ints: []i64,
        floats: []f64,
    };

    /// The packed backing kind for a given element type. Scalar element types
    /// pack; anything else stays boxed.
    pub fn backingTagForType(element_type: HIRType) std.meta.Tag(Backing) {
        return switch (element_type) {
            .Byte, .Tetra => .bytes,
            .Int => .ints,
            .Float => .floats,
            else => .boxed,
        };
    }

    /// Allocate an uninitialized backing of `n` elements appropriate for
    /// `element_type`. Callers are responsible for initializing the contents.
    pub fn allocBacking(allocator: std.mem.Allocator, element_type: HIRType, n: usize) !Backing {
        return switch (backingTagForType(element_type)) {
            .bytes => .{ .bytes = try allocator.alloc(u8, n) },
            .ints => .{ .ints = try allocator.alloc(i64, n) },
            .floats => .{ .floats = try allocator.alloc(f64, n) },
            .boxed => .{ .boxed = try allocator.alloc(HIRValue, n) },
        };
    }

    /// Physical number of element slots in the backing (analogous to the old
    /// `elements.len`).
    pub fn backingLen(self: HIRArray) usize {
        return switch (self.backing) {
            .boxed => |s| s.len,
            .bytes => |s| s.len,
            .ints => |s| s.len,
            .floats => |s| s.len,
        };
    }

    /// Identity of the underlying storage, used for reference equality.
    pub fn backingPtr(self: HIRArray) usize {
        return switch (self.backing) {
            .boxed => |s| @intFromPtr(s.ptr),
            .bytes => |s| @intFromPtr(s.ptr),
            .ints => |s| @intFromPtr(s.ptr),
            .floats => |s| @intFromPtr(s.ptr),
        };
    }

    pub fn isBoxed(self: HIRArray) bool {
        return self.backing == .boxed;
    }

    /// Read element `i`, boxing packed scalars into an `HIRValue`.
    pub fn get(self: HIRArray, i: usize) HIRValue {
        return switch (self.backing) {
            .boxed => |s| s[i],
            .bytes => |s| if (self.element_type == .Tetra) HIRValue{ .tetra = s[i] } else HIRValue{ .byte = s[i] },
            .ints => |s| HIRValue{ .int = s[i] },
            .floats => |s| HIRValue{ .float = s[i] },
        };
    }

    /// Write element `i`, unboxing scalars into the packed backing. For packed
    /// backings, non-matching scalar values are coerced numerically; this only
    /// occurs for well-typed programs where the element type already matches.
    pub fn set(self: *HIRArray, i: usize, value: HIRValue) void {
        switch (self.backing) {
            .boxed => |s| s[i] = value,
            .bytes => |s| s[i] = scalarToByte(value),
            .ints => |s| s[i] = scalarToInt(value),
            .floats => |s| s[i] = scalarToFloat(value),
        }
    }

    /// Grow (or shrink) the backing to `new_len` slots, filling any newly added
    /// trailing slots with the element default. Does not touch `capacity` or
    /// `length`; callers update those.
    pub fn resizeBacking(self: *HIRArray, allocator: std.mem.Allocator, new_len: usize) !void {
        switch (self.backing) {
            .boxed => |s| {
                const old = s.len;
                const ns = try allocator.realloc(s, new_len);
                if (new_len > old) for (ns[old..]) |*e| {
                    e.* = nothing_value;
                };
                self.backing = .{ .boxed = ns };
            },
            .bytes => |s| {
                const old = s.len;
                const ns = try allocator.realloc(s, new_len);
                if (new_len > old) @memset(ns[old..], 0);
                self.backing = .{ .bytes = ns };
            },
            .ints => |s| {
                const old = s.len;
                const ns = try allocator.realloc(s, new_len);
                if (new_len > old) @memset(ns[old..], 0);
                self.backing = .{ .ints = ns };
            },
            .floats => |s| {
                const old = s.len;
                const ns = try allocator.realloc(s, new_len);
                if (new_len > old) @memset(ns[old..], 0);
                self.backing = .{ .floats = ns };
            },
        }
    }

    /// Shallow-copy the backing into a freshly allocated slice of the same kind.
    /// For boxed backings the `HIRValue` entries are copied by value (no deep
    /// copy), matching the previous `@memcpy`-based behavior.
    pub fn cloneBackingShallow(self: HIRArray, allocator: std.mem.Allocator) !Backing {
        return switch (self.backing) {
            .boxed => |s| .{ .boxed = try allocator.dupe(HIRValue, s) },
            .bytes => |s| .{ .bytes = try allocator.dupe(u8, s) },
            .ints => |s| .{ .ints = try allocator.dupe(i64, s) },
            .floats => |s| .{ .floats = try allocator.dupe(f64, s) },
        };
    }
};

fn scalarToByte(value: HIRValue) u8 {
    return switch (value) {
        .byte => |b| b,
        .tetra => |t| t,
        .int => |i| if (i < 0) 0 else if (i > 255) 255 else @intCast(i),
        .float => |f| if (f < 0) 0 else if (f > 255) 255 else @intFromFloat(f),
        else => 0,
    };
}

fn scalarToInt(value: HIRValue) i64 {
    return switch (value) {
        .int => |i| i,
        .byte => |b| @as(i64, b),
        .tetra => |t| @as(i64, t),
        .float => |f| @intFromFloat(f),
        else => 0,
    };
}

fn scalarToFloat(value: HIRValue) f64 {
    return switch (value) {
        .float => |f| f,
        .int => |i| @floatFromInt(i),
        .byte => |b| @floatFromInt(b),
        .tetra => |t| @floatFromInt(t),
        else => 0.0,
    };
}

pub const HIRStruct = struct {
    type_name: []const u8,
    fields: []HIRStructField,
    value_pool: []HIRValue = &.{},
    field_name: ?[]const u8 = null,
    path: ?[]const u8 = @as(?[]const u8, null),
    owner: ValueOwner = .Runtime,
    scope_id: u32 = 0,
};

pub const HIRStructField = struct {
    name: []const u8,
    value: *HIRValue,
    field_type: HIRType,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRMap = struct {
    entries: []HIRMapEntry,
    key_pool: []HIRValue = &.{},
    value_pool: []HIRValue = &.{},
    key_type: HIRType,
    value_type: HIRType,
    path: ?[]const u8 = @as(?[]const u8, null),
    else_value: ?*HIRValue = null,
    owner: ValueOwner = .Runtime,
    scope_id: u32 = 0,
};

pub const HIRMapEntry = struct {
    key: *HIRValue,
    value: *HIRValue,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIREnum = struct {
    type_name: []const u8,
    variant_name: []const u8,
    variant_index: u32,
    path: ?[]const u8 = @as(?[]const u8, null),
    scope_id: u32 = 0,
};

pub const HIRGroup = struct {
    type_name: []const u8,
    member_index: u32,
    payload: Payload,
    path: ?[]const u8 = @as(?[]const u8, null),
    scope_id: u32 = 0,

    pub const Payload = union(enum) {
        enum_variant: HIREnum,
        struct_instance: HIRStruct,
    };
};

pub const HIRUnion = struct {
    union_type_id: u32,
    member_index: u32,
    payload: *HIRValue,
    owner: ValueOwner = .Runtime,
    scope_id: u32 = 0,
};
