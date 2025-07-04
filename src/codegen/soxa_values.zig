const HIRType = @import("soxa_types.zig").HIRType;

pub const HIRValue = union(enum(u8)) {
    int: i32,
    u8: u8,
    float: f64,
    string: []const u8,
    tetra: u8, // NEW: Direct u8 storage for tetras
    nothing,
    // Phase 1: Complex data types
    array: HIRArray,
    struct_instance: HIRStruct,
    tuple: HIRTuple,
    map: HIRMap,
    enum_variant: HIREnum,
};

pub const HIRArray = struct {
    elements: []HIRValue,
    element_type: HIRType,
    capacity: u32,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRStruct = struct {
    type_name: []const u8,
    fields: []HIRStructField,
    field_name: ?[]const u8 = null, // Track field name for nested struct access
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRStructField = struct {
    name: []const u8,
    value: HIRValue,
    field_type: HIRType,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRTuple = struct {
    elements: []HIRValue,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRMap = struct {
    entries: []HIRMapEntry,
    key_type: HIRType,
    value_type: HIRType,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIRMapEntry = struct {
    key: HIRValue,
    value: HIRValue,
    path: ?[]const u8 = @as(?[]const u8, null),
};

pub const HIREnum = struct {
    type_name: []const u8,
    variant_name: []const u8,
    variant_index: u32,
    path: ?[]const u8 = @as(?[]const u8, null),
};
