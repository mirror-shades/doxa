const std = @import("std");
const instructions = @import("instructions.zig");

pub const GCConfig = struct {
    initial_threshold: i32 = 3,
    growth_factor: f32 = 1.5, // Grow threshold after each collection
};

pub const GC = struct {
    allocator: std.mem.Allocator,
    objects: std.ArrayList(*GCObject),
    collection_threshold: i32,
    config: GCConfig,
    roots: std.ArrayList(*anyopaque), // Track root objects that shouldn't be collected
    paused: bool = false, // Allow pausing collection for critical sections
    marked: std.AutoHashMap(*GCObject, void),

    pub const GCObject = struct {
        gc: *GC,
        ref_count: i32,
        type: instructions.ValueType,
        data: union {
            int: i32,
            float: f32,
            string: []const u8,
            array: *instructions.ArrayValue,
            struct_val: *instructions.StructValue,
        },

        pub fn incRef(self: *GCObject) void {
            self.ref_count += 1;
            std.debug.print("GC: Incremented ref count for object of type {}, new count: {}\n", .{ self.type, self.ref_count });
        }

        pub fn decRef(self: *GCObject) void {
            self.ref_count -= 1;
            std.debug.print("GC: Decremented ref count for object of type {}, new count: {}\n", .{ self.type, self.ref_count });
            if (self.ref_count == 0) {
                std.debug.print("GC: Object of type {} ready for collection\n", .{self.type});
                self.gc.collect();
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, config: GCConfig) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(*GCObject).init(allocator),
            .roots = std.ArrayList(*anyopaque).init(allocator), // Initialize roots
            .collection_threshold = config.initial_threshold,
            .config = config,
            .paused = false,
            .marked = std.AutoHashMap(*GCObject, void).init(allocator),
        };
    }

    pub fn deinit(self: *GC) void {
        self.objects.deinit();
        self.roots.deinit();
        self.marked.deinit();
    }

    pub fn incRef(self: *GC, obj: anytype) !void {
        const T = @TypeOf(obj);
        var found_obj: ?*GCObject = null; // Track the object we're working with

        switch (T) {
            *instructions.ArrayValue => {
                std.debug.print("GC: Tracking array object\n", .{});
                for (self.objects.items) |gc_obj| {
                    if (gc_obj.type == .ARRAY and gc_obj.data.array == obj) {
                        gc_obj.incRef();
                        found_obj = gc_obj;
                        return;
                    }
                }
                const gc_obj = try self.allocator.create(GCObject);
                gc_obj.* = .{
                    .gc = self,
                    .ref_count = 1,
                    .type = .ARRAY,
                    .data = .{ .array = obj },
                };
                try self.objects.append(gc_obj);
                found_obj = gc_obj;
                std.debug.print("GC: Created new array object, total objects: {}\n", .{self.objects.items.len});
            },
            []const u8, []u8 => {
                std.debug.print("GC: Tracking string: {s}\n", .{obj});
                for (self.objects.items) |gc_obj| {
                    if (gc_obj.type == .STRING and std.mem.eql(u8, gc_obj.data.string, obj)) {
                        gc_obj.incRef();
                        found_obj = gc_obj;
                        return;
                    }
                }
                const string_copy = try self.allocator.dupe(u8, obj);
                const gc_obj = try self.allocator.create(GCObject);
                gc_obj.* = .{
                    .gc = self,
                    .ref_count = 1,
                    .type = .STRING,
                    .data = .{ .string = string_copy },
                };
                try self.objects.append(gc_obj);
                found_obj = gc_obj;
                std.debug.print("GC: Created new string object, total objects: {}\n", .{self.objects.items.len});
            },
            *instructions.StructValue => {
                std.debug.print("GC: Tracking struct object\n", .{});
                for (self.objects.items) |gc_obj| {
                    if (gc_obj.type == .STRUCT and gc_obj.data.struct_val == obj) {
                        gc_obj.incRef();
                        found_obj = gc_obj;
                        return;
                    }
                }
                const gc_obj = try self.allocator.create(GCObject);
                gc_obj.* = .{
                    .gc = self,
                    .ref_count = 1,
                    .type = .STRUCT,
                    .data = .{ .struct_val = obj },
                };
                try self.objects.append(gc_obj);
                found_obj = gc_obj;
            },
            else => {
                std.debug.print("GC: Unsupported type for incRef: {}\n", .{T});
                @compileError("Unsupported type for GC");
            },
        }

        if (found_obj) |obj_ref| {
            std.debug.print("GC: Object at {*} ref count increased to {}\n", .{ obj, obj_ref.ref_count });
        }
    }
};
