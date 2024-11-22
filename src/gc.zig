const std = @import("std");
const instructions = @import("instructions.zig");

pub const GCConfig = struct {
    initial_threshold: usize = 100,
    growth_factor: f32 = 1.5,  // Grow threshold after each collection
};

pub const GC = struct {
    allocator: std.mem.Allocator,
    objects: std.ArrayList(*GCObject),
    collection_threshold: usize,
    config: GCConfig,

    pub const GCObject = struct {
        ref_count: u32,
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
            std.debug.print("GC: Incremented ref count for object of type {}, new count: {}\n", 
                .{self.type, self.ref_count});
        }
        
        pub fn decRef(self: *GCObject) void {
            self.ref_count -= 1;
            std.debug.print("GC: Decremented ref count for object of type {}, new count: {}\n", 
                .{self.type, self.ref_count});
            if (self.ref_count == 0) {
                std.debug.print("GC: Object of type {} ready for collection\n", .{self.type});
            }
        }
    };

    pub fn init(allocator: std.mem.Allocator, config: GCConfig) GC {
        return GC{
            .allocator = allocator,
            .objects = std.ArrayList(*GCObject).init(allocator),
            .collection_threshold = config.initial_threshold,
            .config = config,
        };
    }

    pub fn deinit(self: *GC) void {
        for (self.objects.items) |obj| {
            obj.decRef();
            self.allocator.destroy(obj);
        }
        self.objects.deinit();
    }

    pub fn incRef(self: *GC, obj: anytype) !void {
        const T = @TypeOf(obj);
        switch (T) {
            *instructions.ArrayValue => {
                std.debug.print("GC: Tracking array object\n", .{});
                for (self.objects.items) |gc_obj| {
                    if (gc_obj.type == .ARRAY and gc_obj.data.array == obj) {
                        gc_obj.incRef();
                        return;
                    }
                }
                const new_obj = try self.allocator.create(GCObject);
                new_obj.* = .{
                    .ref_count = 1,
                    .type = .ARRAY,
                    .data = .{ .array = obj },
                };
                try self.objects.append(new_obj);
                std.debug.print("GC: Created new array object, total objects: {}\n", 
                    .{self.objects.items.len});
            },
            []const u8, []u8 => {
                std.debug.print("GC: Tracking string: {s}\n", .{obj});
                for (self.objects.items) |gc_obj| {
                    if (gc_obj.type == .STRING and std.mem.eql(u8, gc_obj.data.string, obj)) {
                        gc_obj.incRef();
                        return;
                    }
                }
                const new_obj = try self.allocator.create(GCObject);
                new_obj.* = .{
                    .ref_count = 1,
                    .type = .STRING,
                    .data = .{ .string = obj },
                };
                try self.objects.append(new_obj);
                std.debug.print("GC: Created new string object, total objects: {}\n", 
                    .{self.objects.items.len});
            },
            else => {
                std.debug.print("GC: Unsupported type for incRef: {}\n", .{T});
                @compileError("Unsupported type for GC");
            },
        }
    }

    pub fn collect(self: *GC) void {
        const initial_count = self.objects.items.len;
        std.debug.print("\nGC: Starting collection... initial count: {}\n", .{initial_count});
        
        var i: u32 = 0;
        while (i < self.objects.items.len) {
            const obj = self.objects.items[i];
            if (obj.ref_count == 0) {
                std.debug.print("GC: Collecting object of type {}\n", .{obj.type});
                _ = self.objects.swapRemove(i);
                self.allocator.destroy(obj);
            } else {
                i += 1;
            }
        }
        const collected = initial_count - self.objects.items.len;
        std.debug.print("GC: Collected {} objects\n", .{collected});
        
        // Adjust threshold after collection
        const new_threshold = @as(usize, @intFromFloat(
            @as(f32, @floatFromInt(self.collection_threshold)) * self.config.growth_factor
        ));
        self.collection_threshold = new_threshold;
        
        std.debug.print("GC: Collection complete. New threshold: {}\n", 
            .{self.collection_threshold});
    }

    pub fn maybeCollect(self: *GC) void {
        if (self.objects.items.len > self.collection_threshold) {
            std.debug.print("\nGC: Object count ({}) exceeded threshold ({}), triggering collection\n", 
                .{self.objects.items.len, self.collection_threshold});
            self.collect();
        }
    }
};

pub fn init(allocator: std.mem.Allocator, config: GCConfig) GC {
    return GC.init(allocator, config);
}