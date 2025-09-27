const std = @import("std");

pub const Phase = enum {
    NONE,
    LEXIC_A,
    PARSING,
    SEMANTIC_A,
    GENERATE_S,
    GENERATE_B,
    GENERATE_L,
    EXECUTION,
};

const Record = struct {
    phase: Phase,
    duration_ns: u64,
    payload: ?std.json.Value,
};

pub const Profiler = struct {
    records: std.StringArrayHashMap(Record),
    allocator: std.mem.Allocator,
    current_phase: Phase,
    start_time: ?u64,
    end_time: ?u64,
    active: bool,

    pub fn init(allocator: std.mem.Allocator, active: bool) Profiler {
        return .{
            .current_phase = .NONE,
            .start_time = null,
            .end_time = null,
            .records = std.StringArrayHashMap(Record).init(allocator),
            .allocator = allocator,
            .active = active,
        };
    }

    pub fn deinit(self: *Profiler) void {
        var it = self.records.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.payload != null) {
                entry.value_ptr.payload = null;
            }
        }
        self.records.deinit();
    }

    fn startTimer(self: *Profiler, phase: Phase) void {
        self.end_time = null;
        self.current_phase = phase;
        const now = std.time.nanoTimestamp();
        self.start_time = @as(u64, @intCast(now));
    }

    fn stopTimer(self: *Profiler) void {
        const now = std.time.nanoTimestamp();
        self.end_time = @as(u64, @intCast(now));
    }

    fn getDuration(self: *Profiler) ?u64 {
        self.start_time orelse return null;
        self.end_time orelse return null;
        return self.end_time - self.start_time;
    }

    fn buildProfile(self: *Profiler) std.json.Value {
        var profile = std.json.Value{ .object = std.json.ObjectMap.init(self.allocator) };

        var iterator = self.records.iterator();
        while (iterator.next()) |entry| {
            const record = entry.value_ptr.*;

            var record_json = std.json.Value{ .object = std.json.ObjectMap.init(self.allocator) };
            record_json.object.put("phase", std.json.Value{ .string = @tagName(record.phase) }) catch {
                std.debug.print("Failed to add phase to record\n", .{});
                record_json.object.deinit();
                return profile;
            };
            record_json.object.put("duration_ns", std.json.Value{ .integer = @intCast(record.duration_ns) }) catch {
                std.debug.print("Failed to add duration to record\n", .{});
                record_json.object.deinit();
                return profile;
            };
            if (record.payload) |payload| {
                record_json.object.put("payload", payload) catch {
                    std.debug.print("Failed to add payload to record\n", .{});
                    record_json.object.deinit();
                    return profile;
                };
            }

            profile.object.put(entry.key_ptr.*, record_json) catch {
                std.debug.print("Failed to add record to profile\n", .{});
                record_json.object.deinit();
                return profile;
            };
        }

        return profile;
    }

    fn addRecord(self: *Profiler, phase: Phase, payload: ?std.json.Value) void {
        if (self.start_time == null or self.end_time == null) {
            std.debug.print("Timer not started\n", .{});
            return;
        }
        const start: u64 = self.start_time orelse {
            std.debug.print("Timer not started\n", .{});
            return;
        };
        const end: u64 = self.end_time orelse {
            std.debug.print("Timer not stopped\n", .{});
            return;
        };

        const record = Record{
            .phase = phase,
            .duration_ns = end - start,
            .payload = payload,
        };
        self.records.put(@tagName(record.phase), record) catch {
            std.debug.print("Failed to add record\n", .{});
            return;
        };
    }

    pub fn startPhase(self: *Profiler, phase: Phase) void {
        if (!self.active) return;
        self.startTimer(phase);
    }

    pub fn stopPhase(self: *Profiler) void {
        if (!self.active) return;
        if (self.current_phase == .NONE) {
            std.debug.print("Phase stopped without being started\n", .{});
            return;
        }
        self.stopTimer();
        self.addRecord(self.current_phase, null);
        self.current_phase = .NONE;
        self.start_time = null;
        self.end_time = null;
    }

    pub fn dump(self: *Profiler) !void {
        if (!self.active) return;

        std.debug.print("\n", .{});
        std.debug.print("==========================================\n", .{});
        std.debug.print("Dumping profiler\n", .{});
        std.debug.print("==========================================\n", .{});
        std.debug.print("\n", .{});

        var itr = self.records.iterator();
        while (itr.next()) |entry| {
            const record = entry.value_ptr.*;
            const duration_ns = record.duration_ns;
            if (duration_ns >= 1_000_000) {
                std.debug.print("{s}:\t {}ms\n", .{ entry.key_ptr.*, duration_ns / 1_000_000 });
            } else if (duration_ns >= 1_000) {
                std.debug.print("{s}:\t {}μs\n", .{ entry.key_ptr.*, duration_ns / 1_000 });
            } else {
                std.debug.print("{s}:\t {}ns\n", .{ entry.key_ptr.*, duration_ns });
            }
        }

        std.debug.print("\n", .{});
        std.debug.print("==========================================\n", .{});
        std.debug.print("\n", .{});
    }
};
