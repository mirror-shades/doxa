const std = @import("std");
const rl = @import("raylib.zig");

// Global application/drawing state managed implicitly
var window_open: bool = false;

pub const raylib = rl; // Re-export raw wrappers for explicit control

pub const doxa = struct {
    pub fn Init(width: i64, height: i64, fps: i64, name: []const u8) !void {
        if (window_open) return error.WindowAlreadyOpen;
        try rl.InitWindowDoxa(width, height, name);
        rl.SetTargetFPSDoxa(fps);
        window_open = true;
    }

    pub fn Draw() void {
        rl.BeginDrawing();
        // EndDrawing is implicitly deferred by the VM via defer_stacks in module call bridge
    }
};


