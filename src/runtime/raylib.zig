const std = @import("std");
const rl = @import("../utils/cimport.zig");

// Minimal raylib runtime wrapper. We'll expand and integrate with Doxa's
// module system later. For now, expose just the requested API surface.

pub const Color = rl.Color;

// Doxa-facing color type to avoid leaking C ABI details
pub const DoxaColor = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255,
};

pub fn InitWindow(width: c_int, height: c_int, title: [*:0]const u8) void {
    rl.InitWindow(width, height, title);
}

// Doxa-friendly InitWindow: validates i64 dims and converts to C string
pub fn InitWindowDoxa(width: i64, height: i64, title: []const u8) !void {
    if (width < 0 or height < 0) return error.InvalidArgument;
    if (width > std.math.maxInt(c_int) or height > std.math.maxInt(c_int)) return error.InvalidArgument;

    var buf: [256]u8 = undefined;
    const ztitle = try std.fmt.bufPrintZ(&buf, "{s}", .{title});
    rl.InitWindow(@intCast(width), @intCast(height), ztitle.ptr);
}

pub fn SetTargetFPSDoxa(fps: i64) void {
    rl.SetTargetFPS(@intCast(fps));
}

pub fn CloseWindow() void {
    rl.CloseWindow();
}

pub fn WindowShouldClose() bool {
    return rl.WindowShouldClose();
}

pub fn BeginDrawing() void {
    rl.BeginDrawing();
}

pub fn EndDrawing() void {
    rl.EndDrawing();
}

pub const SKYBLUE: Color = rl.SKYBLUE;

// Doxa-friendly ClearBackground accepting DoxaColor
pub fn ClearBackgroundDoxa(color: DoxaColor) void {
    rl.ClearBackground(.{ .r = color.r, .g = color.g, .b = color.b, .a = color.a });
}

// Convenience: SKYBLUE as DoxaColor
pub const SKYBLUE_DOXA: DoxaColor = .{ .r = rl.SKYBLUE.r, .g = rl.SKYBLUE.g, .b = rl.SKYBLUE.b, .a = rl.SKYBLUE.a };

pub fn ClearBackground(color: Color) void {
    rl.ClearBackground(color);
}

pub const doxa_module = struct {
    pub const name = "raylib";
    pub const functions = &.{
        .{ .name = "InitWindow", .func = InitWindowDoxa },
        .{ .name = "CloseWindow", .func = CloseWindow },
        .{ .name = "WindowShouldClose", .func = WindowShouldClose },
        .{ .name = "BeginDrawing", .func = BeginDrawing },
        .{ .name = "EndDrawing", .func = EndDrawing },
        .{ .name = "ClearBackground", .func = ClearBackgroundDoxa },
        .{ .name = "SetTargetFPS", .func = SetTargetFPSDoxa },
    };
    pub const constants = &.{
        .{ .name = "SKYBLUE", .value = .{ .color = SKYBLUE_DOXA } },
    };
};
