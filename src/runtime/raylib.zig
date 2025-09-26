const std = @import("std");
const rl = @import("../utils/cimport.zig");

// Minimal raylib runtime wrapper. We'll expand and integrate with Doxa's
// module system later. For now, expose just the requested API surface.

pub const Color = rl.Color;
pub const DoxaColor = rl.DoxaColor;
pub const ColorName = rl.ColorName;
pub const Vector2 = rl.Vector2;

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

// Color constants
pub const SKYBLUE: Color = rl.SKYBLUE;
pub const RED: Color = rl.RED;
pub const GREEN: Color = rl.GREEN;
pub const BLUE: Color = rl.BLUE;
pub const YELLOW: Color = rl.YELLOW;
pub const PURPLE: Color = rl.PURPLE;
pub const PINK: Color = rl.PINK;
pub const ORANGE: Color = rl.ORANGE;
pub const WHITE: Color = rl.WHITE;
pub const BLACK: Color = rl.BLACK;
pub const GRAY: Color = rl.GRAY;
pub const LIGHTGRAY: Color = rl.LIGHTGRAY;
pub const DARKGRAY: Color = rl.DARKGRAY;
pub const GOLD: Color = rl.GOLD;
pub const LIME: Color = rl.LIME;
pub const VIOLET: Color = rl.VIOLET;
pub const BROWN: Color = rl.BROWN;
pub const BEIGE: Color = rl.BEIGE;

// Doxa-friendly ClearBackground accepting DoxaColor
pub fn ClearBackgroundDoxa(color: DoxaColor) void {
    rl.ClearBackground(.{ .r = color.r, .g = color.g, .b = color.b, .a = color.a });
}

// Re-export the byte-based color creation functions
pub const bytesToColor = rl.bytesToColor;

// Clean internal aliases without "Doxa" in the name
pub fn rgbToColor(r: u8, g: u8, b: u8) DoxaColor {
    return rl.rgbToColor(r, g, b);
}
pub fn stringToColor(color_str: []const u8) ?DoxaColor {
    return rl.stringToColor(color_str);
}
pub fn colorNameToColor(color_name: ColorName) DoxaColor {
    return rl.colorNameToColor(color_name);
}

pub fn ClearBackground(color: Color) void {
    rl.ClearBackground(color);
}

// Drawing functions
pub fn DrawCircle(centerX: i32, centerY: i32, radius: f32, color: Color) void {
    rl.DrawCircle(centerX, centerY, radius, color);
}

pub fn DrawCircleV(center: Vector2, radius: f32, color: Color) void {
    rl.DrawCircleV(center, radius, color);
}

pub fn DrawRectangle(posX: i32, posY: i32, width: i32, height: i32, color: Color) void {
    rl.DrawRectangle(posX, posY, width, height, color);
}

pub fn DrawRectangleV(position: Vector2, size: Vector2, color: Color) void {
    rl.DrawRectangleV(position, size, color);
}

pub fn GetTime() f64 {
    return rl.GetTime();
}

pub fn DrawFPS(x: i32, y: i32, fps: i64) void {
    _ = fps; // raylib DrawFPS ignores explicit fps; it shows measured FPS
    rl.DrawFPS(x, y);
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
        .{ .name = "DrawCircle", .func = DrawCircle },
        .{ .name = "DrawCircleV", .func = DrawCircleV },
        .{ .name = "DrawRectangle", .func = DrawRectangle },
        .{ .name = "DrawRectangleV", .func = DrawRectangleV },
        .{ .name = "GetTime", .func = GetTime },
        .{ .name = "DrawFPS", .func = DrawFPS },
    };
    pub const constants = &.{};

    // Simple array of all valid field names for validation
    pub const valid_fields = [_][]const u8{ "InitWindow", "CloseWindow", "WindowShouldClose", "BeginDrawing", "EndDrawing", "ClearBackground", "SetTargetFPS", "DrawCircle", "DrawCircleV", "DrawRectangle", "DrawRectangleV", "GetTime", "DrawFPS" };
};
