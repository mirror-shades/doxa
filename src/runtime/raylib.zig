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

pub const SKYBLUE: Color = rl.SKYBLUE;

// Doxa-friendly ClearBackground accepting DoxaColor
pub fn ClearBackgroundDoxa(color: DoxaColor) void {
    rl.ClearBackground(.{ .r = color.r, .g = color.g, .b = color.b, .a = color.a });
}

// Helper function to convert string to DoxaColor
pub fn stringToDoxaColor(color_str: []const u8) ?DoxaColor {
    if (rl.stringToColorName(color_str)) |color_name| {
        return rl.colorNameToDoxaColor(color_name);
    }
    return null;
}

// Re-export the colorNameToDoxaColor function from cimport
pub const colorNameToDoxaColor = rl.colorNameToDoxaColor;

// Re-export the byte-based color creation functions
pub const bytesToDoxaColor = rl.bytesToDoxaColor;
pub const rgbToDoxaColor = rl.rgbToDoxaColor;

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
        .{ .name = "bytesToDoxaColor", .func = bytesToDoxaColor },
        .{ .name = "rgbToDoxaColor", .func = rgbToDoxaColor },
    };
    pub const constants = &.{
        .{ .name = "DARKGRAY", .value = .{ .color = rl.colorNameToDoxaColor(.DARKGRAY) } },
        .{ .name = "MAROON", .value = .{ .color = rl.colorNameToDoxaColor(.MAROON) } },
        .{ .name = "ORANGE", .value = .{ .color = rl.colorNameToDoxaColor(.ORANGE) } },
        .{ .name = "DARKGREEN", .value = .{ .color = rl.colorNameToDoxaColor(.DARKGREEN) } },
        .{ .name = "DARKBLUE", .value = .{ .color = rl.colorNameToDoxaColor(.DARKBLUE) } },
        .{ .name = "DARKPURPLE", .value = .{ .color = rl.colorNameToDoxaColor(.DARKPURPLE) } },
        .{ .name = "DARKBROWN", .value = .{ .color = rl.colorNameToDoxaColor(.DARKBROWN) } },
        .{ .name = "GRAY", .value = .{ .color = rl.colorNameToDoxaColor(.GRAY) } },
        .{ .name = "RED", .value = .{ .color = rl.colorNameToDoxaColor(.RED) } },
        .{ .name = "GOLD", .value = .{ .color = rl.colorNameToDoxaColor(.GOLD) } },
        .{ .name = "LIME", .value = .{ .color = rl.colorNameToDoxaColor(.LIME) } },
        .{ .name = "BLUE", .value = .{ .color = rl.colorNameToDoxaColor(.BLUE) } },
        .{ .name = "VIOLET", .value = .{ .color = rl.colorNameToDoxaColor(.VIOLET) } },
        .{ .name = "BROWN", .value = .{ .color = rl.colorNameToDoxaColor(.BROWN) } },
        .{ .name = "LIGHTGRAY", .value = .{ .color = rl.colorNameToDoxaColor(.LIGHTGRAY) } },
        .{ .name = "PINK", .value = .{ .color = rl.colorNameToDoxaColor(.PINK) } },
        .{ .name = "YELLOW", .value = .{ .color = rl.colorNameToDoxaColor(.YELLOW) } },
        .{ .name = "GREEN", .value = .{ .color = rl.colorNameToDoxaColor(.GREEN) } },
        .{ .name = "SKYBLUE", .value = .{ .color = rl.colorNameToDoxaColor(.SKYBLUE) } },
        .{ .name = "PURPLE", .value = .{ .color = rl.colorNameToDoxaColor(.PURPLE) } },
        .{ .name = "BEIGE", .value = .{ .color = rl.colorNameToDoxaColor(.BEIGE) } },
        .{ .name = "WHITE", .value = .{ .color = rl.colorNameToDoxaColor(.WHITE) } },
        .{ .name = "BLACK", .value = .{ .color = rl.colorNameToDoxaColor(.BLACK) } },
    };

    // Simple array of all valid field names for validation
    pub const valid_fields = [_][]const u8{ "InitWindow", "CloseWindow", "WindowShouldClose", "BeginDrawing", "EndDrawing", "ClearBackground", "SetTargetFPS", "DrawCircle", "DrawCircleV", "DrawRectangle", "DrawRectangleV", "GetTime", "bytesToDoxaColor", "rgbToDoxaColor", "DARKGRAY", "MAROON", "ORANGE", "DARKGREEN", "DARKBLUE", "DARKPURPLE", "DARKBROWN", "GRAY", "RED", "GOLD", "LIME", "BLUE", "VIOLET", "BROWN", "LIGHTGRAY", "PINK", "YELLOW", "GREEN", "SKYBLUE", "PURPLE", "BEIGE", "WHITE", "BLACK" };
};
