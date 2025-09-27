const std = @import("std");
const rl = @import("raylib.zig");

var window_open: bool = false;

pub const raylib = rl;

pub const doxa = struct {
    pub const Color = rl.DoxaColor;

    pub fn Init(width: i64, height: i64, fps: i64, name: []const u8) !void {
        if (window_open) return error.WindowAlreadyOpen;
        try rl.InitWindowDoxa(width, height, name);
        rl.SetTargetFPSDoxa(fps);
        window_open = true;
    }

    pub fn Draw() void {
        rl.BeginDrawing();
    }

    pub fn Running() bool {
        return !rl.WindowShouldClose();
    }

    pub fn ClearBackground(color: Color) void {
        rl.ClearBackgroundDoxa(color);
    }

    pub fn bytesToColor(r: u8, g: u8, b: u8, a: u8) Color {
        return rl.bytesToColor(r, g, b, a);
    }

    pub fn rgbToColor(r: u8, g: u8, b: u8) Color {
        return rl.rgbToColor(r, g, b);
    }

    pub fn stringToColor(color_str: []const u8) ?Color {
        return rl.stringToColor(color_str);
    }
};

pub const doxa_module = struct {
    pub const name = "doxa";
    pub const functions = &.{
        .{ .name = "Init", .func = doxa.Init },
        .{ .name = "Draw", .func = doxa.Draw },
        .{ .name = "Running", .func = doxa.Running },
        .{ .name = "bytesToColor", .func = doxa.bytesToColor },
        .{ .name = "rgbToColor", .func = doxa.rgbToColor },
        .{ .name = "stringToColor", .func = doxa.stringToColor },
    };
    pub const constants = &.{
        .{ .name = "DARKGRAY", .value = .{ .color = rl.colorNameToColor(.DARKGRAY) } },
        .{ .name = "MAROON", .value = .{ .color = rl.colorNameToColor(.MAROON) } },
        .{ .name = "ORANGE", .value = .{ .color = rl.colorNameToColor(.ORANGE) } },
        .{ .name = "DARKGREEN", .value = .{ .color = rl.colorNameToColor(.DARKGREEN) } },
        .{ .name = "DARKBLUE", .value = .{ .color = rl.colorNameToColor(.DARKBLUE) } },
        .{ .name = "DARKPURPLE", .value = .{ .color = rl.colorNameToColor(.DARKPURPLE) } },
        .{ .name = "DARKBROWN", .value = .{ .color = rl.colorNameToColor(.DARKBROWN) } },
        .{ .name = "GRAY", .value = .{ .color = rl.colorNameToColor(.GRAY) } },
        .{ .name = "RED", .value = .{ .color = rl.colorNameToColor(.RED) } },
        .{ .name = "GOLD", .value = .{ .color = rl.colorNameToColor(.GOLD) } },
        .{ .name = "LIME", .value = .{ .color = rl.colorNameToColor(.LIME) } },
        .{ .name = "BLUE", .value = .{ .color = rl.colorNameToColor(.BLUE) } },
        .{ .name = "VIOLET", .value = .{ .color = rl.colorNameToColor(.VIOLET) } },
        .{ .name = "BROWN", .value = .{ .color = rl.colorNameToColor(.BROWN) } },
        .{ .name = "LIGHTGRAY", .value = .{ .color = rl.colorNameToColor(.LIGHTGRAY) } },
        .{ .name = "PINK", .value = .{ .color = rl.colorNameToColor(.PINK) } },
        .{ .name = "YELLOW", .value = .{ .color = rl.colorNameToColor(.YELLOW) } },
        .{ .name = "GREEN", .value = .{ .color = rl.colorNameToColor(.GREEN) } },
        .{ .name = "SKYBLUE", .value = .{ .color = rl.colorNameToColor(.SKYBLUE) } },
        .{ .name = "PURPLE", .value = .{ .color = rl.colorNameToColor(.PURPLE) } },
        .{ .name = "BEIGE", .value = .{ .color = rl.colorNameToColor(.BEIGE) } },
        .{ .name = "WHITE", .value = .{ .color = rl.colorNameToColor(.WHITE) } },
        .{ .name = "BLACK", .value = .{ .color = rl.colorNameToColor(.BLACK) } },
    };
};
