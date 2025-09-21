const c = @cImport({
    @cInclude("raylib.h");
});

pub const Texture2D = c.Texture2D;
pub const Color = c.Color;
pub const Image = c.Image;
pub const InitWindow = c.InitWindow;
pub const CloseWindow = c.CloseWindow;
pub const WindowShouldClose = c.WindowShouldClose;
pub const BeginDrawing = c.BeginDrawing;
pub const EndDrawing = c.EndDrawing;
pub const ClearBackground = c.ClearBackground;
pub const SetTargetFPS = c.SetTargetFPS;
pub const DrawTexture = c.DrawTexture;
pub const DrawTextureEx = c.DrawTextureEx;
pub const Vector2 = c.Vector2;
pub const LoadImage = c.LoadImage;
pub const LoadTextureFromImage = c.LoadTextureFromImage;
pub const UnloadImage = c.UnloadImage;
pub const UnloadTexture = c.UnloadTexture;
pub const ImageResize = c.ImageResize;
pub const DARKGRAY = c.DARKGRAY;
pub const MAROON = c.MAROON;
pub const ORANGE = c.ORANGE;
pub const DARKGREEN = c.DARKGREEN;
pub const DARKBLUE = c.DARKBLUE;
pub const DARKPURPLE = c.DARKPURPLE;
pub const DARKBROWN = c.DARKBROWN;
pub const GRAY = c.GRAY;
pub const RED = c.RED;
pub const GOLD = c.GOLD;
pub const LIME = c.LIME;
pub const BLUE = c.BLUE;
pub const VIOLET = c.VIOLET;
pub const BROWN = c.BROWN;
pub const LIGHTGRAY = c.LIGHTGRAY;
pub const PINK = c.PINK;
pub const YELLOW = c.YELLOW;
pub const GREEN = c.GREEN;
pub const SKYBLUE = c.SKYBLUE;
pub const PURPLE = c.PURPLE;
pub const BEIGE = c.BEIGE;
pub const WHITE = c.WHITE;
pub const BLACK = c.BLACK;

// Doxa-friendly color type
pub const DoxaColor = struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255,
};

// Color enum for easy mapping
pub const ColorName = enum {
    DARKGRAY,
    MAROON,
    ORANGE,
    DARKGREEN,
    DARKBLUE,
    DARKPURPLE,
    DARKBROWN,
    GRAY,
    RED,
    GOLD,
    LIME,
    BLUE,
    VIOLET,
    BROWN,
    LIGHTGRAY,
    PINK,
    YELLOW,
    GREEN,
    SKYBLUE,
    PURPLE,
    BEIGE,
    WHITE,
    BLACK,
};

// Helper function to convert ColorName to DoxaColor
pub fn colorNameToDoxaColor(color_name: ColorName) DoxaColor {
    return switch (color_name) {
        .DARKGRAY => .{ .r = DARKGRAY.r, .g = DARKGRAY.g, .b = DARKGRAY.b, .a = DARKGRAY.a },
        .MAROON => .{ .r = MAROON.r, .g = MAROON.g, .b = MAROON.b, .a = MAROON.a },
        .ORANGE => .{ .r = ORANGE.r, .g = ORANGE.g, .b = ORANGE.b, .a = ORANGE.a },
        .DARKGREEN => .{ .r = DARKGREEN.r, .g = DARKGREEN.g, .b = DARKGREEN.b, .a = DARKGREEN.a },
        .DARKBLUE => .{ .r = DARKBLUE.r, .g = DARKBLUE.g, .b = DARKBLUE.b, .a = DARKBLUE.a },
        .DARKPURPLE => .{ .r = DARKPURPLE.r, .g = DARKPURPLE.g, .b = DARKPURPLE.b, .a = DARKPURPLE.a },
        .DARKBROWN => .{ .r = DARKBROWN.r, .g = DARKBROWN.g, .b = DARKBROWN.b, .a = DARKBROWN.a },
        .GRAY => .{ .r = GRAY.r, .g = GRAY.g, .b = GRAY.b, .a = GRAY.a },
        .RED => .{ .r = RED.r, .g = RED.g, .b = RED.b, .a = RED.a },
        .GOLD => .{ .r = GOLD.r, .g = GOLD.g, .b = GOLD.b, .a = GOLD.a },
        .LIME => .{ .r = LIME.r, .g = LIME.g, .b = LIME.b, .a = LIME.a },
        .BLUE => .{ .r = BLUE.r, .g = BLUE.g, .b = BLUE.b, .a = BLUE.a },
        .VIOLET => .{ .r = VIOLET.r, .g = VIOLET.g, .b = VIOLET.b, .a = VIOLET.a },
        .BROWN => .{ .r = BROWN.r, .g = BROWN.g, .b = BROWN.b, .a = BROWN.a },
        .LIGHTGRAY => .{ .r = LIGHTGRAY.r, .g = LIGHTGRAY.g, .b = LIGHTGRAY.b, .a = LIGHTGRAY.a },
        .PINK => .{ .r = PINK.r, .g = PINK.g, .b = PINK.b, .a = PINK.a },
        .YELLOW => .{ .r = YELLOW.r, .g = YELLOW.g, .b = YELLOW.b, .a = YELLOW.a },
        .GREEN => .{ .r = GREEN.r, .g = GREEN.g, .b = GREEN.b, .a = GREEN.a },
        .SKYBLUE => .{ .r = SKYBLUE.r, .g = SKYBLUE.g, .b = SKYBLUE.b, .a = SKYBLUE.a },
        .PURPLE => .{ .r = PURPLE.r, .g = PURPLE.g, .b = PURPLE.b, .a = PURPLE.a },
        .BEIGE => .{ .r = BEIGE.r, .g = BEIGE.g, .b = BEIGE.b, .a = BEIGE.a },
        .WHITE => .{ .r = WHITE.r, .g = WHITE.g, .b = WHITE.b, .a = WHITE.a },
        .BLACK => .{ .r = BLACK.r, .g = BLACK.g, .b = BLACK.b, .a = BLACK.a },
    };
}

// Helper function to convert string to ColorName
pub fn stringToColorName(color_str: []const u8) ?ColorName {
    if (std.mem.eql(u8, color_str, "DARKGRAY")) return .DARKGRAY;
    if (std.mem.eql(u8, color_str, "MAROON")) return .MAROON;
    if (std.mem.eql(u8, color_str, "ORANGE")) return .ORANGE;
    if (std.mem.eql(u8, color_str, "DARKGREEN")) return .DARKGREEN;
    if (std.mem.eql(u8, color_str, "DARKBLUE")) return .DARKBLUE;
    if (std.mem.eql(u8, color_str, "DARKPURPLE")) return .DARKPURPLE;
    if (std.mem.eql(u8, color_str, "DARKBROWN")) return .DARKBROWN;
    if (std.mem.eql(u8, color_str, "GRAY")) return .GRAY;
    if (std.mem.eql(u8, color_str, "RED")) return .RED;
    if (std.mem.eql(u8, color_str, "GOLD")) return .GOLD;
    if (std.mem.eql(u8, color_str, "LIME")) return .LIME;
    if (std.mem.eql(u8, color_str, "BLUE")) return .BLUE;
    if (std.mem.eql(u8, color_str, "VIOLET")) return .VIOLET;
    if (std.mem.eql(u8, color_str, "BROWN")) return .BROWN;
    if (std.mem.eql(u8, color_str, "LIGHTGRAY")) return .LIGHTGRAY;
    if (std.mem.eql(u8, color_str, "PINK")) return .PINK;
    if (std.mem.eql(u8, color_str, "YELLOW")) return .YELLOW;
    if (std.mem.eql(u8, color_str, "GREEN")) return .GREEN;
    if (std.mem.eql(u8, color_str, "SKYBLUE")) return .SKYBLUE;
    if (std.mem.eql(u8, color_str, "PURPLE")) return .PURPLE;
    if (std.mem.eql(u8, color_str, "BEIGE")) return .BEIGE;
    if (std.mem.eql(u8, color_str, "WHITE")) return .WHITE;
    if (std.mem.eql(u8, color_str, "BLACK")) return .BLACK;
    return null;
}

// Helper function to create DoxaColor from 4 bytes (RGBA format)
pub fn bytesToDoxaColor(r: u8, g: u8, b: u8, a: u8) DoxaColor {
    return DoxaColor{ .r = r, .g = g, .b = b, .a = a };
}

// Helper function to create DoxaColor from 3 bytes (RGB format, alpha defaults to 255)
pub fn rgbToDoxaColor(r: u8, g: u8, b: u8) DoxaColor {
    return DoxaColor{ .r = r, .g = g, .b = b, .a = 255 };
}

const std = @import("std");
pub const ImageFlipHorizontal = c.ImageFlipHorizontal;
pub const ImageFlipVertical = c.ImageFlipVertical;
pub const Rectangle = c.Rectangle;
pub const DrawTexturePro = c.DrawTexturePro;
pub const DrawCircle = c.DrawCircle;
pub const DrawCircleV = c.DrawCircleV;
pub const DrawRectangle = c.DrawRectangle;
pub const DrawRectangleV = c.DrawRectangleV;
pub const GetTime = c.GetTime;
// Input functions
pub const IsKeyPressed = c.IsKeyPressed;
pub const IsKeyDown = c.IsKeyDown;
pub const IsKeyReleased = c.IsKeyReleased;
pub const IsKeyUp = c.IsKeyUp;

// Mouse functions
pub const IsMouseButtonPressed = c.IsMouseButtonPressed;
pub const IsMouseButtonDown = c.IsMouseButtonDown;
pub const IsMouseButtonReleased = c.IsMouseButtonReleased;
pub const IsMouseButtonUp = c.IsMouseButtonUp;
pub const GetMousePosition = c.GetMousePosition;

// Mouse button constants
pub const MOUSE_BUTTON_LEFT = c.MOUSE_BUTTON_LEFT;
pub const MOUSE_BUTTON_RIGHT = c.MOUSE_BUTTON_RIGHT;
pub const MOUSE_BUTTON_MIDDLE = c.MOUSE_BUTTON_MIDDLE;

// Key constants
pub const KEY_SPACE = c.KEY_SPACE;
pub const KEY_ESCAPE = c.KEY_ESCAPE;
pub const KEY_ENTER = c.KEY_ENTER;
pub const KEY_UP = c.KEY_UP;
pub const KEY_DOWN = c.KEY_DOWN;
pub const KEY_LEFT = c.KEY_LEFT;
pub const KEY_RIGHT = c.KEY_RIGHT;

// Input state enum
pub const InputState = enum {
    NONE,
    SPACE,
    ESCAPE,
    ENTER,
    UP,
    DOWN,
    LEFT,
    RIGHT,
    MOUSE_LEFT_CLICK,
    MOUSE_RIGHT_CLICK,
};
