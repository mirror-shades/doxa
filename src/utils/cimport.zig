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
pub const SKYBLUE = c.SKYBLUE;
pub const WHITE = c.WHITE;
pub const RED = c.RED;
pub const ORANGE = c.ORANGE;
pub const ImageFlipHorizontal = c.ImageFlipHorizontal;
pub const ImageFlipVertical = c.ImageFlipVertical;
pub const Rectangle = c.Rectangle;
pub const DrawTexturePro = c.DrawTexturePro;
pub const DrawCircle = c.DrawCircle;
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
