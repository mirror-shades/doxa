const std = @import("std");
const builtin = @import("builtin");

const BOOL = std.os.windows.BOOL;
const UINT = std.os.windows.UINT;

extern "kernel32" fn SetConsoleOutputCP(code_page: UINT) callconv(.winapi) BOOL;

/// Switch the Windows console output code page to UTF-8 so that Unicode text
/// is rendered correctly. A no-op on every other platform.
pub fn enableUtf8Console() void {
    if (builtin.os.tag == .windows) {
        _ = SetConsoleOutputCP(65001);
    }
}
