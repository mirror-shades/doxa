const std = @import("std");
const builtin = @import("builtin");

const BOOL = std.os.windows.BOOL;
const UINT = std.os.windows.UINT;
const DWORD = std.os.windows.DWORD;
const HANDLE = std.os.windows.HANDLE;

const STD_INPUT_HANDLE: DWORD = @bitCast(@as(i32, -10));
const STD_OUTPUT_HANDLE: DWORD = @bitCast(@as(i32, -11));
const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));
const HANDLE_FLAG_INHERIT: DWORD = 0x00000001;
const INVALID_HANDLE_VALUE: HANDLE = @ptrFromInt(std.math.maxInt(usize));

extern "kernel32" fn SetConsoleOutputCP(code_page: UINT) callconv(.winapi) BOOL;
extern "kernel32" fn GetStdHandle(n_std_handle: DWORD) callconv(.winapi) HANDLE;
extern "kernel32" fn SetHandleInformation(h_object: HANDLE, mask: DWORD, flags: DWORD) callconv(.winapi) BOOL;

/// Switch the Windows console output code page to UTF-8 so that Unicode text
/// is rendered correctly. A no-op on every other platform.
pub fn enableUtf8Console() void {
    if (builtin.os.tag == .windows) {
        _ = SetConsoleOutputCP(65001);
    }
}

/// Clear `HANDLE_FLAG_INHERIT` on this process's standard handles.
///
/// Zig spawns with `CreateProcessW(..., bInheritHandles = TRUE, ...)` and no
/// `PROC_THREAD_ATTRIBUTE_HANDLE_LIST`, so every child inherits every handle in
/// this process that is marked inheritable — including the pipes a test harness
/// is handed. A harness runs hundreds of children; if any of them (or their own
/// children) outlives the process, it keeps those pipes open, and whoever is
/// reading the other end never sees EOF. Sealing our own handles removes that
/// class of stall. A no-op on every other platform.
pub fn sealStdHandles() void {
    if (builtin.os.tag != .windows) return;
    for ([_]DWORD{ STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE }) |which| {
        const handle = GetStdHandle(which);
        if (handle == INVALID_HANDLE_VALUE) continue;
        _ = SetHandleInformation(handle, HANDLE_FLAG_INHERIT, 0);
    }
}
