const std = @import("std");

pub const HANDLE = std.os.windows.HANDLE;
pub const DWORD = std.os.windows.DWORD;
pub const BOOL = std.os.windows.BOOL;

/// Standard device handles, as accepted by `GetStdHandle`. These are the
/// `(DWORD)-11` / `(DWORD)-12` pseudo-handles from the Win32 API.
pub const STD_OUTPUT_HANDLE: DWORD = @bitCast(@as(i32, -11));
pub const STD_ERROR_HANDLE: DWORD = @bitCast(@as(i32, -12));

/// Keeps `StackSizeInBytes.*` of the calling thread's stack for the handler of
/// its stack-overflow exception (`fault.installHandlers`).
pub extern "kernel32" fn SetThreadStackGuarantee(StackSizeInBytes: *u32) callconv(.winapi) BOOL;
pub extern "kernel32" fn SetUnhandledExceptionFilter(filter: ?*const fn (*std.os.windows.EXCEPTION_POINTERS) callconv(.winapi) c_long) callconv(.winapi) ?*const anyopaque;

pub extern "kernel32" fn GetStdHandle(nStdHandle: DWORD) callconv(.winapi) HANDLE;

pub extern "kernel32" fn WriteFile(
    hFile: HANDLE,
    lpBuffer: [*]const u8,
    nNumberOfBytesToWrite: DWORD,
    lpNumberOfBytesWritten: *DWORD,
    lpOverlapped: ?*anyopaque,
) callconv(.winapi) BOOL;
