const std = @import("std");
const doxa_rt = @import("runtime/doxa_rt.zig");

extern fn doxa_program_main() callconv(.c) void;

pub fn main() void {
    const argv = std.process.argsAlloc(std.heap.page_allocator) catch {
        doxa_program_main();
        return;
    };
    defer std.process.argsFree(std.heap.page_allocator, argv);
    doxa_rt.doxa_set_args(@as(i32, @intCast(argv.len)), @ptrCast(argv.ptr));
    doxa_program_main();
}
