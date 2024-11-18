const std = @import("std");

pub const Reporting = struct {
    error_count: u32,
    had_error: bool,

    pub fn init() Reporting {
        return Reporting{
            .error_count = 0,
            .had_error = false,
        };
    }

    pub fn reportFatalError(self: *Reporting, comptime fmt: []const u8, args: anytype) void {
        self.had_error = true;
        self.error_count += 1;
        std.debug.print("DoxVM: ", .{});
        std.debug.print("There was a fatal error: ", .{});
        std.debug.print(fmt ++ "\n", args);
        std.debug.panic(fmt ++ "\n", args);
    }
};
