//! The `doxa` executable: hand control to the driver.

const std = @import("std");
const driver = @import("driver/driver.zig");

pub fn main(init: std.process.Init) !void {
    return driver.run(init);
}
