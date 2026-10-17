//! Read the entry source file and validate its extension.

const std = @import("std");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const constants = @import("../common/constants.zig");

pub fn read(io: std.Io, allocator: std.mem.Allocator, gpa: std.mem.Allocator, script_path: []const u8) ![]const u8 {
    return std.Io.Dir.cwd().readFileAlloc(
        io,
        script_path,
        allocator,
        .limited(constants.MAX_SOURCE_FILE_BYTES),
    ) catch |err| {
        switch (err) {
            error.FileNotFound => {
                const cwd_path = std.process.currentPathAlloc(io, gpa) catch null;
                defer if (cwd_path) |path| gpa.free(path);

                std.debug.print(
                    "Error: could not find script '{s}' (looked relative to {s})\n",
                    .{
                        script_path,
                        cwd_path orelse "the current directory",
                    },
                );
            },
            error.AccessDenied => std.debug.print(
                "Error: permission denied reading script '{s}'\n",
                .{script_path},
            ),
            error.IsDir => std.debug.print(
                "Error: '{s}' is a directory, not a Doxa source file\n",
                .{script_path},
            ),
            error.StreamTooLong => std.debug.print(
                "Error: script '{s}' exceeds the maximum size of {d} bytes\n",
                .{ script_path, constants.MAX_SOURCE_FILE_BYTES },
            ),
            else => std.debug.print(
                "Error: could not read script '{s}': {s}\n",
                .{ script_path, @errorName(err) },
            ),
        }

        std.process.exit(constants.EXIT_CODE_USAGE);
    };
}

pub fn ensureDoxa(path: []const u8, path_uri: []const u8, reporter: *Reporter) void {
    if (!std.mem.endsWith(u8, path, constants.DOXA_EXTENSION)) {
        const loc = Location{
            .file = path,
            .file_uri = path_uri,
            .range = .{
                .start_line = 0,
                .start_col = 0,
                .end_line = 0,
                .end_col = 0,
            },
        };
        reporter.reportCompileError(loc, null, "'{s}' is not a doxa file", .{path});
        std.process.exit(constants.EXIT_CODE_USAGE);
    }
}
