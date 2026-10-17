//! `doxa init`: scaffold a new project.

const std = @import("std");
const constants = @import("../common/constants.zig");

pub fn run(io: std.Io, project_name: ?[]const u8) !u8 {
    const cwd = std.Io.Dir.cwd();

    var dir = if (project_name) |name| dir: {
        cwd.createDir(io, name, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {
                std.debug.print("Error: '{s}' already exists\n", .{name});
                return constants.EXIT_CODE_USAGE;
            },
            else => return err,
        };
        break :dir try cwd.openDir(io, name, .{});
    } else dir: {
        if (!try isDirEmpty(io, cwd)) {
            if (!try promptYesNo(io, "Current directory is not empty. Create project here?")) {
                std.debug.print("Aborted.\n", .{});
                return 0;
            }
        }
        break :dir try cwd.openDir(io, ".", .{});
    };
    defer if (project_name != null) dir.close(io);

    try dir.createDir(io, "src", .default_dir);
    var src_dir = try dir.openDir(io, "src", .{});
    defer src_dir.close(io);

    try src_dir.writeFile(io, .{ .sub_path = "main.doxa", .data = SCAFFOLD_MAIN });
    try dir.writeFile(io, .{ .sub_path = "build.doxa", .data = SCAFFOLD_BUILD });

    if (project_name) |name| {
        std.debug.print("Initialized Doxa project in {s}/\n", .{name});
    } else {
        std.debug.print("Initialized Doxa project in current directory\n", .{});
    }
    return 0;
}

// Hello-world entry the scaffold compiles with `doxa run build.doxa`.
const SCAFFOLD_MAIN =
    \\import io from @std()
    \\
    \\io.println("Hello, World!")
    \\
;

// Canonical build script shape (Context + Builder.executable + execute) with no
// external library dependencies, so the scaffold builds out of the box.
const SCAFFOLD_BUILD =
    \\import build from @std()
    \\
    \\var c is build.Context.host()
    \\c.debug is false
    \\c.optimization is build.Optimization.Speed
    \\
    \\var exe is build.Executable.new("app", "src/main.doxa", "bin/app")
    \\
    \\c.addArtifact(exe)
    \\
    \\build.execute(c)
    \\
;

fn isDirEmpty(io: std.Io, dir: std.Io.Dir) !bool {
    var iterable = try dir.openDir(io, ".", .{ .iterate = true });
    defer iterable.close(io);
    var it = iterable.iterate();
    return (try it.next(io)) == null;
}

fn promptYesNo(io: std.Io, question: []const u8) !bool {
    std.debug.print("{s} [Y/n] ", .{question});

    var stdin_buffer: [256]u8 = undefined;
    var stdin_reader = std.Io.File.stdin().reader(io, &stdin_buffer);
    const reader = &stdin_reader.interface;

    var line: [256]u8 = undefined;
    var len: usize = 0;
    while (len < line.len) {
        const byte = std.Io.Reader.takeByte(reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (byte == '\n') break;
        line[len] = byte;
        len += 1;
    }

    const trimmed = std.mem.trim(u8, line[0..len], " \t\r");
    if (trimmed.len == 0) return true;
    return trimmed[0] == 'y' or trimmed[0] == 'Y';
}
