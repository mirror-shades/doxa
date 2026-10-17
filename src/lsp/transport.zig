//! JSON-RPC message framing over a byte stream, and the response sinks the
//! server writes through.

const std = @import("std");

pub const ResponseSink = struct {
    context: *anyopaque,
    sendFn: *const fn (context: *anyopaque, payload: []const u8) anyerror!void,
};

pub const StdIoSink = struct {
    io: std.Io,
    trace_io: bool,

    pub fn init(io: std.Io, trace_io: bool) StdIoSink {
        return .{
            .io = io,
            .trace_io = trace_io,
        };
    }

    pub fn asResponseSink(self: *StdIoSink) ResponseSink {
        return .{
            .context = @ptrCast(self),
            .sendFn = StdIoSink.send,
        };
    }

    fn send(context: *anyopaque, payload: []const u8) !void {
        const self: *StdIoSink = @ptrCast(@alignCast(context));

        var stdout_buffer: [4096]u8 = undefined;
        var stdout_writer = std.Io.File.stdout().writer(
            self.io,
            &stdout_buffer,
        );
        const writer = &stdout_writer.interface;

        if (self.trace_io) {
            std.debug.print(
                "[lsp-io] writing Content-Length: {d}\n",
                .{payload.len},
            );
            std.debug.print(
                "[lsp-io] >> {s}\n",
                .{payload},
            );
        }

        try writer.print(
            "Content-Length: {d}\r\n\r\n",
            .{payload.len},
        );
        try writer.writeAll(payload);
        try writer.flush();
    }
};

pub const CaptureSink = struct {
    allocator: std.mem.Allocator,
    responses: std.array_list.Managed([]u8),

    pub fn init(allocator: std.mem.Allocator) CaptureSink {
        return .{
            .allocator = allocator,
            .responses = std.array_list.Managed([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *CaptureSink) void {
        for (self.responses.items) |payload| {
            self.allocator.free(payload);
        }
        self.responses.deinit();
    }

    pub fn asResponseSink(self: *CaptureSink) ResponseSink {
        return .{
            .context = @ptrCast(self),
            .sendFn = CaptureSink.send,
        };
    }

    fn send(context: *anyopaque, payload: []const u8) !void {
        const self: *CaptureSink = @ptrCast(@alignCast(context));
        const copy = try self.allocator.dupe(u8, payload);
        try self.responses.append(copy);
    }
};

pub fn readMessage(allocator: std.mem.Allocator, trace_io: bool, reader: anytype) ![]u8 {
    var content_length: ?usize = null;

    while (true) {
        const line = try readLineAlloc(allocator, reader);
        defer allocator.free(line);

        if (line.len == 0) {
            if (trace_io) {
                std.debug.print("[lsp-io] reached EOF while reading headers\n", .{});
            }
            return error.EndOfStream;
        }
        const trimmed = trimLine(line);
        if (trace_io) {
            if (trimmed.len == 0) {
                std.debug.print("[lsp-io] header terminator detected\n", .{});
            } else {
                std.debug.print("[lsp-io] header line: '{s}'\n", .{trimmed});
            }
        }
        if (trimmed.len == 0) {
            break;
        }

        if (std.ascii.startsWithIgnoreCase(trimmed, "content-length:")) {
            const parts = std.mem.trim(u8, trimmed["content-length:".len..], " \t");
            content_length = std.fmt.parseInt(usize, parts, 10) catch {
                return error.InvalidMessage;
            };
            if (trace_io) {
                std.debug.print("[lsp-io] parsed Content-Length: {d}\n", .{content_length.?});
            }
        }
    }

    const length = content_length orelse return error.InvalidMessage;
    if (trace_io) {
        std.debug.print("[lsp-io] reading payload ({d} bytes)\n", .{length});
    }
    const payload = try allocator.alloc(u8, length);
    _ = try std.Io.Reader.readSliceShort(@constCast(reader), payload);
    if (trace_io) {
        std.debug.print("[lsp-io] << {s}\n", .{payload});
    }
    return payload;
}

fn readLineAlloc(allocator: std.mem.Allocator, reader: anytype) ![]u8 {
    var buffer: [4096]u8 = undefined;
    var len: usize = 0;

    while (true) {
        if (len >= buffer.len) return error.StreamTooLong;

        const byte = std.Io.Reader.takeByte(@constCast(reader)) catch |err| switch (err) {
            error.EndOfStream => break,
            error.ReadFailed => return error.ReadFailed,
        };

        if (byte == '\n') break;
        buffer[len] = byte;
        len += 1;
    }

    return allocator.dupe(u8, buffer[0..len]);
}

fn trimLine(line: []const u8) []const u8 {
    if (line.len > 0 and line[line.len - 1] == '\r') {
        return line[0 .. line.len - 1];
    }
    return line;
}

pub fn writeJsonValue(writer: *std.Io.Writer, value: anytype) !void {
    try writer.print("{f}", .{std.json.fmt(value, .{})});
}

pub fn jsonStringifyAlloc(
    allocator: std.mem.Allocator,
    value: anytype,
    options: std.json.Stringify.Options,
) ![]u8 {
    var aw: std.Io.Writer.Allocating = .init(allocator);
    errdefer aw.deinit();

    try std.json.Stringify.value(
        value,
        options,
        &aw.writer,
    );

    return try aw.toOwnedSlice();
}
