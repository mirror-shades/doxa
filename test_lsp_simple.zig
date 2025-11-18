const std = @import("std");
const process = std.process;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    // Start the LSP server
    var child = process.Child.init(&[_][]const u8{ "doxa", "--lsp" }, allocator);
    child.stdout_behavior = .Pipe;
    child.stderr_behavior = .Pipe;
    child.stdin_behavior = .Pipe;

    try child.spawn();

    // Give it a moment to start
    std.Thread.sleep(100 * std.time.ns_per_ms);

    // Send a simple test message
    const test_message = "Hello LSP Server!\n";

    // Send input using writer interface like test_run.zig does
    {
        var stdin_buffer: [1024]u8 = undefined;
        var stdin_writer = child.stdin.?.writer(&stdin_buffer);
        const stdin = &stdin_writer.interface;

        std.debug.print("Sending test message: {s}", .{test_message});
        try stdin.writeAll(test_message);
        try stdin.flush();
    }

    // Close stdin
    child.stdin.?.close();
    child.stdin = null;

    // Give the server a moment to process
    std.Thread.sleep(500 * std.time.ns_per_ms);

    // Wait for the server to process and exit (with timeout)
    const term = child.wait() catch |err| {
        std.debug.print("Wait error: {}, killing child\n", .{err});
        _ = child.kill() catch {};
        const term2 = child.wait() catch |wait_err| {
            std.debug.print("Failed to wait for killed child: {}\n", .{wait_err});
            return;
        };
        std.debug.print("LSP server exited with: {}\n", .{term2});
        return;
    };
    std.debug.print("LSP server exited with: {}\n", .{term});

    // Try to read any remaining stderr output
    var stderr_buf: [4096]u8 = undefined;
    std.debug.print("LSP server stderr output:\n", .{});
    while (child.stderr.?.read(&stderr_buf)) |bytes_read| {
        if (bytes_read == 0) break;
        std.debug.print("STDERR: {s}", .{stderr_buf[0..bytes_read]});
    } else |err| {
        std.debug.print("Error reading stderr: {}\n", .{err});
    }

    // Try to read any stdout output
    var stdout_buf: [4096]u8 = undefined;
    std.debug.print("\nLSP server stdout output:\n", .{});
    while (child.stdout.?.read(&stdout_buf)) |bytes_read| {
        if (bytes_read == 0) break;
        std.debug.print("{s}", .{stdout_buf[0..bytes_read]});
    } else |err| {
        std.debug.print("Error reading stdout: {}\n", .{err});
    }
}
