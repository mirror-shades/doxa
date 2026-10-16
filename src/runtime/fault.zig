//! The one fault path: every way a program fails at run time ends here, in
//! `report`. A leaf below the rest of the runtime, so an arena or a map that
//! runs out of memory reaches it as a program fault does (`doxa_fault`).

const std = @import("std");
const builtin = @import("builtin");
const instrument = @import("instrument.zig");
const win32 = @import("win32.zig");

/// Write a slice to a raw WASI file descriptor. `std.Io.Threaded`'s syscall
/// path is not usable under wasi preview1 (writes fail with ENOTCAPABLE), so —
/// like the Windows branch — the runtime uses the platform's direct write call.
pub fn writeWasiFd(fd: i32, slice: []const u8) void {
    if (slice.len == 0) return;
    var iov = [1]std.os.wasi.ciovec_t{.{ .base = slice.ptr, .len = slice.len }};
    var written: usize = 0;
    _ = std.os.wasi.fd_write(fd, &iov, 1, &written);
}

pub fn writeStderr(slice: []const u8) void {
    if (builtin.os.tag == .windows) {
        const handle = win32.GetStdHandle(win32.STD_ERROR_HANDLE);
        if (handle == std.os.windows.INVALID_HANDLE_VALUE) return;
        var written: u32 = 0;
        _ = win32.WriteFile(handle, slice.ptr, @as(u32, @intCast(slice.len)), &written, null);
        return;
    }
    if (builtin.os.tag == .wasi) {
        writeWasiFd(2, slice);
        return;
    }
    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.Io.File.stderr().writerStreaming(std.Io.Threaded.global_single_threaded.io(), &stderr_buffer);
    const stderr = &stderr_writer.interface;
    _ = stderr.write(slice) catch return;
    _ = stderr.flush() catch return;
}

// ── Faults ──
//
// Every way a program fails at run time ends in `report`, one line on
// stderr: what happened, where, then its details —
// `Index out of bounds at main.doxa:4:12: index 7, length 3`.

pub const FaultKind = enum(u32) {
    panic,
    assert_fail,
    unreachable_code,
    div_by_zero,
    overflow,
    index_out_of_bounds,
    pop_empty,
    parse_int,
    parse_float,
    parse_byte,
    /// `--instrument=memcheck`: a read of memory its arena reclaimed. The
    /// verifier rules this out, so it is a compiler or runtime bug.
    use_after_reclaim,
    /// An arena or map could not get memory from the system. Raised by the
    /// runtime, which has no site for it: under `-g` the backtrace names the
    /// program's frames.
    out_of_memory,

    fn headline(self: FaultKind) []const u8 {
        return switch (self) {
            .panic => "Panic",
            .assert_fail => "Assertion failed",
            .unreachable_code => "Reached unreachable code",
            .div_by_zero => "Division by zero",
            .overflow => "Integer overflow",
            .index_out_of_bounds => "Index out of bounds",
            .pop_empty => "Pop from an empty array",
            .parse_int => "Not a valid int",
            .parse_float => "Not a valid float",
            .parse_byte => "Not a valid byte",
            .use_after_reclaim => "Read of memory its arena reclaimed",
            .out_of_memory => "Out of memory",
        };
    }

    /// 2 is reserved for a compiler-internal trap; every program fault is 1.
    fn exitCode(self: FaultKind) u8 {
        return if (self == .unreachable_code or self == .use_after_reclaim) 2 else 1;
    }
};

/// Where a fault can happen: one constant per site in the program's IR, so
/// the runtime stays program-independent. `file` is the record's display path.
pub const DoxaFaultSite = extern struct {
    kind: u32,
    line: u32,
    column: u32,
    file: [*:0]const u8,
};


/// Set by a program built with debug information: only then do its frames
/// carry lines. The runtime's own frames always do, so their presence cannot
/// decide it.
pub var backtrace_enabled = false;

/// The return address of the program's `__doxa_enter` (`emit.zig`): the
/// return into `__doxa_init` from the call into the entry. A backtrace
/// stops there, as every frame past it is the compiler's. Null until the
/// entry runs, and in a program built without debug information.
export var doxa_entry_return: ?*const anyopaque = null;


/// The program's frames, innermost first, from the one that faulted at
/// `first`: `  in <name> at <file>:<line>:<column>`. A frame whose source is
/// not a `.doxa` file is the runtime's or the entry glue's, and is skipped.
/// The names are the display names the `!DISubprogram`s carry.
///
/// When no frame can be shown, one line says why: a backtrace that silently
/// vanishes is a bug no one sees.
fn writeBacktrace(first: usize) void {
    if (!std.options.allow_stack_tracing) return noBacktrace("stack tracing is disabled in this runtime", null);
    const info = std.debug.getSelfDebugInfo() catch |err| return noBacktrace("cannot read the program's debug information", err);
    var addresses: [64]usize = undefined;
    // A fault is a crash, so the unwinder may fall back to frame pointers
    // where unwind information fails, as Zig's own panic handler does.
    const trace = std.debug.captureCurrentStackTrace(.{ .first_address = first, .allow_unsafe_unwind = true }, &addresses);
    if (trace.return_addresses.len == 0) return noBacktrace("the stack could not be unwound to the faulting frame", null);
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const io = std.Options.debug_io;
    var shown: usize = 0;
    var failure: ?struct { address: usize, err: anyerror } = null;
    for (trace.return_addresses) |return_address| {
        if (doxa_entry_return) |end| if (return_address == @intFromPtr(end)) break;
        var symbols: std.ArrayList(std.debug.Symbol) = .empty;
        // A return address follows its call; one before it is in the call.
        info.getSymbols(io, arena.allocator(), arena.allocator(), return_address -| 1, true, &symbols) catch |err| {
            if (failure == null) failure = .{ .address = return_address -| 1, .err = err };
            continue;
        };
        for (symbols.items) |symbol| {
            const location = symbol.source_location orelse continue;
            // Line 0 is code the compiler made, which no source line stands for.
            if (location.line == 0 or !std.mem.endsWith(u8, location.file_name, ".doxa")) continue;
            writeStderr("  in ");
            writeStderr(symbol.name orelse "?");
            writeStderr(" at ");
            writeStderr(framePath(location.file_name));
            var buffer: [48]u8 = undefined;
            writeStderr(std.fmt.bufPrint(&buffer, ":{d}:{d}\n", .{ location.line, location.column }) catch unreachable);
            shown += 1;
        }
    }
    if (shown != 0) return;
    if (failure) |f| return unresolved(info, io, arena.allocator(), f.address, f.err);
    noBacktrace("no frame has a .doxa source line", null);
}

/// Why the frame at `address` has no source: no loaded module holds it, or
/// the module holding it has debug information the runtime cannot read.
fn unresolved(info: *std.debug.SelfInfo, io: std.Io, arena: std.mem.Allocator, address: usize, err: anyerror) void {
    var buffer: [512]u8 = undefined;
    const module = info.getModuleName(io, address) catch {
        return noBacktrace(std.fmt.bufPrint(&buffer, "no loaded module holds the frame at 0x{x}", .{address}) catch unreachable, err);
    };
    // The executable itself has no module name of its own.
    const name = if (module.len != 0) module else std.process.executablePathAlloc(io, arena) catch "the executable";
    noBacktrace(std.fmt.bufPrint(&buffer, "cannot read the debug information of {s} for the frame at 0x{x}", .{ name, address }) catch "cannot read a frame's debug information", err);
}

/// `  (no backtrace: <why>[: error.X])`.
fn noBacktrace(why: []const u8, err: ?anyerror) void {
    writeStderr("  (no backtrace: ");
    writeStderr(why);
    if (err) |e| {
        writeStderr(": error.");
        writeStderr(@errorName(e));
    }
    writeStderr(")\n");
}

/// A frame's file as the program names it. The emitted path is relative,
/// but each format adds a root of its own: lld-link roots a CodeView path
/// at a drive (`C:/t.doxa`), and a DWARF reader joins the compile unit's
/// `.` directory on (`./t.doxa`). Neither root is the program's.
fn framePath(path: []const u8) []const u8 {
    if (builtin.os.tag == .windows and path.len > 3 and path[1] == ':' and (path[2] == '/' or path[2] == '\\')) return path[3..];
    if (path.len > 2 and path[0] == '.' and (path[1] == '/' or path[1] == '\\')) return path[2..];
    return path;
}

/// Report a `kind` fault and terminate. `site` is where in the program it
/// happened, or null for a fault the runtime raises with no site of its own
/// (out of memory). `text` is the site's message (a panic's or an
/// assertion's) or the text that failed to parse; `aux0` and `aux1` are an
/// out-of-bounds index and length. The backtrace starts at `first`.
pub fn report(kind: FaultKind, site: ?*const DoxaFaultSite, text: []const u8, aux0: i64, aux1: i64, first: usize) noreturn {
    var buffer: [128]u8 = undefined;
    writeStderr(kind.headline());
    if (site) |at| writeStderr(std.fmt.bufPrint(&buffer, " at {s}:{d}:{d}", .{ std.mem.span(at.file), at.line, at.column }) catch unreachable);
    switch (kind) {
        .panic, .assert_fail => if (text.len != 0) {
            writeStderr(": ");
            writeStderr(text);
        },
        .parse_int, .parse_float, .parse_byte => {
            writeStderr(": \"");
            writeStderr(text);
            writeStderr("\"");
        },
        .index_out_of_bounds => writeStderr(std.fmt.bufPrint(&buffer, ": index {d}, length {d}", .{ aux0, aux1 }) catch unreachable),
        .unreachable_code, .div_by_zero, .overflow, .pop_empty, .use_after_reclaim, .out_of_memory => {},
    }
    writeStderr("\n");
    if (backtrace_enabled) writeBacktrace(first);
    instrument.flush();
    std.process.exit(kind.exitCode());
}

/// An allocation failed: the out-of-memory fault, from the caller's frame.
pub fn outOfMemory() noreturn {
    report(.out_of_memory, null, "", 0, 0, @returnAddress());
}
