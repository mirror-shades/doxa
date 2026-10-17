//! Turn the process arguments into a command. Parsing never executes a
//! command: usage and help errors travel back to the driver, which owns the
//! exit status.

const std = @import("std");
const Reporting = @import("../utils/reporting.zig");
const ReporterOptions = Reporting.ReporterOptions;
const constants = @import("../common/constants.zig");
const instrument = @import("../runtime/instrument.zig");
const build_config = @import("../build/config.zig");
const Options = @import("options.zig").Options;
const help = @import("help.zig");

pub const LspOptions = struct {
    reporter_options: ReporterOptions,
    trace_io: bool = false,
};

pub const LspHarnessOptions = struct {
    reporter_options: ReporterOptions,
    script_path: []const u8,
};

pub const BuildCommand = struct {
    options: Options,
};

pub const Command = union(enum) {
    init: ?[]const u8,
    report: []const []const u8,
    lsp_stdio: LspOptions,
    lsp_harness: LspHarnessOptions,
    build: BuildCommand,

    pub fn deinit(self: *Command, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .build => |*b| b.options.deinit(allocator),
            else => {},
        }
    }
};

pub fn parse(init: std.process.Init) !Command {
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len < 2) return fail("Error: No arguments provided\n", .{});

    const verb = args[1];
    if (std.mem.eql(u8, verb, "init")) {
        const project_name: ?[]const u8 = if (args.len > 2) blk: {
            if (args.len > 3) return fail("Error: `doxa init` takes at most one project name\n", .{});
            break :blk args[2];
        } else null;
        return .{ .init = project_name };
    }

    if (std.mem.eql(u8, verb, "report")) {
        return .{ .report = args[2..] };
    }

    if (std.mem.eql(u8, verb, "--lsp")) {
        var trace_io = false;
        for (args[2..]) |arg| {
            if (std.mem.eql(u8, arg, "--lsp-debug-io")) {
                trace_io = true;
            } else {
                return fail("Error: Unknown flag for --lsp: '{s}'\n", .{arg});
            }
        }
        return .{ .lsp_stdio = .{ .reporter_options = .{}, .trace_io = trace_io } };
    }

    if (std.mem.eql(u8, verb, "--lsp-debug")) {
        if (args.len < 3) return fail("Error: Provide a .doxa file for --lsp-debug\n", .{});
        if (args.len > 3) return fail("Error: Unexpected extra arguments for --lsp-debug\n", .{});
        if (!std.mem.endsWith(u8, args[2], constants.DOXA_EXTENSION)) {
            return fail("Error: '{s}' is not a .doxa file\n", .{args[2]});
        }
        return .{ .lsp_harness = .{ .reporter_options = .{}, .script_path = args[2] } };
    }

    const mode: build_config.Mode = if (std.mem.eql(u8, verb, "run"))
        .run
    else if (std.mem.eql(u8, verb, "compile"))
        .compile
    else
        return fail("Error: specify `run` or `compile`\n", .{});

    const allocator = init.gpa;
    var options = Options.init(allocator, mode);
    errdefer options.deinit(allocator);

    const raw_options = args[2..];
    var option_end = raw_options.len;
    var split_seen = false;
    for (raw_options, 0..) |arg, idx| {
        if (std.mem.eql(u8, arg, "--")) {
            option_end = idx;
            split_seen = true;
            break;
        }
    }

    if (split_seen and mode == .compile) {
        return fail("Error: `--` program arguments are only valid with `doxa run`\n", .{});
    }

    const option_args = raw_options[0..option_end];
    const run_program_args = if (split_seen and mode == .run)
        raw_options[option_end + 1 ..]
    else
        raw_options[0..0];

    var expecting_output = false;
    for (option_args) |arg| {
        if (expecting_output) {
            try replaceOwned(allocator, &options.build.output_path, arg);
            expecting_output = false;
            continue;
        }
        if (std.mem.eql(u8, arg, "--debug-verbose")) {
            options.reporter_options.debug_verbose = true;
        } else if (std.mem.eql(u8, arg, "--debug-lexer")) {
            options.reporter_options.debug_lexer = true;
        } else if (std.mem.eql(u8, arg, "--debug-parser")) {
            options.reporter_options.debug_parser = true;
        } else if (std.mem.eql(u8, arg, "--debug-semantic")) {
            options.reporter_options.debug_semantic = true;
        } else if (std.mem.eql(u8, arg, "--debug-memory")) {
            options.reporter_options.debug_memory = true;
        } else if (std.mem.eql(u8, arg, "--profile")) {
            options.profile = true;
        } else if (std.mem.startsWith(u8, arg, "--profile-out=")) {
            const path = arg["--profile-out=".len..];
            if (path.len == 0) {
                std.debug.print("Error: --profile-out requires a file path\n", .{});
                return error.Usage;
            }
            options.profile = true;
            try replaceOwned(allocator, &options.profile_out, path);
        } else if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--output")) {
            expecting_output = true;
        } else if (std.mem.startsWith(u8, arg, "--cache-dir=")) {
            const dir = arg["--cache-dir=".len..];
            if (options.build.cache_dir_owned) allocator.free(options.build.cache_dir);
            options.build.cache_dir = try allocator.dupe(u8, dir);
            options.build.cache_dir_owned = true;
        } else if (std.mem.startsWith(u8, arg, "--arch=")) {
            try replaceOwned(allocator, &options.build.target_arch, arg["--arch=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--os=")) {
            try replaceOwned(allocator, &options.build.target_os, arg["--os=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--abi=")) {
            try replaceOwned(allocator, &options.build.target_abi, arg["--abi=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--link=")) {
            try options.build.link_libs.append(try allocator.dupe(u8, arg["--link=".len..]));
        } else if (std.mem.startsWith(u8, arg, "--libdir=")) {
            try options.build.lib_dirs.append(try allocator.dupe(u8, arg["--libdir=".len..]));
        } else if (std.mem.startsWith(u8, arg, "--framework=")) {
            try options.build.frameworks.append(try allocator.dupe(u8, arg["--framework=".len..]));
        } else if (std.mem.startsWith(u8, arg, "--include=")) {
            try options.build.include_dirs.append(try allocator.dupe(u8, arg["--include=".len..]));
        } else if (std.mem.startsWith(u8, arg, "--opt-mode=")) {
            options.build.opt = build_config.Opt.fromMode(arg["--opt-mode=".len..]) orelse {
                std.debug.print("Error: invalid --opt-mode (expected debug|safe|fast|small): {s}\n", .{arg});
                return error.Usage;
            };
        } else if (std.mem.startsWith(u8, arg, "--opt=")) {
            const level = std.fmt.parseInt(i32, arg["--opt=".len..], 10) catch {
                std.debug.print("Error: invalid --opt level: {s}\n", .{arg});
                return error.Usage;
            };
            options.build.opt = build_config.Opt.fromLevel(level);
        } else if (arg.len >= 3 and arg[0] == '-' and arg[1] == 'O') {
            const level = std.fmt.parseInt(i32, arg[2..], 10) catch {
                std.debug.print("Error: invalid optimization flag: {s}\n", .{arg});
                return error.Usage;
            };
            options.build.opt = build_config.Opt.fromLevel(level);
        } else if (std.mem.eql(u8, arg, "--emit-opt-ir")) {
            options.build.emit_opt_ir = true;
        } else if (std.mem.eql(u8, arg, "--emit-asm")) {
            options.build.emit_asm = true;
        } else if (std.mem.eql(u8, arg, "--emit-hir")) {
            options.build.emit_hir = true;
        } else if (std.mem.eql(u8, arg, "--strip")) {
            options.build.debug_info = false;
        } else if (std.mem.eql(u8, arg, "--emit-complexity")) {
            options.build.emit_complexity = true;
        } else if (std.mem.eql(u8, arg, "--emit-memory")) {
            options.build.emit_memory = true;
        } else if (std.mem.startsWith(u8, arg, "--instrument=")) {
            options.build.instrument = parseInstrument(arg["--instrument=".len..]) orelse return error.Usage;
        } else if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            help.print();
            return error.Help;
        } else if (std.mem.endsWith(u8, arg, constants.DOXA_EXTENSION)) {
            try replaceOwned(allocator, &options.script_path, arg);
        } else {
            return fail("Error: Unknown command or invalid file: '{s}'\n", .{arg});
        }
    }

    if (options.script_path == null) return fail("Error: No file specified\n", .{});
    if (mode == .compile and options.build.output_path == null) {
        return fail("Error: compile mode requires -o/--output <path>\n", .{});
    }

    if (mode == .run) {
        const script = options.script_path orelse unreachable;
        const total = 1 + run_program_args.len;
        const duplicated = try allocator.alloc([]const u8, total);
        errdefer allocator.free(duplicated);
        var copied: usize = 0;
        errdefer {
            var i: usize = 0;
            while (i < copied) : (i += 1) allocator.free(duplicated[i]);
        }
        duplicated[0] = try allocator.dupe(u8, script);
        copied += 1;
        for (run_program_args, 0..) |arg, idx| {
            duplicated[idx + 1] = try allocator.dupe(u8, arg);
            copied += 1;
        }
        options.program_args = duplicated;
    }

    return .{ .build = .{ .options = options } };
}

fn fail(comptime fmt: []const u8, args: anytype) error{Usage} {
    std.debug.print(fmt, args);
    help.print();
    return error.Usage;
}

fn replaceOwned(allocator: std.mem.Allocator, slot: *?[]const u8, value: []const u8) !void {
    if (slot.*) |old| allocator.free(old);
    slot.* = try allocator.dupe(u8, value);
}

/// `--instrument=<member>,...`: `calls`, `lines`, `memtrace`, `memcheck`.
fn parseInstrument(list: []const u8) ?instrument.Members {
    var members: instrument.Members = .{};
    var it = std.mem.splitScalar(u8, list, ',');
    while (it.next()) |member| {
        if (std.mem.eql(u8, member, "calls")) {
            members.calls = true;
        } else if (std.mem.eql(u8, member, "lines")) {
            members.lines = true;
        } else if (std.mem.eql(u8, member, "memtrace")) {
            members.memtrace = true;
        } else if (std.mem.eql(u8, member, "memcheck")) {
            members.memcheck = true;
        } else {
            std.debug.print("Error: unknown --instrument member '{s}' (expected calls, lines, memtrace or memcheck)\n", .{member});
            return null;
        }
    }
    if (!members.any()) {
        std.debug.print("Error: --instrument= names no member\n", .{});
        return null;
    }
    return members;
}
