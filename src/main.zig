const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;
const Reporting = @import("reporting.zig");

const Token = @import("lexer.zig").Token;
const instructions = @import("instructions.zig");
const VM = @import("vm.zig").VM;
const Frame = @import("vm.zig").Frame;

///==========================================================================
/// Constants
///==========================================================================

const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_REPL_LINE_LENGTH = 1024;
const DOXA_EXTENSION = ".doxa";

///==========================================================================
/// Variables
///==========================================================================

var hadError: bool = false;
var debugLexer: bool = false;

///==========================================================================
/// Types & Errors
///==========================================================================

const SourceFile = struct {
    allocator: std.mem.Allocator,
    contents: []u8,

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !SourceFile {
        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        
        const file_size = try file.getEndPos();
        const buffer = try allocator.alloc(u8, file_size);
        const bytes_read = try file.readAll(buffer);
        
        return SourceFile{
            .allocator = allocator,
            .contents = buffer[0..bytes_read],
        };
    }

    pub fn deinit(self: *SourceFile) void {
        self.allocator.free(self.contents);
    }
};

///==========================================================================
/// Error Reporting
///==========================================================================

pub fn reportMinimalError(line: usize, message: []const u8) void {
    reportError(line, "", message);
}

pub fn reportError(line: usize, where: []const u8, message: []const u8) void {
    std.debug.print("[line {}] {s} Error: {s}\n", .{line, where, message});
    hadError = true;
}

///==========================================================================
/// Run
///==========================================================================

fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    // init lexer
    var lexer = Lexer.init(allocator, source);
    try lexer.initKeywords();
    defer lexer.deinit();

    var reporter = Reporting.Reporting.initStderr();
    defer reporter.deinit();

    const tokens = lexer.lexTokens() catch |err| switch (err) {
        Reporting.ErrorList.UnterminatedString => {
            reportMinimalError(lexer.line, "Unterminated string.");
            return err;
        },
        Reporting.ErrorList.UnterminatedArray => {
            reportMinimalError(lexer.line, "Unterminated array.");
            return err;
        },
        Reporting.ErrorList.ExpectedCommaOrClosingBracket => {
            reportMinimalError(lexer.line, "Expected ',' or ']' in array.");
            return err;
        },
        Reporting.ErrorList.InvalidNumber => {
            reportMinimalError(lexer.line, "Invalid number.");
            return err;
        },
        Reporting.ErrorList.UnexpectedCharacter => {
            reportMinimalError(lexer.line, "Unexpected character.");
            return err;
        },
        else => |e| return e,
    };

    //try runVM(allocator, tokens.items);

    // var parser = Parser.init(allocator, tokens.items);
    // defer parser.deinit();
    // try parser.parse();
    // const ast = parser.parse() catch |err| {
    //     reportError(parser.tokens[parser.current].line, "", "Parsing error.");
    //     return err;
    // };
    // _ = ast;

    // // Code Generator
    // var code_generator = CodeGenerator.init(allocator);
    // defer code_generator.deinit();

    // try code_generator.generate(ast);

    // Optionally, you can now execute the instructions with a virtual machine
    // var vm = VM.init(code_generator.instructions.toOwnedSlice());
    // try vm.run();

    if (debugLexer) {
        for (tokens.items) |tok| {
            std.debug.print("Token: {any} '{s}' {any}\n", .{ tok.type, tok.lexeme, tok.literal });
        }
    }

}

fn runRepl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    while (true) {
        std.debug.print("> ", .{});
        const line = try stdin.readUntilDelimiterAlloc(allocator, '\n', MAX_REPL_LINE_LENGTH);
        defer allocator.free(line);
        if (line.len == 0) continue;
        try run(allocator, line);
        if (hadError) {
            std.debug.print("Error: {s}\n", .{line});
        }
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    var source = try SourceFile.init(allocator, path);
    defer source.deinit();
    try run(allocator, source.contents);
    if (hadError) {
        std.process.exit(EXIT_CODE_ERROR);
    }
}

///==========================================================================
/// Main
///==========================================================================


fn vmTest(allocator: std.mem.Allocator) !void {
    // Create an empty array first
    var array_val = std.ArrayList(Frame).init(allocator);
    defer array_val.deinit();

    const code = [_]u8{
        // Set up a variable that will cause an error
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // 0: Push zero sized array onto stack
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,  // 2: Store in var[0]

        @intFromEnum(instructions.OpCode.OP_TRY),         // 4: Start try block
        @intFromEnum(instructions.OpCode.OP_VAR), 0,      // 5: Load array
        @intFromEnum(instructions.OpCode.OP_CONST), 1,    // 7: Push index 999
        @intFromEnum(instructions.OpCode.OP_ARRAY_GET),   // 9: This will throw
        @intFromEnum(instructions.OpCode.OP_JUMP), 3,     // 10: Skip catch block (jump to END_TRY)

        @intFromEnum(instructions.OpCode.OP_CATCH),       // 12: Start catch block
        @intFromEnum(instructions.OpCode.OP_CONST), 2,    // 13: Push -1

        @intFromEnum(instructions.OpCode.OP_END_TRY),     // 15: End try-catch
        @intFromEnum(instructions.OpCode.OP_HALT),        // 16: Stop execution
    };

    var constants = [_]Frame{
        Frame{ 
            .value = .{
            .type = .ARRAY,
            .data = .{ .array_val = array_val },
            .nothing = false,
        },
        .allocator = null,  
            .owns_value = false, 
    },
    Frame{ // constant[1] = index 999
        .value = .{
            .type = .INT,
            .data = .{ .int = 999 },
            .nothing = false,
        },
        .allocator = null,
            .owns_value = true,
        },
        Frame{ // constant[2] = error result (-1)
            .value = .{
            .type = .INT,
            .data = .{ .int = -1 },
            .nothing = false,
        },
        .allocator = null,
        .owns_value = true,
        },
    };

    var reporter = Reporting.init();
    defer reporter.deinit();

    const vm = VM.init(allocator, &code, &constants, reporter);
    try vm.run();
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    // Skip the executable name
    var script_path: ?[]const u8 = null;
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        if (std.mem.eql(u8, args[i], "--debug-lexer")) {
            debugLexer = true;
        } else {
            // Assume it's a script path
            if (script_path != null) {
                std.debug.print("Usage: doxa [--debug-lexer] [script]\n", .{});
                std.process.exit(EXIT_CODE_USAGE);
            }
            script_path = args[i];
        }
    }
    if (script_path) |path| {
        if (!std.mem.endsWith(u8, path, DOXA_EXTENSION)) {
            std.debug.print("Error: {s} is not a doxa file\n", .{path});
            std.process.exit(EXIT_CODE_USAGE);
        }
        try runFile(allocator, path);
    } else {
        try runRepl(allocator);
    }
}