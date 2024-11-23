const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

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

pub const LexerError = error{
    UnterminatedString,
    UnterminatedArray,
    ExpectedCommaOrClosingBracket,
    InvalidNumber,
    UnexpectedCharacter,
};

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

    const tokens = lexer.lexTokens() catch |err| switch (err) {
        LexerError.UnterminatedString => {
            reportMinimalError(lexer.line, "Unterminated string.");
            return err;
        },
        LexerError.UnterminatedArray => {
            reportMinimalError(lexer.line, "Unterminated array.");
            return err;
        },
        LexerError.ExpectedCommaOrClosingBracket => {
            reportMinimalError(lexer.line, "Expected ',' or ']' in array.");
            return err;
        },
        LexerError.InvalidNumber => {
            reportMinimalError(lexer.line, "Invalid number.");
            return err;
        },
        LexerError.UnexpectedCharacter => {
            reportMinimalError(lexer.line, "Unexpected character.");
            return err;
        },
        else => |e| return e,
    };

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
