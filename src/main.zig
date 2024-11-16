const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const Parser = @import("parser.zig").Parser;

const EXIT_CODE_USAGE = 64;
const EXIT_CODE_ERROR = 65;
const MAX_REPL_LINE_LENGTH = 1024;
const DOXA_EXTENSION = ".doxa";

var hadError: bool = false;

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

pub fn reportMinimalError(line: usize, message: []const u8) void {
    reportError(line, "", message);
}

pub fn reportError(line: usize, where: []const u8, message: []const u8) void {
    std.debug.print("[line {}] {s} Error: {s}\n", .{line, where, message});
    hadError = true;
}

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


    // Parser
    var parser = Parser.init(allocator, tokens.items);
    defer parser.deinit();
    const ast = parser.parse() catch |err| {
        reportError(parser.tokens[parser.current].line, "", "Parsing error.");
        return err;
    };
    _ = ast;

    // // Code Generator
    // var code_generator = CodeGenerator.init(allocator);
    // defer code_generator.deinit();

    // try code_generator.generate(ast);

    // Optionally, you can now execute the instructions with a virtual machine
    // var vm = VM.init(code_generator.instructions.toOwnedSlice());
    // try vm.run();
    
    // Debug print all tokens
    for (tokens.items) |tok| {
        std.debug.print("Token: {any} '{s}'\n", .{ tok.type, tok.lexeme });
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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const leaked = gpa.deinit();
        if (leaked == .leak) std.debug.print("Warning: Memory leak detected!\n", .{});
    }
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len > 2) {
        std.debug.print("Usage: doxa [script]\n", .{});
        std.process.exit(EXIT_CODE_USAGE);
    } else if (args.len == 2) {
        if (!std.mem.endsWith(u8, args[1], DOXA_EXTENSION)) {
            std.debug.print("Error: {s} is not a doxa file\n", .{args[1]});
            std.process.exit(EXIT_CODE_USAGE);
        }
        try runFile(allocator, args[1]);
    } else {
        try runRepl(allocator);
    }
}
