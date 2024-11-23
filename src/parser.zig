const std = @import("std");
const Token = @import("lexer.zig").Token;
const Reporting = @import("reporting.zig").Reporting;

pub const Parser = struct {
    allocator: std.mem.Allocator,
    tokens: []Token,
    mode: Modes = .Strict,
    reporter: Reporting,

    const Modes = enum {
        Normal,
        Warning,
        Strict,
    };

    pub fn init(allocator: std.mem.Allocator, tokens: []Token) Parser {
        return .{
            .allocator = allocator,
            .tokens = tokens,
            .reporter = Reporting.initStderr(),
        };
    }

    pub fn deinit(self: *Parser) void {
        // nothing to do
        _ = self;
    }

    pub fn parse(self: *Parser) !void {
        // just print the tokens
        try self.checkDirective();
        try self.findEntryPoint();
    }

    fn checkDirective(self: *Parser) !void {
        // if the first token is a HASH, check for directives
        if (self.tokens[0].type == .HASH) {
            if (self.tokens[1].type != .IDENTIFIER) {
                self.reporter.reportCompileError(
                    .{
                        .line = self.tokens[1].line,
                        .column = self.tokens[1].column,
                        .file = "<input>",
                    },
                    "Expected identifier after # directive.",
                    .{},
                );
                return error.InvalidDirective;
            }
            const directive = self.tokens[1].lexeme;
            var lower_directive: [256]u8 = undefined;
            const lower = std.ascii.lowerString(&lower_directive, directive);
            if (std.mem.eql(u8, lower, "warning")) {
                self.mode = .Warning;
            }
            else if (std.mem.eql(u8, directive, "strict")) {
                self.mode = .Strict;
            }
            else if (std.mem.eql(u8, directive, "normal")) {
                self.mode = .Normal;
            }
            else {
                self.reporter.reportCompileError(
                    .{
                        .line = self.tokens[1].line,
                        .column = self.tokens[1].column,
                        .file = "<input>",
                    },
                    "Invalid directive.",
                    .{},
                );
                return error.InvalidDirective;
            }
        }
        std.debug.print("Parser mode: {}\n", .{self.mode});
    }

    fn findEntryPoint(self: *Parser) !void {
        // find the first function declaration and set it as the entry point
        var found = false;
        var token_index: u32 = 0;
        var entry_point_index: u32 = 0;
        // find entry point
        // error if multiple entry points
        for (self.tokens) |tok| {
            if (tok.type == .ARROW) {
                if (found) {
                    self.reporter.reportCompileError(
                        .{
                            .line = tok.line,
                            .column = tok.column,
                            .file = "<input>",
                        },
                        "Multiple entry points.",
                        .{},
                    );
                    return error.MultipleEntryPoints;
                }
                // we found the entry point
                std.debug.print("Entry point: {s}{d}\n", .{ tok.lexeme, token_index });
                found = true;
                entry_point_index = token_index;
            }
            token_index += 1;
        }
        // error if no entry point and strict mode
        if (!found and self.mode == .Strict) {
            self.reporter.reportCompileError(
                .{ .line = 1, .column = 1, .file = "<input>" },
                "No entry point found.",
                .{},
            );
            return error.NoEntryPoint;
        }
        // warn if no entry point and warning mode
        if (!found and self.mode == .Warning) {
            self.reporter.reportWarning(
                "STRICT VIOLATION: No entry point found.",
                .{},
            );
        }
        // error if entry point is not a function declaration
        if (self.tokens[entry_point_index + 1].type != .PUBLIC and self.tokens[entry_point_index + 1].type != .FUNCTION_KEYWORD) {
            self.reporter.reportCompileError(
                .{
                    .line = self.tokens[entry_point_index + 1].line,
                    .column = self.tokens[entry_point_index + 1].column,
                    .file = "<input>",
                },
                "Entry point must be a function declaration.",
                .{},
            );
            return error.InvalidEntryPoint;
        }
        // error if func comes before public
        if (self.tokens[entry_point_index + 1].type == .FUNCTION_KEYWORD and self.tokens[entry_point_index + 2].type == .PUBLIC) {
            self.reporter.reportCompileError(
                .{
                    .line = self.tokens[entry_point_index + 1].line,
                    .column = self.tokens[entry_point_index + 1].column,
                    .file = "<input>",
                },
                "Function declarations must come after public.",
                .{},
            );
            return error.InvalidEntryPoint;
        }
        // error if public is not followed by a function declaration
        if (self.tokens[entry_point_index + 1].type == .PUBLIC and self.tokens[entry_point_index + 2].type != .FUNCTION_KEYWORD) {
            self.reporter.reportCompileError(
                .{
                    .line = self.tokens[entry_point_index + 2].line,
                    .column = self.tokens[entry_point_index + 2].column,
                    .file = "<input>",
                },
                "Public must be followed by a function declaration.",
                .{},
            );
            return error.InvalidEntryPoint;
        }
        // continue parsing
        var parsing_index = entry_point_index;
        while (parsing_index < self.tokens.len) {
            const tok = self.tokens[parsing_index];
            _ = tok;
            parsing_index += 1;
        }
    }
};
