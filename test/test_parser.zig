const std = @import("std");
const testing = std.testing;

const LexicalAnalyzer = @import("../src/analysis/lexical.zig").LexicalAnalyzer;
const Parser = @import("../src/parser/parser_types.zig").Parser;
const Reporting = @import("../src/utils/reporting.zig");
const ErrorCode = @import("../src/utils/errors.zig").ErrorCode;

/// Every declaration site a name is bound at, as a template whose `{s}` is
/// the name.
const sites = [_][]const u8{
    "function {s}() {{}}\n",
    "function f({s} :: int) {{}}\n",
    "struct S {{\n    method f({s} :: int) {{}}\n}}\n",
    "entry function main() {{\n    var {s} is 1\n}}\n",
};

/// The diagnostic codes parsing `source` reports, as one string.
fn parseCodes(allocator: std.mem.Allocator, source: []const u8) ![]const u8 {
    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var lexer = try LexicalAnalyzer.init(testing.io, allocator, source, "test/keyword_names.doxa", &reporter);
    try lexer.initKeywords();
    const tokens = try lexer.lexTokens();
    var parser = Parser.init(testing.io, allocator, tokens.items, "test/keyword_names.doxa", "", &reporter);
    if (parser.execute()) |_| {} else |_| {}
    var codes: std.ArrayListUnmanaged(u8) = .empty;
    for (reporter.diagnostics.items) |diagnostic| try codes.appendSlice(allocator, diagnostic.code orelse "-");
    return codes.items;
}

test "parser: a keyword is refused the same way at every declaration site" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var reporter = Reporting.Reporter.init(testing.io, allocator, .{ .log_to_stderr = false }, null);
    defer reporter.deinit();
    var keywords = try LexicalAnalyzer.init(testing.io, allocator, "", "test/keywords.doxa", &reporter);
    try keywords.initKeywords();

    var it = keywords.keywords.keyIterator();
    while (it.next()) |keyword| {
        // A symbol (`∨`) is an operator, never spelled like a name.
        if (!std.ascii.isAlphabetic(keyword.*[0])) continue;
        // `zig` opens an embedded Zig block in the lexer, so it never
        // reaches a declaration site as a token.
        if (std.mem.eql(u8, keyword.*, "zig")) continue;
        inline for (sites) |site| {
            const source = try std.fmt.allocPrint(allocator, site, .{keyword.*});
            const codes = try parseCodes(allocator, source);
            if (!std.mem.eql(u8, codes, ErrorCode.KEYWORD_AS_NAME)) {
                std.debug.print("`{s}`: expected only {s}, got `{s}`\n", .{ source, ErrorCode.KEYWORD_AS_NAME, codes });
                return error.KeywordNotRefusedByName;
            }
        }
    }
}
