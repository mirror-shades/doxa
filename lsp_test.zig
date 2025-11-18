const std = @import("std");
const reporting = @import("src/utils/reporting.zig");
const Reporter = reporting.Reporter;
const MemoryManager = @import("src/utils/memory.zig").MemoryManager;
const LexicalAnalyzer = @import("src/analysis/lexical.zig").LexicalAnalyzer;
const Token = @import("src/types/token.zig").Token;
const Parser = @import("src/parser/parser_types.zig").Parser;
const SemanticAnalyzer = @import("src/analysis/semantic/semantic.zig").SemanticAnalyzer;
const ast = @import("src/ast/ast.zig");
const Errors = @import("src/utils/errors.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var memory_manager = try MemoryManager.init(allocator);
    defer memory_manager.deinit();

    var reporter = Reporter.init(allocator, .{ .log_to_file = false });
    defer reporter.deinit();

    // Read test.doxa
    const file = try std.fs.cwd().openFile("test.doxa", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 1024 * 1024);
    defer allocator.free(content);

    const path = "test.doxa";
    const uri = "file:///C:/dev/zig/doxa/test.doxa"; // Adjust path as needed

    std.debug.print("Analyzing file: {s}\n", .{path});
    std.debug.print("Content:\n{s}\n", .{content});

    // Perform lexical analysis (catch errors like LSP server does)
    var lexer = try LexicalAnalyzer.init(memory_manager.getAnalysisAllocator(), content, path, &reporter);
    defer lexer.deinit();
    try lexer.initKeywords();

    var tokens = lexer.lexTokens() catch blk: {
        std.debug.print("Lexical analysis failed, continuing with partial results...\n", .{});
        break :blk std.array_list.Managed(Token).initCapacity(memory_manager.getAnalysisAllocator(), 0) catch unreachable;
    };
    defer tokens.deinit();

    // Perform parsing (only if we have tokens)
    var statements: []ast.Stmt = &[_]ast.Stmt{};
    if (tokens.items.len > 0) {
        var parser = Parser.init(memory_manager.getAnalysisAllocator(), tokens.items, path, uri, &reporter);
        defer parser.deinit();
        statements = parser.execute() catch &[_]ast.Stmt{};
    }

    // Perform semantic analysis
    if (statements.len > 0) {
        var parser_stub = Parser.init(memory_manager.getAnalysisAllocator(), tokens.items, path, uri, &reporter);
        defer parser_stub.deinit();
        var semantic = SemanticAnalyzer.init(memory_manager.getAnalysisAllocator(), &reporter, &memory_manager, &parser_stub);
        defer semantic.deinit();
        _ = semantic.analyze(statements) catch {};
    }

    // Build and print LSP diagnostics payload
    const payload = try reporter.buildPublishDiagnosticsPayload(allocator, uri);
    defer allocator.free(payload);

    std.debug.print("\nLSP DIAGNOSTICS PAYLOAD:\n{s}\n", .{payload});
}
