const std = @import("std");
const ast = @import("../ast/ast.zig");

const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorList = Reporting.ErrorList;

//======================================================================

pub const SemanticAnalyzer = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporter,

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporter) SemanticAnalyzer {
        return .{
            .allocator = allocator,
            .reporter = reporter,
        };
    }

    pub fn deinit(_: *SemanticAnalyzer) void {
        // nothing to deinit yet
    }

    pub fn analyze(self: *SemanticAnalyzer, statements: []ast.Stmt) ErrorList![]ast.Stmt {
        _ = self;
        return statements;
    }
};
