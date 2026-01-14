const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const reporting = @import("reporting");
const harness = @import("harness.zig");

const Reporter = reporting.Reporter;
const Location = reporting.Location;
const Range = reporting.Range;
const RelatedInformation = reporting.RelatedInformation;
const convertPathToUri = reporting.convertPathToUri;

fn runCheck(name: []const u8, func: anytype, passed: *usize, skipped: *usize) anyerror!void {
    _ = name;
    func() catch |err| switch (err) {
        error.SkipZigTest => {
            skipped.* += 1;
            return;
        },
        else => return err,
    };
    passed.* += 1;
}

fn checkConvertPathToUriPosixPath() anyerror!void {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "/tmp/project/main.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file:///tmp/project/main.doxa", uri);
}

fn checkConvertPathToUriWindowsDrive() anyerror!void {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "C:\\dev\\doxa\\file.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file:///C:/dev/doxa/file.doxa", uri);
}

fn checkConvertPathToUriWindowsUnc() anyerror!void {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "\\\\server\\share\\sub\\file.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file://server/share/sub/file.doxa", uri);
}

fn checkConvertUriToPathPosixPath() anyerror!void {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file:///tmp/project/main.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("/tmp/project/main.doxa", path);
}

fn checkConvertUriToPathWindowsDrive() anyerror!void {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file:///C:/dev/doxa/file.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("C:\\dev\\doxa\\file.doxa", path);
}

fn checkConvertUriToPathWindowsUnc() anyerror!void {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file://server/share/sub/file.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("\\\\server\\share\\sub\\file.doxa", path);
}

fn checkReporterToLspDiagnosticsSerializesRelatedInfo() anyerror!void {
    var reporter = Reporter.init(testing.allocator, .{ .log_to_file = false, .log_to_stderr = false });
    defer reporter.deinit();

    const primary_uri = "file:///src/main.doxa";
    const primary_loc = Location{
        .file = "src/main.doxa",
        .file_uri = primary_uri,
        .range = .{
            .start_line = 2,
            .start_col = 1,
            .end_line = 2,
            .end_col = 5,
        },
    };

    const related_loc = Location{
        .file = "src/other.doxa",
        .file_uri = "file:///src/other.doxa",
        .range = .{
            .start_line = 10,
            .start_col = 4,
            .end_line = 10,
            .end_col = 9,
        },
    };

    const related = [_]RelatedInformation{
        .{ .message = "first seen here", .location = related_loc },
    };

    reporter.reportWithRelated(
        .CompileTime,
        .Warning,
        primary_loc,
        "E100",
        &related,
        "duplicate symbol",
        .{},
    );

    const diagnostics = try reporter.toLspDiagnostics(testing.allocator, primary_uri);
    defer testing.allocator.free(diagnostics);

    const expected =
        "[{\"range\":{\"start\":{\"line\":1,\"character\":0},\"end\":{\"line\":1,\"character\":4}},\"severity\":2,\"source\":\"DoxVM\",\"code\":\"E100\",\"message\":\"duplicate symbol\",\"relatedInformation\":[{\"location\":{\"uri\":\"file:///src/other.doxa\",\"range\":{\"start\":{\"line\":9,\"character\":3},\"end\":{\"line\":9,\"character\":8}}},\"message\":\"first seen here\"}]}]";
    try testing.expectEqualStrings(expected, diagnostics);
}

fn checkReporterPublishTrackingDetectsChangesAndThrottles() anyerror!void {
    var reporter = Reporter.init(testing.allocator, .{
        .log_to_file = false,
        .log_to_stderr = false,
        .publish_debounce_ns = std.time.ns_per_ms,
    });
    defer reporter.deinit();

    const uri = "file:///src/main.doxa";
    const loc = Location{
        .file = "src/main.doxa",
        .file_uri = uri,
        .range = Range{
            .start_line = 1,
            .start_col = 1,
            .end_line = 1,
            .end_col = 5,
        },
    };

    reporter.report(.CompileTime, .Warning, loc, "E1", "message {s}", .{"one"});
    try testing.expect(reporter.diagnosticsChanged(uri));
    try testing.expect(!reporter.shouldThrottlePublish(uri, 0));

    try reporter.markDiagnosticsPublished(uri, 0);
    try testing.expect(!reporter.diagnosticsChanged(uri));
    try testing.expect(reporter.shouldThrottlePublish(uri, std.time.ns_per_ms / 2));
    try testing.expect(!reporter.shouldThrottlePublish(uri, 2 * std.time.ns_per_ms));

    reporter.clear();
    reporter.report(.CompileTime, .Warning, loc, "E1", "message {s}", .{"two"});
    try testing.expect(reporter.diagnosticsChanged(uri));

    try reporter.markDiagnosticsPublished(uri, 2 * std.time.ns_per_ms);
    reporter.dropPublishedDiagnostics(uri);
    try testing.expect(reporter.diagnosticsChanged(uri));
}

test "lsp suite" {
    harness.printSection("LSP");

    var passed: usize = 0;
    var skipped: usize = 0;

    try runCheck("convertPathToUri posix path", checkConvertPathToUriPosixPath, &passed, &skipped);
    try runCheck("convertPathToUri windows drive", checkConvertPathToUriWindowsDrive, &passed, &skipped);
    try runCheck("convertPathToUri windows unc", checkConvertPathToUriWindowsUnc, &passed, &skipped);
    try runCheck("convertUriToPath posix path", checkConvertUriToPathPosixPath, &passed, &skipped);
    try runCheck("convertUriToPath windows drive", checkConvertUriToPathWindowsDrive, &passed, &skipped);
    try runCheck("convertUriToPath windows unc", checkConvertUriToPathWindowsUnc, &passed, &skipped);
    try runCheck("Reporter.toLspDiagnostics", checkReporterToLspDiagnosticsSerializesRelatedInfo, &passed, &skipped);
    try runCheck("Reporter publish tracking", checkReporterPublishTrackingDetectsChangesAndThrottles, &passed, &skipped);

    harness.printSuiteSummary("LSP", .{ .passed = passed, .failed = 0, .untested = skipped });
}
