const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const reporting = @import("reporting");

const Reporter = reporting.Reporter;
const Location = reporting.Location;
const Range = reporting.Range;
const RelatedInformation = reporting.RelatedInformation;
const convertPathToUri = reporting.convertPathToUri;

test "convertPathToUri posix path" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "/tmp/project/main.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file:///tmp/project/main.doxa", uri);
}

test "convertPathToUri windows drive" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "C:\\dev\\doxa\\file.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file:///C:/dev/doxa/file.doxa", uri);
}

test "convertPathToUri windows unc" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const uri = try convertPathToUri(testing.allocator, "\\\\server\\share\\sub\\file.doxa");
    defer testing.allocator.free(uri);
    try testing.expectEqualStrings("file://server/share/sub/file.doxa", uri);
}

test "convertUriToPath posix path" {
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file:///tmp/project/main.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("/tmp/project/main.doxa", path);
}

test "convertUriToPath windows drive" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file:///C:/dev/doxa/file.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("C:\\dev\\doxa\\file.doxa", path);
}

test "convertUriToPath windows unc" {
    if (builtin.os.tag != .windows) return error.SkipZigTest;

    const path = try reporting.convertUriToPath(testing.allocator, "file://server/share/sub/file.doxa");
    defer testing.allocator.free(path);
    try testing.expectEqualStrings("\\\\server\\share\\sub\\file.doxa", path);
}

test "Reporter.toLspDiagnostics serializes related info" {
    var reporter = Reporter.init(testing.allocator, .{ .log_to_file = false });
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

test "Reporter publish tracking detects changes and throttles" {
    var reporter = Reporter.init(testing.allocator, .{
        .log_to_file = false,
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
