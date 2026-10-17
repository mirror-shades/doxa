//! Compilation-phase failure policy: turn a phase error into a located
//! diagnostic and exit with the usage status.

const std = @import("std");
const Reporting = @import("../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const ErrorCode = @import("../utils/errors.zig").ErrorCode;
const constants = @import("../common/constants.zig");

pub fn exitIfCompileErrors(reporter: *Reporter) void {
    if (reporter.hasCompileErrors()) {
        std.process.exit(constants.EXIT_CODE_USAGE);
    }
}

/// Fallback for a compilation phase that returned an error without emitting a
/// diagnostic of its own. Turns what would otherwise be a bare Zig error (and
/// stack trace) into a located, severity-tagged message.
pub fn reportPhaseFailure(reporter: *Reporter, phase: []const u8, err: anyerror) void {
    reporter.reportCompileError(
        null,
        ErrorCode.INTERNAL_ERROR,
        "internal compiler error in the {s} phase: {s}",
        .{ phase, @errorName(err) },
    );
}
