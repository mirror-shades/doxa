const std = @import("std");

pub const ErrorList = error{
    // Stack errors
    StackOverflow,
    StackUnderflow,

    // Value/Type errors
    TypeError,
    DivisionByZero,
    IntegerOverflow,
    FloatOverflow,

    // Variable errors
    VariableNotFound,
    VariableOutOfScope,
    VariableIndexOutOfBounds,
    AccessingCleanedVariable,
    CannotAssignToConstant,

    // Function/Frame errors
    InvalidFunction,
    NoActiveFrame,
    FrameStackOverflow,
    InsufficientArguments,
    CannotPopGlobalFrame,

    // Array/String errors
    IndexOutOfBounds,
    InvalidConstant,

    // Try-Catch errors
    NoTryBlock,
    NoCatchWithoutTry,
    NoTryBlockToEnd,
    UncaughtError,

    // lexer
    UnterminatedString,
    UnterminatedArray,
    UnterminatedParenthesis,
    UnterminatedMultilineComment,
    ExpectedCommaOrClosingBracket,
    ExpectedCommaOrClosingParenthesis,
    InvalidNumber,
    InvalidEscapeSequence,
    UnexpectedCharacter,
    Overflow,
    InvalidCharacter,
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8InvalidCodepoint,
    Utf8CodepointTooLarge,
    InvalidUnicodeEscape,
    CodepointTooLarge,
    Utf8CannotEncodeSurrogateHalf,
    LeadingZeros,
    MultipleExponents,
    InvalidExponent,

    // parser
    OutOfMemory,
    ExpectedSemicolon,
    ExpectedClosingParen,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedAssignmentOperator,
    ExpectedLiteral,
    UnexpectedToken,
    ExpectedRightParen,
    ExpectedRightBracket,
    InvalidAssignmentTarget,
    UndefinedVariable,
    InvalidOperand,
    InvalidExpression,
    InvalidOperator,
    ExpectedRightBrace,
};

/// Reporting provides structured error handling and reporting capabilities
/// for the DoxVM compiler and runtime.
pub const Reporting = struct {
    /// Total count of errors encountered
    error_count: u32 = 0,

    /// Total count of warnings encountered
    warning_count: u32 = 0,

    /// Whether any errors occurred
    had_error: bool = false,

    /// Whether any warnings occurred
    had_warning: bool = false,

    /// Output writer for error messages
    writer: std.fs.File.Writer,

    /// Initialize a new Reporter that writes to the given writer
    pub fn init(writer: std.fs.File.Writer) Reporting {
        return .{
            .writer = writer,
        };
    }

    pub fn deinit(self: *Reporting) void {
        _ = self;
        //self.writer.flush() catch {};
    }

    /// Initialize a Reporter that writes to stderr
    pub fn initStderr() Reporting {
        return Reporting.init(std.io.getStdErr().writer());
    }

    /// Location information for error reporting
    pub const Location = struct {
        line: u32,
        column: u32,
        file: []const u8,

        pub fn format(
            self: Location,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            _ = fmt;
            _ = options;
            try writer.print("{s}:{d}:{d}", .{
                self.file,
                self.line,
                self.column,
            });
        }
    };

    /// Report a fatal error and panic
    pub fn reportFatalError(self: *Reporting, comptime fmt: []const u8, args: anytype) noreturn {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: Fatal error: " ++ fmt ++ "\n", args) catch {};
        std.process.exit(1);
    }

    /// Report a compile-time error with location information
    pub fn reportCompileError(
        self: *Reporting,
        location: Location,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: {}: Compile error: " ++ fmt ++ "\n", .{location} ++ args) catch {};
    }

    /// Report a runtime error
    pub fn reportRuntimeError(self: *Reporting, comptime fmt: []const u8, args: anytype) void {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: Runtime error: " ++ fmt ++ "\n", args) catch {};
    }

    /// Report a warning
    pub fn reportWarning(self: *Reporting, comptime fmt: []const u8, args: anytype) void {
        self.had_warning = true;
        self.warning_count += 1;
        self.writer.print("DoxVM: Warning: " ++ fmt ++ "\n", args) catch {};
    }

    /// Get total error count
    pub fn getErrorCount(self: Reporting) u32 {
        return self.error_count;
    }

    /// Get total warning count
    pub fn getWarningCount(self: Reporting) u32 {
        return self.warning_count;
    }

    /// Returns true if any errors occurred
    pub fn hadError(self: Reporting) bool {
        return self.had_error;
    }

    /// Returns true if any warnings occurred
    pub fn hadWarning(self: Reporting) bool {
        return self.had_warning;
    }
};
