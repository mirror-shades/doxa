const std = @import("std");

pub const ErrorList = error{
    // Memory & Stack Management
    StackOverflow,
    StackUnderflow,
    FrameStackOverflow,
    OutOfMemory,

    // Type System & Value Handling
    TypeError,
    DivisionByZero,
    IntegerOverflow,
    FloatOverflow,
    InvalidType,
    InvalidOperand,
    InvalidOperator,
    InvalidConstant,
    InvalidExpression,

    // Variable Management
    VariableNotFound,
    VariableOutOfScope,
    VariableIndexOutOfBounds,
    AccessingCleanedVariable,
    CannotAssignToConstant,
    UndefinedVariable,
    VariableAlreadyDefined,
    InvalidAssignment,
    InvalidAssignmentTarget,

    // Function & Frame Management
    InvalidFunction,
    NoActiveFrame,
    InsufficientArguments,
    CannotPopGlobalFrame,
    NotCallable,
    InvalidFunctionCall,
    InvalidFunctionDeclaration,
    InvalidReturnStatement,
    ReturnValue,
    ReturnNothing,
    InvalidArgumentCount,
    ExpectedLeftOperand,
    ExpectedRightOperand,
    ExpectedOperand,
    ExpectedCallable,

    // Collections & Indexing
    IndexOutOfBounds,
    UnterminatedArray,
    UnterminatedString,
    UnterminatedParenthesis,

    // Error Handling
    NoTryBlock,
    NoCatchWithoutTry,
    NoTryBlockToEnd,
    UncaughtError,

    // Lexical Analysis
    UnterminatedMultilineComment,
    ExpectedCommaOrClosingBracket,
    ExpectedCommaOrClosingParenthesis,
    InvalidNumber,
    InvalidEscapeSequence,
    UnexpectedCharacter,
    InvalidCharacter,
    LeadingZeros,
    MultipleExponents,
    InvalidExponent,

    // Unicode & UTF-8 Handling
    Utf8InvalidStartByte,
    Utf8ExpectedContinuation,
    Utf8OverlongEncoding,
    Utf8InvalidCodepoint,
    Utf8CodepointTooLarge,
    Utf8CannotEncodeSurrogateHalf,
    InvalidUnicodeEscape,
    CodepointTooLarge,

    // Syntax & Parsing
    ExpectedSemicolon,
    ExpectedClosingParen,
    ExpectedExpression,
    ExpectedIdentifier,
    ExpectedAssignmentOperator,
    ExpectedLiteral,
    UnexpectedToken,
    ExpectedRightParen,
    ExpectedLeftParen,
    ExpectedRightBracket,
    ExpectedLeftBracket,
    ExpectedLeftBrace,
    ExpectedRightBrace,
    ExpectedThen,
    ExpectedElse,
    ExpectedColon,
    ExpectedCommaOrBracket,
    ExpectedEnumVariant,
    ExpectedCommaOrBrace,
    ExpectedCommaOrParen,
    ExpectedFunctionParam,
    ExpectedFunctionReturnType,
    ExpectedFunctionBody,
    ExpectedFunctionParams,
    ExpectedType,
    ExpectedFunctionKeyword,
    ExpectedFunctionName,
    UnsupportedCompoundOperator,

    // Miscellaneous
    NotImplemented,
    Overflow,

    // Strict Mode
    UnknownType,
    StrictModeRequiresType,
    StrictModeNoDynamicTypes,

    // Print Statements
    UnsupportedPrintStatement,
    InvalidPrintStatement,
    FileTooBig,
    InputOutput,
    BrokenPipe,
    DiskQuota,
    AccessDenied,
    Unexpected,
    SystemResources,
    NoSpaceLeft,
    DeviceBusy,
    WouldBlock,
    OperationAborted,
    ConnectionResetByPeer,
    InvalidArgument,
    NotOpenForWriting,
    LockViolation,
};

/// Reporting provides structured error handling and reporting capabilities
/// for the DoxVM compiler and runtime.
pub const Reporting = struct {
    /// Total count of errors encountered
    error_count: i32 = 0,

    /// Total count of warnings encountered
    warning_count: i32 = 0,

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
        line: i32,
        column: i32,
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
    pub fn getErrorCount(self: Reporting) i32 {
        return self.error_count;
    }

    /// Get total warning count
    pub fn getWarningCount(self: Reporting) i32 {
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
