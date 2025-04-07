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
    UnknownDirective,
    InvalidDirective,
    ExpectedEquals,
    ExpectedEnumValue,
    InvalidReturnValue,
    TooManyArguments,
    NoDefaultValue,
    TooFewArguments,
    CannotModifyTuple,
    InvalidMapKey,
    KeyNotFound,
    ExpectedCatch,
    HeterogeneousArray,
    MissingTypeAnnotation,
    BangNegationNotSupported,
    UndeclaredFunction,
    UnknownMethod,
    MethodNotFound,
    EmptyArray,
    UndeclaredType,

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
    ImmutableVariable,
    UseIsForAssignment,
    ExpectedVarOrConst,

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
    ConstAssignment,
    MissingEntryPointFunction,

    // Collections & Indexing
    IndexOutOfBounds,
    UnterminatedArray,
    UnterminatedString,
    UnterminatedParenthesis,
    ExpectedComma,
    ExpectedDot,
    ExpectedArrow,
    UnsupportedArrayType,
    ArrayTypeMismatch,
    NotAStruct,
    EmptyStruct,
    EmptyMatch,
    FieldNotFound,
    InvalidFieldAccess,
    InvalidFieldAccessTarget,
    InvalidEnumVariant,
    NoMatchCase,
    UndefinedType,
    UndefinedProperty,
    MethodNotImplemented,

    // Error Handling
    NoTryBlock,
    ExpectedInKeyword,
    ExpectedTypeAnnotation,
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
    ExpectedString,
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
    ExpectedSemicolonOrBrace,
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
    ExpectedArrayExpression,
    ExpectedArrayType,
    ExpectedWhereKeyword,
    UnsupportedCompoundOperator,
    MissingParameterType,
    MissingReturnType,
    ExpectedStringLiteral,
    ParserDidNotAdvance,

    // Miscellaneous
    NotImplemented,
    Overflow,
    PermissionDenied,
    ProcessNotFound,
    MessageTooBig,
    Canceled,
    EndOfStream,
    StreamTooLong,

    // Safe Mode
    UnknownType,
    SafeModeRequiresType,
    SafeModeNoDynamicTypes,

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

    // Entry Points
    MultipleEntryPoints,
    MisplacedEntryPoint,
    EntryPointMustBeMain,
    MissingEntryPoint,
    ExpectedFunction,
    InvalidEntryPoint,

    // Module System
    ModuleNotFound,
    ModuleAlreadyLoaded,
    ModuleParseError,
    ExpectedModuleName,
    ExpectedImportName,
    UnsafeImportInSafeModule,
    MisplacedPublicModifier,
    ModuleNotImplemented,

    // modules
    SharingViolation,
    PathAlreadyExists,
    FileNotFound,
    PipeBusy,
    NameTooLong,
    InvalidUtf8,
    InvalidWtf8,
    BadPathName,
    NetworkNotFound,
    AntivirusInterference,
    SymLinkLoop,
    ProcessFdQuotaExceeded,
    SystemFdQuotaExceeded,
    NoDevice,
    IsDir,
    NotDir,
    FileLocksNotSupported,
    FileBusy,
    Unseekable,
    ConnectionTimedOut,
    NotOpenForReading,
    SocketNotConnected,
    IncompleteRead,
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

    /// Initialize a new Reporter that writes to stderr
    pub fn init() Reporting {
        return .{
            .writer = std.io.getStdErr().writer(),
            .error_count = 0,
            .warning_count = 0,
            .had_error = false,
            .had_warning = false,
        };
    }

    pub fn deinit(self: *Reporting) void {
        _ = self;
        //self.writer.flush() catch {};
    }

    /// Location information for error reporting
    pub const Location = struct {
        line: i32,
        column: usize,
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
