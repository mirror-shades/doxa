const std = @import("std");

pub const ErrorList = error{
    // Memory & Stack Management
    StackOverflow,
    StackUnderflow,
    FrameStackOverflow,
    OutOfMemory,
    NoRootScope,
    StorageNotFound,

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
    NullAssignmentValue,
    ExpectedStructType,
    UnknownVariableType,
    UnknownCustomType,
    InvalidExpressionType,
    ImportMustHaveFrom,

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
    ConstMustHaveInitializer,
    byteOverflow,
    byteUnderflow,
    CannotModifyConstant,
    TypeMismatch,

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
    FunctionNotFound,

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
    InvalidFieldType,
    MissingFieldTypes,
    InvalidEnumVariant,
    NoMatchCase,
    UndefinedType,
    UndefinedProperty,
    MethodNotImplemented,

    // Error Handling
    NoTryBlock,
    ExpectedInKeyword,
    ExpectedTypeAnnotation,
    ExpectedTypeAnnotationOrInitializer,
    NoCatchWithoutTry,
    NoTryBlockToEnd,
    UncaughtError,
    AssertionFailed,

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
    ByteValueTooLarge,
    InvalidInternalMethod,

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
    ExpectedNewline,
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
    ExpectedRightUnion,
    ExpectedThen,
    ExpectedElse,
    ExpectedColon,
    ExpectedCommaOrBracket,
    ExpectedNewlineOrBrace,
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
    ImportMustHaveAlias,
    CircularImport,
    UnknownType,
    UnknownFieldOrMethod,
    EllipsisWithoutNewline,

    // Miscellaneous
    NotImplemented,
    Overflow,
    PermissionDenied,
    ProcessNotFound,
    MessageTooBig,
    Canceled,
    EndOfStream,
    StreamTooLong,
    InternalParserError,
    SemanticError,
    RuntimeError,

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
    TwoMainFunctions,
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
    ModuleAlreadyExists,
    ModuleLoadError,

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

    // LLVM
    UnsupportedOperator,
    UnsupportedLiteral,
    UnsupportedCallType,
    UnsupportedType,
    UnsupportedTypeExpr,
    UnsupportedFunction,
    UnsupportedDeclaration,
    UnsupportedExpression,
    UnsupportedStatement,
    UnsupportedModule,
    UnsupportedInitializer,
    MissingCondition,
    MissingThenBranch,
    ModuleVerificationFailed,
    EmitFailed,
    FunctionNotDeclared,
    FunctionVerificationFailed,
    ReturnOutsideFunction,

    // memory
    DuplicateVariableName,
    NothingTypeMustBeConst,
    ExpectedPattern,
};

/// Reporting provides structured error handling and reporting capabilities
/// for the DoxVM compiler and runtime.
pub const Reporter = struct {
    /// Total count of errors encountered
    error_count: i32 = 0,

    is_debug: bool = false,

    /// Total count of warnings encountered
    warning_count: i32 = 0,

    /// Whether any errors occurred
    had_error: bool = false,

    /// Whether any warnings occurred
    had_warning: bool = false,

    /// Output writer for error messages
    writer: std.fs.File.Writer,

    /// Allocator for error lists and other dynamic memory
    allocator: ?std.mem.Allocator = null,

    /// Initialize a new Reporter that writes to stderr
    /// Optionally provide an allocator for dynamic memory operations
    pub fn initWithAllocator(allocator: ?std.mem.Allocator) Reporter {
        return .{
            .writer = std.io.getStdErr().writer(),
            .allocator = allocator,
            .error_count = 0,
            .warning_count = 0,
            .had_error = false,
            .had_warning = false,
        };
    }

    /// Initialize a new Reporter with default settings
    pub fn init(is_debug: bool) Reporter {
        var reporter = Reporter.initWithAllocator(null);
        reporter.is_debug = is_debug;
        return reporter;
    }

    pub fn deinit(self: *Reporter) void {
        _ = self;
        //self.writer.flush() catch {};
    }

    // TODO: location in AST too, why?
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

    pub const RuntimeError = struct {
        message: []const u8,
        location: struct {
            file: []const u8,
            line: usize,
            column: usize,
        },
    };

    /// Report a fatal error and panic
    pub fn reportFatalError(self: *Reporter, comptime fmt: []const u8, args: anytype) noreturn {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: Fatal error: " ++ fmt ++ "\n", args) catch {};
        std.process.exit(1);
    }

    /// Report a compile-time error with location information
    pub fn reportCompileError(
        self: *Reporter,
        location: Location,
        comptime fmt: []const u8,
        args: anytype,
    ) void {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: {}: Compile error: " ++ fmt ++ "\n", .{location} ++ args) catch {};
    }

    /// Report a runtime error
    pub fn reportRuntimeError(self: *Reporter, comptime fmt: []const u8, args: anytype) void {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: Runtime error: " ++ fmt ++ "\n", args) catch {};
    }

    /// Report a warning
    pub fn reportWarning(self: *Reporter, comptime fmt: []const u8, args: anytype) void {
        self.had_warning = true;
        self.warning_count += 1;
        self.writer.print("DoxVM: Warning: " ++ fmt ++ "\n", args) catch {};
    }

    /// Report a generic error - used by compiler and other components
    pub fn throwError(self: *Reporter, comptime fmt: []const u8, args: anytype, err: ?ErrorList) ErrorList {
        self.had_error = true;
        self.error_count += 1;
        self.writer.print("DoxVM: Error: " ++ fmt ++ "\n", args) catch {};
        return err orelse ErrorList.RuntimeError;
    }

    /// Report a generic error - used by compiler and other components
    pub fn reportError(self: *Reporter, comptime fmt: []const u8, args: anytype) void {
        self.throwError(fmt, args, null) catch {};
    }

    pub fn printf(self: *Reporter, comptime fmt: []const u8, args: anytype) void {
        self.writer.print(fmt, args) catch {};
    }

    pub fn printl(self: *Reporter, comptime fmt: []const u8) void {
        self.writer.print(fmt ++ "\n", .{}) catch {};
    }

    /// Get total error count
    pub fn getErrorCount(self: Reporter) i32 {
        return self.error_count;
    }

    /// Get total warning count
    pub fn getWarningCount(self: Reporter) i32 {
        return self.warning_count;
    }

    /// Returns true if any errors occurred
    pub fn hadError(self: Reporter) bool {
        return self.had_error;
    }

    /// Returns true if any warnings occurred
    pub fn hadWarning(self: Reporter) bool {
        return self.had_warning;
    }

    pub fn debug(self: *Reporter, comptime fmt: []const u8, args: anytype) void {
        if (self.is_debug) {
            self.writer.print("DoxVM: Debug: " ++ fmt ++ "\n", args) catch {};
        }
    }
};
