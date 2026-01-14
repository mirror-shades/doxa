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
    InvalidTokenType,
    UnknownDirective,
    InvalidDirective,
    ExpectedEnumValue,
    InvalidReturnValue,
    TooManyArguments,
    NoDefaultValue,
    TooFewArguments,
    InvalidMapKey,
    KeyNotFound,
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
    InvalidAliasArgument,

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
    UnsupportedFunctionCallType,
    MapLiteralsMustBeStatements,
    ExpectedMapKeyword,
    ExpectedReturnsKeyword,
    ExpectedMethod,
    ExpectedLeftBraceOrReturnsKeyword,

    // Collections & Indexing
    IndexOutOfBounds,
    UnterminatedArray,
    UnterminatedString,
    UnterminatedZigBlock,
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
    ThisTypeMismatch,

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
    // Inline Zig blocks
    EmptyInput,
    InvalidParamType,
    InvalidReturnType,
    InlineZigNotValid,
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
    ExpectedMapKey,
    DuplicateElseClause,
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
    ScopeIdAlreadyUsed,

    // Print Statements
    UnsupportedPrintStatement,
    InvalidPrintStatement,
    MissingFormatParts,
    InvalidPrintExpression,
    MissingPlaceholderIndices,
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

    // File I/O and Source Reading
    FileReadError,
    LineTooLong,
    WriteFailed,
    ReadFailed,

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
    MissingFromKeyword,
    ExpectedModulePath,
    ExpectedImportSymbol,
    FileNotFound,
    NotSupported,
    FileSystem,
    UnrecognizedVolume,

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

    // String Interpolation
    UnmatchedOpenBrace,
    NotEnoughInterpolationArguments,
    TooManyInterpolationArguments,
};

pub const ErrorCode = struct {
    // 1xxx - Variables & Types
    pub const VARIABLE_NOT_FOUND = "E1001";
    pub const DUPLICATE_VARIABLE = "E1002";
    pub const TYPE_MISMATCH = "E1003";
    pub const TETRA_TYPE_ERROR = "E1004";
    pub const DIVISION_REQUIRES_NUMERIC_OPERANDS = "E1005";
    pub const MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS = "E1006";
    pub const ARITHMETIC_REQUIRES_NUMERIC_OPERANDS = "E1007";
    pub const VARIABLE_DECLARATION_MISSING_ANNOTATION = "E1008";
    pub const NOTHING_TYPE_MUST_BE_CONST = "E1009";
    pub const STRUCT_FIELD_COUNT_MISMATCH = "E1010";
    pub const STRUCT_FIELD_NAME_MISMATCH = "E1011";
    pub const UNKNOWN_METHOD = "E1012";
    pub const CANNOT_CALL_METHOD_ON_TYPE = "E1013";
    pub const INVALID_OPERAND_TYPE = "E1014";
    pub const INVALID_ASSIGNMENT_TARGET = "E1015";
    pub const INCOMPATIBLE_TYPES_FOR_COMPARISON = "E1016";
    pub const INVALID_CONDITION_TYPE = "E1017";
    pub const DUPLICATE_BOUND_VARIABLE_NAME = "E1018";
    pub const SYMBOL_NOT_FOUND = "E1019";
    pub const UNDEFINED_VARIABLE = "E1020";
    pub const UNKNOWN_TYPE_FOUND_IN_PEEK_INSTRUCTION = "E1021";
    pub const EXPECTED_TYPE_ANNOTATION = "E1022";
    pub const SELF_REFERENTIAL_INITIALIZER = "E1023";
    pub const ARITHMETIC_OVERFLOW = "E1024";
    pub const INVALID_ALIAS_ARGUMENT = "E1025";
    pub const INVALID_ALIAS_PARAMETER = "E1026";
    pub const ALIAS_PARAMETER_REQUIRED = "E1027";
    pub const ALIAS_ARGUMENT_NOT_NEEDED = "E1028";
    pub const RANGE_REQUIRES_NUMERIC_OPERANDS = "E1029";

    // 2xxx - Syntax & Parsing
    pub const SYNTAX_ERROR = "E2001";
    pub const UNEXPECTED_TOKEN = "E2002";
    pub const MISSING_BRACKET = "E2003";
    pub const USE_IS_FOR_ASSIGNMENT = "E2004";
    pub const MISSING_ENTRY_POINT_FUNCTION = "E2005";
    pub const EXPECTED_EXPRESSION = "E2006";
    pub const EXPECTED_IDENTIFIER = "E2007";
    pub const EXPECTED_TYPE = "E2008";
    pub const EXPECTED_CLOSING_PARENTHESIS = "E2009";
    pub const EXPECTED_CLOSING_BRACKET = "E2010";
    pub const EXPECTED_CLOSING_BRACE = "E2011";
    pub const EXPECTED_FUNCTION_NAME = "E2012";
    pub const EXPECTED_FUNCTION_PARAMS = "E2013";
    pub const EXPECTED_FUNCTION_BODY = "E2014";
    pub const INVALID_CHARACTER = "E2015";
    pub const EXPONENTIATION_REQUIRES_NUMERIC_OPERANDS = "E2016";
    pub const UNSUPPORTED_OPERATOR = "E2017";
    pub const CONTINUE_USED_OUTSIDE_OF_LOOP = "E2018";
    pub const BREAK_USED_OUTSIDE_OF_LOOP = "E2019";
    pub const UNHANDLED_STATEMENT_TYPE = "E2020";
    pub const EXPECTED_THEN = "E2021";
    pub const EXPECTED_ELSE = "E2022";
    pub const EXPECTED_NEWLINE = "E2023";
    pub const INVALID_ARGUMENT_COUNT = "E2024";
    pub const UNKNOWN_TYPE = "E2025";
    pub const NOT_TRUTHY = "E2026";
    pub const EXPECTED_OPEN_BRACE = "E2027";

    // 3xxx - Memory & Ownership
    pub const MEMORY_ERROR = "E3001";
    pub const SCOPE_ERROR = "E3002";
    pub const REFERENCE_ERROR = "E3003";
    pub const VARIABLE_STORAGE_NOT_FOUND = "E3004";

    // 4xxx - Runtime & VM
    pub const RUNTIME_ERROR = "E4001";
    pub const STACK_OVERFLOW = "E4002";
    pub const DIVISION_BY_ZERO = "E4003";

    // 5xxx - Functions & Calls
    pub const FUNCTION_NOT_FOUND = "E5001";
    pub const INVALID_FUNCTION_CALL = "E5002";
    pub const MISSING_PARAMETER_TYPE = "E5003";
    pub const MISSING_RETURN_TYPE = "E5004";
    pub const TOO_MANY_ARGUMENTS = "E5005";
    pub const TOO_FEW_ARGUMENTS = "E5006";
    pub const INVALID_ARGUMENT_TYPE = "E5007";
    pub const NO_DEFAULT_VALUE_FOR_PARAMETER = "E5008";
    pub const ARGUMENT_COUNT_MISMATCH = "E5009";
    pub const INVALID_FUNCTION_TYPE = "E5010";
    pub const MISSING_RETURN_VALUE = "E5011";
    pub const UNSUPPORTED_FUNCTION_CALL_TYPE = "E5012";
    pub const EXPECTED_METHOD = "E5013";
    pub const THIS_TYPE_MISMATCH = "E5014";
    pub const INVALID_ARGUMENT = "E5015";
    pub const EXPECTED_LEFT_BRACE_OR_RETURNS_KEYWORD = "E5016";

    // 6xxx - Collections & Data Structures
    pub const INVALID_ARRAY_TYPE = "E6001";
    pub const ARRAY_TYPE_MISMATCH = "E6002";
    pub const INVALID_MAP_KEY = "E6003";
    pub const KEY_NOT_FOUND = "E6004";
    pub const INDEX_OUT_OF_BOUNDS = "E6005";
    pub const INVALID_ARRAY_INDEX_TYPE = "E6006";
    pub const INVALID_MAP_KEY_TYPE = "E6007";
    pub const CANNOT_INDEX_TYPE = "E6008";
    pub const FIELD_NOT_FOUND = "E6009";
    pub const STRUCT_HAS_NO_FIELDS = "E6010";
    pub const CANNOT_ACCESS_FIELD_ON_TYPE = "E6011";
    pub const INVALID_STRING_OPERATION = "E6012";
    pub const INVALID_STRING_TYPE = "E6013";
    pub const INVALID_STRING_INDEX_TYPE = "E6014";
    pub const INVALID_RETURN_TYPE_FOR_UNION = "E6015";
    pub const PRIVATE_FIELD_ACCESS = "E6016";
    pub const UNDEFINED_FIELD = "E6017";
    pub const ARRAY_REQUIRES_DYNAMIC_STORAGE = "E6018";

    // 7xxx - Modules & Imports
    pub const MODULE_NOT_FOUND = "E7001";
    pub const CIRCULAR_IMPORT = "E7002";
    pub const IMPORT_MUST_HAVE_ALIAS = "E7003";
    pub const MISPLACED_PUBLIC_MODIFIER = "E7004";
    pub const SYMBOL_NOT_FOUND_IN_IMPORT_MODULE = "E7005";
    pub const EXPECTED_MODULE_NAME = "E7006";
    pub const EXPECTED_MODULE_PATH = "E7007";
    pub const EXPECTED_IMPORT_SYMBOL = "E7007";
    pub const MISSING_FROM_KEYWORD = "E7008";
    pub const INVALID_PRINT_EXPRESSION = "E7009";

    // 8xxx - Internal Errors
    pub const INTERNAL_ERROR = "E8001";
    pub const NOT_IMPLEMENTED = "E8002";
    pub const PARSER_DID_NOT_ADVANCE = "E8003";
    pub const STACK_UNDERFLOW = "E8004";

    // 9xxx - I/O Operations
    pub const INVALID_FILE_PATH_TYPE = "E9001";
    pub const INVALID_FILE_CONTENT_TYPE = "E9002";
    pub const INVALID_COMMAND_TYPE = "E9003";
    pub const COULD_NOT_DELETE_ARTIFACT = "E9004";
    pub const SOXA_FILE_INCOMPATIBLE = "E9005";
};
