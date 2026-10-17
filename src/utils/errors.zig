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
    MissingExpressionType,
    MissingBindingType,
    IncompleteType,
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
    FieldDeclaredWithVarOrConst,
    ExpectedReturnsKeyword,
    ExpectedMethod,
    ExpectedLeftBraceOrReturnsKeyword,

    // Collections & Indexing
    IndexOutOfBounds,
    UnterminatedArray,
    UnterminatedString,
    UnclosedPlaceholder,
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
    InvalidEscapeSequence,
    UnexpectedCharacter,
    InvalidCharacter,
    LeadingZeros,
    MultipleExponents,
    InvalidExponent,
    ByteValueTooLarge,
    InvalidInternalMethod,

    // Inline Zig blocks
    EmptyInput,
    InvalidParamType,
    InvalidReturnType,
    InlineZigNotValid,

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
    NestingTooDeep,
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
    ModuleRootUnknown,
    DuplicateStableKey,
    ImportedNameNotFound,
    InvalidZigModule,

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
    pub const UNDEFINED_VARIABLE = "E1020";
    pub const EXPECTED_TYPE_ANNOTATION = "E1022";
    pub const SELF_REFERENTIAL_INITIALIZER = "E1023";
    pub const ARITHMETIC_OVERFLOW = "E1024";
    pub const INVALID_ALIAS_ARGUMENT = "E1025";
    pub const INVALID_ALIAS_PARAMETER = "E1026";
    pub const ALIAS_PARAMETER_REQUIRED = "E1027";
    pub const ALIAS_ARGUMENT_NOT_NEEDED = "E1028";
    pub const RANGE_REQUIRES_NUMERIC_OPERANDS = "E1029";
    pub const ENUM_REQUIRES_INITIALIZER = "E1030";
    pub const BYTE_VALUE_OUT_OF_RANGE = "E1031";
    pub const INTEGER_VALUE_OUT_OF_RANGE = "E1032";
    pub const NON_EXHAUSTIVE_MATCH = "E1033";
    pub const INEXACT_FLOAT_LITERAL = "E1034";
    pub const SHADOWED_NAME = "E1035";
    pub const CONSTANT_DIVISION_BY_ZERO = "E1036";
    pub const UNPARSABLE_CONVERSION = "E1037";
    pub const IF_VALUE_REQUIRES_ELSE = "E1038";

    // 2xxx - Syntax & Parsing
    pub const SYNTAX_ERROR = "E2001";
    pub const USE_IS_FOR_ASSIGNMENT = "E2004";
    pub const MISSING_ENTRY_POINT_FUNCTION = "E2005";
    pub const UNUSED_VARIABLE = "W2006";
    pub const UNUSED_PARAMETER = "W2007";
    pub const UNUSED_FUNCTION = "W2008";
    pub const UNUSED_IMPORT = "W2009";
    pub const UNREACHABLE_CODE = "W2010";
    pub const CONDITION_ALWAYS_TRUE = "W2011";
    pub const EXPRESSION_HAS_NO_EFFECT = "W2012";
    pub const EXPECTED_EXPRESSION = "E2006";
    pub const EXPECTED_TYPE = "E2008";
    pub const EXPECTED_CLOSING_PARENTHESIS = "E2009";
    pub const EXPECTED_CLOSING_BRACE = "E2011";
    pub const UNSUPPORTED_OPERATOR = "E2017";
    pub const CONTINUE_USED_OUTSIDE_OF_LOOP = "E2018";
    pub const BREAK_USED_OUTSIDE_OF_LOOP = "E2019";
    pub const EXPECTED_THEN = "E2021";
    pub const EXPECTED_NEWLINE = "E2023";
    pub const UNKNOWN_TYPE = "E2025";
    pub const INVALID_PLACEHOLDER_EXPRESSION = "E2028";
    pub const NESTED_DECLARATION = "E2029";
    pub const INVALID_THIS = "E2030";
    pub const NESTING_TOO_DEEP = "E2031";
    pub const KEYWORD_AS_NAME = "E2032";

    // 3xxx - Memory & Ownership
    pub const INVALID_BORROW_SOURCE = "E3005";
    pub const BORROW_CONFLICT = "E3006";

    // 5xxx - Functions & Calls
    pub const INVALID_FUNCTION_CALL = "E5002";
    pub const MISSING_PARAMETER_TYPE = "E5003";
    pub const MISSING_RETURN_TYPE = "E5004";
    pub const TOO_MANY_ARGUMENTS = "E5005";
    pub const TOO_FEW_ARGUMENTS = "E5006";
    pub const INVALID_ARGUMENT_TYPE = "E5007";
    pub const NO_DEFAULT_VALUE_FOR_PARAMETER = "E5008";
    pub const INVALID_FUNCTION_TYPE = "E5010";
    pub const MISSING_RETURN_VALUE = "E5011";
    pub const EXPECTED_LEFT_BRACE_OR_RETURNS_KEYWORD = "E5016";

    // 6xxx - Collections & Data Structures
    pub const INVALID_ARRAY_TYPE = "E6001";
    pub const INVALID_ARRAY_INDEX_TYPE = "E6006";
    pub const CANNOT_INDEX_TYPE = "E6008";
    pub const FIELD_NOT_FOUND = "E6009";
    pub const CANNOT_ACCESS_FIELD_ON_TYPE = "E6011";
    pub const INVALID_STRING_INDEX_TYPE = "E6014";
    pub const INVALID_RETURN_TYPE_FOR_UNION = "E6015";
    pub const PRIVATE_FIELD_ACCESS = "E6016";
    pub const ARRAY_REQUIRES_DYNAMIC_STORAGE = "E6018";
    pub const CANNOT_INFER_ARRAY_ELEMENT_TYPE = "E6019";

    // 7xxx - Modules & Imports
    pub const MODULE_NOT_FOUND = "E7001";
    pub const CIRCULAR_IMPORT = "E7002";
    pub const SYMBOL_NOT_FOUND_IN_IMPORT_MODULE = "E7005";
    pub const EXPECTED_MODULE_NAME = "E7006";
    pub const EXPECTED_MODULE_PATH = "E7007";
    pub const EXPECTED_IMPORT_SYMBOL = "E7014";
    pub const MISSING_FROM_KEYWORD = "E7008";
    pub const INVALID_IMPORT = "E7010";
    pub const MODULE_NAMESPACE_NOT_A_VALUE = "E7011";
    pub const MODULE_ROOT_UNKNOWN = "E7012";
    pub const DUPLICATE_STABLE_KEY = "E7013";

    // 8xxx - Internal Errors
    pub const INTERNAL_ERROR = "E8001";
    pub const NOT_IMPLEMENTED = "E8002";
    pub const PARSER_DID_NOT_ADVANCE = "E8003";
};
