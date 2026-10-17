const ast = @import("../ast/ast.zig");
const FunctionParam = ast.FunctionParam;
const TokenImport = @import("./token.zig");
const TokenType = TokenImport.TokenType;
const MemoryImport = @import("../utils/memory.zig");
const MemoryManager = MemoryImport.MemoryManager;
const Scope = MemoryImport.Scope;
const HIRType = @import("../codegen/hir/types.zig").HIRType;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;

pub const StructField = struct {
    name: []const u8,
    field_type_info: *ast.TypeInfo,
    index: u32,
    is_public: bool = false,
};

pub const Tetra = enum {
    true,
    false,
    both,
    neither,
};

pub const TokenLiteral = union(enum) {
    int: i64,
    byte: u8,
    float: f64,
    string: []const u8,
    tetra: Tetra,
    nothing: void,
    array: []TokenLiteral,
    /// A string literal with placeholders, as the lexer split it. Only a
    /// `STRING` token carries one; the parser turns it into an
    /// `InterpolatedString`, so no AST literal ever holds it.
    template: []const StringSegment,
};

/// A piece of an interpolated string literal. The lexer decides a literal's
/// pieces once, and nothing re-scans them.
pub const StringSegment = union(enum) {
    /// Literal text, its escapes decoded.
    text: []const u8,
    /// A placeholder: its source between the braces, as written, and where
    /// that source begins.
    placeholder: Placeholder,

    pub const Placeholder = struct {
        source: []const u8,
        line: usize,
        column: usize,
    };
};

pub const CustomTypeKind = enum {
    Struct,
    Enum,
    Group,
};

/// A group member as its declaration writes it: the qualifier the group uses
/// for it and the identity of the member type.
pub const GroupMemberSource = struct {
    qualifier: []const u8,
    ref: ast.TypeRef,
};

pub const EnumVariant = struct {
    name: []const u8,
    index: u32,
};

pub const CustomTypeInfo = struct {
    /// The type's identity; `ref.name` is its declared name.
    ref: ast.TypeRef,
    kind: CustomTypeKind,
    enum_variants: ?[]EnumVariant = null,
    struct_fields: ?[]StructField = null,
    group_members: ?[]GroupMemberSource = null,
};
