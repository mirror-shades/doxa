const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Token = @import("../../types/token.zig").Token;
const TokenType = @import("../../types/token.zig").TokenType;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const Types = @import("../../types/types.zig");
const SoxaTypes = @import("soxa_types.zig");
const HIRType = SoxaTypes.HIRType;
const StructId = SoxaTypes.StructId;
const EnumId = SoxaTypes.EnumId;
const FunctionInfo = SoxaTypes.FunctionInfo;
const SymbolTable = @import("symbol_table.zig").SymbolTable;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const SemanticAnalyzer = @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer;
const UnionTable = @import("../../common/union_table.zig").UnionTable;
const TypeRef = ast.TypeRef;

/// Codegen's view of the program's types. Named types are keyed by their
/// canonical key (`ModuleGraph.typeKey`), never by a spelling; an expression's
/// type is the analyzer's answer wherever the analyzer recorded one.
pub const TypeSystem = struct {
    custom_types: std.StringHashMap(CustomTypeInfo),
    allocator: std.mem.Allocator,
    reporter: *Reporting.Reporter,
    semantic: *const SemanticAnalyzer,
    /// The analyzer's union table: the one part of analysis codegen extends,
    /// since lowering may name a union analysis never spelled.
    unions: *UnionTable,

    pub const CustomTypeInfo = struct {
        name: []const u8,
        kind: Types.CustomTypeKind,
        enum_variants: ?[]Types.EnumVariant = null,
        struct_fields: ?[]StructField = null,
        group_members: ?[]GroupMemberSource = null,

        pub const CustomTypeKind = Types.CustomTypeKind;
        pub const EnumVariant = Types.EnumVariant;

        /// A group member: the qualifier the group writes and the member
        /// type's canonical key.
        pub const GroupMemberSource = struct {
            qualifier: []const u8,
            key: []const u8,
        };

        pub const StructField = struct {
            name: []const u8,
            field_type: HIRType,
            index: u32,
            custom_type_name: ?[]const u8 = null,
        };

        pub fn getEnumVariantIndex(self: *const CustomTypeInfo, variant_name: []const u8) ?u32 {
            if ((self.kind != .Enum and self.kind != .Group) or self.enum_variants == null) {
                return null;
            }
            return Types.enumVariantIndex(self.enum_variants.?, variant_name);
        }

        pub fn getStructFieldIndex(self: *const CustomTypeInfo, field_name: []const u8) ?u32 {
            if (self.kind != .Struct or self.struct_fields == null) return null;
            return Types.structFieldIndex(self.struct_fields.?, field_name);
        }
    };

    pub const FieldResolveResult = struct { t: HIRType, custom_type_name: ?[]const u8 = null };

    /// The struct type a canonical key names.
    pub fn structTypeForName(self: *TypeSystem, key: []const u8) HIRType {
        return HIRType{ .Struct = self.semantic.struct_table.idByKey(key) orelse 0 };
    }

    /// The enum type a canonical key names.
    pub fn enumTypeForName(self: *TypeSystem, key: []const u8) HIRType {
        return HIRType{ .Enum = self.semantic.enum_table.idByKey(key) orelse 0 };
    }

    /// The HIR type a canonical key names.
    pub fn customTypeForName(self: *TypeSystem, key: []const u8) HIRType {
        const custom_type = self.custom_types.get(key) orelse return .Unknown;
        return switch (custom_type.kind) {
            .Struct => self.structTypeForName(key),
            .Enum => self.enumTypeForName(key),
            .Group => HIRType{ .Group = self.semantic.group_table.idByKey(key) orelse 0 },
        };
    }

    /// The canonical key of a named type, read from the table that registered
    /// it.
    pub fn refKey(self: *const TypeSystem, ref: TypeRef) ?[]const u8 {
        const custom = self.semantic.custom_types.get(ref) orelse return null;
        return switch (custom.kind) {
            .Struct => self.semantic.struct_table.keyOf(self.semantic.struct_table.idOf(ref) orelse return null),
            .Enum => self.semantic.enum_table.keyOf(self.semantic.enum_table.idOf(ref) orelse return null),
            .Group => self.semantic.group_table.keyOf(self.semantic.group_table.idOf(ref) orelse return null),
        };
    }

    /// The HIR type of a named type.
    pub fn typeForRef(self: *const TypeSystem, ref: TypeRef) HIRType {
        const custom = self.semantic.custom_types.get(ref) orelse return .Unknown;
        return switch (custom.kind) {
            .Struct => HIRType{ .Struct = self.semantic.struct_table.idOf(ref) orelse 0 },
            .Enum => HIRType{ .Enum = self.semantic.enum_table.idOf(ref) orelse 0 },
            .Group => HIRType{ .Group = self.semantic.group_table.idOf(ref) orelse 0 },
        };
    }

    pub fn init(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, semantic: *const SemanticAnalyzer, unions: *UnionTable) TypeSystem {
        return TypeSystem{
            .custom_types = std.StringHashMap(CustomTypeInfo).init(allocator),
            .allocator = allocator,
            .reporter = reporter,
            .semantic = semantic,
            .unions = unions,
        };
    }

    pub fn deinit(self: *TypeSystem) void {
        self.custom_types.deinit();
    }

    /// The names a peek lists for a group value: its flattened members, in
    /// the order its box indexes them.
    pub fn getGroupMemberNames(self: *TypeSystem, group_key: []const u8) ![][]const u8 {
        const groups = &self.semantic.group_table;
        const id = groups.idByKey(group_key) orelse return &.{};
        const members = groups.members(id) orelse return &.{};
        const names = try self.allocator.alloc([]const u8, members.len);
        for (members, names) |member, *name| name.* = member.qualifier;
        return names;
    }

    pub fn convertTypeInfo(self: *TypeSystem, type_info: ast.TypeInfo) HIRType {
        return switch (type_info.base) {
            .Int => .Int,
            .Float => .Float,
            .String => .String,
            .Tetra => .Tetra,
            .Byte => .Byte,
            .Array => {
                if (type_info.array_type) |element_type| {
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = self.convertTypeInfo(element_type.*);
                    return HIRType{ .Array = element_type_ptr };
                } else {
                    const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    unknown_ptr.* = .Unknown;
                    return HIRType{ .Array = unknown_ptr };
                }
            },
            .Map => {
                const key_type = self.allocator.create(HIRType) catch return .Unknown;
                const value_type = self.allocator.create(HIRType) catch return .Unknown;

                if (type_info.map_key_type) |kt| {
                    key_type.* = self.convertTypeInfo(kt.*);
                } else {
                    key_type.* = .String;
                }

                if (type_info.map_value_type) |vt| {
                    value_type.* = self.convertTypeInfo(vt.*);
                } else {
                    value_type.* = .Unknown;
                }

                return HIRType{ .Map = .{ .key = key_type, .value = value_type } };
            },
            .Union => blk: {
                const ut = type_info.union_type orelse break :blk .Unknown;
                const lowered = self.allocator.alloc(*const HIRType, ut.types.len) catch break :blk .Unknown;
                defer self.allocator.free(lowered);
                for (ut.types, lowered) |member, *slot| {
                    const member_ptr = self.allocator.create(HIRType) catch break :blk .Unknown;
                    member_ptr.* = self.convertTypeInfo(member.*);
                    slot.* = member_ptr;
                }
                break :blk self.unions.intern(self.semantic.unionNames(), lowered) catch .Unknown;
            },
            .Struct => if (type_info.custom_type) |custom| self.typeForRef(custom.resolved()) else .Nothing,
            .Custom => if (type_info.custom_type) |custom| self.typeForRef(custom.resolved()) else .Unknown,
            // A resolved enum may arrive as `.Enum{custom}`; an anonymous
            // `.Enum` has no type to lower to.
            .Enum => if (type_info.custom_type) |custom| self.typeForRef(custom.resolved()) else .Nothing,
            else => .Nothing,
        };
    }

    /// The HIR view of the type the analyzer inferred for `expr`, or `null` when
    /// the analyzer never visited it. Codegen asks for a builtin's type this way,
    /// so the semantic layer (`inferBuiltinCall`) owns the rules and HIR only
    /// lowers the answer.
    fn hirTypeFromSemanticCache(self: *TypeSystem, expr: *ast.Expr) ?HIRType {
        const type_info = self.semantic.getCachedExprType(expr) orelse return null;
        return self.convertTypeInfo(type_info.*);
    }

    /// The type an expression names in value position (`Color` in
    /// `Color.Red`, a constructor's receiver), as analysis resolved it.
    fn namedType(self: *TypeSystem, expr: *ast.Expr) ?TypeRef {
        const resolved = self.semantic.resolutionOf(expr) orelse return null;
        return switch (resolved) {
            .type => |ref| ref,
            else => null,
        };
    }

    pub fn inferTypeFromLiteral(_: *TypeSystem, literal: TokenLiteral) HIRType {
        return switch (literal) {
            .int => .Int,
            .float => .Float,
            .string => .String,
            .tetra => .Tetra,
            .byte => .Byte,
            .nothing => .Nothing,
            else => .Unknown,
        };
    }

    /// A binding narrowed by `as` is tracked as a single-member union view so
    /// the backend can unwrap its box on load; structurally it *is* that
    /// member, so field resolution must see the member and not the view.
    pub fn memberView(t: HIRType) HIRType {
        if (t == .Union and t.Union.members.len == 1) return t.Union.members[0].*;
        return t;
    }

    /// The struct a group contributes for a field read. A field can only be read
    /// through a group once the value has been narrowed to the member that owns
    /// it, so exactly one member may declare `field_name`; two members that
    /// disagree mean we cannot answer and the read stays unresolved.
    pub fn groupMemberStructForField(self: *TypeSystem, group_id: u32, field_name: []const u8) ?u32 {
        const const_table = &self.semantic.struct_table;
        const members = self.semantic.group_table.members(group_id) orelse return null;
        var found: ?u32 = null;
        for (members) |member| {
            if (member.kind != .Struct) continue;
            const fields = const_table.fields(member.id) orelse continue;
            for (fields) |f| {
                if (!std.mem.eql(u8, f.name, field_name)) continue;
                if (found) |prev| {
                    if (prev != member.id) return null;
                }
                found = member.id;
                break;
            }
        }
        return found;
    }

    pub fn resolveFieldAccessType(self: *TypeSystem, e: *ast.Expr, symbol_table: *SymbolTable) ?FieldResolveResult {
        return switch (e.data) {
            .Variable => |var_token| blk: {
                // A variable whose declaration tracked its named type.
                if (symbol_table.getVariableCustomType(var_token.lexeme)) |key| {
                    break :blk FieldResolveResult{ .t = self.customTypeForName(key), .custom_type_name = key };
                }
                // A type named in value position (`Point` in `Point.origin`).
                if (self.namedType(e)) |ref| {
                    break :blk FieldResolveResult{ .t = self.typeForRef(ref), .custom_type_name = self.refKey(ref) };
                }
                // Otherwise the analyzer's type for this occurrence.
                if (self.semantic.getCachedExprType(e)) |type_info| {
                    break :blk FieldResolveResult{
                        .t = self.convertTypeInfo(type_info.*),
                        .custom_type_name = if (type_info.custom_type) |custom| self.refKey(custom.resolved()) else null,
                    };
                }
                break :blk FieldResolveResult{
                    .t = symbol_table.getTrackedVariableType(var_token.lexeme) orelse .Unknown,
                    .custom_type_name = null,
                };
            },
            .This => blk: {
                // `this` is the receiver the method bound on entry.
                if (symbol_table.getVariableCustomType("this")) |key| {
                    break :blk FieldResolveResult{ .t = self.structTypeForName(key), .custom_type_name = key };
                }
                break :blk FieldResolveResult{ .t = HIRType{ .Struct = 0 }, .custom_type_name = null };
            },
            .FieldAccess => |fa| blk: {
                // `Color.Red`: the qualifier names an enum.
                if (self.namedType(fa.object)) |ref| {
                    if (self.semantic.custom_types.get(ref)) |custom| {
                        if (custom.kind == .Enum) {
                            return FieldResolveResult{ .t = self.typeForRef(ref), .custom_type_name = self.refKey(ref) };
                        }
                    }
                }
                // First, try to resolve based on the object's custom type name (works
                // for plain struct variables and nested field access).
                if (self.resolveFieldAccessType(fa.object, symbol_table)) |base| {
                    if (base.custom_type_name) |struct_name| {
                        if (self.custom_types.get(struct_name)) |ctype| {
                            if (ctype.kind == .Struct) {
                                if (ctype.struct_fields) |fields| {
                                    for (fields) |f| {
                                        if (std.mem.eql(u8, f.name, fa.field.lexeme)) {
                                            return FieldResolveResult{ .t = f.field_type, .custom_type_name = f.custom_type_name };
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // The object's own type decides which struct's fields to read. A
                // group value is narrowed to one of its members before a field can
                // be read through it, so unwrap a single-member union view first.
                const obj_type = memberView(self.inferTypeFromExpression(fa.object, symbol_table));

                // Fallback: if we know the object's HIR type is a struct via the
                // semantic struct table (e.g., for array-of-struct indexing like
                // zoo[0].name), use the struct_id and field metadata from there.
                const const_table = &self.semantic.struct_table;
                switch (obj_type) {
                    .Struct => |sid| {
                        if (const_table.fields(sid)) |fields| {
                            for (fields) |f| {
                                if (std.mem.eql(u8, f.name, fa.field.lexeme)) {
                                    // Prefer enum type name when this field is an enum,
                                    // so that peek on zoo[0].animal_type can report
                                    // "Species" instead of generic "enum".
                                    var result_name: ?[]const u8 = null;

                                    // For enum-typed fields, use the AST custom_type
                                    // when available (e.g., "Species").
                                    const ti = f.type_info.*;
                                    if (f.hir_type == .Enum) {
                                        if (ti.custom_type) |custom| result_name = self.refKey(custom.resolved());
                                    } else if (f.nested_struct_id) |nested_id| {
                                        result_name = const_table.keyOf(nested_id);
                                    }

                                    return FieldResolveResult{
                                        .t = f.hir_type,
                                        .custom_type_name = result_name,
                                    };
                                }
                            }
                        }
                    },
                    .Group => |gid| {
                        // Semantic narrows the binding to the matched member, so
                        // exactly one member declares the field. Report ambiguity
                        // by not resolving rather than guessing which member.
                        if (self.groupMemberStructForField(gid, fa.field.lexeme)) |sid| {
                            if (const_table.fields(sid)) |fields| {
                                for (fields) |f| {
                                    if (std.mem.eql(u8, f.name, fa.field.lexeme)) {
                                        break :blk FieldResolveResult{
                                            .t = f.hir_type,
                                            .custom_type_name = const_table.keyOf(sid),
                                        };
                                    }
                                }
                            }
                        }
                    },
                    else => {},
                }

                break :blk null;
            },
            else => null,
        };
    }

    pub fn inferTypeFromExpression(self: *TypeSystem, expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const result = switch (expr.data) {
            .Literal => |lit| self.inferTypeFromLiteral(lit),
            .InterpolatedString => .String,
            .Exists => .Tetra,
            .ForAll => .Tetra,
            .StructLiteral => if (self.namedType(expr)) |ref| self.typeForRef(ref) else HIRType{ .Struct = 0 },
            .Map => |map_expr| {
                const entries = map_expr.entries;
                const key_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                const value_type_ptr = self.allocator.create(HIRType) catch return .Unknown;

                if (entries.len > 0) {
                    const first = entries[0];
                    const inferred_key = self.inferTypeFromExpression(first.key, symbol_table);
                    const inferred_val = self.inferTypeFromExpression(first.value, symbol_table);

                    key_type_ptr.* = switch (inferred_key) {
                        // Enums are represented as integer discriminants at runtime.
                        .Enum => .Int,
                        else => inferred_key,
                    };
                    value_type_ptr.* = inferred_val;
                } else {
                    key_type_ptr.* = .String;
                    value_type_ptr.* = .Unknown;
                }

                return HIRType{ .Map = .{ .key = key_type_ptr, .value = value_type_ptr } };
            },
            .Variable => |var_token| {
                // A type named in value position.
                if (self.namedType(expr)) |ref| return self.typeForRef(ref);

                // An active `as`/match narrowing lives only in the symbol
                // table; it narrows the variable for the branch and must win
                // over the semantic scope's declared type. Outside a narrowing
                // this map is empty and the declared type is used below.
                if (symbol_table.getVariableNarrowing(var_token.lexeme)) |narrowed| {
                    return narrowed;
                }

                if (symbol_table.current_function != null) {
                    const var_type = symbol_table.getTrackedVariableType(var_token.lexeme);
                    if (var_type != null and var_type.? != .Unknown) {
                        return var_type.?;
                    }
                }

                if (self.hirTypeFromSemanticCache(expr)) |analyzed| return analyzed;
                return symbol_table.getTrackedVariableType(var_token.lexeme) orelse .Unknown;
            },
            .FieldAccess => {
                if (self.resolveFieldAccessType(expr, symbol_table)) |res| {
                    if (res.t != .Unknown) return res.t;
                }
                return self.hirTypeFromSemanticCache(expr) orelse .Unknown;
            },
            .EnumMember => self.hirTypeFromSemanticCache(expr) orelse .Unknown,
            .Binary => |binary| {
                // Use the centralized binary operation result type inference
                // which correctly handles division (always Float) and other type promotions
                return self.inferBinaryOpResultType(binary.operator.type, binary.left.?, binary.right.?, symbol_table);
            },
            // A call's type is its callee's declared return type, which the
            // analyzer recorded for the call expression.
            .FunctionCall => self.hirTypeFromSemanticCache(expr) orelse .Unknown,
            .Array => {
                const elements = expr.data.Array;
                if (elements.len > 0) {
                    const element_type = self.inferTypeFromExpression(elements[0], symbol_table);
                    const element_type_ptr = self.allocator.create(HIRType) catch return .Unknown;
                    element_type_ptr.* = element_type;
                    return HIRType{ .Array = element_type_ptr };
                }
                const unknown_ptr = self.allocator.create(HIRType) catch return .Unknown;
                unknown_ptr.* = .Unknown;
                return HIRType{ .Array = unknown_ptr };
            },
            .Index => |index| {
                const container_type = self.inferTypeFromExpression(index.array, symbol_table);
                return switch (container_type) {
                    .Array => |element_type_ptr| {
                        return element_type_ptr.*;
                    },
                    .String => .String,
                    .Map => blk: {
                        const map_info = container_type.Map;
                        const value_ty = map_info.value.*;
                        // Prefer declared value type when available; fall back to int for untyped maps.
                        const actual_value_ty = if (value_ty != .Unknown and value_ty != .Nothing) value_ty else .Int;
                        const has_else = self.mapExpressionHasElse(index.array);
                        if (has_else) {
                            break :blk actual_value_ty;
                        }

                        const value_ptr = self.allocator.create(HIRType) catch break :blk .Unknown;
                        value_ptr.* = actual_value_ty;
                        const nothing_ptr = self.allocator.create(HIRType) catch break :blk .Unknown;
                        nothing_ptr.* = .Nothing;
                        const members = [_]*const HIRType{ value_ptr, nothing_ptr };
                        break :blk self.unions.intern(self.semantic.unionNames(), &members) catch .Unknown;
                    },
                    else => .String,
                };
            },
            // Single authority: the semantic layer types every `@`-call
            // (`inferBuiltinCall`, driven by `builtin_methods`) during analysis;
            // codegen reads that answer instead of re-deriving the rules.
            .InternalCall => self.hirTypeFromSemanticCache(expr) orelse .Unknown,
            .Logical => .Tetra,
            .Unary => |unary| {
                if (unary.operator.type == .MINUS) {
                    if (unary.right) |right| {
                        const operand_type = self.inferTypeFromExpression(right, symbol_table);
                        return if (operand_type == .Unknown) .Int else operand_type;
                    }
                    return .Int;
                }
                return .Tetra;
            },
            .Grouping => |grouping| {
                if (grouping) |inner_expr| {
                    return self.inferTypeFromExpression(inner_expr, symbol_table);
                } else {
                    return .Nothing;
                }
            },
            .Block => |block| {
                // A block expression's value is its final (or lifted) expression;
                // a block with no value yields `nothing`. Without this case a
                // block-valued expression (e.g. a match arm `{ arr }`) fell
                // through to the `else => .String` default and was mis-typed.
                if (block.value) |value_expr| {
                    return self.inferTypeFromExpression(value_expr, symbol_table);
                }
                return .Nothing;
            },
            .Range => {
                const element_type = self.allocator.create(HIRType) catch return .Unknown;
                element_type.* = .Int;
                return HIRType{ .Array = element_type };
            },
            .Cast => |cast| {
                // For 'as' / cast expressions, the result type is the target type.
                // Reuse the existing AST -> TypeInfo -> HIRType lowering to stay
                // consistent with semantic analysis.
                const type_info_ptr = ast.typeInfoFromExpr(self.allocator, cast.target_type) catch return .Unknown;
                defer self.allocator.destroy(type_info_ptr);
                return self.convertTypeInfo(type_info_ptr.*);
            },
            .Increment => |operand| {
                // Increment returns the same type as the operand
                const operand_type = self.inferTypeFromExpression(operand, symbol_table);
                // If operand type is unknown, default to Int (for literals like 5++)
                return if (operand_type == .Unknown) .Int else operand_type;
            },
            .Decrement => |operand| {
                // Decrement returns the same type as the operand
                const operand_type = self.inferTypeFromExpression(operand, symbol_table);
                // If operand type is unknown, default to Int (for literals like 5--)
                return if (operand_type == .Unknown) .Int else operand_type;
            },
            .Match => |match_expr| {
                // Infer type from the first case body (all cases should return the same type)
                if (match_expr.cases.len > 0) {
                    const first_case_body = match_expr.cases[0].body;
                    return self.inferTypeFromExpression(first_case_body, symbol_table);
                }
                return .Unknown;
            },
            .If => |if_expr| {
                // Infer type from the then branch (and else branch if it exists)
                if (if_expr.then_branch) |then_branch| {
                    const then_type = self.inferTypeFromExpression(then_branch, symbol_table);
                    if (if_expr.else_branch) |else_branch| {
                        const else_type = self.inferTypeFromExpression(else_branch, symbol_table);
                        // Try to find a common type
                        if (@as(std.meta.Tag(HIRType), then_type) == @as(std.meta.Tag(HIRType), else_type)) {
                            return then_type;
                        }
                        // For numeric types, try to find common type
                        const then_tag = @as(std.meta.Tag(HIRType), then_type);
                        const else_tag = @as(std.meta.Tag(HIRType), else_type);
                        if ((then_tag == .Int or then_tag == .Float or then_tag == .Byte) and
                            (else_tag == .Int or else_tag == .Float or else_tag == .Byte))
                        {
                            // Use computeNumericCommonType to find common type
                            // We need a dummy operator type - use PLUS as it's the most permissive
                            const common_type = self.computeNumericCommonType(then_type, else_type, .PLUS);
                            if (common_type != .Unknown) {
                                return common_type;
                            }
                        }
                        // Default to then_type if we can't find a common type
                        return then_type;
                    }
                    return then_type;
                }
                return .Unknown;
            },
            .This => if (symbol_table.getVariableCustomType("this")) |key| self.structTypeForName(key) else .Unknown,
            else => .String,
        };
        return result;
    }

    pub fn inferBinaryOpResultType(self: *TypeSystem, operator_type: TokenType, left_expr: *ast.Expr, right_expr: *ast.Expr, symbol_table: *SymbolTable) HIRType {
        const left_type = self.inferTypeFromExpression(left_expr, symbol_table);
        const right_type = self.inferTypeFromExpression(right_expr, symbol_table);

        if (operator_type == .PLUS) {
            if (left_type == .String and right_type == .String) {
                return .String;
            } else if (left_type == .Array and right_type == .Array) {
                // Array concatenation: return the left array type (both should have same element type)
                return left_type;
            } else {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use + operator between {s} and {s}",
                    .{ @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown;
            }
        }

        const result_type = switch (operator_type) {
            .MINUS, .ASTERISK, .SLASH, .MODULO, .POWER => {
                const common_type = self.computeNumericCommonType(left_type, right_type, operator_type);
                if (common_type != .Unknown) {
                    return common_type;
                }
                self.reporter.reportCompileError(
                    left_expr.base.location(),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use {s} operator between {s} and {s}",
                    .{ @tagName(operator_type), @tagName(left_type), @tagName(right_type) },
                );
                return .Unknown;
            },
            .EQUALITY, .BANG_EQUAL, .LESS, .GREATER, .LESS_EQUAL, .GREATER_EQUAL => {
                return .Tetra;
            },
            else => .Int,
        };

        return result_type;
    }

    /// Centralized numeric type promotion rules
    /// 1. Float dominance: If either operand is Float or operator is division (/), promote to Float
    /// 2. Int fallback: If either operand is Int, promote to Int
    /// 3. No promotion: If neither is Float or Int, operands are Byte
    pub fn computeNumericCommonType(_: *TypeSystem, left_type: HIRType, right_type: HIRType, operator_type: TokenType) HIRType {
        const resolved_left = if (left_type == .Unknown) .Int else left_type;
        const resolved_right = if (right_type == .Unknown) .Int else right_type;

        if (resolved_left == .String or resolved_right == .String or
            resolved_left == .Array or resolved_right == .Array or
            resolved_left == .Map or resolved_right == .Map or
            resolved_left == .Struct or resolved_right == .Struct or
            resolved_left == .Enum or resolved_right == .Enum or
            resolved_left == .Union or resolved_right == .Union or
            resolved_left == .Function or resolved_right == .Function or
            resolved_left == .Nothing or resolved_right == .Nothing)
        {
            return .Unknown;
        }

        if (operator_type == .SLASH) {
            return .Float;
        }

        if (operator_type == .DOUBLE_SLASH) {
            // Integer division only works on integer types
            if (resolved_left == .Float or resolved_right == .Float) {
                return .Unknown; // Type error
            }
        }

        if (resolved_left == .Float or resolved_right == .Float) {
            return .Float;
        }

        if (resolved_left == .Int or resolved_right == .Int) {
            return .Int;
        }

        if (resolved_left == .Byte and resolved_right == .Byte) {
            return .Byte;
        }

        return .Unknown;
    }

    fn mapExpressionHasElse(self: *TypeSystem, expr: *ast.Expr) bool {
        return switch (expr.data) {
            .MapLiteral => |map_literal| map_literal.else_value != null,
            .Variable => blk: {
                const type_info = self.semantic.getCachedExprType(expr) orelse break :blk false;
                break :blk type_info.base == .Map and type_info.map_has_else_value;
            },
            else => false,
        };
    }

    pub fn astTypeToLowerName(_: *TypeSystem, base: ast.Type) []const u8 {
        return switch (base) {
            .Int => "int",
            .Byte => "byte",
            .Float => "float",
            .String => "string",
            .Tetra => "tetra",
            .Nothing => "nothing",
            .Array => "array",
            .Struct => "struct",
            .Enum => "enum",
            .Map => "map",
            .Function => "function",
            .Custom => "custom",
            .Union => "union",
        };
    }
};
