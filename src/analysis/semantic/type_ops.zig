//! Type utilities: deep copies, array sizes, annotations, and constant initializers.

const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const Memory = @import("../../utils/memory.zig");
const Variable = Memory.Variable;
const Reporting = @import("../../utils/reporting.zig");
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const helpers = @import("helpers.zig");
const names = @import("names.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const consteval = @import("../consteval.zig");
const resolution = @import("resolution.zig");
const Types = @import("../../types/types.zig");
const TokenLiteral = Types.TokenLiteral;
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;

pub fn deepCopyTypeInfo(self: *SemanticAnalyzer, type_info: ast.TypeInfo) !ast.TypeInfo {
    var copied = type_info;

    // Clear pointer fields; we'll rebuild them freshly as needed
    copied.array_type = null;
    copied.struct_fields = null;
    copied.function_type = null;
    copied.variants = null;
    copied.union_type = null;

    switch (type_info.base) {
        .Array => {
            if (type_info.array_type) |elem| {
                const elem_copy = try ast.TypeInfo.createDefault(self.allocator);
                elem_copy.* = try deepCopyTypeInfo(self, elem.*);
                copied.array_type = elem_copy;
            }
        },
        .Struct, .Custom => {
            if (type_info.struct_fields) |fields| {
                const new_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
                for (fields, 0..) |field, i| {
                    const ti_copy = try ast.TypeInfo.createDefault(self.allocator);
                    ti_copy.* = try deepCopyTypeInfo(self, field.type_info.*);
                    new_fields[i] = .{ .name = field.name, .type_info = ti_copy };
                }
                copied.struct_fields = new_fields;
            }
        },
        .Function => {
            if (type_info.function_type) |fn_type| {
                const params_copy = try self.allocator.alloc(ast.TypeInfo, fn_type.params.len);
                for (fn_type.params, 0..) |p, i| {
                    params_copy[i] = try deepCopyTypeInfo(self, p);
                }
                const ret_copy = try ast.TypeInfo.createDefault(self.allocator);
                ret_copy.* = try deepCopyTypeInfo(self, fn_type.return_type.*);
                const fn_copy = try self.allocator.create(ast.FunctionType);
                fn_copy.* = .{
                    .params = params_copy,
                    .return_type = ret_copy,
                    .param_aliases = if (fn_type.param_aliases) |pa| try self.allocator.dupe(bool, pa) else null,
                };
                copied.function_type = fn_copy;
            }
        },
        .Union => {
            if (type_info.union_type) |u| {
                const new_types = try self.allocator.alloc(*ast.TypeInfo, u.types.len);
                for (u.types, 0..) |member, i| {
                    const m_copy = try ast.TypeInfo.createDefault(self.allocator);
                    m_copy.* = try deepCopyTypeInfo(self, member.*);
                    new_types[i] = m_copy;
                }
                const u_copy = try self.allocator.create(ast.UnionType);
                u_copy.* = .{ .types = new_types };
                copied.union_type = u_copy;
            }
        },
        .Enum => {
            if (type_info.variants) |v| {
                // Duplicate outer slice only; inner strings are not owned here
                copied.variants = try self.allocator.dupe([]const u8, v);
            }
        },
        else => {},
    }

    return copied;
}

/// Best-effort resolution of fixed-array sizes from their declared size
/// expressions (literals, constant expressions, or references to bound
/// consts), persisting the result into `type_info`. Unlike resolveArraySizes
/// this never reports: if a size cannot be evaluated yet (e.g. during an
/// early inference pass, before the referenced consts are bound) it is left
/// unresolved for a later pass to fill in.
pub fn tryResolveArraySizes(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, type_expr: *ast.TypeExpr) void {
    if (type_expr.data != .Array) return;
    const array_type = type_expr.data.Array;
    if (type_info.base == .Array and type_info.array_size == null) {
        if (array_type.size) |size_expr| switch (consteval.evaluate(size_expr, ConstantNames{ .analyzer = self })) {
            .value => |size| if (size == .int and size.int >= 0) {
                type_info.array_size = @intCast(size.int);
                type_info.array_storage = .fixed;
            },
            .not_constant, .fault => {},
        };
    }
    if (type_info.base == .Array) {
        if (type_info.array_type) |child| {
            tryResolveArraySizes(self, child, array_type.element_type);
        }
    }
}

pub fn resolveArraySizes(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, type_expr: *ast.TypeExpr) ErrorList!void {
    switch (type_expr.data) {
        .Array => |*array_type| {
            if (array_type.size) |size_expr| {
                if (type_info.base == .Array and type_info.array_size == null) {
                    const location = getLocationFromBase(size_expr.base);
                    switch (consteval.evaluate(size_expr, ConstantNames{ .analyzer = self })) {
                        .value => |size| if (size == .int and size.int >= 0) {
                            type_info.array_size = @intCast(size.int);
                            type_info.array_storage = .fixed;
                        } else {
                            self.reporter.reportCompileError(location, ErrorCode.INVALID_ARRAY_TYPE, "array size must be a non-negative integer", .{});
                            self.fatal_error = true;
                        },
                        .not_constant => {
                            self.reporter.reportCompileError(location, ErrorCode.INVALID_ARRAY_TYPE, "array size must be a compile-time constant integer", .{});
                            self.fatal_error = true;
                        },
                        .fault => |fault| {
                            consteval.report(self.reporter, getLocationFromBase(fault.site.base), fault.fault);
                            self.fatal_error = true;
                        },
                    }
                }
            }
            if (type_info.base == .Array) {
                if (type_info.array_type) |child_type_info| {
                    try resolveArraySizes(self, child_type_info, array_type.element_type);
                }
            }
        },
        .Struct => |fields| {
            if (type_info.base == .Struct or type_info.base == .Custom) {
                if (type_info.struct_fields) |struct_fields| {
                    for (fields, struct_fields) |field, *sf| {
                        try resolveArraySizes(self, sf.type_info, field.type_expr);
                    }
                }
            }
        },
        .Union => |types| {
            if (type_info.base == .Union) {
                if (type_info.union_type) |union_type| {
                    for (types, union_type.types) |ut, ut_info| {
                        try resolveArraySizes(self, ut_info, ut);
                    }
                }
            }
        },
        else => {},
    }
}

pub fn tryTagConstLiteralArray(self: *SemanticAnalyzer, type_info: *ast.TypeInfo, initializer: *ast.Expr) void {
    if (type_info.base != .Array) return;
    if (type_info.is_mutable) return;
    if (type_info.array_storage == .fixed) return;
    if (!isCompileTimeArrayLiteral(self, initializer)) return;

    type_info.array_storage = .const_literal;
    if (type_info.array_size == null) {
        type_info.array_size = literalArrayLength(initializer);
    }
}

pub fn isCompileTimeArrayLiteral(self: *SemanticAnalyzer, expr: *ast.Expr) bool {
    return switch (expr.data) {
        .Array => |elements| blk: {
            for (elements) |element| {
                switch (element.data) {
                    .Literal => {},
                    .Array => if (!isCompileTimeArrayLiteral(self, element)) return false,
                    else => return false,
                }
            }
            break :blk true;
        },
        else => false,
    };
}

fn literalArrayLength(expr: *ast.Expr) usize {
    return switch (expr.data) {
        .Array => |elements| elements.len,
        else => 0,
    };
}

pub fn ensureDynamicArrayStorage(self: *SemanticAnalyzer, array_type: *const ast.TypeInfo, location: Location, usage: []const u8) bool {
    if (array_type.array_storage == .dynamic) return true;

    const storage_desc = switch (array_type.array_storage) {
        .fixed => "fixed-size",
        .const_literal => "const literal",
        .dynamic => unreachable,
    };

    self.reporter.reportCompileError(
        location,
        ErrorCode.ARRAY_REQUIRES_DYNAMIC_STORAGE,
        "{s} arrays cannot be used with {s}; dynamic storage is required",
        .{ storage_desc, usage },
    );
    self.fatal_error = true;
    return false;
}

/// The type an annotation names, resolved against the current record. The
/// identity is also recorded on the annotation itself, so every later
/// reader of the syntax sees the resolved type.
pub fn typeExprToTypeInfo(self: *SemanticAnalyzer, type_expr: *ast.TypeExpr) ErrorList!*ast.TypeInfo {
    try names.resolveTypeExpr(self, type_expr, self.current_module);
    return ast.typeInfoFromExpr(self.allocator, type_expr);
}

/// The value of a `const` initializer that is a constant expression;
/// null for a `var`, whose value is never known at compile time, and for
/// any other initializer. A fault is reported here.
pub fn constantInitializer(self: *SemanticAnalyzer, init_expr: *const ast.Expr, is_const: bool) error{ConstantFault}!?TokenLiteral {
    if (!is_const) return null;
    return switch (consteval.evaluate(init_expr, ConstantNames{ .analyzer = self })) {
        .value => |value| value,
        .not_constant => null,
        .fault => |fault| {
            consteval.report(self.reporter, getLocationFromBase(fault.site.base), fault.fault);
            self.fatal_error = true;
            return error.ConstantFault;
        },
    };
}

/// What `consteval` asks of analysis: the constant a name holds, and the
/// length of a fixed-size array.
const ConstantNames = struct {
    analyzer: *SemanticAnalyzer,

    fn storageOf(self: ConstantNames, expr: *const ast.Expr) ?*Memory.ValueStorage {
        if (expr.data != .Variable) return null;
        const variable = (names.lookupVariable(self.analyzer, expr.data.Variable.lexeme) catch return null) orelse return null;
        return self.analyzer.memory.scope_manager.value_storage.get(variable.storage_id);
    }

    pub fn constantOf(self: ConstantNames, expr: *const ast.Expr) ?TokenLiteral {
        return (self.storageOf(expr) orelse return null).comptime_value;
    }

    pub fn fixedLengthOf(self: ConstantNames, expr: *const ast.Expr) ?i64 {
        const storage = self.storageOf(expr) orelse return null;
        if (storage.type_info.base != .Array) return null;
        return @intCast(storage.type_info.array_size orelse return null);
    }
};
