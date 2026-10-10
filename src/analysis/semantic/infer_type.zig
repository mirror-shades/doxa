const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const Errors = @import("../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const ErrorList = Errors.ErrorList;
const TokenLiteral = @import("../../types/types.zig").TokenLiteral;
const helpers = @import("./helpers.zig");
const unifyTypes = helpers.unifyTypes;
const getLocationFromBase = helpers.getLocationFromBase;
const eval = @import("eval_utils.zig");
const names = @import("names.zig");
const ParamRef = @import("semantic.zig").ParamRef;
const TypeRef = ast.TypeRef;
const Scope = @import("../../utils/memory.zig").Scope;
const builtin_methods = @import("../../runtime/builtin_methods.zig");

pub const SemanticError = std.mem.Allocator.Error || ErrorList;

/// The type of a builtin's subject argument. Expression inference can miss a
/// variable's type (e.g. a global or an imported binding) even though its
/// declaration is in scope, so fall back to the stored declaration type.
fn inferBuiltinSubjectType(self: *SemanticAnalyzer, subject: *ast.Expr) SemanticError!*ast.TypeInfo {
    const subject_type = try inferTypeFromExpr(self, subject);
    // An empty literal takes its element type from its context, and a
    // built-in that accepts any array is all the context it has: it holds
    // no element, so it is an array of `nothing`.
    if (subject.data == .Array and subject.data.Array.len == 0 and subject_type.base == .Array and subject_type.array_type == null) {
        const element = try ast.TypeInfo.createDefault(self.allocator);
        element.* = .{ .base = .Nothing };
        subject_type.array_type = element;
    }
    if (subject_type.base != .Nothing or subject.data != .Variable) return subject_type;
    if (try names.lookupVariable(self, subject.data.Variable.lexeme)) |variable| {
        if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
            return storage.type_info;
        }
    }
    return subject_type;
}

/// The arguments of a built-in as one list: the receiver, when there is one,
/// followed by the written arguments. Every call shape that reaches a `@` rule
/// builds the list this way, so the list is not a per-branch concern.
fn builtinArgs(
    self: *SemanticAnalyzer,
    receiver: ?*ast.Expr,
    rest: []const *ast.Expr,
) SemanticError![](*ast.Expr) {
    const buffer = try self.allocator.alloc(*ast.Expr, rest.len + 1);
    const prefix: usize = if (receiver) |r| blk: {
        buffer[0] = r;
        break :blk 1;
    } else 0;
    @memcpy(buffer[prefix .. prefix + rest.len], rest);
    return buffer[0 .. prefix + rest.len];
}

/// Type rules for the built-in `@`-methods, reached by every call shape.
/// `receiver` is absent only for `@std`, whose argument list is empty.
///
/// Memoizing entry point: the `.InternalCall` cases of `inferTypeFromExpr`
/// return straight through here, so without caching the result a builtin is
/// re-inferred — and a failing one re-reported — on every analysis pass over
/// the same expression.
fn inferBuiltinCall(
    self: *SemanticAnalyzer,
    expr: *ast.Expr,
    fname: []const u8,
    receiver: ?*ast.Expr,
    rest: []const *ast.Expr,
) SemanticError!*ast.TypeInfo {
    const start = self.reporter.diagnostics.items.len;
    const result = try inferBuiltinCallInner(self, expr, fname, receiver, rest);
    try self.type_cache.put(expr.base.id, result);

    // Every argument of every builtin ends up with a type, whichever rule above
    // happened to need. A rule is free to validate only its subject — `@find`
    // reads `args[0]` and stops — but the generator asks for the type of an
    // argument wherever one is written, so an argument no rule visited was a
    // program the lowering rejected: `@find(text, @pack([0]))` never reached the
    // inner `@pack`, and the array literal's node had no analyzed type.
    //
    // This is the walk being made total for builtins, and it belongs here rather
    // than in each rule so that totality is a property of the dispatch instead
    // of a thing every new `@` method has to remember. The rules keep their own
    // checks; this only fills the cache, and it runs after them so a rule that
    // reported an error does not report a second one underneath it.
    if (self.reporter.firstErrorSince(start) != null) return result;
    const args = try builtinArgs(self, receiver, rest);
    defer self.allocator.free(args);
    for (args) |arg| _ = try inferTypeFromExpr(self, arg);

    return result;
}

fn inferBuiltinCallInner(
    self: *SemanticAnalyzer,
    expr: *ast.Expr,
    fname: []const u8,
    receiver: ?*ast.Expr,
    rest: []const *ast.Expr,
) SemanticError!*ast.TypeInfo {
    const args = try builtinArgs(self, receiver, rest);
    defer self.allocator.free(args);

    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    errdefer self.allocator.destroy(type_info);

    type_info.* = .{ .base = .Nothing };

    // Helper to validate argument count and return early if invalid
    // Returns true if validation passed, false if we should return early
    const validateBuiltinArgs = struct {
        fn check(sem: *SemanticAnalyzer, e: *ast.Expr, name: []const u8, arg_count: usize) bool {
            if (builtin_methods.getArgCountRangeByName(name)) |range| {
                if (arg_count < range.min or arg_count > range.max) {
                    if (arg_count < range.min) {
                        sem.reporter.reportCompileError(
                            getLocationFromBase(e.base),
                            ErrorCode.TOO_FEW_ARGUMENTS,
                            "Too few arguments to @{s}: expected {d}, got {d}",
                            .{ name, range.min, arg_count },
                        );
                    } else {
                        sem.reporter.reportCompileError(
                            getLocationFromBase(e.base),
                            ErrorCode.TOO_MANY_ARGUMENTS,
                            "Too many arguments to @{s}: expected {d}, got {d}",
                            .{ name, range.max, arg_count },
                        );
                    }
                    sem.fatal_error = true;
                    return false;
                }
                return true;
            }
            return true; // If method not found, let it fall through to manual handling
        }
    };

    if (std.mem.eql(u8, fname, "length")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const t0 = try inferBuiltinSubjectType(self, args[0]);
        if (t0.base != .Array and t0.base != .String) {
            self.reporter.reportCompileError(
                getLocationFromBase(args[0].base),
                ErrorCode.INVALID_ARRAY_TYPE,
                "@length requires array or string, got {s}",
                .{@tagName(t0.base)},
            );
            self.fatal_error = true;
            return type_info;
        }
        type_info.* = .{ .base = .Int };
        return type_info;
    } else if (std.mem.eql(u8, fname, "push")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        const val_t = if (coll_t.base == .Array and coll_t.array_type != null)
            try inferTypeIn(self, args[1], coll_t.array_type.?)
        else
            try inferTypeFromExpr(self, args[1]);
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@push")) return type_info;
            if (coll_t.array_type) |elem| {
                try helpers.unifyElement(self, elem, val_t, args[1], .{ .location = getLocationFromBase(args[1].base) });
            } else if (val_t.base == .Array and val_t.array_type != null) {
                self.reporter.reportCompileError(
                    getLocationFromBase(args[1].base),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot push typed array into array with unspecified element type",
                    .{},
                );
                self.fatal_error = true;
            }
        } else if (coll_t.base == .String) {
            // A string is not a byte array: appending is string concatenation,
            // so the value must itself be a string. `byte[]` crosses with `@pack`.
            if (val_t.base != .String) {
                self.reporter.reportCompileError(
                    getLocationFromBase(args[1].base),
                    ErrorCode.TYPE_MISMATCH,
                    "@push on string requires string value, got {s}",
                    .{@tagName(val_t.base)},
                );
                self.fatal_error = true;
            }
        } else {
            self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@push requires array or string, got {s}", .{@tagName(coll_t.base)});
            self.fatal_error = true;
        }
        return type_info;
    } else if (std.mem.eql(u8, fname, "pop")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        if (args[0].data == .Variable and std.mem.eql(u8, args[0].data.Variable.lexeme, "list")) {
            if (coll_t.base == .Array) {
                if (coll_t.array_type) |elem| type_info.* = elem.*;
            } else if (coll_t.base == .String) {
                type_info.* = .{ .base = .String };
            } else {
                self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@pop requires array or string, got {s}", .{@tagName(coll_t.base)});
                self.fatal_error = true;
                return type_info;
            }
        }
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@pop")) return type_info;
            if (coll_t.array_type) |elem| type_info.* = elem.*;
            return type_info;
        } else if (coll_t.base == .String) {
            type_info.* = .{ .base = .String };
            return type_info;
        } else {
            self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@pop requires array or string, got {s}", .{@tagName(coll_t.base)});
            self.fatal_error = true;
            return type_info;
        }
    } else if (std.mem.eql(u8, fname, "insert")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        const idx_t = try inferTypeFromExpr(self, args[1]);
        if (idx_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@insert index must be int, got {s}", .{@tagName(idx_t.base)});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@insert")) return type_info;
            const val_t = if (coll_t.array_type) |elem| try inferTypeIn(self, args[2], elem) else try inferTypeFromExpr(self, args[2]);
            if (coll_t.array_type) |elem| {
                try helpers.unifyElement(self, elem, val_t, args[2], .{ .location = getLocationFromBase(args[2].base) });
            }
        } else if (coll_t.base == .String) {
            // A string is not a byte array: only a string can be inserted.
            // `byte[]` crosses with `@pack`.
            const val_t = try inferTypeFromExpr(self, args[2]);
            if (val_t.base != .String) {
                self.reporter.reportCompileError(
                    getLocationFromBase(args[2].base),
                    ErrorCode.TYPE_MISMATCH,
                    "@insert on string requires string value, got {s}",
                    .{@tagName(val_t.base)},
                );
                self.fatal_error = true;
            }
        } else {
            self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@insert requires array or string, got {s}", .{@tagName(coll_t.base)});
            self.fatal_error = true;
            return type_info;
        }
        return type_info;
    } else if (std.mem.eql(u8, fname, "remove")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        const idx_t = try inferTypeFromExpr(self, args[1]);
        if (idx_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@remove index must be int, got {s}", .{@tagName(idx_t.base)});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@remove")) return type_info;
            if (coll_t.array_type) |elem| {
                type_info.* = elem.*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
            return type_info;
        } else if (coll_t.base == .String) {
            type_info.* = .{ .base = .String };
            return type_info;
        }
        self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARRAY_TYPE, "@remove requires array or string, got {s}", .{@tagName(coll_t.base)});
        self.fatal_error = true;
        return type_info;
    } else if (std.mem.eql(u8, fname, "slice")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        const start_t = try inferTypeFromExpr(self, args[1]);
        const len_t = try inferTypeFromExpr(self, args[2]);
        if (start_t.base != .Int or len_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice start/length must be ints", .{});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .String) {
            type_info.* = .{ .base = .String };
        } else if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@slice")) return type_info;
            if (coll_t.array_type) |elem| {
                const new_elem = try ast.TypeInfo.createDefault(self.allocator);
                new_elem.* = elem.*;
                type_info.* = .{ .base = .Array, .array_type = new_elem };
            } else {
                type_info.* = .{ .base = .Array };
            }
        } else {
            self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice requires array or string, got {s}", .{@tagName(coll_t.base)});
            self.fatal_error = true;
        }
        return type_info;
    } else if (std.mem.eql(u8, fname, "string") or
        std.mem.eql(u8, fname, "int") or
        std.mem.eql(u8, fname, "float") or
        std.mem.eql(u8, fname, "byte") or
        std.mem.eql(u8, fname, "type") or
        std.mem.eql(u8, fname, "pack") or
        std.mem.eql(u8, fname, "unpack"))
    {
        // Simple builtins: validate args and return type from centralized data
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        // Analyze the operand even though the result type comes from the table.
        // Inference is what walks the operand expression, and that walk is what
        // lazily resolves and registers module-qualified custom types
        // (`std.error.Method.OutOfBounds`). Skipping it leaves a qualified enum
        // literal unresolved in HIR, where it lowers to field loads on a bogus
        // pointer instead of a constant.
        const operand_type = try inferTypeFromExpr(self, args[0]);
        if (builtin_methods.getMethodInfoByName(fname)) |info| {
            // `@pack` consumes a `byte[]`: the bridge from byte data into a
            // string is one-way and explicit. The element goes through the same
            // coercion rule as `@push`, so comptime int literals narrow to `byte`
            // while a runtime `int[]` is rejected.
            if (std.mem.eql(u8, fname, "pack")) {
                if (operand_type.base != .Array) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(args[0].base),
                        ErrorCode.TYPE_MISMATCH,
                        "@pack requires a byte[] argument, got {s}",
                        .{@tagName(operand_type.base)},
                    );
                    self.fatal_error = true;
                    return type_info;
                }
                // `byte[]` is the argument's context: a literal, empty or
                // not, is typed by it as a declaration's initializer is.
                const byte_elem = try ast.TypeInfo.createDefault(self.allocator);
                byte_elem.* = .{ .base = .Byte };
                var expected: ast.TypeInfo = .{ .base = .Array, .array_type = byte_elem };
                const start = self.reporter.diagnostics.items.len;
                try helpers.unifyTypesExpr(self, &expected, operand_type, args[0], .{ .location = getLocationFromBase(args[0].base) });
                if (self.reporter.firstErrorSince(start) != null) return type_info;
            }
            type_info.* = .{ .base = info.return_type };
            if (info.return_element_type) |elem_base| {
                const elem = try ast.TypeInfo.createDefault(self.allocator);
                elem.* = .{ .base = elem_base };
                type_info.array_type = elem;
            }
            return type_info;
        }
        // Fallback for methods not in data structure
        return type_info;
    } else if (std.mem.eql(u8, fname, "exit")) {
        // Validate argument count using centralized data
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        // Validate argument type (int | byte)
        if (args.len > 0) {
            const arg_type = try inferTypeFromExpr(self, args[0]);
            if (arg_type.base != .Int and arg_type.base != .Byte) {
                self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.TYPE_MISMATCH, "@exit: argument must be an integer", .{});
                self.fatal_error = true;
            }
        }
        // Get return type from centralized data
        if (builtin_methods.getMethodInfoByName(fname)) |info| {
            type_info.* = .{ .base = info.return_type };
            return type_info;
        }
        type_info.* = .{ .base = .Nothing };
        return type_info;
    } else if (std.mem.eql(u8, fname, "panic")) {
        // Validate argument count using centralized data
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        // Validate argument type (string)
        if (args.len > 0) {
            const arg_type = try inferTypeFromExpr(self, args[0]);
            if (arg_type.base != .String) {
                self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.TYPE_MISMATCH, "@panic: argument must be a string", .{});
                self.fatal_error = true;
            }
        }
        // Get return type from centralized data
        if (builtin_methods.getMethodInfoByName(fname)) |info| {
            type_info.* = .{ .base = info.return_type };
            return type_info;
        }
        type_info.* = .{ .base = .Nothing };
        return type_info;
    } else if (std.mem.eql(u8, fname, "clear")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        if (coll_t.base != .Array and coll_t.base != .String) {
            self.reporter.reportCompileError(
                getLocationFromBase(args[0].base),
                ErrorCode.INVALID_ARRAY_TYPE,
                "@clear requires array or string, got {s}",
                .{@tagName(coll_t.base)},
            );
            self.fatal_error = true;
            return type_info;
        }
        // @clear returns nothing
        type_info.* = .{ .base = .Nothing };
        return type_info;
    } else if (std.mem.eql(u8, fname, "find")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        if (coll_t.base != .Array and coll_t.base != .String) {
            self.reporter.reportCompileError(
                getLocationFromBase(args[0].base),
                ErrorCode.INVALID_ARRAY_TYPE,
                "@find requires array or string, got {s}",
                .{@tagName(coll_t.base)},
            );
            self.fatal_error = true;
            return type_info;
        }
        type_info.* = .{ .base = .Int };
        return type_info;
    } else if (std.mem.eql(u8, fname, "assert")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        if (args.len >= 1) {
            const cond_t = try inferTypeFromExpr(self, args[0]);
            if (cond_t.base != .Tetra) {
                self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.TYPE_MISMATCH, "@assert condition must be tetra", .{});
                self.fatal_error = true;
            }
        }
        if (args.len >= 2) {
            const msg_t = try inferTypeFromExpr(self, args[1]);
            if (msg_t.base != .String) {
                self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.TYPE_MISMATCH, "@assert message must be string", .{});
                self.fatal_error = true;
            }
        }
        type_info.* = .{ .base = .Nothing };
        return type_info;
    } else if (std.mem.eql(u8, fname, "std")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        type_info.* = .{ .base = .String };
        return type_info;
    } else if (std.mem.eql(u8, fname, "print")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        // `@print` writes bytes straight to stdout; only a string carries a
        // (ptr, len) pair it can. A value is rendered with `"{}"` in a format
        // string, which is the path that knows the operand's declared type.
        const t0 = try inferTypeFromExpr(self, args[0]);
        if (t0.base != .String) {
            self.reporter.reportCompileError(
                getLocationFromBase(args[0].base),
                ErrorCode.INVALID_ARGUMENT_TYPE,
                "@print requires string, got {s}; write \"{{expr}}\" to render a value",
                .{@tagName(t0.base)},
            );
            self.fatal_error = true;
            return type_info;
        }
        type_info.* = .{ .base = .Nothing };
        return type_info;
    }

    self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.NOT_IMPLEMENTED, "Unknown builtin '@{s}'", .{fname});
    self.fatal_error = true;
    return type_info;
}

/// The type of `expr`. Every answer is recorded in `type_cache` under the
/// expression's node id, whichever arm of the inference produced it and
/// whether or not that arm reported an error: later stages read an
/// expression's type from the cache instead of deriving it again
/// (plan/type-authority.md), so an arm that returned without recording would
/// leave them nothing to read.
/// The type of `expr` in a position that expects `expected`. An array literal
/// there is typed by it, element by element: each element is checked against
/// the expected element type, so the elements need not agree with one another
/// (`[IOError.NotFound, ParseError.Eof]` is an `Error[]`). Anything else is
/// inferred on its own, and the caller unifies it with `expected` as before.
pub fn inferTypeIn(self: *SemanticAnalyzer, expr: *ast.Expr, expected: *const ast.TypeInfo) SemanticError!*ast.TypeInfo {
    if (expr.data != .Array or expected.base != .Array) return inferTypeFromExpr(self, expr);
    const element_type = expected.array_type orelse return inferTypeFromExpr(self, expr);
    if (self.type_cache.get(expr.base.id)) |cached| return cached;
    for (expr.data.Array) |element| {
        const actual = try inferTypeIn(self, element, element_type);
        if (!helpers.adoptByteLiteral(self, element, actual, element_type)) break;
        try helpers.unifyTypesExpr(self, element_type, actual, element, .{ .location = getLocationFromBase(element.base) });
    }
    const type_info = try ast.TypeInfo.createDefault(self.allocator);
    type_info.* = .{
        .base = .Array,
        .array_type = element_type,
        .array_storage = expected.array_storage,
        .array_size = expected.array_size,
    };
    try self.type_cache.put(expr.base.id, type_info);
    return type_info;
}

pub fn inferTypeFromExpr(self: *SemanticAnalyzer, expr: *ast.Expr) SemanticError!*ast.TypeInfo {
    if (self.type_cache.get(expr.base.id)) |cached| {
        return cached;
    }
    const inferred = try inferTypeFromExprUncached(self, expr);
    try self.type_cache.put(expr.base.id, inferred);
    return inferred;
}

fn inferTypeFromExprUncached(self: *SemanticAnalyzer, expr: *ast.Expr) SemanticError!*ast.TypeInfo {
    var type_info = try ast.TypeInfo.createDefault(self.allocator);
    errdefer self.allocator.destroy(type_info);

    switch (expr.data) {
        .Map => |*map_expr| {
            type_info.* = .{ .base = .Map };
            const resolved = try resolveMapTypes(self, map_expr.entries, map_expr.key_type, map_expr.value_type, expr.base);
            map_expr.key_type = resolved.key_type;
            map_expr.value_type = resolved.value_type;
            type_info.map_key_type = resolved.key_type;
            type_info.map_value_type = resolved.value_type;
            type_info.map_has_else_value = false;
            try validateMapEntries(self, map_expr.entries, resolved.key_type, resolved.value_type);
        },
        .MapLiteral => |*map_literal| {
            type_info.* = .{ .base = .Map, .map_has_else_value = (map_literal.else_value != null) };
            const resolved = try resolveMapTypes(self, map_literal.entries, map_literal.key_type, map_literal.value_type, expr.base);
            map_literal.key_type = resolved.key_type;
            map_literal.value_type = resolved.value_type;
            type_info.map_key_type = resolved.key_type;
            type_info.map_value_type = resolved.value_type;
            try validateMapEntries(self, map_literal.entries, resolved.key_type, resolved.value_type);
            if (map_literal.else_value) |else_expr| {
                const else_type = try inferTypeFromExpr(self, else_expr);
                if (resolved.value_type) |expected_val| {
                    try helpers.unifyTypes(self, expected_val, else_type, .{ .location = getLocationFromBase(else_expr.base) });
                }
            }
        },
        .Literal => |lit| {
            type_info.inferFrom(lit);
            switch (lit) {
                .int => |i| type_info.comptime_int = i,
                .byte => |b| type_info.comptime_int = @as(i64, b),
                else => {},
            }
        },
        .Break => {
            type_info.base = .Nothing;
        },
        .Binary => |bin| {
            const left_type = try inferTypeFromExpr(self, bin.left.?);
            const right_type = try inferTypeFromExpr(self, bin.right.?);
            // `x == .Red`: a shorthand compared against an enum is its variant.
            if (bin.operator.type == .EQUALITY or bin.operator.type == .BANG_EQUAL) {
                helpers.contextualizeEnumMember(self, bin.left.?, left_type, right_type);
                helpers.contextualizeEnumMember(self, bin.right.?, right_type, left_type);
                const span = ast.SourceSpan{ .location = getLocationFromBase(expr.base) };
                _ = helpers.reportUndeclaredVariant(self, bin.left.?, left_type, right_type, span);
                _ = helpers.reportUndeclaredVariant(self, bin.right.?, right_type, left_type, span);
            }

            // The parser rewrites a compound assignment on an index or field target into
// an `IndexAssign`/`FieldAssignment` whose value is a `Binary` holding the
// *compound* lexeme (`"/="`) beside the base operator's token type (`SLASH`) —
// `precedence.zig` rewrites `.type` but copies `.lexeme` from the compound
// token. Dispatching on the raw lexeme therefore matched none of the branches
// below and fell through to the catch-all at the end of this switch, which
// reports the left operand's type. So `a[i] /= 3` inferred as `Int`, and
// storing a `float` into an `int[]` element slipped past the narrowing check
// and truncated silently. Key off the token type, which is authoritative.
const op: []const u8 = switch (bin.operator.type) {
    .PLUS => "+",
    .MINUS => "-",
    .ASTERISK => "*",
    .SLASH => "/",
    .DOUBLE_SLASH => "//",
    .MODULO => "%",
    .POWER => "**",
    .LESS => "<",
    .GREATER => ">",
    .LESS_EQUAL => "<=",
    .GREATER_EQUAL => ">=",
    .EQUALITY => "==",
    .BANG_EQUAL => "!=",
    else => bin.operator.lexeme,
};

            // Arithmetic on a union is never well typed: the operand has to be
            // narrowed first. Checked ahead of the per-operator rules, which would
            // otherwise let a union sit beside a numeric operand, infer a result, and
            // defer the failure to codegen — where the union has already vanished from
            // the diagnostic.
            const is_arithmetic = std.mem.eql(u8, op, "+") or std.mem.eql(u8, op, "-") or
                std.mem.eql(u8, op, "*") or std.mem.eql(u8, op, "/") or
                std.mem.eql(u8, op, "//") or std.mem.eql(u8, op, "%") or
                std.mem.eql(u8, op, "**");
            if (is_arithmetic and (left_type.base == .Union or right_type.base == .Union)) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.TYPE_MISMATCH,
                    "Cannot use {s} operator on union type; narrow it with 'as' or match first",
                    .{op},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // An integer literal beside a byte is a byte: `b + 1` is byte
            // arithmetic, so `b += 1` keeps `b` a byte.
            if (is_arithmetic and !std.mem.eql(u8, op, "/")) {
                if (!helpers.adoptByteLiteral(self, bin.left.?, left_type, right_type) or
                    !helpers.adoptByteLiteral(self, bin.right.?, right_type, left_type))
                {
                    type_info.base = .Nothing;
                    return type_info;
                }
            }

            if (std.mem.eql(u8, op, "/")) {
                if (left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                        "Division requires numeric operands, got {s}",
                        .{@tagName(left_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                if (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DIVISION_REQUIRES_NUMERIC_OPERANDS,
                        "Division requires numeric operands, got {s}",
                        .{@tagName(right_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                type_info.* = .{ .base = .Float };
            } else if (std.mem.eql(u8, op, "//")) {
                const left_is_int_or_byte = left_type.base == .Int or left_type.base == .Byte;
                const right_is_int_or_byte = right_type.base == .Int or right_type.base == .Byte;

                if (!left_is_int_or_byte or !right_is_int_or_byte) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                        "Integer division requires integer or byte operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "%")) {
                if ((left_type.base != .Int and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Byte))
                {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.MODULO_REQUIRES_INTEGER_OR_BYTE_OPERANDS,
                        "Modulo requires integer or byte operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
                if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "+")) {
                if (left_type.base == .String and right_type.base == .String) {
                    type_info.* = .{ .base = .String };
                } else if (left_type.base == .Array and right_type.base == .Array) {
                    // Concatenation preserves the element type, so `(a + b)[i]`
                    // still resolves through the `string`/`byte[]` barrier.
                    const elem_src = left_type.array_type orelse right_type.array_type;
                    if (elem_src) |src| {
                        const elem = try ast.TypeInfo.createDefault(self.allocator);
                        elem.* = src.*;
                        if (left_type.array_type != null and right_type.array_type != null) {
                            const start = self.reporter.diagnostics.items.len;
                            try helpers.unifyTypes(self, elem, right_type.array_type.?, .{ .location = getLocationFromBase(expr.base) });
                            if (self.reporter.firstErrorSince(start) != null) {
                                type_info.base = .Nothing;
                                return type_info;
                            }
                        }
                        type_info.* = .{ .base = .Array, .array_type = elem };
                    } else {
                        type_info.* = .{ .base = .Array };
                    }
                } else if ((left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte) and
                    (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte))
                {
                    if (left_type.base == .Float or right_type.base == .Float) {
                        type_info.* = .{ .base = .Float };
                    } else if (left_type.base == .Int or right_type.base == .Int) {
                        type_info.* = .{ .base = .Int };
                    } else {
                        type_info.* = .{ .base = .Byte };
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.TYPE_MISMATCH,
                        "Cannot use + operator between {s} and {s}. Both operands must be the same type.",
                        .{ @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (std.mem.eql(u8, op, "-") or std.mem.eql(u8, op, "*")) {
                if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                        "Arithmetic requires numeric operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (left_type.base == .Float or right_type.base == .Float) {
                    type_info.* = .{ .base = .Float };
                } else if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "**")) {
                if ((left_type.base != .Int and left_type.base != .Float and left_type.base != .Byte) or
                    (right_type.base != .Int and right_type.base != .Float and right_type.base != .Byte))
                {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.ARITHMETIC_REQUIRES_NUMERIC_OPERANDS,
                        "Power operator requires numeric operands",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (left_type.base == .Float or right_type.base == .Float) {
                    type_info.* = .{ .base = .Float };
                } else if (left_type.base == .Int or right_type.base == .Int) {
                    type_info.* = .{ .base = .Int };
                } else {
                    type_info.* = .{ .base = .Byte };
                }
            } else if (std.mem.eql(u8, op, "<") or std.mem.eql(u8, op, ">") or
                std.mem.eql(u8, op, "<=") or std.mem.eql(u8, op, ">=") or
                std.mem.eql(u8, op, "==") or std.mem.eql(u8, op, "!="))
            {
                const left_numeric = (left_type.base == .Int or left_type.base == .Float or left_type.base == .Byte);
                const right_numeric = (right_type.base == .Int or right_type.base == .Float or right_type.base == .Byte);

                if (left_numeric and right_numeric) {
                    type_info.* = .{ .base = .Tetra };
                } else if (left_type.base == right_type.base) {
                    type_info.* = .{ .base = .Tetra };
                } else if ((left_type.base == .Custom and right_type.base == .Enum) or
                    (left_type.base == .Enum and right_type.base == .Custom))
                {
                    type_info.* = .{ .base = .Tetra };
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INCOMPATIBLE_TYPES_FOR_COMPARISON,
                        "Cannot compare {s} with {s}",
                        .{ @tagName(left_type.base), @tagName(right_type.base) },
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else {
                try unifyTypes(self, left_type, right_type, .{ .location = getLocationFromBase(expr.base) });
                type_info.* = left_type.*;
            }
        },
        .Variable => |var_token| {
            if (try names.resolveVariableExpr(self, expr)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    type_info.* = storage.type_info.*;
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INTERNAL_ERROR,
                        "Internal error: Variable storage not found",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            } else if (try names.namespaceOf(self, expr)) |_| {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.MODULE_NAMESPACE_NOT_A_VALUE,
                    "Module namespace '{s}' is not a value",
                    .{var_token.lexeme},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
            } else if (self.current_initializing_var != null and std.mem.eql(u8, self.current_initializing_var.?, var_token.lexeme)) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.SELF_REFERENTIAL_INITIALIZER,
                    "Variable '{s}' cannot reference itself in its own initializer",
                    .{var_token.lexeme},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
            } else if (names.suggestName(self, var_token.lexeme)) |suggested| {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.UNDEFINED_VARIABLE,
                    "Undefined variable: '{s}'. Did you mean '{s}'?",
                    .{ var_token.lexeme, suggested },
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.VARIABLE_NOT_FOUND,
                    "Undefined variable",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
            }
        },
        .Unary => |unary| {
            const operand_type = try inferTypeFromExpr(self, unary.right.?);
            type_info.* = operand_type.*;
            // Preserve comptime-literal-ness through unary minus (value negated)
            // so negative literals stay comptime constants; clear it otherwise.
            if (unary.operator.type == .MINUS) {
                if (operand_type.comptime_int) |v| {
                    type_info.comptime_int = -v;
                }
            } else {
                type_info.comptime_int = null;
            }
        },
        .FunctionCall => |function_call| {
            if (function_call.callee.data == .FieldAccess) {
                return inferMemberCall(self, expr, function_call.callee, function_call.arguments, type_info);
            } else {
                const callee_type = try inferTypeFromExpr(self, function_call.callee);
                if (callee_type.base == .Function) {
                    if (callee_type.function_type) |func_type| {
                        const expected_arg_count: usize = func_type.params.len;
                        var provided_arg_count: usize = 0;
                        var has_placeholders: bool = false;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data != .DefaultArgPlaceholder) {
                                provided_arg_count += 1;
                            } else {
                                has_placeholders = true;
                            }
                        }

                        if (function_call.arguments.len > expected_arg_count) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_MANY_ARGUMENTS,
                                "Too many arguments: expected at most {}, got {}",
                                .{ expected_arg_count, function_call.arguments.len },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }

                        if (provided_arg_count < expected_arg_count and !has_placeholders) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.TOO_FEW_ARGUMENTS,
                                "Too few arguments: expected {}, got {} (use ~ to explicitly skip parameters)",
                                .{ expected_arg_count, provided_arg_count },
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }

                        var param_index: usize = 0;
                        for (function_call.arguments) |arg_expr_it| {
                            if (arg_expr_it.expr.data == .DefaultArgPlaceholder) {
                                if (param_index < expected_arg_count) param_index += 1;
                                continue;
                            }
                            if (param_index >= expected_arg_count) break;

                            if (func_type.param_aliases != null and func_type.param_aliases.?[param_index] and !arg_expr_it.is_alias) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(arg_expr_it.expr.base),
                                    ErrorCode.ALIAS_PARAMETER_REQUIRED,
                                    "Function parameter requires an alias argument (use ^ before the argument)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }

                            if (func_type.param_aliases != null and !func_type.param_aliases.?[param_index] and arg_expr_it.is_alias) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(arg_expr_it.expr.base),
                                    ErrorCode.ALIAS_ARGUMENT_NOT_NEEDED,
                                    "Function parameter does not require an alias argument (remove ^ before the argument)",
                                    .{},
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }

                            const arg_type = try inferTypeIn(self, arg_expr_it.expr, &func_type.params[param_index]);
                            if (arg_expr_it.is_alias) {
                                if (!try checkAliasArgument(self, arg_expr_it.expr, func_type, param_index)) {
                                    type_info.base = .Nothing;
                                    return type_info;
                                }
                            } else if (func_type.params[param_index].base != .Nothing) {
                                try helpers.unifyTypesExpr(self, &func_type.params[param_index], arg_type, arg_expr_it.expr, .{ .location = getLocationFromBase(expr.base) });
                            }
                            param_index += 1;
                        }

                        type_info.* = func_type.return_type.*;
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_FUNCTION_TYPE,
                            "Function type has no return type information",
                            .{},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_FUNCTION_CALL,
                        "Cannot call non-function type {s}",
                        .{@tagName(callee_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                }
            }
        },
        .Index => |index| {
            const array_type = try inferTypeFromExpr(self, index.array);
            const index_type = try inferTypeFromExpr(self, index.index);

            if (array_type.base == .Array) {
                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                        "Array index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.array_type) |elem_type| {
                    type_info.* = elem_type.*;
                } else {
                    if (index.array.data == .Variable) {
                        const var_name = index.array.data.Variable.lexeme;
                        if (self.current_scope) |scope| {
                            if (scope.lookupVariable(var_name)) |variable| {
                                if (scope.manager.value_storage.get(variable.storage_id)) |storage| {
                                    if (storage.type_info.base == .Array) {
                                        if (storage.type_info.array_type) |declared_elem_type| {
                                            type_info.* = declared_elem_type.*;
                                            return type_info;
                                        } else {}
                                    }
                                } else {}
                            } else {}
                        } else {}
                    } else if (index.array.data == .FieldAccess) {
                        // Handle field access like this.FileToContext[file].line_breaks
                        const field_access = index.array.data.FieldAccess;
                        const object_type = try inferTypeFromExpr(self, field_access.object);

                        if (object_type.base == .Struct or object_type.base == .Custom) {
                            if (object_type.struct_fields) |fields| {
                                for (fields) |field| {
                                    if (std.mem.eql(u8, field.name, field_access.field.lexeme)) {
                                        if (field.type_info.base == .Array) {
                                            if (field.type_info.array_type) |elem_type| {
                                                type_info.* = elem_type.*;
                                                return type_info;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // If we can't determine the element type, default to Nothing
                    type_info.base = .Nothing;
                }
            } else if (array_type.base == .Map) {
                // `m[.Red]`: the map's key type is the shorthand's context.
                if (array_type.map_key_type) |key_type| {
                    helpers.contextualizeEnumMember(self, index.index, index_type, key_type);
                    _ = helpers.reportUndeclaredVariant(self, index.index, index_type, key_type, .{ .location = getLocationFromBase(index.index.base) });
                }
                if (index_type.base != .String and index_type.base != .Int and index_type.base != .Enum and index_type.base != .Custom) {
                    if (index_type.base == .Union) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                            .{},
                        );
                    } else {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.INVALID_MAP_KEY_TYPE,
                            "Map key must be string, int, or enum, got {s}",
                            .{@tagName(index_type.base)},
                        );
                    }
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                if (array_type.map_value_type) |value_type| {
                    if (array_type.map_has_else_value) {
                        type_info.* = value_type.*;
                    } else {
                        try assignValueOrNothingUnion(self, type_info, value_type);
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE, // TODO: dedicated error code
                        "Map value type not inferred; declare explicit 'returns <Type>' or add a first entry to infer",
                        .{},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            } else if (array_type.base == .String) {
                if (index_type.base != .Int) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_STRING_INDEX_TYPE,
                        "String index must be integer, got {s}",
                        .{@tagName(index_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }

                type_info.* = .{ .base = .String };
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_INDEX_TYPE,
                    "Cannot index non-array/map/string type {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
        },
        .FieldAccess => |field| {
            // A member of a namespace: `ns.fn`, `ns.global`, `ns.Type`.
            if (try names.namespaceOf(self, field.object)) |namespace| {
                return memberOfNamespace(self, expr, namespace, field.field, type_info);
            }

            const object_type = try inferTypeFromExpr(self, field.object);
            switch (object_type.base) {
                .Union => {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                        "Cannot access field '{s}' on union type; narrow it with 'as' or match first",
                        .{field.field.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                },
                .Custom => {
                    const ref = if (object_type.custom_type) |custom| custom.resolved() else {
                        // An anonymous enum value carries no named type.
                        type_info.* = .{ .base = .Enum };
                        return type_info;
                    };
                    const custom = (try self.customType(ref)) orelse {
                        self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.TYPE_MISMATCH, "Undefined type '{s}'", .{ref.name});
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    };
                    switch (custom.kind) {
                        .Struct => {
                            const struct_field = findField(custom.struct_fields orelse &.{}, field.field.lexeme) orelse {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(expr.base),
                                    ErrorCode.FIELD_NOT_FOUND,
                                    "Field '{s}' not found in struct '{s}'",
                                    .{ field.field.lexeme, ref.name },
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            };
                            // A private field is read only through `this`.
                            if (!struct_field.is_public and field.object.data != .This) {
                                self.reporter.reportCompileError(
                                    getLocationFromBase(expr.base),
                                    ErrorCode.PRIVATE_FIELD_ACCESS,
                                    "Cannot access private field '{s}' of struct '{s}'; a private field is reached only through `this`",
                                    .{ struct_field.name, ref.name },
                                );
                                self.fatal_error = true;
                                type_info.base = .Nothing;
                                return type_info;
                            }
                            type_info.* = struct_field.field_type_info.*;
                        },
                        // `Color.Red`, `Group.Member`: the qualifier names the
                        // value's type.
                        .Enum, .Group => type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref } },
                    }
                },
                .Struct => {
                    const fields = object_type.struct_fields orelse &.{};
                    for (fields) |struct_field| {
                        if (std.mem.eql(u8, struct_field.name, field.field.lexeme)) {
                            type_info.* = struct_field.type_info.*;
                            return type_info;
                        }
                    }
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.FIELD_NOT_FOUND,
                        "Field '{s}' not found in struct",
                        .{field.field.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                },
                .Enum => type_info.* = .{ .base = .Custom, .custom_type = object_type.custom_type },
                else => {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                        "Cannot access field on non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                },
            }
        },
        .Array => |elements| {
            if (elements.len == 0) {
                type_info.* = .{ .base = .Array, .array_type = null };
            } else {
                // The element type is the elements' promoted type: a float
                // among int or byte elements makes the array float[], wherever
                // it sits (docs/math.md). Every element is then checked against
                // that type, so a literal widens and a runtime value does not.
                const element_types = try self.allocator.alloc(*ast.TypeInfo, elements.len);
                defer self.allocator.free(element_types);
                var joined = try inferTypeFromExpr(self, elements[0]);
                element_types[0] = joined;
                for (elements[1..], element_types[1..]) |element, *element_type| {
                    element_type.* = try inferTypeFromExpr(self, element);
                    if (element_type.*.base == .Float and (joined.base == .Int or joined.base == .Byte)) joined = element_type.*;
                    // An empty element takes its element type from a sibling
                    // that has one: `[[], [1]]` is an `int[][]`.
                    if (helpers.hasUninferredElement(joined) and !helpers.hasUninferredElement(element_type.*)) joined = element_type.*;
                }
                for (elements, element_types) |element, element_type| {
                    if (element_type == joined) continue;
                    try helpers.unifyTypesExpr(self, joined, element_type, element, .{ .location = getLocationFromBase(expr.base) });
                }
                const array_type = try ast.TypeInfo.createDefault(self.allocator);
                array_type.* = joined.*;
                // An element type is a type, not a literal: `[65]` is an
                // `int[]` whose elements a byte context may not narrow.
                array_type.comptime_int = null;
                type_info.* = .{ .base = .Array, .array_type = array_type };
            }
        },
        .Struct => |fields| {
            const struct_fields = try self.allocator.alloc(ast.StructFieldType, fields.len);
            for (fields, struct_fields) |field, *struct_field| {
                const field_type = try inferTypeFromExpr(self, field.value);
                struct_field.* = .{
                    .name = field.name.lexeme,
                    .type_info = field_type,
                    .is_public = true, // Struct literal fields are always public
                };
            }
            type_info.* = .{ .base = .Struct, .struct_fields = struct_fields };
        },
        .If => |if_expr| {
            var condition_type = try inferTypeFromExpr(self, if_expr.condition.?);

            // If condition is a function, automatically infer its return type as if it were called
            if (condition_type.base == .Function) {
                if (condition_type.function_type) |func_type| {
                    condition_type = func_type.return_type;
                } else {
                    condition_type = try ast.TypeInfo.createDefault(self.allocator);
                    condition_type.* = .{ .base = .Nothing, .is_mutable = false };
                }
            }

            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (if_expr.condition.?.data == .Literal) {
                const literal = if_expr.condition.?.data.Literal;
                const tag = @tagName(literal.tetra);
                self.reporter.reportWarning(
                    getLocationFromBase(if_expr.condition.?.base),
                    ErrorCode.CONDITION_ALWAYS_TRUE,
                    "condition is always {s}",
                    .{tag},
                );
            }

            const then_type = try inferTypeFromExpr(self, if_expr.then_branch.?);
            if (if_expr.else_branch) |else_branch| {
                const else_type = try inferTypeFromExpr(self, else_branch);

                // If the parser produced an implicit else that is a Nothing literal,
                // treat this as if there is no else-branch to avoid forcing unification
                // of a concrete then-type with Nothing in statement contexts.
                const else_is_implicit_nothing = (else_branch.data == .Literal and else_type.base == .Nothing);
                if (else_is_implicit_nothing) {
                    type_info.* = then_type.*;
                    return type_info;
                }

                // Special handling for peek expressions - allow different types
                // since peek is used for output/printing and the actual return value
                // should be the same as the expression being peeked
                const then_is_peek = if_expr.then_branch.?.data == .Peek;
                const else_is_peek = else_branch.data == .Peek;

                if ((then_is_peek and else_type.base == .Nothing) or (else_is_peek and then_type.base == .Nothing)) {
                    type_info.* = if (then_is_peek) then_type.* else else_type.*;
                } else if (then_is_peek and else_is_peek) {
                    type_info.* = then_type.*;
                } else {
                    if (then_type.base == .Nothing and else_type.base != .Nothing) {
                        type_info.* = else_type.*;
                    } else if (else_type.base == .Nothing and then_type.base != .Nothing) {
                        type_info.* = then_type.*;
                    } else if (!helpers.typesEqual(self, then_type, else_type)) {
                        var members = [_]*ast.TypeInfo{ then_type, else_type };
                        const u = try helpers.createUnionType(self, members[0..]);
                        type_info.* = u.*;
                    } else {
                        type_info.* = then_type.*;
                    }
                }
            } else {
                type_info.* = then_type.*;
            }
        },
        .Logical => |logical| {
            const left_type = try inferTypeFromExpr(self, logical.left);
            const right_type = try inferTypeFromExpr(self, logical.right);

            if (left_type.base != .Tetra or right_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_OPERAND_TYPE,
                    "Logical operators require tetra operands",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        .Match => |match_expr| {
            const subject_type = try inferTypeFromExpr(self, match_expr.value);
            try helpers.checkGroupMatchExhaustive(self, match_expr, subject_type, getLocationFromBase(expr.base));

            if (match_expr.cases.len > 0) {
                var matched_var_name: ?[]const u8 = null;
                if (match_expr.value.data == .Variable) {
                    matched_var_name = match_expr.value.data.Variable.lexeme;
                }
                const group = helpers.matchSubjectGroup(self, subject_type);
                const subject_enum: ?ast.TypeRef = blk: {
                    const custom = subject_type.custom_type orelse break :blk null;
                    const ct = (try self.customType(custom.resolved())) orelse break :blk null;
                    break :blk if (ct.kind == .Enum) custom.resolved() else null;
                };

                var union_types = std.array_list.Managed(*ast.TypeInfo).init(self.allocator);
                defer union_types.deinit();

                for (match_expr.cases) |*case| {
                    try self.resolveMatchPatterns(case, group, subject_enum);
                    const case_type = try self.inferMatchCaseTypeWithNarrow(case.*, subject_type, matched_var_name);
                    try union_types.append(case_type);
                }

                if (union_types.items.len > 0) {
                    // The match is one of its arms' values: their common type,
                    // or the union of them. `Common.X` and `IO.Y` are two types,
                    // though both are enums.
                    type_info.* = (try helpers.createUnionType(self, union_types.items)).*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .Grouping => |grouped_expr| {
            if (grouped_expr) |expr_in_parens| {
                type_info.* = (try inferTypeFromExpr(self, expr_in_parens)).*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .Assignment => |*assign| {
            if (assign.value) |value| {
                const prev_bve = self.block_value_expected;
                self.block_value_expected = true;
                defer self.block_value_expected = prev_bve;
                // The target is resolved first: its storage's type is the
                // context the value is typed in.
                const assigned = try names.resolveAssignmentTarget(self, expr, &assign.name);
                const target_storage = if (assigned) |variable| self.memory.scope_manager.value_storage.get(variable.storage_id) else null;
                const value_type = if (target_storage) |storage|
                    try inferTypeIn(self, value, storage.type_info)
                else
                    try inferTypeFromExpr(self, value);
                if (assigned) |variable| {
                    // A store into a union `^` parameter keeps its member only
                    // through a narrowing of it to one member: un-narrowed, or
                    // narrowed to a smaller union, it may store another.
                    if (self.getStoreTarget(expr.base.id)) |target| {
                        if (self.union_alias_params.get(target.storage)) |param| {
                            if (!variable.is_view or target.read.base == .Union) try self.member_mutators.put(param, {});
                        }
                    }
                    if (target_storage) |storage| {
                        if (storage.constant) {
                            self.reporter.reportCompileError(
                                getLocationFromBase(expr.base),
                                ErrorCode.INVALID_ASSIGNMENT_TARGET,
                                "Cannot assign to immutable variable '{s}'",
                                .{assign.name.lexeme},
                            );
                            self.fatal_error = true;
                            type_info.base = .Nothing;
                            return type_info;
                        }
                        try helpers.unifyTypesExpr(self, storage.type_info, value_type, value, .{ .location = getLocationFromBase(expr.base) });
                    }
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.VARIABLE_NOT_FOUND,
                        "Undefined variable '{s}'",
                        .{assign.name.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            }
            type_info.* = .{ .base = .Nothing };
        },
        .ReturnExpr => |return_expr| {
            if (return_expr.value) |value| {
                const prev_bve = self.block_value_expected;
                self.block_value_expected = true;
                defer self.block_value_expected = prev_bve;
                type_info.* = (try self.checkReturnValue(value, getLocationFromBase(expr.base))).*;
            } else {
                self.checkBareReturn(getLocationFromBase(expr.base));
                type_info.* = .{ .base = .Nothing };
            }
        },
        .Unreachable => {
            type_info.* = .{ .base = .Nothing };
        },
        .InternalCall => |method_call| {
            const method_name = method_call.method.lexeme;
            // `@std` takes no arguments; its parser-supplied receiver is a
            // placeholder, not an argument.
            if (method_call.method.type == .STD) {
                return try inferBuiltinCall(self, expr, method_name, null, &.{});
            }
            return try inferBuiltinCall(self, expr, method_name, method_call.receiver, method_call.arguments);
        },
        .EnumMember => {
            // Untyped until its context names the enum; checked when the
            // record's analysis ends (`reportUntypedVariants`).
            try self.bare_variants.append(self.allocator, expr);
            type_info.* = .{ .base = .Enum };
        },
        .DefaultArgPlaceholder => {
            type_info.* = .{ .base = .Nothing };
        },
        .Input => {
            type_info.* = .{ .base = .String };
        },
        .Peek => |_peek| {
            const expr_type = try inferTypeFromExpr(self, _peek.expr);
            type_info.* = expr_type.*;
        },
        .PeekStruct => |_peek_struct| {
            const expr_type = try inferTypeFromExpr(self, _peek_struct.expr);
            type_info.* = expr_type.*;
        },
        .InterpolatedString => |template| {
            for (template.parts) |part| {
                switch (part) {
                    .Expression => |part_expr| {
                        _ = try inferTypeFromExpr(self, part_expr);
                    },
                    .String => {},
                }
            }
            type_info.* = .{ .base = .String };
        },
        .Print => |print_expr| {
            _ = try inferTypeFromExpr(self, print_expr.expr);
            type_info.* = .{ .base = .Nothing };
        },
        .IndexAssign => |index_assign| {
            const array_type = try inferTypeFromExpr(self, index_assign.array);
            const index_type = try inferTypeFromExpr(self, index_assign.index);
            const value_type = if (array_type.base == .Array and array_type.array_type != null)
                try inferTypeIn(self, index_assign.value, array_type.array_type.?)
            else if (array_type.base == .Map and array_type.map_value_type != null)
                try inferTypeIn(self, index_assign.value, array_type.map_value_type.?)
            else
                try inferTypeFromExpr(self, index_assign.value);

            if (array_type.base != .Array and array_type.base != .Map) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.CANNOT_INDEX_TYPE,
                    "Cannot assign to index of non-array/non-map type {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (array_type.base == .Array and index_type.base != .Int) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_INDEX_TYPE,
                    "Array index must be integer, got {s}",
                    .{@tagName(index_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            } else if (array_type.base == .Map and index_type.base != .Int and index_type.base != .String and index_type.base != .Enum and index_type.base != .Custom) {
                if (index_type.base == .Union) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE,
                        "Map keys cannot be union types. Keys must be concrete types like int, string, or enum",
                        .{},
                    );
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_MAP_KEY_TYPE,
                        "Map key must be integer, string, or enum, got {s}",
                        .{@tagName(index_type.base)},
                    );
                }
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (array_type.array_type) |elem_type| {
                try helpers.unifyElement(self, elem_type, value_type, index_assign.value, .{ .location = getLocationFromBase(expr.base) });
            }
            // A map entry is an element store too: its key and its value fill
            // the map's key and value slots.
            if (array_type.base == .Map) {
                if (array_type.map_key_type) |key_type| {
                    try helpers.unifyElement(self, key_type, index_type, index_assign.index, .{ .location = getLocationFromBase(index_assign.index.base) });
                }
                if (array_type.map_value_type) |map_value_type| {
                    try helpers.unifyElement(self, map_value_type, value_type, index_assign.value, .{ .location = getLocationFromBase(index_assign.value.base) });
                }
            }

            type_info.* = .{ .base = .Nothing };
        },
        .FieldAssignment => |field_assign| {
            const object_type = try inferTypeFromExpr(self, field_assign.object);
            const value_type = try inferTypeFromExpr(self, field_assign.value);

            const fields: []const ast.StructFieldType = switch (object_type.base) {
                .Struct => object_type.struct_fields orelse &.{},
                .Custom => blk: {
                    const ref = (object_type.custom_type orelse break :blk &.{}).resolved();
                    const custom = (try self.customType(ref)) orelse break :blk &.{};
                    if (custom.kind != .Struct) break :blk &.{};
                    break :blk try structFieldTypes(self, custom.struct_fields orelse &.{});
                },
                else => {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.CANNOT_ACCESS_FIELD_ON_TYPE,
                        "Cannot assign to field of non-struct type {s}",
                        .{@tagName(object_type.base)},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                },
            };

            for (fields) |struct_field| {
                if (std.mem.eql(u8, struct_field.name, field_assign.field.lexeme)) {
                    // A private field is written only through `this`.
                    if (!struct_field.is_public and field_assign.object.data != .This) {
                        self.reporter.reportCompileError(
                            getLocationFromBase(expr.base),
                            ErrorCode.PRIVATE_FIELD_ACCESS,
                            "Cannot assign private field '{s}'; a private field is reached only through `this`",
                            .{struct_field.name},
                        );
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    try helpers.unifyTypesExpr(self, struct_field.type_info, value_type, field_assign.value, .{ .location = getLocationFromBase(expr.base) });
                    break;
                }
            } else {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.FIELD_NOT_FOUND,
                    "Field '{s}' not found in struct",
                    .{field_assign.field.lexeme},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Nothing };
        },
        .Exists => |*exists| {
            const array_type = try inferTypeFromExpr(self, exists.array);

            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            // The binding's type outlives this call: every read of the bound
            // name records it as a store target, so it cannot live on this
            // frame.
            const bound_var_type = try ast.TypeInfo.createDefault(self.allocator);
            bound_var_type.* = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int };

            self.checkFreshName(quantifier_scope, exists.variable);
            const bound = quantifier_scope.createValueBinding(
                exists.variable.lexeme,
                eval.convertTypeToTokenType(bound_var_type.base),
                bound_var_type,
                true,
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
                        "Duplicate bound variable name '{s}' in exists quantifier",
                        .{exists.variable.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    quantifier_scope.deinit();
                    return type_info;
                } else {
                    quantifier_scope.deinit();
                    return err;
                }
            };

            exists.storage = bound.storage_id;

            const prev_scope = self.current_scope;
            self.current_scope = quantifier_scope;

            const condition_type = try inferTypeFromExpr(self, exists.condition);

            self.current_scope = prev_scope;

            quantifier_scope.deinit();

            if (array_type.base != .Array) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_TYPE,
                    "Exists requires array type, got {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Exists condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        .ForAll => |*for_all| {
            const array_type = try inferTypeFromExpr(self, for_all.array);

            const quantifier_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);

            // The binding's type outlives this call: every read of the bound
            // name records it as a store target, so it cannot live on this
            // frame.
            const bound_var_type = try ast.TypeInfo.createDefault(self.allocator);
            bound_var_type.* = if (array_type.array_type) |elem_type|
                elem_type.*
            else
                ast.TypeInfo{ .base = .Int };

            self.checkFreshName(quantifier_scope, for_all.variable);
            const bound = quantifier_scope.createValueBinding(
                for_all.variable.lexeme,
                eval.convertTypeToTokenType(bound_var_type.base),
                bound_var_type,
                true,
            ) catch |err| {
                if (err == error.DuplicateVariableName) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.DUPLICATE_BOUND_VARIABLE_NAME,
                        "Duplicate bound variable name '{s}' in forall quantifier",
                        .{for_all.variable.lexeme},
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    quantifier_scope.deinit();
                    return type_info;
                } else {
                    quantifier_scope.deinit();
                    return err;
                }
            };

            for_all.storage = bound.storage_id;

            const prev_scope = self.current_scope;
            self.current_scope = quantifier_scope;

            const condition_type = try inferTypeFromExpr(self, for_all.condition);

            self.current_scope = prev_scope;

            quantifier_scope.deinit();

            if (array_type.base != .Array) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_ARRAY_TYPE,
                    "ForAll requires array type, got {s}",
                    .{@tagName(array_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "ForAll condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Tetra };
        },
        .Assert => |assert| {
            const condition_type = try inferTypeFromExpr(self, assert.condition);
            if (condition_type.base != .Tetra) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.INVALID_CONDITION_TYPE,
                    "Assert condition must be tetra, got {s}",
                    .{@tagName(condition_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            type_info.* = .{ .base = .Nothing };
        },
        .StructDecl => {
            type_info.* = .{ .base = .Struct };
        },
        .StructLiteral => |struct_lit| {
            const ref = (try names.resolveTypeName(self, struct_lit.name.lexeme, self.current_module)) orelse {
                self.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(struct_lit.name).location,
                    ErrorCode.UNKNOWN_TYPE,
                    "Unknown type '{s}'",
                    .{struct_lit.name.lexeme},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            };
            try names.annotate(self, expr, .{ .type = ref });
            const custom = (try self.customType(ref)) orelse return error.UndefinedType;
            if (custom.kind != .Struct) {
                self.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(struct_lit.name).location,
                    ErrorCode.TYPE_MISMATCH,
                    "'{s}' is not a struct",
                    .{ref.name},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
            const decl_fields = try structFieldTypes(self, custom.struct_fields orelse &.{});
            type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = ref }, .struct_fields = decl_fields, .is_mutable = false };

            if (decl_fields.len != struct_lit.fields.len) {
                self.reporter.reportCompileError(
                    ast.SourceSpan.fromToken(struct_lit.name).location,
                    ErrorCode.STRUCT_FIELD_COUNT_MISMATCH,
                    "struct '{s}' expects {d} field{s}, but this literal provides {d}",
                    .{ ref.name, decl_fields.len, if (decl_fields.len == 1) "" else "s", struct_lit.fields.len },
                );
                self.fatal_error = true;
            }

            for (struct_lit.fields) |lit_field| {
                for (decl_fields) |decl_field| {
                    if (!std.mem.eql(u8, decl_field.name, lit_field.name.lexeme)) continue;
                    // A private field is set only by the struct itself.
                    const inside = if (self.enclosing) |enclosing| enclosing.ref.eql(ref) else false;
                    if (!decl_field.is_public and !inside) {
                        self.reporter.reportCompileError(
                            ast.SourceSpan.fromToken(lit_field.name).location,
                            ErrorCode.PRIVATE_FIELD_ACCESS,
                            "Cannot set private field '{s}' of struct '{s}' outside the struct",
                            .{ decl_field.name, ref.name },
                        );
                        self.fatal_error = true;
                    }
                    const lit_type = try inferTypeFromExpr(self, lit_field.value);
                    try helpers.unifyTypesExpr(self, decl_field.type_info, lit_type, lit_field.value, ast.SourceSpan.fromToken(lit_field.name));
                    break;
                } else {
                    const declared_list = try declaredFieldList(self, decl_fields);
                    defer self.allocator.free(declared_list);
                    self.reporter.reportCompileError(
                        ast.SourceSpan.fromToken(lit_field.name).location,
                        ErrorCode.STRUCT_FIELD_NAME_MISMATCH,
                        "struct '{s}' has no field '{s}'; declared fields: {s}",
                        .{ ref.name, lit_field.name.lexeme, declared_list },
                    );
                    self.fatal_error = true;
                }
            }
        },
        .EnumDecl => {
            type_info.* = .{ .base = .Enum };
        },
        .GroupDecl => {
            type_info.* = .{ .base = .Enum, .custom_type = .{ .ref = .{ .module = self.current_module, .name = expr.data.GroupDecl.name.lexeme } } };
        },
        .ArrayType => {
            type_info.* = .{ .base = .Array };
        },
        .Block => |block| {
            // Ensure statements inside a block expression are validated so that
            // statement-level transformations (e.g., @push lowering) occur within blocks
            const prev_scope = self.current_scope;
            const block_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            self.current_scope = block_scope;
            defer {
                self.current_scope = prev_scope;
                block_scope.deinit();
            }

            try self.validateStatements(block.statements);

            var value_type_ptr: ?*ast.TypeInfo = null;
            if (block.value) |value_expr| {
                value_type_ptr = try inferTypeFromExpr(self, value_expr);
            }

            if (self.in_loop_scope) {
                if (value_type_ptr) |vt| {
                    type_info.* = vt.*;
                } else {
                    type_info.* = .{ .base = .Nothing };
                }
            } else if (value_type_ptr) |vt| {
                type_info.* = vt.*;
            } else {
                type_info.* = .{ .base = .Nothing };
            }
        },
        .TypeExpr => |type_expr| {
            type_info.* = (try self.typeExprToTypeInfo(type_expr)).*;
        },
        .Cast => |cast| {
            if (cast.else_branch == null) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.EXPECTED_CLOSING_BRACE,
                    "'as' requires an 'else' block",
                    .{},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
            const target_type_info = try self.typeExprToTypeInfo(cast.target_type);
            type_info.* = target_type_info.*;
            expr.data.Cast.target = target_type_info;

            const value_type = try inferTypeFromExpr(self, cast.value);
            const group_cast = classifyGroupCast(self, value_type, target_type_info);
            if (value_type.base != .Union and group_cast != .valid) {
                if (group_cast == .not_a_member) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "'{s}' is not a member of group '{s}'",
                        try helpers.typeLabels(self, target_type_info, value_type),
                    );
                } else {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.INVALID_OPERAND_TYPE,
                        "Type casting 'as' can only be used with union types, got {s}",
                        .{@tagName(value_type.base)},
                    );
                }
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            // When this cast is the initializer of a declaration, the declared
            // name (carried on the cast node) is narrowed to the target type in
            // the then branch and to the remainder in the else branch.
            const cast_decl_name = cast.decl_name;

            // TODO: warn on discarded non-Nothing values in if/as blocks without lift
            // TODO: warn on unused variable when both branches diverge via return
            var then_type: ?*ast.TypeInfo = null;
            if (cast.then_branch) |then_expr| {
                const then_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                defer then_scope.deinit();

                const prev_scope = self.current_scope;
                self.current_scope = then_scope;
                defer self.current_scope = prev_scope;

                try bindNarrowedCastType(self, then_scope, cast.value, target_type_info);
                if (cast_decl_name) |name| expr.data.Cast.decl_then = try bindCastDeclName(then_scope, name, target_type_info);

                if (then_expr.data == .Block and then_expr.data.Block.value != null) {
                    self.reporter.reportWarning(
                        getLocationFromBase(expr.base),
                        ErrorCode.CONDITION_ALWAYS_TRUE,
                        "lift in 'as' then block has no effect; value comes from the narrowed original",
                        .{},
                    );
                }

                const inferred_then = try inferTypeFromExpr(self, then_expr);
                if (then_expr.data != .Block) {
                    then_type = inferred_then;
                }

                if (cast_decl_name) |name| then_scope.propagateUsedToParent(name);
                if (castValueBindingName(cast.value)) |name| then_scope.propagateUsedToParent(name);
            }

            var else_diverges = false;
            var else_type: ?*ast.TypeInfo = null;
            if (cast.else_branch) |else_expr| {
                const else_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
                defer else_scope.deinit();

                const prev_else_scope = self.current_scope;
                self.current_scope = else_scope;
                defer self.current_scope = prev_else_scope;

                const remainder_type = try helpers.subtractTypeFromUnion(self, value_type, target_type_info);
                try bindNarrowedCastType(self, else_scope, cast.value, remainder_type);
                if (cast_decl_name) |name| expr.data.Cast.decl_else = try bindCastDeclName(else_scope, name, remainder_type);

                else_type = try inferTypeFromExpr(self, else_expr);
                if (expressionDiverges(else_expr)) {
                    else_diverges = true;
                    const nothing_type = try ast.TypeInfo.createDefault(self.allocator);
                    nothing_type.* = .{ .base = .Nothing };
                    else_type = nothing_type;
                }

                if (cast_decl_name) |name| else_scope.propagateUsedToParent(name);
                if (castValueBindingName(cast.value)) |name| else_scope.propagateUsedToParent(name);
            }

            if (else_type) |et| {
                const label = struct {
                    fn run(t: *ast.TypeInfo) []const u8 {
                        if (t.custom_type) |ct| return ct.displayName();
                        return switch (t.base) {
                            .Int => "int",
                            .Byte => "byte",
                            .Float => "float",
                            .String => "string",
                            .Tetra => "tetra",
                            .Nothing => "nothing",
                            else => @tagName(t.base),
                        };
                    }
                }.run;

                const fallback_matches_target = !self.block_value_expected or (et.base == .Nothing and else_diverges) or helpers.typesEqual(self, target_type_info, et);
                if (!fallback_matches_target) {
                    self.reporter.reportCompileError(
                        getLocationFromBase(expr.base),
                        ErrorCode.TYPE_MISMATCH,
                        "Fallback for 'as' must produce type {s}, got {s}",
                        .{ label(target_type_info), label(et) },
                    );
                    self.fatal_error = true;
                    type_info.base = .Nothing;
                    return type_info;
                }
            }

            type_info.* = target_type_info.*;

            if (then_type) |tt| {
                if (else_type) |et| {
                    if (tt.base == et.base) {
                        type_info.* = tt.*;
                    } else {
                        var types = [_]*ast.TypeInfo{ tt, et };
                        const union_type = try helpers.createUnionType(self, &types);
                        type_info.* = union_type.*;
                    }
                } else {
                    type_info.* = tt.*;
                }
            }
        },
        .Loop => |loop| {
            const outer_loop_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            const prev_scope = self.current_scope;
            self.current_scope = outer_loop_scope;
            defer {
                self.current_scope = prev_scope;
                outer_loop_scope.deinit();
            }

            const prev_in_loop_scope = self.in_loop_scope;
            self.in_loop_scope = true;
            defer self.in_loop_scope = prev_in_loop_scope;

            if (loop.var_decl) |vd| {
                var single: [1]ast.Stmt = .{vd.*};
                try self.validateStatements(single[0..]);
            }

            if (loop.condition) |cond| {
                _ = try inferTypeFromExpr(self, cond);
                if (cond.data == .Literal) {
                    const literal = cond.data.Literal;
                    const tag = @tagName(literal.tetra);
                    self.reporter.reportWarning(
                        getLocationFromBase(cond.base),
                        ErrorCode.CONDITION_ALWAYS_TRUE,
                        "condition is always {s}",
                        .{tag},
                    );
                }
            }

            // Create iteration scope for loop body
            const iteration_scope = try self.memory.scope_manager.createScope(self.current_scope, self.memory);
            defer iteration_scope.deinit();

            // Process loop body in iteration scope
            {
                const saved_scope = self.current_scope;
                self.current_scope = iteration_scope;
                defer self.current_scope = saved_scope;

                _ = try inferTypeFromExpr(self, loop.body);
            }

            if (loop.step) |stp| {
                _ = try inferTypeFromExpr(self, stp);
            }

            type_info.* = .{ .base = .Nothing };
        },
        .This => {
            // `this` is the receiver of the method whose body is being checked.
            const enclosing = self.enclosing orelse {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_THIS, "`this` is only available inside a method", .{});
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            };
            if (!enclosing.has_this) {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_THIS, "`this` is only available inside a method; '{s}' functions have no receiver", .{enclosing.ref.name});
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }
            type_info.* = .{ .base = .Custom, .custom_type = .{ .ref = enclosing.ref }, .is_mutable = false };
        },
        .Range => |range| {
            const start_type = try inferTypeFromExpr(self, range.start);
            const end_type = try inferTypeFromExpr(self, range.end);

            if (start_type.base != .Int and start_type.base != .Float and start_type.base != .Byte) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range start value must be numeric, got {s}",
                    .{@tagName(start_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            if (end_type.base != .Int and end_type.base != .Float and end_type.base != .Byte) {
                self.reporter.reportCompileError(
                    getLocationFromBase(expr.base),
                    ErrorCode.RANGE_REQUIRES_NUMERIC_OPERANDS,
                    "Range end value must be numeric, got {s}",
                    .{@tagName(end_type.base)},
                );
                self.fatal_error = true;
                type_info.base = .Nothing;
                return type_info;
            }

            const element_type = try ast.TypeInfo.createDefault(self.allocator);
            element_type.* = .{ .base = .Int };
            type_info.* = .{ .base = .Array, .array_type = element_type };
        },
    }

    return type_info;
}

fn validateFunctionCallArguments(self: *SemanticAnalyzer, expr: *ast.Expr, arguments: []const ast.CallArgument, func_type: *const ast.FunctionType) SemanticError!bool {
    const expected_arg_count: usize = func_type.params.len;
    var provided_arg_count: usize = 0;
    var has_placeholders: bool = false;
    for (arguments) |arg_expr_it| {
        if (arg_expr_it.expr.data != .DefaultArgPlaceholder) {
            provided_arg_count += 1;
        } else {
            has_placeholders = true;
        }
    }

    if (arguments.len > expected_arg_count) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.TOO_MANY_ARGUMENTS,
            "Too many arguments: expected at most {}, got {}",
            .{ expected_arg_count, arguments.len },
        );
        self.fatal_error = true;
        return false;
    }

    if (provided_arg_count < expected_arg_count and !has_placeholders) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.TOO_FEW_ARGUMENTS,
            "Too few arguments: expected {}, got {} (use ~ to explicitly skip parameters)",
            .{ expected_arg_count, provided_arg_count },
        );
        self.fatal_error = true;
        return false;
    }

    var param_index: usize = 0;
    for (arguments) |arg_expr_it| {
        if (arg_expr_it.expr.data == .DefaultArgPlaceholder) {
            if (param_index < expected_arg_count) param_index += 1;
            continue;
        }
        if (param_index >= expected_arg_count) break;

        if (func_type.param_aliases != null and func_type.param_aliases.?[param_index] and !arg_expr_it.is_alias) {
            self.reporter.reportCompileError(
                getLocationFromBase(arg_expr_it.expr.base),
                ErrorCode.ALIAS_PARAMETER_REQUIRED,
                "Function parameter requires an alias argument (use ^ before the argument)",
                .{},
            );
            self.fatal_error = true;
            return false;
        }

        if (func_type.param_aliases != null and !func_type.param_aliases.?[param_index] and arg_expr_it.is_alias) {
            self.reporter.reportCompileError(
                getLocationFromBase(arg_expr_it.expr.base),
                ErrorCode.ALIAS_ARGUMENT_NOT_NEEDED,
                "Function parameter does not require an alias argument (remove ^ before the argument)",
                .{},
            );
            self.fatal_error = true;
            return false;
        }

        const arg_type = try inferTypeIn(self, arg_expr_it.expr, &func_type.params[param_index]);
        if (arg_expr_it.is_alias) {
            if (!try checkAliasArgument(self, arg_expr_it.expr, func_type, param_index)) return false;
        } else if (func_type.params[param_index].base != .Nothing) {
            var expected_type = func_type.params[param_index];
            try helpers.unifyTypesExpr(self, &expected_type, arg_type, arg_expr_it.expr, .{ .location = getLocationFromBase(expr.base) });
        }
        param_index += 1;
    }

    return true;
}

/// A `^` argument lends its storage, so the storage's type — the binding
/// beneath any narrowing view — must be exactly the parameter's: a callee may
/// store any value of its parameter type, which a narrower or wider caller slot
/// cannot hold. The one exception is a union parameter that preserves its
/// member, which may be lent storage of any one member type; that is settled
/// once every body is analyzed (`settleMemberLoans`). An unannotated parameter
/// accepts any storage.
fn checkAliasArgument(self: *SemanticAnalyzer, arg: *ast.Expr, signature: *const ast.FunctionType, index: usize) !bool {
    const param = &signature.params[index];
    if (param.base == .Nothing) return true;
    // The parser admits only a name after `^`; an unresolved one is already
    // `Undefined variable`.
    const target = self.getStoreTarget(arg.base.id) orelse return true;
    const lent_to: ParamRef = .{ .signature = signature, .index = @intCast(index) };
    if (helpers.typesEqual(self, target.slot, param)) {
        // Lending a union `^` parameter on: it preserves its member only if
        // the parameter it is lent to does.
        if (self.union_alias_params.get(target.storage)) |from| {
            try self.member_relays.append(self.allocator, .{ .from = from, .to = lent_to });
        }
        return true;
    }
    if (param.base == .Union and target.slot.base != .Union) {
        for (param.union_type.?.types) |member| {
            if (!helpers.typesEqual(self, target.slot, member)) continue;
            try self.member_loans.append(self.allocator, .{ .param = lent_to, .location = getLocationFromBase(arg.base) });
            return true;
        }
    }
    self.reporter.reportCompileError(
        getLocationFromBase(arg.base),
        ErrorCode.INVALID_ALIAS_ARGUMENT,
        "An alias argument lends its storage, so its type must be exactly the parameter's: {s} is not {s}",
        try helpers.typeLabels(self, target.slot, param),
    );
    self.fatal_error = true;
    return false;
}

const MapTypeResolution = struct {
    key_type: ?*ast.TypeInfo,
    value_type: ?*ast.TypeInfo,
};

fn resolveMapTypes(
    self: *SemanticAnalyzer,
    entries: []*ast.MapEntry,
    explicit_key: ?*ast.TypeInfo,
    explicit_value: ?*ast.TypeInfo,
    expr_base: ast.Base,
) SemanticError!MapTypeResolution {
    var key_type = explicit_key;
    var value_type = explicit_value;

    if (entries.len > 0) {
        if (key_type == null) {
            key_type = try inferTypeFromExpr(self, entries[0].key);
        }
        if (value_type == null) {
            value_type = try inferTypeFromExpr(self, entries[0].value);
        }
    }

    if (key_type == null) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr_base),
            ErrorCode.TYPE_MISMATCH,
            "Unable to infer map key type. Add an entry or declare an explicit ':: <Type>'",
            .{},
        );
        self.fatal_error = true;
    }

    if (value_type == null) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr_base),
            ErrorCode.TYPE_MISMATCH,
            "Unable to infer map value type. Add an entry or declare an explicit 'returns <Type>'",
            .{},
        );
        self.fatal_error = true;
    }

    return .{
        .key_type = key_type,
        .value_type = value_type,
    };
}

fn validateMapEntries(
    self: *SemanticAnalyzer,
    entries: []*ast.MapEntry,
    key_type: ?*ast.TypeInfo,
    value_type: ?*ast.TypeInfo,
) SemanticError!void {
    if (key_type) |expected_key| {
        for (entries) |entry| {
            const entry_key_type = try inferTypeFromExpr(self, entry.key);
            try helpers.unifyTypesExpr(self, expected_key, entry_key_type, entry.key, .{ .location = getLocationFromBase(entry.key.base) });
        }
    }

    if (value_type) |expected_value| {
        for (entries) |entry| {
            const entry_value_type = try inferTypeFromExpr(self, entry.value);
            try helpers.unifyTypesExpr(self, expected_value, entry_value_type, entry.value, .{ .location = getLocationFromBase(entry.value.base) });
        }
    }
}

fn createNothingType(self: *SemanticAnalyzer) SemanticError!*ast.TypeInfo {
    const nothing_type = try ast.TypeInfo.createDefault(self.allocator);
    nothing_type.* = .{ .base = .Nothing, .is_mutable = false };
    return nothing_type;
}

fn assignValueOrNothingUnion(self: *SemanticAnalyzer, dest: *ast.TypeInfo, value_type: *ast.TypeInfo) SemanticError!void {
    var members_len: usize = 0;
    var has_nothing = false;
    if (value_type.base == .Union and value_type.union_type != null) {
        const union_members = value_type.union_type.?.types;
        members_len = union_members.len;
        for (union_members) |member| {
            if (member.base == .Nothing) {
                has_nothing = true;
                break;
            }
        }
    } else {
        members_len = 1;
    }

    var total_members: usize = members_len;
    if (!has_nothing) total_members += 1;
    const member_slice = try self.allocator.alloc(*ast.TypeInfo, total_members);

    var idx: usize = 0;
    if (value_type.base == .Union and value_type.union_type != null) {
        for (value_type.union_type.?.types) |member| {
            member_slice[idx] = try self.deepCopyTypeInfoPtr(member);
            idx += 1;
        }
    } else {
        member_slice[idx] = try self.deepCopyTypeInfoPtr(value_type);
        idx += 1;
    }

    if (!has_nothing) {
        member_slice[idx] = try createNothingType(self);
    }

    const union_info = try self.allocator.create(ast.UnionType);
    union_info.* = .{
        .types = member_slice,
        .current_type_index = null,
    };

    dest.* = .{
        .base = .Union,
        .union_type = union_info,
        .is_mutable = value_type.is_mutable,
    };
}

/// Whether an expression unconditionally transfers control away and therefore
/// never produces a value (an explicit return, `@panic`, `@exit`, or
/// unreachable). Used to accept diverging `as`/`if` fallback branches.
fn expressionDiverges(expr: *ast.Expr) bool {
    return switch (expr.data) {
        .ReturnExpr, .Unreachable => true,
        .InternalCall => |ic| ic.method.type == .PANIC or ic.method.type == .EXIT,
        .Block => |block| blockDiverges(block.statements),
        else => false,
    };
}

fn blockDiverges(statements: []ast.Stmt) bool {
    if (statements.len == 0) return false;
    return statementDiverges(statements[statements.len - 1]);
}

fn statementDiverges(stmt: ast.Stmt) bool {
    return switch (stmt.data) {
        .Return, .Break, .Continue => true,
        .Expression => |maybe_expr| if (maybe_expr) |e| expressionDiverges(e) else false,
        else => false,
    };
}

/// How `as T` relates to a group-typed subject. `valid` means `T` names one of
/// the group's members and the cast narrows; `not_a_member` means the subject is
/// a group but `T` is outside it, which is the case a plain "not a union" message
/// would misreport.
const GroupCast = enum { not_a_group, valid, not_a_member };

fn classifyGroupCast(self: *SemanticAnalyzer, value_type: *ast.TypeInfo, target_type: *ast.TypeInfo) GroupCast {
    const group = helpers.matchSubjectGroup(self, value_type) orelse return .not_a_group;
    if (target_type.base != .Custom) return .not_a_member;
    const target = (target_type.custom_type orelse return .not_a_member).resolved();
    const group_id = self.group_table.idOf(group) orelse return .not_a_member;
    for (self.group_table.members(group_id) orelse &.{}) |member| {
        if (member.ref.eql(target)) return .valid;
    }
    return .not_a_member;
}

/// Returns the variable name that `bindNarrowedCastType` would shadow for the
/// given cast value, so usage of that narrowed binding can be propagated to the
/// original variable when the narrowing scope is torn down.
fn castValueBindingName(cast_value: *ast.Expr) ?[]const u8 {
    if (cast_value.data == .Variable) return cast_value.data.Variable.lexeme;
    if (cast_value.data == .FieldAccess) {
        const field_access = cast_value.data.FieldAccess;
        if (field_access.object.data == .Variable) return field_access.object.data.Variable.lexeme;
    }
    return null;
}

fn bindNarrowedName(scope: *Scope, name: []const u8, narrowed_type: *ast.TypeInfo) !void {
    const token_type = eval.convertTypeToTokenType(narrowed_type.base);
    const view = scope.createValueBinding(
        name,
        token_type,
        narrowed_type,
        false,
    ) catch return;
    view.is_view = true;
}

/// Bind the name a cast declares inside one of its branches. The declaration
/// has no value until the cast resolves, so inside a branch the name is its own
/// binding, holding the subject narrowed to that branch's type.
fn bindCastDeclName(scope: *Scope, name: []const u8, narrowed_type: *ast.TypeInfo) !?ast.CastBinding {
    const binding = scope.createValueBinding(
        name,
        eval.convertTypeToTokenType(narrowed_type.base),
        narrowed_type,
        false,
    ) catch |err| switch (err) {
        error.DuplicateVariableName => return null,
        else => |e| return e,
    };
    return .{ .storage = binding.storage_id, .type_info = narrowed_type };
}

fn bindNarrowedCastType(self: *SemanticAnalyzer, scope: *Scope, cast_value: *ast.Expr, narrowed_type: *ast.TypeInfo) !void {
    if (cast_value.data == .Variable) {
        try bindNarrowedName(scope, cast_value.data.Variable.lexeme, narrowed_type);
    } else if (cast_value.data == .FieldAccess) {
        const field_access = cast_value.data.FieldAccess;
        if (field_access.object.data == .Variable) {
            const obj_name = field_access.object.data.Variable.lexeme;
            const fld_name = field_access.field.lexeme;
            if (try names.lookupVariable(self, obj_name)) |variable| {
                if (self.memory.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                    const original_type = storage.type_info.*;
                    var struct_fields: ?[]const ast.StructFieldType = null;
                    if (original_type.base == .Struct and original_type.struct_fields != null) {
                        struct_fields = original_type.struct_fields.?;
                    } else if (original_type.base == .Custom and original_type.custom_type != null) {
                        if (try self.customType(original_type.custom_type.?.resolved())) |custom_type| {
                            if (custom_type.kind == .Struct) struct_fields = try structFieldTypes(self, custom_type.struct_fields orelse &.{});
                        }
                    }
                    if (struct_fields) |fields_arr| {
                        const dup_fields = try self.allocator.alloc(ast.StructFieldType, fields_arr.len);
                        for (fields_arr, 0..) |sf, i| {
                            if (std.mem.eql(u8, sf.name, fld_name)) {
                                dup_fields[i] = .{ .name = sf.name, .type_info = narrowed_type };
                            } else {
                                dup_fields[i] = sf;
                            }
                        }
                        const narrowed_struct = try ast.TypeInfo.createDefault(self.allocator);
                        narrowed_struct.* = .{ .base = .Struct, .struct_fields = dup_fields };
                        const view = scope.createValueBinding(
                            obj_name,
                            .STRUCT,
                            narrowed_struct,
                            false,
                        ) catch return;
                        view.is_view = true;
                    }
                }
            }
        }
    }
}

/// Join declared struct field names into a human-readable list for error
/// messages, e.g. `name, entry_point, output`.
fn declaredFieldList(self: *SemanticAnalyzer, fields: []const ast.StructFieldType) ![]u8 {
    var list = std.array_list.Managed(u8).init(self.allocator);
    errdefer list.deinit();
    for (fields, 0..) |field, i| {
        if (i > 0) try list.appendSlice(", ");
        try list.appendSlice(field.name);
    }
    if (fields.len == 0) try list.appendSlice("(none)");
    return list.toOwnedSlice();
}

fn findField(fields: []const @import("../../types/types.zig").StructField, name: []const u8) ?@import("../../types/types.zig").StructField {
    for (fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field;
    }
    return null;
}

/// A registered struct's fields in the shape a `TypeInfo` carries them.
fn structFieldTypes(self: *SemanticAnalyzer, fields: []const @import("../../types/types.zig").StructField) SemanticError![]ast.StructFieldType {
    const out = try self.allocator.alloc(ast.StructFieldType, fields.len);
    for (fields, out) |field, *dest| {
        dest.* = .{ .name = field.name, .type_info = field.field_type_info, .is_public = field.is_public };
    }
    return out;
}

/// The type of `namespace`'s member `name` in value position: a function, a
/// global, or a type. The access is recorded as a resolution. A namespace is
/// not a value.
fn memberOfNamespace(self: *SemanticAnalyzer, expr: *ast.Expr, namespace: @import("../../module/ids.zig").ModuleId, name: ast.Token, type_info: *ast.TypeInfo) SemanticError!*ast.TypeInfo {
    const bound = try names.member(self, namespace, name);
    const symbol = switch (bound.binding) {
        .symbol => |symbol| symbol,
        .namespace => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.MODULE_NAMESPACE_NOT_A_VALUE,
                "Module namespace '{s}' is not a value",
                .{name.lexeme},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
            return type_info;
        },
    };
    const resolved = names.symbolResolution(self, symbol).?;
    try names.annotate(self, expr, resolved);
    if (resolved == .global) {
        try self.recordGlobalSite(.{ .symbol = symbol, .place = .{ .member = expr } });
    }
    const variable = (try names.declaredVariable(self, symbol)) orelse {
        // The defining record bound the name but registered nothing under it:
        // its registration is still active (a declaration cycle).
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.CIRCULAR_IMPORT,
            "'{s}' is used before its module finished declaring it",
            .{name.lexeme},
        );
        self.fatal_error = true;
        type_info.base = .Nothing;
        return type_info;
    };
    const storage = self.memory.scope_manager.value_storage.get(variable.storage_id) orelse return error.StorageNotFound;
    // A qualified global is rewritten in place into its link name, which
    // codegen reads as any other name.
    if (resolved == .global) try names.recordStoreTarget(self, expr.base.id, variable, variable, true);
    type_info.* = storage.type_info.*;
    return type_info;
}

/// The type an expression names in value position — a bare type name or a
/// namespace-qualified one — recorded as a type resolution; null when the
/// expression is not a type name.
fn typeNamed(self: *SemanticAnalyzer, expr: *ast.Expr) SemanticError!?TypeRef {
    switch (expr.data) {
        .Variable => |tok| {
            const result = try names.resolveName(self, tok.lexeme);
            const symbol = result.symbol orelse return null;
            if (symbol.kind != .Type) return null;
            try names.annotate(self, expr, .{ .type = symbol.typeRef() });
            return symbol.typeRef();
        },
        .FieldAccess => |field| {
            const namespace = (try names.namespaceOf(self, field.object)) orelse return null;
            const bound = try names.member(self, namespace, field.field);
            const symbol = switch (bound.binding) {
                .symbol => |symbol| symbol,
                .namespace => return null,
            };
            if (symbol.kind != .Type) return null;
            try names.annotate(self, expr, .{ .type = symbol.typeRef() });
            return symbol.typeRef();
        },
        else => return null,
    }
}

/// A method or function of `owner` called through `receiver` (null when it is
/// called through the type, `T.f()`). A private one is reachable only from
/// inside the struct: through `this`, or through the type from the struct's own
/// functions and methods.
fn methodOf(self: *SemanticAnalyzer, expr: *ast.Expr, owner: TypeRef, name: []const u8, receiver: ?*const ast.Expr) SemanticError!?@import("semantic.zig").StructMethodInfo {
    const methods = (try self.methodsOf(owner)) orelse return null;
    const method = methods.get(name) orelse return null;
    const reachable = if (receiver) |r|
        r.data == .This
    else if (self.enclosing) |enclosing|
        enclosing.ref.eql(owner)
    else
        false;
    if (!method.is_public and !reachable) {
        self.reporter.reportCompileError(
            getLocationFromBase(expr.base),
            ErrorCode.PRIVATE_FIELD_ACCESS,
            "Cannot call private {s} '{s}' of struct '{s}' outside the struct",
            .{ if (method.is_static) "function" else "method", name, owner.name },
        );
        self.fatal_error = true;
        return null;
    }
    return method;
}

/// A call through a field access: a module function (`ns.fn(...)`), a static
/// method or constructor (`T.m(...)`), or a method on a value (`x.m(...)`).
/// The callee is recorded as a resolution, so codegen dispatches without
/// re-deriving any of this.
fn inferMemberCall(self: *SemanticAnalyzer, expr: *ast.Expr, callee: *ast.Expr, arguments: []const ast.CallArgument, type_info: *ast.TypeInfo) SemanticError!*ast.TypeInfo {
    const field_access = callee.data.FieldAccess;
    const method_name = field_access.field.lexeme;

    if (try names.namespaceOf(self, field_access.object)) |namespace| {
        const function_type = try memberOfNamespace(self, callee, namespace, field_access.field, try ast.TypeInfo.createDefault(self.allocator));
        try self.type_cache.put(callee.base.id, function_type);
        if (function_type.base != .Function or function_type.function_type == null) {
            if (function_type.base != .Nothing) {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.INVALID_FUNCTION_CALL, "'{s}' is not a function", .{method_name});
                self.fatal_error = true;
            }
            type_info.base = .Nothing;
            return type_info;
        }
        if (!try validateFunctionCallArguments(self, expr, arguments, function_type.function_type.?)) {
            type_info.base = .Nothing;
            return type_info;
        }
        type_info.* = function_type.function_type.?.return_type.*;
        return type_info;
    }

    if (try typeNamed(self, field_access.object)) |owner| {
        const method = (try methodOf(self, expr, owner, method_name, null)) orelse {
            if (!self.fatal_error) {
                self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "Unknown method '{s}' on struct '{s}'", .{ method_name, owner.name });
                self.fatal_error = true;
            }
            type_info.base = .Nothing;
            return type_info;
        };
        if (!method.is_static) {
            self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "Method '{s}' of '{s}' needs a receiver; call it on a value", .{ method_name, owner.name });
            self.fatal_error = true;
            type_info.base = .Nothing;
            return type_info;
        }
        try names.annotate(self, callee, .{ .static_method = .{ .owner = owner, .name = method.name } });
        if (!try validateFunctionCallArguments(self, expr, arguments, method.signature)) {
            type_info.base = .Nothing;
            return type_info;
        }
        type_info.* = method.signature.return_type.*;
        return type_info;
    }

    const object_type = try inferTypeFromExpr(self, field_access.object);
    switch (object_type.base) {
        // A built-in type has no methods of its own: a compiler method is
        // `@`-prefixed (`xs.@push(1)`, `@push(xs, 1)`), so `xs.push(1)` is
        // never one (docs/methods.md).
        .Array, .String => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.UNKNOWN_METHOD,
                "{s} has no method '{s}'; compiler methods are `@`-prefixed: `.@{s}(...)`",
                .{ if (object_type.base == .Array) "An array" else "A string", method_name, method_name },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        .Map => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.UNKNOWN_METHOD,
                "Maps do not have methods; use indexing (map[key]) or assignment (map[key] is value)",
                .{},
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        .Struct, .Custom => {
            if (object_type.custom_type) |custom| {
                const owner = custom.resolved();
                const start = self.reporter.diagnostics.items.len;
                if (try methodOf(self, expr, owner, method_name, field_access.object)) |method| {
                    // A struct's function called through `this` takes no
                    // receiver; any other value cannot stand in for the type.
                    if (method.is_static and field_access.object.data != .This) {
                        self.reporter.reportCompileError(getLocationFromBase(expr.base), ErrorCode.UNKNOWN_METHOD, "'{s}' is a function of '{s}'; call it as {s}.{s}()", .{ method_name, owner.name, owner.name, method_name });
                        self.fatal_error = true;
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    try names.annotate(self, callee, if (method.is_static)
                        .{ .static_method = .{ .owner = owner, .name = method.name } }
                    else
                        .{ .method = .{ .owner = owner, .name = method.name } });
                    if (!try validateFunctionCallArguments(self, expr, arguments, method.signature)) {
                        type_info.base = .Nothing;
                        return type_info;
                    }
                    type_info.* = method.signature.return_type.*;
                    return type_info;
                }
                if (self.reporter.firstErrorSince(start) != null) {
                    type_info.base = .Nothing;
                    return type_info;
                }
            }

            // A function-typed field called like a method.
            if (object_type.struct_fields) |fields| {
                for (fields) |field| {
                    if (std.mem.eql(u8, field.name, method_name) and field.type_info.base == .Function) {
                        if (!try validateFunctionCallArguments(self, expr, arguments, field.type_info.function_type.?)) {
                            type_info.base = .Nothing;
                            return type_info;
                        }
                        type_info.* = field.type_info.function_type.?.return_type.*;
                        return type_info;
                    }
                }
            }

            const display_name = if (object_type.custom_type) |custom| custom.displayName() else "<struct>";
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.UNKNOWN_METHOD,
                "Unknown method '{s}' on struct '{s}'",
                .{ method_name, display_name },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
        else => {
            self.reporter.reportCompileError(
                getLocationFromBase(expr.base),
                ErrorCode.CANNOT_CALL_METHOD_ON_TYPE,
                "Cannot call method '{s}' on type {s}",
                .{ method_name, @tagName(object_type.base) },
            );
            self.fatal_error = true;
            type_info.base = .Nothing;
        },
    }
    return type_info;
}
