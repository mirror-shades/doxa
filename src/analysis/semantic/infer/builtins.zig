//! Type rules for the built-in `@` methods.

const std = @import("std");
const ast = @import("../../../ast/ast.zig");
const SemanticAnalyzer = @import("../semantic.zig").SemanticAnalyzer;
const Errors = @import("../../../utils/errors.zig");
const ErrorCode = Errors.ErrorCode;
const helpers = @import("../helpers.zig");
const getLocationFromBase = helpers.getLocationFromBase;
const names = @import("../names.zig");
const builtin_methods = @import("../../../runtime/builtin_methods.zig");
const infer_type = @import("../infer_type.zig");
const SemanticError = infer_type.SemanticError;

/// The type of a builtin's subject argument. Expression inference can miss a
/// variable's type (e.g. a global or an imported binding) even though its
/// declaration is in scope, so fall back to the stored declaration type.
/// `@remove` or `@slice` of an array nothing gave an element type: there
/// is no element type for the result to have.
fn uninferredElement(self: *SemanticAnalyzer, subject: *ast.Expr, type_info: *ast.TypeInfo) *ast.TypeInfo {
    self.reporter.reportCompileError(
        getLocationFromBase(subject.base),
        ErrorCode.CANNOT_INFER_ARRAY_ELEMENT_TYPE,
        "cannot infer element type of empty array literal; add an element type annotation (e.g. `int[]`)",
        .{},
    );
    self.fatal_error = true;
    type_info.* = .{ .base = .Error };
    return type_info;
}

/// A built-in that changes its collection (`@push`, `@pop`, `@insert`,
/// `@remove`, `@clear`) needs a name that may change: a `const` name is
/// refused, as `x is …` is (E1015). An array whose storage forbids the change
/// is `ensureDynamicArrayStorage`'s to report.
fn ensureMutableSubject(self: *SemanticAnalyzer, subject: *const ast.Expr, subject_type: *const ast.TypeInfo, usage: []const u8) SemanticError!bool {
    if (subject.data != .Variable) return true;
    if (subject_type.base == .Array and subject_type.array_storage != .dynamic) return true;
    const name = subject.data.Variable.lexeme;
    const storage_id = (try names.storageBeneath(self, name)) orelse return true;
    const storage = self.memory.scope_manager.value_storage.get(storage_id) orelse return true;
    if (!storage.constant) return true;
    self.reporter.reportCompileError(
        getLocationFromBase(subject.base),
        ErrorCode.INVALID_ASSIGNMENT_TARGET,
        "Cannot change immutable variable '{s}' with {s}",
        .{ name, usage },
    );
    self.fatal_error = true;
    return false;
}

pub fn inferBuiltinSubjectType(self: *SemanticAnalyzer, subject: *ast.Expr) SemanticError!*ast.TypeInfo {
    const subject_type = try infer_type.inferTypeFromExpr(self, subject);
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

/// Type rules for the built-in `@`-methods, reached by every call shape.
/// `receiver` is absent only for `@std`, whose argument list is empty.
///
/// Memoizing entry point: the `.InternalCall` cases of `inferTypeFromExpr`
/// return straight through here, so without caching the result a builtin is
/// re-inferred — and a failing one re-reported — on every analysis pass over
/// the same expression.
pub fn inferBuiltinCall(
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
    for (args) |arg| _ = try infer_type.inferTypeFromExpr(self, arg);

    return result;
}

/// The arguments of a built-in as one list: the receiver, when there is one,
/// followed by the written arguments. Every call shape that reaches a `@` rule
/// builds the list this way, so the list is not a per-branch concern.
pub fn builtinArgs(
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

pub fn inferBuiltinCallInner(
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
        if (t0.base == .Error) return type_info;
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
        if (coll_t.base == .Error) return type_info;
        if (!try ensureMutableSubject(self, args[0], coll_t, "@push")) return type_info;
        const val_t = if (coll_t.base == .Array and coll_t.array_type != null)
            try infer_type.inferTypeIn(self, args[1], coll_t.array_type.?)
        else
            try infer_type.inferTypeFromExpr(self, args[1]);
        if (val_t.base == .Error) return type_info;
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
        if (coll_t.base == .Error) return type_info;
        if (!try ensureMutableSubject(self, args[0], coll_t, "@pop")) return type_info;
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
        if (coll_t.base == .Error) return type_info;
        if (!try ensureMutableSubject(self, args[0], coll_t, "@insert")) return type_info;
        const idx_t = try infer_type.inferTypeFromExpr(self, args[1]);
        if (idx_t.base == .Error) return type_info;
        if (idx_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@insert index must be int, got {s}", .{@tagName(idx_t.base)});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@insert")) return type_info;
            const val_t = if (coll_t.array_type) |elem| try infer_type.inferTypeIn(self, args[2], elem) else try infer_type.inferTypeFromExpr(self, args[2]);
            if (val_t.base == .Error) return type_info;
            if (coll_t.array_type) |elem| {
                try helpers.unifyElement(self, elem, val_t, args[2], .{ .location = getLocationFromBase(args[2].base) });
            }
        } else if (coll_t.base == .String) {
            // A string is not a byte array: only a string can be inserted.
            // `byte[]` crosses with `@pack`.
            const val_t = try infer_type.inferTypeFromExpr(self, args[2]);
            if (val_t.base == .Error) return type_info;
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
        if (coll_t.base == .Error) return type_info;
        if (!try ensureMutableSubject(self, args[0], coll_t, "@remove")) return type_info;
        const idx_t = try infer_type.inferTypeFromExpr(self, args[1]);
        if (idx_t.base == .Error) return type_info;
        if (idx_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARRAY_INDEX_TYPE, "@remove index must be int, got {s}", .{@tagName(idx_t.base)});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@remove")) return type_info;
            const elem = coll_t.array_type orelse return uninferredElement(self, args[0], type_info);
            type_info.* = elem.*;
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
        if (coll_t.base == .Error) return type_info;
        const start_t = try infer_type.inferTypeFromExpr(self, args[1]);
        if (start_t.base == .Error) return type_info;
        const len_t = try infer_type.inferTypeFromExpr(self, args[2]);
        if (len_t.base == .Error) return type_info;
        if (start_t.base != .Int or len_t.base != .Int) {
            self.reporter.reportCompileError(getLocationFromBase(args[1].base), ErrorCode.INVALID_ARGUMENT_TYPE, "@slice start/length must be ints", .{});
            self.fatal_error = true;
            return type_info;
        }
        if (coll_t.base == .String) {
            type_info.* = .{ .base = .String };
        } else if (coll_t.base == .Array) {
            if (!self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@slice")) return type_info;
            const elem = coll_t.array_type orelse return uninferredElement(self, args[0], type_info);
            const new_elem = try ast.TypeInfo.createDefault(self.allocator);
            new_elem.* = elem.*;
            type_info.* = .{ .base = .Array, .array_type = new_elem };
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
        const operand_type = try infer_type.inferTypeFromExpr(self, args[0]);
        if (operand_type.base == .Error) return type_info;
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
                try helpers.unifyTypesExpr(self, &expected, operand_type, args[0], .{ .location = getLocationFromBase(args[0].base) });
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
            const arg_type = try infer_type.inferTypeFromExpr(self, args[0]);
            if (arg_type.base == .Error) return type_info;
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
            const arg_type = try infer_type.inferTypeFromExpr(self, args[0]);
            if (arg_type.base == .Error) return type_info;
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
        if (coll_t.base == .Error) return type_info;
        if (!try ensureMutableSubject(self, args[0], coll_t, "@clear")) return type_info;
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
        if (coll_t.base == .Array and !self.ensureDynamicArrayStorage(coll_t, getLocationFromBase(args[0].base), "@clear")) return type_info;
        // @clear returns nothing
        type_info.* = .{ .base = .Nothing };
        return type_info;
    } else if (std.mem.eql(u8, fname, "find")) {
        if (!validateBuiltinArgs.check(self, expr, fname, args.len)) return type_info;
        const coll_t = try inferBuiltinSubjectType(self, args[0]);
        if (coll_t.base == .Error) return type_info;
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
            const cond_t = try infer_type.inferTypeFromExpr(self, args[0]);
            if (cond_t.base == .Error) return type_info;
            if (cond_t.base != .Tetra) {
                self.reporter.reportCompileError(getLocationFromBase(args[0].base), ErrorCode.TYPE_MISMATCH, "@assert condition must be tetra", .{});
                self.fatal_error = true;
            }
        }
        if (args.len >= 2) {
            const msg_t = try infer_type.inferTypeFromExpr(self, args[1]);
            if (msg_t.base == .Error) return type_info;
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
        const t0 = try infer_type.inferTypeFromExpr(self, args[0]);
        if (t0.base == .Error) return type_info;
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
