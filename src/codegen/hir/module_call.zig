const std = @import("std");
const ast = @import("../../ast/ast.zig");
const HIRGenerator = @import("soxa_generator.zig").HIRGenerator;
const HIRType = @import("soxa_types.zig").HIRType;
const CallKind = @import("soxa_types.zig").CallKind;
const HIRInstruction = @import("soxa_instructions.zig").HIRInstruction;

/// Resolved module, local, or builtin function call.
pub const ResolvedCall = struct {
    qualified_name: []const u8,
    call_kind: CallKind,
    function_index: ?u32,
    /// When true, `qualified_name` was allocated with `generator.allocator` (HIR-owned, do not free during emission).
    name_allocated: bool,
};

pub const CallTarget = union(enum) {
    function: ResolvedCall,
    struct_static: struct {
        qualified_name: []const u8,
        name_allocated: bool,
        function_index: ?u32,
    },
    struct_method: struct {
        struct_name: []const u8,
        field_access: ast.FieldAccess,
    },
    struct_constructor: []const u8,
    internal_method: ast.FieldAccess,
};

/// Walk `std` → `std.process` for nested module namespaces.
pub fn moduleNamespaceFromExpr(generator: *HIRGenerator, expr: *ast.Expr) !?[]const u8 {
    return switch (expr.data) {
        .Variable => |var_tok| blk: {
            if (generator.isModuleNamespace(var_tok.lexeme)) {
                break :blk try generator.allocator.dupe(u8, var_tok.lexeme);
            }
            break :blk null;
        },
        .FieldAccess => |fa| blk: {
            const parent = try moduleNamespaceFromExpr(generator, fa.object) orelse break :blk null;
            defer generator.allocator.free(parent);
            break :blk try std.fmt.allocPrint(generator.allocator, "{s}.{s}", .{ parent, fa.field.lexeme });
        },
        else => null,
    };
}

fn resolveModuleFieldCall(generator: *HIRGenerator, field_access: ast.FieldAccess) !?ResolvedCall {
    if (try moduleNamespaceFromExpr(generator, field_access.object)) |ns| {
        defer generator.allocator.free(ns);
        const qualified = try std.fmt.allocPrint(generator.allocator, "{s}.{s}", .{ ns, field_access.field.lexeme });
        return .{
            .qualified_name = qualified,
            .call_kind = .ModuleFunction,
            .function_index = generator.getFunctionIndex(qualified),
            .name_allocated = true,
        };
    }
    return null;
}

pub fn resolveBareCallee(generator: *HIRGenerator, bare_name: []const u8) ResolvedCall {
    if (generator.getFunctionIndex(bare_name)) |idx| {
        return .{
            .qualified_name = bare_name,
            .call_kind = .LocalFunction,
            .function_index = idx,
            .name_allocated = false,
        };
    }

    if (generator.resolveQualifiedModuleLocalFunction(bare_name)) |qualified_name| {
        return .{
            .qualified_name = qualified_name,
            .call_kind = .ModuleFunction,
            .function_index = generator.getFunctionIndex(qualified_name),
            .name_allocated = false,
        };
    }

    if (bare_name.len > 0 and bare_name[0] == '@') {
        return .{
            .qualified_name = bare_name,
            .call_kind = .BuiltinFunction,
            .function_index = null,
            .name_allocated = false,
        };
    }

    return .{
        .qualified_name = bare_name,
        .call_kind = .ModuleFunction,
        .function_index = null,
        .name_allocated = false,
    };
}

fn structNameForReceiver(generator: *HIRGenerator, recv_var_name: []const u8) []const u8 {
    if (generator.symbol_table.getVariableCustomType(recv_var_name)) |ctype| return ctype;
    return recv_var_name;
}

/// Classify any function-call callee (module, local, struct method, internal, etc.).
pub fn classifyCallTarget(generator: *HIRGenerator, callee: *ast.Expr) !CallTarget {
    return switch (callee.data) {
        .Variable => |var_token| .{ .function = resolveBareCallee(generator, var_token.lexeme) },
        .FieldAccess => |field_access| try classifyFieldAccessCall(generator, field_access),
        else => error.UnsupportedCallTarget,
    };
}

fn classifyFieldAccessCall(generator: *HIRGenerator, field_access: ast.FieldAccess) !CallTarget {
    if (field_access.object.data == .Variable) {
        const var_name = field_access.object.data.Variable.lexeme;
        const struct_name = structNameForReceiver(generator, var_name);
        if (!std.mem.eql(u8, struct_name, var_name)) {
            if (generator.struct_methods.get(struct_name)) |method_table| {
                if (method_table.get(field_access.field.lexeme)) |_| {
                    return .{
                        .struct_method = .{
                            .struct_name = struct_name,
                            .field_access = field_access,
                        },
                    };
                }
            }
        }
    }

    // Module-qualified struct static method / constructor: ns.Struct.method(...).
    // The object is itself a field access (`ns.Struct`) where `ns` is a module
    // namespace and `Struct` is an imported struct type. Struct methods are emitted
    // under their bare `Struct.method` name, so resolve them here before treating
    // the call as a plain module function (`ns.Struct.method`, which does not exist).
    if (field_access.object.data == .FieldAccess) {
        const inner = field_access.object.data.FieldAccess;
        if (try moduleNamespaceFromExpr(generator, inner.object)) |ns| {
            defer generator.allocator.free(ns);
            const type_name = inner.field.lexeme;
            const method_name = field_access.field.lexeme;

            if (generator.isCustomType(type_name)) |ct| {
                if (ct.kind == .Struct and std.mem.eql(u8, method_name, "New")) {
                    return .{ .struct_constructor = type_name };
                }
            }

            if (generator.struct_methods.get(type_name)) |method_table| {
                if (method_table.get(method_name)) |mi| {
                    if (mi.is_static) {
                        const qualified = try std.fmt.allocPrint(generator.allocator, "{s}.{s}", .{ type_name, method_name });
                        return .{
                            .struct_static = .{
                                .qualified_name = qualified,
                                .name_allocated = true,
                                .function_index = generator.getFunctionIndex(qualified),
                            },
                        };
                    }
                }
            }
        }
    }

    if (try resolveModuleFieldCall(generator, field_access)) |resolved| {
        return .{ .function = resolved };
    }

    if (field_access.object.data == .Variable) {
        const type_name = field_access.object.data.Variable.lexeme;

        if (generator.isCustomType(type_name)) |ct| {
            if (ct.kind == .Struct) {
                if (std.mem.eql(u8, field_access.field.lexeme, "New")) {
                    return .{ .struct_constructor = type_name };
                }

                const qualified = try std.fmt.allocPrint(
                    generator.allocator,
                    "{s}.{s}",
                    .{ type_name, field_access.field.lexeme },
                );
                if (generator.getFunctionIndex(qualified)) |idx| {
                    return .{
                        .struct_static = .{
                            .qualified_name = qualified,
                            .name_allocated = true,
                            .function_index = idx,
                        },
                    };
                }
            }
        }

        const struct_name = structNameForReceiver(generator, type_name);
        if (generator.struct_methods.get(struct_name)) |method_table| {
            if (method_table.get(field_access.field.lexeme)) |_| {
                return .{
                    .struct_method = .{
                        .struct_name = struct_name,
                        .field_access = field_access,
                    },
                };
            }
        }
    }

    return .{ .internal_method = field_access };
}

/// Return-type inference for function-call expressions (HIR).
pub fn inferFunctionCallReturnType(generator: *HIRGenerator, expr: *ast.Expr) HIRType {
    const call = expr.data.FunctionCall;
    const target = classifyCallTarget(generator, call.callee) catch return .Unknown;

    return switch (target) {
        .function => |resolved| {
            defer if (resolved.name_allocated) generator.allocator.free(resolved.qualified_name);

            if (resolved.call_kind == .BuiltinFunction and std.mem.eql(u8, resolved.qualified_name, "pop")) {
                if (call.arguments.len == 1) {
                    const arg_type = generator.inferTypeFromExpression(call.arguments[0].expr);
                    return switch (arg_type) {
                        .Array => |elem_ptr| elem_ptr.*,
                        .String => .String,
                        else => .Unknown,
                    };
                }
                return .Unknown;
            }

            return generator.inferCallReturnType(resolved.qualified_name, resolved.call_kind) catch .Unknown;
        },
        .struct_static => |ss| {
            defer if (ss.name_allocated) generator.allocator.free(ss.qualified_name);
            return generator.inferCallReturnType(ss.qualified_name, .LocalFunction) catch .Unknown;
        },
        .struct_method => |sm| {
            const qualified_name = std.fmt.allocPrint(
                generator.allocator,
                "{s}.{s}",
                .{ sm.struct_name, sm.field_access.field.lexeme },
            ) catch return .Unknown;
            defer generator.allocator.free(qualified_name);
            if (generator.inferCallReturnType(qualified_name, .LocalFunction)) |return_type| {
                return return_type;
            } else |_| {}

            if (generator.struct_methods.get(sm.struct_name)) |method_table| {
                if (method_table.get(sm.field_access.field.lexeme)) |mi| {
                    return generator.convertTypeInfo(mi.return_type.*);
                }
            }
            return .Unknown;
        },
        .struct_constructor => |type_name| blk: {
            if (generator.isCustomType(type_name)) |_| {
                break :blk generator.type_system.structTypeForName(type_name);
            }
            break :blk .Unknown;
        },
        .internal_method => |_| generator.type_system.inferInternalMethodCallReturnType(expr),
    };
}

/// Whether a tail call can be emitted for this function-call expression.
pub fn tryEmitTailCall(generator: *HIRGenerator, expr: *ast.Expr) bool {
    const call = expr.data.FunctionCall;
    const target = classifyCallTarget(generator, call.callee) catch return false;
    const resolved = switch (target) {
        .function => |r| r,
        else => return false,
    };

    for (call.arguments) |arg| {
        generator.generateExpression(arg.expr, true, true) catch return false;
    }

    const function_index = resolved.function_index orelse return false;
    const return_type = generator.inferCallReturnType(resolved.qualified_name, resolved.call_kind) catch .Nothing;
    const target_module = generator.computeTargetModule(resolved.qualified_name, resolved.call_kind) catch return false;

    const tail_call = HIRInstruction{ .Call = .{
        .function_index = function_index,
        .qualified_name = resolved.qualified_name,
        .arg_count = @intCast(call.arguments.len),
        .call_kind = resolved.call_kind,
        .target_module = target_module,
        .return_type = return_type,
        .tail = true,
    } };

    generator.instructions.append(tail_call) catch return false;
    return true;
}
