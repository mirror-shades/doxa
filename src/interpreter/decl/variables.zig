const std = @import("std");
const HIRValue = @import("../../codegen/hir/soxa_values.zig").HIRValue;
const Core = @import("../core.zig");
const HIRFrame = Core.HIRFrame;
const HotVar = Core.HotVar;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const TypeInfo = @import("../../ast/ast.zig").TypeInfo;

pub const VariableOps = struct {
    inline fn maybeAddHotVar(vm: anytype, name: []const u8, storage_id: u32) void {
        // Only when turbo_mode is enabled
        if (!vm.turbo_mode) return;
        // Avoid duplicates
        var i: usize = 0;
        while (i < vm.hot_var_count) : (i += 1) {
            const hv = vm.hot_vars[i];
            if (hv != null and std.mem.eql(u8, hv.?.name, name)) return;
        }
        if (vm.hot_var_count < vm.hot_vars.len) {
            vm.hot_vars[vm.hot_var_count] = HotVar{ .name = name, .storage_id = storage_id };
            vm.hot_var_count += 1;
        }
    }
    // Execute LoadVar instruction
    pub fn execLoadVar(vm: anytype, v: anytype) !void {
        if (vm.turbo_mode) {
            // TURBO: Check hot variable cache first (array lookup - fastest possible)
            for (vm.hot_vars[0..vm.hot_var_count]) |hot_var| {
                if (hot_var != null and std.mem.eql(u8, hot_var.?.name, v.var_name)) {
                    if (vm.memory_manager.scope_manager.value_storage.get(hot_var.?.storage_id)) |storage| {
                        const hir_value = vm.tokenLiteralToHIRValueWithType(storage.value, storage.type_info);
                        try vm.stack.push(HIRFrame.initFromHIRValue(hir_value));
                        return; // Ultra-fast path
                    }
                    break;
                }
            }
        }

        // FALLBACK: Standard variable lookup (populate cache for next time)
        if (vm.current_scope.lookupVariable(v.var_name)) |variable| {
            if (vm.memory_manager.scope_manager.value_storage.get(variable.storage_id)) |storage| {
                const hir_value = vm.tokenLiteralToHIRValueWithType(storage.value, storage.type_info);
                try vm.stack.push(HIRFrame.initFromHIRValue(hir_value));
            } else {
                // Propagate error so VM halts execution immediately
                return vm.reporter.reportRuntimeError(
                    null,
                    ErrorCode.VARIABLE_NOT_FOUND,
                    "Variable storage not found for: {s}",
                    .{v.var_name},
                );
            }
        } else {
            // Propagate undefined variable error
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Undefined variable: {s}", .{v.var_name});
        }
    }

    // Execute StoreVar instruction
    pub fn execStoreVar(vm: anytype, v: anytype) !void {
        // Store top of stack to variable
        const value = try vm.stack.pop();

        // Perform type coercion if needed
        const coerced_value = vm.coerceValue(value.value, v.expected_type);

        const token_literal = vm.hirValueToTokenLiteral(coerced_value);
        const token_type = vm.hirValueToTokenType(coerced_value);
        const type_info_value = vm.hirValueToTypeInfo(coerced_value);
        const type_info = try vm.getCanonicalTypeInfo(type_info_value);

        if (@hasField(@TypeOf(v), "scope_kind") and v.scope_kind == .Local) {
            // Local semantics:
            // - If a variable exists anywhere in the active scope chain:
            //     * If it's constant, shadow by creating a new current-scope binding
            //     * Else update that existing storage
            // - If not found, create in current scope
            if (vm.current_scope.lookupVariable(v.var_name)) |nearest_var| {
                if (vm.memory_manager.scope_manager.value_storage.getPtr(nearest_var.storage_id)) |storage| {
                    if (storage.*.constant) {
                        // Shadow constant from ancestor scope with a new local binding
                        const created = vm.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                        };
                        maybeAddHotVar(vm, v.var_name, created.storage_id);
                    } else {
                        // Update nearest existing (non-constant) variable
                        storage.*.value = token_literal;
                        storage.*.type = token_type;
                        storage.*.type_info = type_info;
                        maybeAddHotVar(vm, v.var_name, nearest_var.storage_id);
                    }
                } else {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
                }
            } else {
                // Not found anywhere - create in current scope
                const created2 = vm.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                };
                maybeAddHotVar(vm, v.var_name, created2.storage_id);
            }
        } else {
            // Non-local: nearest existing variable across active scopes; otherwise create in current scope
            if (vm.current_scope.lookupVariable(v.var_name)) |variable| {
                if (vm.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                    if (storage.*.constant) {
                        return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Cannot modify constant variable: {s}", .{v.var_name});
                    }

                    storage.*.value = token_literal;
                    storage.*.type = token_type;
                    storage.*.type_info = type_info;
                    maybeAddHotVar(vm, v.var_name, variable.storage_id);
                } else {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
                }
            } else {
                const created3 = vm.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, false) catch |err| {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create variable {s}: {}", .{ v.var_name, err });
                };
                maybeAddHotVar(vm, v.var_name, created3.storage_id);
            }
        }
    }

    // Execute StoreConst instruction
    pub fn execStoreConst(vm: anytype, v: anytype) !void {
        // Store top of stack as a constant binding
        const value = try vm.stack.pop();

        const token_literal = vm.hirValueToTokenLiteral(value.value);
        const token_type = vm.hirValueToTokenType(value.value);
        const type_info_value = vm.hirValueToTypeInfo(value.value);
        const type_info = try vm.getCanonicalTypeInfo(type_info_value);

        // If a binding exists in any active scope, update only if it's uninitialized constant (nothing)
        if (vm.current_scope.lookupVariable(v.var_name)) |variable| {
            if (vm.memory_manager.scope_manager.value_storage.getPtr(variable.storage_id)) |storage| {
                if (storage.*.constant) {
                    const is_nothing = storage.*.value == .nothing;
                    if (!is_nothing) {
                        // Shadow initialized constant with a new local constant in the current scope
                        _ = vm.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, true) catch |err| {
                            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create constant {s}: {}", .{ v.var_name, err });
                        };
                        return;
                    }
                }
                storage.*.value = token_literal;
                storage.*.type = token_type;
                storage.*.type_info = type_info;
                storage.*.constant = true;
            } else {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable storage not found for: {s}", .{v.var_name});
            }
        } else {
            // Create constant in the CURRENT scope so it does not leak beyond iteration
            _ = vm.current_scope.createValueBinding(v.var_name, token_literal, token_type, type_info, true) catch |err| {
                return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Failed to create constant {s}: {}", .{ v.var_name, err });
            };
        }
    }

    // Execute PushStorageId instruction
    pub fn execPushStorageId(vm: anytype, p: anytype) !void {
        // Push a variable's storage ID onto the stack for alias arguments
        if (vm.current_scope.lookupVariable(p.var_name)) |variable| {
            try vm.stack.push(HIRFrame.initFromHIRValue(HIRValue{ .storage_id_ref = variable.storage_id }));
        } else {
            return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Variable not found for alias argument: {s}", .{p.var_name});
        }
    }

    // Execute StoreParamAlias instruction
    pub fn execStoreParamAlias(vm: anytype, s: anytype) !void {
        // Store a parameter as an alias to an existing storage ID
        const storage_id_value = try vm.stack.pop();
        switch (storage_id_value.value) {
            .storage_id_ref => |storage_id| {
                // Get the original storage to determine type info
                if (vm.memory_manager.scope_manager.value_storage.get(storage_id)) |original_storage| {
                    const token_type = original_storage.type;
                    const type_info = try vm.getCanonicalTypeInfo(original_storage.type_info.*);

                    // Create alias variable in current scope
                    const alias_var = try vm.current_scope.createAliasFromStorageId(s.param_name, storage_id, token_type, type_info);
                    maybeAddHotVar(vm, s.param_name, alias_var.storage_id);
                } else {
                    return vm.reporter.reportRuntimeError(null, ErrorCode.VARIABLE_NOT_FOUND, "Storage not found for alias parameter: {s}", .{s.param_name});
                }
            },
            else => {
                return vm.reporter.reportRuntimeError(null, ErrorCode.INVALID_ALIAS_PARAMETER, "Expected storage ID reference for alias parameter: {s}", .{s.param_name});
            },
        }
    }
};
