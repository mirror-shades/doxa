//! Registration of inline `zig` and `.zig` functions in the Types stage.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const TypeInfo = ast.TypeInfo;
const TypeRef = ast.TypeRef;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const ErrorCode = Errors.ErrorCode;
const names = @import("names.zig");
const graph_mod = @import("../../module/graph.zig");
const ModuleRecord = graph_mod.ModuleRecord;
const resolution = @import("resolution.zig");
const Types = @import("../../types/types.zig");
const SemanticAnalyzer = @import("semantic.zig").SemanticAnalyzer;
const returns = @import("returns.zig");

/// The Types stage of an inline `zig` block or a `.zig` file: resolve each
/// signature's Doxa types and bind each function in the record's module
/// scope, so `Z.fn` and an imported Zig function resolve like any other
/// module function. A block resolves `DoxaEnum_X` through its owner's
/// bindings; a `.zig` file has no Doxa bindings to resolve through.
pub fn registerZigFunctions(self: *SemanticAnalyzer, record: *ModuleRecord) ErrorList!void {
    const unit = &record.zig.?;
    const scope = self.moduleScope(record.id);
    // Fallible returns resolve to `nothing | <enum>`; the wrapper needs the
    // enum's variants to synthesize the Zig error set the body returns
    // against. Their `path` spelling is captured before resolution replaces
    // `custom_type.written` with the resolved ref.
    var error_sets = std.array_list.Managed(graph_mod.ZigErrorSet).init(self.allocator);
    defer error_sets.deinit();
    for (unit.sigs) |*sig| {
        const error_path: ?[]const u8 = blk: {
            if (sig.return_type.base != .Union) break :blk null;
            for (sig.return_type.union_type.?.types) |member| {
                if (member.base != .Enum) continue;
                if (member.custom_type) |custom| switch (custom) {
                    .written => |written| break :blk written,
                    .ref => {},
                };
            }
            break :blk null;
        };

        for (sig.param_types) |*param| try resolveZigType(self, record, param);
        try resolveZigType(self, record, &sig.return_type);

        if (error_path) |path| {
            const resolved = try zigErrorSetVariants(self, sig.return_type);
            var already = false;
            for (error_sets.items) |seen| {
                if (std.mem.eql(u8, seen.path, path)) already = true;
            }
            if (!already) try error_sets.append(.{ .path = path, .ref = resolved.ref, .variants = resolved.variants });
        }

        const return_type = try ast.TypeInfo.createDefault(self.allocator);
        return_type.* = sig.return_type;
        const function_type = try self.allocator.create(ast.FunctionType);
        function_type.* = .{ .params = sig.param_types, .return_type = return_type };
        const type_info = try ast.TypeInfo.createDefault(self.allocator);
        type_info.* = .{ .base = .Function, .function_type = function_type, .is_mutable = false };
        const variable = try scope.createValueBinding(sig.name, .FUNCTION, type_info, true);
        variable.used = true;
    }
    if (error_sets.items.len > 0) unit.error_sets = try error_sets.toOwnedSlice();
}

/// The resolved identity and variant names of the enum member of a fallible
/// return, in declaration order — the order a Zig error set's names must
/// follow for the generated `name -> discriminant` switch to agree with
/// Doxa.
pub fn zigErrorSetVariants(self: *SemanticAnalyzer, return_type: ast.TypeInfo) ErrorList!struct {
    ref: ast.TypeRef,
    variants: []const []const u8,
} {
    const enum_member = for (return_type.union_type.?.types) |member| {
        if (member.base == .Enum) break member;
    } else return error.UndefinedType;

    const ref = switch (enum_member.custom_type orelse return error.UndefinedType) {
        .ref => |r| r,
        .written => return error.UndefinedType,
    };
    const id = self.enum_table.idOf(ref) orelse return error.UndefinedType;
    const variants = self.enum_table.variants(id) orelse return error.UndefinedType;

    const variant_names = try self.allocator.alloc([]const u8, variants.len);
    for (variants, 0..) |variant, i| variant_names[i] = variant.name;
    return .{ .ref = ref, .variants = variant_names };
}

pub fn resolveZigType(self: *SemanticAnalyzer, record: *ModuleRecord, type_info: *ast.TypeInfo) ErrorList!void {
    const owner = record.owner orelse {
        if (hasWrittenType(type_info)) {
            self.reporter.reportCompileError(
                record.zig.?.location,
                ErrorCode.UNKNOWN_TYPE,
                "Zig module '{s}' names a Doxa type; a `.zig` file has no Doxa bindings, so `DoxaEnum_<Name>` is only available in an inline `zig` block",
                .{self.graph.moduleName(record.id)},
            );
            self.fatal_error = true;
            return error.UndefinedType;
        }
        return;
    };
    try names.resolveTypeInfo(self, type_info, owner, record.zig.?.location);
}

pub fn hasWrittenType(type_info: *const ast.TypeInfo) bool {
    if (type_info.custom_type) |custom| if (custom == .written) return true;
    if (type_info.array_type) |element| return hasWrittenType(element);
    return false;
}
