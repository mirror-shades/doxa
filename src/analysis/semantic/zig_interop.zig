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
    // A struct result's spelling is captured the same way, for the alias the
    // wrapper declares for its mirror.
    var structs: std.ArrayListUnmanaged(graph_mod.ZigStruct) = .empty;
    for (unit.sigs) |*sig| {
        const struct_path: ?[]const u8 = blk: {
            const result = resultOf(&sig.return_type) orelse break :blk null;
            if (result.base != .Custom) break :blk null;
            break :blk switch (result.custom_type.?) {
                .written => |written| written,
                .ref => null,
            };
        };

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
        try checkZigStructs(self, record, sig, struct_path, &structs);

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
        const variable = try scope.createValueBinding(sig.name, type_info, true);
        variable.used = true;
    }
    if (error_sets.items.len > 0) unit.error_sets = try error_sets.toOwnedSlice();
    if (structs.items.len > 0) unit.structs = try structs.toOwnedSlice(self.allocator);
}

/// What a signature's result carries: the type itself, or a fallible
/// result's payload.
fn resultOf(return_type: *const ast.TypeInfo) ?*const ast.TypeInfo {
    if (return_type.base != .Union) return return_type;
    for (return_type.union_type.?.types) |member| {
        if (member.base != .Enum) return member;
    }
    return null;
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

/// The struct rules of an inline-Zig signature (`plan/inline-zig-structs.md`):
/// a `DoxaStruct_<path>` crosses only as a result, names a struct, and that
/// struct is plain data the wrapper can fill.
fn checkZigStructs(self: *SemanticAnalyzer, record: *ModuleRecord, sig: *const ast.ZigFnSig, spelling: ?[]const u8, structs: *std.ArrayListUnmanaged(graph_mod.ZigStruct)) ErrorList!void {
    var check: StructCheck = .{ .self = self, .record = record, .sig = sig, .structs = structs };
    defer check.path.deinit(self.allocator);
    for (sig.param_types) |*param| {
        if (refOf(param) != null and param.base == .Custom) return check.refuse("a struct parameter is not supported yet; pass its fields", .{});
    }
    const result = resultOf(&sig.return_type) orelse return;
    if (result.base != .Custom) return;
    const ref = refOf(result).?;
    try check.crossing(ref);
    const entry = for (structs.items) |*s| {
        if (s.ref.eql(ref)) break s;
    } else unreachable;
    const alias = spelling orelse return;
    for (entry.aliases) |known| {
        if (std.mem.eql(u8, known, alias)) return;
    }
    const aliases = try self.allocator.alloc([]const u8, entry.aliases.len + 1);
    @memcpy(aliases[0..entry.aliases.len], entry.aliases);
    aliases[entry.aliases.len] = alias;
    entry.aliases = aliases;
}

/// The type a resolved named type refers to.
fn refOf(type_info: *const ast.TypeInfo) ?TypeRef {
    return switch (type_info.custom_type orelse return null) {
        .ref => |r| r,
        .written => null,
    };
}

/// One signature's struct check: it refuses what cannot cross, and records
/// each struct that can, with its fields as they cross, for the wrapper.
const StructCheck = struct {
    self: *SemanticAnalyzer,
    record: *ModuleRecord,
    sig: *const ast.ZigFnSig,
    structs: *std.ArrayListUnmanaged(graph_mod.ZigStruct),
    /// The structs being checked around the current one. A struct that
    /// reaches itself by value is already E2034, so meeting one ends the walk
    /// without a second report.
    path: std.ArrayListUnmanaged(TypeRef) = .empty,

    /// `ref` is a struct whose every field the wrapper can fill. Recorded
    /// after the structs it holds, so the wrapper declares them first.
    fn crossing(check: *StructCheck, ref: TypeRef) ErrorList!void {
        const info = (try check.self.customType(ref)) orelse return error.UndefinedType;
        if (info.kind != .Struct) {
            return check.refuse("`DoxaStruct_{s}` names {s}, not a struct", .{ ref.name, kindName(info.kind) });
        }
        for (check.path.items) |outer| {
            if (outer.eql(ref)) return;
        }
        for (check.structs.items) |known| {
            if (known.ref.eql(ref)) return;
        }
        try check.path.append(check.self.allocator, ref);
        defer _ = check.path.pop();
        // The wrapper fills the struct on behalf of the file declaring the
        // block, so a private field is set only in a struct that file declares.
        const own = ref.module == check.record.owner.?;
        const declared = info.struct_fields orelse &.{};
        const fields = try check.self.allocator.alloc(graph_mod.ZigStructField, declared.len);
        for (declared, fields) |field, *out| {
            if (!field.is_public and !own) {
                return check.refuse("field '{s}' of struct '{s}' is private to another file, so the wrapper cannot set it", .{ field.name, ref.name });
            }
            const crosses = try check.fieldType(field.field_type_info) orelse {
                return check.refuse("field '{s}' of struct '{s}' has a type that cannot cross the boundary", .{ field.name, ref.name });
            };
            out.* = .{ .name = field.name, .type_info = crosses };
        }
        try check.structs.append(check.self.allocator, .{ .aliases = &.{}, .ref = ref, .fields = fields });
    }

    /// A field's type as it crosses (`ZigStructField.type_info`): a scalar,
    /// `string`, an enum, an array of those, or a struct that itself crosses
    /// (checked, and reported, recursively). Null when it cannot cross.
    fn fieldType(check: *StructCheck, t: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
        switch (t.base) {
            .Int, .Float, .Byte, .Tetra, .String, .Enum => return t.*,
            .Array => return check.arrayType(t),
            .Custom, .Struct => {
                const ref = refOf(t) orelse return null;
                const info = (try check.self.customType(ref)) orelse return null;
                switch (info.kind) {
                    .Enum => return .{ .base = .Enum, .is_mutable = false, .custom_type = .{ .ref = ref } },
                    .Group => return null,
                    .Struct => {
                        try check.crossing(ref);
                        return .{ .base = .Custom, .is_mutable = false, .custom_type = .{ .ref = ref } };
                    },
                }
            },
            else => return null,
        }
    }

    /// An array as it crosses: its elements, at any depth, are scalars,
    /// `string`s or enums. Structs do not cross inside arrays in this phase.
    fn arrayType(check: *StructCheck, array: *const ast.TypeInfo) ErrorList!?ast.TypeInfo {
        const element = array.array_type.?;
        const crossed: ast.TypeInfo = switch (element.base) {
            .Int, .Float, .Byte, .String, .Enum => element.*,
            .Array => try check.arrayType(element) orelse return null,
            .Custom => blk: {
                const ref = refOf(element) orelse return null;
                const info = (try check.self.customType(ref)) orelse return null;
                if (info.kind != .Enum) return null;
                break :blk .{ .base = .Enum, .is_mutable = false, .custom_type = .{ .ref = ref } };
            },
            else => return null,
        };
        const element_ptr = try check.self.allocator.create(ast.TypeInfo);
        element_ptr.* = crossed;
        return .{ .base = .Array, .is_mutable = false, .array_type = element_ptr };
    }

    fn refuse(check: *StructCheck, comptime fmt: []const u8, args: anytype) ErrorList!void {
        return refuseZigStruct(check.self, check.record, check.sig, fmt, args);
    }
};

fn kindName(kind: Types.CustomTypeKind) []const u8 {
    return switch (kind) {
        .Struct => "a struct",
        .Enum => "an enum",
        .Group => "a group",
    };
}

fn refuseZigStruct(self: *SemanticAnalyzer, record: *ModuleRecord, sig: *const ast.ZigFnSig, comptime fmt: []const u8, args: anytype) ErrorList!void {
    var message: std.Io.Writer.Allocating = .init(self.allocator);
    defer message.deinit();
    message.writer.print(fmt, args) catch return error.OutOfMemory;
    self.reporter.reportCompileError(
        record.zig.?.location,
        ErrorCode.NOT_IMPLEMENTED,
        "inline zig '{s}.{s}': {s}",
        .{ record.zig.?.name, sig.name, message.written() },
    );
    self.fatal_error = true;
    return error.UnsupportedType;
}

pub fn resolveZigType(self: *SemanticAnalyzer, record: *ModuleRecord, type_info: *ast.TypeInfo) ErrorList!void {
    const owner = record.owner orelse {
        if (hasWrittenType(type_info)) {
            self.reporter.reportCompileError(
                record.zig.?.location,
                ErrorCode.UNKNOWN_TYPE,
                "Zig module '{s}' names a Doxa type; a `.zig` file has no Doxa bindings, so `DoxaEnum_<Name>` and `DoxaStruct_<Name>` are only available in an inline `zig` block",
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
