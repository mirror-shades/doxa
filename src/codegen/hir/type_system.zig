const std = @import("std");
const ast = @import("../../ast/ast.zig");
const SoxaTypes = @import("types.zig");
const HIRType = SoxaTypes.HIRType;
const Errors = @import("../../utils/errors.zig");
const Reporting = @import("../../utils/reporting.zig");
const SemanticAnalyzer = @import("../../analysis/semantic/semantic.zig").SemanticAnalyzer;
const UnionTable = @import("../../common/union_table.zig").UnionTable;
const type_lowering = @import("../../common/type_lowering.zig");
const TypeLowering = type_lowering.TypeLowering;
const TypeLoweringError = type_lowering.Error;
const TypeRef = ast.TypeRef;

/// Codegen's view of the program's types. Named types are keyed by their
/// canonical key (`ModuleGraph.typeKey`), never by a spelling; an expression's
/// type is the analyzer's answer wherever the analyzer recorded one.
pub const TypeSystem = struct {
    allocator: std.mem.Allocator,
    reporter: *Reporting.Reporter,
    semantic: *const SemanticAnalyzer,
    /// The analyzer's union table: the one part of analysis codegen extends,
    /// since lowering may name a union analysis never spelled.
    unions: *UnionTable,

    /// The HIR type of a named type.
    pub fn init(allocator: std.mem.Allocator, reporter: *Reporting.Reporter, semantic: *const SemanticAnalyzer, unions: *UnionTable) TypeSystem {
        return TypeSystem{
            .allocator = allocator,
            .reporter = reporter,
            .semantic = semantic,
            .unions = unions,
        };
    }

    /// Lowering for codegen, which runs after every type is registered and
    /// only looks a named type's id up.
    fn lowering(self: *const TypeSystem) TypeLowering {
        return .{
            .allocator = self.allocator,
            .graph = self.semantic.graph,
            .names = self.semantic.unionNames(),
            .unions = self.unions,
            .assign = null,
        };
    }

    /// The HIR type of an analyzed type. Analysis hands codegen complete
    /// types; one it cannot lower is a compiler bug and fails the compile.
    pub fn lowerType(self: *const TypeSystem, type_info: *const ast.TypeInfo) Errors.ErrorList!HIRType {
        return self.lowering().lower(type_info) catch |err| self.loweringFailed(err, @tagName(type_info.base));
    }

    /// The type a named type's declaration declares.
    pub fn typeForRef(self: *const TypeSystem, ref: TypeRef) Errors.ErrorList!HIRType {
        return self.lowering().named(ref) catch |err| self.loweringFailed(err, ref.name);
    }

    fn loweringFailed(self: *const TypeSystem, err: TypeLoweringError, what: []const u8) Errors.ErrorList {
        if (err == error.OutOfMemory) return error.OutOfMemory;
        self.reporter.reportInternal(
            "cannot lower the analyzed type {s} ({s}). This is a compiler bug, not an error in the program",
            .{ what, @errorName(err) },
            @src(),
        );
        return Errors.ErrorList.IncompleteType;
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
};
