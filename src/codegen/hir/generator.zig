//! Lowers the analyzed program to the register HIR (`plan/register-hir.md`).
//!
//! The generator walks each function's analyzed AST once and emits into a
//! `FunctionBuilder`. A local is an SSA variable unless it is lent as `^`, in
//! which case it is a slot; merges are block parameters. Every allocation
//! names the arena it is made in: a function's body arena (a child of its
//! `@caller`), a loop's arenas, or the root arena for top-level code. A heap
//! value stored somewhere that may outlive it is passed through `rehome` (or
//! `clone`); the arena pass (`register/arenas.zig`) decides which of those a
//! static region makes unnecessary.
//!
//! Types are the analyzer's (`plan/type-authority.md`): an expression's type
//! is read through `typeOf`, a store's through `bindingTypeOf`, and every
//! value is brought to the type of its destination by `convert`.

const std = @import("std");
const ast = @import("../../ast/ast.zig");
const Reporting = @import("../../utils/reporting.zig");
const Reporter = Reporting.Reporter;
const Location = Reporting.Location;
const Errors = @import("../../utils/errors.zig");
const ErrorList = Errors.ErrorList;
const semantic_module = @import("../../analysis/semantic/semantic.zig");
const SemanticAnalyzer = semantic_module.SemanticAnalyzer;
const StoreTarget = semantic_module.StoreTarget;
const Resolution = @import("../../analysis/semantic/resolution.zig").Resolution;
const module_graph = @import("../../module/graph.zig");
const inline_zig = @import("../../inline_zig/compiler.zig");
const TypeSystem = @import("type_system.zig").TypeSystem;
const ParamMutation = @import("param_mutation.zig");
const ir = @import("register/ir.zig");
const builder_mod = @import("register/builder.zig");
const hir_print = @import("register/print.zig");
const lower_expr = @import("lower_expr.zig");
const lower_control = @import("lower_control.zig");

const FunctionBuilder = builder_mod.FunctionBuilder;
const Var = builder_mod.Var;
pub const HIRType = ir.HIRType;
const Type = ir.Type;
const ValueId = ir.ValueId;
const BlockId = ir.BlockId;
const TypeRef = ast.TypeRef;

pub const Error = std.mem.Allocator.Error || ErrorList || builder_mod.Error;

/// A Doxa function or method of the program, as a call site and the lowering
/// of its body see it.
const FunctionDecl = struct {
    link_name: []const u8,
    module: module_graph.ModuleId,
    /// The struct whose instance method this is; its receiver is parameter 1.
    receiver: ?ir.StructId,
    params: []const ast.FunctionParam,
    /// The analyzed type of each declared parameter.
    param_types: []const HIRType,
    /// Whether the body writes through each by-value parameter, so it must
    /// own a copy (`param_mutation.zig`).
    param_mutated: []const bool,
    body: []ast.Stmt,
    signature: ir.Signature,
    is_entry: bool,
};

/// The program-level half of lowering: function, global and inline-Zig tables
/// shared by every function, and the module the lowering produces.
pub const Generator = struct {
    /// An arena: the lowered module lives as long as it.
    alloc: std.mem.Allocator,
    reporter: *Reporter,
    semantic: *const SemanticAnalyzer,
    graph: *module_graph.ModuleGraph,
    type_system: TypeSystem,

    functions: std.ArrayListUnmanaged(FunctionDecl) = .empty,
    function_ids: std.HashMapUnmanaged(module_graph.SymbolKey, ir.FunctionId, module_graph.SymbolKeyContext, std.hash_map.default_max_load_percentage) = .empty,
    zig_functions: std.ArrayListUnmanaged(ZigDecl) = .empty,
    zig_ids: std.StringHashMapUnmanaged(ir.ZigFunctionId) = .empty,
    globals: std.ArrayListUnmanaged(ir.Global) = .empty,
    global_ids: std.StringHashMapUnmanaged(ir.GlobalId) = .empty,

    /// Struct types that reach a reflection site (`"{x}"`, `@string`, a peek)
    /// keep their runtime descriptor; a group or unresolved reflection target
    /// keeps every one (`force_struct_descriptors`).
    reflected_structs: std.StringHashMapUnmanaged(void) = .empty,
    force_struct_descriptors: bool = false,

    const ZigDecl = struct {
        link_name: []const u8,
        sig: *const ast.ZigFnSig,
        signature: ir.Signature,
    };

    /// A generator for the analyzed program. The module graph's link names
    /// are final, and every global site is given its global's link name.
    pub fn init(alloc: std.mem.Allocator, reporter: *Reporter, semantic: *SemanticAnalyzer) !Generator {
        var self = Generator{
            .alloc = alloc,
            .reporter = reporter,
            .semantic = semantic,
            .graph = semantic.graph,
            .type_system = TypeSystem.init(alloc, reporter, semantic, &semantic.union_table),
        };
        try self.bindGlobalLinkNames();
        return self;
    }

    /// Give every declaration of and reference to a module-level global the
    /// global's link name, so two modules' same-named globals can never share
    /// storage, and a namespace member access (`ns.counter`) is a plain
    /// reference to the global it resolved to.
    fn bindGlobalLinkNames(self: *Generator) !void {
        for (self.semantic.global_sites.values()) |site| {
            const link = try self.graph.mangle(self.alloc, site.symbol.module, .global, &.{site.symbol.name});
            switch (site.place) {
                .name => |token| token.lexeme = link,
                .member => |expr| {
                    var tok = expr.data.FieldAccess.field;
                    tok.lexeme = link;
                    expr.data = .{ .Variable = tok };
                },
            }
        }
    }

    pub fn generate(self: *Generator) Error!ir.Module {
        const records = try self.programRecords();
        for (records) |record| try self.collectSignatures(record);

        const init_id: ir.FunctionId = @enumFromInt(self.functions.items.len);
        const functions = try self.alloc.alloc(ir.Function, self.functions.items.len + 1);
        for (self.functions.items, 0..) |_, i| {
            functions[i] = try self.lowerFunction(@enumFromInt(i));
        }
        functions[@intFromEnum(init_id)] = try self.lowerInit(records);

        return .{
            .program = try self.program(init_id),
            .functions = functions,
            .zig_takes_arena = try self.zigTakesArena(),
            .init = init_id,
            .reflected_structs = try self.reflectedStructs(),
            .force_struct_descriptors = self.force_struct_descriptors,
        };
    }

    /// The Doxa records whose code is part of the program, in stable-key
    /// order, so the output never depends on the order modules were found in.
    fn programRecords(self: *Generator) ![]*module_graph.ModuleRecord {
        var records: std.ArrayListUnmanaged(*module_graph.ModuleRecord) = .empty;
        for (self.graph.records.items) |record| {
            if (record.kind != .doxa or record.status != .Analyzed) continue;
            try records.append(self.alloc, record);
        }
        std.sort.pdq(*module_graph.ModuleRecord, records.items, {}, struct {
            fn lessThan(_: void, a: *module_graph.ModuleRecord, b: *module_graph.ModuleRecord) bool {
                return std.mem.lessThan(u8, a.stable_module_key, b.stable_module_key);
            }
        }.lessThan);
        return records.items;
    }

    // ── Signatures ──

    fn collectSignatures(self: *Generator, record: *module_graph.ModuleRecord) Error!void {
        for (record.statements()) |*stmt| {
            switch (stmt.data) {
                .FunctionDecl => try self.registerFunction(record.id, stmt),
                .Expression => |maybe| if (maybe) |expr| {
                    if (expr.data == .StructDecl) try self.registerMethods(record.id, &expr.data.StructDecl);
                },
                else => {},
            }
        }
    }

    fn registerFunction(self: *Generator, module: module_graph.ModuleId, stmt: *ast.Stmt) Error!void {
        const func = &stmt.data.FunctionDecl;
        const variable = self.semantic.moduleScope(module).lookupLocalVariable(func.name.lexeme).?;
        const storage = self.semantic.memory.scope_manager.value_storage.get(variable.storage_id).?;
        const signature = storage.type_info.function_type.?;

        // A function without `returns` takes the analyzer's inferred type.
        var ret = try self.lowerType(&func.return_type_info);
        if (func.return_type_info.base == .Nothing) {
            if (self.semantic.function_return_types.get(stmt.base.id)) |inferred| ret = try self.lowerType(inferred);
        }
        try self.addFunction(.{ .module = module, .name = func.name.lexeme }, .{
            .link_name = try self.graph.mangle(self.alloc, module, .function, &.{func.name.lexeme}),
            .module = module,
            .receiver = null,
            .params = func.params,
            .param_types = try self.lowerTypes(signature.params),
            .param_mutated = try self.mutatedParams(func.body, func.params),
            .body = func.body,
            .signature = undefined,
            .is_entry = func.is_entry,
        }, ret);
    }

    fn registerMethods(self: *Generator, module: module_graph.ModuleId, decl: *ast.StructDecl) Error!void {
        const ref = TypeRef{ .module = module, .name = decl.name.lexeme };
        const methods = self.semantic.struct_methods.get(ref).?;
        const struct_id = self.semantic.struct_table.idOf(ref).?;
        for (decl.methods) |method| {
            const info = methods.get(method.name.lexeme).?;
            try self.addFunction(.{ .module = module, .name = try methodKeyName(self.alloc, decl.name.lexeme, method.name.lexeme) }, .{
                .link_name = try self.graph.mangle(self.alloc, module, .method, &.{ decl.name.lexeme, method.name.lexeme }),
                .module = module,
                .receiver = if (method.is_static) null else struct_id,
                .params = method.params,
                .param_types = try self.lowerTypes(info.signature.params),
                .param_mutated = try self.mutatedParams(method.body, method.params),
                .body = method.body,
                .signature = undefined,
                .is_entry = false,
            }, try self.lowerType(info.signature.return_type));
        }
    }

    /// Register `decl` under `key`, with the signature a call site passes:
    /// `@caller`, the receiver of a method, then each parameter — a `^`
    /// parameter as its address and the arena that owns it.
    fn addFunction(self: *Generator, key: module_graph.SymbolKey, decl: FunctionDecl, ret: HIRType) Error!void {
        var params: std.ArrayListUnmanaged(Type) = .empty;
        var roles: std.ArrayListUnmanaged(ir.ParamRole) = .empty;
        try params.append(self.alloc, .arena);
        try roles.append(self.alloc, .caller_arena);
        if (decl.receiver) |sid| {
            try params.append(self.alloc, .of(.{ .Struct = sid }));
            try roles.append(self.alloc, .value);
        }
        for (decl.params, decl.param_types) |param, t| {
            if (param.is_alias) {
                try params.append(self.alloc, .{ .ref = t });
                try roles.append(self.alloc, .{ .alias = .{ .arena = @intCast(params.items.len) } });
                try params.append(self.alloc, .arena);
                try roles.append(self.alloc, .alias_arena);
            } else {
                try params.append(self.alloc, .of(t));
                try roles.append(self.alloc, .value);
            }
        }
        var full = decl;
        full.signature = .{ .params = params.items, .roles = roles.items, .ret = ret };
        const id: ir.FunctionId = @enumFromInt(self.functions.items.len);
        try self.functions.append(self.alloc, full);
        try self.function_ids.put(self.alloc, key, id);
    }

    fn mutatedParams(self: *Generator, body: []const ast.Stmt, params: []const ast.FunctionParam) ![]const bool {
        const out = try self.alloc.alloc(bool, params.len);
        for (params, out) |param, *m| m.* = ParamMutation.bodyMutatesVariable(body, param.name.lexeme);
        return out;
    }

    fn lowerTypes(self: *Generator, infos: []const ast.TypeInfo) Error![]const HIRType {
        const out = try self.alloc.alloc(HIRType, infos.len);
        for (infos, out) |*info, *t| t.* = try self.lowerType(info);
        return out;
    }

    // ── Callees ──

    pub const Callee = union(enum) {
        doxa: ir.FunctionId,
        zig: ir.ZigFunctionId,
    };

    /// The Doxa function or method `key` names.
    pub fn functionId(self: *Generator, key: module_graph.SymbolKey) ir.FunctionId {
        return self.function_ids.get(key).?;
    }

    pub fn methodId(self: *Generator, method: Resolution.Method) Error!ir.FunctionId {
        return self.functionId(.{ .module = method.owner.module, .name = try methodKeyName(self.alloc, method.owner.name, method.name) });
    }

    pub fn functionDecl(self: *const Generator, id: ir.FunctionId) *const FunctionDecl {
        return &self.functions.items[@intFromEnum(id)];
    }

    /// The wrapper the inline-Zig compiler exports for `symbol`, registered
    /// the first time a call names it.
    pub fn zigId(self: *Generator, symbol: module_graph.SymbolRef) Error!ir.ZigFunctionId {
        const link_name = try self.graph.mangle(self.alloc, symbol.module, .function, &.{symbol.name});
        if (self.zig_ids.get(link_name)) |id| return id;
        const sig = self.graph.declOf(symbol).?.zig_function;
        var params: std.ArrayListUnmanaged(Type) = .empty;
        var roles: std.ArrayListUnmanaged(ir.ParamRole) = .empty;
        if (inline_zig.takesArena(sig)) {
            try params.append(self.alloc, .arena);
            try roles.append(self.alloc, .caller_arena);
        }
        for (sig.param_types) |*param| {
            try params.append(self.alloc, .of(try self.lowerType(param)));
            try roles.append(self.alloc, .value);
        }
        const id: ir.ZigFunctionId = @enumFromInt(self.zig_functions.items.len);
        try self.zig_functions.append(self.alloc, .{
            .link_name = link_name,
            .sig = sig,
            .signature = .{ .params = params.items, .roles = roles.items, .ret = try self.lowerType(&sig.return_type) },
        });
        try self.zig_ids.put(self.alloc, link_name, id);
        return id;
    }

    pub fn zigSignature(self: *const Generator, id: ir.ZigFunctionId) ir.Signature {
        return self.zig_functions.items[@intFromEnum(id)].signature;
    }

    // ── Globals ──

    /// The global a module-level binding names, by its link name and the
    /// type of its storage.
    pub fn globalId(self: *Generator, link_name: []const u8, ty: HIRType) Error!ir.GlobalId {
        if (self.global_ids.get(link_name)) |id| return id;
        const id: ir.GlobalId = @enumFromInt(self.globals.items.len);
        try self.globals.append(self.alloc, .{ .name = link_name, .ty = ty });
        try self.global_ids.put(self.alloc, link_name, id);
        return id;
    }

    // ── Functions ──

    fn lowerFunction(self: *Generator, id: ir.FunctionId) Error!ir.Function {
        const decl = self.functionDecl(id).*;
        var l = try Lowering.init(self, decl.link_name, decl.signature.ret, decl.module);

        // Parameters, in the signature's order.
        const caller = try l.b.addParam(.arena, .caller_arena);
        var values = try self.alloc.alloc(ValueId, decl.signature.params.len);
        values[0] = caller;
        for (decl.signature.params[1..], decl.signature.roles[1..], 1..) |ty, role, i| values[i] = try l.b.addParam(ty, role);

        l.caller = caller;
        try l.start();
        try lower_control.collectLent(&l, decl.body);
        const body = try l.b.define(.{ .scope_enter = .{ .parent = caller } }, .arena);
        try l.pushArena(body, .owned);

        var next: usize = 1;
        if (decl.receiver != null) {
            l.this = values[1];
            next = 2;
        }
        for (decl.params, decl.param_types, decl.param_mutated) |param, ty, mutated| {
            const storage = try self.paramStorage(param);
            if (param.is_alias) {
                try l.locals.put(self.alloc, storage, .{ .alias = .{ .ref = values[next], .arena = values[next + 1] } });
                next += 2;
            } else {
                var value = values[next];
                // A by-value heap parameter the body writes through is copied,
                // so the caller's object is not changed (`docs/alias.md`).
                if (mutated and ir.isHeapType(ty)) value = try l.b.define(.{ .clone = .{ .arena = body, .operand = value } }, .of(ty));
                try l.declareLocal(storage, param.name.lexeme, ty, value);
                next += 1;
            }
        }

        try lower_control.functionBody(&l, decl.body);
        return l.finish();
    }

    /// `__doxa_init`: the imported records' globals, in stable-key order, then
    /// the entry record's top-level code, then the entry function. Top-level
    /// code allocates in the root arena, where globals live.
    fn lowerInit(self: *Generator, records: []const *module_graph.ModuleRecord) Error!ir.Function {
        const entry = self.semantic.entry_module;
        var l = try Lowering.init(self, "__doxa_init", .Nothing, entry);
        try l.start();
        try l.pushArena(l.root, .borrowed);
        try l.pushDefers();

        for (records) |record| {
            if (record.id == entry) continue;
            l.module = record.id;
            for (record.statements()) |stmt| {
                if (stmt.data != .VarDecl) continue;
                if (!try lower_control.statement(&l, stmt)) return l.finish();
            }
        }
        l.module = entry;
        for (self.graph.record(entry).statements()) |stmt| {
            if (!try lower_control.statement(&l, stmt)) return l.finish();
        }

        for (self.functions.items, 0..) |decl, i| {
            if (!decl.is_entry) continue;
            _ = try l.call(@enumFromInt(i), &.{l.root});
            break;
        }
        if (!try l.popDefers()) return l.finish();
        try l.b.ret(null);
        return l.finish();
    }

    // ── The module ──

    fn program(self: *Generator, init_id: ir.FunctionId) Error!ir.Program {
        const count = self.functions.items.len + 1;
        const signatures = try self.alloc.alloc(ir.Signature, count);
        const names = try self.alloc.alloc([]const u8, count);
        for (self.functions.items, signatures[0..self.functions.items.len], names[0..self.functions.items.len]) |decl, *sig, *name| {
            sig.* = decl.signature;
            name.* = decl.link_name;
        }
        signatures[@intFromEnum(init_id)] = .{ .params = &.{}, .roles = &.{}, .ret = .Nothing };
        names[@intFromEnum(init_id)] = "__doxa_init";

        const zig_sigs = try self.alloc.alloc(ir.Signature, self.zig_functions.items.len);
        const zig_names = try self.alloc.alloc([]const u8, self.zig_functions.items.len);
        for (self.zig_functions.items, zig_sigs, zig_names) |decl, *sig, *name| {
            sig.* = decl.signature;
            name.* = decl.link_name;
        }

        const structs = &self.semantic.struct_table;
        const struct_count = structs.entries.items.len + 1;
        const struct_fields = try self.alloc.alloc([]const HIRType, struct_count);
        const struct_names = try self.alloc.alloc([]const u8, struct_count);
        const struct_keys = try self.alloc.alloc([]const u8, struct_count);
        const struct_field_names = try self.alloc.alloc([]const []const u8, struct_count);
        // `StructId` 0 is no struct.
        struct_fields[0] = &.{};
        struct_names[0] = "?struct";
        struct_keys[0] = "";
        struct_field_names[0] = &.{};
        for (structs.entries.items) |entry| {
            const types = try self.alloc.alloc(HIRType, entry.fields.len);
            const field_names = try self.alloc.alloc([]const u8, entry.fields.len);
            for (entry.fields, types, field_names) |field, *t, *field_name| {
                t.* = field.hir_type;
                field_name.* = field.name;
            }
            struct_fields[entry.id] = types;
            struct_names[entry.id] = entry.ref.name;
            struct_keys[entry.id] = entry.key.?;
            struct_field_names[entry.id] = field_names;
        }

        const enums = &self.semantic.enum_table;
        const enum_names = try self.alloc.alloc([]const u8, enums.entries.items.len);
        const enum_keys = try self.alloc.alloc([]const u8, enums.entries.items.len);
        const enum_variants = try self.alloc.alloc([]const []const u8, enums.entries.items.len);
        for (enums.entries.items, enum_names, enum_keys, enum_variants) |entry, *name, *key, *variants| {
            name.* = entry.ref.name;
            key.* = entry.key.?;
            const variant_names = try self.alloc.alloc([]const u8, entry.variants.len);
            for (entry.variants) |v| variant_names[v.index] = v.name;
            variants.* = variant_names;
        }

        const groups = &self.semantic.group_table;
        const group_members = try self.alloc.alloc([]const HIRType, groups.entries.items.len);
        const group_names = try self.alloc.alloc([]const u8, groups.entries.items.len);
        for (groups.entries.items, group_members, group_names) |entry, *members, *name| {
            const types = try self.alloc.alloc(HIRType, entry.members.len);
            for (entry.members, types) |member, *t| t.* = groupMemberType(member);
            members.* = types;
            name.* = entry.ref.name;
        }

        return .{
            .functions = signatures,
            .function_names = names,
            .zig_functions = zig_sigs,
            .zig_function_names = zig_names,
            .globals = self.globals.items,
            .struct_fields = struct_fields,
            .group_members = group_members,
            .struct_names = struct_names,
            .enum_names = enum_names,
            .group_names = group_names,
            .struct_keys = struct_keys,
            .enum_keys = enum_keys,
            .struct_field_names = struct_field_names,
            .enum_variants = enum_variants,
        };
    }

    fn zigTakesArena(self: *Generator) Error![]const bool {
        const out = try self.alloc.alloc(bool, self.zig_functions.items.len);
        for (self.zig_functions.items, out) |decl, *takes| takes.* = inline_zig.takesArena(decl.sig);
        return out;
    }

    fn reflectedStructs(self: *Generator) Error![]const []const u8 {
        const out = try self.alloc.alloc([]const u8, self.reflected_structs.count());
        var it = self.reflected_structs.keyIterator();
        var i: usize = 0;
        while (it.next()) |key| : (i += 1) out[i] = key.*;
        std.sort.pdq([]const u8, out, {}, struct {
            fn lessThan(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.lessThan(u8, a, b);
            }
        }.lessThan);
        return out;
    }

    /// Record that a value of type `t` reaches a reflection site, through the
    /// containers it can be printed as part of.
    pub fn markReflected(self: *Generator, t: HIRType) Error!void {
        switch (t) {
            .Struct => |sid| {
                const key = self.semantic.struct_table.keyOf(sid) orelse {
                    self.force_struct_descriptors = true;
                    return;
                };
                try self.reflected_structs.put(self.alloc, key, {});
            },
            .Array => |a| try self.markReflected(a.element.*),
            .Map => |m| {
                try self.markReflected(m.key.*);
                try self.markReflected(m.value.*);
            },
            .Union => |u| for (u.members) |m| try self.markReflected(m.*),
            .Group, .Unknown, .Poison => self.force_struct_descriptors = true,
            .Int, .Byte, .Float, .String, .Tetra, .Nothing, .Enum, .Function => {},
        }
    }

    // ── The analyzer's answers ──

    pub fn lowerType(self: *const Generator, info: *const ast.TypeInfo) Error!HIRType {
        return self.type_system.lowerType(info);
    }

    pub fn typeForRef(self: *const Generator, ref: TypeRef) Error!HIRType {
        return self.type_system.typeForRef(ref);
    }

    /// The analyzer's type for `expr` as written: the names `@type` or a
    /// peek shows. Every read of the analyzer's per-node types in codegen goes
    /// through here or `storeTarget`.
    pub fn typeInfoOf(self: *const Generator, expr: *const ast.Expr) Error!*const ast.TypeInfo {
        if (self.semantic.getCachedExprType(expr)) |info| return info;
        const location = expr.base.location();
        self.reporter.reportInternal(
            "no analyzed type for the {s} expression at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ @tagName(std.meta.activeTag(expr.data)), location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingExpressionType;
    }

    /// The type of `expr`, as the analyzer inferred it, with narrowing
    /// applied to this occurrence.
    pub fn typeOf(self: *const Generator, expr: *const ast.Expr) Error!HIRType {
        return self.lowerType(try self.typeInfoOf(expr));
    }

    /// The storage a name, an assignment or a declaration at `node` denotes.
    pub fn storeTarget(self: *const Generator, node: *const ast.Base) Error!StoreTarget {
        if (self.semantic.getStoreTarget(node.id)) |target| return target;
        const location = node.location();
        self.reporter.reportInternal(
            "no analyzed binding for the store at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ location.file, location.range.start_line, location.range.start_col },
            @src(),
        );
        return ErrorList.MissingBindingType;
    }

    pub fn resolutionOf(self: *const Generator, expr: *const ast.Expr) ?Resolution {
        return self.semantic.resolutionOf(expr);
    }

    pub fn paramStorage(self: *const Generator, param: ast.FunctionParam) Error!u32 {
        if (param.storage) |storage| return storage;
        self.reporter.reportInternal(
            "no analyzed binding for parameter '{s}' at {s}:{d}:{d}. This is a compiler bug, not an error in the program",
            .{ param.name.lexeme, param.name.file, param.name.line, param.name.column },
            @src(),
        );
        return ErrorList.MissingBindingType;
    }

    /// The members a box of type `boxed` holds, in the order its member index
    /// names them.
    pub fn boxMembers(self: *const Generator, boxed: HIRType) Error![]const HIRType {
        switch (boxed) {
            .Union => |u| {
                const out = try self.alloc.alloc(HIRType, u.members.len);
                for (u.members, out) |m, *t| t.* = m.*;
                return out;
            },
            .Group => |gid| {
                const members = self.semantic.group_table.members(gid) orelse &.{};
                const out = try self.alloc.alloc(HIRType, members.len);
                for (members, out) |m, *t| t.* = groupMemberType(m);
                return out;
            },
            else => unreachable, // only a union or a group is boxed
        }
    }

    /// Whether a box member of type `member` is a value of `named`: the type
    /// itself, or for a group, one of the members it flattened into.
    pub fn memberIsNamed(self: *const Generator, member: HIRType, named: HIRType) bool {
        if (member.eql(named)) return true;
        if (named != .Group) return false;
        for (self.semantic.group_table.members(named.Group) orelse &.{}) |group_member| {
            if (member.eql(groupMemberType(group_member))) return true;
        }
        return false;
    }

    /// The indexes of every member of `boxed` that is a `named`.
    pub fn membersNamed(self: *const Generator, boxed: HIRType, named: HIRType) Error![]const u32 {
        var out: std.ArrayListUnmanaged(u32) = .empty;
        for (try self.boxMembers(boxed), 0..) |member, i| {
            if (self.memberIsNamed(member, named)) try out.append(self.alloc, @intCast(i));
        }
        return out.items;
    }

    pub fn report(self: *const Generator, location: Location, code: []const u8, comptime fmt: []const u8, args: anytype) void {
        self.reporter.reportCompileError(location, code, fmt, args);
    }
};

/// The type a group's flattened member names.
pub fn groupMemberType(member: anytype) HIRType {
    return switch (member.kind) {
        .Enum => .{ .Enum = member.id },
        .Struct => .{ .Struct = member.id },
        .Group => .{ .Group = member.id },
    };
}

/// The internal key name of a method: `Struct.method`. `.` cannot occur in an
/// identifier, so a method key never meets a top-level function's.
fn methodKeyName(alloc: std.mem.Allocator, struct_name: []const u8, method_name: []const u8) ![]const u8 {
    return std.fmt.allocPrint(alloc, "{s}.{s}", .{ struct_name, method_name });
}

/// What a local binding of the function being lowered is.
pub const Local = union(enum) {
    /// An SSA variable, and the arena its heap value must live in: the arena
    /// innermost where it was declared.
    ssa: struct { v: Var, home: Var },
    /// A local lent as `^`: addressable storage.
    slot: ir.SlotId,
    /// A `^` parameter: its address and the arena that owns it.
    alias: struct { ref: ValueId, arena: ValueId },
};

/// An open arena of the function being lowered, innermost last.
const ArenaFrame = struct {
    /// The arena's current value: a loop body arena changes each iteration.
    v: Var,
    /// Whether this function opened it, and must close it on every exit.
    owned: bool,
};

pub const Ownership = enum { owned, borrowed };

/// A loop being lowered: where `break` and `continue` go, and how many
/// arenas and defer frames are open outside it.
pub const LoopContext = struct {
    break_block: BlockId,
    continue_block: BlockId,
    arena_depth: usize,
    defer_depth: usize,
};

/// The lowering of one function: the builder, and the generator's view of
/// its locals, open arenas, loops and pending `defer`s.
pub const Lowering = struct {
    g: *Generator,
    b: FunctionBuilder,
    module: module_graph.ModuleId,
    ret: HIRType,
    /// `@caller`; null for `__doxa_init`, which runs in the root arena.
    caller: ?ValueId = null,
    /// `@root`, defined at the top of the entry block by `start`.
    root: ValueId = undefined,
    this: ?ValueId = null,
    locals: std.AutoHashMapUnmanaged(u32, Local) = .empty,
    /// Locals lent as `^` somewhere in the body, which are slots.
    lent: std.AutoHashMapUnmanaged(u32, void) = .empty,
    arenas: std.ArrayListUnmanaged(ArenaFrame) = .empty,
    loops: std.ArrayListUnmanaged(LoopContext) = .empty,
    defers: std.ArrayListUnmanaged(std.ArrayListUnmanaged(*ast.Expr)) = .empty,

    pub const expr = lower_expr.expr;
    pub const cond = lower_expr.cond;
    pub const call = lower_expr.callDoxa;

    fn init(g: *Generator, name: []const u8, ret: HIRType, module: module_graph.ModuleId) Error!Lowering {
        return .{ .g = g, .b = try FunctionBuilder.init(g.alloc, name, ret), .module = module, .ret = ret };
    }

    /// Begin the body, once every parameter is added.
    fn start(self: *Lowering) Error!void {
        self.root = try self.b.define(.root_arena, .arena);
    }

    fn finish(self: *Lowering) Error!ir.Function {
        return self.b.finish();
    }

    // ── Arenas ──

    pub fn pushArena(self: *Lowering, opened: ValueId, ownership: Ownership) Error!void {
        const v = try self.b.declareVar(.arena);
        try self.b.defVar(v, opened);
        try self.arenas.append(self.g.alloc, .{ .v = v, .owned = ownership == .owned });
    }

    pub fn popArena(self: *Lowering) void {
        _ = self.arenas.pop();
    }

    /// The innermost open arena: where the code being lowered allocates.
    pub fn arena(self: *Lowering) Error!ValueId {
        return self.b.useVar(self.arenas.items[self.arenas.items.len - 1].v);
    }

    pub fn innermostArenaVar(self: *const Lowering) Var {
        return self.arenas.items[self.arenas.items.len - 1].v;
    }

    /// Close every arena this function opened above `depth`, innermost
    /// first: the edge leaves them.
    pub fn exitArenas(self: *Lowering, depth: usize) Error!void {
        var i = self.arenas.items.len;
        while (i > depth) {
            i -= 1;
            const frame = self.arenas.items[i];
            if (!frame.owned) continue;
            try self.b.effect(.{ .scope_exit = .{ .arena = try self.b.useVar(frame.v) } });
        }
    }

    // ── Defers ──

    pub fn pushDefers(self: *Lowering) Error!void {
        try self.defers.append(self.g.alloc, .empty);
    }

    /// Pop the innermost defer frame and run its actions, last first. False
    /// when one of them leaves the block.
    pub fn popDefers(self: *Lowering) Error!bool {
        const frame = self.defers.pop().?;
        return self.runDeferred(frame.items);
    }

    pub fn addDefer(self: *Lowering, action: *ast.Expr) Error!void {
        try self.defers.items[self.defers.items.len - 1].append(self.g.alloc, action);
    }

    /// Run every pending defer above `depth`, innermost first: what an edge
    /// out of those frames does before it leaves.
    pub fn runDefersTo(self: *Lowering, depth: usize) Error!bool {
        var i = self.defers.items.len;
        while (i > depth) {
            i -= 1;
            if (!try self.runDeferred(self.defers.items[i].items)) return false;
        }
        return true;
    }

    fn runDeferred(self: *Lowering, actions: []const *ast.Expr) Error!bool {
        var j = actions.len;
        while (j > 0) {
            j -= 1;
            if (try self.expr(actions[j]) == null) return false;
        }
        return true;
    }

    // ── Locals ──

    /// Bind a new local of `ty`, initialized to `value`, in the innermost
    /// arena. A local lent as `^` is a slot; any other is an SSA variable.
    pub fn declareLocal(self: *Lowering, storage: u32, name: []const u8, ty: HIRType, value: ValueId) Error!void {
        if (self.lent.contains(storage)) {
            const home = try self.arena();
            const slot = try self.b.addSlot(ty, home, name);
            const ref = try self.b.define(.{ .slot_addr = slot }, .{ .ref = ty });
            try self.b.effect(.{ .store = .{ .ref = ref, .value = try self.rehome(home, value, ty) } });
            try self.locals.put(self.g.alloc, storage, .{ .slot = slot });
            return;
        }
        const v = try self.b.declareVar(.of(ty));
        try self.b.defVar(v, value);
        try self.locals.put(self.g.alloc, storage, .{ .ssa = .{ .v = v, .home = self.innermostArenaVar() } });
    }

    /// Read the storage `target` names, of its slot type.
    pub fn readStorage(self: *Lowering, target: StoreTarget, name: []const u8) Error!ValueId {
        const ty = try self.g.lowerType(target.slot);
        if (target.global) {
            return self.b.define(.{ .global_load = try self.g.globalId(name, ty) }, .of(ty));
        }
        return switch (self.local(target.storage)) {
            .ssa => |s| self.b.useVar(s.v),
            .slot => |slot| self.b.define(.{ .load = .{ .operand = try self.b.define(.{ .slot_addr = slot }, .{ .ref = ty }) } }, .of(ty)),
            .alias => |a| self.b.define(.{ .load = .{ .operand = a.ref } }, .of(ty)),
        };
    }

    /// Write `value`, already of the storage's slot type, into the storage
    /// `target` names. A heap value is re-homed into the arena that owns the
    /// storage.
    pub fn writeStorage(self: *Lowering, target: StoreTarget, name: []const u8, value: ValueId) Error!void {
        const ty = try self.g.lowerType(target.slot);
        if (target.global) {
            const id = try self.g.globalId(name, ty);
            try self.b.effect(.{ .global_store = .{ .global = id, .value = try self.rehome(self.root, value, ty) } });
            return;
        }
        switch (self.local(target.storage)) {
            .ssa => |s| try self.b.defVar(s.v, try self.rehome(try self.b.useVar(s.home), value, ty)),
            .slot => |slot| {
                const ref = try self.b.define(.{ .slot_addr = slot }, .{ .ref = ty });
                const home = self.b.slots.items[@intFromEnum(slot)].arena;
                try self.b.effect(.{ .store = .{ .ref = ref, .value = try self.rehome(home, value, ty) } });
            },
            .alias => |a| try self.b.effect(.{ .store = .{ .ref = a.ref, .value = try self.rehome(a.arena, value, ty) } }),
        }
    }

    /// The local `storage` names. Analysis bound every name lowering reads.
    pub fn local(self: *const Lowering, storage: u32) Local {
        return self.locals.get(storage).?;
    }

    /// `value` placed so it outlives `arena`: itself for a value that is not
    /// heap, else a `rehome` the arena pass removes where a static region
    /// already outlives `arena`.
    pub fn rehome(self: *Lowering, into: ValueId, value: ValueId, ty: HIRType) Error!ValueId {
        if (!ir.isHeapType(ty)) return value;
        return self.b.define(.{ .rehome = .{ .arena = into, .operand = value } }, .of(ty));
    }

    // ── Values ──

    pub fn constant(self: *Lowering, c: ir.Constant, ty: HIRType) Error!ValueId {
        return self.b.define(.{ .constant = c }, .of(ty));
    }

    pub fn nothing(self: *Lowering) Error!ValueId {
        return self.constant(.nothing, .Nothing);
    }

    pub fn define(self: *Lowering, op: ir.Op, ty: HIRType) Error!ValueId {
        return self.b.define(op, .of(ty));
    }

    pub fn effect(self: *Lowering, op: ir.Op) Error!void {
        return self.b.effect(op);
    }

    /// Bring `value`, of type `from`, to `to`: the type of the storage it
    /// lands in or of the expression it becomes. A member becoming a union or
    /// a group is boxed, a box of another box type is re-packed, a box a check
    /// proved is read as its member, a number is converted, and a fixed array
    /// admitted as a dynamic one is copied into a header (`array.from_fixed`).
    /// Anything else reaching here is a compiler bug.
    pub fn convert(self: *Lowering, value: ValueId, from: HIRType, to: HIRType) Error!ValueId {
        if (from.eql(to)) return value;
        if (to.isBoxed()) {
            const op: ir.Op = if (from.isBoxed()) .{ .repack = .{ .operand = value } } else .{ .box = .{ .operand = value } };
            return self.define(op, to);
        }
        if (from.isBoxed()) return self.define(.{ .unbox = .{ .operand = value } }, to);
        if (isNumber(from) and isNumber(to)) return self.define(.{ .convert = .{ .operand = value } }, to);
        if (from == .Array and to == .Array and from.Array.size != null and to.Array.size == null) {
            const element = try self.convertElements(from.Array.element.*, to.Array.element.*);
            if (element) return self.define(.{ .array_from_fixed = .{ .arena = try self.arena(), .operand = value } }, to);
        }
        const no_names = ir.Program{ .functions = &.{}, .function_names = &.{}, .zig_functions = &.{}, .zig_function_names = &.{}, .globals = &.{}, .struct_fields = &.{}, .group_members = &.{} };
        var from_text = std.Io.Writer.Allocating.init(self.g.alloc);
        var to_text = std.Io.Writer.Allocating.init(self.g.alloc);
        hir_print.writeHIRType(&from_text.writer, &no_names, from) catch {};
        hir_print.writeHIRType(&to_text.writer, &no_names, to) catch {};
        const at = self.b.location orelse Location{ .file = "?", .file_uri = null, .range = .{ .start_line = 0, .start_col = 0, .end_line = 0, .end_col = 0 } };
        self.g.reporter.reportInternal("cannot convert a {s} to a {s} at {s}:{d}:{d}. This is a compiler bug, not an error in the program", .{ from_text.written(), to_text.written(), at.file, at.range.start_line, at.range.start_col }, @src());
        return ErrorList.TypeMismatch;
    }

    /// Whether a fixed array's elements of `from` are the dynamic array's
    /// `to` once `array.from_fixed` copies them: equal, or a nested fixed
    /// array the copy turns dynamic.
    fn convertElements(self: *Lowering, from: HIRType, to: HIRType) Error!bool {
        if (from.eql(to)) return true;
        if (from == .Array and to == .Array and from.Array.size != null and to.Array.size == null) return self.convertElements(from.Array.element.*, to.Array.element.*);
        return false;
    }

    /// Lower `e` and bring it to `to`; null when control leaves it. An array
    /// literal is built as the array its context expects.
    pub fn exprAs(self: *Lowering, e: *ast.Expr, to: HIRType) Error!?ValueId {
        if (e.data == .Array and to == .Array) {
            const literal = try self.g.typeOf(e);
            if (literal == .Array and literal.Array.size == null and to.Array.size != null and to.Array.size.? == e.data.Array.len) {
                return lower_expr.arrayLiteralAs(self, e, to);
            }
        }
        const v = try self.expr(e) orelse return null;
        return try self.convert(v, try self.g.typeOf(e), to);
    }

    /// `t` with every fixed size dropped: the dynamic array a fixed one
    /// converts to.
    pub fn dynamicOf(self: *Lowering, t: HIRType) Error!HIRType {
        if (t != .Array) return t;
        const element = try self.g.alloc.create(HIRType);
        element.* = try self.dynamicOf(t.Array.element.*);
        return .{ .Array = .{ .element = element } };
    }

    pub fn location(self: *const Lowering) ?Location {
        return self.b.location;
    }
};

fn isNumber(t: HIRType) bool {
    return t == .Int or t == .Byte or t == .Float;
}
