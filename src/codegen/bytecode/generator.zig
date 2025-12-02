const std = @import("std");
const module = @import("module.zig");
const hir_types = @import("../hir/soxa_types.zig");
const hir_instructions = @import("../hir/soxa_instructions.zig");

pub const BytecodeGenerator = struct {
    allocator: std.mem.Allocator,
    artifact_dir: []const u8,
    artifact_stem: []const u8,
    source_path: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator, artifact_dir: []const u8, artifact_stem: []const u8) BytecodeGenerator {
        return BytecodeGenerator{
            .allocator = allocator,
            .artifact_dir = artifact_dir,
            .artifact_stem = artifact_stem,
            .source_path = null,
        };
    }

    pub fn generate(self: *BytecodeGenerator, program: *const hir_types.HIRProgram) !module.BytecodeModule {
        var ctx = try Context.init(self.allocator, program);
        defer ctx.deinit();

        for (program.instructions) |inst| {
            try ctx.lowerInstruction(inst);
        }

        const instruction_slice = try ctx.takeInstructions();
        const functions = try self.lowerFunctions(program, &ctx);
        const modules = try ctx.takeModules();

        const artifact_path = try self.buildArtifactPath(program);

        var source_path_copy: ?[]const u8 = null;
        if (self.source_path) |path| {
            source_path_copy = try self.allocator.dupe(u8, path);
        }

        return module.BytecodeModule{
            .allocator = self.allocator,
            .spec_version = module.SpecVersion,
            .constants = program.constant_pool,
            .string_pool = program.string_pool,
            .instructions = instruction_slice,
            .functions = functions,
            .modules = modules,
            .artifact_path = artifact_path,
            .source_path = source_path_copy,
        };
    }

    fn processSoxaFile(self: *BytecodeGenerator, ctx: *Context) !void {
        const soxa_path = try std.fmt.allocPrint(self.allocator, "{s}/{s}.soxa", .{ self.artifact_dir, self.artifact_stem });
        defer self.allocator.free(soxa_path);

        const file = std.fs.cwd().openFile(soxa_path, .{}) catch {
            return;
        };
        defer file.close();

        var file_buf: [8192]u8 = undefined;
        const bytes_read = try file.readAll(&file_buf);
        const file_content = file_buf[0..bytes_read];

        var load_alias_count: u32 = 0;
        var store_alias_count: u32 = 0;

        var start: usize = 0;
        while (start < file_content.len) {
            const newline_pos = std.mem.indexOf(u8, file_content[start..], "\n") orelse file_content.len;
            const line_end = start + newline_pos;
            const line = file_content[start..line_end];
            start = line_end + 1;

            if (std.mem.startsWith(u8, line, "    LoadAlias")) {
                load_alias_count += 1;

                // Parse LoadAlias instruction: "    LoadAlias 1 \"param\""
                // Simple approach: find "LoadAlias " and extract the number after it
                const loadalias_start = std.mem.indexOf(u8, line, "LoadAlias ") orelse continue;
                const after_loadalias = line[loadalias_start + 9 ..]; // Skip "LoadAlias "
                // Skip leading whitespace
                const trimmed = std.mem.trim(u8, after_loadalias, " ");
                // Find the first space after the number
                const space_pos = std.mem.indexOf(u8, trimmed, " ") orelse trimmed.len;
                const slot_str = trimmed[0..space_pos];
                const slot = std.fmt.parseInt(u32, slot_str, 10) catch continue;

                // Generate LoadSlot with kind:alias
                ctx.instructions.append(self.allocator, .{ .LoadSlot = .{ .slot = slot, .kind = .alias, .module_id = null } }) catch continue;
            } else if (std.mem.startsWith(u8, line, "    StoreAlias")) {
                store_alias_count += 1;
                // Parse StoreAlias instruction: "    StoreAlias 1 \"param\" Int"
                // Find the slot number after "StoreAlias "
                const storealias_start = std.mem.indexOf(u8, line, "StoreAlias ") orelse continue;
                const after_storealias = line[storealias_start + 10 ..]; // Skip "StoreAlias "
                // Skip leading whitespace
                const trimmed = std.mem.trim(u8, after_storealias, " ");
                // Find the first space after the number
                const space_pos = std.mem.indexOf(u8, trimmed, " ") orelse trimmed.len;
                const slot_str = trimmed[0..space_pos];
                const slot = std.fmt.parseInt(u32, slot_str, 10) catch continue;

                // Parse the type part: find the last space and extract the type
                const last_space = std.mem.lastIndexOf(u8, trimmed, " ") orelse continue;
                const type_str = trimmed[last_space + 1 ..];

                // Convert type string to BytecodeType
                // Accept only concrete primitive bytecode types here. For custom
                // structs/enums, this field will be declared by name elsewhere,
                // so seeing the literal words "Struct"/"Enum" is invalid.
                const type_tag = if (std.mem.eql(u8, type_str, "Int")) .Int else if (std.mem.eql(u8, type_str, "Byte")) .Byte else if (std.mem.eql(u8, type_str, "Float")) .Float else if (std.mem.eql(u8, type_str, "String")) .String else if (std.mem.eql(u8, type_str, "Tetra")) .Tetra else if (std.mem.eql(u8, type_str, "Nothing")) .Nothing else if (std.mem.eql(u8, type_str, "Union") or std.mem.eql(u8, type_str, "Map") or std.mem.eql(u8, type_str, "Function") or std.mem.eql(u8, type_str, "Array")) {
                    std.debug.print("Unsupported alias type in StoreAlias: '{s}' (disallowed here)\n", .{type_str});
                    continue; // Disallow complex/aggregate types for alias tag parsing here
                } else if (std.mem.eql(u8, type_str, "Struct") or std.mem.eql(u8, type_str, "Enum")) {
                    std.debug.print("Invalid generic type token in StoreAlias: '{s}'. Use a concrete type name (e.g. 'Person').\n", .{type_str});
                    continue; // Must be a concrete custom type name
                } else .Struct; // relies on this being validated earlier in semantic analysis

                // Generate StoreSlot with kind:alias
                ctx.instructions.append(self.allocator, .{ .StoreSlot = .{ .target = .{ .slot = slot, .kind = .alias, .module_id = null }, .type_tag = type_tag } }) catch continue;
            }
        }
    }

    fn buildArtifactPath(self: *BytecodeGenerator, program: *const hir_types.HIRProgram) !?[]u8 {
        const raw_name: []const u8 = blk: {
            if (self.artifact_stem.len != 0) {
                break :blk self.artifact_stem;
            }
            if (program.module_map.count() != 0) {
                var it = program.module_map.iterator();
                if (it.next()) |entry| break :blk entry.key_ptr.*;
            }

            if (program.function_table.len != 0) break :blk program.function_table[0].qualified_name;

            break :blk "module";
        };

        const sanitized = try self.allocator.dupe(u8, raw_name);
        defer self.allocator.free(sanitized);
        for (sanitized) |*ch| {
            switch (ch.*) {
                '/', '\\', ' ' => ch.* = '_',
                else => {},
            }
        }

        const file_name = try std.fmt.allocPrint(self.allocator, "{s}.boxa", .{sanitized});
        defer self.allocator.free(file_name);

        return try std.fs.path.join(self.allocator, &.{ self.artifact_dir, file_name });
    }

    fn lowerFunctions(self: *BytecodeGenerator, program: *const hir_types.HIRProgram, ctx: *Context) ![]module.BytecodeFunction {
        var list = std.ArrayList(module.BytecodeFunction){};
        errdefer list.deinit(self.allocator);

        for (program.function_table) |fn_info| {
            const param_types = try self.copyParamTypes(fn_info.param_types);
            errdefer self.allocator.free(param_types);

            const param_alias = try self.allocator.dupe(bool, fn_info.param_is_alias);
            errdefer self.allocator.free(param_alias);

            const start_ip = ctx.lookupLabelPosition(fn_info.start_label) orelse 0;
            const body_ip = if (fn_info.body_label) |label| ctx.lookupLabelPosition(label) else null;
            const module_id = try ctx.moduleIdForFunction(fn_info.qualified_name);

            try list.append(self.allocator, .{
                .name = fn_info.name,
                .qualified_name = fn_info.qualified_name,
                .module_id = module_id,
                .arity = fn_info.arity,
                .return_type = try module.typeFromHIR(switch (fn_info.return_type) {
                    .Unknown => .Nothing,
                    else => fn_info.return_type,
                }),
                .start_label = fn_info.start_label,
                .body_label = fn_info.body_label,
                .start_ip = start_ip,
                .body_ip = body_ip,
                .local_var_count = fn_info.local_var_count,
                .is_entry = fn_info.is_entry,
                .param_types = param_types,
                .param_is_alias = param_alias,
            });
        }

        return list.toOwnedSlice(self.allocator);
    }

    fn copyParamTypes(self: *BytecodeGenerator, hir_params: []const hir_types.HIRType) ![]module.BytecodeType {
        const out = try self.allocator.alloc(module.BytecodeType, hir_params.len);
        for (hir_params, 0..) |hir_type, idx| {
            out[idx] = try module.typeFromHIR(hir_type);
        }
        return out;
    }

    const Context = struct {
        allocator: std.mem.Allocator,
        instructions: std.ArrayList(module.Instruction),
        label_ids: std.StringHashMap(u32),
        label_positions: std.AutoHashMap(u32, u32),
        next_label_id: u32,
        instructions_taken: bool,
        modules: std.ArrayList(module.ModuleDescriptor),
        modules_taken: bool,
        module_ids: std.StringHashMap(module.ModuleId),
        default_module_id: module.ModuleId,
        alias_slots: std.AutoHashMap(module.SlotIndex, void),
        slot_cache: std.AutoHashMap(u32, module.SlotOperand),

        fn init(allocator: std.mem.Allocator, program: *const hir_types.HIRProgram) !Context {
            var context = Context{
                .allocator = allocator,
                .instructions = .{},
                .label_ids = std.StringHashMap(u32).init(allocator),
                .label_positions = std.AutoHashMap(u32, u32).init(allocator),
                .next_label_id = 0,
                .instructions_taken = false,
                .modules = .{},
                .modules_taken = false,
                .module_ids = std.StringHashMap(module.ModuleId).init(allocator),
                .default_module_id = 0,
                .alias_slots = std.AutoHashMap(module.SlotIndex, void).init(allocator),
                .slot_cache = std.AutoHashMap(u32, module.SlotOperand).init(allocator),
            };

            try context.seedModules(program);
            return context;
        }

        fn deinit(self: *Context) void {
            if (!self.instructions_taken) {
                self.instructions.deinit(self.allocator);
            }
            self.label_ids.deinit();
            self.label_positions.deinit();
            if (!self.modules_taken) {
                self.modules.deinit(self.allocator);
            }
            self.module_ids.deinit();
            self.alias_slots.deinit();
            self.slot_cache.deinit();
        }

        fn takeInstructions(self: *Context) ![]module.Instruction {
            const slice = try self.instructions.toOwnedSlice(self.allocator);
            self.instructions_taken = true;
            return slice;
        }

        fn takeModules(self: *Context) ![]module.ModuleDescriptor {
            const slice = try self.modules.toOwnedSlice(self.allocator);
            self.modules_taken = true;
            return slice;
        }

        fn seedModules(self: *Context, program: *const hir_types.HIRProgram) !void {
            var it = program.module_map.iterator();
            var next_id: module.ModuleId = 0;
            while (it.next()) |entry| {
                const id = next_id;
                next_id += 1;
                try self.modules.append(self.allocator, .{
                    .id = id,
                    .name = entry.key_ptr.*, // Borrowed from HIR program
                    .global_var_count = entry.value_ptr.*.global_var_count,
                });
                try self.module_ids.put(entry.key_ptr.*, id);
            }

            if (self.modules.items.len == 0) {
                const fallback_name = "main";
                try self.modules.append(self.allocator, .{ .id = 0, .name = fallback_name, .global_var_count = 0 });
                try self.module_ids.put(fallback_name, 0);
            }

            self.default_module_id = self.modules.items[0].id;
        }

        fn moduleIdForFunction(self: *Context, qualified_name: []const u8) !module.ModuleId {
            if (std.mem.indexOfScalar(u8, qualified_name, '.')) |dot_idx| {
                const module_name = qualified_name[0..dot_idx];
                return self.ensureModuleId(module_name);
            }

            return self.default_module_id;
        }

        fn ensureModuleId(self: *Context, name: []const u8) !module.ModuleId {
            if (self.module_ids.get(name)) |existing| {
                return existing;
            }

            const new_id: module.ModuleId = @intCast(self.modules.items.len);
            try self.module_ids.put(name, new_id);
            try self.modules.append(self.allocator, .{ .id = new_id, .name = name, .global_var_count = 0 });
            return new_id;
        }

        fn moduleIdFor(self: *Context, name_opt: ?[]const u8) !module.ModuleId {
            if (name_opt) |name| {
                return self.ensureModuleId(name);
            }
            return self.default_module_id;
        }

        fn touchGlobalSlot(self: *Context, module_id: module.ModuleId, slot: module.SlotIndex) void {
            const idx: usize = @intCast(module_id);
            if (idx >= self.modules.items.len) return;

            const required: u32 = slot + 1;
            if (self.modules.items[idx].global_var_count < required) {
                self.modules.items[idx].global_var_count = required;
            }
        }

        fn makeSlotOperand(self: *Context, scope: hir_types.ScopeKind, slot_raw: u32, module_context: ?[]const u8) !module.SlotOperand {
            const slot: module.SlotIndex = @intCast(slot_raw);
            const operand = switch (scope) {
                .Local => blk: {
                    const is_alias = self.alias_slots.contains(slot);
                    break :blk module.SlotOperand{
                        .slot = slot,
                        .kind = if (is_alias) .alias else .local,
                        .module_id = null,
                    };
                },
                .GlobalLocal => blk: {
                    // GlobalLocal behaves like ModuleGlobal but represents script-level globals
                    const module_id = try self.moduleIdFor(module_context);
                    self.touchGlobalSlot(module_id, slot);
                    break :blk module.SlotOperand{
                        .slot = slot,
                        .kind = .module_global,
                        .module_id = module_id,
                    };
                },
                .ModuleGlobal => blk: {
                    const module_id = try self.moduleIdFor(module_context);
                    self.touchGlobalSlot(module_id, slot);
                    break :blk module.SlotOperand{
                        .slot = slot,
                        .kind = .module_global,
                        .module_id = module_id,
                    };
                },
                .ImportedModule => blk: {
                    const name = module_context orelse return error.MissingModuleContext;
                    const module_id = try self.ensureModuleId(name);
                    self.touchGlobalSlot(module_id, slot);
                    break :blk module.SlotOperand{
                        .slot = slot,
                        .kind = .imported_module,
                        .module_id = module_id,
                    };
                },
                .Builtin => module.SlotOperand{
                    .slot = slot,
                    .kind = .builtin,
                    .module_id = null,
                },
            };
            try self.slot_cache.put(slot_raw, operand);
            return operand;
        }

        fn slotFromCache(self: *Context, slot_raw: u32) ?module.SlotOperand {
            if (self.slot_cache.get(slot_raw)) |operand| {
                return operand;
            }
            return null;
        }

        fn resolveCallModuleId(self: *Context, kind: hir_types.CallKind, module_name: ?[]const u8) !?module.ModuleId {
            return switch (kind) {
                .LocalFunction => self.default_module_id,
                .ModuleFunction => try self.moduleIdFor(module_name),
                .BuiltinFunction => null,
            };
        }

        fn lowerInstruction(self: *Context, inst: hir_instructions.HIRInstruction) !void {
            switch (inst) {
                .Const => |payload| try self.instructions.append(self.allocator, .{ .PushConst = .{ .constant_index = payload.constant_id } }),
                .Dup => try self.instructions.append(self.allocator, .Dup),
                .Pop => try self.instructions.append(self.allocator, .Pop),
                .Swap => try self.instructions.append(self.allocator, .Swap),
                .LoadVar => |payload| {
                    const operand = try self.makeSlotOperand(payload.scope_kind, payload.var_index, payload.module_context);
                    try self.instructions.append(self.allocator, .{ .LoadSlot = operand });
                },
                .StoreVar => |payload| {
                    const operand = try self.makeSlotOperand(payload.scope_kind, payload.var_index, payload.module_context);
                    const safe_hir_type = switch (payload.expected_type) {
                        .Unknown => .Int,
                        else => payload.expected_type,
                    };
                    try self.instructions.append(self.allocator, .{ .StoreSlot = .{ .target = operand, .type_tag = try module.typeFromHIR(safe_hir_type) } });
                },
                .StoreConst => |payload| {
                    const operand = try self.makeSlotOperand(payload.scope_kind, payload.var_index, payload.module_context);
                    if (self.slot_cache.get(payload.var_index) == null) {
                        try self.slot_cache.put(payload.var_index, operand);
                    }
                    try self.instructions.append(self.allocator, .{ .StoreConstSlot = operand });
                },
                .StoreDecl => |payload| {
                    const operand = try self.makeSlotOperand(payload.scope_kind, payload.var_index, payload.module_context);
                    if (self.slot_cache.get(payload.var_index) == null) {
                        try self.slot_cache.put(payload.var_index, operand);
                    }
                    const safe_hir_type = switch (payload.declared_type) {
                        .Unknown => .Int,
                        else => payload.declared_type,
                    };
                    try self.instructions.append(self.allocator, .{ .StoreSlot = .{ .target = operand, .type_tag = try module.typeFromHIR(safe_hir_type) } });
                },
                .PushStorageId => |payload| {
                    const operand = self.slotFromCache(payload.var_index) orelse try self.makeSlotOperand(payload.scope_kind, payload.var_index, null);
                    try self.instructions.append(self.allocator, .{ .PushStorageRef = operand });
                },
                .StoreParamAlias => |payload| {
                    const alias_slot: module.SlotIndex = payload.var_index;
                    try self.alias_slots.put(alias_slot, {});
                    try self.instructions.append(self.allocator, .{ .BindAlias = .{ .alias_slot = alias_slot, .type_tag = try module.typeFromHIR(payload.param_type) } });
                },
                .LoadAlias => |payload| {
                    // For alias parameters, we need to load from the alias slot
                    // This will be handled by the VM using the alias reference
                    try self.instructions.append(self.allocator, .{ .LoadSlot = .{ .slot = payload.slot_index, .kind = .alias, .module_id = null } });
                },
                .StoreAlias => |payload| {
                    // For alias parameters, we need to store to the alias slot
                    // This will be handled by the VM using the alias reference
                    try self.instructions.append(self.allocator, .{ .StoreAlias = .{ .slot_index = payload.slot_index, .type_tag = try module.typeFromHIR(payload.expected_type) } });
                },
                .ResolveAlias => |payload| {
                    // Resolve an alias to its target slot
                    try self.instructions.append(self.allocator, .{ .ResolveAlias = .{ .target_slot = payload.target_slot } });
                },
                .BindAlias => |payload| {
                    // Bind an alias to its target variable
                    const alias_slot: module.SlotIndex = payload.alias_slot;
                    try self.alias_slots.put(alias_slot, {});
                    try self.instructions.append(self.allocator, .{ .BindAlias = .{ .alias_slot = alias_slot, .type_tag = try module.typeFromHIR(payload.target_type) } });
                },
                .Arith => |payload| try self.instructions.append(self.allocator, .{ .Arith = .{ .op = payload.op, .type_tag = try module.typeFromHIR(payload.operand_type) } }),
                .Convert => |payload| try self.instructions.append(self.allocator, .{ .Convert = .{ .from = try module.typeFromHIR(payload.from_type), .to = try module.typeFromHIR(payload.to_type) } }),
                .Compare => |payload| try self.instructions.append(self.allocator, .{ .Compare = .{ .op = payload.op, .type_tag = try module.typeFromHIR(payload.operand_type) } }),
                .TypeCheck => |payload| try self.instructions.append(self.allocator, .{ .TypeCheck = .{ .type_name = payload.target_type } }),
                .LogicalOp => |payload| try self.instructions.append(self.allocator, .{ .LogicalOp = .{ .op = payload.op } }),
                .StringOp => |payload| try self.instructions.append(self.allocator, .{ .StringOp = .{ .op = payload.op } }),
                .Jump => |payload| {
                    const id = try self.ensureLabelId(payload.label);
                    try self.instructions.append(self.allocator, .{ .Jump = .{ .label_id = id } });
                },
                .JumpCond => |payload| {
                    const true_id = try self.ensureLabelId(payload.label_true);
                    const false_id = try self.ensureLabelId(payload.label_false);
                    const cond_type = try module.typeFromHIR(payload.condition_type);

                    // Special handling for tetra values
                    if (payload.condition_type == .Tetra) {
                        // For tetra values, we need to handle both/neither cases
                        // The logic is:
                        // - if condition is false (0) -> jump to false
                        // - if condition is true (1) -> jump to true
                        // - if condition is both (2) -> jump to true
                        // - if condition is neither (3) -> jump to false

                        // Check if condition is false (0) -> jump to false (pops condition)
                        try self.instructions.append(self.allocator, .{ .JumpIfFalse = .{ .label_id = false_id, .condition_type = cond_type } });

                        // If we reach here, condition is true (1) -> jump to true (condition already popped by JumpIfFalse)
                        try self.instructions.append(self.allocator, .{ .Jump = .{ .label_id = true_id } });
                        // This will fall through to the next instruction
                    } else {
                        // Standard boolean logic for non-tetra values
                        try self.instructions.append(self.allocator, .{ .JumpIfFalse = .{ .label_id = false_id, .condition_type = cond_type } });
                        try self.instructions.append(self.allocator, .{ .Jump = .{ .label_id = true_id } });
                    }
                },
                .Label => |payload| {
                    const id = try self.ensureLabelId(payload.name);
                    try self.instructions.append(self.allocator, .{ .Label = .{ .id = id } });
                    try self.label_positions.put(id, @intCast(self.instructions.items.len - 1));
                },
                .Call => |payload| try self.instructions.append(self.allocator, .{ .Call = .{
                    .target = .{
                        .function_index = payload.function_index,
                        .qualified_name = payload.qualified_name,
                        .call_kind = payload.call_kind,
                        .target_module = payload.target_module,
                        .target_module_id = try self.resolveCallModuleId(payload.call_kind, payload.target_module),
                    },
                    .arg_count = payload.arg_count,
                    .return_type = try module.typeFromHIR(switch (payload.return_type) {
                        .Unknown => .Nothing,
                        else => payload.return_type,
                    }),
                } }),
                .TailCall => |payload| try self.instructions.append(self.allocator, .{ .TailCall = .{
                    .target = .{
                        .function_index = payload.function_index,
                        .qualified_name = payload.qualified_name,
                        .call_kind = payload.call_kind,
                        .target_module = payload.target_module,
                        .target_module_id = try self.resolveCallModuleId(payload.call_kind, payload.target_module),
                    },
                    .arg_count = payload.arg_count,
                    .return_type = try module.typeFromHIR(switch (payload.return_type) {
                        .Unknown => .Nothing,
                        else => payload.return_type,
                    }),
                } }),
                .Return => |payload| try self.instructions.append(self.allocator, .{ .Return = .{ .has_value = payload.has_value, .return_type = try module.typeFromHIR(switch (payload.return_type) {
                    .Unknown => .Nothing,
                    else => payload.return_type,
                }) } }),
                .TypeOf => |payload| try self.instructions.append(self.allocator, .{ .TypeOf = .{ .value_type = try module.typeFromHIR(switch (payload.value_type) {
                    .Unknown => .Nothing,
                    else => payload.value_type,
                }) } }),
                .GetField => |payload| try self.instructions.append(self.allocator, .{ .GetField = .{ .field_name = payload.field_name, .container_type = try module.typeFromHIR(switch (payload.container_type) {
                    .Unknown => .Nothing,
                    else => payload.container_type,
                }), .field_index = payload.field_index } }),
                .SetField => |payload| try self.instructions.append(self.allocator, .{ .SetField = .{ .field_name = payload.field_name, .container_type = try module.typeFromHIR(switch (payload.container_type) {
                    .Unknown => .Nothing,
                    else => payload.container_type,
                }), .field_index = payload.field_index } }),
                .StoreFieldName => |payload| try self.instructions.append(self.allocator, .{ .StoreFieldName = .{ .field_name = payload.field_name } }),
                .TryBegin => |payload| {
                    const id = try self.ensureLabelId(payload.catch_label);
                    try self.instructions.append(self.allocator, .{ .TryBegin = .{ .label_id = id } });
                },
                .TryCatch => |payload| {
                    const exc_type = if (payload.exception_type) |t| module.typeFromHIR(t) catch return error.UnknownType else null;
                    try self.instructions.append(self.allocator, .{ .TryCatch = .{ .exception_type = exc_type } });
                },
                .Throw => |payload| try self.instructions.append(self.allocator, .{ .Throw = .{ .exception_type = try module.typeFromHIR(payload.exception_type) } }),
                .EnterScope => |payload| try self.instructions.append(self.allocator, .{ .EnterScope = .{ .scope_id = payload.scope_id, .var_count = payload.var_count } }),
                .ExitScope => |payload| try self.instructions.append(self.allocator, .{ .ExitScope = .{ .scope_id = payload.scope_id } }),
                .ArrayNew => |payload| try self.instructions.append(self.allocator, .{ .ArrayNew = .{
                    .element_type = try module.typeFromHIR(switch (payload.element_type) {
                        .Unknown => .Nothing,
                        else => payload.element_type,
                    }),
                    .static_size = payload.size,
                    .nested_element_type = if (payload.nested_element_type) |nested| module.typeFromHIR(nested) catch return error.UnknownType else null,
                    .storage_kind = payload.storage_kind,
                } }),
                .ArrayGet => |payload| try self.instructions.append(self.allocator, .{ .ArrayGet = .{ .bounds_check = payload.bounds_check } }),
                .ArraySet => |payload| try self.instructions.append(self.allocator, .{ .ArraySet = .{ .bounds_check = payload.bounds_check } }),
                .ArrayPush => |payload| try self.instructions.append(self.allocator, .{ .ArrayPush = .{ .resize = payload.resize_behavior } }),
                .ArrayPop => try self.instructions.append(self.allocator, .ArrayPop),
                .ArrayInsert => try self.instructions.append(self.allocator, .ArrayInsert),
                .ArrayRemove => try self.instructions.append(self.allocator, .ArrayRemove),
                .ArraySlice => try self.instructions.append(self.allocator, .ArraySlice),
                .ArrayLen => try self.instructions.append(self.allocator, .ArrayLen),
                .ArrayConcat => try self.instructions.append(self.allocator, .ArrayConcat),
                .Range => |payload| try self.instructions.append(self.allocator, .{ .Range = .{ .element_type = try module.typeFromHIR(payload.element_type) } }),
                .Exists => |payload| try self.instructions.append(self.allocator, .{ .Exists = .{ .predicate_type = try module.typeFromHIR(payload.predicate_type) } }),
                .Forall => |payload| try self.instructions.append(self.allocator, .{ .Forall = .{ .predicate_type = try module.typeFromHIR(payload.predicate_type) } }),
                // Compound assignment operations
                .ArrayGetAndAdd => |payload| {
                    try self.instructions.append(self.allocator, .{ .ArrayGetAndAdd = .{ .bounds_check = payload.bounds_check } });
                },
                .ArrayGetAndSub => |payload| try self.instructions.append(self.allocator, .{ .ArrayGetAndSub = .{ .bounds_check = payload.bounds_check } }),
                .ArrayGetAndMul => |payload| try self.instructions.append(self.allocator, .{ .ArrayGetAndMul = .{ .bounds_check = payload.bounds_check } }),
                .ArrayGetAndDiv => |payload| try self.instructions.append(self.allocator, .{ .ArrayGetAndDiv = .{ .bounds_check = payload.bounds_check } }),
                .ArrayGetAndMod => |payload| try self.instructions.append(self.allocator, .{ .ArrayGetAndMod = .{ .bounds_check = payload.bounds_check } }),
                .ArrayGetAndPow => |payload| try self.instructions.append(self.allocator, .{ .ArrayGetAndPow = .{ .bounds_check = payload.bounds_check } }),
                .StructNew => |payload| try self.instructions.append(self.allocator, .{ .StructNew = .{
                    .type_name = payload.type_name,
                    .field_count = payload.field_count,
                    .field_types = payload.field_types,
                    .size_bytes = payload.size_bytes,
                } }),
                .EnumNew => |payload| try self.instructions.append(self.allocator, .{ .EnumNew = .{
                    .enum_name = payload.enum_name,
                    .variant_name = payload.variant_name,
                    .variant_index = payload.variant_index,
                } }),
                .Print => try self.instructions.append(self.allocator, .Print),
                .PrintBegin => try self.instructions.append(self.allocator, .PrintBegin),
                .PrintStr => |payload| try self.instructions.append(self.allocator, .{ .PrintStr = .{ .const_id = payload.const_id } }),
                .PrintVal => try self.instructions.append(self.allocator, .PrintVal),
                .PrintNewline => try self.instructions.append(self.allocator, .PrintNewline),
                .PrintEnd => try self.instructions.append(self.allocator, .PrintEnd),
                .PrintInterpolated => |payload| try self.instructions.append(self.allocator, .{ .PrintInterpolated = .{
                    .format_parts = payload.format_parts,
                    .placeholder_indices = payload.placeholder_indices,
                    .argument_count = payload.argument_count,
                    .format_part_ids = payload.format_part_ids,
                } }),
                .Peek => |payload| try self.instructions.append(self.allocator, .{ .Peek = .{
                    .name = payload.name,
                    .value_type = try module.typeFromHIR(switch (payload.value_type) {
                        .Unknown => .Nothing,
                        else => payload.value_type,
                    }),
                    .location = payload.location,
                    .union_members = payload.union_members,
                } }),
                .PeekStruct => |payload| try self.instructions.append(self.allocator, .{ .PeekStruct = .{
                    .type_name = payload.type_name,
                    .field_count = payload.field_count,
                    .field_names = payload.field_names,
                    .field_types = payload.field_types,
                    .location = payload.location,
                    .should_pop_after_peek = payload.should_pop_after_peek,
                } }),
                .PrintStruct => |payload| try self.instructions.append(self.allocator, .{ .PrintStruct = .{
                    .type_name = payload.type_name,
                    .field_count = payload.field_count,
                    .field_names = payload.field_names,
                    .field_types = payload.field_types,
                    .location = payload.location,
                    .should_pop_after_peek = payload.should_pop_after_peek,
                } }),
                .Map => |payload| try self.instructions.append(self.allocator, .{ .Map = .{
                    .entries = payload.entries,
                    .key_type = try module.typeFromHIR(switch (payload.key_type) {
                        .Unknown => .Nothing,
                        else => payload.key_type,
                    }),
                    .value_type = try module.typeFromHIR(switch (payload.value_type) {
                        .Unknown => .Nothing,
                        else => payload.value_type,
                    }),
                    .has_else_value = payload.has_else_value,
                } }),
                .MapGet => |payload| try self.instructions.append(self.allocator, .{ .MapGet = .{ .key_type = try module.typeFromHIR(switch (payload.key_type) {
                    .Unknown => .Nothing,
                    else => payload.key_type,
                }) } }),
                .MapSet => |payload| try self.instructions.append(self.allocator, .{ .MapSet = .{ .key_type = try module.typeFromHIR(switch (payload.key_type) {
                    .Unknown => .Nothing,
                    else => payload.key_type,
                }) } }),
                .AssertFail => |payload| try self.instructions.append(self.allocator, .{ .AssertFail = .{ .location = payload.location, .has_message = payload.has_message } }),
                .Unreachable => |payload| try self.instructions.append(self.allocator, .{ .Unreachable = .{ .location = payload.location } }),
                .Halt => try self.instructions.append(self.allocator, .Halt),
                .LoadModule => |payload| try self.instructions.append(self.allocator, .{ .LoadModule = .{ .module_name = payload.module_name } }),
            }
        }

        fn ensureLabelId(self: *Context, name: []const u8) !u32 {
            if (self.label_ids.get(name)) |existing| {
                return existing;
            }
            const id = self.next_label_id;
            self.next_label_id += 1;
            try self.label_ids.put(name, id);
            return id;
        }

        fn lookupLabelPosition(self: *Context, name: []const u8) ?u32 {
            if (self.label_ids.get(name)) |id| {
                if (self.label_positions.get(id)) |pos| {
                    return pos;
                }
            }
            return null;
        }
    };
};
