const std = @import("std");
const Parser = @import("../parser/parser_types.zig").Parser;
const Errors = @import("../utils/errors.zig");
const ErrorList = Errors.ErrorList;

/// Resolves the module graph before semantic analysis: every namespace that is
/// imported, inline-zig, or otherwise reachable is loaded so the AST can refer
/// to it by name.
///
/// The resolver deliberately does not rewrite the AST. `Color.Red` and
/// `Error.IOError.NotFound` stay `FieldAccess` chains so the enum/group
/// qualifier survives into semantic analysis and codegen; the bare `.Red` form
/// is produced directly by the parser and carries its context from the
/// surrounding declaration or pattern instead.
pub const Resolver = struct {
    parser: *Parser,

    pub fn init(parser: *Parser) Resolver {
        return .{ .parser = parser };
    }

    pub fn resolve(self: *Resolver) ErrorList!void {
        try self.loadInitialModules();
        try self.parser.ensureSpecificImports();
        try self.parser.ensureReachableModuleDependencies();
    }

    fn loadInitialModules(self: *Resolver) ErrorList!void {
        var keys = std.StringHashMap(void).init(self.parser.allocator);
        defer keys.deinit();

        var it = self.parser.module_namespaces.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.ast == null and !entry.value_ptr.is_inline_zig) {
                try keys.put(entry.key_ptr.*, {});
            }
        }

        var key_it = keys.iterator();
        while (key_it.next()) |key_entry| {
            const namespace = key_entry.key_ptr.*;
            if (self.parser.module_namespaces.get(namespace)) |existing| {
                if (existing.ast == null) {
                    _ = try self.parser.loadAndRegisterModule(existing.file_path, namespace, null);
                }
            }
        }
    }
};
