// src/parser/symbol_table.zig

const std = @import("std");
const ast = @import("../ast.zig");
const token = @import("../token.zig");

pub const SymbolState = enum {
    Declared, // Forward declaration
    Defined, // Full definition available
    Imported, // From another module
};

pub const Symbol = struct {
    name: []const u8,
    is_public: bool,
    declared_at: token.Token,
    state: SymbolState, // Add this field
    kind: union(enum) {
        Function: struct {
            params: []ast.Parameter,
            return_type: ast.TypeInfo,
            is_safe: bool,
        },
        Variable: struct {
            type_info: ast.TypeInfo,
            is_const: bool,
        },
        Struct: struct {
            fields: []ast.StructField,
        },
        Enum: struct {
            variants: []ast.EnumVariant,
        },
        Module: struct {
            path: []const u8,
            is_safe: bool,
        },
        Import: struct {
            module_path: []const u8,
            imported_symbol: ?[]const u8, // null means whole module
        },
    },
};

pub const SymbolTable = struct {
    allocator: std.mem.Allocator,
    symbols: std.StringHashMap(Symbol),
    imports: std.StringHashMap([]const u8), // module name -> path
    parent: ?*SymbolTable,
    scope_level: u32,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .allocator = allocator,
            .symbols = std.StringHashMap(Symbol).init(allocator),
            .imports = std.StringHashMap([]const u8).init(allocator),
            .parent = null,
            .scope_level = 0,
        };
    }

    pub fn createChildScope(self: *SymbolTable) !*SymbolTable {
        var child = try self.allocator.create(SymbolTable);
        child.* = SymbolTable.init(self.allocator);
        child.parent = self;
        child.scope_level = self.scope_level + 1;
        return child;
    }

    pub fn addImport(self: *SymbolTable, name: []const u8, path: []const u8, specific_symbol: ?[]const u8) !void {
        try self.symbols.put(name, .{
            .name = name,
            .is_public = false, // imports are always private to the module
            .declared_at = undefined, // you might want to track this
            .state = .Imported,
            .kind = .{
                .Import = .{
                    .module_path = path,
                    .imported_symbol = specific_symbol,
                },
            },
        });
    }

    pub fn addSymbol(self: *SymbolTable, symbol: Symbol) !void {
        try self.symbols.put(symbol.name, symbol);
    }

    pub fn lookup(self: *SymbolTable, name: []const u8) ?Symbol {
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }
        return if (self.parent) |parent| parent.lookup(name) else null;
    }

    pub fn deinit(self: *SymbolTable) void {
        self.symbols.deinit();
        self.imports.deinit();
    }

    pub fn validateNoCircularImports(self: *SymbolTable) !void {
        _ = self;
        // Implementation to detect circular imports
    }

    pub fn validateAllDeclaredSymbolsAreDefined(self: *SymbolTable) !void {
        var it = self.symbols.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.state == .Declared) {
                // Error: symbol declared but never defined
                return error.UndefinedSymbol;
            }
        }
    }

    pub fn recordForwardDeclaration(self: *SymbolTable, symbol: Symbol) !void {
        const existing = self.symbols.get(symbol.name);
        if (existing != null) {
            return error.SymbolAlreadyDeclared;
        }
        var sym = symbol;
        sym.state = .Declared;
        try self.symbols.put(sym.name, sym);
    }

    pub fn defineSymbol(self: *SymbolTable, name: []const u8, definition: Symbol) !void {
        var existing = self.symbols.getPtr(name) orelse return error.SymbolNotDeclared;
        existing.* = definition;
        existing.state = .Defined;
    }
};
