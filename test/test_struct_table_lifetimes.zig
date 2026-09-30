const std = @import("std");
const testing = std.testing;

const StructTable = @import("../src/common/struct_table.zig").StructTable;
const ast = @import("../src/ast/ast.zig");
const HIRType = @import("../src/codegen/hir/soxa_types.zig").HIRType;
const IRPrinter = @import("../src/codegen/llvmir/ir_printer.zig").IRPrinter;

// `IRPrinter.registerStructTableLayouts` used to store the struct table's own
// `field.name` slices into `struct_field_names_by_type`, and
// `IRPrinter.deinit` frees every inner string of that map. The result was a
// cross-owner free (and a double free when both share an allocator) that
// clobbered the table's field names between semantic analysis and any later
// phase — observed when an inline-Zig wrapper read the table. The printer must
// own copies; this asserts the table's storage is never borrowed into that map.
test "struct table field names survive IR printer deinit" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var table = StructTable.init(allocator);
    var int_field = ast.TypeInfo{ .base = .Int };
    const inputs = [_]StructTable.FieldInput{
        .{ .name = "alpha", .type_info = &int_field },
        .{ .name = "beta", .type_info = &int_field },
    };
    const id = try table.registerStruct("Sample", &inputs);
    // `registerStructTableLayouts` skips entries whose field HIR types are not
    // resolved, so make them concrete before the printer runs.
    table.setFieldHIRType(id, 0, .Int);
    table.setFieldHIRType(id, 1, .Int);

    const table_fields = table.fields(id) orelse return error.MissingStruct;
    try testing.expectEqual(@as(usize, 2), table_fields.len);

    const struct_table_ptr: *anyopaque = @ptrCast(&table);
    var printer = IRPrinter.init(
        testing.io,
        allocator,
        null,
        null,
        struct_table_ptr,
        std.StringHashMap([]HIRType).init(allocator),
        null,
        false,
        .Wrap,
    );
    try printer.registerStructTableLayouts();

    const stored = printer.struct_field_names_by_type.get("Sample") orelse return error.MissingLayout;
    try testing.expectEqual(@as(usize, 2), stored.len);
    // The map's inner names must be printer-owned copies. If they alias the
    // table's slices, `IRPrinter.deinit` frees the table's storage.
    try testing.expect(stored[0].ptr != table_fields[0].name.ptr);
    try testing.expect(stored[1].ptr != table_fields[1].name.ptr);

    printer.deinit();

    // The table's names are intact after the printer's lifetime.
    try testing.expectEqualStrings("alpha", table_fields[0].name);
    try testing.expectEqualStrings("beta", table_fields[1].name);
}
