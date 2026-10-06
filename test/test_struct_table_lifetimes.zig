const std = @import("std");
const testing = std.testing;

const StructTable = @import("../src/common/struct_table.zig").StructTable;
const GroupTable = @import("../src/common/group_table.zig").GroupTable;
const EnumTable = @import("../src/common/enum_table.zig").EnumTable;
const ast = @import("../src/ast/ast.zig");
const ModuleGraph = @import("../src/module/graph.zig").ModuleGraph;
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

    var graph = try ModuleGraph.init(testing.io, allocator, &.{});
    defer graph.deinit();
    const record = try graph.addRecord(null, "pkg//sample.doxa", .doxa);

    var table = StructTable.init(allocator);
    var int_field = ast.TypeInfo{ .base = .Int };
    const inputs = [_]StructTable.FieldInput{
        .{ .name = "alpha", .type_info = &int_field },
        .{ .name = "beta", .type_info = &int_field },
    };
    const id = try table.registerStruct(.{ .module = record.id, .name = "Sample" }, &inputs);
    // `registerStructTableLayouts` skips entries whose field HIR types are not
    // resolved, so make them concrete before the printer runs.
    table.setFieldHIRType(id, 0, .Int);
    table.setFieldHIRType(id, 1, .Int);

    const table_fields = table.fields(id) orelse return error.MissingStruct;
    try testing.expectEqual(@as(usize, 2), table_fields.len);

    // The printer names structs by canonical key, which a final graph fixes.
    try graph.finalizeMangling();
    try table.assignKeys(&graph);

    var groups = GroupTable.init(allocator);
    defer groups.deinit();
    var enums = EnumTable.init(allocator);
    defer enums.deinit();
    var printer = IRPrinter.init(
        testing.io,
        allocator,
        &groups,
        &enums,
        &table,
        null,
        false,
        .Wrap,
    );
    try printer.registerStructTableLayouts();

    const stored = printer.struct_field_names_by_type.get(table.keyOf(id).?) orelse return error.MissingLayout;
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
