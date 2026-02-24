const std = @import("std");

pub const Method = struct {
    label: []const u8,
    detail: []const u8,
    documentation: []const u8,
};

const METHODS = &[_]Method{
    .{
        .label = "@length",
        .detail = "Return the number of elements in a string or array",
        .documentation = "**Input**: string | array\n**Output**: int\nReturn the size of the collection.",
    },
    .{
        .label = "@push",
        .detail = "Append a value to the end of an array or string",
        .documentation = "**Input**: array | string, value\n**Output**: nothing\nAdd value to the end of the collection.",
    },
    .{
        .label = "@pop",
        .detail = "Remove the last element of a collection",
        .documentation = "**Input**: array | string\n**Output**: any\nPop the last element; panics on empty collections.",
    },
    .{
        .label = "@insert",
        .detail = "Insert at the given index",
        .documentation = "**Input**: array | string, index, value\n**Output**: nothing\nInsert value at index; panics on corruption.",
    },
    .{
        .label = "@remove",
        .detail = "Remove the element at the given index",
        .documentation = "**Input**: array | string, index\n**Output**: any\nRemove and return the element at index.",
    },
    .{
        .label = "@clear",
        .detail = "Empty the contents of a string or array",
        .documentation = "**Input**: array | string\n**Output**: nothing\nRemove every element from the collection.",
    },
    .{
        .label = "@find",
        .detail = "Find the first index of a value",
        .documentation = "**Input**: array | string, value\n**Output**: int\nReturn the first index or -1 when missing.",
    },
    .{
        .label = "@slice",
        .detail = "Return a subsection of a string or array",
        .documentation = "**Input**: array | string, start, length\n**Output**: array | string\nPanics on invalid ranges.",
    },
    .{
        .label = "@string",
        .detail = "Convert any value to a string",
        .documentation = "**Input**: any\n**Output**: string\nReturn a human readable representation of the value.",
    },
    .{
        .label = "@int",
        .detail = "Convert a literal to an integer",
        .documentation = "**Input**: float | byte | string\n**Output**: int\nPanics on invalid format or overflow.",
    },
    .{
        .label = "@float",
        .detail = "Convert a literal to a floating point value",
        .documentation = "**Input**: int | byte | string\n**Output**: float\nPanics on invalid format.",
    },
    .{
        .label = "@byte",
        .detail = "Convert to an unsigned byte",
        .documentation = "**Input**: int | float | string\n**Output**: byte\nPanics on overflow or invalid format.",
    },
    .{
        .label = "@type",
        .detail = "Return the runtime type name as a string",
        .documentation = "**Input**: any\n**Output**: string\nUse to inspect values in debugging.",
    },
    .{
        .label = "@random",
        .detail = "Produce a pseudo-random floating point value",
        .documentation = "**Input**: none\n**Output**: float\nReturns a random number between 0.0 and 1.0; may error at compile time if randomness is unavailable.",
    },
    .{
        .label = "@print",
        .detail = "Write text to the standard output",
        .documentation = "**Input**: string\n**Output**: nothing\nPanics on I/O failures.",
    },
    .{
        .label = "@assert",
        .detail = "Assert that a condition is true",
        .documentation = "**Input**: bool, string?\n**Output**: nothing\nPanics when the condition is false.",
    },
    .{
        .label = "@panic",
        .detail = "Abort execution with a message",
        .documentation = "**Input**: string\n**Output**: never\nAlways halts the program with the given message.",
    },
};

pub fn all() []const Method {
    return METHODS;
}

pub fn find(label: []const u8) ?*const Method {
    for (METHODS) |method| {
        if (std.mem.eql(u8, method.label, label)) {
            return &method;
        }
    }
    return null;
}
