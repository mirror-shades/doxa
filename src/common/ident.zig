//! ASCII identifier predicates shared by the lexer and the stdlib catalog.

const std = @import("std");

pub fn isStart(c: u8) bool {
    return std.ascii.isAlphabetic(c) or c == '_';
}

pub fn isContinue(c: u8) bool {
    return isStart(c) or std.ascii.isDigit(c);
}
