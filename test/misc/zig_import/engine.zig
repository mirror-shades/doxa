//! Plain Zig an inline block imports: it names no Doxa type, and its own
//! errors are the block's to translate.
const helper = @import("helper.zig");

pub fn twice(x: i64) error{Negative}!i64 {
    if (x < 0) return error.Negative;
    return helper.double(x);
}
