pub const DoxaAbiTag = enum(u32) {
    Int = 0,
    Float = 1,
    Byte = 2,
    String = 3,
    Tetra = 4,
    Nothing = 5,
};

pub const DoxaAbiValue = extern struct {
    tag: DoxaAbiTag,
    flags: u32,
    payload0: u64,
    payload1: u64,
};

pub const DoxaAbiStatus = enum(i32) {
    ok = 0,
    bad_arity = 1,
    bad_tag = 2,
    bad_value = 3,
    internal = 255,
};
