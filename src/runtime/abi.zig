//! The runtime's data ABI: the layouts codegen emits and the runtime reads.
//!
//! Every type here is `extern`: it crosses the `callconv(.c)` boundary or is
//! laid out by the IR printer (`%DoxaValue`, `%ArrayHeader`, …). The functions
//! that operate on them live in the sibling modules; `doxa_rt.zig` re-exports
//! these names as the public runtime surface.

const scope_arena = @import("scope_arena.zig");

const Scope = scope_arena.Scope;

/// Canonical string representation: pointer + byte length.
///
/// This is the internal model for every layer (parser, HIR, LLVM IR).
/// It matches Zig's `[]const u8` slice semantics: valid indices are `0..len`,
/// embedded U+0000 is permitted, and length is O(1) without scanning.
///
/// Important: this struct cannot cross a `callconv(.c)` boundary by value.
/// On Windows x64 the 16-byte aggregate disagrees between sret (Zig's
/// expectation for `callconv(.c)`) and RAX:RDX register return (LLVM's
/// lowering). All exported functions therefore use explicit `(ptr, len)`
/// pairs and out-parameter returns. Only internal Zig↔Zig calls may pass
/// DoxaString by value.
pub const DoxaString = extern struct {
    ptr: ?[*]const u8,
    len: u64,
};

pub const DoxaPeekInfo = extern struct {
    file: ?[*:0]const u8,
    name: ?[*:0]const u8,
    type_name: ?[*:0]const u8,
    union_members: ?*const [*:0]const u8,
    union_member_count: u32,
    active_member_index: i32,
    has_location: u32,
    line: u32,
    column: u32,
};

/// Canonical runtime representation for Doxa values. This is the shared value
/// model for the native (LLVM) backend and any C callers. The layout must stay
/// in sync with `%DoxaValue` in `src/codegen/llvmir/ir_printer.zig`.
///
/// Tag values:
///   0 = Int      (payload_bits = i64)
///   1 = Float    (payload_bits = bitcast f64)
///   2 = Byte     (payload_bits[0..8] = u8)
///   3 = String   (payload_bits = pointer bits, payload_len = byte length)
///   4 = Array    (payload_bits = *ArrayHeader as bits)
///   5 = Struct   (payload_bits = pointer to struct instance)
///   6 = Enum     (payload_bits = i64 variant index; type via metadata)
///   7 = Tetra    (payload_bits[0..2] = u2)
///   8 = Nothing  (payload_bits ignored)
///   9 = Function (payload_bits = pointer to function closure/descriptor)
///
/// All non-String tags leave `payload_len = 0`; only String uses it.
///
/// Boxing rules:
///   - Unboxed: Int, Float, Byte, Tetra, Enum discriminant
///   - Boxed:   String, Array, Struct, Function
///   - Sentinel: Nothing (tag = 8, payload_bits = 0)
///
/// Box encoding (a value of a union or group type):
///   - For an unboxed value, `reserved == 0`.
///   - For a box, `reserved` packs:
///       bit 31        : is_boxed flag
///       bits 16..30   : box_id — the box type, one id space for unions and
///                       groups, indexing the box registry (`BoxDesc`)
///       bits 0..15    : member_index — the member of that box type held
///   - `tag` always describes the active payload kind (Int, Float, Struct, …).
pub const DoxaValue = extern struct {
    tag: u32,
    reserved: u32,
    payload_bits: i64,
    payload_len: i64,
};

/// High-level tag enumeration for `DoxaValue.tag`. The numeric values are part
/// of the ABI and must stay in sync with the documentation above.
pub const DoxaTag = enum(u32) {
    Int = 0,
    Float = 1,
    Byte = 2,
    String = 3,
    Array = 4,
    Struct = 5,
    Enum = 6,
    Tetra = 7,
    Nothing = 8,
    Function = 9,
};

/// Encoding and decoding of a box's `reserved` word. Kept in sync with the
/// IR printer, which writes it (`buildDoxaValue`, `repackBox`).
pub const DoxaBoxMeta = struct {
    pub const is_boxed_bit: u32 = 1 << 31;
    pub const box_id_shift: u5 = 16;
    pub const box_id_mask: u32 = 0x7FFF << box_id_shift; // 15 bits
    pub const max_box_id: u32 = 0x7FFF;
    pub const member_index_mask: u32 = 0xFFFF; // 16 bits

    pub fn isBoxed(reserved: u32) bool {
        return (reserved & is_boxed_bit) != 0;
    }

    pub fn boxId(reserved: u32) u32 {
        return (reserved & box_id_mask) >> box_id_shift;
    }

    pub fn memberIndex(reserved: u32) u32 {
        return reserved & member_index_mask;
    }
};

/// A box type's members, in the order its member indexes name them. An enum
/// member has its display name and descriptor; any other member has neither.
pub const BoxDesc = extern struct {
    member_count: u64,
    member_names: [*]const ?[*:0]const u8,
    member_enums: [*]const ?*const EnumDesc,
};

pub const ArrayHeader = extern struct {
    data: ?*anyopaque,
    len: u64,
    cap: u64,
    elem_size: u64,
    elem_tag: u64,
    /// The scope arena this array was allocated in (`arena_of`): the compiler
    /// places every heap element stored into the array here first.
    scope: *Scope,
    /// For struct elements (tag 7) of a descriptor-free scalar struct, the
    /// struct's size in i64 words, so an element store copies it word for
    /// word (it has no header); 0 otherwise. Set by `doxa_array_set_elem_words` and
    /// carried to every array built from this one.
    elem_words: u64,
};

pub const StructDesc = extern struct {
    type_name: ?[*:0]const u8,
    field_count: u64,
    field_names: ?[*]const ?[*:0]const u8,
    field_tags: ?[*]const u64,
    /// Per field: the descriptor of an enum field (tag 8), else null.
    field_enums: ?[*]const ?*const EnumDesc,
    /// Per field: the word count of a nested descriptor-free scalar struct
    /// (tag 7), which has no header to clone it by; 0 for any other field.
    /// Null when the struct has no such field.
    field_struct_words: ?[*]const u64,
};

/// What a described struct carries, in the two words just before its fields:
/// its layout and the arena that owns it, as an `ArrayHeader` carries its
/// arena. A struct pointer points at the fields, so field offsets
/// (`layout.zig`) do not see the header. Only a struct that keeps its
/// descriptor has one; a descriptor-skipped struct is stored flat and copied
/// by its word count, and nothing reads a header from it.
pub const StructHeader = extern struct {
    desc: *const StructDesc,
    scope: *Scope,
};

pub const EnumDesc = extern struct {
    variant_count: u64,
    variant_names: ?[*]const ?[*:0]const u8,
};

/// An array element that is itself an array: the payload word is an
/// `*ArrayHeader`.
pub const ARRAY_TAG: u64 = 6;
/// A union or group element: a `DoxaValue` stored inline, 24 bytes. The box
/// keeps the member it holds, which a bare payload word would lose. Struct
/// fields use the same tag for a union or group field.
pub const VALUE_TAG: u64 = 9;
