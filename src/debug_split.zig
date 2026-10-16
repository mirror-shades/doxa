//! Debug information beside an ELF binary, not in it, as Windows keeps a
//! `.pdb` (`plan/diagnostics-follow-up.md` step 2). Zig's `objcopy` cannot
//! rewrite an ELF executable yet, so the compiler does it here.
//!
//! The linked binary itself becomes `<binary>.debug`: an unstripped binary
//! is a valid debug-link target for gdb, lldb, perf and Zig's reader, which
//! the runtime's backtrace uses. `strip` returns the binary to ship:
//!
//! - every byte a segment loads is kept where it was, so nothing that runs
//!   moves;
//! - the non-allocated sections that are kept (the symbol table, `.comment`,
//!   the section names) are laid out again after the last loaded byte,
//!   without the `.debug_*` sections;
//! - a removed section's header stays, as `SHT_NULL` with no name, so no
//!   section index changes and the symbol table needs no rewriting;
//! - a `.gnu_debuglink` section names the debug file and holds its CRC-32,
//!   which readers check before trusting it.
//!
//! Little-endian ELF32 and ELF64 only. TODO: big-endian targets keep their
//! debug information in the binary.

const std = @import("std");
const elf = std.elf;

pub const Error = error{
    /// Not an ELF file this splitter handles: the caller keeps the debug
    /// information in the binary.
    UnsupportedElf,
    /// An ELF file whose headers contradict themselves.
    MalformedElf,
} || std.mem.Allocator.Error;

/// The binary to ship for `linked`: its debug sections gone, and a
/// `.gnu_debuglink` to `debug_name`, a file whose CRC-32 is `debug_crc`.
pub fn strip(alloc: std.mem.Allocator, linked: []const u8, debug_name: []const u8, debug_crc: u32) Error![]u8 {
    if (linked.len < elf.EI_NIDENT or !std.mem.eql(u8, linked[0..4], elf.MAGIC)) return error.MalformedElf;
    if (linked[elf.EI_DATA] != elf.ELFDATA2LSB) return error.UnsupportedElf;
    return switch (linked[elf.EI_CLASS]) {
        elf.ELFCLASS64 => stripClass(elf.Elf64_Ehdr, elf.Elf64_Phdr, elf.Elf64_Shdr, alloc, linked, debug_name, debug_crc),
        elf.ELFCLASS32 => stripClass(elf.Elf32_Ehdr, elf.Elf32_Phdr, elf.Elf32_Shdr, alloc, linked, debug_name, debug_crc),
        else => error.UnsupportedElf,
    };
}

fn stripClass(
    comptime Ehdr: type,
    comptime Phdr: type,
    comptime Shdr: type,
    alloc: std.mem.Allocator,
    linked: []const u8,
    debug_name: []const u8,
    debug_crc: u32,
) Error![]u8 {
    var header = try read(Ehdr, linked, 0);
    if (header.e_shnum == 0 or header.e_shstrndx == elf.SHN_UNDEF or header.e_shstrndx >= header.e_shnum) return error.UnsupportedElf;
    if (header.e_shentsize != @sizeOf(Shdr) or (header.e_phnum != 0 and header.e_phentsize != @sizeOf(Phdr))) return error.MalformedElf;

    const sections = try alloc.alloc(Shdr, header.e_shnum);
    defer alloc.free(sections);
    for (sections, 0..) |*section, i| section.* = try read(Shdr, linked, header.e_shoff + i * @sizeOf(Shdr));
    const names = try bytes(linked, sections[header.e_shstrndx].sh_offset, sections[header.e_shstrndx].sh_size);

    // Everything up to the last byte a segment or an allocated section
    // holds stays where it is.
    var kept_end: u64 = header.e_ehsize;
    for (0..header.e_phnum) |i| {
        const segment = try read(Phdr, linked, header.e_phoff + i * @sizeOf(Phdr));
        kept_end = @max(kept_end, segment.p_offset + segment.p_filesz, header.e_phoff + header.e_phnum * @sizeOf(Phdr));
    }
    for (sections) |section| {
        if (section.sh_flags & elf.SHF_ALLOC == 0 or section.sh_type == elf.SHT_NOBITS) continue;
        kept_end = @max(kept_end, section.sh_offset + section.sh_size);
    }

    var out: std.ArrayListUnmanaged(u8) = .empty;
    errdefer out.deinit(alloc);
    try out.appendSlice(alloc, try bytes(linked, 0, kept_end));
    var new_names: std.ArrayListUnmanaged(u8) = .empty;
    defer new_names.deinit(alloc);
    try new_names.append(alloc, 0);

    for (sections[1..], 1..) |*section, i| {
        const name = try cString(names, section.sh_name);
        if (section.sh_flags & elf.SHF_ALLOC == 0 and std.mem.startsWith(u8, name, ".debug_")) {
            section.* = std.mem.zeroes(Shdr);
            continue;
        }
        section.sh_name = @intCast(new_names.items.len);
        try new_names.appendSlice(alloc, name);
        try new_names.append(alloc, 0);
        // The names are written last, once they are all known.
        if (i == header.e_shstrndx) continue;
        if (section.sh_flags & elf.SHF_ALLOC != 0 or section.sh_type == elf.SHT_NOBITS) continue;
        const data = try bytes(linked, section.sh_offset, section.sh_size);
        try pad(alloc, &out, @max(section.sh_addralign, 1));
        section.sh_offset = @intCast(out.items.len);
        try out.appendSlice(alloc, data);
    }

    // `.gnu_debuglink`: the file's name, padded to four bytes, then its CRC.
    const link_name: u32 = @intCast(new_names.items.len);
    try new_names.appendSlice(alloc, ".gnu_debuglink\x00");
    try pad(alloc, &out, 4);
    const link_offset = out.items.len;
    try out.appendSlice(alloc, debug_name);
    try out.append(alloc, 0);
    try pad(alloc, &out, 4);
    try out.appendSlice(alloc, &std.mem.toBytes(std.mem.nativeToLittle(u32, debug_crc)));
    var link = std.mem.zeroes(Shdr);
    link.sh_name = link_name;
    link.sh_type = elf.SHT_PROGBITS;
    link.sh_offset = @intCast(link_offset);
    link.sh_size = @intCast(out.items.len - link_offset);
    link.sh_addralign = 4;

    const strings = &sections[header.e_shstrndx];
    strings.sh_offset = @intCast(out.items.len);
    strings.sh_size = @intCast(new_names.items.len);
    try out.appendSlice(alloc, new_names.items);

    try pad(alloc, &out, @alignOf(Shdr));
    header.e_shoff = @intCast(out.items.len);
    header.e_shnum += 1;
    for (sections) |section| try out.appendSlice(alloc, &std.mem.toBytes(section));
    try out.appendSlice(alloc, &std.mem.toBytes(link));
    @memcpy(out.items[0..@sizeOf(Ehdr)], &std.mem.toBytes(header));
    return out.toOwnedSlice(alloc);
}

fn read(comptime T: type, file: []const u8, offset: u64) error{MalformedElf}!T {
    return std.mem.bytesToValue(T, try bytes(file, offset, @sizeOf(T)));
}

fn bytes(file: []const u8, offset: u64, len: u64) error{MalformedElf}![]const u8 {
    if (offset > file.len or len > file.len - offset) return error.MalformedElf;
    return file[@intCast(offset)..][0..@intCast(len)];
}

fn cString(names: []const u8, offset: u32) error{MalformedElf}![]const u8 {
    if (offset >= names.len) return error.MalformedElf;
    const end = std.mem.indexOfScalarPos(u8, names, offset, 0) orelse return error.MalformedElf;
    return names[offset..end];
}

fn pad(alloc: std.mem.Allocator, out: *std.ArrayListUnmanaged(u8), alignment: u64) std.mem.Allocator.Error!void {
    const target = std.mem.alignForward(u64, out.items.len, alignment);
    try out.appendNTimes(alloc, 0, @intCast(target - out.items.len));
}
