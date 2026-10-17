//! The `doxa --help` text.

const std = @import("std");

pub fn print() void {
    std.debug.print("Doxa Programming Language\n", .{});
    std.debug.print("\nUsage:\n", .{});
    std.debug.print("  doxa init [project-name]        # Scaffold a new Doxa project\n", .{});
    std.debug.print("  doxa run [general options] <file.doxa>\n", .{});
    std.debug.print("  doxa compile [general options] <file.doxa> -o <output> [compile options]\n", .{});
    std.debug.print("  doxa report <binary> [--json]   # Show what --instrument / --emit-* found\n", .{});
    std.debug.print("  doxa --lsp [--lsp-debug-io]     # Start the Language Server Protocol loop\n", .{});
    std.debug.print("  doxa --lsp-debug <file.doxa>    # Run the in-process LSP debug harness\n", .{});
    std.debug.print("\nGeneral options:\n", .{});
    std.debug.print("  --profile                         # Print a per-phase compilation profile\n", .{});
    std.debug.print("  --profile-out=<path>              # Also write the profile as JSON to <path>\n", .{});
    std.debug.print("  --help, -h                        # Show this help message\n", .{});
    std.debug.print("  --debug-[stage]                   # Enable debug output for [stage]\n", .{});
    std.debug.print("                                    # lexer, parser, semantic, hir, memory\n", .{});
    std.debug.print("  --debug-verbose                   # Enable all debug output\n", .{});
    std.debug.print("  --cache-dir=<dir>                 # Build cache directory (default: .doxa-cache)\n", .{});
    std.debug.print("  --strip                           # Omit debug info (on by default: debugger, profiler, backtraces)\n", .{});
    std.debug.print("  --instrument=calls,lines,memtrace # Count calls / statements / memory by site; read with doxa report\n", .{});
    std.debug.print("  --instrument=memcheck             # Fault on a read of memory an arena reclaimed\n", .{});
    std.debug.print("  --emit-complexity                 # Write static loop findings; read with doxa report\n", .{});
    std.debug.print("  --emit-memory                     # Write where allocations live and every copy; read with doxa report\n", .{});
    std.debug.print("\nCompile options:\n", .{});
    std.debug.print("  -o, --output <path>               # Output executable path (required)\n", .{});
    std.debug.print("  --arch=<arch>                     # Target CPU architecture (default: host)\n", .{});
    std.debug.print("  --os=<os>                         # Target operating system (default: host)\n", .{});
    std.debug.print("  --abi=<abi>                       # Target ABI (optional)\n", .{});
    std.debug.print("  --link=<name>                     # Link a native library (-l<name>); repeatable\n", .{});
    std.debug.print("  --libdir=<dir>                    # Library search path (-L<dir>); repeatable\n", .{});
    std.debug.print("  --framework=<name>                # Link a macOS framework; repeatable\n", .{});
    std.debug.print("  --include=<dir>                   # Header search path (-I<dir>); repeatable\n", .{});
    std.debug.print("  --opt-mode=<debug|safe|fast|small># Zig release mode for the runtime and link step\n", .{});
    std.debug.print("  -O0..-O3 | --opt=0..3             # clang -O level for the program (-O2 == zig cc -O2)\n", .{});
    std.debug.print("  --emit-opt-ir                     # Also write optimized LLVM IR (<stem>.opt.ll) to cache\n", .{});
    std.debug.print("  --emit-asm                        # Also write target assembly (<stem>.s) to cache\n", .{});
    std.debug.print("  --emit-hir                        # Also write the register HIR (<stem>.hir) to cache\n", .{});
    std.debug.print("  --lsp-debug-io                    # Trace raw LSP I/O when used with --lsp\n", .{});
    std.debug.print("\nExamples:\n", .{});
    std.debug.print("  doxa run file.doxa\n", .{});
    std.debug.print("  doxa compile file.doxa -o bin/myapp\n", .{});
    std.debug.print("  doxa compile file.doxa -o bin/myapp --arch=x86_64 --os=linux -O2\n", .{});
}
