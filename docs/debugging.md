# Debugging Doxa programs

Doxa writes standard debug information and leaves debugging to the tools you
already have. It ships no debugger of its own.

## Building with debug info

Pass `-g` to `run` or `compile`:

```
doxa compile -g main.doxa -o bin/main
```

The binary then carries a line table for every `.doxa` file it was built from:
DWARF on Linux and macOS, and on Windows a `.pdb` written beside the binary.
`-g` is off by default. Without it the program's code carries no debug
information, and the emitted code is identical to a build that never had the
flag.

What a debugger sees:

- **Functions** under the names the program gives them: `scale`, or
  `Point.norm` for a method. DWARF also records each one's mangled link name
  (`__doxa_m1_…`) as its linkage name.
- **Lines and columns.** A step moves one statement at a time and stops at
  the statement's start, such as the `if` of `if x > k`. A crash, a breakpoint
  or a profiler sample inside a statement is attributed to the expression that
  was running.
- **Optimized builds** (`--opt=1..3`) keep what survives optimization. That is
  the usual optimized-debugging experience: a statement the optimizer merged
  away has no stop of its own.

Local variables and Doxa types are not described yet.

## Finding the source

Debug information names each file by its path inside the project
(`main.doxa`, `lib/util.doxa`), and a standard-library file by its place in
`std` (`std//io/io.doxa`). It never records a path on the machine that built
it, so a binary built in one checkout debugs the same in another. Point your
debugger at the project directory once: `directory <project>` in gdb,
`settings set target.source-map` in lldb, or the source-file search path in
Visual Studio and WinDbg. Windows tools show such a file under a drive root,
as `C:\main.doxa`.

## Profiling

The same line table lets a sampling profiler (perf, ETW/WPA, VTune,
Superluminal, Instruments) attribute samples to `.doxa` lines. Profile an
optimized build (`-g --opt=2`): it measures the program you ship.
