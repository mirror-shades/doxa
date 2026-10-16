# Debugging Doxa programs

Doxa writes standard debug information and leaves debugging to the tools you
already have. It ships no debugger of its own.

## Building with debug info

Every `run` and `compile` carries debug information unless you pass
`--strip`:

```
doxa compile main.doxa -o bin/main          # with debug info
doxa compile --strip main.doxa -o bin/main  # without
```

The binary carries a line table for every `.doxa` file it was built from:
DWARF on Linux and macOS, and on Windows a `.pdb` written beside the binary.
It costs the program nothing: the machine code is the same with and without
it, but for one call at start-up that turns backtraces on. Nor does it cost
measurable compile time. `--strip` leaves the program's code without debug
information and its faults without a backtrace.

What a debugger sees:

- **Functions** under the names the program gives them: `scale`, or
  `Point.norm` for a method. DWARF also records each one's mangled link name
  (`__doxa_m1_…`) as its linkage name.
- **Lines and columns.** A step moves one statement at a time and stops at
  the statement's start, such as the `if` of `if x > k`. A crash, a breakpoint
  or a profiler sample inside a statement is attributed to the expression that
  was running.
- **Locals, parameters and globals** under the names the program spells: a
  `var`, a parameter, `this`, a `^` parameter, a binding an `as` or a `match`
  arm declares, and a module-level global. A local lent as `^` is read from its
  storage; every other is named where it is defined, so an assignment inside a
  branch shows the value it holds there.
- **Types.** `int`, `float` and `byte` are their scalar types; `tetra` and
  every enum are enumerations of their values; a `string` is its pointer and
  length; a struct is its fields, each at the offset the runtime lays it at; a
  union or group is its box. A fixed array expands element by element. A
  dynamic array points at its header: `len` is its length and `data` points at
  its elements, so view `data` as an array of `len` to browse them. A map shows
  its length, but its entries are not browsable yet. A function value is a
  pointer to its signature, and each function's own signature is described.
- **Optimized builds** (`--opt=1..3`) keep what survives optimization. That is
  the usual optimized-debugging experience: a statement the optimizer merged
  away has no stop of its own, and a value it folded away has no name.

## Backtraces on fault

A program follows a [runtime fault](methods.md#runtime-faults)'s line with the
Doxa frames that led to it, innermost first:

```
Index out of bounds at b.doxa:2:12: index 7, length 3
  in get at b.doxa:2:12
  in outer at b.doxa:6:5
  in main at b.doxa:11:5
```

Each frame is named as the program spells it, at the line and column of its
call. The runtime's own frames are left out, and so is the call into the entry
function. A fault while a global's initializer runs shows that initializer as
`<globals>`. Optimized builds keep the frames that survive inlining, and
CodeView often records no column there (`b.doxa:2:0`). The trace goes to stderr
and doesn't change the exit status. Under `--strip` the fault prints its line
alone.

On Windows the trace currently ends with one more frame, `<globals>` at line 1:
CodeView cannot mark the call into the entry as having no source line.

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
optimized build (`--opt=2`): it measures the program you ship.

A sampler answers *how long*. To learn *how often*, instrument the build:

```
doxa run fib.doxa --instrument=calls
doxa report fib
```

```
==========================================
Calls
==========================================
Built at -O0.

  177	fib	fib.doxa:1:10
  1	main	fib.doxa:6:23
```

Each function counts its own entries, hottest first. `--json` prints the same
data for tooling. An instrumented program writes its counts when it ends:
returning from the entry, at `@exit`, or at a fault. They go to
`<binary>.profdata`, beside the `<binary>.profmap` the compiler wrote. For
`doxa run` that is the cache, so `doxa report <stem>` finds both there. For
`doxa compile -o out/fib`, it is `out/`, read with `doxa report out/fib`. A
report refuses counts from another build of the program. At `--opt=1` and up,
an inlined function runs no entry of its own, so its calls count toward its
caller. An instrumented binary is a diagnostic build: never time it.

`--instrument=lines` counts each statement every time it runs, listed by its
start (`file:line:column`) and its function, hottest first. Members combine:
`--instrument=calls,lines` reports both tables. A loop's header counts once,
as the loop statement; its condition and step run with the loop and have no
row of their own.

## Static findings

`--emit-complexity` asks the compiler what each loop repeats, without running
anything:

```
doxa run loops.doxa --emit-complexity
doxa report loops
```

The report lists each function's deepest loop nest, every allocation or heap
store made inside a loop with the arena it lands in, calls inside loops to
functions that themselves allocate or loop, and copies the compiler left to a
runtime check inside a loop. An allocation marked `!` lands where no iteration
of its loop reclaims it, such as an element pushed into an array declared
outside the loop. That memory grows with the loop's iterations and is held
until its arena closes. The operations are named as `--emit-hir` names them
(`array_push`, `str_concat`). The findings are written beside the binary as
`<binary>.complexity` and change nothing in the program. When the same build
is also instrumented, one `doxa report` shows the counts and the findings
together, and `--json` prints both.
