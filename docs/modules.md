# Modules

A Doxa program is a set of files. Each file is a module with its own namespace:
the names it declares and the names it imports. Nothing a file declares is
visible to another file unless that file imports it.

## Declaring what a file exports

A top-level declaration is private to its file unless it is marked `public`:

```doxa
public function area(w :: int, h :: int) returns int {
    return w * h
}

function helper() returns int {   # only this file can call it
    return 1
}

public struct Point {
    public x :: int,
    public y :: int,
}
```

A non-`public` top-level declaration is private to its file. A struct's
private members are narrower still: they belong to the struct alone, reachable
only from inside it (see [Structs](struct.md#visibility)).

## Importing

Two forms bring names into a file.

`module` binds a namespace. Its members are reached through it:

```doxa
module geometry from "./geometry.doxa"

const a is geometry.area(2, 3)
const p is $geometry.Point { x is 1, y is 2 }
```

`import` binds the named public declarations directly:

```doxa
import area, Point from "./geometry.doxa"

const a is area(2, 3)
const p is $Point { x is 1, y is 2 }
```

Both name the same entities: `geometry.Point` and an imported `Point` are one
type. An imported name may itself be a namespace — a module the target exports
with `public module` — and binds as one:

```doxa
module std from @std()            # the standard library aggregator
import io from @std()             # std's own `public module io`

io.println("hi")
std.io.println("hi")
```

A type, function, or global from another file is named only through an import
or a namespace. There is no implicit global table: two files may each declare a
`Node`, and each sees its own unless it imports the other's.

### Re-exports

`public module m from S` and `public import n from S` expose the binding to the
file's importers, which is how an aggregator such as `std/std.doxa` presents its
children.

### Specifiers

A specifier is a string. A path without an extension means `.doxa`; a `.zig`
path imports a Zig file. A relative path is looked for, in order: beside the
importing file, in a `modules/` directory beside it, then in each ancestor
directory of the importing file — never in the working directory, so a
program resolves the same files wherever `doxa` is run from.

A specifier `<root>//<path>` names a file under a declared root (see Roots)
and is looked for nowhere else: `std//io/io.doxa`, `pkg//src/util.doxa`. It is
the spelling of a module's stable name, so every module can be imported by
it. `@std()` is the string `"std//std.doxa"`, the standard library's entry
file; it imports like any other specifier.

The specifier only finds a file. A file's identity is its real path, so two
spellings of one file are one module, loaded once.

### Roots

Every file must live under a declared root: the entry file's directory
(`pkg`), the installed standard library (`std`), or a directory passed with
`--include=<dir>` (`inc0`, `inc1`, … in order). A file
outside every root is an error. A module's name in emitted code derives from its
root-relative path, never from the checkout location, so a build is identical
wherever the project lives.

## Loading

Loading is lazy. A `module` alias names its target without reading it; the file
is parsed when a name is first resolved through it, and an `import` reads the
file only to find the names it imports. Using `std.io` loads `std` and `std.io`
and leaves the rest of the standard library unread.

## Cycles

Two files may name each other with `module`: a namespace needs nothing from its
target until a member is used. An `import` cycle is an error, because each
file's imported names cannot be found until the other's are:

```
Circular import detected:
  pkg//a.doxa imports
  pkg//b.doxa imports
  pkg//a.doxa (circular dependency)
```

A name bound twice in one file — by declarations, aliases, imports, or `zig`
blocks — is an error pointing at both sites.

## Type and function declarations

`struct`, `enum`, `group`, and `function` declarations appear at the top level
of a file. One declared inside a block or function is an error (E2029).

## Zig

An inline `zig Name { … }` block is a module of its own, owned by the file that
declares it and bound in that file as the namespace `Name`. See
[Zig blocks](zig.md).
