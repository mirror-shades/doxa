# Hello World

Let's start with three ways we can print the string "Hello World" to the screen.

## Peek Operator

Doxa uses a question mark symbol as a shorthand form of print debugging. Any value can be appended with `?` and the program will return the file, line number, value name (if it exists), type, and value. Here we will simply peek at the string

```
"Hello World"?
```
```
[my_file.doxa:1:14] :: string is "Hello World"
```
Notice that there is no value name associated with this string. We can see the difference by assigning to to a variable first.
```
var hello is "Hello World"
hello?
```
```
[my_file.doxa:2:6] hello :: string is "Hello World"
```

## Intrinsic Print

There is also an intrinsic print method. All intrinsic methods are prefixed with the @ symbol.

```
@print("Hello World")
```
```
Hello World
```
Note that line breaks must be input directly with @print.

```
@print("Hello World")
@print("Hello World\n")
@print("Hello World")
```
```
Hello WorldHello World
Hello World
```

## Standard Library

All intrinsic methods are inherently unsafe however. For the best safety the standard library functions should be used.

```
module std from @std()
std.io.println("Hello World")
std.io.println("Hello World")
```
```
Hello World
Hello World
```
The `println` function appends the native line terminator for your OS (like C++ `std::endl`’s newline part) and flushes stdout. Since I/O can fail, `println` returns a union of error enums (see `std/io/io.doxa`).

If you assign that result and peek it, the debugger lists each union arm using the **registered enum names** from the standard library (for example `IO` and `Common`, matching `enum IO` / `enum Common` in `std/error/error.doxa`):

```
module std from @std()
var result is std.io.println("Hello World")
result?
```
```
Hello World
[example.doxa:3:12] result :: nothing | IO | Common is nothing
```

(Path and line numbers depend on your file. After a successful call the runtime value is `nothing`. The signature in `io.doxa` names only `error.IO | error.Common`, but the compiler widens that to **`nothing | IO | Common`** whenever the body uses a bare `return` on the success path, so peeks list every arm you can actually produce.)
