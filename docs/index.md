# Doxa

Doxa is a compiled language which focuses on simplicity, readability, and gradualized type safety. It enables seamless transition from prototype to production code through safe and normal directives.

Doxa is built using Zig. It's designed to be a highly readable language that brings gradual type safety which is controlled by directives. This allows Doxa to act as a flexible and dynamically typed scripting language while providing the ability to enforce type safety when needed.

Here is a comparison of a simple program written in Doxa's normal mode vs safe mode:

```
var x is 1;
var y is 2;

fn add(a, b) {
    return (a + b);
}

add(x, y);
```

```
#safe

fn add(a :: int, b :: int) returns(int) {
    return (a + b);
}

-> fn main() {
    var a :: int is 1;
    var b :: int is 2;

    add(a, b);
}
```

The normal mode is a superset of the safe mode. This means that any valid safe mode code is also valid normal mode code.
Transition from prototype to production code is aided by the use of the `#warn` directive. Warn mode will compile the code in
normal mode but provides warnings where safe syntax isn't followed.
