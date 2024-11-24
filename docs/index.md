# Doxa 

Doxa is a highly flexible compiled language focused on simplicity, readability, and gradualized type safety. It enables seamless transition from prototype to production code through strict and normal directives. 

Doxa is built using Zig. It's designed to be a highly readable language that brings gradual type safety which is controlled by directives. This allows Doxa to act as a flexible and dynamically typed scripting language while providing the ability to enforce type safety when needed.

Here is a comparison of a simple program written in Doxa's normal mode vs strict mode:

```
var x = 1;
var y = 2;

fn add(a, b) {
    return (a + b);
}

add(x, y);
```


```
#strict

fn add(a: int, b: int) returns(int) {
    return (a + b);
}

-> fn main() {
    var a: int = 1;
    var b: int = 2;

    add(a, b);
}
```
