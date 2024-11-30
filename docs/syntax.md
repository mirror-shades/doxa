By default, Doxa runs in normal mode. Strict mode is enabled via `#strict` at the file start.
To preserve type safety, strict files can only import other strict files. There is an intermediate
mode called `warn` which compiles files in normal mode but gives warnings when strict conventions are not followed.

## General syntax

both `||` and `&&` are supported alongside `and` and `or`. a clarity warning is given if both are used in the same file.

No operator or function overloading.

Memory management is handled via garbage collection.

No classes, only structs and composition.

Try catch is based on Zig's error union model which allows for safer error handling.

```
fn divide(a: int, b: int) !int {
    if (b == 0) return error.DivisionByZero;
    return a / b;
}

// Usage
var result = try divide(10, 0);
// or
var result = divide(10, 0) catch |err| {
    log.error("Failed to divide: {}", err);
    return;
};
```

Doxa has some choice code keywords:

- `function` and `fn` are functionally identical
- `and` and `&&` are functionally identical
- `or` and `||` are functionally identical

warnings are given for projects that mix symbolic and keyword conditionals.

```
struct Animal {
    name: string
}

struct Dog {
    // Composition instead of inheritance
    animal: Animal,
    breed: string,

    fn bark(self) {
        print("${self.animal.name} says woof!");
    }
}

var dog = Dog {
    animal: Animal { name: "Spot" },
    breed: "Labrador"
};
```

modules are supported. strict files can only import other strict files.
normal files can import both strict and normal files.

```
// math.doxa
#strict

// Explicit exports
export fn add(a: int, b: int) -> int {
    return a + b;
}

// main.doxa
import { add } from "./math.doxa";

// Explicit usage
var sum = add(1, 2);
```

match is supported rather than switch. match cases must be exhaustive but
else is supported as a fallback.

```
enum Number {
    ONE,
    TWO,
    UNKNOWN
}

match x {
    .ONE => print("one"),
    .TWO => print("two"),
    .UNKNOWN => print("unknown")
}

// this is functionally identical to the previous example
match x {
    .ONE => print("one"),
    .TWO => print("two"),
    else => print("unknown")
}
```

all cases must be covered

```
//this is an error, as one or more cases are not covered
match x {
    .ONE => print("one"),
    .TWO => print("two")
}
```

arrays can only be declared with a homogeneous type. Heterogeneous arrays
are not supported. Structs are the preferred way to group different types.

```
// Only homogeneous arrays allowed
var nums: int[] = [1, 2, 3];       // OK
var strs: string[] = ["a", "b"];   // OK

// These would be errors
var mixed = [1, "two", true];      // Error: mixed types in array
var noType = [1, 2, 3];            // Error: array type must be explicit
```

## Doxa normal syntax

in normal mode, all variables are dynamically typed by default.
this can be explicitly marked with the auto type. this allows for
cross type assignments.

```
var x = 1;                //int
x = true;                 //bool

var x: auto = 3.14;      //float
x = "pi";                //string
```

constants are declared with the const keyword

```
const x = 1;              //int
const x = "two";          //string
const x = [1, 2, 3];      //array
```

constants cannot be declared without a value but
type can be inferred like variables.

```
const x;                  //error, needs a value
const x = 1;              //int
const y: auto = 3.14;    //float
```

type declarations are allowed. when a type is declared safety is enforced.

```
var x: int = 1;           //int
x = "two";                //error, expected type int
```

variables can be declared without a value

```
var x: int;               //int
var x: string;            //string
var x: float;             //float
var x: bool;              //bool
var x: []int;             //array
```

variables can be declared without a type

```
var x;                    //auto
var x: auto;              //auto
```

cross-type assignments are allowed for auto variables

```
var x = 5;                //auto
x = "five";               //auto
```

arrays can be declared either with a homogeneous type or a mixed type

## Doxa strict syntax

constants must be declared with a type and a value

```
const x;                  //error, needs a type and a value
const x: int;             //error, needs a value
```

variables can be declared without a value as long as they are given an explicit type.
if a variable is declared without a type, it and error is thrown. There is an auto type
which can be used however type safety is still enforced.

```
var x: int;               //int
var x = "two";            //error, needs explicit type
var x: auto = 3.14;       //float
```

variables can be declared without a value as long as they are given an explicit type

```
var x: int;               //int
var x: string;            //string
var x: float;             //float
var x: bool;              //bool
var x: []int;             //array
```

variables cannot be declared without a type

```
var x;                    //error, needs explicit type
var x: auto;              //error, needs explicit type
```

Auto will default the variable to the type of the initial value. no cross-type
assignments are allowed for strict variables

```
var x: auto = 5;          //int
x = "five";               //error, given value does not match type int
```

Arrays must be explicitly typed.

```
var x: int[] = [1, 2, 3];   //OK
var x = [1, 2, 3];         //error, needs explicit type
```

## Conditionals

In Doxa, all conditionals are expressions, so they return a value.
this allows a unified syntax for conditionals and assignments.

All of the following are functionally equivalent:

```
var a;
if false then a is 5
else if true then a is 20
else a is 30;

var b;
if (false) then {b = 5;}
else if (true) then {b = 20;}
else {b = 30;}

var c is
if false then 5
else if true then 20
else 30;

var d =
if (false) then {5;}
else if (true) then {20;}
else {30;};
```

if an if expression doesn't return a value, it defaults to `nothing`.

```
var x = if (y == 1) { y = 1 } else { y = 2 };
print(x); // prints the 'nothing' value

var x = if (false) { y = 1 } else {};
print(x); // prints the 'nothing' value
```

if statements without an else branch default to `nothing`. the following are functionally equivalent:

```
var x = if (false) { y = 1 }; // warning raised about possible 'nothing' return value
var x = if (false) { y = 1 } else nothing;
```
