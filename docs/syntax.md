By default, Doxa runs in normal mode. Strict mode is enabled via `#strict` at the file start. To preserve type safety, strict files can only import other strict files. There is an intermediate mode called `warn` which compiles files in normal mode but gives warnings when strict conventions are not followed.

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

modules are supported. strict files can only import other strict files. normal files can import both strict and normal files.

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

match is supported rather than switch.

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
```

else is supported

```
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


arrays can only be declared with a homogeneous type. Heterogeneous arrays are not supported. Structs are the preferred way to group different types.

```
// Only homogeneous arrays allowed
var nums: []int = [1, 2, 3];       // OK
var strs: []string = ["a", "b"];   // OK

// These would be errors
var mixed = [1, "two", true];      // Error: mixed types in array
var noType = [1, 2, 3];            // Error: array type must be explicit
```

## Doxa normal syntax

constants are declared with the const keyword

```
const x = 1;              //int
const x = "two";          //string
const x = 3.14;           //float
const x = true;           //bool
const x = [1, 2, 3];      //array
```

constants cannot be declared without a value

```
const x;                  //error, needs a value
```

all variables are auto by default

```
var x = 1;                //auto
var x = "two";            //auto
var x = 3.14;             //auto
var x = true;             //auto
var x = [1, 2, 3];        //auto
```

type declarations are allowed

```
var x: int = 1;           //int
var x: string = "two";    //string
var x: float = 3.14;      //float
var x: bool = true;       //bool
var x: []int = [1, 2, 3]; //array
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

```
const x;                  //error, needs a type and a value
const x: int;             //error, needs a value
```

constants can be declared with an auto type

```
const x: auto;            //auto
```

variables can be declared without a value as long as they are given an explicit type

```
var x: int;               //int
var x = "two";            //error, needs explicit type
var x = 3.14;             //error, needs explicit type
var x = true;             //error, needs explicit type
var x = [1, 2, 3];        //error, needs explicit type
```

type declarations are required, same as normal syntax

```
var x: int = 1;           //int
var x: string = "two";    //string
var x: float = 3.14;      //float
var x: bool = true;       //bool
var x: []int = [1, 2, 3]; //array
```

variables can be declared without a value as long as they are given an explicit type

```
var x: int;               //int
var x: string;            //string
var x: float;             //float
var x: bool;              //bool
var x: []int;             //array
```

auto variables can be declared without a type but it is cast to the type of the value

```
var x: auto = 5;          //int
var x: auto = "five";     //string
var x: auto = 3.14;       //float
var x: auto = true;       //bool
var x: auto = [1, 2, 3];  //array
```

variables cannot be declared without a type

```
var x;                    //error, needs explicit type
var x: auto;              //error, needs explicit type
```

no cross-type assignments are allowed for strict variables

```
var x: auto = 5;          //int
x = "five";               //error, given value does not match type int
```
## Conditionals

In Doxa, all conditionals are expressions, so they return a value.
this allows a unified syntax for conditionals and assignments.

All of the following are functionally equivalent:

```
var x = if y == 1 then 1 else 2;

var x = if (y == 1) then 1 else 2;

var x = if (y == 1) then { 1 } else { 2 };

var x;
if y == 1 then x = 1 else x = 2;

var x;
if (y == 1) then x = 1 else x = 2;

var x;
if (y == 1) then { x = 1 } else { x = 2 };
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
