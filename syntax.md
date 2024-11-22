# Doxa

Doxa is a highly flexible compiled language focused on simplicity, readability,
and gradualized type safety. It enables seamless transition from prototype to
production code through strict and normal directives.

By default, Doxa runs in normal mode. Strict mode is enabled via `#strict` at the
file start. To preserve type safety, strict files can only import other strict files.
There is an intermediate mode called `warn` which compiles files in normal mode but
gives warnings when strict conventions are not followed.

## General syntax

Entry point can be declared with the `->` operator. if no entry point is declared in normal
mode, the file runs line by line as a script. an entry point must be declared in strict
mode.

The following are interchangeable: `function` and `fn`, `and` and `&&`, `or` and `||`.

If both syntaxes are used in the same file, a clarity warning is given.

No operator or function overloading.

Memory management is handled via garbage collection.

No classes, only structs and composition.

Try catch is based on Zig's error union model which allows for safeetr

Doxa has some choice code keywords:

- `function` and `fn` are functionally identical
- `and` and `&&` are functionally identical
- `or` and `||` are functionally identical

warnings are given for projects that mix symbolic and keyword conditionals.

There are no classes or inheritance. Structs can be used to compose
complex types and methods can be added to structs.

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

default typing is public but private fields are supported.

```
struct User {
    name: string,
    email: string,

    fn get_hash(self) -> string {
        return self.password_hash;  // OK - internal access
    }

    private:
        password_hash: string,
}

var user = User {
    name: "John",
    email: "john@example.com",
    password_hash: "abc123"  // Error cannot set private field
};

print(user.password_hash);  // Error cannot access private field
print(user.get_hash());     // OK - using public method
```

Private and public work as switches and apply to all values and methods below.
This provides a lot of flexibility on organization.

```
struct User {
    name: string // public by default
    email: string // public by default

    fn get_hash(self) -> returns(string) {  // OK public by default
        return self.password_hash;
    }

    private:
        password_hash: string, // private by assignment

        fn get_user(self) -> User { // private by assignment
            return self;
        }

    public:
        get_hash: fn(self: User) -> string, // public by assignment again

}
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

functions can use default parameters but the use of default arguments is not
supported. all default arguments are explicitly declared using the `~` operator.

```
fn add(a: int = 1, b: int) -> int {
    return a + b;
}


add(~, 2); // returns 3
add(2, 2); // returns 4
add(2, ~); // error, b is not a default parameter
add(2);    // error, expected 2 arguments, got 1
```

arrays can only be declared with a homogeneous type. Heterogeneous arrays
are not supported. Structs are the preferred way to group different types.

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

```
var x: []int = [1, 2, 3]; //homogeneous
var x: []auto = [1, "two", true]; //mixed
```

if additional type safety is needed, functions can be marked as `safe` which
will check the types of all arguments and return values.

```
fn add(a, b) {
    return a + b;
}
add(1, 2); // Returns 3
add(1, "2"); // Runtime error: Cannot add int and string
add("1", "2"); // Returns "12" (string concatenation)

// this fails at compile time due to undefined parameter and return types
safe fn add(a, b) {
    return a + b;
}

safe fn add(a: int, b: int) returns(int) {
    return a + b;
}
add(1, 2); // Returns 3
add(1, "2"); // Runtime error: expected int, got string
add("1", "2"); // Runtime error: expected int, got string
```

## Doxa strict syntax

Doxa strict is similar to normal syntax but with much more strict type safety.
Use the `#warn` directive to help gradually transition normal files to strict mode.

constants must be declared with a type and a value

```
const x; //error, needs a type and a value
const x: int; //error, needs a value
```

constants can be declared with an auto type

```
const x: auto; //auto
```

variables can be declared without a value as long as they are given an explicit type

```
var x: int; //int
var x = "two"; //error, needs explicit type
var x = 3.14; //error, needs explicit type
var x = true; //error, needs explicit type
var x = [1, 2, 3]; //error, needs explicit type
```

type declarations are required, same as normal syntax

```
var x: int = 1; //int
var x: string = "two"; //string
var x: float = 3.14; //float
var x: bool = true; //bool
var x: []int = [1, 2, 3]; //array
```

variables can be declared without a value as long as they are given an explicit type

```
var x: int; //int
var x: string; //string
var x: float; //float
var x: bool; //bool
var x: []int; //array
```

auto variables can be declared without a type but it is cast to the type of the value

```
var x: auto = 5; //int
var x: auto = "five"; //string
var x: auto = 3.14; //float
var x: auto = true; //bool
var x: auto = [1, 2, 3]; //array
```

variables cannot be declared without a type

```
var x; //error, needs explicit type
var x: auto; //error, needs explicit type
```

no cross-type assignments are allowed for strict variables

```
var x: auto = 5; //int
x = "five"; //error, given value does not match type int
```

all functions must be declared with parameter and return types

```
fn add(a, b) { //error, undefined parameter and return types
    return a + b;
}

fn add(a: int, b: int) returns(int) {
    return a + b;
}
```
