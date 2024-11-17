strict mode is signalled by a declaration at the top of the file #strict

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

## Doxa strict syntax

strict mode is signalled by a declaration at the top of the file #strict
strict files can only include other strict files

all variables are typed by default

constants cannot be declared without a value and a type

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

## General syntax

both `||` and `&&` are supported alongside `and` and `or`. a clarity warning is given if both are used in the same file.

No operator or function overloading.

No classes, only structs and composition.

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
