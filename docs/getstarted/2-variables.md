# Variables

In Doxa variables are assigned and reassigned using the keyword `is`. This was chosen over an `=` because of the bugs inherent to confusing assignment `=` with equality `==`. By disallowing single `=` we avoid this class of error entirely.

Doxa has two kinds of variable declaration, `var` which is mutable, and `const` which is constant. All variable declarations will be prefaced with one of these two keywords.

```
var x is 5
const y is 10
x is x + y
x?
```
```
[./my_file.doxa:4:2] x :: int is 15
```
If we tried to reassign the variable `y`, which is marked constant, we would get the following error:
```
DoxVM: [CompileTime][Error][E1015] ./my_file.doxa:3:1: Cannot assign to immutable variable 'y'
   2 | const y is 10
   3 | y is x + y
     | ^ Error: Cannot assign to immutable variable 'y'
   4 | y?
```

## Typing

When declaring a variable, a type inference will be made. If you would like a variable to be uninitiallized, it must be provided a type using `::`. All variables have static types known at compile time meaning you cannot reassign a variable to a value of a different type.

```
var x :: int is 5
var y is 5 # this is inferred as an int
var z :: int # this is initialized with a default value of 0
```

Uninitialized variables are given a default value depending on their type. Read the doc page for more details.