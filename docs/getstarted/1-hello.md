# Hello World

Let's start with three ways we can print the string "Hello World" to the screen.

## Peek Operator

Doxa uses a question mark symbol as a shorthand form of print debugging. Any value can be appended with `?` and the program will return the file, line number, value name (if it exists), type, and value. Here we will simply peek at the string literal.

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
Peeking is a safe operation with no side effects and can be done without any risk of runtime errors.

## Intrinsic Print

There is also an intrinsic print method provided at compiler level. All intrinsic methods are prefixed with the @ symbol. The `@print` method does what it says, takes a string and prints it to the screen.

```
@print("Hello World")
```
```
Hello World
```
Note that line breaks must be handled manually with `@print`.

```
@print("Hello World")
@print("Hello World")
```
```
Hello WorldHello World
```
Intrinsic methods like `@print` are unsafe, and so improper use can cause unexpected errors including crashes. This should be kept in mind whenever using compiler level intrinsic methods.

## Standard Library

The standard library provides safe print functions in the `io` module. Here we will use `io.println`. This function handles new lines in a cross platform friendly way so we don't have to worry about manually inserting line breaks.

```
module std from @std()
std.io.println("Hello World")
std.io.println("Hello World")
```
```
Hello World
Hello World
```
If we peek at the values returned we can see that the intrinsic `@print()` method simply returns `nothing`, while `std.io.println` returns a union of `nothing | StdError`. This is what lets us unwrap a failed function call safely. We will learn more about this when we look at unions.
```
module std from @std()
var intrinsic_result is @print("Hello World\n")
intrinsic_result?
var std_result is std.io.println("Hello World")
std_result?
```
```
Hello World
[my_file.doxa:3:17] intrinsic_result :: nothing is nothing
Hello World
[my_file.doxa:5:11] std_result :: >nothing | StdError is nothing
```

