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

```
@print("Hello World")
```