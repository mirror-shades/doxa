# Loops

Doxa provides three types of loops: `while`, `for`, and `each`. While `while` and `for` work like traditional loops in most languages, `each` offers a more modern, collection-focused approach.

## While

The `while` loop continues executing as long as a condition remains true:

```
var currentNumber is 0
while currentNumber <= 100 {
    currentNumber += 1
}
```

## For

The `for` loop uses traditional C-style syntax with initialization, condition, and increment:

```
for (var i is 0; i < x; i++) {
    doSomething(myArray[i])
}
```

## Each

The `each` loop is Doxa's modern iteration construct for collections. It's cleaner and safer than indexed loops:

```
each x in arr {
    doSomething(x)
}
```

### Each with Index

You can also access the index using the `at` keyword:

```
each x at i in arr {
    if i < 3 then doSomething(x)
}
```

The `each` loop eliminates the need for manual index management and reduces the risk of off-by-one errors that are common with traditional `for` loops.
