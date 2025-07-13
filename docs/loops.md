# Loops

## While

var currentNumber is 0;
while currentNumber <= 100 {
currentNumber += 1;
}

## For

```
for (var i is 0; i < x; i++) {
    doSomething(myArray[i]);
}
```

## Each

```
each x in arr {
    doSomething(x);
}

// at specifies an index
    each x at i in arr {
    if i < 3 then doSomething(x);
}
```
