In Doxa, all conditionals are expressions, so they return a value.
this allows a unified syntax for conditionals and assignments.

```
var x = if (y == 1) then 1 else 2;

var x = if (y == 1) { 1 } else { 2 };
```

The following are functionally equivalent

```
var x;
if (y == 1) then x = 1 else x = 2;

var x;
if (y == 1) { x = 1 } else { x = 2 };
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
var x = if (false) { y = 1 };
var x = if (false) { y = 1 } else nothing;
```
