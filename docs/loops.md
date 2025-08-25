# Loops

Doxa provides two core loop constructs: `while` and `do`. The `for` keyword is optional and provides a loop variable that can be used with either loop type. This design allows flexible loop syntax that can simulate C-style `for` loops when needed.

## Core Loop Constructs

### While Loops

**Purpose**: Condition-based loop that executes while a condition is true.

```doxa
while <condition> { ... }
```

**Examples**:

```doxa
// Basic while loop
while i < 10 {
  print(i)
  i += 1
}

// With early exit
while has_next() {
  const item is next()
  if item == target { break }
}
```

### Do Loops

**Purpose**: Step-focused loop that executes a step following each iteration. Following C traditions, steps always execute after each iteration of the loop body, before the next condition check.

```doxa
do <step> { ... }
```

**Examples**:

```doxa
// Basic do loop with increment
var i is 0
do i += 1 {
  i? // 0, 1, 2...
}

// Do-while equivalent
var i is 0
do i += 1 while i < 10 {
  i? // 0, 1, 2... 9
}

// Complex step logic
do {
  i += fibonacci(j)
  j += 1
} while i < limit { ... }
```

## Loop Variables with `for`

The `for` keyword optionally declares a mutable integer variable which is exposed to the loop. `for` can only be used with a corresponding `do`.

```doxa
for i while <condition> { ... }      // for + while
for i do <step> { ... }               // for + do
for i while <condition> do <step> { ... }  // full C-style syntax
```

**Examples**:

```doxa
// Simple for loop (simulates for(i=0; i<10; i++))
for i while i < 10 do i++ {
  print(i)  // i automatically increments
}

// Custom step
for i while i < 20 do i += 2 {
  print(i)  // i = 0, 2, 4, 6, ...
}

// complex step
for i while i < 100 do {
    if i > 10 then i += 2 else i++
    if i >= 100 then checkWinCondition()
    } {
  print(i)  // i = 0, 10, 20, 30, ...
}
```

## Infinite Loops

Use `while true` or just `do` for infinite loops:

```doxa
while true { ... }  // Traditional infinite loop
do { ... }          // Cleaner do-based infinite loop
```

## Loop Combinations

Here are all the possible combinations for loops using these constructs. Notice that regardless of which constructs you choose, they will be ordered for -> while -> do.

Loops while a given condition met  
`while y`

Loops using a given step  
`do z`

Loops while a given condition met using a given step  
`while y do z`

Loops with an index using a given step  
`for x do z`

Loops with an index while a given condition met using a given step  
`for x while y do z`

## Collection Iteration

### Each Loops

`each` provides syntactic sugar for iterating over collections:

```doxa
each item in collection { ... }
each item at index in collection { ... }
```

**Expands to**:

```doxa
for i while i < @length(collection) do i++ {
  item is collection[i]
  // or with index:
  const index is i
  ...
}
```

**Notes**:

- The `at` variable provides an immutable copy of the current index
- Nested loops create independent index copies
- Modifying the collection during iteration may cause undefined behavior

## Control Flow

All loops support:

- **`break`**: Exit the loop immediately
- **`continue`**: Skip to next iteration

Continues will run any given `do` step before skipping to the next iteration of the loop.
