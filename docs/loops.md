# Loops

Doxa takes a unique approach to loops. The best way to think about it is that the loop syntax is fully composible. The structure of the full loop will be similar to anyone familiar to C style loops:

```
// doxa
for x while x < 10 do x += 1 {
  ...
}
// c
for(int x, x < 10; x++) {

}
```

Each section of the above the for loop works the same, the key difference being each section is marked by its own keyword. This main advantage this provides is that each section of the for loop becomes decomposable. Doxa allows you to use any combination of loop constructs to add functionality to the loop as needed. Regardless of which construct you use, it will always follow the syntactical precedent of C loops: for -> while -> do.

## Core Loop Constructs

### For Loops

**Purpose**: Loop which has a scope-level int which persists between loops. The variable is always an int will be defaulted to 0 if not initialized.

```
for hand {
  hand += draw_card();
  if hand > 21 then bust();
  else if hand > 17 then hold();
  else hit();
}
```

### While Loops

**Purpose**: Condition-based loop that executes while a condition is true.

```doxa
while <condition> { ... }
```

**Examples**:

```doxa
// With early exit
while has_next() {
  const item is next()
  if item == 0 then break
}
```

### Do Loops

**Purpose**: Step-focused loop that executes a step following each iteration. Following C traditions, steps always execute after each iteration of the loop body, before the next condition check.

```doxa
do <step> { ... }
```

**Examples**:

```doxa
// Basic do loop
const the_time is getTheTime()
do the_time is getTheTime() {
  if the_time > expiration_time then break
}

// complex do block
do {
  if getTheTime() > the_time + 100 then {
    the_time = getTheTime()
  } else log_too_early()
} {

  if the_time > expiration_time then {
    break
  } else {
    log_loop()
  }
}
```

## Infinite Loops

You can use `while true` or just `do` for infinite loops:

```doxa
while true { ... }  // Traditional infinite loop
do { ... }          // Cleaner do-based infinite loop
```

## Loop Combinations

### While Do Loop

```doxa
// Countdown from 10 to 1, then liftoff
var countdown is 10
while countdown > 0 do {
    countdown -= 1
} {
    "T-minus " + countdown + " seconds"?
}
"Liftoff!"?
```

### For Do Loop

```doxa
var sensor_reading is 0
for reading do reading = get_reading() {
    if reading > 7 then {
        @print("Warning: reading is too high!")
        break
    }
}
```

### For While Do Loop

```doxa
// Default initialization: process tasks until done
var tasks = ["Write report", "Submit assignment", "Attend meeting"]
var index is 0  // we start at index 0
for index while index < tasks.length do index += 1 {
    "Task: " + tasks[index]?
}

// Explicit initialization with complex do block
for i is 0 while i < @length(tasks) do {
    // Update: move to next task and log progress
    i += 1
    @print("Progress: {i} / {@length(tasks)}\n")
} {
    @print("Now doing: tasks[i]\n")
}
```

## Loop Table

Here are all the possible combinations for loops using these constructs. Notice that regardless of which constructs you choose, they will be ordered for -> while -> do.

Loops with an int exposed:
`for x`

Loops while a given condition met  
`while y`

Loops using a given step  
`do z`

Loops while a given condition met using a given step  
`while y do z`

Loops with an index using a given step  
`for x do z`

Loops with an int exposed while a given condition met using a given step  
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

**Each Loop Notes**:

- The `at` variable provides an immutable copy of the current index
- Nested loops create independent index copies
- Modifying the collection during iteration may cause undefined behavior

## Control Flow

All loops support:

- **`break`**: Exit the loop immediately
- **`continue`**: Skip to next iteration

Continues will run any given `do` step before skipping to the next iteration of the loop.
