# Doxa Language Reference

## Tetras

### Type overview

A tetra is a type which represents four cornered logic as opposed to binary boolean logic. You can think of a tetra a bit like an extended boolean type. Where bools can exist in only two states, true or false, tetras can exist in four. The four states which tetras can exist in are:

`true` - These first two values should be familiar  
`false`  
`both` - This is equivalent to both true _and_ false  
`neither` - This is equivalent neither to true _or_ false

### An Example

Let's start with a condition, in this case, the condition of being alive. In traditional boolean logic there would be only two states for this condition, true or false, or in terms of the condition, alive or dead. But a tetra can represent logical conditions outside of this true/false, alive/dead binary.

Here are four values to represent the four possible states or corners of a tetra:

```
const Bob is true
const Shakespeare is false
const zombie is both
const angel is neither
```

Now let's create some conditional logic to understand how a tetra works. Here is a function to discover whether something is alive or not. It takes a single tetra for input then decides if something is alive or not based on whether it is true or false. it then compared the two flags, isAlive and isDead to determine the state.

```
function living(alive :: tetra) returns string {
    var isAlive is false
    var isDead is false

    if alive then isAlive is true
    if not alive then isDead is true

    if isAlive and isDead then return "is alive and dead"
    if isAlive then return "is alive"
    if isDead then return "is dead"
    return "neither alive or dead"
}
```

Notice that if this function could be written without the flags, by comparing the state of the tetra itself. We are using flags here to deconstruct it the way we would have to using traditional boolean logic. Here is an execution of the function:

```
@print("Bob {living(Bob)}\n")
@print("Shakespeare {living(Shakespeare)}\n")
@print("zombie {living(zombie)}\n")
@print("angel {living(angel)}\n")
```

And here is the output I received:

```
Bob is alive
Shakespeare is dead
zombie is alive and dead
angel neither alive or dead
```

When deconstructed like this we can see the logic is relatively simple. True only triggers conditions which are true. False only triggers conditions which are false. Both will trigger conditions even when negated. Neither will not trigger a condition even when negated.

Keep in mind, a tetra is a value. It doesn't alter the logical flow of conditionals, rather it triggers those conditionals in a paradoxical fashion.

Special attention should (not) be paid to the else clause. The else clause is exactly what it sounds like: "if a condition is met, then do x, OR ELSE, do y". Since both will always trigger a condition, and neither will never trigger a condition, else blocks work as you would assume, both will never trigger the else clause, and neither will always trigger the else clause.

### A use case

Where a tetra really excels is dealing with fuzzy or ambigious data. Take a common example of ambigous data, API calls. Here we are fetching data from an API whose validity we may not be sure of. The call will be stored in a struct which uses a tetra called valid: 

true mean the data was valid
false means the data was invalid
both means the data was ambigous, possibly valid, possibly not
neither means no data was returned

This approach lets us handle ambigous data without using flags or nested if statements. Remember, a both value triggers all conditionals, a neither will never trigger a conditional.

```
struct Call {
    data :: string
    valid :: tetra
}

const result :: Call is apiCall()

if not result.valid then log(result) // if the result is not valid or ambigous it is logged 
if result.valid then serveData(result) // if the result is valid or ambigous it is served
// if no data the conditionals do not trigger and the result falls through
```
