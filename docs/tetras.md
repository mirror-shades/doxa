# Doxa Language Reference

## Tetras

### Type overview

A tetra is a type which tries to represent four cornered logic rather than traditional binary logic. You can think of a tetra a like an extended boolean type. In its simplest sense, where bools can exist in only two states, true or false, tetras can exist in four:

`true` - These first two values should be familiar  
`false`  
`both` - This is equivalent to both true _and_ false  
`neither` - This is equivalent neither to true _or_ false

### An Example

Let's start with a condition, in this case, the condition of being alive. In traditional boolean logic there would be only two states for this condition, true or false, alive or dead. But a tetra can represent logical conditions outside of this true/false, alive/dead binary.

Here are four values to represent the four possible states or corners of a tetra:

```
const Bob is true
const Shakespeare is false
const zombie is both
const angel is neither
```

Now let's create some conditional logic to understand how a tetra works. Here is a function to discover whether something is alive or not. It takes a single tetra for input then decides if something is alive or not based on whether it is true or false. it then compares the two flags, isAlive and isDead  in order to determine the state.

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

You might notice this function could be written without the flags, by comparing the state of the tetra itself. We are using flags here to deconstruct it in a way we might parse this to using traditional boolean logic. Here is an execution of the function:

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

When deconstructed like this we can see the logic is relatively simple. True only triggers conditions which are true. False only triggers conditions which are false. Both will always trigger a condition. Neither will never trigger a condition. 

A purely functional way to imagine this is that both is a true value which cannot be negated, and neither is a false value which cannot be negated.

Keep in mind, a tetra is a value just like a boolean is. A tetra doesn't alter the flow of conditional branches, rather it provides states which trigger conditionals in a paradoxical way.

We treat the else clause as exactly what it sounds like: "if a condition is met, then do x, or else do y". Since both will always trigger a condition, and neither will never trigger a condition, both will never trigger the else clause, and neither will always trigger the else clause.

### A use case

Where a tetra really excels is dealing with fuzzy or ambigious data. Take a common example of ambigous data, API calls. Here we are fetching data from an API whose validity we may not be sure of. The call will be stored in a struct which uses a tetra called valid: 

true mean the data was valid
false means the data was invalid
both means the data was ambigous, possibly valid, possibly not
neither means no data was returned

This approach lets us handle ambigous data without using flags or nested if statements:

```
struct Call {
    data :: string
    valid :: tetra
}

const result :: Call is apiCall()

if not result.valid then log(result) # if the result is not valid or ambigous it is logged 
if result.valid then serveData(result) # if the result is valid or ambigous it is served
# if no data the conditionals do not trigger and the result falls through
```

## Intentionality

In order to reduce complexity adding contradition into the system must be intentional. There are two ways of adding contradition (both or neither) into the system. First, you can set a tetra manually as both or neither. Second, you can use a special operator called paradoxical and, which will take the input (true and false) as both. Other than this expect a traditional binary result (true or false). All first order logic in doxa necessarily returns a value of true or false.