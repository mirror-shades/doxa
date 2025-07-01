# Doxa Language Reference

## Tetras

### Type overview

A tetra is a type which represents four cornered logic as opposed to Booles binary logic. You can think of a tetra a bit like an extended boolean type. Where bools can only exist in only two states, true or false, tetras can exist in four. The four states which tetras can exist in are:

`true` - These first two values should be familiar  
`false`  
`both` - This is equivilant to both true _and_ false  
`neither` - This is equivilatnt neither to true _or_ false

### An Example

Let's start with a condition, in this case, the condition of being alive. In traditional boolean logic there would be only two states for this condition, true or false, or in term of the condition, alive or dead. But a tetra can represent logical conditions outside of this true/false, alive/dead binary.

Here are four values to represent the four possible states or corners of a tetra:

```
const Bob is true;
const Shakespere is false;
const zombie is both;
const angel is neither;
```

Now let's create some conditional logic to understand how a tetra works. Here is a function to discover whether something is alive or not:

```
function living(alive :: tetra) {
    if alive then "I am alive"?;
    if not alive then "I am dead"?;
}
```

Notice that if this function was in another language and was using a boolean, there would only be two possible branches when executed. If you would like to follow along, here is how I ran the values through the function:

```
"Bob"?;
living(Bob);
"Shakespere"?;
living(Shakespere);
"zombie"?;
living(zombie);
"angel"?;
living(angel);
```

And here is the ouput I received:

```
[C:\dev\zig\doxa\test.doxa:11:6] value = "Bob"
[C:\dev\zig\doxa\test.doxa:7:31] value = "I am alive"
[C:\dev\zig\doxa\test.doxa:13:13] value = "Shakespere"
[C:\dev\zig\doxa\test.doxa:8:34] value = "I am dead"
[C:\dev\zig\doxa\test.doxa:15:9] value = "zombie"
[C:\dev\zig\doxa\test.doxa:7:31] value = "I am alive"
[C:\dev\zig\doxa\test.doxa:8:34] value = "I am dead"
[C:\dev\zig\doxa\test.doxa:17:8] value = "angel"
```

The output here might seems strange but the logic behind it is very simple. True only triggers conditions which are true. False only triggers conditions which are false. Both will trigger both the condition regardless if it is true or false. Neither will never trigger a condition.

This means zombie triggers both the `alive` condition and its negation `not alive`.

```
[C:\dev\zig\doxa\test.doxa:15:9] value = "zombie"
[C:\dev\zig\doxa\test.doxa:7:31] value = "I am alive"
[C:\dev\zig\doxa\test.doxa:8:34] value = "I am dead"
```

Angel triggers neither and so we see no output from either branch.

```
[C:\dev\zig\doxa\test.doxa:17:8] value = "angel"
```

Keep in mind, a tetra is a value. It doesn't actually alter the logic flow of your program, it's purpose is simply to follow the logic flow in ways basic true and false cannot.

Special attention should (not) be paid to the else clause. The else clause is exactly what it sounds like - "if a condition is ment, then do x, OR ELSE, do y". As we now know, both will always trigger a condition, and neither will never trigger a condition. The effect with else blocks is then as you would assume, both with never trigger an else clause, and neither will always trigger an else clause.

```
const zombie is both;
const angel is neither;

function living(alive :: tetra) {
    if alive then "I am alive"?; else "I am not alive"?;
    if not alive then "I am dead"?; else "I am not dead"?;
}

"zombie"?;
living(zombie);
"angel"?;
living(angel);
```

```
[C:\dev\zig\doxa\test.doxa:9:9] value = "zombie"
[C:\dev\zig\doxa\test.doxa:5:31] value = "I am alive"
[C:\dev\zig\doxa\test.doxa:6:34] value = "I am dead"
[C:\dev\zig\doxa\test.doxa:11:8] value = "angel"
[C:\dev\zig\doxa\test.doxa:5:55] value = "I am not alive"
[C:\dev\zig\doxa\test.doxa:6:57] value = "I am not dead"
```
