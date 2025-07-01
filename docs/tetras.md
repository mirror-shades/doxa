# Doxa Language Reference

## Tetras

### Type overview

A tetra is a type which represents four cornered logic. You can think of a tetra like an extended boolean type which would represent two cornered logic. The four states which tetras can exist in are:

`true` - These first two values should be familiar  
`false`  
`both` - This is equivilant to both true _and_ false  
`neither` - This is equivilatnt neither to true _or_ false

### An Example

Let's start with a condition, in this case, 'living'. In traditional boolean logic there would be two states for this condition, true or false. But what about logical conditions outside of true and false? Here is a four cornered representation of the 'living' condition:

```
const Bob is true;
const Shakespere is false;
const zombie is both;
const angel is neither;
```

Now let's try running these values through some logic:

```
function living(alive :: tetra) {
    if alive then "I am alive"?;
    if not alive then "I am dead"?;
}
```

If you are following along, here is what I called and the output I received:

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

The output here might seems strange but the logic behind it is very simple. True only triggers conditions which are true. False only triggers conditions which are false. Both will trigger both the condition regardless if it is true or false. Neither will never trigger a condition. This means zombie triggers both the `alive` condition and its negation `not alive`. Angel triggers neither and so we see no output.

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

### First order logic

Doxa has extensive for traditional first order logics that work as expected with true and false values. These can be represented in formal unicode notation:

```
const arr :: int[] = [1, 2, 3, 4, 5]

// existential quantifier ∃, element of ∈
∃x ∈ arr : x > 3; // true

// universal quantifier ∀, where :
∀x ∈ arr : x > 3; // false

// NOT ¬
¬false; // true

// biconditional ↔
false ↔ false; // true

// XOR ⊕
true ⊕ true; // false

// AND ∧
true ∧ false; // false

// OR ∨
true ∨ false; // true

// NAND ↑
true ↑ false; // true

// NOR ↓
true ↓ false; // false

// implication →
true → false; // false
```

This unicode support is paired with plaintext keywords which act in an identical fashion:

```
∃ - exists
∀ - forall
∈ - in
: - where
¬ - not
↔ - iff
⊕ - xor
∧ - and
∨ - or
↑ - nand
↓ - nor
→ - implies
```

This means formal logical representation can be written in either way:

```
const arr :: int[] = [1, 2, 3, 4, 5]

¬(∀x ∈ arr : x > 3); // true
not (forall x in arr where x > 3); // true
```
