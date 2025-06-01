# Doxa Language Reference

## Tetras

### Type overview

A tetra is a type which represents four cornered logic. You can think of a tetra like an extended boolean type which would represent two cornered logic. The four states which tetras can exist in are:

`true` - These first two values should be familiar  
`false`  
`both` - This is equivilant to both true _and_ false  
`neither` - This is equivilatnt neither to true _or_ false

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
