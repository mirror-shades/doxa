Since Doxa has a tightly limited number of types, structs are useful for modeling more complex data. Structs are based off composition to avoid pitfalls inherent to inheretance. Fields, functions, and methods are all private unless marked `public`.

Struct instantiation **always** requires a `$` sigil prefix. An identifier followed by `{` without `$` is always parsed as an implicit match expression — never as a struct literal.

### Real world example

```doxa
struct Point {
    x :: int, # unless marked public, fields are private
    y :: int,

    public id :: int,

    public function New(x :: int, y :: int) returns Point { # functions are static
        return $Point { # note the $ prefix
            x is x,
            y is y,
            id is 0,
        }
    }

    function safeSub(a :: int, b :: int) returns int { # private: callable only inside Point
        const result is a - b
        if result > 255 or result < 0 then return -1
        return result
    }

    public method getDelta() returns int { # a method has a receiver, `this`
        return Point.safeSub(this.x, this.y)
    }
}

const p is Point.New(5, 2)
p.getDelta()? # 3
```

### Visibility

A private member belongs to its struct alone:

- A private field is read and written only through `this` (`this.x`), so only
  in the struct's own methods.
- A private method is called only through `this` (`this.reset()`).
- A private function is called through `this` or through the type
  (`Point.safeSub(...)`) from inside the struct.
- A struct literal may set a private field only inside the struct, so a struct
  with private fields is built by its own functions.

Nothing else reaches a private member — not other structs in the same file, and
not module-level functions. A struct's function has no `this`; only a method
does.

### Basic Composition Example

```doxa
struct Animal {
    public name :: string,
}

struct Dog {
    # Composition instead of inheritance
    animal :: Animal,
    breed :: string,

    public function new(name :: string, breed :: string) returns Dog {
        return $Dog {
            animal is $Animal { name is name },
            breed is breed,
        }
    }

    public method bark() {
        @print("{this.animal.name} says woof!\n")
    }
}

const dog is Dog.new("Spot", "Labrador")
dog.bark() # Spot says woof!
```

### Performance

Structs are allocated from the current scope's arena. All field values are stored in a single contiguous pool alongside field metadata, giving excellent cache locality. Structs created and assigned within the same scope benefit from same-scope move semantics — no deep copy occurs when the source and destination share an arena.
```
