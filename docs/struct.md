Since Doxa has a tightly limited number of types, structs are useful for modeling more complex data. Structs are based off composition to avoid pitfalls inherent to inheretance. Variables, functions, and methods are all private unless marked pub.

Struct instantiation uses a `$` sigil prefix to diferentiate it from a normal identifier. 

### Real world example

struct Point {  
    x :: int, // unless marked pub, values are private
    y :: int,

    pub id :: int,

    pub function New(x :: int, y :: int) { // fucntions and methods are private unless marked pub as well
        return $Point { // note the $ prefix
            x is x,
            y is y,
        }
    }

    function safeSub(a :: int, b :: int) returns int { // normal functions within structs are static
        result is a - b
        if result > 255 or result < 0 then return -1
        return result
    }

    pub method get() { // the `method` keyword lets you access 
        return $Point {
            this.x,
            this.y
        }
    }

    pub method getDelta() returns int {
        return Point.safeSub(this.x, this.y) // static methods can be called within instance methods
    }

}

### Basic Composition Example

```doxa
struct Animal {
    name :: string
}

struct Dog {
    // Composition instead of inheritance
    animal :: Animal,
    breed :: string,

    pub method bark() {
        @print("{this.animal.name} says woof!")
    }
}

var dog is $Dog {
    animal is $Animal {
        name is "Spot"
        },
    breed is "Labrador"
}

dog.bark() // Spot says woof!
```
