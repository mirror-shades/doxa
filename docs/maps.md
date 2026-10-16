# Maps

Maps can be used to create dictionaries between key/value pairs. Each entry is
written `key then value`; entries are separated by newlines or commas. Maps are
mutable and can be modified during runtime: assigning to a key updates it, or
adds it if it is missing.

```doxa
map Dict {
    1 then "first"
    2 then "third"
}

Dict[2] is "second"
Dict[3] is "third"
```

### Explicit Typing

The key type is declared with `:: <Type>` and the value type with
`returns <Type>`, mirroring function declarations. An empty map must declare
both:

```doxa
enum Player { Main, Rival }

map PlayerToScore :: Player returns int {
    .Main then 10
}

map Scores :: string returns int {}
Scores["Bob"] is 3
```

The declared key type also gives `.Variant` keys their enum, so `.Main` above
needs no `Player.` prefix.

### Accessing Maps

A lookup returns the value type joined with `nothing`, since the key may be
missing. Narrow it with `as` before using it as a plain value:

```doxa
enum Lookup { Found, FoundNothing }

map MyMap {
    "foo" then "bar"
}

function find(key :: string) returns Lookup {
    var found is MyMap[key]
    found? # string | nothing
    const narrow is found as string else return Lookup.FoundNothing
    narrow? # "bar"
    return Lookup.Found
}

find("foo")?  # .Found
find("nope")? # .FoundNothing
```

### Default Values

An `else` entry gives the value a lookup of a missing key returns:

```doxa
map Names :: int returns string {
    1 then "Bob"
    else "unknown"
}

Names[1]? # "Bob"
Names[9]? # "unknown"
```

### Union Returns

The value type can be a union for more flexibility on value storage.

```doxa
map CarSeats :: int returns string | nothing {
    1 then "Bob",
    2 then nothing,
    3 then "Sam",
    4 then nothing
}
```

### Notes on Inference

Without explicit types, the key and value types are inferred from the first
entry, and every later entry must match them: `map M { 1 then "a", 2 then 3 }`
is rejected (E1003) because `3` is not a `string`. A union value type is never
inferred, so a map that stores more than one value type must declare it, as
`CarSeats` does above. A map with no entries and no declared types is an error.

### Identity

A map is a program-level declaration. Putting it in an array, a struct field
or a variable holds that same map, never a copy, so a write through any of
them changes the declared map:

```doxa
map Global :: int returns string {
    1 then "g"
}

var all is [Global]
all[0][1] is "changed"   # Global[1] is now "changed"
```

### Performance

Maps are allocated from the current scope's arena. All keys and values are stored in contiguous pools — two slices holding all keys and all values respectively. Map mutations are performed in-place: setting a value for an existing key updates the value slot directly without copying the map. New entries extend the pools via realloc.

