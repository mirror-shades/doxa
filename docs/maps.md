# Maps

### Simple Mapping (Immutable)

Maps can store key-value pairs. By default, using const makes the map immutable.

```doxa
const map Entries {
    1 is "wow",
    2 is "cool",
}
```

### Mutable Maps

Using var allows the map data to be updated.

```doxa
var map Dict {
    1 is "first"
    2 is "third"
}

Dict[2] is "second"
```

### Explicit Typing (Optional)

If types cannot be inferred from the first entry or for empty maps, explicit key and/or value types are required. The syntax mirrors function declarations:

```doxa
const map PlayerToData :: Player returns Data {
    .Main is mainData
}
```

### Union Returns

Return types can be return unions for more flexibility on value storage.

```doxa
var map CarSeats returns string | nothing {
    1 is "Bob",
    2 is nothing,
    3 is "Sam"
    4 is nothing
}
```

### Accessing Maps

```doxa
var entry is MyMap["troll"]
var entry is MyMap[.Troll]

// if mutable
MyMap["troll"].hp is 100
```

### Notes on Inference

Key type is inferred from the first entry if not explicitly declared.

Value type is encouraged to be explicit (returns <ValueType>).

Empty maps cannot be inferred and must have both key and value types declared.
