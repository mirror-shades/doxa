# Maps

Maps can be used to create dictionaries between key/value pairs. Maps are mutable an can be modified during runtime. 
Key and value type is inferred from the first entry if not explicitly declared.

```doxa
map Dict {
    1 is "first"
    2 is "third"
}

Dict[2] is "second"
```

### Explicit Typing

For empty maps, explicit key and/or value types are required. The syntax mirrors function declarations:

```doxa
const map PlayerToData :: Player returns Data {
    .Main is mainData
}
```

### Accessing Maps

```doxa
map MyMap {
    "foo" then "bar"
}

var entry is MyMap["foo"]

entry? // union of string | nothing
const narrow_entry is entry as string else return MyEnum.FoundNothing
narrow_entry? // "bar"
```

### Union Returns

Return types can be return unions for more flexibility on value storage.

```doxa
map CarSeats returns string | nothing {
    1 is "Bob",
    2 is nothing,
    3 is "Sam"
    4 is nothing
}
```

### Notes on Inference

