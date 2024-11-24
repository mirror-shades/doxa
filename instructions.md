note that there is a jump to get over the else block. the else block is 2 bytes long which need to be
accounted for in the else jump.

```
    const code = [_]u8{
        // Set initial value to 42
        @intFromEnum(instructions.OpCode.OP_CONST), 0,         // Push 42 (bytes 0-1)
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,      // Store in var 0 (bytes 2-3)

        // Push condition (false)
        @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push false (bytes 4-5)
        @intFromEnum(instructions.OpCode.OP_JUMP_IF_FALSE), 7, // Skip 7 bytes if false (bytes 6-7)

        // If block (7 bytes)
        @intFromEnum(instructions.OpCode.OP_CONST), 2,         // Push 99 (bytes 8-9)
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 1,       // Store in var 1 (bytes 10-11)
        @intFromEnum(instructions.OpCode.OP_VAR), 1,          // Access var 1 (bytes 12-13)
        @intFromEnum(instructions.OpCode.OP_JUMP), 2,         // Jump over else block (bytes 14-15)

        // Else block (2 bytes)
        @intFromEnum(instructions.OpCode.OP_VAR), 0,          // Access var 0 (bytes 16-17)

        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(42),       // constant[0]: initial value
        Frame.initBoolean(true), // constant[1]: condition
        Frame.initInt(99),       // constant[2]: if block value
    };
```

const and addition

```
    const code = [_]u8{
        // Set initial value to 42
        @intFromEnum(instructions.OpCode.OP_CONST), 0,
        @intFromEnum(instructions.OpCode.OP_CONST), 1,
        @intFromEnum(instructions.OpCode.OP_IADD),
        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(5),       // constant[0]
        Frame.initInt(10),       // constant[1]
    };
```

math operations

```
    const code = [_]u8{
        // Set initial value to 42
        @intFromEnum(instructions.OpCode.OP_CONST), 0,
        @intFromEnum(instructions.OpCode.OP_CONST), 1,
        @intFromEnum(instructions.OpCode.OP_IADD),
        @intFromEnum(instructions.OpCode.OP_CONST), 2,
        @intFromEnum(instructions.OpCode.OP_IMUL),
        @intFromEnum(instructions.OpCode.OP_CONST), 3,
        @intFromEnum(instructions.OpCode.OP_IDIV),
        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(5),       // constant[0]
        Frame.initInt(10),       // constant[1]
        Frame.initInt(20),       // constant[2]
        Frame.initInt(3),      // constant[3]
    };
```

set and reassign variables

```
    const code = [_]u8{
        // Set initial value to 42
        @intFromEnum(instructions.OpCode.OP_CONST), 0,
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,
        @intFromEnum(instructions.OpCode.OP_CONST), 1,
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,
        @intFromEnum(instructions.OpCode.OP_CONST), 2,
        @intFromEnum(instructions.OpCode.OP_VAR), 0,
        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(5),       // constant[0]
        Frame.initInt(10),       // constant[1]
        Frame.initInt(99),       // constant[2]
    };
```

block scope

```
    const code = [_]u8{
        // Outside block: set var[0] = 5
        @intFromEnum(instructions.OpCode.OP_CONST), 0,         // Push 5
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,      // Store in var[0]
        @intFromEnum(instructions.OpCode.OP_VAR), 0,          // Push var[0] to verify it's set

        // Begin block
        @intFromEnum(instructions.OpCode.OP_BEGIN_BLOCK),

        // Inside block: set var[1] = 10
        @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push 10
        @intFromEnum(instructions.OpCode.OP_SET_VAR), 1,      // Store in var[1]
        @intFromEnum(instructions.OpCode.OP_VAR), 1,          // Push var[1] value

        // End block (should clean up var[1])
        @intFromEnum(instructions.OpCode.OP_END_BLOCK),

        // Try to access var[1] - this should fail because it's out of scope
        @intFromEnum(instructions.OpCode.OP_VAR), 1,          // This should error!

        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(5),       // constant[0]
        Frame.initInt(10),      // constant[1]
    };
```

equality operator

```
const code = [_]u8{
    @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push first number
    @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push second number
    @intFromEnum(instructions.OpCode.OP_EQUAL),         // Compare equality
    // Could also test: OP_LT, OP_GT, OP_LTE, OP_GTE, OP_NEQ
    @intFromEnum(instructions.OpCode.OP_HALT),
};

var constants_array = [_]Frame{
    Frame.initInt(5),    // constant[0]
    Frame.initInt(6),    // constant[1]
};
```

and or not

```
    const code = [_]u8{
        @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push first boolean
        @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push second boolean
        @intFromEnum(instructions.OpCode.OP_AND),        // Logical AND
        // Could also test: OP_OR, OP_NOT
        @intFromEnum(instructions.OpCode.OP_HALT),
};

var constants_array = [_]Frame{
    Frame.initBoolean(true),     // constant[0]
    Frame.initBoolean(false),    // constant[1]
};
```

string concatenation and substring

```
const code = [_]u8{
    @intFromEnum(instructions.OpCode.OP_CONST), 0,    // Push first string
    @intFromEnum(instructions.OpCode.OP_CONST), 1,    // Push second string
    @intFromEnum(instructions.OpCode.OP_STR_CONCAT),     // String concatenation
    @intFromEnum(instructions.OpCode.OP_SUBSTR), 2, 6, // Substring
    @intFromEnum(instructions.OpCode.OP_HALT),
};

// Create constants array with error handling
var constants_array = [_]Frame{
    try Frame.initString(&interner, "Hello, "),    // constant[0]
    try Frame.initString(&interner, "World!"),     // constant[1]
};
```

array operations

```
    const code = [_]u8{
        // Create first array [42, 17]
        @intFromEnum(instructions.OpCode.OP_ARRAY_NEW), 2,   // Create array with capacity 2
        @intFromEnum(instructions.OpCode.OP_CONST), 0,       // Push 42
        @intFromEnum(instructions.OpCode.OP_ARRAY_PUSH),     // Push to array
        @intFromEnum(instructions.OpCode.OP_CONST), 1,       // Push 17
        @intFromEnum(instructions.OpCode.OP_ARRAY_PUSH),     // Push to array

        // Create second array [99]
        @intFromEnum(instructions.OpCode.OP_ARRAY_NEW), 1,   // Create second array
        @intFromEnum(instructions.OpCode.OP_CONST), 2,       // Push 99
        @intFromEnum(instructions.OpCode.OP_ARRAY_PUSH),     // Push to array

        // Concatenate arrays -> [42, 17, 99]
        @intFromEnum(instructions.OpCode.OP_ARRAY_CONCAT),   // Concat arrays

        // Get array length
        @intFromEnum(instructions.OpCode.OP_DUP),           // Duplicate array for length check
        @intFromEnum(instructions.OpCode.OP_ARRAY_LEN),     // Get length (should be 3)
        @intFromEnum(instructions.OpCode.OP_POP),           // Pop the length result

        // Modify second element (index 1) to be 55
        @intFromEnum(instructions.OpCode.OP_DUP),           // Duplicate array for modification
        @intFromEnum(instructions.OpCode.OP_CONST), 3,      // Push index 1
        @intFromEnum(instructions.OpCode.OP_CONST), 4,      // Push new value 55
        @intFromEnum(instructions.OpCode.OP_ARRAY_SET),     // Set array[1] = 55

        // Get element at index 1 (should be 55)
        @intFromEnum(instructions.OpCode.OP_DUP),           // Duplicate array
        @intFromEnum(instructions.OpCode.OP_CONST), 3,      // Push index 1
        @intFromEnum(instructions.OpCode.OP_ARRAY_GET),     // Get array[1]
        @intFromEnum(instructions.OpCode.OP_POP),           // Pop the retrieved value

        // Take slice [1:3] -> [55, 99]
        @intFromEnum(instructions.OpCode.OP_ARRAY_SLICE), 1, 3,  // Slice from index 1 to 3

        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    var constants_array = [_]Frame{
        Frame.initInt(42),    // constant[0]
        Frame.initInt(17),    // constant[1]
        Frame.initInt(99),    // constant[2]
        Frame.initInt(1),     // constant[3] - index for array access
        Frame.initInt(55),    // constant[4] - new value for array[1]
    };
```

struct operations

```
    const code = [_]u8{
        // Create a new struct of type "Person"
        @intFromEnum(instructions.OpCode.OP_STRUCT_NEW), 3, 0,  // 3 fields, type name at constant[0]

        // Set name field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push field name "name"
        @intFromEnum(instructions.OpCode.OP_CONST), 2,         // Push value "John Doe"
        @intFromEnum(instructions.OpCode.OP_SET_FIELD),        // Set the field

        // Set age field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 3,         // Push field name "age"
        @intFromEnum(instructions.OpCode.OP_CONST), 4,         // Push value 30
        @intFromEnum(instructions.OpCode.OP_SET_FIELD),        // Set the field

        // Set active field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 5,         // Push field name "active"
        @intFromEnum(instructions.OpCode.OP_CONST), 6,         // Push value true
        @intFromEnum(instructions.OpCode.OP_SET_FIELD),        // Set the field

        // Get and check age field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 3,         // Push field name "age"
        @intFromEnum(instructions.OpCode.OP_GET_FIELD),        // Get the field value
        @intFromEnum(instructions.OpCode.OP_POP),              // Pop the retrieved value

        // Get and check name field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push field name "name"
        @intFromEnum(instructions.OpCode.OP_GET_FIELD),        // Get the field value
        @intFromEnum(instructions.OpCode.OP_POP),              // Pop the retrieved value

        // Get and check active field
        @intFromEnum(instructions.OpCode.OP_DUP),              // Duplicate struct reference
        @intFromEnum(instructions.OpCode.OP_CONST), 5,         // Push field name "active"
        @intFromEnum(instructions.OpCode.OP_GET_FIELD),        // Get the field value
        @intFromEnum(instructions.OpCode.OP_POP),              // Pop the retrieved value

        @intFromEnum(instructions.OpCode.OP_HALT),
    };

    // Create the constants array with all needed values
    var constants_array = [_]Frame{
        try Frame.initString(&interner, "Person"),    // constant[0] - struct type name
        try Frame.initString(&interner, "name"),      // constant[1] - field name
        try Frame.initString(&interner, "John Doe"),  // constant[2] - name value
        try Frame.initString(&interner, "age"),       // constant[3] - field name
        Frame.initInt(30),                            // constant[4] - age value
        try Frame.initString(&interner, "active"),    // constant[5] - field name
        Frame.initBoolean(true),                      // constant[6] - active value
    };
```

enum and match

```
const code = [_]u8{
    // First set up our enum variable
    @intFromEnum(instructions.OpCode.OP_CONST), 0,         // Push enum value ONE
    @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,       // Store in var[0]

    // Now do the match
    @intFromEnum(instructions.OpCode.OP_VAR), 0,           // Load x onto stack
    @intFromEnum(instructions.OpCode.OP_MATCH),            // Start match

    // Pattern ONE
    @intFromEnum(instructions.OpCode.OP_MATCH_ARM), 0, 5,  // Compare with variant 0, jump 5 bytes if no match
    @intFromEnum(instructions.OpCode.OP_CONST), 1,         // Push result 1
    @intFromEnum(instructions.OpCode.OP_JUMP), 10,         // Jump to end

    // Pattern TWO
    @intFromEnum(instructions.OpCode.OP_MATCH_ARM), 1, 5,  // Compare with variant 1, jump 5 bytes if no match
    @intFromEnum(instructions.OpCode.OP_CONST), 2,         // Push result 2
    @intFromEnum(instructions.OpCode.OP_JUMP), 5,          // Jump to end

    // Else branch
    @intFromEnum(instructions.OpCode.OP_CONST), 3,         // Push result 0

    @intFromEnum(instructions.OpCode.OP_MATCH_END),        // End match
    @intFromEnum(instructions.OpCode.OP_HALT),
};

var constants = [_]Frame{
    Frame{  // constant[0] = Enum.ONE
        .value = .{
            .type = .ENUM,
            .data = .{ .enum_val = .{ .type_name = "Number", .variant = 0 } },
            .nothing = false
        },
        .allocator = null,
        .owns_value = true,
    },
    Frame{  // constant[1] = 1
        .value = .{
            .type = .INT,
            .data = .{ .int = 1 },
            .nothing = false
        },
        .allocator = null,
        .owns_value = true,
    },
    Frame{  // constant[2] = 2
        .value = .{
            .type = .INT,
            .data = .{ .int = 2 },
            .nothing = false
        },
        .allocator = null,
        .owns_value = true,
    },
};
```

try catch block

```
const code = [_]u8{
    // Set up a variable that will cause an error
    @intFromEnum(instructions.OpCode.OP_CONST), 0,    // 0: Push zero sized array onto stack
    @intFromEnum(instructions.OpCode.OP_SET_VAR), 0,  // 2: Store in var[0]

    @intFromEnum(instructions.OpCode.OP_TRY),         // 4: Start try block
    @intFromEnum(instructions.OpCode.OP_VAR), 0,      // 5: Load array
    @intFromEnum(instructions.OpCode.OP_CONST), 1,    // 7: Push index 999
    @intFromEnum(instructions.OpCode.OP_ARRAY_GET),   // 9: This will throw
    @intFromEnum(instructions.OpCode.OP_JUMP), 3,     // 10: Skip catch block (jump to END_TRY)

    @intFromEnum(instructions.OpCode.OP_CATCH),       // 12: Start catch block
    @intFromEnum(instructions.OpCode.OP_CONST), 2,    // 13: Push -1

    @intFromEnum(instructions.OpCode.OP_END_TRY),     // 15: End try-catch
    @intFromEnum(instructions.OpCode.OP_HALT),        // 16: Stop execution
};

var constants = [_]Frame{
    Frame{ // constant[0] = empty array
        .value = .{
            .type = .ARRAY,
            .data = .{ .array_val = array_val },
            .nothing = false,
        },
        .allocator = null,  // Don't set allocator since we're handling cleanup here
        .owns_value = false, // Don't own the value since we're handling cleanup here
    },
    Frame{ // constant[1] = index 999
        .value = .{
            .type = .INT,
            .data = .{ .int = 999 },
            .nothing = false,
        },
        .allocator = null,
        .owns_value = true,
    },
    Frame{ // constant[2] = error result (-1)
        .value = .{
            .type = .INT,
            .data = .{ .int = -1 },
            .nothing = false,
        },
        .allocator = null,
        .owns_value = true,
    },
};
```
