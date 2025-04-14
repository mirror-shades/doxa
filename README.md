# Doxa Programming Language

Doxa is a simple programming language with an interpreter and compiler implementation in Zig.

## Components

- **Lexer**: Tokenizes source code
- **Parser**: Builds an AST from tokens
- **Interpreter**: Directly executes AST nodes (mature implementation)
- **Compiler**: Compiles AST to bytecode (work in progress)
- **VM**: Executes bytecode (work in progress)

## Usage

### Running a Doxa Program (Interpreted Mode)

```bash
zig build run -- program.doxa
```

Or with debug output:

```bash
zig build run -- --debug program.doxa
```

### Compiling a Doxa Program (Compilation Mode - WIP)

The compilation mode is still under development.

```bash
zig build run -- --compile program.doxa
```

## Development Status

- ✅ **Interpreter**: Fully functional, supports all language features
- 🚧 **Compiler**: Basic implementation, some features missing
- 🚧 **VM**: Basic implementation, needs integration with compiler

## Core Concepts

### Value Types

- Numbers (integers and floats)
- Strings
- Booleans
- Arrays
- Maps (dictionaries)
- Structs
- Functions

### Control Flow

- If/else statements
- While loops
- For loops
- Try/catch for error handling

## Example

```
// Variable declaration
var x is 5;
const PI is 3.14159;

// Function definition
function add(a, b) {
    return a + b;
}

// Using control flow
if (x > 3) then {
    ("x is greater than 3")?;
} else {
    ("x is not greater than 3")?;
}

// Loops
while (x > 0) {
    (x)?;
    x -= 1;
}

// Arrays
var arr is [1, 2, 3, 4, 5];
print(arr[0]);  // Prints: 1
```

## Architecture

The Doxa implementation consists of:

1. **Tokenizer**: Converts source code to tokens
2. **Parser**: Converts tokens to an AST
3. **Compiler**: Converts AST to bytecode (optional)
4. **VM**: Executes bytecode (when in compiled mode)
5. **Interpreter**: Directly executes AST (when in interpreted mode)
