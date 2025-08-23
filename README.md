# Doxa Programming Language [[Docs]](https://mirror-shades.github.io/doxa/)[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/mirror-shades/doxa)

Doxa is inspired by Nagarjuna's four cornered logic, known as Catuṣkoṭi. Doxa does not use bools but instead tetras. A tetra (short for tetralemma) is a logical value with four possible states or corners:

```
P (true)
¬ P (false)
P ∧ ¬ P (both)
¬ ( P ∨ ¬ P ) (neither)
```

See documentation for a more detailed explaination of how this works.

## Some cool stuff

peek operator

union types

deterministic memory management

## Components

![Pipeline](./pipeline.svg)

- **Lexer**: Tokenizes source code
- **Parser**: Builds an AST from tokens
- **Soxa** A stack based HIR for high level optimization
- **VM**: Stack based, operates from Soxa IR
- **LLVM IR** Code generator to turn Soxa into LLVM IR for native compilation

## Memory management

Memory is managed but not via garbage collection. Doxa uses an automated refence counter much like Swift but with some unique changes which leverage the power of the Zig language allocation system. Without getting overly technical, each scope is allocated as an arena which is cleaned up when that scope is exited. Combined with traditional refrence counting this provides an extremely robust and _predictable_ form of automatic memory management which avoids many of the pitfalls of garbage collection while remaining totally automatic.

## Usage

```bash
zig build run -- [--debug] ./path/to/file.doxa
```

## Development Status

- ✅ **AST Interpreter**: Fully functional, supports all language features, will be deprecated for VM
- ✅ **Soxa VM**: currently implementing stack based HIR and VM
- ❌ **LLVM IR**: Minimal prototyping, many features missing, future

## Native Types

### Atomic

- int (32-bit integer)
- float (64-bit float)
- byte (8-bit uint hex literal)
- string
- tetra (four-value logic unit)
- nothing (void type)

### Molecular

- array (homogeneous)
- map
- struct
- enum
- union
- function

### Meta

- alias (pass-by-alias for mutation)
- intrinsic (@length, @bytes, etc.)

## Example

```solidity
// a brainfuck interpreter implemented in doxa
// mirror-shades

const symbols is [ ">", "<", "+", "-", ".", ",", "[", "]" ]
var tape :: byte[10]
var loops :: int is 0
var loopSpot :: int[]
var tp :: int is 0
var ip :: int is 0

fn getInput() -> byte {
    var userInput :: string is @input()
    var newByte :: byte is @bytes(userInput)[0]
    return newByte
}

fn startLoop() {
    if @length(loopSpot) == loops then {
        @push(loopSpot, ip)
    } else {
        loopSpot[loops] is ip
    }
    loops += 1
}

fn endLoop() {
    if loops >= 0 then {
        if tape[tp] == 0 then {
            loops -= 1
        } else {
            const loopPointer is loops - 1
            ip is loopSpot[loopPointer]
            // cancels the ip += 1 from the main loop
            ip -= 1
        }
    }
}

fn checkClosingBracket(scan :: string) -> tetra {
    var pointer :: int is 0
    var openBrackets :: int is 0
    while(pointer < @length(scan)) {
        if(scan[pointer] == "[") then openBrackets += 1
        if(scan[pointer] == "]") then openBrackets -= 1
        pointer += 1
    }
    return(openBrackets == 0)
}


fn interpret(scan :: string) {
    const scanLength is @length(scan)

    var closedBrackets :: tetra is checkClosingBracket(scan)
    @assert(closedBrackets, "Unmatched brackets")

    while(ip < scanLength) {
        var currentInstruction is scan[ip]
        if(currentInstruction == ">") then tp += 1
        if(currentInstruction == "<") then tp -= 1
        if(currentInstruction == "+") then tape[tp] += 1
        if(currentInstruction == "-") then tape[tp] -= 1
        if(currentInstruction == ".") then tape[tp]?
        if(currentInstruction == ",") then tape[tp] is getInput()
        if(currentInstruction == "[") then startLoop()
        if(currentInstruction == "]") then endLoop()
        ip += 1
    }
}

entry fn main() {
    interpret(",+.")
}

```
