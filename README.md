# Doxa Programming Language [[Docs]](https://mirror-shades.github.io/doxa/)[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/mirror-shades/doxa)

Doxa is inspired by Nagarjuna's four cornered logic, known as Catuṣkoṭi. Doxa does not use bools but instead tetras. A tetra (short for tetralemma) is a logical value with four possible states or corners:

```
P (true)
¬ P (false)
P ∧ ¬ P (both)
¬ ( P ∨ ¬ P ) (neither)
```

Doxa is high level, statically typed, memory managed language. It has a highly readable and consistant syntax aimed at reducing cognative load, and producing code which is simple and aesthetic while remaining type safe. It features an extended logical value called a tetra, as well as a full suite of first order logic operators including existential and universal quantifers. 


## Usage

```bash
Usage:
  doxa [run | compile] [options] <file.doxa>        # Execute with HIR VM (explicit)

Options:
  --debug-[stage]                   # Enable debug output for [stage]
                                    # lexer, parser, semantic, hir, bytecode, execution
  --debug-verbose                   # Enable all debug output
  --profile                         # Enable profiling
  --help, -h                        # Show this help message

Examples:
  doxa run file_name.doxa           # Execute the script
  doxa compile file_name.doxa       # Compile to object file
```
    
### Building from source

Current build uses Zig 0.15.1. The only dependancy other than the Zig compiler is the built in runtime modules, at this time currently Raylib. The runtime libraries are installed during the build process. Eventually a real FFI can replace these runtime dependancies altogether.

compile from source and run a file

```bash
zig build run -- [--debug] ./path/to/file.doxa
```

be sure to build before running compiler tests

```bash
zig build
zig build test
```

## Native Types

Doxa is based upon a very small number of types with enums, structs, and type unions providing a huge degree of flexibility to how these core types can be used. Exhaustive match statements and union type narrowing allow for extremly simple yet powerful error handling patterns that takes the idea of errors as values very literally.

### Atomic

- int (64-bit integer)
- float (64-bit float)
- byte (8-bit uint hex literal)
- string
- tetra (four-value logic unit)
- nothing (void type)

### Molecular

- array (homogeneous)
- struct
- enum
- map
- union

## Components

![Pipeline](./pipeline.svg)

- **Lexer**: Tokenizes source code
- **Parser**: Builds an AST from tokens
- **Soxa** A stack based HIR for high level optimization
- **VM**: Stack based, operates from Soxa IR
- **LLVM IR** Code generator to turn Soxa into LLVM IR for native compilation

## TODO:

- standard lib
- add instrospective key access while using each loops over maps using `at`
- finish the last few internal methods 
- SOXA to LLVMIR code gen

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

function getInput() returns byte {
    @print("Input: ")
    var userInput :: string is @input()
    var newByte :: byte is @byte(userInput[0]) as byte else 0x00
    return newByte
}

function startLoop() {
    if @length(loopSpot) == loops then {
        @push(loopSpot, ip)
    } else {
        loopSpot[loops] is ip
    }
    loops += 1
}

function endLoop() {
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

function checkClosingBracket(scan :: string) returns tetra {
    var pointer :: int is 0
    var openBrackets :: int is 0
    while(pointer < @length(scan)) {
        if(scan[pointer] == "[") then openBrackets += 1
        if(scan[pointer] == "]") then openBrackets -= 1
        pointer += 1
    }
    return(openBrackets == 0)
}

function interpret(scan :: string) {
    const scanLength is @length(scan)

    var closedBrackets :: tetra is checkClosingBracket(scan)
    @assert(closedBrackets, "Unmatched brackets")

    while(ip < scanLength) do ip += 1 {
        var currentInstruction is scan[ip]
        if(currentInstruction == ">") then tp += 1
        if(currentInstruction == "<") then tp -= 1
        if(currentInstruction == "+") then tape[tp] += 0x01
        if(currentInstruction == "-") then tape[tp] -= 0x01
        if(currentInstruction == ".") then @print("Output: {tape[tp]}\n")
        if(currentInstruction == ",") then tape[tp] is getInput()
        if(currentInstruction == "[") then startLoop()
        if(currentInstruction == "]") then endLoop()
    }
}

entry function main() {
    interpret(",+.")
}
```
