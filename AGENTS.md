# Doxa Agent Guide

## Build & Test Commands
- **Build compiler**: `zig build`
- **Run all tests**: `zig build test`
- **Run specific test**: `zig build run -- <--debug> <test.doxa>` to focus on one test case

## Code Style Guidelines

### Formatting & Structure
- Use Zig standard formatting (`zig fmt`)
- 4-space indentation
- Group related code blocks with empty lines

### Naming Conventions
- `snake_case` for variables and functions
- `PascalCase` for types and structs
- `SCREAMING_SNAKE_CASE` for constants

### Error Handling
- Use Zig error sets for error definitions
- Prefer `try` for error propagation
- Use `defer` for resource cleanup

### Types & Imports
- Explicit type annotations preferred
- Import modules at top of file:
  ```zig
  const std = @import("std");
  const testing = std.testing;
  ```

### Testing Principles
- Tests live in `test/` directory
- Use `test "descriptive name" { ... }` blocks
- Verify both success and failure cases