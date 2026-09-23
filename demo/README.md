# Brainfuck on WebAssembly (WASI)

Proof that `doxa compile --arch=wasm32 --os=wasi` produces a runnable wasm.

## Build

From the repository root, with the compiler built (`zig build`):

```
doxa compile demo/bf.doxa -o ./demo/dist/bf.wasm --arch=wasm32 --os=wasi --opt-mode=small
```

This writes `demo/dist/bf.wasm` — **81,980 bytes**.

`--os=wasi` suppresses the host `.exe` suffix, so the output lands exactly where named.

## Run

`demo/run_wasm.mjs` compiles the module under Node's WASI (preview1) and passes the
Brainfuck source text as `argv[1]`:

```
node demo/run_wasm.mjs demo/tests/hello.bf
```

Expected stdout (exactly 13 bytes, `Hello World!` + newline):

```
Hello World!
```

On older Node releases the harness may need `--experimental-wasi-unstable-preview1`;
Node 24 accepts `version: 'preview1'` without a flag.
