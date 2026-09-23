import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { dirname, resolve } from 'node:path';
import { WASI } from 'node:wasi';

const here = dirname(fileURLToPath(import.meta.url));

const sourcePath = process.argv[2];
if (!sourcePath) {
  console.error('usage: node demo/run_wasm.mjs <source.bf>');
  process.exit(2);
}
const source = readFileSync(sourcePath, 'utf8');

const wasi = new WASI({
  version: 'preview1',
  args: ['bf', source],
  env: {},
  returnOnExit: true,
});

const wasmPath = resolve(here, 'dist', 'bf.wasm');
const module = await WebAssembly.compile(readFileSync(wasmPath));
const instance = await WebAssembly.instantiate(module, wasi.getImportObject());
const exitCode = wasi.start(instance);
if (typeof exitCode === 'number' && exitCode !== 0) {
  process.exit(exitCode);
}
