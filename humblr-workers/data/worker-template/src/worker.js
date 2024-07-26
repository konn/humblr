import { WASI } from '@cloudflare/workers-wasi';
import ghc_wasm_jsffi from './ghc_wasm_jsffi.js';
import wasm_module from './handlers.wasm';

const wasi = new WASI();

const instance_exports = {};
const instance = new WebAssembly.Instance(wasm_module, {
	wasi_snapshot_preview1: wasi.wasiImport,
	ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

// Ideally, we want to initialise recator module by calling wasi.initialise(instance).
// However, this function seems unimplemented in cloudflare's WASI implementation.
// Instead of it, we just give a dummy _start function to the exports, and call 'wasi.start()' instead.
await wasi.start({ exports: { _start() {}, ...instance.exports } });
const handlers = await instance.exports.handlers();

export default handlers;
