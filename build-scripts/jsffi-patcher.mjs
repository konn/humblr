import * as fs from "node:fs";
import * as process from "node:process";

const code = `
const setImmediate = (fn) => setTimeout(fn, 0);

class FinalizationRegistry {
  constructor(_callback) {}
  register($1, $2, $3) {
    return;
  }
  unregister(..._args) {
    return 1;
  }
}
`;

const input_path = process.argv[2];
const orig = await fs.promises.readFile(input_path, "utf-8");
const re = new RegExp("// A simple & fast.+(?=export default)", "mis");
let new_str = orig.replace(re, code);
if (new_str.match(/WorkerEntrypoint/)) {
  new_str = `import { WorkerEntrypoint } from 'cloudflare:workers';\n${new_str}`;
}

await fs.promises.writeFile(input_path, new_str);
