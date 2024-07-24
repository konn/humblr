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
const new_str = orig.replace(re, code);

await fs.promises.writeFile(input_path, new_str);
