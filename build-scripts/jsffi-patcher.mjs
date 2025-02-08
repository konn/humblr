import * as fs from "node:fs";
import * as process from "node:process";

const input_path = process.argv[2];
const output_path = process.argv[3];
let new_str = await fs.promises.readFile(input_path, "utf-8");
if (new_str.match(/WorkerEntrypoint/)) {
  new_str = `import { WorkerEntrypoint } from 'cloudflare:workers';\n${new_str}`;
}

await fs.promises.writeFile(output_path, new_str);
