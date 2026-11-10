import { createRequire } from "node:module";
import { resolve } from "node:path";

type Call = { kind: "write" | "log"; text: string };

const require = createRequire(import.meta.url);
const write = process.stdout.write.bind(process.stdout);
const log = console.log.bind(console);
const marker = /^\u0001(REF|TINY|END)(?: (.*))?\u0001$/;

let block: Call[] | null = null;
let ref: Call[] = [];
let name = "";
let checks = 0;
let failures = 0;

const record = (call: Call): void => {
  const m = call.kind === "write" ? marker.exec(call.text) : null;
  if (m) {
    if (m[1] === "REF") {
      name = m[2] ?? "";
      block = [];
    } else if (m[1] === "TINY") {
      ref = block ?? [];
      block = [];
    } else {
      checks++;
      const tiny = JSON.stringify(block);
      const expected = JSON.stringify(ref);
      if (tiny !== expected) {
        failures++;
        write(`channel ${name}\n  stdlib: ${expected}\n  tiny:   ${tiny}\n`);
      }
      block = null;
    }
  } else if (block) {
    block.push(call);
  } else if (call.kind === "write") {
    write(call.text);
  } else {
    log(call.text);
  }
};

process.stdout.write = ((text: string) => {
  record({ kind: "write", text: String(text) });
  return true;
}) as typeof process.stdout.write;
console.log = (...args: unknown[]) => record({ kind: "log", text: args.map(String).join(" ") });

process.on("exit", () => {
  write(`${checks} channel checks, ${failures} failures\n`);
  if (failures > 0) process.exitCode = 1;
});

for (const file of process.argv.slice(2)) require(resolve(file));
