import { execFileSync } from "node:child_process";
import { existsSync } from "node:fs";
import { gzipSync } from "node:zlib";

const libraries = ["printf", "hashtbl", "random", "md5", "buffer"] as const;
const variants = ["stdlib", "tiny"] as const;
const out = "_build/default/size/out/size";
const esbuild = process.env.ESBUILD ?? "node_modules/.bin/esbuild";

if (!existsSync(esbuild)) {
  console.error(`esbuild not found at ${esbuild}. Run npm install, or set ESBUILD.`);
  process.exit(1);
}

type Size = { raw: number; gzip: number };

const measure = (entry: string): Size => {
  const code = execFileSync(esbuild, [entry, "--bundle", "--minify", "--format=esm", "--log-level=error"]);
  return { raw: code.length, gzip: gzipSync(code, { level: 9 }).length };
};

const cell = (s: Size): string => `${s.raw} | ${s.gzip}`;

console.log("| library | stdlib raw | stdlib gzip | tiny raw | tiny gzip |");
console.log("| --- | ---: | ---: | ---: | ---: |");
for (const library of libraries) {
  const [stdlib, tiny] = variants.map((v) => measure(`${out}/${library}_${v}.js`));
  console.log(`| ${library} | ${cell(stdlib!)} | ${cell(tiny!)} |`);
}
