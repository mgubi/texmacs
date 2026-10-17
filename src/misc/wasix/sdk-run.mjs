// TeXmacs (the WASIX build) run by the Wasmer SDK under node: the runtime of
// the browser, without the browser.
//
//   node misc/wasix/sdk-run.mjs <build dir> <input.tm> <output.pdf>
//
// The SDK is the one installed in <build dir>/sdk (npm install @wasmer/sdk),
// the program <build dir>/out/texmacs.wasm, its files <build dir>/out/
// texmacs-fs.{bin,json} (pack-fs.py). The document goes in as
// /workspace/in.tm, the PDF comes out of /workspace/out.pdf.

import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { pathToFileURL } from "node:url";

const [build, input, output] = process.argv.slice(2);
if (!output) {
  console.error("usage: sdk-run.mjs <build dir> <input.tm> <output.pdf>");
  process.exit(2);
}
const sdk = resolve(build, "sdk/node_modules/@wasmer/sdk/dist/node.js");
const { Wasmer } = await import(pathToFileURL(sdk));

const t0 = performance.now();
const ms = () => Math.round(performance.now() - t0) + " ms";
const wasm = await readFile(resolve(build, "out/texmacs.wasm"));
const blob = await readFile(resolve(build, "out/texmacs-fs.bin"));
const index = JSON.parse(await readFile(resolve(build, "out/texmacs-fs.json"), "utf8"));
const files = {};
for (const [path, offset, size] of index)
  files[path] = blob.subarray(offset, offset + size);

const wasmer = new Wasmer({ cache: false });
try {
  const pkg = await wasmer.packages.create({
    modules: { texmacs: wasm },
    commands: { texmacs: { module: "texmacs" } },
    files,
  });
  console.log(`package: ${index.length} files (${ms()})`);
  const sandbox = await wasmer.sandboxes.create({
    packages: [pkg],
    files: { "in.tm": await readFile(input) },
    env: { TEXMACS_PATH: "/texmacs", TEXMACS_HOME_PATH: "/workspace/home",
           HOME: "/workspace/home" },
  });
  try {
    const out = await sandbox
      .command(pkg, ["-headless", "-c", "/workspace/in.tm", "/workspace/out.pdf", "-q"],
               { cwd: "/workspace" })
      .run({ check: false, timeoutMs: 600000 });
    console.log(`exit ${out.exitCode} (${out.reason}), ${ms()}`);
    process.stdout.write(out.stdout.text());
    process.stderr.write(out.stderr.text());
    const pdf = await sandbox.fs.readFile("/workspace/out.pdf");
    await writeFile(output, pdf);
    console.log(`${output}: ${pdf.length} bytes`);
  } finally {
    await sandbox.close();
  }
} finally {
  await wasmer.close();
}
