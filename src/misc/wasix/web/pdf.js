// The page of the WASIX build (misc/wasix/web/pdf.html): TeXmacs run by the
// Wasmer SDK (sdk/, a copy of @wasmer/sdk), headless, on the document of the
// text area; the PDF it writes is shown in the frame.
//
// The program (texmacs.wasm) and its files (texmacs-fs.bin, an index in
// texmacs-fs.json, made by misc/wasix/pack-fs.py) make one package of the
// SDK, its files under /texmacs; each run has a sandbox of its own, the
// document and the PDF in its /workspace.

import { Wasmer } from "./sdk/dist/index.js";

const $ = id => document.getElementById (id);
const log = s => { $("log").textContent += s + "\n"; $("log").scrollTop = 1e9; };
const status = s => { $("status").textContent = s; };
const t0 = performance.now ();
const ms = () => Math.round (performance.now () - t0) + " ms";

const HELLO = `<TeXmacs|2.1>

<style|generic>

<\\body>
  Hello <with|font-series|bold|WASIX>: <math|<big|int><rsub|0><rsup|1>x<rsup|2>*d*x=<frac|1|3>>.

  <\\equation*>
    <big|sum><rsub|n=1><rsup|\\<infty\\>><frac|1|n<rsup|2>>=<frac|\\<pi\\><rsup|2>|6>
  </equation*>
</body>
`;

async function fetchBytes (url) {
  const r = await fetch (url);
  if (!r.ok) throw new Error (`${url}: ${r.status}`);
  return new Uint8Array (await r.arrayBuffer ());
}

let wasmer, pkg;

async function setup () {
  $("source").value = HELLO;
  if (!crossOriginIsolated)
    log ("warning: the page is not cross-origin isolated (COOP/COEP): the SDK needs it");
  const [wasm, blob, index] = await Promise.all ([
    fetchBytes ("texmacs.wasm"), fetchBytes ("texmacs-fs.bin"),
    fetch ("texmacs-fs.json").then (r => r.json ())]);
  log (`downloaded: program ${(wasm.length / 1e6).toFixed (1)} MB, files ${(blob.length / 1e6).toFixed (1)} MB (${ms ()})`);
  const files = {};
  for (const [path, offset, size] of index)
    files[path] = blob.subarray (offset, offset + size);
  wasmer = new Wasmer ();
  await wasmer.ready ();
  pkg = await wasmer.packages.create ({
    modules: { texmacs: wasm },
    commands: { texmacs: { module: "texmacs" } },
    files });
  log (`package: ${index.length} files (${ms ()})`);
  status ("ready");
  $("run").disabled = false;
  window.tmWasix = { ready: true, typeset };
}

async function typeset () {
  $("run").disabled = true;
  status ("typesetting…");
  const start = performance.now ();
  const sandbox = await wasmer.sandboxes.create ({
    packages: [pkg],
    files: { "in.tm": $("source").value },
    env: { TEXMACS_PATH: "/texmacs", TEXMACS_HOME_PATH: "/workspace/home",
           HOME: "/workspace/home" } });
  try {
    const out = await sandbox
      .command (pkg, ["-headless", "-c", "/workspace/in.tm", "/workspace/out.pdf", "-q"],
                { cwd: "/workspace" })
      .run ({ check: false, timeoutMs: 600000 });
    const time = Math.round (performance.now () - start);
    log (`exit ${out.exitCode} (${out.reason}) in ${time} ms`);
    const text = (out.stdout.text () + out.stderr.text ())
      .split ("\n").filter (l => l && !l.includes ("cannot load picture")).join ("\n");
    if (text) log (text);
    const pdf = await sandbox.fs.readFile ("/workspace/out.pdf");
    $("pdf").src = URL.createObjectURL (new Blob ([pdf], { type: "application/pdf" }));
    status (`PDF of ${pdf.length} bytes, ${time} ms`);
    return { exit: out.exitCode, bytes: pdf.length, ms: time };
  } catch (e) {
    log ("error: " + e.message);
    status ("failed");
    return { error: e.message };
  } finally {
    await sandbox.close ();
    $("run").disabled = false;
  }
}

$("run").addEventListener ("click", typeset);
setup ().catch (e => { log ("error: " + (e.stack || e)); status ("failed"); });
