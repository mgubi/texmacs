// The page of Tau in Safari, which the tests of browser-test.mjs do not
// reach (puppeteer drives Firefox and Chrome): Safari gives a worker a much
// smaller stack, and what recursed deeply stopped the program there (the
// user manual, see docs/tau-design.md). Run by hand, on a Mac:
//
//   node misc/tau/test/safari-test.mjs [--dir build-tau/out/web] [--manual]
//
// Safari is driven by safaridriver, which needs "Allow remote automation"
// (Safari > Settings > Developer). A window of Safari opens. --manual also
// opens the user manual, which takes a minute or two.

import { spawn } from "node:child_process";
import fs from "node:fs";
import net from "node:net";
import path from "node:path";
import { serve } from "../../wasm/serve.mjs";

const args = process.argv.slice(2);
const opt = (name, dflt) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : dflt; };
const dir = path.resolve(opt("--dir", "build-tau/out/web"));
const sleep = ms => new Promise(r => setTimeout(r, ms));
const freePort = () => new Promise(resolve => {
	const s = net.createServer();
	s.listen(0, "127.0.0.1", () => { const p = s.address().port; s.close(() => resolve(p)); });
});

if (!fs.existsSync(path.join(dir, "tau.wasm"))) { console.log(`safari: no build of the page in ${dir}`); process.exit(2); }
const port = await freePort(), driverPort = await freePort();
const server = await serve(dir, port);
const driver = spawn("/usr/bin/safaridriver", ["-p", String(driverPort)], { stdio: "ignore" });
await sleep(1500);

let session = null;
async function wd(method, where, body) {
	const r = await fetch(`http://localhost:${driverPort}${where}`, {
		method, headers: { "Content-Type": "application/json" }, body: body ? JSON.stringify(body) : undefined,
		signal: AbortSignal.timeout(60000)
	});
	const j = await r.json();
	if (j.value && j.value.error) throw new Error(j.value.message || j.value.error);
	return j.value;
}
const run = (script, ...a) => wd("POST", `/session/${session}/execute/sync`, { script, args: a });

let failed = 0;
function check(what, ok, got) {
	if (ok) { console.log("safari: " + what); return; }
	failed++;
	console.log("safari: FAILED " + what + (got === undefined ? "" : ": " + JSON.stringify(got)));
}
const stopped = () => run("return document.querySelectorAll('.tau-failure').length + window.tauErrors.length");

try {
	session = (await wd("POST", "/session", { capabilities: { alwaysMatch: { browserName: "safari" } } })).sessionId;
	await wd("POST", `/session/${session}/url`, { url: `http://127.0.0.1:${port}/index.html?debug&nohome` });
	await run(`window.tauErrors = []; const w = () => { if (!window.tau) return setTimeout(w, 20);
		tau.worker.addEventListener('message', e => { if (e.data.t === 'stopped' || e.data.t === 'failed') window.tauErrors.push(e.data.text); });
		tau.worker.addEventListener('error', e => window.tauErrors.push(e.message)); }; w(); return 1;`);
	let started = false;
	for (let i = 0; i < 120 && !started; i++) { await sleep(1000); started = await run("return !!(window.tau && tau.state.started)"); }
	check("the page starts", started);
	await sleep(3000);
	check("with its menus and its tabs", await run("return document.querySelectorAll('#tau-menu .tau-entry').length >= 8 && document.querySelectorAll('.tau-doc-tab').length >= 1"));

	// a new document, text which is typed
	await run("document.querySelector('.tau-doc-button').click(); return 1;");
	await sleep(2000);
	const paints = await run("return tau.state.paints");
	await run("tau.send({ t: 'text', view: tau.active.view, text: 'Typed in Safari.' }); return 1;");
	await sleep(2000);
	check("typing draws the view again", (await run("return tau.state.paints")) > paints);
	check("and marks the document", await run("return Array.from(document.querySelectorAll('.tau-doc-tab.tau-current .tau-doc-name')).some(e => e.textContent.endsWith('•'))"));
	check("the program did not stop", (await stopped()) === 0, await run("return window.tauErrors"));

	// a dialog with a view which is edited, opened and closed
	await run("tau.send({ t: 'scheme', code: '(open-macros-editor :global)' }); return 1;");
	await sleep(4000);
	check("a dialog with a field which is edited", await run("return document.querySelectorAll('.tau-dialog canvas.tau-editable').length === 1"));
	await run("document.querySelectorAll('.tau-dialog-close').forEach(b => b.click()); return 1;");
	await sleep(1500);
	await run("tau.send({ t: 'scheme', code: '(gc)' }); return 1;");
	await sleep(1500);
	check("closing it leaves the program running", (await stopped()) === 0, await run("return window.tauErrors"));

	// the panel of the files: a folder, a file brought in, a zip and back,
	// the document saved in the panel
	await run("tau.files.browse(); return 1;");
	await sleep(1500);
	check("the panel of the files shows the documents", await run("const b = document.querySelector('.tau-files'); return !!b && b.dataset.loaded === '1' && b.dataset.dir === '/home/tau/Documents'"));
	await run(`const data = new DataTransfer(); data.items.add(new File(['kept in Safari'], 'safari.txt'));
		const input = document.querySelector('.tau-files input[data-what=files]'); input.files = data.files;
		input.dispatchEvent(new Event('change', { bubbles: true })); return 1;`);
	await sleep(1500);
	check("a file which is added is listed", await run("return Array.from(document.querySelectorAll('.tau-files-row'), r => r.dataset.name).join() === 'safari.txt'"));
	await run(`window.zipped = null; (async () => { const text = 'The quick brown fox. '.repeat(40);
		const blob = await tau.files.zip([{ rel: 'd/one.txt', bytes: new TextEncoder().encode(text) }]);
		const l = await tau.files.unzip(new Uint8Array(await blob.arrayBuffer()));
		const b = await tau.files.makeBackup();
		window.zipped = { same: new TextDecoder().decode(l[0].bytes) === text, small: blob.size < text.length, backup: b.files }; })().catch(e => { window.zipped = { error: String(e) }; }); return 1;`);
	await sleep(2500);
	const zipped = await run("return window.zipped");
	check("a zip is made and read back, and a backup made", zipped && zipped.same && zipped.small && zipped.backup >= 1, zipped);
	await run("document.querySelector('.tau-files .tau-dialog-close').click(); return 1;");
	await run(`tau.send({ t: 'scheme', code: '(choose-file save-buffer-as "Save TeXmacs file" "texmacs")' }); return 1;`);
	await sleep(1500);
	await run("const i = document.querySelector('.tau-files-foot .tau-files-name'); i.value = 'safari.tm'; document.querySelector('.tau-files [data-what=ok]').click(); return 1;");
	await sleep(2500);
	check("the document is saved from the panel", await run("return Array.from(document.querySelectorAll('.tau-doc-tab.tau-current .tau-doc-name')).some(e => e.textContent === 'safari.tm') && !document.querySelector('.tau-files')"),
		await run("return Array.from(document.querySelectorAll('.tau-doc-tab .tau-doc-name'), e => e.textContent)"));
	check("the program is still running", (await stopped()) === 0, await run("return window.tauErrors"));

	if (args.includes("--manual")) {
		// the user manual: thousands of links, which overflowed the stack
		await run(`tau.send({ t: 'scheme', code: '(load-help-book "main/man-user-manual")' }); return 1;`);
		let there = false;
		for (let i = 0; i < 90 && !there; i++) {
			await sleep(2000);
			if ((await stopped()) > 0) break;
			there = await run("return Array.from(document.querySelectorAll('.tau-doc-tab.tau-current .tau-doc-name')).some(e => /user manual/i.test(e.textContent))");
		}
		check("the user manual opens", there);
		// its passes of typesetting: the program must still be there after them
		for (let i = 0; i < 60; i++) { await sleep(2000); if ((await stopped()) > 0) break; }
		check("and the program did not stop while it was made", (await stopped()) === 0, await run("return window.tauErrors"));
	}
} catch (error) {
	failed++;
	console.log("safari: FAILED " + (error && error.message || error) +
		(/automation|session/i.test(String(error)) ? " (is \"Allow remote automation\" on, in the Developer settings of Safari?)" : ""));
} finally {
	if (session) try { await wd("DELETE", `/session/${session}`); } catch (error) {}
	driver.kill();
	server.close();
}
console.log(failed ? `safari: ${failed} checks failed` : "safari: all checks pass");
process.exit(failed ? 1 : 0);
