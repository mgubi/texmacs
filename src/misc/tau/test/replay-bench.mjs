// What the page spends to draw a frame which the core gives as commands (the
// prototype of the drawing on the GPU, render-bench.mjs --gpu), in parts: the
// shift of a scroll, the calls to the canvas which say what to draw, and the
// drawing which the canvas does then (?gpusync waits for it). The same for
// the pixels which the core draws, to compare. See docs/tau-design.md,
// "Drawing on the GPU".
//
//   node misc/tau/test/replay-bench.mjs --dir build-tau/out/web --tools <dir>
//        [--browser firefox|safari] [--headed] [--only name] [--gl]
//        [--native] [--core] [--direct] [--nosync] [--width 1900 --height 1180] [--density 2]
//
// Safari is driven by safaridriver ("Allow remote automation"), in a window
// of the screen (its density is that of the screen); Firefox by puppeteer,
// headless unless --headed (headless, the canvas may be drawn without the GPU).
// --gl: the view of the pane drawn by WebGL (?gl, gl.mjs), the pixels as
// well as the commands. --native: the commands drawn by the GPU renderer
// of the core in the worker too (a core built with THORVG, tau_gpu.cpp),
// timed there: the bitmap for the page (--core), the calls, and the GPU
// waited for. --core: the views drawn by it and shown (?coregl), the page
// shifts its canvas and copies a bitmap;
// timed in the worker as with --native. "round trip": from the scroll sent
// by the page to the core done with it (its frame drawn by the page);
// --direct: drawn by it on the canvas of the page, given to the worker
// (?coregl&direct; the pixels are not shown then);
// --nosync: without waiting for the canvases (only the round trips).

import { spawn } from "node:child_process";
import fs from "node:fs";
import net from "node:net";
import os from "node:os";
import path from "node:path";
import { createRequire } from "node:module";
import { serve } from "../../wasm/serve.mjs";

const args = process.argv.slice(2);
const opt = (name, dflt) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : dflt; };
const dir = path.resolve(opt("--dir", "build-tau/out/web"));
const tools = path.resolve(opt("--tools", "build-tau/tools"));
const which = opt("--browser", "firefox");
const width = +opt("--width", "1900"), height = +opt("--height", "1180"), density = +opt("--density", "2");
const sleep = ms => new Promise(r => setTimeout(r, ms));
const freePort = () => new Promise(resolve => {
	const s = net.createServer();
	s.listen(0, "127.0.0.1", () => { const p = s.address().port; s.close(() => resolve(p)); });
});

const head = "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n", tail = "</body>\n";
const words = ("the theory of a typesetting system which is also an editor asks that what is seen be what is printed " +
	"and that the structure of the document stay at hand while one writes its formulas and its text").split(" ");
function prose(n) {
	let s = "";
	for (let i = 0; i < n; i++) s += words[(i * 7 + (i >> 3)) % words.length] + (i % 17 === 16 ? ". " : " ");
	return s.trim();
}
const DOCS = [
	{ name: "prose", text: head + Array.from({ length: 150 }, () => "  " + prose(90) + ".\n\n").join("") + tail },
	{ name: "tables", path: "/texmacs/doc/devel/scheme/api/glue-auto-doc.en.tm" },
	{ name: "manual", scheme: '(load-help-book "main/man-user-manual")', skip: 0.4 }
];

// the browser: ev (a JavaScript expression, which may be a promise) -> its value
const port = await freePort();
const server = await serve(dir, port);
const direct = args.includes("--direct");
// (the worker timed waits for its GPU at each frame: not with --nosync,
// whose round trips would have that wait, 2 to 3 ms, which nothing else has)
const nosync = args.includes("--nosync");
const useGL = args.includes("--gl"), coreGL = args.includes("--core") || direct, native = args.includes("--native") || (coreGL && !nosync);
const url = `http://127.0.0.1:${port}/index.html?debug&nohome${nosync ? "" : "&gpusync"}${useGL ? "&gl" : ""}${coreGL ? "&coregl" : ""}${direct ? "&direct" : ""}`;
let ev, open, close;
if (which === "safari") {
	const driverPort = await freePort();
	const driver = spawn("/usr/bin/safaridriver", ["-p", String(driverPort)], { stdio: "ignore" });
	await sleep(1500);
	let session = null;
	const wd = async (method, where, body) => {
		const r = await fetch(`http://localhost:${driverPort}${where}`, { method, headers: { "Content-Type": "application/json" },
			body: body ? JSON.stringify(body) : undefined, signal: AbortSignal.timeout(900000) });
		const j = await r.json();
		if (j.value && j.value.error) throw new Error(j.value.message || j.value.error);
		return j.value;
	};
	ev = expr => wd("POST", `/session/${session}/execute/async`, { args: [],
		script: `const done = arguments[arguments.length - 1]; Promise.resolve().then(() => (${expr})).then(done, e => done({ error: String(e) }));` });
	open = async () => {
		session = (await wd("POST", "/session", { capabilities: { alwaysMatch: { browserName: "safari" } } })).sessionId;
		await wd("POST", `/session/${session}/timeouts`, { script: 900000 });
		await wd("POST", `/session/${session}/window/rect`, { width, height: height + 80 });
		await wd("POST", `/session/${session}/url`, { url });
	};
	close = async () => { try { await wd("DELETE", `/session/${session}`); } catch (e) {} };
	process.on("exit", () => driver.kill());
}
else {
	const puppeteer = createRequire(path.join(tools, "package.json"))("puppeteer-core");
	const program = ["/Applications/Firefox.app/Contents/MacOS/firefox", "/usr/bin/firefox"].find(p => fs.existsSync(p));
	const browser = await puppeteer.launch({ browser: "firefox", headless: !args.includes("--headed"), executablePath: program,
		userDataDir: fs.mkdtempSync(path.join(os.tmpdir(), "tau-replay-")) });
	let page = null;
	ev = expr => page.evaluate(`(${expr})`);
	open = async () => {
		page = await browser.newPage();
		await page.setViewport({ width, height, deviceScaleFactor: density });
		await page.goto(url, { waitUntil: "load" });
	};
	close = async () => { if (page) await page.close(); page = null; };
	process.on("exit", () => browser.process() && browser.process().kill());
}
console.log(`replay: ${which}${which !== "safari" ? (args.includes("--headed") ? ", headed" : ", headless") : ""}, ${width}×${height}${useGL ? ", WebGL" : ""}${coreGL ? ", drawn in the worker" : ""}`);

const until = async (test, timeout = 120000) => {
	for (const end = Date.now() + timeout; Date.now() < end; await sleep(200)) { try { if (await test()) return; } catch (e) {} }
	throw new Error("waited in vain");
};
const mean = l => l.length ? l.reduce((a, b) => a + b, 0) / l.length : 0;

async function run(doc) {
	await open();
	await until(() => ev("!!(window.tau && tau.state.started)"));
	await ev(`(() => {
		window.pongs = new Map(); window.syncs = 0;
		tau.worker.addEventListener("message", e => { const m = e.data;
			if (m.t === "pong" && window.pongs.has(m.id)) { window.pongs.get(m.id)(); window.pongs.delete(m.id); } });
		window.walls = [];
		window.step = message => new Promise(resolve => {
			const id = "replay" + (++window.syncs), t0 = performance.now();
			window.pongs.set(id, () => { if (message && message.t === "scroll") window.walls.push(performance.now() - t0); resolve(); });
			if (message) tau.worker.postMessage(message);
			tau.worker.postMessage({ t: "ping", id });
		});
		return 1; })()`);
	if (doc.scheme) {
		await ev(`tau.send({ t: "scheme", code: ${JSON.stringify(doc.scheme)} })`);
		let last = -1, same = 0;
		await until(async () => {
			const h = await ev("window.step().then(() => tau.active.extents.height)");
			same = h === last && h > 20000 ? same + 1 : 0;
			last = h;
			return same >= 40;
		}, 900000);
	}
	else {
		const file = doc.path || "/home/tau/Documents/" + doc.name + ".tm";
		if (doc.text) await ev(`tau.fs("write", { files: [{ path: ${JSON.stringify(file)}, bytes: new TextEncoder().encode(${JSON.stringify(doc.text)}).buffer, time: 0 }] })`);
		await ev(`tau.worker.postMessage({ t: "chosen", ticket: 0, path: ${JSON.stringify(file)} })`);
		await until(() => ev(`document.title.includes(${JSON.stringify(path.basename(file))})`));
	}
	await sleep(2500);
	const view = await ev("tau.active.view");
	const canvas = await ev(`(() => { const c = document.querySelector(".tau-pane .tau-canvas"); return { w: c.width, h: c.height, isolated: self.crossOriginIsolated }; })()`);
	const whole = await ev("tau.active.extents.height");
	if (doc.skip) { await ev(`window.step({ t: "scroll", view: ${view}, dx: 0, dy: ${Math.round(whole * doc.skip)} })`); await sleep(1500); }
	// scroll steps (a strip comes in), then whole views (all is drawn again)
	let timeNative = false;
	const scroll = async (dy, n) => {
		await ev("(tau.state.drawParts = [], window.walls = [], 1)");
		if (timeNative) await ev(`window.step({ t: "gpu-bench", on: 1 })`);
		for (let i = 0; i < n; i++) await ev(`window.step({ t: "scroll", view: ${view}, dx: 0, dy: ${dy} })`);
		await sleep(300);
		const parts = await ev("tau.state.drawParts");
		parts.walls = await ev("window.walls");
		if (timeNative) parts.native = JSON.parse(await ev(`new Promise(res => { const on = e => { if (e.data.t === "gpu-stats") { tau.worker.removeEventListener("message", on); res(e.data.text); } }; tau.worker.addEventListener("message", on); tau.worker.postMessage({ t: "gpu-stats" }); })`));
		return parts;
	};
	const pass = async () => {
		const strip = Math.round(canvas.h * 0.08), page = canvas.h;
		await scroll(strip, 10); // (warms the caches of the glyphs)
		const steps = await scroll(strip, 30);
		await scroll(-30 * strip, 1);
		await scroll(page, 3);
		const views = await scroll(page, 8);
		await scroll(-11 * page, 1);
		return { steps, views };
	};
	// (?gl asks for the commands when the page starts: the pixels first)
	if (useGL || coreGL) { await ev(`window.step({ t: "gpu", on: 0 })`); await sleep(1500); }
	const cpu = await pass();
	await ev(`window.step({ t: "gpu", on: 1 })`);
	await sleep(1500);
	timeNative = native;
	const gpu = await pass();
	if (native) await ev(`window.step({ t: "gpu-bench", on: 0 })`);
	await close();
	return { canvas, cpu, gpu };
}

const row = (doc, what, parts) => {
	const p = parts.filter(x => x.calls !== undefined);
	const k = op => mean(p.map(x => (x.ops && x.ops[op]) || 0));
	const ops = p[0] && p[0].ops ? ` | ${k(2).toFixed(0).padStart(5)} glyphs, ${k(1).toFixed(0).padStart(4)} rects, ${(k(3) + k(4) + k(5)).toFixed(0).padStart(3)} paths, ${k(6).toFixed(0).padStart(3)} clips, ${k(8).toFixed(0)} pictures; new: ${mean(p.map(x => x.masks)).toFixed(0)} glyphs, ${mean(p.map(x => x.inks)).toFixed(0)} inks` : "";
	const area = mean(p.map(x => x.w * x.h)) / 1e6;
	console.log(`${doc.padEnd(9)} ${what.padEnd(16)} |${mean(p.map(x => x.shift)).toFixed(2).padStart(7)} |${mean(p.map(x => x.calls)).toFixed(2).padStart(7)} |${mean(p.map(x => x.raster)).toFixed(2).padStart(7)} |` +
		`${mean(p.map(x => x.shift + x.calls + x.raster)).toFixed(2).padStart(7)} | ${String(p.length).padStart(3)} frames, ${area.toFixed(2)} Mpx${ops}`);
};
console.log("document  frames           |  shift |  calls | raster |  total | (means in ms for a frame of the page)");
for (const doc of DOCS) {
	if (opt("--only", "") && doc.name !== opt("--only", "")) continue;
	try {
		const r = await run(doc);
		console.log(`${doc.name}: canvas ${r.canvas.w}×${r.canvas.h}${r.canvas.isolated ? "" : " (not isolated: the clock has coarse steps)"}`);
		const drawn = direct ? "direct" : coreGL ? "bitmaps" : "commands";
		row(doc.name, "pixels, step", r.cpu.steps);
		row("", drawn + ", step", r.gpu.steps);
		row("", "pixels, view", r.cpu.views);
		row("", drawn + ", view", r.gpu.views);
		for (const [what, parts] of [["C++ GPU, step", r.gpu.steps], ["C++ GPU, view", r.gpu.views]]) {
			const q = parts.native;
			if (!q || !q.frames) continue;
			console.log(`${"".padEnd(9)} ${what.padEnd(16)} |${q.show.toFixed(2).padStart(7)} |${q.calls.toFixed(2).padStart(7)} |${q.gpu.toFixed(2).padStart(7)} |` +
				`${(q.show + q.calls + q.gpu).toFixed(2).padStart(7)} | ${String(q.frames).padStart(3)} frames, ${(q.area / 1e6).toFixed(2)} Mpx (in the worker; the first column: the bitmap for the page)`);
		}
		for (const [what, parts] of [["pixels, step", r.cpu.steps], [drawn + ", step", r.gpu.steps], ["pixels, view", r.cpu.views], [drawn + ", view", r.gpu.views]]) {
			const w = parts.walls.slice().sort((a, b) => a - b);
			console.log(`${"".padEnd(9)} round trip, ${what.padEnd(16)} | median ${w[w.length >> 1].toFixed(1)}, mean ${mean(w).toFixed(1)} ms of ${w.length}`);
		}
	}
	catch (error) { console.log(doc.name + ": " + error.message); try { await close(); } catch (e) {} }
}
server.close();
process.exit(0);
