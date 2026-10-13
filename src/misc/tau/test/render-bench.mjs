// How the time of the core divides between typesetting, saying what is to
// be drawn, and drawing it: what a separate thread for the drawing would
// take away from the core. See docs/tau-design.md, "A thread for the
// drawing?".
//
//   node misc/tau/test/render-bench.mjs --dir build-tau/out/web --tools <dir>
//
// Each document is scrolled (every step draws the whole view) and typed
// in, twice: as usual, and "dry" (tau_dry: the drawing commands are written
// down as those of a display list would be, and nothing is drawn). The
// times are those of the core itself (?profile).

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
const program = opt("--browser", ["/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
	"/Applications/Firefox.app/Contents/MacOS/firefox", "/usr/bin/google-chrome", "/usr/bin/firefox"].find(p => fs.existsSync(p)));
const puppeteer = createRequire(path.join(tools, "package.json"))("puppeteer-core");
const sleep = ms => new Promise(r => setTimeout(r, ms));

// the documents: two which are made here, two of TeXmacs
const head = "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n", tail = "</body>\n";
const words = ("the theory of a typesetting system which is also an editor asks that what is seen be what is printed " +
	"and that the structure of the document stay at hand while one writes its formulas and its text").split(" ");
function prose(n) {
	let s = "";
	for (let i = 0; i < n; i++) s += words[(i * 7 + (i >> 3)) % words.length] + (i % 17 === 16 ? ". " : " ");
	return s.trim();
}
function proseDoc() {
	let s = head;
	for (let i = 0; i < 150; i++) s += "  " + prose(90) + ".\n\n";
	return s + tail;
}
function mathDoc() {
	const inline = i => `<math|x<rsub|${i}><rsup|2>+<frac|a<rsub|${i}>|1+<sqrt|b<rsub|${i}>>>\\<leqslant\\>\\<alpha\\><rsup|${i % 9}>*\\<beta\\>>`;
	let s = head;
	for (let i = 0; i < 120; i++) {
		s += "  " + prose(18) + " " + inline(i) + " " + prose(14) + " " + inline(i + 1) + " " + prose(16) + " " + inline(i + 2) + " " + prose(12) + ".\n\n";
		s += "  <\\equation*>\n    <big|sum><rsub|i=1><rsup|n><frac|x<rsub|i><rsup|" + (i % 7) + ">|1+x<rsub|i><rsup|2>>=" +
			"<big|int><rsub|0><rsup|\\<infty\\>>\\<mathe\\><rsup|-t<rsup|2>>*<frac|\\<partial\\>f|\\<partial\\>t>*\\<mathd\\>t+" +
			"<around*|(|<tabular*|<tformat|<table|<row|<cell|a<rsub|" + i + ">>|<cell|b>>|<row|<cell|c>|<cell|d<rsup|2>>>>>>|)>\n  </equation*>\n\n";
	}
	return s + tail;
}
function pictureDoc() {
	let s = head;
	for (let i = 0; i < 60; i++)
		s += "  " + prose(40) + " <image|/texmacs/misc/images/The_Jolly_Writer.png|0.3par|||> " + prose(10) +
			" <image|/texmacs/misc/images/texmacs-512.png|0.25par|||> <image|/texmacs/misc/images/welcome-manual.png|0.3par|||>" + ".\n\n";
	return s + tail;
}
const DOCS = [
	{ name: "prose", text: proseDoc() },
	{ name: "maths", text: mathDoc() },
	{ name: "tables", path: "/texmacs/doc/devel/scheme/api/glue-auto-doc.en.tm" },
	{ name: "pictures", text: pictureDoc() },
	// one structure which is a single unit of the typesetter
	{ name: "bigtable", text: head + "  <block|<tformat|<table|" + Array.from({ length: 60 }, (_, r) =>
		"<row|" + Array.from({ length: 6 }, (_, c) => "<cell|" + prose(4 + (r + c) % 5) + ">").join("|") + ">").join("|") + ">>>\n" + tail },
	{ name: "longpar", text: head + "  " + prose(4000) + ".\n" + tail },
	// the user manual of TeXmacs, made into one book (Help -> Manual -> User manual)
	{ name: "manual", scheme: '(load-help-book "main/man-user-manual")', skip: 0.4 }
];

const port = await new Promise(resolve => {
	const s = net.createServer();
	s.listen(0, "127.0.0.1", () => { const p = s.address().port; s.close(() => resolve(p)); });
});
const server = await serve(dir, port);
const browser = await puppeteer.launch({
	browser: /firefox/i.test(program) ? "firefox" : "chrome", headless: true, executablePath: program,
	userDataDir: fs.mkdtempSync(path.join(os.tmpdir(), "tau-bench-"))
});
console.log("bench: " + path.basename(program));

// (the mean: the clock of a browser has steps of a millisecond)
const median = l => l.length ? l.reduce((a, b) => a + b, 0) / l.length : 0;
const f1 = x => x.toFixed(1).padStart(6);

async function run(doc, density) {
	const page = await browser.newPage();
	await page.setViewport({ width: 1280, height: 860, deviceScaleFactor: density });
	const env = opt("--env", "");
	await page.goto(`http://127.0.0.1:${port}/index.html?debug&nohome&profile` + (env ? "&env=" + encodeURIComponent(env) : ""), { waitUntil: "load" });
	const until = async (test, timeout = 90000) => {
		for (const end = Date.now() + timeout; Date.now() < end; await sleep(100)) { try { const r = await test(); if (r) return r; } catch (error) {} }
		throw new Error("waited in vain (" + doc.name + ")");
	};
	await until(() => page.evaluate(() => !!(window.tau && tau.state.started)));
	await page.evaluate(() => {
		window.logs = []; window.pongs = new Map(); window.syncs = 0;
		tau.worker.addEventListener("message", e => {
			const m = e.data;
			if (m.t === "log") { window.logs.push(m.text); if (/edit-profile typeset:/.test(m.text)) (window.hf = window.hf || []).push(m.text); if (/differ/.test(m.text)) (window.bad = window.bad || []).push(m.text); }
			if (m.t === "pong" && window.pongs.has(m.id)) { window.pongs.get(m.id)(); window.pongs.delete(m.id); }
		});
		// a message for the core, and what the core said until it was done with it
		window.step = message => new Promise(resolve => {
			const id = "bench" + (++window.syncs);
			window.logs = [];
			window.pongs.set(id, () => resolve(window.logs.join("\n")));
			if (message) tau.worker.postMessage(message);
			tau.worker.postMessage({ t: "ping", id });
		});
	});
	let file = doc.path;
	if (doc.text) {
		file = "/home/tau/Documents/" + doc.name + ".tm";
		await page.evaluate((file, text) => tau.fs("write", { files: [{ path: file, bytes: new TextEncoder().encode(text).buffer, time: 0 }] }), file, doc.text);
	}
	if (doc.scheme) {
		// (made in several passes: until its height does not change any more)
		await page.evaluate(code => tau.send({ t: "scheme", code }), doc.scheme);
		let last = -1, same = 0;
		await until(async () => {
			const h = await page.evaluate(() => window.step().then(() => tau.active.extents.height));
			same = h === last && h > 20000 ? same + 1 : 0;
			last = h;
			return same >= 80;
		}, 600000);
	}
	else {
		await page.evaluate(file => tau.worker.postMessage({ t: "chosen", ticket: 0, path: file }), file);
		await until(() => page.evaluate(name => document.title.includes(name), path.basename(file)));
	}
	await sleep(2500);
	await page.evaluate(() => window.step());
	if (process.env.BENCH_DEBUG) console.log(doc.name, await page.title(), (await page.evaluate(() => window.step())).slice(0, 300));
	const view = await page.evaluate(() => tau.active.view);
	const size = await page.$eval(".tau-pane .tau-canvas", c => ({ w: c.width, h: c.height }));
	const whole = await page.evaluate(() => tau.active.extents.height);
	if (doc.skip) { // (into the document, away from its title and contents)
		await page.evaluate((view, dy) => window.step({ t: "scroll", view, dx: 0, dy }), view, Math.round(whole * doc.skip));
		await sleep(1500);
	}
	const num = (text, re) => { const l = []; for (const m of text.matchAll(re)) l.push(parseFloat(m[1])); return l; };
	const pass = async dry => {
		if (dry) await page.evaluate(() => window.step({ t: "dry", on: 1 }));
		const scroll = { turn: [], editors: [], drawing: [], sending: [] };
		for (let i = 0; i < 30; i++) {
			const text = await page.evaluate((view, dy) => window.step({ t: "scroll", view, dx: 0, dy }), view, 90 * density);
			const turns = [...text.matchAll(/tau-profile turn: ([\d.e-]+) ms \(editors ([\d.e-]+), drawing ([\d.e-]+), sending ([\d.e-]+)/g)];
			scroll.turn.push(turns.reduce((n, m) => n + parseFloat(m[1]), 0));
			scroll.editors.push(turns.reduce((n, m) => n + parseFloat(m[2]), 0));
			scroll.drawing.push(turns.reduce((n, m) => n + parseFloat(m[3]), 0));
			scroll.sending.push(turns.reduce((n, m) => n + parseFloat(m[4]), 0));
		}
		// the cursor in the middle of the view, then letters
		const x = Math.round(size.w * 0.5), y = Math.round(size.h * 0.5);
		for (const kind of ["press-left", "release-left"])
			await page.evaluate((view, kind, x, y) => window.step({ t: "mouse", view, kind, x, y, mods: kind === "press-left" ? 1 : 0, at: Date.now() }), view, kind, x, y);
		await sleep(700);
		await page.evaluate(() => window.step());
		const type = { turn: [], typeset: [], drawing: [], sending: [], pixels: [] };
		for (let i = 0; i < 40; i++) {
			const text = await page.evaluate((view, key) => window.step({ t: "key", view, key }), view, i % 6 === 5 ? "space" : "etaoin"[i % 6]);
			const turns = [...text.matchAll(/tau-profile turn: ([\d.e-]+) ms \(editors ([\d.e-]+), drawing ([\d.e-]+), sending ([\d.e-]+), pixels (\d+)/g)];
			type.turn.push(turns.reduce((n, m) => n + parseFloat(m[1]), 0));
			type.drawing.push(turns.reduce((n, m) => n + parseFloat(m[3]), 0));
			type.sending.push(turns.reduce((n, m) => n + parseFloat(m[4]), 0));
			type.pixels.push(turns.reduce((n, m) => n + parseFloat(m[5]), 0));
			if (process.env.BENCH_DEBUG && !dry && i === 20) console.log(text.split("\n").filter(l => /typeset|turn/.test(l)).join("\n"));
			type.typeset.push(num(text, /edit-profile typeset: ([\d.e-]+) ms/g).reduce((a, b) => a + b, 0));
		}
		let counts = null;
		if (dry) {
			const text = await page.evaluate(() => window.step({ t: "dry", on: 0 }));
			const m = /tau-dry: glyphs (\d+), rectangles (\d+), paths (\d+), pictures (\d+), state (\d+), copies (\d+), bytes (\d+)/.exec(text);
			if (m) counts = { glyphs: +m[1], rects: +m[2], paths: +m[3], pictures: +m[4], state: +m[5], copies: +m[6], bytes: +m[7] };
		}
		// (back to the top, and what was typed is taken away, for the next pass)
		for (let i = 0; i < 40; i++) await page.evaluate((view) => window.step({ t: "key", view, key: "backspace" }), view);
		await page.evaluate((view, dy) => window.step({ t: "scroll", view, dx: 0, dy }), view, -30 * 90 * density);
		await sleep(500);
		return { scroll, type, counts };
	};
	await pass(false); // (a first pass warms the caches of the glyphs)
	const real = await pass(false), dry = await pass(true);
	if (opt("--edits", "")) {
		// what each kind of edit costs, stage by stage (the profile of the core)
		const key = k => page.evaluate((view, key) => window.step({ t: "key", view, key }), view, k);
		const x = Math.round(size.w * 0.5), y = Math.round(size.h * 0.5);
		for (const kind of ["press-left", "release-left"])
			await page.evaluate((view, kind, x, y) => window.step({ t: "mouse", view, kind, x, y, mods: kind === "press-left" ? 1 : 0, at: Date.now() }), view, kind, x, y);
		await sleep(800);
		const fields = [["turn", /tau-profile turn: ([\d.e-]+) ms/g], ["typeset", /edit-profile typeset: ([\d.e-]+) ms/g], ["passes", /ms in (\d+) pass/g],
			["bridges", /bridges ([\d.e-]+) ms/g], ["redone", /\((\d+) redone/g], ["pager", /pages ([\d.e-]+) ms/g], ["search", /\(search ([\d.e-]+) ms/g],
			["changes", /changes ([\d.e-]+) ms/g], ["pages made", /\((\d+) pages, /g], ["texts kept", /pages, (\d+) texts/g], ["heads kept", /texts and (\d+) headers/g], ["drawing", /drawing ([\d.e-]+),/g]];
		const table = {};
		for (const [name, keys] of [["a character", "etaoinshrdlu".split("")], ["a new line", Array(6).fill("return")], ["a line removed", Array(6).fill("backspace")]]) {
			const sums = {};
			for (const k of keys) {
				const text = await key(k);
				for (const [f, re] of fields) sums[f] = (sums[f] || 0) + num(text, re).reduce((a, b) => a + b, 0);
			}
			table[name] = Object.fromEntries(fields.map(([f]) => [f, sums[f] / keys.length]));
		}
		for (let i = 0; i < 12; i++) await key("backspace");
		// (the menus are updated a moment after the last key)
		await page.evaluate(() => { window.logs = []; });
		await key("x");
		await sleep(1200);
		const after = await page.evaluate(() => window.logs.join("\n"));
		await key("backspace");
		console.log(doc.name + ": one edit, means in ms of the core");
		console.log("".padEnd(15) + Object.keys(table).map(n => n.padStart(16)).join(""));
		for (const [f] of fields) console.log(f.padEnd(15) + Object.values(table).map(r => r[f].toFixed(1).padStart(16)).join(""));
		console.log((after.split("\n").filter(l => /menus:/.test(l)).slice(0, 3).join("\n")).slice(0, 900));
		await page.close();
		return null;
	}
	if (opt("--stress", "")) {
		// edits which move the pages and change their headers: lines, a
		// chapter and a section with their titles, then all of it undone
		const key = k => page.evaluate((view, key) => window.step({ t: "key", view, key }), view, k);
		const scm = code => page.evaluate(code => window.step({ t: "scheme", code }), code);
		await page.evaluate(() => { window.hf = []; });
		for (let i = 0; i < 45; i++) await key("return");
		await scm("(make-section 'chapter)");
		for (const c of "Stress") await key(c);
		await key("return");
		for (let i = 0; i < 60; i++) await key("return");
		await scm("(make-section 'section)");
		for (const c of "Again") await key(c);
		await key("return");
		for (let i = 0; i < 8; i++) await key("x");
		await sleep(1500);
		await page.evaluate(() => window.step());
		for (let i = 0; i < 140; i++) await scm("(undo 0)");
		await sleep(1500);
		const hf = await page.evaluate(() => window.hf || []);
		const counts = [...new Set(hf.map(l => (/\((\d+) pages, /.exec(l) || [])[1]).filter(Boolean))];
		console.log(doc.name + ": stressed, " + hf.length + " passes, the pages: " + counts.join(", "));
	}
	const bad = await page.evaluate(() => window.bad || []);
	if (bad.length) console.log(doc.name + ": the core complains (" + bad.length + "): " + bad.slice(0, 3).join(" | ").slice(0, 600));
	await page.close();
	return { size, real, dry, whole };
}

const onlyDoc = opt("--only", "");
for (const density of opt("--edits", "") ? [2] : [2, 1]) {
	console.log(`\nbench: view at density ${density}; means in ms of the core, for a step`);
	console.log("document  what     | turn   | typeset/editors | drawing | of which saying what to draw | sending pixels | commands and bytes of a step");
	for (const doc of DOCS) {
		if (onlyDoc && doc.name !== onlyDoc) continue;
		let r;
		try { r = await run(doc, density); } catch (error) { console.log(doc.name + ": " + error.message); continue; }
		if (!r) continue;
		const c = r.dry.counts || { glyphs: 0, rects: 0, paths: 0, pictures: 0, state: 0, copies: 0, bytes: 0 };
		const all = c.glyphs + c.rects + c.paths + c.pictures + c.state + c.copies;
		const s = r.real.scroll, sd = r.dry.scroll, t = r.real.type, td = r.dry.type;
		console.log(`${doc.name.padEnd(9)} scroll   |${f1(median(s.turn))} |${f1(median(s.editors))}           |${f1(median(s.drawing))}  |${f1(median(sd.drawing))}` +
			`                        |${f1(median(s.sending))}          | (the 70 steps of the dry pass: ${all} commands, ${(c.bytes / 1024).toFixed(0)} KB; ${c.glyphs} glyphs, ${c.rects} rectangles, ${c.paths} paths, ${c.pictures} pictures, ${c.state} changes of state, ${c.copies} copies)`);
		console.log(`${"".padEnd(9)} typing   |${f1(median(t.turn))} |${f1(median(t.typeset))}           |${f1(median(t.drawing))}  |${f1(median(td.drawing))}` +
			`                        |${f1(median(t.sending))}          | ${Math.round(median(t.pixels))} pixels drawn of ${r.size.w * r.size.h}; the document is ${Math.round(r.whole / r.size.h)} views high`);
	}
}
await browser.close();
server.close();
