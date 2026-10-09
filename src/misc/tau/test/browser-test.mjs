// The page of Tau in a browser without a display: what the user does, and
// what is then checked of the page and of the messages it exchanges with
// the core. Run by "make browser-check".
//
//   node misc/tau/test/browser-test.mjs [--dir build-tau/out/web]
//        [--tools build-tau/tools] [--browser <program>] [--only <name>]
//        [--shots <dir>] [--list]
//
// --tools is where puppeteer-core is (npm install puppeteer-core there);
// --browser a Firefox (the default: the one of /Applications or of the
// path) or a Chrome; --only runs the tests whose name contains the text;
// --shots keeps a picture of the page at the end of each test.
//
// Each test opens the page anew. What is not tested here: the clipboard of
// the system, a real dead key or input method (their events are sent),
// the file chooser when the browser does not let it be driven, and what
// needs the network (the Python of the plugin, a server).

import fs from "node:fs";
import net from "node:net";
import os from "node:os";
import path from "node:path";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import { serve } from "../../wasm/serve.mjs";

const here = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const opt = (name, dflt) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : dflt; };
const dir = path.resolve(opt("--dir", "build-tau/out/web"));
const tools = path.resolve(opt("--tools", "build-tau/tools"));
const only = opt("--only", "");
const shots = opt("--shots", "");
const mac = process.platform === "darwin";
const MOD = mac ? "Meta" : "Control";

function findBrowser() {
	const given = opt("--browser", "");
	if (given) return given;
	const known = ["/Applications/Firefox.app/Contents/MacOS/firefox", "/usr/bin/firefox", "/snap/bin/firefox",
		"/Applications/Google Chrome.app/Contents/MacOS/Google Chrome", "/usr/bin/google-chrome", "/usr/bin/chromium"];
	return known.find(p => fs.existsSync(p)) || "firefox";
}

let puppeteer;
try { puppeteer = createRequire(path.join(tools, "package.json"))("puppeteer-core"); }
catch (error) {
	console.log(`browser: puppeteer-core is not in ${tools} (npm install puppeteer-core there, or --tools <dir>)`);
	process.exit(2);
}

const sleep = ms => new Promise(r => setTimeout(r, ms));

class Failure extends Error {}
function check(what, ok, got) {
	if (!ok) throw new Failure(what + (got === undefined ? "" : ": " + (typeof got === "string" ? got : JSON.stringify(got))));
}

// What a test does with the page
class Page {
	constructor(page, base) { this.page = page; this.base = base; }

	async open(query = "", width = 1100, height = 760) {
		await this.page.setViewport({ width, height, deviceScaleFactor: 2 });
		await this.page.goto(this.base + "?debug" + (query ? "&" + query : ""), { waitUntil: "load" });
		await this.until("the page starts", () => this.page.evaluate(() => !!(window.tau && tau.state.started)), 90000);
		// what goes to the core and what comes from it
		await this.page.evaluate(() => {
			window.sent = []; window.got = [];
			tau.worker.addEventListener("message", e => {
				const m = e.data;
				if (m.t === "log") { if (/[Ee]rror/.test(m.text)) window.got.push({ t: "error", text: m.text }); }
				else if (m.t !== "paint") window.got.push({ t: m.t, part: m.part, view: m.view, visible: m.visible, name: m.name, text: m.text });
			});
			tau.worker.postMessage = new Proxy(tau.worker.postMessage, {
				apply(target, self, a) { window.sent.push(a[0]); return Reflect.apply(target, self, a); }
			});
		});
		await this.until("the bars come", () => this.count("#tau-menu .tau-entry").then(n => n >= 8), 30000);
		await sleep(600);
	}

	// wait until something holds
	async until(what, test, timeout = 8000) {
		const end = Date.now() + timeout;
		for (;;) {
			let r = false;
			try { r = await test(); } catch (error) {}
			if (r) return r;
			if (Date.now() > end) throw new Failure("waited in vain: " + what);
			await sleep(100);
		}
	}

	count(selector) { return this.page.$$eval(selector, l => l.length); }
	sent(t) { return this.page.evaluate(t => window.sent.filter(m => !t || m.t === t), t); }
	got(t) { return this.page.evaluate(t => window.got.filter(m => !t || m.t === t), t); }
	forget() { return this.page.evaluate(() => { window.sent = []; window.got = []; }); }
	scheme(code) { return this.page.evaluate(code => tau.send({ t: "scheme", code }), code); }
	async errors() { return (await this.got("error")).map(m => m.text); }

	// the element among those of a selector whose text starts with one
	async find(selector, text) {
		for (const h of await this.page.$$(selector)) {
			const t = (await h.evaluate(e => e.textContent)).trim();
			if (t.startsWith(text)) return h;
		}
		return null;
	}

	// open a menu of the menu bar and go through its submenus to an entry
	async menu(...names) {
		let top = null;
		await this.until("the menu " + names[0], async () => (top = await this.find("#tau-menu .tau-entry", names[0])));
		await top.click();
		await this.until("the menu " + names[0] + " opens", () => this.count(".tau-popup .tau-entry").then(n => n > 0));
		let entry = null;
		for (const [i, name] of names.slice(1).entries()) {
			await this.until("the entry " + name, async () => {
				const popups = await this.page.$$(".tau-popup");
				const popup = popups[Math.min(i, popups.length - 1)];
				if (!popup) return false;
				for (const h of await popup.$$(".tau-entry")) {
					const t = (await h.evaluate(e => e.textContent)).trim();
					if (t.startsWith(name)) { entry = h; return true; }
				}
				return false;
			});
			await entry.hover();
			if (i < names.length - 2)
				await this.until("the submenu of " + name, () => this.count(".tau-popup").then(n => n > i + 1));
		}
		return entry;
	}
	async choose(...names) { await (await this.menu(...names)).click(); }

	dialogs() {
		return this.page.$$eval(".tau-dialog", l => l.map(d => ({
			title: d.querySelector(".tau-dialog-name").textContent,
			controls: d.querySelectorAll("input, select, button").length,
			unsupported: d.querySelectorAll(".tau-unsupported").length,
			text: d.textContent
		})));
	}
	dialog(title) { return this.until("the dialog " + title, async () => (await this.dialogs()).find(d => d.title.startsWith(title))); }

	async click(x, y, options) { await this.page.mouse.click(x, y, options); }
	async type(text) { await this.page.keyboard.type(text); }
	async key(name, ...mods) {
		for (const m of mods) await this.page.keyboard.down(m);
		await this.page.keyboard.press(name);
		for (const m of mods.reverse()) await this.page.keyboard.up(m);
	}
	tabs() {
		return this.page.$$eval(".tau-pane", l => l.map(p => Array.from(p.querySelectorAll(".tau-doc-tab"), t => ({
			name: t.querySelector(".tau-doc-name").textContent, current: t.classList.contains("tau-current")
		}))));
	}
	box(selector) {
		return this.page.$eval(selector, e => { const r = e.getBoundingClientRect(); return { x: r.left, y: r.top, w: r.width, h: r.height, hidden: e.hidden }; });
	}
	extents() { return this.page.evaluate(() => ({ ...tau.active.extents })); }
	// a new document in the first pane, which has the keyboard
	async newDocument() {
		const before = (await this.tabs())[0].length;
		await this.page.click(".tau-doc-button");
		await this.until("a new tab", async () => (await this.tabs())[0].length === before + 1);
		await sleep(300);
	}
}

// ---------------------------------------------------------------------------
// The tests
// ---------------------------------------------------------------------------

const tests = [];
const test = (name, run) => tests.push({ name, run });

test("start", async p => {
	await p.open();
	check("the menu bar", (await p.count("#tau-menu .tau-entry")) >= 8);
	const icons = await p.page.$$eval("#tau-icons-0 img", l => [l.length, l.filter(i => i.naturalWidth > 0).length]);
	check("the icons of the main bar are drawn", icons[0] > 10 && icons[0] === icons[1], icons);
	const column = await p.box("#tau-icons-0"), middle = await p.box("#tau-middle");
	check("the main icons are a column at the left", column.w < 60 && column.h > 400, column);
	check("the views are at its right", middle.x > column.w, middle);
	const tabs = await p.tabs();
	check("one pane with tabs, one of them shown", tabs.length === 1 && tabs[0].filter(t => t.current).length === 1, tabs);
	check("the title of the page", (await p.page.title()).endsWith("– Tau"), await p.page.title());
	check("the keyboard is in the text area of the views", await p.page.evaluate(() => document.activeElement.classList.contains("tau-keyboard")));
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("typing", async p => {
	await p.open();
	await p.newDocument();
	await p.forget();
	await p.type("Hello, a<b & x>y");
	await p.until("the text is sent", async () => (await p.sent("text")).length === 16);
	check("the text as it was typed", (await p.sent("text")).map(m => m.text).join("") === "Hello, a<b & x>y");
	await p.until("the document is marked as changed", async () => (await p.tabs())[0].some(t => t.current && t.name.endsWith("•")));
	await p.forget();
	await p.key("ArrowLeft", "Shift");
	await p.key("Enter");
	check("the keys by their names", JSON.stringify((await p.sent("key")).map(m => m.key)) === '["S-left","return"]', await p.sent("key"));
	// a dead key, as the browser tells it
	await p.forget();
	await p.page.evaluate(() => {
		const a = document.querySelector(".tau-keyboard");
		a.dispatchEvent(new CompositionEvent("compositionstart", { data: "" }));
		a.dispatchEvent(new CompositionEvent("compositionupdate", { data: "´" }));
		a.value = "é";
		a.dispatchEvent(new CompositionEvent("compositionend", { data: "é" }));
		a.dispatchEvent(new InputEvent("input", { data: "é" }));
	});
	await sleep(300);
	check("the composition is shown, then ended", JSON.stringify((await p.sent("key")).map(m => m.key)) === '["pre-edit:1:´","pre-edit:"]', await p.sent("key"));
	check("what was composed is sent once", JSON.stringify((await p.sent("text")).map(m => m.text)) === '["é"]', await p.sent("text"));
	// a key while nothing has the keyboard goes to the view
	await p.page.evaluate(() => document.activeElement.blur());
	await p.forget();
	await p.type("q");
	await p.until("the key reaches the view", async () => (await p.sent("text")).some(m => m.text === "q"));
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("zoom keys", async p => {
	await p.open();
	await p.click(600, 500);
	const h0 = (await p.extents()).height;
	await p.forget();
	await p.key("=", MOD, "Shift");
	await p.until("the document is larger", async () => (await p.extents()).height > h0 * 1.1);
	const zoomIn = (await p.sent("key")).map(m => m.key);
	check("the key is " + (mac ? "M-+" : "C-+"), zoomIn.length === 1 && zoomIn[0] === (mac ? "M-+" : "C-+"), zoomIn);
	await p.key("-", MOD);
	await p.until("the document is as before", async () => Math.abs((await p.extents()).height - h0) < 3);
	check("the page itself was not zoomed", (await p.page.evaluate(() => window.devicePixelRatio)) === 2);
});

test("menus", async p => {
	await p.open();
	await p.click(600, 500);
	const entry = await p.menu("Insert", "Mathematics", "Inline formula");
	check("three menus are open", (await p.count(".tau-popup")) >= 2);
	await p.forget();
	await entry.click();
	await p.until("the menus close", async () => (await p.count(".tau-popup")) === 0);
	check("the action was asked for", (await p.sent("invoke")).length === 1);
	const footer = () => p.page.$eval("#tau-footer", e => e.textContent);
	try { await p.until("the footer tells the formula", async () => /math/.test(await footer()), 20000); }
	catch (error) { check("the footer tells the formula", false, await footer()); }
	// the context menu, where the pointer is
	await p.click(700, 420, { button: "right" });
	await p.until("the context menu", async () => (await p.count(".tau-popup")) === 1);
	const at = await p.box(".tau-popup");
	check("it is under the pointer", Math.abs(at.x - 700) < 30 && Math.abs(at.y - 420) < 30, at);
	await p.key("Escape");
	await p.until("Escape closes it", async () => (await p.count(".tau-popup")) === 0);
});

test("question", async p => {
	await p.open();
	await p.click(600, 500);
	await p.choose("Format", "Whitespace", "Rigid");
	const d = await p.dialog("Enter data");
	check("an input, Cancel and Ok", d.controls === 4, d);
	await p.type("3em");
	await p.key("Enter");
	await p.until("the dialog goes", async () => (await p.dialogs()).length === 0);
	check("the keyboard is back in the view", await p.page.evaluate(() => document.activeElement.classList.contains("tau-keyboard")));
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("preferences", async p => {
	await p.open();
	await p.choose("Edit", "Preferences");
	const d = await p.dialog("User preferences");
	check("its controls are all shown", d.controls > 50 && d.unsupported === 0, d);
	check("its tabs", (await p.count(".tau-dialog .tau-tab")) >= 6);
	const pick = (label, value) => p.page.evaluate((label, value) => {
		const cell = Array.from(document.querySelectorAll(".tau-aligned-left")).find(e => e.textContent.includes(label));
		const select = cell && cell.nextElementSibling.querySelector("select");
		if (!select) return null;
		const before = select.value;
		if (value !== null) { select.value = value; select.dispatchEvent(new Event("change", { bubbles: true })); }
		return before;
	}, label, value);
	check("the detailed menus at first", (await pick("Details in menus", "Simplified menus")) === "Detailed menus");
	await p.until("the answer goes to the core", async () => (await p.sent("answer")).length === 1);
	await sleep(600);
	await p.page.click(".tau-dialog-close");
	await p.until("the dialog goes", async () => (await p.dialogs()).length === 0);
	await p.choose("Edit", "Preferences");
	await p.dialog("User preferences");
	check("the preference was kept", (await pick("Details in menus", "Detailed menus")) === "Simplified menus");
	await sleep(600);
});

test("view in a dialog", async p => {
	await p.open();
	await p.click(600, 500);
	await p.choose("Format", "Font");
	const d = await p.dialog("Font selector");
	check("nothing which is not shown", d.unsupported === 0, d);
	await p.until("the sample is drawn", () => p.page.evaluate(() => {
		const c = document.querySelector(".tau-dialog canvas.tau-embedded");
		if (!c || c.width < 100) return false;
		const data = c.getContext("2d").getImageData(0, 0, c.width, Math.min(c.height, 200)).data;
		let dark = 0;
		for (let i = 0; i < data.length; i += 4) if (data[i] < 100) dark++;
		return dark > 50;
	}), 15000);
	await p.forget();
	await p.page.evaluate(() => {
		const select = document.querySelector(".tau-dialog .tau-choice"), o = select.options[3];
		select.value = o.value;
		select.dispatchEvent(new Event("change", { bubbles: true }));
	});
	await p.until("its parts are described again", async () => (await p.got("refresh")).length >= 1);
});

test("editing in a dialog", async p => {
	await p.open();
	await p.click(600, 500);
	await p.choose("Tools", "Macros", "Edit macros");
	await p.dialog("Macros editor");
	const field = await p.until("the field which is edited", () => p.page.$(".tau-dialog canvas.tau-editable"));
	const pane = await p.page.evaluate(() => tau.active.view);
	await field.click();
	await p.forget();
	await p.type("abc");
	await p.until("the text goes to the view of the field", async () => (await p.sent("text")).length === 3);
	const views = new Set((await p.sent("text")).map(m => m.view));
	check("which is not the view of the pane", views.size === 1 && !views.has(pane), [...views]);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("search bar", async p => {
	await p.open();
	await p.click(600, 500);
	await p.choose("Edit", "Search");
	await p.until("the bar under the views", async () => !(await p.box("#tau-bottom-0")).hidden);
	const input = await p.until("its field", () => p.page.$("#tau-bottom-0 input"));
	await input.click();
	await p.type("website");
	check("the field keeps the keyboard and the text", (await p.page.evaluate(() => document.activeElement.value)) === "website");
	check("each key was told to the core", (await p.sent("answer")).length >= 7);
	await p.key("Escape");
	await p.until("Escape closes the bar", async () => (await p.box("#tau-bottom-0")).hidden);
});

test("tabs and files", async p => {
	await p.open();
	await p.newDocument();
	const n = (await p.tabs())[0].length;
	// a file of the user, dropped on the view
	await p.page.evaluate(() => {
		const text = "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n  A file of the user.\n</body>\n";
		const data = new DataTransfer();
		data.items.add(new File([text], "mine.tm"));
		document.querySelector(".tau-canvas").dispatchEvent(new DragEvent("drop", { dataTransfer: data, bubbles: true, cancelable: true }));
	});
	await p.until("the file is a tab, and shown", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm"));
	check("one tab more", (await p.tabs())[0].length === n + 1);
	check("the title of the page", (await p.page.title()) === "mine.tm – Tau");
	// changed, then saved: it goes back to the user
	await p.type("Edited. ");
	await p.until("it is marked as changed", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm •"));
	await p.forget();
	await p.key("s", MOD);
	await p.until("the saved file is given to the user", async () => (await p.got("download")).some(m => m.name === "mine.tm"));
	await p.until("it is not marked any more", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm"));
	// save under a name: the name is asked
	await p.type("More. ");
	await p.choose("File", "Save as");
	await p.dialog("Save");
	await p.forget();
	await p.type("other.tm");
	await p.key("Enter");
	await p.until("the file of that name is given", async () => (await p.got("download")).some(m => m.name === "other.tm"));
	// the first tab again
	await p.page.click(".tau-doc-tab");
	await p.until("the first tab is shown", async () => (await p.tabs())[0][0].current);
	// closing a document which was changed asks
	await p.page.$$eval(".tau-doc-tab", l => l[l.length - 1].click());
	await p.until("the last tab is shown", async () => { const t = (await p.tabs())[0]; return t[t.length - 1].current; });
	await p.type("x");
	await p.until("it is marked as changed", async () => (await p.tabs())[0].some(t => t.current && t.name.endsWith("•")));
	const before = (await p.tabs())[0].length;
	await p.page.$$eval(".tau-doc-tab.tau-current .tau-doc-close", l => l[0].click());
	await p.dialog("Question");
	await (await p.find(".tau-dialog .tau-button", "yes")).click();
	await p.until("the tab goes", async () => (await p.tabs())[0].length === before - 1);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("panes", async p => {
	await p.open();
	await p.click(600, 500);
	await p.choose("View", "New window");
	await p.until("a second pane", async () => (await p.tabs()).length === 2);
	await p.until("which has the keyboard", () => p.page.evaluate(() => document.querySelectorAll(".tau-pane")[1].classList.contains("tau-active")));
	check("a line between them", (await p.count(".tau-divider")) === 1);
	const bars = () => p.page.$eval("#tau-icons-2", e => e.textContent);
	const second = await bars();
	// the bars are those of the pane which has the keyboard
	const first = await p.box(".tau-pane .tau-canvas");
	await p.click(first.x + 150, first.y + 150);
	await p.until("the bars of the first pane", async () => (await bars()) !== second);
	// the line is dragged
	const widths = () => p.page.$$eval(".tau-pane", l => l.map(e => e.offsetWidth));
	const w = await widths(), d = await p.box(".tau-divider");
	await p.page.mouse.move(d.x + 2, d.y + 200);
	await p.page.mouse.down();
	await p.page.mouse.move(d.x - 148, d.y + 200, { steps: 4 });
	await p.page.mouse.up();
	const w2 = await widths();
	check("the first pane is narrower by what was dragged", Math.abs(w[0] - w2[0] - 150) < 6 && Math.abs(w2[1] - w[1] - 150) < 6, [w, w2]);
	await p.until("the views are drawn at their new sizes", () => p.page.$$eval(".tau-canvas", l => l.every(c => Math.abs(c.width - 2 * c.clientWidth) < 3)));
	// the second pane is closed
	await p.page.$$eval(".tau-pane-close", l => l[l.length - 1].click());
	await p.until("one pane again", async () => (await p.tabs()).length === 1);
	check("no line any more", (await p.count(".tau-divider")) === 0);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("side tool", async p => {
	await p.open();
	await p.click(600, 500);
	check("no tool at first", (await p.box("#tau-side-0")).hidden);
	await p.scheme('(begin (set-boolean-preference "developer tool" #t) (set-boolean-preference "side tools" #t))');
	await sleep(800);
	await p.scheme("(open-paragraph-format)");
	await p.until("the tool at the right of the views", async () => !(await p.box("#tau-side-0")).hidden);
	await p.until("with its inputs", async () => (await p.count("#tau-side-0 input, #tau-side-0 select")) >= 5);
	check("and no dialog", (await p.dialogs()).length === 0);
});

test("bars above", async p => {
	await p.open("bars=top");
	const bar = await p.box("#tau-icons-0"), mode = await p.box("#tau-icons-1"), middle = await p.box("#tau-middle");
	check("the main icons are a row", bar.w > 1000 && bar.h < 50, bar);
	check("the mode icons under them, the views under all", mode.y >= bar.y + bar.h - 1 && middle.y > mode.y, [bar, mode, middle]);
});

test("what does not fit", async p => {
	await p.open("", 520, 420);
	const shown = selector => p.page.$$eval(selector + " > .tau-chev", l => l.map(a => a.classList.contains("tau-shown")));
	check("a chevron under the column of the main icons, none above", JSON.stringify(await shown("#tau-icons-0")) === "[false,true]", await shown("#tau-icons-0"));
	await p.page.click("#tau-icons-0 .tau-chev-next .tau-chevron");
	await p.until("it scrolls, and there is one above", async () => (await shown("#tau-icons-0"))[0]);
	check("the column was scrolled", (await p.page.$eval("#tau-icons-0", e => e.scrollTop)) > 20);
	// a menu taller than the page
	await (await p.find("#tau-menu .tau-entry", "Insert")).click();
	await p.until("the menu opens", async () => (await p.count(".tau-popup .tau-entry")) > 5);
	const fits = await p.page.$eval(".tau-popup", e => e.scrollHeight <= e.clientHeight + 1);
	check("the menu does not fit", !fits);
	check("a chevron at its bottom", (await shown(".tau-popup"))[1]);
	await p.page.hover(".tau-popup .tau-chev-next .tau-chevron");
	await p.until("the pointer on it scrolls the menu", async () => (await p.page.$eval(".tau-popup", e => e.scrollTop)) > 20);
});

// ---------------------------------------------------------------------------
// The run
// ---------------------------------------------------------------------------

if (args.includes("--list")) { for (const t of tests) console.log(t.name); process.exit(0); }
if (!fs.existsSync(path.join(dir, "tau.wasm"))) {
	console.log(`browser: no build of the page in ${dir} (make web)`);
	process.exit(2);
}

const port = await new Promise(resolve => {
	const s = net.createServer();
	s.listen(0, "127.0.0.1", () => { const p = s.address().port; s.close(() => resolve(p)); });
});
const server = await serve(dir, port);
const program = findBrowser();
const browser = await puppeteer.launch({
	browser: /firefox/i.test(program) ? "firefox" : "chrome", headless: true, executablePath: program,
	userDataDir: fs.mkdtempSync(path.join(os.tmpdir(), "tau-browser-"))
});
if (shots) fs.mkdirSync(shots, { recursive: true });

let failed = 0, ran = 0;
for (const t of tests) {
	if (only && !t.name.includes(only)) continue;
	ran++;
	const page = await browser.newPage();
	const errors = [];
	page.on("pageerror", e => errors.push(String(e)));
	const start = Date.now();
	let problem = null;
	try {
		await t.run(new Page(page, `http://127.0.0.1:${port}/index.html`));
		// (the pictures of the documentation come from another site, which
		// the browser refuses: not an error of the page)
		const real = errors.filter(e => !/Cross-Origin/.test(e));
		if (real.length) problem = "an error of the page: " + real[0].slice(0, 300);
	} catch (error) {
		problem = error instanceof Failure ? error.message : String(error && error.stack || error).slice(0, 600);
	}
	if (shots) try { await page.screenshot({ path: path.join(shots, t.name.replace(/\W+/g, "-") + ".png") }); } catch (error) {}
	await page.close();
	const time = ((Date.now() - start) / 1000).toFixed(1) + " s";
	if (problem) { failed++; console.log(`browser: FAILED ${t.name} (${time}): ${problem}`); }
	else console.log(`browser: ${t.name} (${time})`);
}
await browser.close();
if (server && server.close) server.close();
console.log(failed ? `browser: ${failed} of ${ran} tests failed` : `browser: ${ran} tests pass`);
process.exit(failed ? 1 : 0);
