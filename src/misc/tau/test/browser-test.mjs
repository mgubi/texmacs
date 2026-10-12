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
		// (nothing is kept from a test to the next, unless the test asks: "home")
		const home = /(^|&)home($|&)/.test(query);
		query = query.replace(/(^|&)home($|&)/, "$2");
		await this.page.goto(this.base + "?debug" + (home ? "" : "&nohome") + (query ? "&" + query : ""), { waitUntil: "load" });
		await this.until("the page starts", () => this.page.evaluate(() => !!(window.tau && tau.state.started)), 90000);
		// what goes to the core and what comes from it
		await this.page.evaluate(() => {
			window.sent = []; window.got = [];
			tau.worker.addEventListener("message", e => {
				const m = e.data;
				if (m.t === "pong") return;
				if (m.t === "log") { if (/[Ee]rror/.test(m.text)) window.got.push({ t: "error", text: m.text }); }
				else if (m.t === "paint") window.got.push({ t: "paint", view: m.view, w: m.w, h: m.h, width: m.width, height: m.height });
				else window.got.push({ t: m.t, part: m.part, view: m.view, visible: m.visible, name: m.name, text: m.text, width: m.width, height: m.height });
			});
			tau.worker.postMessage = new Proxy(tau.worker.postMessage, {
				apply(target, self, a) { if (a[0].t !== "ping") window.sent.push(a[0]); return Reflect.apply(target, self, a); }
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

	// the panel of the files in a mode (browse, open, save), once it shows
	// a folder: the folder, the names in it, the name which is proposed
	filesShown() {
		return this.page.evaluate(() => {
			const b = document.querySelector(".tau-files");
			if (!b || !b.dataset.loaded) return null;
			const name = b.querySelector(".tau-files-foot .tau-files-name"), ask = b.querySelector(".tau-files-ask");
			return { mode: b.dataset.mode, dir: b.dataset.dir, title: b.querySelector(".tau-dialog-name").textContent,
				names: Array.from(b.querySelectorAll(".tau-files-row"), r => r.dataset.name), name: name ? name.value : "",
				place: b.querySelector(".tau-files-place.tau-current").dataset.place,
				asking: ask.style.display === "none" ? "" : ask.textContent,
				tools: b.querySelector(".tau-files-tools").style.display !== "none" };
		});
	}
	filesPanel(mode, test = () => true) {
		return this.until("the panel of the files (" + mode + ")", async () => {
			const f = await this.filesShown();
			return f && f.mode === mode && test(f) ? f : null;
		});
	}
	// a row of the panel: clicked, clicked twice, or one of its actions
	filesRow(name, what = "click") {
		return this.page.evaluate((name, what) => {
			const row = Array.from(document.querySelectorAll(".tau-files-row")).find(r => r.dataset.name === name);
			if (what === "click") row.click();
			else if (what === "dblclick") row.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
			else row.querySelector('[data-act="' + what + '"]').click();
		}, name, what);
	}
	// a name, in the field which has the keyboard (the one of the panel, or
	// of a question in it)
	async filesName(name) {
		await this.page.evaluate(() => document.activeElement.select());
		await this.type(name);
	}
	// files which are dropped on the panel
	filesDrop(files) {
		return this.page.evaluate(files => {
			const data = new DataTransfer();
			for (const [name, text] of files) data.items.add(new File([text], name));
			document.querySelector(".tau-files-veil").dispatchEvent(new DragEvent("drop", { dataTransfer: data, bubbles: true, cancelable: true }));
		}, files);
	}

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
	// the panel of the loading is gone, the icon of Tau is at the top left
	await p.until("the panel of the loading goes", async () => (await p.count(".tau-loading")) === 0);
	const icon = await p.box("#tau-app img");
	check("the icon of Tau at the top left", icon.x < 20 && icon.y < 20 && icon.w > 15, icon);
	await p.page.click("#tau-app");
	await p.until("its menu", async () => (await p.count(".tau-app-menu")) === 1);
	const text = await p.page.$eval(".tau-app-menu", e => e.textContent);
	check("says what Tau is and how to start it again", text.includes("GNU TeXmacs") && text.includes("Start Tau again"), text.slice(0, 120));
	await p.key("Escape");
	await p.until("Escape closes it", async () => (await p.count(".tau-app-menu")) === 0);
});

test("loading", async p => {
	// the panel while Tau loads: watched from the first moment of the page
	await p.page.setViewport({ width: 1100, height: 760, deviceScaleFactor: 2 });
	await p.page.evaluateOnNewDocument(() => {
		window.seenLoading = [];
		const watch = () => {
			const d = document.querySelector(".tau-loading-detail");
			if (d && window.seenLoading[window.seenLoading.length - 1] !== d.textContent) window.seenLoading.push(d.textContent);
			if (!(window.tau && window.tau.state.started)) requestAnimationFrame(watch);
		};
		requestAnimationFrame(watch);
	});
	await p.open();
	const seen = await p.page.evaluate(() => window.seenLoading);
	check("the share which is downloaded was told", seen.some(t => /^Downloading: \d+%/.test(t)), seen);
	check("then that TeXmacs starts", seen.includes("Starting TeXmacs…"), seen);
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

test("drawing and scrolling", async p => {
	await p.open();
	await p.newDocument();
	await p.type("Some text.");
	await sleep(500);
	// a key draws the part of the view which changes, not all of it
	await p.forget();
	await p.type("x");
	await p.until("the view is drawn again", async () => (await p.got("paint")).length >= 1);
	const paint = (await p.got("paint"))[0];
	check("only a part of the canvas is sent", paint.w * paint.h < 0.25 * paint.width * paint.height, paint);
	// the document is taller than the pane: a bar at the right
	check("a scroll bar at the right", !(await p.box(".tau-scroll-y")).hidden);
	const scroll = () => p.page.evaluate(() => tau.active.scroll.y);
	const thumb = await p.box(".tau-scroll-y .tau-thumb");
	check("the view is at the top", (await scroll()) === 0, await scroll());
	await p.page.mouse.move(thumb.x + thumb.w / 2, thumb.y + thumb.h / 2);
	await p.page.mouse.down();
	await p.page.mouse.move(thumb.x + thumb.w / 2, thumb.y + thumb.h / 2 + 120, { steps: 4 });
	await p.page.mouse.up();
	await p.until("dragging the thumb scrolls the view", async () => (await scroll()) > 200);
	const moved = await p.box(".tau-scroll-y .tau-thumb");
	check("and the thumb follows", moved.y > thumb.y + 60, [thumb, moved]);
	// a click above the thumb goes back by what is seen
	const track = await p.box(".tau-scroll-y");
	const before = await scroll();
	await p.click(track.x + track.w / 2, track.y + 3);
	await p.until("a click above the thumb scrolls back", async () => (await scroll()) < before);
});

test("paste from the menu", async p => {
	await p.open();
	await p.newDocument();
	await p.type("word");
	await p.key("ArrowLeft", "Shift"); await p.key("ArrowLeft", "Shift");
	await p.forget();
	await p.key("c", MOD);
	await p.until("what is copied is given to the page", async () => (await p.got("clipboard")).some(m => m.text === "rd"));
	await p.key("End");
	await p.forget();
	await p.choose("Edit", "Paste");
	await p.until("the page is asked for its clipboard", async () => (await p.got("paste-request")).length === 1);
	await p.until("and answers", async () => (await p.sent("paste")).length === 1);
	await p.until("the document changes", async () => (await p.got("paint")).length >= 1);
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

test("colours and palettes", async p => {
	await p.open();
	await p.newDocument();
	await p.type("colour");
	await p.key("ArrowLeft", "Shift"); await p.key("ArrowLeft", "Shift");
	await p.menu("Format", "Color");
	const swatches = () => p.page.$$eval(".tau-popup .tau-tile .tau-color", l => l.map(e => getComputedStyle(e).backgroundColor));
	await p.until("the grid of colours", async () => (await swatches()).length >= 16);
	const classical = await swatches();
	check("the colours are drawn", new Set(classical).size > 8 && !classical.includes("rgba(0, 0, 0, 0)"), classical.slice(0, 6));
	// the palette is chosen in the menu, which stays open and changes
	const sets = await p.page.$$eval(".tau-popup select option", l => l.map(o => o.value));
	check("the sets of colours to choose from", sets.includes("Classical") && sets.includes("Nordic"), sets);
	await p.forget();
	await p.page.evaluate(() => {
		const select = document.querySelector(".tau-popup select");
		select.value = "Nordic";
		select.dispatchEvent(new Event("change", { bubbles: true }));
	});
	await p.until("the grid is described again", async () => (await p.got("refresh")).length >= 1);
	await p.until("with the groups of the palette", () => p.page.$$eval(".tau-popup .tau-group", l => l.map(e => e.textContent).join(",").includes("Accents")));
	const nordic = await swatches();
	check("and other colours", nordic.length >= 16 && nordic.join() !== classical.join(), nordic.slice(0, 6));
	check("the menu is still open", (await p.count(".tau-popup")) >= 2);
	// a colour is chosen
	await p.forget();
	await (await p.page.$$(".tau-popup .tau-tile .tau-entry"))[10].click();
	await p.until("the colour is applied", async () => (await p.sent("invoke")).length === 1 && (await p.count(".tau-popup")) === 0);
	// (the next tests start from the standard grid)
	await p.scheme('(set-typographic-palette-set "Classical")');
	await sleep(300);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
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
		for (let i = 0; i < data.length; i += 4) if (data[i + 3] > 200 && data[i] < 100) dark++;
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
	// (a new document which is exported is not proposed as a document of TeXmacs)
	await p.choose("File", "Export", "Pdf");
	const first = await p.filesPanel("save");
	check("a name with the suffix of the format for a new document", first.name === "untitled.pdf", first);
	await p.key("Escape");
	await p.until("the panel is given up", async () => (await p.count(".tau-files")) === 0);
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
	// changed, then saved: it is kept, and given to the user when asked
	await p.type("Edited. ");
	await p.until("it is marked as changed", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm •"));
	await p.forget();
	await p.key("s", MOD);
	await p.until("it is not marked any more", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm"));
	check("saving does not give the file to the user", (await p.got("download")).length === 0);
	await p.choose("File", "Download");
	await p.until("it is given when asked for", async () => (await p.got("download")).some(m => m.name === "mine.tm"));
	// save under a name: the panel of the files, in the folder of the
	// document and with its name
	await p.type("More. ");
	await p.choose("File", "Save as");
	let f = await p.filesPanel("save");
	check("the folder and the name of the document", f.dir === "/home/tau/Documents" && f.name === "mine.tm" && f.names.includes("mine.tm"), f);
	await p.forget();
	await p.filesName("other.tm");
	await p.key("Enter");
	await p.until("the document has the new name, and is not marked as changed", async () => (await p.tabs())[0].some(t => t.current && t.name === "other.tm"));
	check("the panel is gone", (await p.count(".tau-files")) === 0);
	// a name which is taken: asked first
	await p.choose("File", "Save as");
	await p.filesPanel("save");
	await p.filesRow("mine.tm");
	f = await p.filesPanel("save", f => f.name === "mine.tm");
	await p.key("Enter");
	f = await p.filesPanel("save", f => f.asking.includes("Replace mine.tm"));
	await p.key("Escape");
	await p.filesPanel("save", f => !f.asking);
	await p.key("Escape");
	await p.until("the panel is given up", async () => (await p.count(".tau-files")) === 0);
	check("the document keeps its name", (await p.tabs())[0].some(t => t.current && t.name === "other.tm"));
	// load: the documents which are kept are offered
	await p.choose("File", "Load");
	f = await p.filesPanel("open");
	check("the two documents which were saved", f.names.includes("mine.tm") && f.names.includes("other.tm"), f);
	await p.filesRow("mine.tm", "dblclick");
	await p.until("the one which is chosen is shown", async () => (await p.tabs())[0].some(t => t.current && t.name === "mine.tm"));
	// exported: the file is for the computer
	await p.forget();
	await p.choose("File", "Export", "Pdf");
	f = await p.filesPanel("save");
	check("the name with the suffix of the format", f.name === "mine.pdf", f);
	await p.key("Enter");
	await p.until("the PDF is given to the user", async () => (await p.got("download")).some(m => m.name === "mine.pdf"), 30000);
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

test("files in the browser", async p => {
	await p.open();
	await p.newDocument();
	const n = (await p.tabs())[0].length;
	await p.choose("File", "Files in this browser");
	let f = await p.filesPanel("browse");
	check("the documents of the user, none yet", f.dir === "/home/tau/Documents" && f.names.length === 0 && f.place === "files", f);
	// what is typed while the panel is there is not for the document
	await p.forget();
	await p.type("zz");
	await sleep(300);
	check("the keys stay in the panel", (await p.sent("key")).length === 0 && (await p.sent("text")).length === 0, await p.sent());
	// a folder, with files which are dropped in it
	await (await p.find(".tau-files-tools .tau-button", "New folder")).click();
	await p.filesPanel("browse", f => f.asking.startsWith("New folder"));
	await p.filesName("proj");
	await p.key("Enter");
	await p.filesPanel("browse", f => f.names.includes("proj") && !f.asking);
	await p.filesRow("proj");
	await p.filesPanel("browse", f => f.dir === "/home/tau/Documents/proj");
	const doc = "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n  A document of a project.\n</body>\n";
	await p.filesDrop([["a.tm", doc], ["pic.svg", "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'/>"]]);
	f = await p.filesPanel("browse", f => f.names.length === 2);
	check("the files which were dropped", f.names.join() === "a.tm,pic.svg", f);
	// what comes with a name which is taken is asked about: kept besides, or written over
	await p.filesDrop([["pic.svg", "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'/>"]]);
	f = await p.filesPanel("browse", f => f.asking.startsWith("pic.svg exists already"));
	check("nothing is written before the answer", f.names.length === 2, f);
	// (the question is about this folder: going elsewhere gives it up)
	await p.page.click(".tau-files-crumbs a:nth-of-type(2)");
	await p.filesPanel("browse", f => f.dir === "/home/tau/Documents" && !f.asking);
	await p.filesRow("proj");
	f = await p.filesPanel("browse", f => f.dir === "/home/tau/Documents/proj");
	check("given up: nothing was written", f.names.join() === "a.tm,pic.svg", f);
	await p.filesDrop([["pic.svg", "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='8'/>"]]);
	await p.filesPanel("browse", f => f.asking.startsWith("pic.svg exists already"));
	await p.page.click('.tau-files-ask [data-what="other"]');
	await p.filesPanel("browse", f => !f.asking && f.names.join() === "a.tm,pic 2.svg,pic.svg");
	await p.filesDrop([["pic 2.svg", "<svg xmlns='http://www.w3.org/2000/svg' width='9' height='9'><script>window.opener.hacked = true</script></svg>"]]);
	await p.filesPanel("browse", f => f.asking.startsWith("pic 2.svg exists already"));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.filesPanel("browse", f => !f.asking && f.names.length === 3);
	// an image is shown as a picture in a tab of the browser: its scripts do not run
	const shown = new Promise(resolve => browser.once("targetcreated", t => resolve(t.page())));
	await p.filesRow("pic 2.svg", "open");
	const tab = await shown;
	await p.until("the picture is in the tab", () => tab.evaluate(() => { const i = document.querySelector("img"); return !!i && i.complete && i.naturalWidth === 9; }));
	check("the tab is not the file", (await tab.evaluate(() => location.href)) === "about:blank" && !(await p.page.evaluate(() => window.hacked)));
	await tab.close();
	await p.page.bringToFront();
	await p.filesRow("pic 2.svg", "delete");
	await p.filesPanel("browse", f => f.asking.startsWith("Delete pic 2.svg"));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.filesPanel("browse", f => !f.asking && f.names.join() === "a.tm,pic.svg");
	// renamed, copied to the computer (a folder as a zip)
	await p.filesRow("a.tm", "rename");
	await p.filesPanel("browse", f => f.asking.startsWith("Rename a.tm"));
	await p.filesName("b.tm");
	await p.key("Enter");
	await p.filesPanel("browse", f => f.names.join() === "b.tm,pic.svg");
	await p.filesRow("b.tm", "save copy");
	await p.until("the file is given to the user", () => p.page.evaluate(() => tau.state.downloads.some(d => d.name === "b.tm" && d.size > 50)));
	await p.page.click(".tau-files-crumbs a:nth-of-type(2)");
	await p.filesPanel("browse", f => f.dir === "/home/tau/Documents");
	await p.filesRow("proj", "save copy");
	await p.until("the folder is given as a zip", () => p.page.evaluate(() => tau.state.downloads.some(d => d.name === "proj.zip" && d.size > 100)));
	// a zip, there and back; a backup has the files
	const back = await p.page.evaluate(async () => {
		const enc = new TextEncoder(), text = "The quick brown fox. ".repeat(40);
		const blob = await tau.files.zip([{ rel: "d/one.txt", bytes: enc.encode(text), time: Date.UTC(2026, 0, 2, 3, 4, 6) }, { rel: "two", bytes: enc.encode("2") }]);
		const l = await tau.files.unzip(new Uint8Array(await blob.arrayBuffer()));
		// (a zip with its folders; one whose bytes changed; one of more files than the end record of a zip counts)
		const bytes = async files => new Uint8Array(await (await tau.files.zip(files)).arrayBuffer());
		const withDir = await tau.files.unzip(await bytes([{ rel: "empty/", bytes: new Uint8Array(0) }, { rel: "f", bytes: enc.encode("f") }]), true);
		const hurt = await bytes([{ rel: "x.txt", bytes: enc.encode("unchanged") }]);
		hurt[30 + 5 + 2] ^= 1;
		let damaged = "";
		try { await tau.files.unzip(hurt); } catch (error) { damaged = error.message; }
		const many = [];
		for (let i = 0; i < 66000; i++) many.push({ rel: "m/" + i, bytes: new Uint8Array(0) });
		const all = (await tau.files.unzip(await bytes(many))).length;
		await tau.fs("mkdir", { path: "/home/tau/Documents/nothing-in-it" });
		const b = await tau.files.makeBackup();
		await tau.fs("remove", { path: "/home/tau/Documents/nothing-in-it" });
		const r = await tau.files.readBackup(new Uint8Array(await b.blob.arrayBuffer()));
		return { names: l.map(e => e.rel).join(), same: new TextDecoder().decode(l[0].bytes) === text, small: blob.size < text.length,
			backup: r.entries.map(e => e.rel), application: r.note.application, missing: r.missing,
			emptyDir: r.entries.some(e => e.rel === "Documents/nothing-in-it" && e.dir),
			dirs: withDir.map(e => e.rel + (e.dir ? "/" : "")).join(), damaged, all };
	});
	check("a zip keeps its folders", back.dirs === "empty/,f", back.dirs);
	check("a zip whose bytes changed is refused", back.damaged.includes("damaged"), back.damaged);
	check("a zip of very many files", back.all === 66000, back.all);
	check("the backup has the folders which are empty, and all its files", back.emptyDir && back.missing === 0, back);
	check("a zip is read back", back.names === "d/one.txt,two" && back.same && back.small, back);
	check("the backup has the files of the user", back.backup.includes("Documents/proj/b.tm") && back.application === "Tau", back);
	// what is not a backup is refused, and a backup cannot write outside the home directory
	const bad = await p.page.evaluate(async () => {
		const enc = new TextEncoder(), bytes = async files => new Uint8Array(await (await tau.files.zip(files)).arrayBuffer());
		let refused = "";
		try { await tau.files.readBackup(await bytes([{ rel: "home/Documents/x.tm", bytes: enc.encode("x") }])); } catch (error) { refused = error.message; }
		const note = { rel: "tau-backup.json", bytes: enc.encode(JSON.stringify({ format: 1, application: "Tau" })) };
		const r = await tau.files.readBackup(await bytes([note, { rel: "home/../../etc/x", bytes: enc.encode("x") }, { rel: "home/Documents/ok.tm", bytes: enc.encode("x") }, { rel: "elsewhere/y", bytes: enc.encode("y") }]));
		const note3 = { rel: "tau-backup.json", bytes: enc.encode(JSON.stringify({ format: 1, application: "Tau", files: 3 })) };
		const part = await tau.files.readBackup(await bytes([note3, { rel: "home/Documents/ok.tm", bytes: enc.encode("x") }]));
		return { refused, kept: r.entries.map(e => e.rel).join(), missing: part.missing };
	});
	check("a backup which lost files says so", bad.missing === 2, bad);
	check("a zip which is not a backup is refused", bad.refused.includes("not a backup"), bad);
	check("only the paths inside the home directory are restored", bad.kept === "Documents/ok.tm", bad);
	// (a page which does not keep its files does not restore)
	const no = await p.page.evaluate(() => tau.files.makeBackup().then(async b => {
		const r = await tau.files.readBackup(new Uint8Array(await b.blob.arrayBuffer()));
		const files = r.entries.map(e => ({ rel: e.rel, bytes: e.bytes.slice().buffer, time: e.time }));
		return new Promise(resolve => {
			const id = 987654, on = e => { if (e.data.t === "fs" && e.data.id === id) { tau.worker.removeEventListener("message", on); resolve(e.data.error || "restored"); } };
			tau.worker.addEventListener("message", on);
			tau.worker.postMessage({ t: "fs", id, op: "restore", files });
		});
	}));
	check("no restore in a page which keeps nothing", no.includes("does not keep the files"), no);
	// a zip which is added is unpacked
	await p.page.evaluate(async () => {
		const blob = await tau.files.zip([{ rel: "notes/x.txt", bytes: new TextEncoder().encode("x") }]);
		const data = new DataTransfer();
		data.items.add(new File([blob], "notes.zip"));
		const input = document.querySelector('.tau-files input[data-what="zip"]');
		input.files = data.files;
		input.dispatchEvent(new Event("change", { bubbles: true }));
	});
	await p.filesPanel("browse", f => f.names.includes("notes"));
	// deleted, after a question
	await p.filesRow("notes", "delete");
	await p.filesPanel("browse", f => f.asking.startsWith("Delete notes and everything in it"));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.filesPanel("browse", f => !f.asking && f.names.join() === "proj");
	// the files of TeXmacs: shown, not changed; one is copied to the
	// TeXmacs folder of the user
	await p.page.click('.tau-files-place[data-place="sys"]');
	f = await p.filesPanel("browse", f => f.dir === "/texmacs");
	check("the files of TeXmacs", f.names.includes("styles") && f.names.includes("LICENSE") && !f.tools, f);
	await p.filesRow("LICENSE", "customize");
	await p.until("it is copied", () => p.count(".tau-toast").then(n => n > 0));
	await p.page.click('.tau-files-place[data-place="tm"]');
	f = await p.filesPanel("browse", f => f.dir === "/home/tau/.TeXmacs");
	check("the copy in the TeXmacs folder of the user", f.names.includes("LICENSE") && f.tools, f);
	// a folder of TeXmacs is copied into the one of the user: what the user has there stays
	const merged = await p.page.evaluate(async () => {
		const mine = "/home/tau/.TeXmacs/progs/kernel/boot/mine.scm";
		await tau.fs("write", { files: [{ path: mine, bytes: new TextEncoder().encode(";; mine").buffer, time: 0 }] });
		await tau.fs("copy", { from: "/texmacs/progs/kernel/boot", to: "/home/tau/.TeXmacs/progs/kernel/boot" });
		const names = (await tau.fs("list", { dir: "/home/tau/.TeXmacs/progs/kernel/boot" })).entries.map(e => e.name);
		let refused = "";
		try { await tau.fs("copy", { from: "/texmacs/progs/kernel", to: "/home/tau/.TeXmacs/LICENSE" }); } catch (error) { refused = error.message; }
		await tau.fs("remove", { path: "/home/tau/.TeXmacs/progs" });
		return { names, refused };
	});
	check("the files of the user stay in a folder which is customized", merged.names.includes("mine.scm") && merged.names.includes("boot.scm"), merged);
	check("a folder does not take the place of a file", merged.refused.includes("is a file"), merged);
	// a document is opened from the panel
	await p.page.click('.tau-files-place[data-place="files"]');
	await p.filesPanel("browse", f => f.dir === "/home/tau/Documents");
	await p.filesRow("proj");
	await p.filesPanel("browse", f => f.names.includes("b.tm"));
	await p.filesRow("b.tm", "open");
	await p.until("the document is a tab", async () => (await p.tabs())[0].some(t => t.current && t.name === "b.tm"));
	check("one tab more, and no panel", (await p.tabs())[0].length === n + 1 && (await p.count(".tau-files")) === 0);
	// the keyboard is the document's again; a save proposes its folder
	await p.type("More.");
	await p.until("typing reaches the document", async () => (await p.tabs())[0].some(t => t.current && t.name === "b.tm •"));
	await p.choose("File", "Save as");
	f = await p.filesPanel("save");
	check("the folder of the document", f.dir === "/home/tau/Documents/proj" && f.name === "b.tm", f);
	await p.key("Escape");
	await p.until("the panel is given up", async () => (await p.count(".tau-files")) === 0);
	// an image is asked for in the same panel
	await p.scheme('(choose-file (lambda (u) (set-message (url->system u) "chosen")) "Load image" "image")');
	f = await p.filesPanel("open");
	check("the title of the question", f.title === "Load image", f);
	await p.filesRow("proj");
	await p.filesPanel("open", f => f.names.includes("pic.svg"));
	await p.filesRow("pic.svg", "dblclick");
	await p.until("the image is given to the core", async () => (await p.count(".tau-files")) === 0);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("dropped on a view, given in the address", async p => {
	await p.open();
	await p.newDocument();
	const drop = files => p.page.evaluate(files => {
		const data = new DataTransfer();
		for (const [name, text] of files) data.items.add(new File([text], name));
		const canvas = document.querySelector(".tau-pane .tau-canvas"), r = canvas.getBoundingClientRect();
		canvas.dispatchEvent(new DragEvent("drop", { dataTransfer: data, bubbles: true, cancelable: true, clientX: r.left + r.width / 2, clientY: r.top + 120 }));
	}, files);
	const doc = "<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n  Dropped.\n</body>\n";
	// a document is kept and opened; a second one of the same name does not replace it
	await drop([["note.tm", doc]]);
	await p.until("the document is a tab", async () => (await p.tabs())[0].some(t => t.current && t.name === "note.tm"));
	await drop([["note.tm", doc]]);
	await p.until("the second one has a name of its own", async () => (await p.tabs())[0].some(t => t.current && t.name === "note 2.tm"));
	// an image is kept too, and inserted in the document
	await p.forget();
	await drop([["dot.svg", "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='30'><circle cx='20' cy='15' r='10'/></svg>"]]);
	await p.until("the image goes to the editor", async () => (await p.sent("drop")).some(m => m.paths.join() === "/home/tau/Documents/dot.svg"));
	await p.until("the document is changed", async () => (await p.tabs())[0].some(t => t.current && t.name === "note 2.tm •"));
	check("no tab for the image", !(await p.tabs())[0].some(t => t.name.startsWith("dot")));
	await p.page.evaluate(() => tau.files.browse());
	const f = await p.filesPanel("browse");
	check("what was dropped is among the documents", f.names.join() === "dot.svg,note 2.tm,note.tm", f);
	await p.key("Escape");
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
	// (saved, so that the page can be left without a question)
	await p.key("s", MOD);
	await p.until("the document is saved", async () => (await p.tabs())[0].some(t => t.current && t.name === "note 2.tm"));
	// the address of the page: a document of the web, and a file of TeXmacs
	await p.open("open=index.html&file=/texmacs/doc/about/welcome/new-welcome.en.tm");
	await p.until("the document of the web is a tab", async () => (await p.tabs())[0].some(t => t.name.startsWith("index.html")), 20000);
	await p.until("the file of TeXmacs is a tab", async () => (await p.tabs())[0].filter(t => /Welcome|new-welcome/.test(t.name)).length >= 1, 20000);
	await p.page.evaluate(() => tau.files.browse());
	const g = await p.filesPanel("browse");
	check("a document of the web is not among those of the user", g.names.length === 0, g);
	await p.key("Escape");
	await p.open("file=/etc/passwd");
	check("another path is refused", (await p.page.$$eval(".tau-notice", l => l.map(e => e.textContent).join())).includes("No document /etc/passwd"));
});

test("busy", async p => {
	await p.open();
	await p.newDocument();
	const shown = () => p.page.evaluate(() => ({ bar: !document.getElementById("tau-busy-bar").hidden, cursor: document.body.classList.contains("tau-busy"),
		word: !document.getElementById("tau-working").hidden, busy: tau.state.busy }));
	await sleep(700);
	let b = await shown();
	check("nothing is shown while the core answers", !b.bar && !b.cursor && !b.word, b);
	// the core works for three seconds
	await p.scheme("(with t (texmacs-time) (while (< (- (texmacs-time) t) 3000) (noop)))");
	await p.until("the bar and the cursor show that it works", async () => { const b = await shown(); return b.bar && b.cursor && b.busy; }, 2500);
	check("not yet the word in the footer", !(await shown()).word);
	// (what is typed meanwhile waits)
	await p.type("Late.");
	await p.until("after a while, the word in the footer", async () => (await shown()).word, 3000);
	await p.until("all of it goes when the core answers again", async () => { const b = await shown(); return !b.bar && !b.cursor && !b.word && !b.busy; }, 6000);
	await p.until("what was typed meanwhile is in the document", async () => (await p.tabs())[0].some(t => t.current && t.name.endsWith("•")));
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("the wheel for the editor", async p => {
	await p.open();
	await p.newDocument();
	// the page of the design of the fonts: a document which fits its view,
	// with a list in it which the wheel scrolls (the editor captures it)
	await p.scheme("(open-font-design)");
	const position = async () => {
		await p.scheme('(set-message (with t (buffer-tree) (if (> (tree-arity t) 2) (string-append "at " (tree->string (tree-ref (tree-ref t 2) 0))) "no list")) "")');
		await sleep(300);
		return p.page.$eval("#tau-footer-left", e => e.textContent);
	};
	await p.until("the page of the fonts, its list at the top", async () => (await position()).includes("at 100%"), 30000);
	const box = await p.page.$eval(".tau-pane .tau-canvas", c => { const r = c.getBoundingClientRect(); return { x: r.left + r.width / 2, y: r.top + r.height / 2 }; });
	await p.page.mouse.move(box.x, box.y);
	await p.forget();
	for (let i = 0; i < 5; i++) { await p.page.mouse.wheel({ deltaY: 120 }); await sleep(120); }
	await p.until("the wheel reaches the core", async () => (await p.sent("scroll")).length > 0);
	let at = "";
	await p.until("the list moved down, not the page", async () => { at = await position(); return /at \d/.test(at) && !at.includes("at 100%"); });
	const scrolled = parseFloat(at.replace(/.*at /, ""));
	check("down the list", scrolled < 100 && scrolled > 50, at);
	for (let i = 0; i < 8; i++) { await p.page.mouse.wheel({ deltaY: -120 }); await sleep(120); }
	await p.until("and back to its top", async () => (await position()).includes("at 100%"));
	// a fast swipe: the steps do not queue up for the core, which takes a
	// while for each; they are added up while it works, and it is not
	// shown as busy
	await p.forget();
	const swipe = await p.page.evaluate(async (x, y) => {
		const canvas = document.querySelector(".tau-pane .tau-canvas");
		let busy = false;
		for (let i = 0; i < 40; i++) {
			canvas.dispatchEvent(new WheelEvent("wheel", { deltaY: 12, clientX: x, clientY: y, bubbles: true, cancelable: true }));
			await new Promise(r => setTimeout(r, 10));
			busy = busy || tau.state.busy;
		}
		for (let i = 0; i < 15; i++) { await new Promise(r => setTimeout(r, 100)); busy = busy || tau.state.busy; }
		return { busy };
	}, box.x, box.y);
	const steps = (await p.sent("scroll")).length;
	check("forty steps of the wheel are fewer messages", steps >= 2 && steps <= 20, steps);
	check("the core is not shown as busy while the list scrolls", !swipe.busy);
	check("all of the swipe arrived", (await p.sent("scroll")).reduce((n, m) => n + m.dy, 0) === 40 * 12 * 2, await p.sent("scroll"));
	at = await position();
	check("the list followed", parseFloat(at.replace(/.*at /, "")) < 100, at);
	// The scroll bar of the list, with the cursor in the list: a canvas is
	// scrolled back to its cursor when the document changes, which undid
	// what the bar asked. A click on the bar goes there; a drag follows the
	// pointer, also when it leaves the narrow bar sideways, and stays.
	const r = await p.page.$eval(".tau-pane .tau-canvas", c => { const r = c.getBoundingClientRect(); return { x: r.left, y: r.top, w: r.width, h: r.height }; });
	const bar = Math.round(r.x + r.w - 22), top = r.y + 280, bottom = r.y + r.h - 20;
	const where = async () => parseFloat((await position()).replace(/.*at /, ""));
	await p.page.mouse.click(Math.round(r.x + 300), Math.round(top + 60));
	await sleep(500);
	await p.page.mouse.click(bar, Math.round(top + (bottom - top) * 0.3));
	await p.until("a click on the bar of the list goes there", async () => { const w = await where(); return w > 55 && w < 85; });
	await p.page.mouse.move(bar, Math.round(top + (bottom - top) * 0.3));
	await p.page.mouse.down();
	const seen = [];
	for (let i = 1; i <= 8; i++) {
		await p.page.mouse.move(bar + (i % 2 ? 12 : -30), Math.round(top + (bottom - top) * (0.3 + 0.06 * i)));
		await sleep(150);
		if (i % 2 === 0) seen.push(await where());
	}
	await p.page.mouse.up();
	await sleep(800);
	check("the list follows the drag of its bar", seen.every((w, i) => i === 0 || w < seen[i - 1] - 3) && seen[seen.length - 1] < 35, seen);
	await p.until("and stays where it was left", async () => Math.abs((await where()) - seen[seen.length - 1]) < 1);
	// past the end of the bar the drag goes to its end, and no further
	await p.page.mouse.move(bar, Math.round(top + (bottom - top) * 0.5));
	await p.page.mouse.down();
	await p.page.mouse.move(bar - 40, Math.round(top + (bottom - top) * 0.8));
	await sleep(150);
	await p.page.mouse.move(bar - 40, Math.round(r.y + r.h - 1));
	await sleep(300);
	await p.page.mouse.up();
	await p.until("a drag past the end of the bar goes to the end", async () => (await where()) < 1);
	// two clicks on the bar are the bar's: the cursor does not go under
	// it, which scrolled the list back
	await p.page.mouse.click(bar, Math.round(top + (bottom - top) * 0.5));
	await p.page.mouse.click(bar, Math.round(top + (bottom - top) * 0.5));
	await p.until("a double click on the bar goes there", async () => { const w = await where(); return w > 35 && w < 65; });
	const twice = await where();
	await sleep(800);
	check("and the list stays there", Math.abs((await where()) - twice) < 1, [twice, await where()]);
	// an ordinary document still scrolls as a whole
	await p.page.click(".tau-doc-tab");
	await sleep(500);
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("kept in the browser", async p => {
	await p.open("home");
	check("this page keeps the home directory", (await p.count(".tau-notice")) === 0);
	await p.newDocument();
	await p.type("To be kept.");
	await p.choose("File", "Save as");
	await p.filesPanel("save");
	const name = "kept-" + Date.now() + ".tm";
	await p.filesName(name);
	await p.key("Enter");
	await p.until("the document has its name", async () => (await p.tabs())[0].some(t => t.current && t.name === name));
	await sleep(1500); // (written to the browser a moment later)
	check("it stays saved", (await p.tabs())[0].some(t => t.current && t.name === name), await p.tabs());
	// the page again: the document is there
	await p.open("home");
	await p.choose("File", "Load");
	let f = await p.filesPanel("open");
	check("the document is offered after the page was loaded again", f.names.includes(name), f);
	await p.filesRow(name);
	await p.filesPanel("open", f => f.names.includes(name));
	await p.page.click('.tau-files [data-what="ok"]');
	await p.until("and opens", async () => (await p.tabs())[0].some(t => t.current && t.name === name));
	// a backup of what is kept, the document deleted, the backup restored:
	// Tau starts again with the document
	await p.scheme("(tau-files)");
	await p.filesPanel("browse", f => f.names.includes(name));
	await p.page.evaluate(async () => {
		const b = await tau.files.makeBackup();
		window.backup = new Uint8Array(await b.blob.arrayBuffer());
		window.backupFiles = b.files;
	});
	check("the backup has files", (await p.page.evaluate(() => window.backupFiles)) >= 1);
	// a backup which does not fit what is there (a folder where a document
	// is) is refused before anything is written, and Tau goes on
	await p.page.evaluate(async name => {
		const enc = new TextEncoder();
		const note = { rel: "tau-backup.json", bytes: enc.encode(JSON.stringify({ format: 1, application: "Tau", files: 2 })) };
		const blob = await tau.files.zip([note, { rel: "home/Documents/first.txt", bytes: enc.encode("1") },
			{ rel: "home/Documents/" + name + "/inside.txt", bytes: enc.encode("2") }]);
		window.refused = "";
		tau.files.restoreBackup(new Uint8Array(await blob.arrayBuffer())).catch(error => { window.refused = error.message + (error.restart ? " (restart)" : ""); });
	}, name);
	await p.filesPanel("browse", f => f.asking.startsWith("Restore the backup"));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.until("the backup is refused", () => p.page.evaluate(() => window.refused !== ""));
	check("why it is refused", (await p.page.evaluate(() => window.refused)).includes("is a file here and a folder in the backup"), await p.page.evaluate(() => window.refused));
	await p.page.evaluate(() => tau.files.browse());
	f = await p.filesPanel("browse", f => f.names.includes(name));
	check("nothing of it was written", !f.names.includes("first.txt"), f);
	await p.filesRow(name, "delete");
	await p.filesPanel("browse", f => f.asking.startsWith("Delete " + name));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.filesPanel("browse", f => !f.asking && !f.names.includes(name));
	await p.page.evaluate(() => { window.before = true; tau.files.restoreBackup(window.backup); });
	await p.filesPanel("browse", f => f.asking.startsWith("Restore the backup"));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.until("the page starts again", () => p.page.evaluate(() => !window.before && !!(window.tau && tau.state.started)), 90000);
	await p.until("the bars come", () => p.count("#tau-menu .tau-entry").then(n => n >= 8), 30000);
	await p.page.evaluate(() => tau.files.browse());
	f = await p.filesPanel("browse");
	check("the document is back", f.names.includes(name), f);
	// an exact copy of a backup: what the backup has not goes
	await p.page.evaluate(async () => { window.backup = new Uint8Array(await (await tau.files.makeBackup()).blob.arrayBuffer()); });
	await p.filesDrop([["extra.txt", "not in the backup"]]);
	await p.filesPanel("browse", f => f.names.includes("extra.txt"));
	await p.key("Escape");
	await p.until("the panel is closed", async () => (await p.count(".tau-files")) === 0);
	await p.page.evaluate(() => { window.before = true; tau.files.restoreBackup(window.backup); });
	f = await p.filesPanel("browse", f => f.asking.startsWith("Restore the backup"));
	check("the two ways to restore", f.asking.includes("Replace everything"), f);
	await p.page.click('.tau-files-ask [data-what="other"]');
	await p.until("the page starts again", () => p.page.evaluate(() => !window.before && !!(window.tau && tau.state.started)), 90000);
	await p.until("the bars come", () => p.count("#tau-menu .tau-entry").then(n => n >= 8), 30000);
	await p.page.evaluate(() => tau.files.browse());
	f = await p.filesPanel("browse");
	check("the document is there, and what the backup had not is gone", f.names.includes(name) && !f.names.includes("extra.txt"), f);
	// the menu of the icon has the backup
	await p.key("Escape");
	await p.page.click("#tau-app");
	check("the backup in the menu of Tau", (await p.page.$$eval(".tau-app-menu .tau-entry", l => l.map(e => e.textContent).join("|"))).includes("Save a backup|Restore a backup"));
	await p.find(".tau-app-menu .tau-entry", "Save a backup").then(h => h.click());
	await p.until("a backup is given to the user", () => p.page.evaluate(() => tau.state.downloads.some(d => /^tau-backup-\d{4}-\d\d-\d\d\.zip$/.test(d.name) && d.size > 200)));
	await p.page.evaluate(() => tau.files.browse());
	f = await p.filesPanel("browse");
	await p.filesRow(name, "delete");
	await p.filesPanel("browse", f => f.asking.startsWith("Delete " + name));
	await p.page.click(".tau-files-ask .tau-files-yes");
	await p.filesPanel("browse", f => !f.names.includes(name));
	await p.key("Escape");
	await sleep(1500);
	// TeXmacs is closed: it starts again
	await p.page.evaluate(() => { window.before = true; });
	await p.scheme("(quit-TeXmacs)");
	await p.until("the page starts again", () => p.page.evaluate(() => !window.before && !!(window.tau && tau.state.started)), 90000);
});

test("preview, full screen, tooltip", async p => {
	await p.open();
	await p.newDocument();
	await p.type("A page to look at.");
	// Preview: the PDF goes to the page, which opens it
	await p.forget();
	await p.choose("File", "Preview");
	await p.until("the PDF is given to the page", async () => (await p.got("open-pdf")).length === 1, 30000);
	check("under the name of the document", /\.pdf$/.test((await p.got("open-pdf"))[0].name), await p.got("open-pdf"));
	check("no dialog of warnings", (await p.dialogs()).length === 0, await p.dialogs());
	// the full screen mode of TeXmacs: the views alone
	await p.scheme("(toggle-full-screen-mode)");
	await p.until("the bars and the tabs go", () => p.page.evaluate(() =>
		document.body.classList.contains("tau-fullscreen") && document.body.classList.contains("tau-no-header")));
	check("the menu bar is not shown", (await p.box("#tau-menu")).h === 0);
	await p.scheme("(toggle-full-screen-mode)");
	await p.until("and come back", () => p.page.evaluate(() =>
		!document.body.classList.contains("tau-fullscreen") && !document.body.classList.contains("tau-no-header")));
	// a tooltip of the document: a small view over it
	await p.scheme('(show-tooltip "test-tip" (cursor-tree) (quote (document "A tip.")) "auto" "auto" "keyboard" 0.8)');
	await p.until("the tooltip is shown", async () => (await p.count(".tau-tooltip canvas")) === 1);
	const tip = await p.box(".tau-tooltip"), view = await p.box(".tau-canvas");
	check("over the view", tip.x >= view.x && tip.y >= view.y && tip.w > 20 && tip.h > 10, [tip, view]);
	await p.until("and drawn", () => p.page.evaluate(() => {
		const c = document.querySelector(".tau-tooltip canvas");
		if (!c || c.width < 10) return false;
		const data = c.getContext("2d").getImageData(0, 0, c.width, c.height).data;
		let dark = 0;
		for (let i = 0; i < data.length; i += 4) if (data[i + 3] > 200 && data[i] < 100) dark++;
		return dark > 20;
	}));
	check("no error of the core", (await p.errors()).length === 0, await p.errors());
});

test("stopped", async p => {
	await p.open();
	await p.newDocument();
	await p.type("x");
	// what the worker says when the program cannot go on
	await p.page.evaluate(() => tau.worker.onmessage({ data: { t: "stopped", text: "Maximum call stack size exceeded." } }));
	await p.until("the page says that Tau stopped", async () => (await p.count(".tau-failure")) === 1);
	check("with a way to start again", !!(await p.find(".tau-failure .tau-button", "Start Tau again")));
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
