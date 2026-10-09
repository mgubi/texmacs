// The page of Tau (docs/tau-design.md). The core runs in a worker
// (tau-worker.js). The page shows its views: each window of the core is a
// pane here, with the documents as tabs and a canvas for the view which
// the window shows. This module tells the core the places of the views,
// the keys and the pointer, draws the pixels it sends, and looks after
// what belongs to the browser: the clipboard and the files of the user.

import * as chrome from "./chrome.mjs";

const panesElement = document.getElementById("tau-panes");
const statusLine = document.getElementById("status");
const params = new URLSearchParams(location.search);
const verbose = params.has("log");

// ?arg=... are the arguments of TeXmacs (a document to open, say)
const worker = new Worker("tau-worker.js" + location.search);
const send = message => worker.postMessage(message);

const panes = new Map();  // number of a window of the core -> pane
let active = null;        // the pane which has the keyboard, or had it last
let buffers = [];         // the documents: { name, title, modified }

// for tests and for the console
const state = { started: false, paints: 0, get view() { return active ? active.view : 0; } };
window.tau = { state, send, worker, panes, get active() { return active; } };
chrome.init({ send, afterAction: () => active && active.canvas.focus() });

function setStatus(text) { statusLine.textContent = text; }

function el(tag, className, text) {
	const e = document.createElement(tag);
	if (className) e.className = className;
	if (text !== undefined) e.textContent = text;
	return e;
}

// Keys, in the notation of TeXmacs: "a", "return", "S-left", "C-x"...
const KEYS = {
	Enter: "return", Backspace: "backspace", Delete: "delete", Tab: "tab", Escape: "escape",
	ArrowLeft: "left", ArrowRight: "right", ArrowUp: "up", ArrowDown: "down",
	Home: "home", End: "end", PageUp: "pageup", PageDown: "pagedown", Insert: "insert",
	" ": "space"
};
for (let i = 1; i <= 12; i++) KEYS["F" + i] = "F" + i;
const isMac = /Mac|iPhone|iPad/.test(navigator.platform);

function keyName(event) {
	if (["Shift", "Control", "Alt", "Meta", "CapsLock", "Dead", "Process"].includes(event.key)) return null;
	let key = KEYS[event.key];
	const special = key !== undefined;
	if (!special) {
		if (event.key.length !== 1 && [...event.key].length !== 1) return null;
		key = event.key;
	}
	const command = event.ctrlKey || event.metaKey || (event.altKey && !isMac);
	// a character typed with Shift (or Option on a Mac) is the character
	// itself; the modifiers which make a command are prefixes
	if (!special && !command) return key;
	if (!special && command) key = key.toLowerCase();
	if (event.shiftKey) key = "S-" + key;
	if (event.ctrlKey) key = "C-" + key;
	if (event.altKey) key = "A-" + key;
	if (event.metaKey) key = "M-" + key;
	return key;
}

// the keys which are left to the browser
function browserKey(event) {
	const mod = isMac ? event.metaKey : event.ctrlKey;
	return mod && ["r", "l", "t", "w", "n", "q"].includes(event.key.toLowerCase()) ||
		event.key === "F5" || event.key === "F11" || event.key === "F12";
}

// The pointer: positions in pixels of the canvas; the modifiers as TeXmacs
// counts them (buttons 1, 2, 4; Shift 256, Control 1024, Alt 2048, Meta 4096)
const BUTTONS = ["left", "middle", "right"];
function mods(event) {
	return (event.buttons & 1 ? 1 : 0) | (event.buttons & 4 ? 2 : 0) | (event.buttons & 2 ? 4 : 0) |
		(event.shiftKey ? 256 : 0) | (event.ctrlKey ? 1024 : 0) |
		(event.altKey ? 2048 : 0) | (event.metaKey ? 4096 : 0);
}

// the key which pastes: it is left to the browser, which then gives its
// clipboard in a "paste" event
function pasteKey(event) {
	return (isMac ? event.metaKey && !event.ctrlKey : event.ctrlKey && !event.metaKey) &&
		!event.altKey && !event.shiftKey && event.key.toLowerCase() === "v";
}

// ---------------------------------------------------------------------------
// A pane: a window of the core, its tabs and the canvas of its view
// ---------------------------------------------------------------------------

function makePane(windowNumber) {
	const pane = {
		window: windowNumber,
		view: 0,       // the number of the view which is shown
		place: 0,      // the number of the last place sent
		buffer: "",    // the name of the document which is shown
		element: el("div", "tau-pane"),
		tabs: el("div", "tau-doc-tabs"),
		canvas: el("canvas", "tau-canvas"),
		extents: { width: 0, height: 0 }, scroll: { x: 0, y: 0 }
	};
	const canvas = pane.canvas, holder = el("div", "tau-view");
	pane.context = canvas.getContext("2d");
	canvas.tabIndex = 0;
	holder.append(canvas);
	pane.element.append(pane.tabs, holder);
	panesElement.append(pane.element);
	panes.set(windowNumber, pane);

	// The place: the size of the canvas in pixels of the screen. Each place
	// has a number, which the core repeats with what it draws for it.
	pane.sendPlace = () => {
		if (!pane.view) return;
		const density = window.devicePixelRatio || 1;
		const width = Math.max(1, Math.round(canvas.clientWidth * density));
		const height = Math.max(1, Math.round(canvas.clientHeight * density));
		pane.place++;
		send({ t: "place", view: pane.view, width, height, density, place: pane.place });
	};
	pane.observer = new ResizeObserver(pane.sendPlace);
	pane.observer.observe(canvas);

	canvas.addEventListener("keydown", event => {
		if (!pane.view || event.isComposing || browserKey(event) || pasteKey(event)) return;
		const key = keyName(event);
		if (!key) return;
		event.preventDefault();
		send({ t: "key", view: pane.view, key });
	});
	canvas.addEventListener("focus", () => {
		activate(pane);
		if (pane.view) send({ t: "focus", view: pane.view, focus: true });
	});
	canvas.addEventListener("blur", () => pane.view && send({ t: "focus", view: pane.view, focus: false }));

	const sendMouse = (kind, event) => {
		if (!pane.view) return;
		const rect = canvas.getBoundingClientRect(), density = window.devicePixelRatio || 1;
		send({
			t: "mouse", view: pane.view, kind,
			x: Math.round((event.clientX - rect.left) * density),
			y: Math.round((event.clientY - rect.top) * density),
			mods: mods(event)
		});
	};
	canvas.addEventListener("pointerdown", event => {
		canvas.focus();
		canvas.setPointerCapture(event.pointerId);
		sendMouse("press-" + (BUTTONS[event.button] || "left"), event);
		event.preventDefault();
	});
	canvas.addEventListener("pointerup", event => {
		sendMouse("release-" + (BUTTONS[event.button] || "left"), event);
	});
	// the moves which have not been sent yet are merged: one for each frame
	let pendingMove = null;
	canvas.addEventListener("pointermove", event => {
		if (!pendingMove) requestAnimationFrame(() => { sendMouse("move", pendingMove); pendingMove = null; });
		pendingMove = event;
	});
	canvas.addEventListener("contextmenu", event => event.preventDefault());

	// The wheel scrolls the view; the steps which have not been sent yet
	// are added up, one message for each frame
	let pendingScroll = null;
	canvas.addEventListener("wheel", event => {
		event.preventDefault();
		if (!pane.view) return;
		const density = window.devicePixelRatio || 1;
		const unit = event.deltaMode === 1 ? 32 : event.deltaMode === 2 ? canvas.clientHeight : 1;
		if (!pendingScroll) {
			pendingScroll = { dx: 0, dy: 0 };
			requestAnimationFrame(() => {
				send({ t: "scroll", view: pane.view,
					dx: Math.round(pendingScroll.dx), dy: Math.round(pendingScroll.dy) });
				pendingScroll = null;
			});
		}
		pendingScroll.dx += event.deltaX * unit * density;
		pendingScroll.dy += event.deltaY * unit * density;
	}, { passive: false });

	// a file which is dropped on a pane is opened there
	canvas.addEventListener("dragover", event => { event.preventDefault(); event.dataTransfer.dropEffect = "copy"; });
	canvas.addEventListener("drop", event => {
		event.preventDefault();
		canvas.focus();
		for (const file of event.dataTransfer.files) openFile(file, 0);
	});
	return pane;
}

function activate(pane) {
	if (active === pane) return;
	if (active) active.element.classList.remove("tau-active");
	active = pane;
	pane.element.classList.add("tau-active");
	showTitle();
}

function removePane(pane) {
	pane.observer.disconnect();
	pane.element.remove();
	panes.delete(pane.window);
	if (active === pane) {
		active = null;
		const next = panes.values().next().value;
		if (next) next.canvas.focus();
	}
}

function titleOf(name) {
	const b = buffers.find(b => b.name === name);
	return b ? b.title : name.replace(/^.*\//, "");
}

function showTitle() {
	document.title = active && active.buffer ? titleOf(active.buffer) + " – Tau" : "Tau";
}

// The tabs of a pane: the documents, the one which the pane shows marked
function showTabs(pane) {
	const list = buffers.slice();
	if (pane.buffer && !list.some(b => b.name === pane.buffer))
		list.push({ name: pane.buffer, title: titleOf(pane.buffer), modified: false });
	const ask = (what, name) => send({ t: "buffer", what, window: pane.window, name: name || "" });
	const tabs = list.map(b => {
		const tab = el("div", "tau-doc-tab" + (b.name === pane.buffer ? " tau-current" : ""));
		tab.title = b.name;
		const close = el("button", "tau-doc-close", "×");
		close.type = "button";
		close.title = "Close the document";
		close.addEventListener("click", event => { event.stopPropagation(); ask("close", b.name); });
		tab.append(el("span", "tau-doc-name", b.title + (b.modified ? " •" : "")), close);
		tab.addEventListener("click", () => { if (b.name !== pane.buffer) ask("switch", b.name); pane.canvas.focus(); });
		return tab;
	});
	const add = el("button", "tau-doc-button", "+");
	add.type = "button";
	add.title = "New document";
	add.addEventListener("click", () => { ask("new"); pane.canvas.focus(); });
	pane.tabs.replaceChildren(...tabs, add);
	if (panes.size > 1) {
		const close = el("button", "tau-doc-button tau-pane-close", "×");
		close.type = "button";
		close.title = "Close this pane";
		close.addEventListener("click", () => ask("close-window"));
		pane.tabs.append(el("span", "tau-glue"), close);
	}
}

// ---------------------------------------------------------------------------
// The files of the user and the clipboard
// ---------------------------------------------------------------------------

// a file of the user goes to the core: for a question of the core (ticket)
// or, dropped, to be opened
async function openFile(file, ticket) {
	const bytes = await file.arrayBuffer();
	worker.postMessage({ t: "open", ticket, name: file.name, bytes }, [bytes]);
}

const ACCEPT = { image: "image/*,.pdf,.eps,.ps,.svg", texmacs: ".tm,.ts,.tp,.tmml,.stm" };

function pickFile(m) {
	const input = el("input");
	input.type = "file";
	if (ACCEPT[m.type]) input.accept = ACCEPT[m.type];
	input.addEventListener("change", () => {
		if (input.files.length) openFile(input.files[0], m.ticket);
		if (active) active.canvas.focus();
	});
	// The browser opens its file chooser only on an action of the user.
	// A click or a key did ask for it, a moment ago; when the browser does
	// not count that any more, a button asks again.
	const allowed = !navigator.userActivation || navigator.userActivation.isActive;
	if (allowed) { input.click(); return; }
	const box = el("div", "tau-ask"), button = el("button", "tau-button", "Choose a file…");
	const cancel = el("button", "tau-button", "Cancel");
	box.append(el("span", "", m.title || "Load file"), button, cancel);
	button.addEventListener("click", () => { input.click(); box.remove(); });
	cancel.addEventListener("click", () => box.remove());
	document.body.append(box);
	button.focus();
}

function download(m) {
	if (!m.bytes) return;
	const url = URL.createObjectURL(new Blob([m.bytes]));
	const a = el("a");
	a.href = url;
	a.download = m.name;
	document.body.append(a);
	a.click();
	a.remove();
	setTimeout(() => URL.revokeObjectURL(url), 10000);
}

// what is pasted in a view: the text and the HTML of the clipboard
document.addEventListener("paste", event => {
	const pane = active;
	if (!pane || document.activeElement !== pane.canvas || !pane.view) return;
	event.preventDefault();
	const data = event.clipboardData;
	const files = Array.from(data.files || []);
	if (files.length) { for (const file of files) openFile(file, 0); return; }
	const text = data.getData("text/plain"), html = data.getData("text/html");
	if (text || html) send({ t: "paste", view: pane.view, text, html });
});

function copy(text) {
	const done = navigator.clipboard && navigator.clipboard.writeText
		? navigator.clipboard.writeText(text) : Promise.reject(new Error("no clipboard"));
	done.catch(error => { if (verbose) console.log("clipboard: " + error.message); });
}

// ---------------------------------------------------------------------------
// The messages of the core
// ---------------------------------------------------------------------------

worker.onmessage = event => {
	const m = event.data;
	if (chrome.handle(m)) return;
	switch (m.t) {
	case "status":
		if (!state.started) setStatus(m.text);
		break;
	case "log":
		if (verbose) console.log(m.text);
		break;
	case "failed":
		setStatus("Tau could not start: " + m.text);
		break;
	case "ready":
		if (!state.started) setStatus("Tau is ready, no view yet");
		break;
	case "view": {
		// the view which a window shows: tell it its place, and give it the
		// keyboard if its pane has it
		// (a new pane takes the keyboard)
		const fresh = !panes.has(m.window);
		const pane = panes.get(m.window) || makePane(m.window);
		pane.view = m.view;
		pane.sendPlace();
		if (fresh || !active) pane.canvas.focus();
		send({ t: "focus", view: pane.view, focus: document.activeElement === pane.canvas });
		break;
	}
	case "buffers": {
		// the tabs keep their places: the documents in the order in which
		// the page came to know them
		const known = buffers.map(b => b.name);
		const rank = b => { const i = known.indexOf(b.name); return i < 0 ? known.length : i; };
		buffers = m.buffers.map((b, i) => [b, i]).sort((x, y) => rank(x[0]) - rank(y[0]) || y[1] - x[1]).map(x => x[0]);
		const shown = new Set(m.windows.map(w => w.window));
		for (const pane of Array.from(panes.values())) if (!shown.has(pane.window)) removePane(pane);
		let made = null;
		for (const w of m.windows) {
			let pane = panes.get(w.window);
			if (!pane) made = pane = makePane(w.window);
			pane.buffer = w.buffer;
		}
		for (const pane of panes.values()) showTabs(pane);
		if (made) made.canvas.focus(); // a new pane takes the keyboard
		showTitle();
		break;
	}
	case "paint": {
		let pane = null;
		for (const p of panes.values()) if (p.view === m.view) pane = p;
		if (!pane || m.place !== pane.place) break; // for an older place
		const canvas = pane.canvas;
		if (canvas.width !== m.width || canvas.height !== m.height) {
			canvas.width = m.width; canvas.height = m.height;
		}
		pane.context.putImageData(
			new ImageData(new Uint8ClampedArray(m.pixels), m.width, m.height), 0, 0);
		pane.extents = m.extents; pane.scroll = m.scroll; pane.caret = m.caret;
		state.paints++;
		state.started = true;
		if (params.has("debug"))
			setStatus(`view ${m.view} · ${m.width}×${m.height} · document ${m.extents.width}×${m.extents.height}` +
				` · scroll ${m.scroll.x},${m.scroll.y} · ${state.paints} paints`);
		else if (statusLine.textContent) setStatus("");
		break;
	}
	case "clipboard": copy(m.text); break;
	case "pick": pickFile(m); break;
	case "download": download(m); break;
	}
};
worker.onerror = event => setStatus("The worker failed: " + (event.message || ""));
