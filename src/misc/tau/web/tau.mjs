// The page of Tau (docs/tau-design.md). The core runs in a worker
// (tau-worker.js). The page shows its views: each window of the core is a
// pane here, with the documents as tabs and a canvas for the view which
// the window shows. This module tells the core the places of the views,
// the keys and the pointer, draws the pixels it sends, and looks after
// what belongs to the browser: the clipboard and the files of the user.

import * as chrome from "./chrome.mjs";
import { translate, makeLayout, learn, askLayout, browserKey, pasteKey } from "./keys.mjs";

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
chrome.init({ send, afterAction: () => active && focusView(active), makeView: node => makeView(node), keyName: event => keyName(event) });

function setStatus(text) { statusLine.textContent = text; }

function el(tag, className, text) {
	const e = document.createElement(tag);
	if (className) e.className = className;
	if (text !== undefined) e.textContent = text;
	return e;
}

const isMac = /Mac|iPhone|iPad/.test(navigator.platform);
const traceKeys = params.has("trace-keys");

// The pointer: positions in pixels of the canvas; the modifiers as TeXmacs
// counts them (buttons 1, 2, 4; Shift 256, Control 1024, Alt 2048, Meta 4096)
const BUTTONS = ["left", "middle", "right"];
function mods(event) {
	return (event.buttons & 1 ? 1 : 0) | (event.buttons & 4 ? 2 : 0) | (event.buttons & 2 ? 4 : 0) |
		(event.shiftKey ? 256 : 0) | (event.ctrlKey ? 1024 : 0) |
		(event.altKey ? 2048 : 0) | (event.metaKey ? 4096 : 0);
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
	const holder = el("div", "tau-view");
	holder.append(pane.canvas);
	pane.element.append(pane.tabs, holder);
	panesElement.append(pane.element);
	panes.set(windowNumber, pane);
	layoutPanes();
	attachView(pane);
	// a file which is dropped on a pane is opened there
	pane.canvas.addEventListener("dragover", event => { event.preventDefault(); event.dataTransfer.dropEffect = "copy"; });
	pane.canvas.addEventListener("drop", event => {
		event.preventDefault();
		focusView(pane);
		for (const file of event.dataTransfer.files) openFile(file, 0);
	});
	return pane;
}

// A view in a dialog or in a tool (chrome.mjs): a canvas of the size which
// the core wishes, or of its container. It is a view as that of a pane;
// one which only shows a document does not take the keyboard.
const embedded = new Map(); // number of a view -> what shows it

function makeView(node) {
	const target = { view: node.view, place: 0, canvas: el("canvas", "tau-embedded"), passive: !node.input };
	if (node.width > 0 && node.height > 0) {
		target.canvas.style.width = node.width + "px";
		target.canvas.style.height = node.height + "px";
	} else target.canvas.classList.add("tau-fill");
	for (const [view, t] of embedded) if (!t.canvas.isConnected && t.seen) { t.observer.disconnect(); embedded.delete(view); }
	embedded.set(node.view, target);
	attachView(target);
	return target.canvas;
}

function targetOf(view) {
	for (const pane of panes.values()) if (pane.view === view) return pane;
	return embedded.get(view) || null;
}

// What a canvas which shows a view does: it tells the core its place, the
// keys and the pointer. target has the canvas and the number of its view.
function attachView(target) {
	const canvas = target.canvas;
	target.context = canvas.getContext("2d");

	// The place: the size of the canvas in pixels of the screen. Each place
	// has a number, which the core repeats with what it draws for it.
	target.sendPlace = () => {
		if (!target.view || !canvas.isConnected) return;
		target.seen = true;
		const density = window.devicePixelRatio || 1;
		const width = Math.max(1, Math.round(canvas.clientWidth * density));
		const height = Math.max(1, Math.round(canvas.clientHeight * density));
		target.place++;
		send({ t: "place", view: target.view, width, height, density, place: target.place });
	};
	target.observer = new ResizeObserver(target.sendPlace);
	target.observer.observe(canvas);
	if (target.passive) return;
	canvas.classList.add("tau-editable");

	const sendMouse = (kind, event) => {
		if (!target.view) return;
		const rect = canvas.getBoundingClientRect(), density = window.devicePixelRatio || 1;
		send({
			t: "mouse", view: target.view, kind,
			x: Math.round((event.clientX - rect.left) * density),
			y: Math.round((event.clientY - rect.top) * density),
			mods: mods(event)
		});
	};
	// (a click on the canvas would take the keyboard from its text area)
	canvas.addEventListener("mousedown", event => event.preventDefault());
	canvas.addEventListener("pointerdown", event => {
		focusView(target);
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
		if (!target.view) return;
		const density = window.devicePixelRatio || 1;
		const unit = event.deltaMode === 1 ? 32 : event.deltaMode === 2 ? canvas.clientHeight : 1;
		if (!pendingScroll) {
			pendingScroll = { dx: 0, dy: 0 };
			requestAnimationFrame(() => {
				send({ t: "scroll", view: target.view,
					dx: Math.round(pendingScroll.dx), dy: Math.round(pendingScroll.dy) });
				pendingScroll = null;
			});
		}
		pendingScroll.dx += event.deltaX * unit * density;
		pendingScroll.dy += event.deltaY * unit * density;
	}, { passive: false });
}

// The panes share the width; the line between two of them is dragged to
// give more of it to one. Their shares are kept as they are when a pane
// comes or goes.
function layoutPanes() {
	for (const d of panesElement.querySelectorAll(".tau-divider")) d.remove();
	const list = Array.from(panesElement.querySelectorAll(".tau-pane"));
	list.slice(1).forEach((right, i) => {
		const left = list[i], divider = el("div", "tau-divider");
		panesElement.insertBefore(divider, right);
		divider.addEventListener("pointerdown", event => {
			event.preventDefault();
			divider.setPointerCapture(event.pointerId);
			divider.classList.add("tau-dragging");
			const x0 = event.clientX, w1 = left.offsetWidth, w2 = right.offsetWidth;
			const g1 = Number(left.style.flexGrow || 1), g2 = Number(right.style.flexGrow || 1);
			const move = e => {
				const d = Math.max(80 - w1, Math.min(w2 - 80, e.clientX - x0));
				left.style.flexGrow = (g1 + g2) * (w1 + d) / (w1 + w2);
				right.style.flexGrow = (g1 + g2) * (w2 - d) / (w1 + w2);
			};
			const up = () => {
				divider.classList.remove("tau-dragging");
				divider.removeEventListener("pointermove", move);
				divider.removeEventListener("pointerup", up);
			};
			divider.addEventListener("pointermove", move);
			divider.addEventListener("pointerup", up);
		});
		// a double click gives them the same width again
		divider.addEventListener("dblclick", () => {
			const g = (Number(left.style.flexGrow || 1) + Number(right.style.flexGrow || 1)) / 2;
			left.style.flexGrow = right.style.flexGrow = g;
		});
	});
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
	layoutPanes();
	if (active === pane) {
		active = null;
		const next = panes.values().next().value;
		if (next) focusView(next);
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
		tab.addEventListener("click", () => { if (b.name !== pane.buffer) ask("switch", b.name); focusView(pane); });
		return tab;
	});
	const add = el("button", "tau-doc-button", "+");
	add.type = "button";
	add.title = "New document";
	add.addEventListener("click", () => { ask("new"); focusView(pane); });
	pane.tabs.replaceChildren(...tabs, add);
	if (panes.size > 1) {
		const close = el("button", "tau-doc-button tau-pane-close", "×");
		close.type = "button";
		close.title = "Close this pane";
		close.addEventListener("click", () => ask("close-window"));
		pane.tabs.append(el("span", "tau-glue"), close);
	}
	chrome.fit(pane.tabs);
}

// ---------------------------------------------------------------------------
// The keyboard
// ---------------------------------------------------------------------------
//
// A canvas cannot have the text of the keyboard: the browser composes (dead
// keys, the accents of a Mac, the input methods of Chinese, Japanese...)
// and pastes in an element which is edited only. So the keyboard of the
// views is one text area which is not seen, put where the cursor of the
// view is (the system shows the candidates of an input method there). The
// view which has the keyboard is the one this text area writes for.
//
// A key press is a key or text (keys.mjs): a key is sent by its name of
// TeXmacs, text is what comes into the text area, sent as it is. What is
// being composed is shown in the document (the "pre-edit" of TeXmacs).

const layout = makeLayout();
askLayout(layout);
let focused = null;   // the pane or the view in a dialog which has the keyboard
let composing = false;

const area = el("textarea", "tau-keyboard");
area.setAttribute("aria-hidden", "true");
area.setAttribute("autocomplete", "off");
area.setAttribute("autocorrect", "off");
area.setAttribute("autocapitalize", "off");
area.spellcheck = false;
area.tabIndex = -1;
document.body.append(area);

function trace(...args) { if (traceKeys) console.log("keys:", ...args); }

// the text area at the cursor of the view which has the keyboard
function placeArea() {
	const t = focused;
	if (!t || !t.canvas.isConnected) return;
	const rect = t.canvas.getBoundingClientRect(), density = window.devicePixelRatio || 1;
	const c = t.caret || { x: 0, y: 0 };
	area.style.left = Math.max(0, Math.min(window.innerWidth - 4, rect.left + c.x / density)) + "px";
	area.style.top = Math.max(0, Math.min(window.innerHeight - 20, rect.top + c.y / density - 16)) + "px";
}

// give the keyboard to a view
function focusView(target) {
	if (!target || target.passive) return;
	if (focused !== target) {
		if (focused && focused.view && document.activeElement === area)
			send({ t: "focus", view: focused.view, focus: false });
		focused = target;
		if (target.view && document.activeElement === area) send({ t: "focus", view: target.view, focus: true });
	}
	if (panes.get(target.window) === target) activate(target);
	placeArea();
	if (document.activeElement !== area) area.focus({ preventScroll: true });
}

area.addEventListener("focus", () => focused && focused.view && send({ t: "focus", view: focused.view, focus: true }));
area.addEventListener("blur", () => focused && focused.view && send({ t: "focus", view: focused.view, focus: false }));

function sendKey(key) {
	if (focused && focused.view) send({ t: "key", view: focused.view, key });
}
function sendText(text) {
	if (text && focused && focused.view) send({ t: "text", view: focused.view, text });
}

function keyEvent(event) {
	return { key: event.key, code: event.code, shiftKey: event.shiftKey, ctrlKey: event.ctrlKey,
		altKey: event.altKey, metaKey: event.metaKey,
		altGraph: !!(event.getModifierState && event.getModifierState("AltGraph")) };
}

// the name of a key for an input of the page which wants the keys (the
// search bar, chrome.mjs): its name, the character of a key which types
function keyName(event) {
	const r = translate(keyEvent(event), isMac, layout);
	return !r ? null : r.key ? r.key : [...event.key].length === 1 ? event.key : null;
}

// a key press in the text area: true when it was taken
function handleKey(event) {
	if (!focused || !focused.view) return false;
	// the keys of a composition are the input method's
	if (composing || event.isComposing || event.keyCode === 229) return false;
	const e = keyEvent(event);
	if (browserKey(e, isMac) || pasteKey(e, isMac)) { trace(event.key, event.code, "left to the browser"); return false; }
	learn(layout, e);
	const r = translate(e, isMac, layout);
	trace(event.key, event.code, (e.shiftKey ? "s" : "") + (e.ctrlKey ? "c" : "") + (e.altKey ? "a" : "") + (e.metaKey ? "m" : ""),
		"->", r ? r.key || "text" : "nothing");
	if (!r || r.text) return false;
	event.preventDefault();
	sendKey(r.key);
	return true;
}

area.addEventListener("keydown", event => { handleKey(event); event.stopPropagation(); });

// the text which the keyboard made
area.addEventListener("input", event => {
	if (composing || event.isComposing) return;
	const text = area.value;
	area.value = "";
	trace("text", JSON.stringify(text));
	sendText(text);
});
area.addEventListener("compositionstart", () => { composing = true; placeArea(); });
area.addEventListener("compositionupdate", event => {
	const text = event.data || "";
	trace("composing", JSON.stringify(text));
	// what is being composed, with the cursor at its end
	sendKey(text ? "pre-edit:" + [...text].length + ":" + text : "pre-edit:");
});
area.addEventListener("compositionend", event => {
	composing = false;
	const text = event.data || "";
	area.value = "";
	trace("composed", JSON.stringify(text));
	sendKey("pre-edit:");
	sendText(text);
});

// The keys which are pressed while nothing of the page has the keyboard
// (after a click on a tab or on the background) are for the view of the
// active pane, which takes the keyboard back. The keys which the browser
// would act on (it zooms the page on Cmd with + or -) are not left to it
// wherever the keyboard is, save in the inputs of the page, where the keys
// of editing are theirs.
const EDITING = ["a", "c", "v", "x", "z", "y"];
document.addEventListener("keydown", event => {
	if (event.defaultPrevented || event.target === area || !active || !active.view) return;
	const at = event.target, e = keyEvent(event);
	const busy = at.closest && at.closest("input, select, textarea, button, .tau-dialog, .tau-popup, .tau-tool");
	const command = e.ctrlKey || e.metaKey;
	if (busy) {
		// a command key which is not the input's: the browser does not get it
		if (!command || browserKey(e, isMac) || EDITING.includes((e.key || "").toLowerCase())) return;
		const r = translate(e, isMac, layout);
		if (!r || !r.key) return;
		event.preventDefault();
		if (["M-+", "M--", "M-0", "C-+", "C--", "C-0"].includes(r.key)) send({ t: "key", view: active.view, key: r.key });
		return;
	}
	if (event.isComposing || browserKey(e, isMac) || pasteKey(e, isMac)) return;
	focusView(active);
	const r = translate(e, isMac, layout);
	if (!r) return;
	event.preventDefault();
	if (r.key) sendKey(r.key);
	else if ([...event.key].length === 1) sendText(event.key); // (the text area did not get this one)
});

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
		if (active) focusView(active);
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

// what is pasted in a view: the text and the HTML of the clipboard, which
// the browser gives to the text area of the keyboard
area.addEventListener("paste", event => {
	event.preventDefault();
	if (!focused || !focused.view) return;
	const data = event.clipboardData;
	const files = Array.from(data.files || []);
	if (files.length) { for (const file of files) openFile(file, 0); return; }
	const text = data.getData("text/plain"), html = data.getData("text/html");
	if (text || html) send({ t: "paste", view: focused.view, text, html });
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
		if (fresh || !active) focusView(pane);
		else send({ t: "focus", view: pane.view, focus: focused === pane && document.activeElement === area });
		break;
	}
	case "buffers": {
		// where the main and the mode icon bars are: the preference, or what
		// the address says (?bars=top or left)
		document.body.classList.toggle("tau-bars-left", (params.get("bars") || m.bars || "left") === "left");
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
		if (made) focusView(made); // a new pane takes the keyboard
		showTitle();
		break;
	}
	case "paint": {
		const pane = targetOf(m.view);
		if (!pane || m.place !== pane.place) break; // for an older place
		const canvas = pane.canvas;
		if (canvas.width !== m.width || canvas.height !== m.height) {
			canvas.width = m.width; canvas.height = m.height;
		}
		pane.context.putImageData(
			new ImageData(new Uint8ClampedArray(m.pixels), m.width, m.height), 0, 0);
		pane.extents = m.extents; pane.scroll = m.scroll; pane.caret = m.caret;
		if (pane === focused && !composing) placeArea();
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
