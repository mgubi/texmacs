// The page of Tau (docs/tau-design.md). The core runs in a worker
// (tau-worker.js). The page shows its views: each window of the core is a
// pane here, with the documents as tabs and a canvas for the view which
// the window shows. This module tells the core the places of the views,
// the keys and the pointer, draws the pixels it sends, and looks after
// what belongs to the browser: the clipboard and the files of the user.

import * as chrome from "./chrome.mjs";
import * as app from "./app.mjs";
import * as files from "./files.mjs";
import { translate, makeLayout, learn, askLayout, browserKey, pasteKey, commandLetter } from "./keys.mjs";

const panesElement = document.getElementById("tau-panes");
const statusLine = document.getElementById("status");
const params = new URLSearchParams(location.search);
const verbose = params.has("log");

// ?arg=... is a document to open (a path under /texmacs or /home/tau)
const worker = new Worker("tau-worker.js" + location.search);
// What the wheel and the pointer say comes faster than the core may
// answer: a step of a list which is typeset again takes it a tenth of a
// second, and steps which wait in a queue are each done in turn, long
// after the hand stopped. So one such message is with the core at a time
// (the worker says when it is done with it: a ping sent after it comes
// back); what comes meanwhile is added up (the wheel) or replaces what
// waited (the pointer, a size), and goes when the core is free. Any other message
// first sends what waits, so that the order is kept.
let pacing = false, paceId = 0;
const held = new Map(); // what waits: "scroll:<view>" or "move:<view>" -> message
function paced(m) {
	if (m.t === "scroll") return "scroll:" + m.view;
	if (m.t === "mouse" && m.kind === "move") return "move:" + m.view;
	// (a size of a canvas too: dragging the line between two panes makes
	// one at each step, each a whole view to draw; the last one waits, and
	// the canvas shows only what is drawn for its last place)
	if (m.t === "place") return "place:" + m.view;
	return null;
}
function sendHeld() {
	for (const m of held.values()) worker.postMessage(m);
	held.clear();
}
function pace() {
	pacing = true;
	worker.postMessage({ t: "ping", id: "pace" + (++paceId) });
}
function paceDone(id) {
	if (id !== "pace" + paceId) return;
	pacing = false;
	if (held.size) { sendHeld(); pace(); }
}
const send = message => {
	const key = paced(message);
	if (!key) { sendHeld(); worker.postMessage(message); return; }
	if (!pacing) { worker.postMessage(message); pace(); return; }
	const waits = held.get(key);
	if (waits && message.t === "scroll") { waits.dx += message.dx; waits.dy += message.dy; waits.x = message.x; waits.y = message.y; }
	else held.set(key, { ...message });
};

const panes = new Map();  // number of a window of the core -> pane
let active = null;        // the pane which has the keyboard, or had it last
let buffers = [];         // the documents: { name, title, modified }

let leaving = false;   // the page is left on purpose: nothing is asked

// the icon of Tau at the top left, with its menu, and the panel of the
// loading (app.mjs)
let homeKept = false;
app.initApp({
	homeKept: () => homeKept,
	nothingKept: params.has("nohome"),
	reload: () => { leaving = true; location.reload(); },
	forget: () => { leaving = true; send({ t: "forget-home" }); },
	files: () => files.browse(),
	backup: () => files.backup(),
	restore: () => files.restore()
});
app.progress({ what: "" });

// A file dropped beside the views (on a bar, the tabs, the footer) is not
// opened by the browser in place of Tau: only the views and the panel of
// the files take what is dropped
document.addEventListener("dragover", event => event.preventDefault());
document.addEventListener("drop", event => event.preventDefault());

// for tests and for the console
const state = { started: false, paints: 0, downloads: [], get view() { return active ? active.view : 0; } };
window.tau = { state, send, worker, panes, files, fs: (op, args, transfer) => fsRequest(op, args, transfer), get active() { return active; } };
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
	addScrollbars(pane, holder);
	// a file which is dropped on a pane is opened there
	pane.canvas.addEventListener("dragover", event => { event.preventDefault(); event.dataTransfer.dropEffect = "copy"; });
	pane.canvas.addEventListener("drop", event => {
		event.preventDefault();
		focusView(pane);
		if (!event.dataTransfer) return;
		const rect = pane.canvas.getBoundingClientRect(), density = window.devicePixelRatio || 1;
		const x = Math.round((event.clientX - rect.left) * density), y = Math.round((event.clientY - rect.top) * density);
		files.dropOnView(event.dataTransfer, paths => send({ t: "drop", view: pane.view, x, y, paths }))
			.catch(error => console.error("Tau: the drop failed", error));
	});
	return pane;
}

// The scroll bars of a pane: where the view is in its document, which the
// core tells with what it draws. A thumb is dragged, a click beside it
// moves by what is seen; the core scrolls, the bars follow.
function addScrollbars(pane, holder) {
	const bars = {};
	for (const axis of ["y", "x"]) {
		const track = el("div", "tau-scroll tau-scroll-" + axis), thumb = el("div", "tau-thumb");
		track.append(thumb);
		holder.append(track);
		bars[axis] = { track, thumb };
		const vertical = axis === "y";
		const size = () => vertical ? pane.canvas.height : pane.canvas.width;       // what is seen
		const whole = () => vertical ? pane.extents.height : pane.extents.width;   // the document
		const length = () => vertical ? track.clientHeight : track.clientWidth;
		const thumbLength = () => Math.max(24, length() * size() / Math.max(whole(), 1));
		const by = d => {
			d = Math.round(d);
			if (d) send({ t: "scroll", view: pane.view, dx: vertical ? 0 : d, dy: vertical ? d : 0 });
		};
		thumb.addEventListener("pointerdown", event => {
			event.preventDefault();
			event.stopPropagation();
			thumb.setPointerCapture(event.pointerId);
			track.classList.add("tau-dragging");
			const start = vertical ? event.clientY : event.clientX;
			const from = pane.scroll[axis];
			let asked = from;
			const move = e => {
				const room = length() - thumbLength();
				if (room <= 0) return;
				const d = ((vertical ? e.clientY : e.clientX) - start) / room * (whole() - size());
				const to = Math.max(0, Math.min(whole() - size(), from + d));
				by(to - asked);
				asked += Math.round(to - asked);
			};
			const up = () => {
				track.classList.remove("tau-dragging");
				thumb.removeEventListener("pointermove", move);
				thumb.removeEventListener("pointerup", up); thumb.removeEventListener("pointercancel", up);
			};
			thumb.addEventListener("pointermove", move);
			thumb.addEventListener("pointerup", up);
			thumb.addEventListener("pointercancel", up);
		});
		track.addEventListener("pointerdown", event => {
			if (event.target !== track) return;
			event.preventDefault();
			const r = thumb.getBoundingClientRect();
			const before = vertical ? event.clientY < r.top : event.clientX < r.left;
			by((before ? -0.9 : 0.9) * size());
		});
	}
	pane.showScroll = () => {
		for (const axis of ["y", "x"]) {
			const { track, thumb } = bars[axis], vertical = axis === "y";
			const size = vertical ? pane.canvas.height : pane.canvas.width;
			const whole = vertical ? pane.extents.height : pane.extents.width;
			const needed = whole > size + 2;
			track.hidden = !needed;
			if (!needed) continue;
			const length = vertical ? track.clientHeight : track.clientWidth;
			const t = Math.max(24, length * size / whole);
			const at = (length - t) * Math.max(0, Math.min(1, pane.scroll[axis] / (whole - size)));
			thumb.style[vertical ? "height" : "width"] = t + "px";
			thumb.style.transform = vertical ? `translateY(${at}px)` : `translateX(${at}px)`;
		}
	};
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
	// (a dialog which is shown again makes a new canvas for its view: the
	// one it replaces, still in the page at this moment, is watched no more)
	const before = embedded.get(node.view);
	if (before) before.observer.disconnect();
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
			mods: mods(event),
			// (when it happened: the core may come to it much later)
			at: performance.timeOrigin + event.timeStamp
		});
	};
	// (a click on the canvas would take the keyboard from its text area)
	canvas.addEventListener("mousedown", event => event.preventDefault());
	// One pointer at a time (a second finger is not a second mouse). A
	// press is always released: when the browser takes the pointer away
	// (pointercancel), the core is told the button came up. A finger which
	// moves scrolls the view; one which does not is a click.
	let pressed = null;   // the button which is down, for the core
	let touch = null;     // a finger on the canvas: { x, y, moved }
	const TAP = 8;        // (how far a finger may move and still click)
	const release = event => {
		if (pressed === null) return;
		sendMouse("release-" + pressed, event);
		pressed = null;
	};
	canvas.addEventListener("pointerdown", event => {
		if (!event.isPrimary) return;
		focusView(target);
		canvas.setPointerCapture(event.pointerId);
		event.preventDefault();
		if (event.pointerType === "touch") { touch = { x: event.clientX, y: event.clientY, moved: false }; return; }
		pressed = BUTTONS[event.button] || "left";
		sendMouse("press-" + pressed, event);
	});
	canvas.addEventListener("pointerup", event => {
		if (!event.isPrimary) return;
		if (touch) {
			const tap = !touch.moved;
			touch = null;
			if (tap) { sendMouse("press-left", event); sendMouse("release-left", event); }
			return;
		}
		release(event);
	});
	const lost = event => { if (event.isPrimary) { touch = null; release(event); } };
	canvas.addEventListener("pointercancel", lost);
	canvas.addEventListener("lostpointercapture", lost);
	// (The moves are paced by send: the last one waits while the core works,
	// and a press or a release sends it first. They were kept for the next
	// frame before, and a move from before a press then came after it, with
	// no button: the core took it for the release, and the release which
	// followed for a second click.)
	canvas.addEventListener("pointermove", event => {
		if (!event.isPrimary) return;
		if (!touch) { sendMouse("move", event); return; }
		if (!touch.moved && Math.hypot(event.clientX - touch.x, event.clientY - touch.y) < TAP) return;
		touch.moved = true;
		if (!target.view) return;
		// (the document follows the finger: it goes up when the finger does)
		const density = window.devicePixelRatio || 1;
		const dx = Math.round((touch.x - event.clientX) * density), dy = Math.round((touch.y - event.clientY) * density);
		touch.x = event.clientX; touch.y = event.clientY;
		const rect = canvas.getBoundingClientRect();
		if (dx || dy) send({ t: "scroll", view: target.view, dx, dy,
			x: Math.round((event.clientX - rect.left) * density), y: Math.round((event.clientY - rect.top) * density) });
	});
	canvas.addEventListener("contextmenu", event => event.preventDefault());

	// The wheel scrolls the view; send adds up the steps which come while
	// the core works
	let wheelX = 0, wheelY = 0; // (the fractions of a pixel which were not sent)
	canvas.addEventListener("wheel", event => {
		event.preventDefault();
		if (!target.view) return;
		const density = window.devicePixelRatio || 1;
		const unit = event.deltaMode === 1 ? 32 : event.deltaMode === 2 ? canvas.clientHeight : 1;
		wheelX += event.deltaX * unit * density;
		wheelY += event.deltaY * unit * density;
		const dx = Math.trunc(wheelX), dy = Math.trunc(wheelY);
		if (!dx && !dy) return;
		wheelX -= dx; wheelY -= dy;
		// (with the place of the pointer, for an editor which takes the wheel)
		const rect = canvas.getBoundingClientRect();
		send({ t: "scroll", view: target.view, dx, dy,
			x: Math.round((event.clientX - rect.left) * density), y: Math.round((event.clientY - rect.top) * density) });
	}, { passive: false });
}

// The tools at the sides of the views are made wider or narrower by their
// edge towards the views
for (const [id, sign] of [["tau-side-0", -1], ["tau-side-1", 1]]) {
	const tool = document.getElementById(id), grip = el("div", "tau-tool-grip");
	if (!tool) continue;
	tool.parentElement.insertBefore(grip, sign < 0 ? tool : tool.nextSibling);
	new MutationObserver(() => { grip.hidden = tool.hidden || !tool.firstElementChild; }).observe(tool, { attributes: true, childList: true });
	grip.hidden = true;
	grip.addEventListener("pointerdown", event => {
		event.preventDefault();
		grip.setPointerCapture(event.pointerId);
		const x0 = event.clientX, w0 = tool.offsetWidth;
		const move = e => { tool.style.width = Math.max(160, Math.min(window.innerWidth * 0.6, w0 + sign * (e.clientX - x0))) + "px"; };
		const up = () => { grip.removeEventListener("pointermove", move); grip.removeEventListener("pointerup", up); grip.removeEventListener("pointercancel", up); };
		grip.addEventListener("pointermove", move);
		grip.addEventListener("pointerup", up);
		grip.addEventListener("pointercancel", up);
	});
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
				divider.removeEventListener("pointerup", up); divider.removeEventListener("pointercancel", up);
			};
			divider.addEventListener("pointermove", move);
			divider.addEventListener("pointerup", up);
			divider.addEventListener("pointercancel", up);
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
area.setAttribute("aria-label", "Document");
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
		// (what was being composed for the other view is given up: its
		// pre-edit would stay there, and the text would come here)
		if (composing) {
			if (focused && focused.view) send({ t: "key", view: focused.view, key: "pre-edit:" });
			composing = false;
			area.value = "";
			area.blur();
		}
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
area.addEventListener("blur", () => {
	// (a composition which is left does not end: the keys are not the
	// input method's any more)
	composing = false;
	if (focused && focused.view) send({ t: "focus", view: focused.view, focus: false });
});

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
let composed = null; // what a composition ended with, a moment ago
area.addEventListener("input", event => {
	if (composing || event.isComposing) return;
	const text = area.value;
	area.value = "";
	// (Safari tells the end of a composition, then the text it inserts once
	// more: it was sent with the end already)
	const again = composed;
	composed = null;
	if (event.inputType === "insertFromComposition" || (again && again.text === text && performance.now() - again.at < 200)) return;
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
	composed = text ? { text, at: performance.now() } : null;
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
		if (!command || browserKey(e, isMac) || EDITING.includes(commandLetter(e))) return;
		const r = translate(e, isMac, layout);
		if (!r || !r.key) return;
		event.preventDefault();
		if (["M-+", "M--", "M-0", "C-+", "C--", "C-0"].includes(r.key)) send({ t: "key", view: active.view, key: r.key });
		return;
	}
	if (event.isComposing || browserKey(e, isMac)) return;
	// (the paste is the browser's, into the text area of the view, which
	// takes the keyboard first: in nothing, the paste went nowhere)
	if (pasteKey(e, isMac)) { focusView(active); return; }
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

// A document which the address of the page gives, opened once the core
// is ready: ?file=<path>, a file which is kept in the browser or a file of
// TeXmacs, and ?open=<address>, a document of the web. That one is
// fetched by the page (another site has to allow it: CORS) and put aside,
// not among the documents of the user: it is only looked at, until it is
// saved under a name. What it refers to (images, included files) is not
// fetched with it. The address runs nothing.
function openFromAddress() {
	const file = params.get("file"), src = params.get("open");
	if (file) {
		if (/^\/(home\/tau|texmacs)\//.test(file) && !file.split("/").includes(".."))
			worker.postMessage({ t: "chosen", ticket: 0, path: file });
		else notice("No document " + file + " in this browser.");
	}
	if (!src) return;
	let address;
	try { address = new URL(src, location.href); }
	catch (error) { notice("Not an address: " + src); return; }
	let name = address.pathname.slice(address.pathname.lastIndexOf("/") + 1);
	try { name = decodeURIComponent(name); } catch (error) {} // (a "%" which is not an escape)
	name = name.replace(/[\/\\]/g, "_") || "document.tm";
	if (!/\.(tm|tmml|ts|tp|stm|tex|html?|md|bib|scm|txt)$/i.test(name)) name += ".tm";
	fetch(address.href).then(response => {
		if (!response.ok) throw new Error(response.status + " " + response.statusText);
		return response.arrayBuffer();
	}).then(bytes => worker.postMessage({ t: "open", ticket: 0, name, bytes, temp: true }, [bytes]))
		.catch(error => notice("Cannot open " + address.href + ": " + (error.message || error) +
			(address.origin !== location.origin ? " (does the site allow it?)" : "")));
}

// The panel of the files (files.mjs) asks the worker, which has the file
// system, and waits for its answers
let fsNext = 1;
const fsWaiting = new Map();
let workerGone = false; // the worker could not start: no answer comes
function fsRequest(op, args, transfer) {
	return new Promise((resolve, reject) => {
		if (workerGone) { reject(new Error("the program of Tau did not start")); return; }
		const id = fsNext++;
		fsWaiting.set(id, { resolve, reject });
		worker.postMessage({ t: "fs", id, op, ...(args || {}) }, transfer || []);
	});
}
function fsAnswer(m) {
	const w = fsWaiting.get(m.id);
	fsWaiting.delete(m.id);
	if (!w) return;
	if (m.error) w.reject(Object.assign(new Error(m.error), { restart: !!m.restart })); else w.resolve(m.value);
}
files.initFiles({
	request: fsRequest,
	chosen: (ticket, path) => worker.postMessage({ t: "chosen", ticket, path }),
	open: path => worker.postMessage({ t: "chosen", ticket: 0, path }),
	reload: () => { leaving = true; location.reload(); },
	focus: () => { if (active) focusView(active); },
	downloaded: (name, blob) => { state.downloads.push({ name, size: blob.size }); }
});

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

// A PDF which TeXmacs made to be looked at or printed (Preview, Print): in
// a tab of the browser, which has what it takes to read, print and save
// it. The browser may refuse a tab which no click asked for (the document
// took long to make): a line with a link then.
function openPdf(m) {
	if (!m.bytes) return;
	// (the address of the PDF is given back once the tab has it, or when the
	// line which offers it goes: each stayed in memory as long as the page)
	const url = URL.createObjectURL(new Blob([m.bytes], { type: "application/pdf" }));
	const forget = () => setTimeout(() => URL.revokeObjectURL(url), 60000);
	const tab = window.open(url, "_blank");
	if (tab) { forget(); return; }
	const bar = el("div", "tau-notice"), link = el("a", "", "Open " + m.name), close = el("button", "tau-doc-close", "×");
	link.href = url;
	link.target = "_blank";
	link.addEventListener("click", () => { bar.remove(); forget(); });
	close.type = "button";
	close.addEventListener("click", () => { bar.remove(); URL.revokeObjectURL(url); });
	bar.append(el("span", "", "The PDF is ready."), link, close);
	panesElement.parentElement.insertBefore(bar, panesElement);
}

// The full screen mode of TeXmacs: the page takes the screen, if the
// browser lets it (it wants a click or a key of the user just before), and
// shows the views alone. When the user leaves it with the browser (Escape),
// TeXmacs is told.
let fullScreenAsked = false;
function fullScreen(on) {
	fullScreenAsked = on;
	document.body.classList.toggle("tau-fullscreen", on);
	const root = document.documentElement;
	if (on && !document.fullscreenElement && root.requestFullscreen) root.requestFullscreen().catch(() => {});
	if (!on && document.fullscreenElement && document.exitFullscreen) document.exitFullscreen().catch(() => {});
	if (active) focusView(active);
}
document.addEventListener("fullscreenchange", () => {
	if (!document.fullscreenElement && fullScreenAsked) { fullScreenAsked = false; send({ t: "fullscreen-left" }); }
});

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

// Paste asked from a menu: the clipboard of the browser is read (the
// browser may ask the user), and what TeXmacs kept is pasted when it
// cannot be
async function pasteFromBrowser() {
	const target = focused || active;
	if (!target || !target.view) return;
	let text = "", html = "";
	try {
		if (navigator.clipboard.read) {
			for (const item of await navigator.clipboard.read()) {
				if (item.types.includes("text/html")) html = await (await item.getType("text/html")).text();
				if (item.types.includes("text/plain")) text = await (await item.getType("text/plain")).text();
			}
		} else text = await navigator.clipboard.readText();
	} catch (error) { if (verbose) console.log("clipboard: " + error.message); }
	send({ t: "paste", view: target.view, text, html });
	focusView(target);
}

// Safari deletes what a site keeps after seven days in which it was not
// visited, unless the site may keep it (persist, which Safari grants to a
// page added to the Home Screen): said once, as long as what the browser
// keeps is there (a flag among it)
const isSafari = /^((?!chrome|chromium|android|crios|fxios|edg).)*safari/i.test(navigator.userAgent);
function evictionNotice() {
	if (!isSafari && !params.has("eviction")) return;
	try {
		if (localStorage.getItem("tau-eviction-said")) return;
		localStorage.setItem("tau-eviction-said", String(Date.now()));
	} catch (error) {}
	notice("Safari deletes the documents which a site keeps once it was not visited for seven days. Save a backup from time to time, or add Tau to the Home Screen.",
		"Save a backup", () => files.backup());
}

// What was copied goes to the clipboard of the browser. The browser may
// refuse it (it wants a click or a key of the user just before, and the
// core answered later): a line then offers to copy it with a click, which
// it accepts. (The keys of the browser would not do: they are the core's.)
let copyBar = null, copyText = "";
function copy(text) {
	const write = t => navigator.clipboard && navigator.clipboard.writeText
		? navigator.clipboard.writeText(t) : Promise.reject(new Error("no clipboard"));
	write(text).then(() => { if (copyBar) { copyBar.remove(); copyBar = null; } }, error => {
		if (verbose) console.log("clipboard: " + error.message);
		copyText = text;
		if (copyBar && copyBar.isConnected) return;
		copyBar = notice("The browser did not let Tau write to the clipboard.", "Copy", () => {
			write(copyText).then(() => { copyBar.remove(); copyBar = null; },
				() => notice("The browser does not let Tau write to its clipboard."));
		});
	});
}

// ---------------------------------------------------------------------------
// The messages of the core
// ---------------------------------------------------------------------------

// While the core works (it loads a document, typesets a long one...) the
// worker answers nothing, and the page shows that it is busy. The page
// asks the worker a few times a second whether it is there: an answer
// which is late by more than a moment is the core at work, and the answer
// ends it. The keys which are typed meanwhile wait, and are not lost.
const BUSY_AFTER = 300, WORKING_AFTER = 2000;
const busyBar = document.getElementById("tau-busy-bar"), working = document.getElementById("tau-working");
let pingAt = 0, pingId = 0;
function setBusy(late) {
	const busy = late > BUSY_AFTER;
	state.busy = busy;
	if (busyBar.hidden === busy) busyBar.hidden = !busy;
	document.body.classList.toggle("tau-busy", busy);
	working.hidden = late <= WORKING_AFTER;
}
setInterval(() => {
	if (!state.started || failed || leaving) { setBusy(0); return; }
	const now = performance.now();
	if (pingAt) { setBusy(now - pingAt); return; }
	if (document.hidden) return;
	pingAt = now;
	worker.postMessage({ t: "ping", id: ++pingId });
}, 150);

worker.onmessage = event => {
	const m = event.data;
	if (m.t === "pong") {
		if (m.id === pingId) { pingAt = 0; setBusy(0); } else paceDone(m.id);
		return;
	}
	if (chrome.handle(m)) return;
	switch (m.t) {
	case "status":
		if (!state.started) setStatus(m.text);
		break;
	case "progress":
		app.progress(m);
		break;
	case "log":
		if (verbose) console.log(m.text);
		break;
	case "storage":
		// what was saved could not be written to the browser (no room
		// left): said once, it is true of all that follows
		if (!storageFailed) {
			storageFailed = true;
			notice("The browser could not store your documents (" + m.text + "): what you save now may be lost when this page is closed. Download a copy of your documents, or save a backup.");
		}
		break;
	case "failed":
		setStatus("Tau could not start: " + m.text);
		app.failed("Tau could not start: " + m.text);
		failure(m.text, "Tau could not start");
		break;
	case "ready":
		if (!state.started) setStatus("Tau is ready, no view yet");
		// another page of Tau keeps the documents and the preferences
		homeKept = !!m.homeKept;
		// (the documents are only in the browser: it is asked not to delete
		// them when it wants room, once)
		if (homeKept && navigator.storage && navigator.storage.persist)
			navigator.storage.persisted().then(yes => yes || navigator.storage.persist())
				.then(yes => { if (verbose) console.log("storage persisted: " + yes); if (!yes) evictionNotice(); }, () => {});
		// (or the browser could not give them: nothing is written over them)
		if (!m.homeKept && m.homeError)
			notice("The documents kept in this browser could not be read (" + m.homeError + "): what is saved here is not kept, and what the browser has is left as it is.");
		else if (!m.homeKept && !params.has("nohome"))
			notice("Tau is open in another page of this browser: what is saved here is not kept when this page is closed.");
		openFromAddress();
		break;
	case "quit":
		// TeXmacs was closed (it asked about what is not saved): it starts
		// again, unless what it wrote could not be stored, which reloading
		// would lose (the files are still in the worker: a backup of them)
		if (m.failed) {
			failure("TeXmacs was closed, but the browser could not store your documents (" + m.failed + "). " +
				"Save a backup before you start Tau again: what was saved since the problem is only in this page.", "Your documents are not stored");
			break;
		}
		leaving = true;
		location.reload();
		break;
	case "saving":
		// what was saved is being written to the browser (tau-pre.js)
		saving = !!m.on;
		break;
	case "storage-erased":
		// another page of Tau forgot what is kept in this browser
		homeKept = false;
		notice("What Tau kept in this browser was deleted from another page: what is saved here is not kept any more. Save a backup to keep it.");
		break;
	case "forget-blocked":
		leaving = false;
		notice("What Tau keeps in this browser is deleted once the other pages of Tau are closed: close them.");
		break;
	case "forget-failed":
		leaving = false;
		notice("The browser did not delete what Tau keeps (" + m.text + ").");
		break;
	case "stopped":
		// (the browser gave no more memory: said as such)
		if (/\bOOM\b|out of memory|Cannot enlarge memory/i.test(m.text))
			failure("Tau ran out of memory: the browser gave it no more (" + m.text + "). Documents which were saved are kept; what was typed since is lost. Closing other tabs, or the documents which are open, may help.", "Tau ran out of memory", m.stack);
		else failure("The program of Tau stopped on an error and cannot go on: " + m.text +
			" Documents which were saved are kept; what was typed since is lost.", undefined, m.stack);
		break;
	case "view": {
		// the view which a window shows: tell it its place, and give it the
		// keyboard if its pane has it
		// (a new pane takes the keyboard; a view which is no longer shown
		// is followed by the one which is, and the core drops what is
		// said to it meanwhile)
		if (m.what !== "shown") break;
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
		// (a narrow screen, a phone, keeps its width for the document)
		const narrow = window.matchMedia && window.matchMedia("(max-width: 480px)").matches;
		document.body.classList.toggle("tau-bars-left", (params.get("bars") || (narrow ? "top" : m.bars) || "left") === "left");
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
		// the rectangle of the canvas which changed
		pane.context.putImageData(new ImageData(new Uint8ClampedArray(m.pixels), m.w, m.h), m.x, m.y);
		pane.extents = m.extents; pane.scroll = m.scroll; pane.caret = m.caret;
		if (pane.showScroll) pane.showScroll();
		if (pane === focused && !composing) placeArea();
		state.paints++;
		if (!state.started) app.started();
		state.started = true;
		if (params.has("debug"))
			setStatus(`view ${m.view} · ${m.width}×${m.height} · document ${m.extents.width}×${m.extents.height}` +
				` · scroll ${m.scroll.x},${m.scroll.y} · ${state.paints} paints`);
		else if (statusLine.textContent) setStatus("");
		break;
	}
	case "clipboard": copy(m.text); break;
	case "paste-request": pasteFromBrowser(); break;
	case "files": files.choose(m); break;
	case "fs": fsAnswer(m); break;
	case "download": download(m); break;
	case "open-pdf": openPdf(m); break;
	case "fullscreen": fullScreen(m.on); break;
	}
};
worker.onerror = event => {
	if (!state.started) {
		// (the worker did not load: nothing will answer, the requests which
		// wait are given up, and the panel of the loading stops)
		const text = "The worker failed: " + (event.message || "it could not be loaded");
		setStatus(text);
		workerGone = true;
		for (const w of fsWaiting.values()) w.reject(new Error("the program of Tau did not start"));
		fsWaiting.clear();
		app.failed("Tau could not start: " + text);
		failure(text, "Tau could not start");
	}
	else if (/RuntimeError|RangeError|call stack/i.test(event.message || "")) failure("The program of Tau stopped on an error and cannot go on: " + event.message);
};

// A line over the views which the user closes
let storageFailed = false;

function notice(text, label, action) {
	const bar = el("div", "tau-notice"), close = el("button", "tau-doc-close", "×");
	bar.setAttribute("role", "status");
	bar.setAttribute("aria-live", "polite");
	close.type = "button";
	close.title = "Close";
	close.addEventListener("click", () => bar.remove());
	bar.append(el("span", "", text));
	if (label) {
		const b = el("button", "tau-button", label);
		b.type = "button";
		b.addEventListener("click", action);
		bar.append(b);
	}
	bar.append(close);
	// (once Tau stopped, in the box which says so: the veil hid it, and
	// what "Forget everything" answered, close the other pages, was not seen)
	if (failureBox) failureBox.insertBefore(bar, failureBox.querySelector(".tau-failure-actions"));
	else panesElement.parentElement.insertBefore(bar, panesElement);
	return bar;
}

// The program stopped: nothing which is typed reaches it any more
let failed = false;
let failureBox = null; // what says so
function failure(text, title, stack) {
	if (failed) return;
	failed = true;
	const box = el("div", "tau-failure"), again = el("button", "tau-button", "Start Tau again");
	failureBox = box;
	again.type = "button";
	again.addEventListener("click", () => { leaving = true; location.reload(); });
	// What is kept in the browser is still within reach: a copy of it, and
	// a way to start from nothing when it is what stops Tau (the worker
	// answers these two without the program)
	const backup = el("button", "tau-button", "Save a backup"), forget = el("button", "tau-button", "Forget everything…");
	backup.type = forget.type = "button";
	backup.addEventListener("click", () => files.backup());
	forget.addEventListener("click", () => {
		if (!confirm("Delete the documents and the preferences which Tau keeps in this browser?")) return;
		leaving = true;
		send({ t: "forget-home" });
	});
	const actions = el("div", "tau-failure-actions");
	actions.append(backup, forget, again);
	box.append(el("div", "tau-failure-title", title || "Tau stopped"), el("div", "", text));
	// (where it happened, for a report: shown on demand, to copy)
	if (stack) {
		const more = el("details", "tau-failure-stack"), pre = el("pre", "", stack);
		more.append(el("summary", "", "Details for a report"), pre);
		box.append(more);
	}
	box.append(actions);
	const veil = el("div", "tau-veil");
	veil.append(box);
	document.body.append(veil);
	again.focus();
}

// Leaving the page with documents which are not saved asks first, and so
// does leaving it while what was saved is still being written to the
// browser (a moment after the save)
let saving = false;
window.addEventListener("beforeunload", event => {
	if (leaving || (failed && !saving) || (!saving && !buffers.some(b => b.modified))) return;
	event.preventDefault();
	event.returnValue = "";
});
