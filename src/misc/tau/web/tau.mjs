// The page of Tau, step 2 of docs/tau-design.md: one view of the editor in
// a bare canvas. The core runs in a worker (tau-worker.js); this module
// tells it the place of the view, the keys and the pointer, and draws the
// pixels it sends.

const canvas = document.getElementById("canvas");
const context = canvas.getContext("2d");
const statusLine = document.getElementById("status");
const params = new URLSearchParams(location.search);
const verbose = params.has("log");

// ?arg=... are the arguments of TeXmacs (a document to open, say)
const worker = new Worker("tau-worker.js" + location.search);
const send = message => worker.postMessage(message);

const state = {
	view: 0,          // the number of the view which is shown
	place: 0,         // the number of the last place sent
	extents: { width: 0, height: 0 },
	scroll: { x: 0, y: 0 },
	paints: 0
};
// for tests and for the console
window.tau = { state, send, worker };

function setStatus(text) { statusLine.textContent = text; }

// The place: the size of the canvas in pixels of the screen. Each place has
// a number, which the core repeats with what it draws for it.
function sendPlace() {
	if (!state.view) return;
	const density = window.devicePixelRatio || 1;
	const width = Math.max(1, Math.round(canvas.clientWidth * density));
	const height = Math.max(1, Math.round(canvas.clientHeight * density));
	state.place++;
	send({ t: "place", view: state.view, width, height, density, place: state.place });
}

worker.onmessage = event => {
	const m = event.data;
	switch (m.t) {
	case "status":
		if (!state.view) setStatus(m.text);
		break;
	case "log":
		if (verbose) console.log(m.text);
		break;
	case "failed":
		setStatus("Tau could not start: " + m.text);
		break;
	case "ready":
		if (!state.view) setStatus("Tau is ready, no view yet");
		break;
	case "view":
		// the view which is shown: tell it its place and give it the keyboard
		state.view = m.view;
		sendPlace();
		send({ t: "focus", view: state.view, focus: document.activeElement === canvas });
		break;
	case "paint":
		if (m.view !== state.view || m.place !== state.place) break; // for an older place
		if (canvas.width !== m.width || canvas.height !== m.height) {
			canvas.width = m.width; canvas.height = m.height;
		}
		context.putImageData(
			new ImageData(new Uint8ClampedArray(m.pixels), m.width, m.height), 0, 0);
		state.extents = m.extents; state.scroll = m.scroll; state.caret = m.caret;
		state.paints++;
		setStatus(`view ${m.view} · ${m.width}×${m.height} · document ${m.extents.width}×${m.extents.height}` +
			` · scroll ${m.scroll.x},${m.scroll.y} · ${state.paints} paints`);
		break;
	}
};
worker.onerror = event => setStatus("The worker failed: " + (event.message || ""));

new ResizeObserver(sendPlace).observe(canvas);

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

canvas.addEventListener("keydown", event => {
	if (!state.view || event.isComposing || browserKey(event)) return;
	const key = keyName(event);
	if (!key) return;
	event.preventDefault();
	send({ t: "key", view: state.view, key });
});

canvas.addEventListener("focus", () => state.view && send({ t: "focus", view: state.view, focus: true }));
canvas.addEventListener("blur", () => state.view && send({ t: "focus", view: state.view, focus: false }));

// The pointer: positions in pixels of the canvas; the modifiers as TeXmacs
// counts them (buttons 1, 2, 4; Shift 256, Control 1024, Alt 2048, Meta 4096)
const BUTTONS = ["left", "middle", "right"];
function mods(event) {
	return (event.buttons & 1 ? 1 : 0) | (event.buttons & 4 ? 2 : 0) | (event.buttons & 2 ? 4 : 0) |
		(event.shiftKey ? 256 : 0) | (event.ctrlKey ? 1024 : 0) |
		(event.altKey ? 2048 : 0) | (event.metaKey ? 4096 : 0);
}
function sendMouse(kind, event) {
	if (!state.view) return;
	const rect = canvas.getBoundingClientRect(), density = window.devicePixelRatio || 1;
	send({
		t: "mouse", view: state.view, kind,
		x: Math.round((event.clientX - rect.left) * density),
		y: Math.round((event.clientY - rect.top) * density),
		mods: mods(event)
	});
}
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

// The wheel scrolls the view; the steps which have not been sent yet are
// added up, one message for each frame
let pendingScroll = null;
canvas.addEventListener("wheel", event => {
	event.preventDefault();
	if (!state.view) return;
	const density = window.devicePixelRatio || 1;
	const unit = event.deltaMode === 1 ? 32 : event.deltaMode === 2 ? canvas.clientHeight : 1;
	if (!pendingScroll) {
		pendingScroll = { dx: 0, dy: 0 };
		requestAnimationFrame(() => {
			send({ t: "scroll", view: state.view,
				dx: Math.round(pendingScroll.dx), dy: Math.round(pendingScroll.dy) });
			pendingScroll = null;
		});
	}
	pendingScroll.dx += event.deltaX * unit * density;
	pendingScroll.dy += event.deltaY * unit * density;
}, { passive: false });

canvas.focus();
