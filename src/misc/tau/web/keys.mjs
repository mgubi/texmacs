// The keys of the browser as TeXmacs names them ("a", "return", "S-left",
// "C-x", "M-+"): what tau.mjs sends to the core. No page is needed here, so
// that the rules are tested by themselves (misc/tau/test/keys-test.mjs).
//
// As the other ports of TeXmacs do (lookup_key and postprocess_key_event of
// the Vue port), a key press is one of two things:
//
// - text: a character key without a modifier which makes a command. The
//   key is not sent; the text which the system makes of it (with its dead
//   keys, its input method) comes from the input events of the page.
// - a key: a key which has a name (return, left, F5...), or a character
//   key with Control, Command or Alt. Its name is the character at that
//   place of the keyboard, with the modifiers as prefixes. Shift and (not
//   on a Mac with Command or Control) Option are no prefix when they
//   change the character: Shift with = is +, so Command with them is M-+,
//   and Command with Shift and n is M-N. They stay when they do not
//   (S-left, S-space).
//
// What the character at a place is, the browsers do not say the same way:
// with Command down some give the key with Shift (Shift with = is "+"),
// some without ("="). So the character is taken from the event when it
// shows the modifier, and else from what is known of the keyboard: the
// layout which the browser tells (Chromium), the keys which were seen
// typing text, and the US keyboard when the key is where it is there.

const NAMED = {
	Enter: "return", Backspace: "backspace", Delete: "delete", Tab: "tab", Escape: "escape",
	ArrowLeft: "left", ArrowRight: "right", ArrowUp: "up", ArrowDown: "down",
	Home: "home", End: "end", PageUp: "pageup", PageDown: "pagedown", Insert: "insert",
	" ": "space", Spacebar: "space"
};
for (let i = 1; i <= 12; i++) NAMED["F" + i] = "F" + i;

const MODIFIERS = new Set(["Shift", "Control", "Alt", "Meta", "OS", "CapsLock", "NumLock", "ScrollLock",
	"AltGraph", "Fn", "FnLock", "Hyper", "Super", "ContextMenu"]);

// the US keyboard: the character of a place, without and with Shift
const US = {
	Backquote: "`~", Digit1: "1!", Digit2: "2@", Digit3: "3#", Digit4: "4$", Digit5: "5%",
	Digit6: "6^", Digit7: "7&", Digit8: "8*", Digit9: "9(", Digit0: "0)", Minus: "-_", Equal: "=+",
	BracketLeft: "[{", BracketRight: "]}", Backslash: "\\|", Semicolon: ";:", Quote: "'\"",
	Comma: ",<", Period: ".>", Slash: "/?"
};
for (let i = 0; i < 26; i++) {
	const c = String.fromCharCode(97 + i);
	US["Key" + c.toUpperCase()] = c + c.toUpperCase();
}

function single(s) { return typeof s === "string" && [...s].length === 1 ? s : null; }
function isLetter(c) { return c.toLowerCase() !== c.toUpperCase(); }
function isAscii(c) { return c.charCodeAt(0) < 0x80 && c.length === 1; }

// What is known of the keyboard of the user: the character of each place
// (KeyboardEvent.code), without and with Shift
export function makeLayout() {
	return { base: new Map(), shift: new Map() };
}

// a key which typed text tells what is at its place
export function learn(layout, event) {
	if (event.ctrlKey || event.metaKey || event.altKey || !event.code) return;
	const c = single(event.key);
	if (!c || c === " ") return;
	if (event.shiftKey) layout.shift.set(event.code, c);
	// (Caps Lock gives the capital without Shift)
	else layout.base.set(event.code, isLetter(c) ? c.toLowerCase() : c);
}

// the layout which the browser tells, where it does (the characters without
// Shift)
export async function askLayout(layout) {
	try {
		if (typeof navigator === "undefined" || !navigator.keyboard || !navigator.keyboard.getLayoutMap) return;
		const map = await navigator.keyboard.getLayoutMap();
		for (const [code, c] of map) if (single(c) && !layout.base.has(code)) layout.base.set(code, c);
	} catch (error) {}
}

// The keys which make a command of a character key
export function isCommand(event, mac) {
	// (AltGr, which types characters, is Control with Alt for some browsers)
	if (event.altGraph) return false;
	return event.ctrlKey || event.metaKey || (event.altKey && !mac);
}

// What a key press is: { key: name } to send, { text: true } when its text
// comes from the input of the page, or null (a modifier, a dead key).
// event: key, code, shiftKey, ctrlKey, altKey, metaKey, and altGraph for
// getModifierState ("AltGraph").
export function translate(event, mac, layout) {
	const k = event.key;
	if (!k || MODIFIERS.has(k) || k === "Dead" || k === "Process" || k === "Unidentified") return null;
	const command = isCommand(event, mac);
	let name = NAMED[k], shift = event.shiftKey, alt = event.altKey;
	if (name === undefined) {
		const typed = single(k);
		if (!typed && !(command && US[event.code])) return null; // a key without a name here
		if (!command) return { text: true };
		// the character at the place of the key, without modifiers
		const us = US[event.code];
		let base = layout && layout.base.get(event.code);
		// on a Mac, Option with Command or Control stays a modifier (M-A-x):
		// the character which Option made is not the key
		const optioned = mac && alt;
		if (base === undefined && typed && !shift && !optioned) base = isLetter(typed) ? typed.toLowerCase() : typed;
		if (base === undefined && typed && shift && !optioned && isLetter(typed)) base = typed.toLowerCase();
		if (base === undefined && us) base = us[0];
		if (base === undefined) base = typed;
		// a place which is not Latin (Russian, Greek...): the shortcut is the
		// one of the Latin key which is there on a US keyboard
		if (!isAscii(base) && us) base = us[0];
		name = base;
		if (shift) {
			let shifted;
			if (isLetter(base)) shifted = base.toUpperCase();
			else if (typed && !optioned && typed !== base && isAscii(typed)) shifted = typed;
			else if (layout && layout.shift.has(event.code) && isAscii(layout.shift.get(event.code)))
				shifted = layout.shift.get(event.code);
			else if (us && us[0] === base) shifted = us[1];
			if (shifted !== undefined && shifted !== base) { name = shifted; shift = false; }
		}
		// Command (or Control) with = zooms in as with +, as in browsers: on
		// most keyboards + is Shift with =
		if (name === "=" && !alt && !shift) name = "+";
	}
	else if (name === "space" && !command && !shift) return { text: true };
	if (shift) name = "S-" + name;
	if (event.ctrlKey) name = "C-" + name;
	if (alt) name = "A-" + name;
	if (event.metaKey) name = "M-" + name;
	return { key: name };
}

// The keys which are left to the browser: those which it keeps anyway
// (windows and tabs) and reloading
export function browserKey(event, mac) {
	const k = (event.key || "").toLowerCase();
	const mod = mac ? event.metaKey : event.ctrlKey;
	return mod && !event.altKey && ["r", "l", "t", "w", "n", "q"].includes(k) && !(event.shiftKey && k !== "t" && k !== "n") ||
		k === "f5" || k === "f11" || k === "f12";
}

// The key which pastes: left to the browser, which then gives its clipboard
// in a "paste" event
export function pasteKey(event, mac) {
	const k = (event.key || "").toLowerCase();
	if (k === "insert" && event.shiftKey && !event.ctrlKey && !event.metaKey && !event.altKey) return true;
	return (mac ? event.metaKey && !event.ctrlKey : event.ctrlKey && !event.metaKey) &&
		!event.altKey && !event.shiftKey && k === "v";
}
