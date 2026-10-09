// The keys of the browser as TeXmacs names them: a table of key presses and
// what misc/tau/web/keys.mjs makes of them. Run by "make check".
//
//   node misc/tau/test/keys-test.mjs

import { translate, makeLayout, learn, browserKey, pasteKey } from "../web/keys.mjs";

// a key press: key, code and the modifiers as letters (s shift, c control,
// a alt, m meta, g AltGr)
function press(key, code, mods = "") {
	return { key, code, shiftKey: mods.includes("s"), ctrlKey: mods.includes("c"),
		altKey: mods.includes("a"), metaKey: mods.includes("m"), altGraph: mods.includes("g") };
}

// a keyboard of which some keys were seen typing
function layoutOf(rows) {
	const layout = makeLayout();
	for (const [code, base, shifted] of rows) {
		learn(layout, press(base, code));
		if (shifted) learn(layout, press(shifted, code, "s"));
	}
	return layout;
}
const none = makeLayout();
const german = layoutOf([["BracketRight", "+", "*"], ["Slash", "-", "_"], ["Digit7", "7", "/"], ["KeyY", "z", "Z"],
	["Minus", "ß", "?"], ["Backslash", "#", "'"]]);
const french = layoutOf([["KeyQ", "a", "A"], ["Digit1", "&", "1"], ["Equal", "=", "+"], ["KeyM", ",", "?"]]);
const russian = layoutOf([["KeyC", "с", "С"], ["KeyV", "м", "М"], ["Equal", "=", "+"]]);

const TEXT = "<text>", NOTHING = "<nothing>";
const cases = [
	// [what, mac, layout, press, expected]
	["a letter", true, none, press("a", "KeyA"), TEXT],
	["a capital", true, none, press("A", "KeyA", "s"), TEXT],
	["space", true, none, press(" ", "Space"), TEXT],
	["shift space", true, none, press(" ", "Space", "s"), "S-space"],
	["option makes a character on a Mac", true, none, press("å", "KeyA", "a"), TEXT],
	["alt is a command elsewhere", false, none, press("a", "KeyA", "a"), "A-a"],
	["AltGr types", false, none, press("@", "KeyQ", "cag"), TEXT],
	["a dead key", true, none, press("Dead", "Quote"), NOTHING],
	["a modifier", true, none, press("Shift", "ShiftLeft", "s"), NOTHING],
	["an input method", true, none, press("Process", "KeyA"), NOTHING],

	["return", true, none, press("Enter", "Enter"), "return"],
	["shift left", true, none, press("ArrowLeft", "ArrowLeft", "s"), "S-left"],
	["command shift left", true, none, press("ArrowLeft", "ArrowLeft", "sm"), "M-S-left"],
	["control alt delete names", false, none, press("Delete", "Delete", "ca"), "A-C-delete"],
	["F5 with shift", false, none, press("F5", "F5", "s"), "S-F5"],
	["control space", false, none, press(" ", "Space", "c"), "C-space"],

	["command c", true, none, press("c", "KeyC", "m"), "M-c"],
	["control c", false, none, press("c", "KeyC", "c"), "C-c"],
	["caps lock does not make a capital", true, none, press("C", "KeyC", "m"), "M-c"],
	["command shift n, the capital given", true, none, press("N", "KeyN", "sm"), "M-N"],
	["command shift n, the capital not given", true, none, press("n", "KeyN", "sm"), "M-N"],
	["control shift z", false, none, press("Z", "KeyZ", "sc"), "C-Z"],

	["command plus, + given", true, none, press("+", "Equal", "sm"), "M-+"],
	["command plus, = given", true, none, press("=", "Equal", "sm"), "M-+"],
	["command =", true, none, press("=", "Equal", "m"), "M-+"],
	["command minus", true, none, press("-", "Minus", "m"), "M--"],
	["command 0", true, none, press("0", "Digit0", "m"), "M-0"],
	["command ?, ? given", true, none, press("?", "Slash", "sm"), "M-?"],
	["command ?, / given", true, none, press("/", "Slash", "sm"), "M-?"],
	["control <", false, none, press("<", "Comma", "sc"), "C-<"],
	["control plus, = given", false, none, press("=", "Equal", "sc"), "C-+"],
	["command ,", true, none, press(",", "Comma", "m"), "M-,"],
	["command ;", true, none, press(";", "Semicolon", "m"), "M-;"],

	["command option s is not ß", true, none, press("ß", "KeyS", "am"), "M-A-s"],
	["command option shift s", true, none, press("Í", "KeyS", "sam"), "M-A-S"],
	["control alt x elsewhere", false, none, press("x", "KeyX", "ca"), "A-C-x"],

	["German: command plus, its own key", true, german, press("+", "BracketRight", "m"), "M-+"],
	["German: command shift plus is *", true, german, press("*", "BracketRight", "sm"), "M-*"],
	["German: the same, the key given without shift", true, german, press("+", "BracketRight", "sm"), "M-*"],
	["German: command minus", true, german, press("-", "Slash", "m"), "M--"],
	["German: command shift 7 is /", true, german, press("7", "Digit7", "sm"), "M-/"],
	["German: command z is where y is", true, german, press("z", "KeyY", "m"), "M-z"],
	["German: command shift ß is ?", true, german, press("ß", "Minus", "sm"), "M-?"],
	["German, nothing typed yet: command plus", true, none, press("+", "BracketRight", "m"), "M-+"],
	["French: command a is where q is", true, french, press("a", "KeyQ", "m"), "M-a"],
	["French: command shift & is 1", true, french, press("&", "Digit1", "sm"), "M-1"],
	["French: command shift , is ?", true, french, press(",", "KeyM", "sm"), "M-?"],
	["Russian: control с is C-c", false, russian, press("с", "KeyC", "c"), "C-c"],
	["Russian: control shift с is C-C", false, russian, press("С", "KeyC", "sc"), "C-C"],
	["Russian, nothing typed yet", false, none, press("м", "KeyV", "c"), "C-v"],
];

let failed = 0;
for (const [what, mac, layout, event, expected] of cases) {
	const r = translate(event, mac, layout);
	const got = r === null ? NOTHING : r.text ? TEXT : r.key;
	if (got !== expected) {
		failed++;
		console.log(`keys: ${what}: ${got}, expected ${expected}`);
	}
}

const checks = [
	["command w is the browser's", browserKey(press("w", "KeyW", "m"), true), true],
	["control w elsewhere", browserKey(press("w", "KeyW", "c"), false), true],
	["control w on a Mac is ours", browserKey(press("w", "KeyW", "c"), true), false],
	["command shift w is ours", browserKey(press("W", "KeyW", "sm"), true), false],
	["command s is ours", browserKey(press("s", "KeyS", "m"), true), false],
	["command v pastes", pasteKey(press("v", "KeyV", "m"), true), true],
	["control v on a Mac does not", pasteKey(press("v", "KeyV", "c"), true), false],
	["shift insert pastes", pasteKey(press("Insert", "Insert", "s"), false), true],
	["command shift v does not", pasteKey(press("V", "KeyV", "sm"), true), false],
];
for (const [what, got, expected] of checks)
	if (got !== expected) { failed++; console.log(`keys: ${what}: ${got}, expected ${expected}`); }

console.log(failed ? `keys: ${failed} of ${cases.length + checks.length} failed`
	: `keys: ${cases.length + checks.length} cases pass`);
process.exit(failed ? 1 : 0);
