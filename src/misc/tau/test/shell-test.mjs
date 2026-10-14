// The desktop program of Tau (src/Tau/tau_shell.cpp) with its core
// (src/Tau/tau_native.cpp): what the two processes do together, and when
// one of them fails. Each test starts the shell with a script of Vue
// (TEXMACS_VUE_SCRIPT: the keys, the clicks, the waits) and a command of
// Scheme for the core (TAU_SHELL_EVAL), and looks at what is left: a file
// which the core wrote, the log of both programs, the processes.
//
//   node misc/tau/test/shell-test.mjs --shell build-tau-shell/tau_shell
//        --core build-tau-native/tau_core --texmacs TeXmacs [--only name]
//        [--keep]
//
// (misc/tau/shell/Makefile: make shell-check.) The home directory is made
// for the run and thrown away after it (--keep keeps it). A window opens
// for each test: the tests use the display.

import { spawn, execFileSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";

const arg = name => { const i = process.argv.indexOf("--" + name); return i < 0 ? null : process.argv[i + 1]; };
const shell = path.resolve(arg("shell") || "build-tau-shell/tau_shell");
const core = path.resolve(arg("core") || "build-tau-native/tau_core");
const texmacs = path.resolve(arg("texmacs") || "TeXmacs");
const only = arg("only");
const keep = process.argv.includes("--keep");

const root = fs.mkdtempSync(path.join(os.tmpdir(), "tau-shell-test-"));
const home = path.join(root, "home");
fs.mkdirSync(path.join(home, "system"), { recursive: true });
const log = path.join(home, "system", "tau.log");

const sleep = ms => new Promise(r => setTimeout(r, ms));
class Failure extends Error {}
function check(what, ok, got) {
	if (!ok) throw new Failure(what + (got === undefined ? "" : ": " + String(got).slice(0, 300)));
}

// what the log says since a test started
let logStart = 0;
const said = () => { try { return fs.readFileSync(log, "utf8").slice(logStart); } catch (e) { return ""; } };
const alive = pid => { try { process.kill(pid, 0); return true; } catch (e) { return false; } };
const children = pid => {
	try { return execFileSync("pgrep", ["-P", String(pid)], { encoding: "utf8" }).split(/\s+/).filter(Boolean).map(Number); }
	catch (e) { return []; }
};

async function until(what, test, timeout = 30000) {
	const end = Date.now() + timeout;
	for (;;) {
		let r = false;
		try { r = await test(); } catch (e) {}
		if (r) return r;
		if (Date.now() > end) throw new Failure("waited in vain: " + what);
		await sleep(200);
	}
}

// A script of Vue: waits of 300 ms (the script does not turn the loop of
// Vue during a long wait), then what is given
function script(lines, waitFirst = 30) {
	const all = [];
	for (let i = 0; i < waitFirst; i++) all.push("wait 300");
	for (const l of lines) {
		if (typeof l === "number") for (let i = 0; i < l; i++) all.push("wait 300");
		else all.push(l);
	}
	for (let i = 0; i < 100; i++) all.push("wait 300");
	const file = path.join(root, "script-" + Date.now() + ".txt");
	fs.writeFileSync(file, all.join("\n") + "\n");
	return file;
}

// the shell, started for a test
function start({ lines = [], scheme = null, coreProgram = core, debug = false, waitFirst } = {}) {
	logStart = fs.existsSync(log) ? fs.statSync(log).size : 0;
	const env = { ...process.env, TEXMACS_PATH: texmacs, TEXMACS_HOME_PATH: home, TAU_CORE: coreProgram,
		TEXMACS_VUE_SCRIPT: script(lines, waitFirst) };
	delete env.TAU_SHELL_EVAL;
	if (scheme) env.TAU_SHELL_EVAL = scheme;
	if (debug) env.TAU_SHELL_DEBUG = "1";
	// (no terminal: both programs print to the log of the home directory)
	const child = spawn(shell, [], { env, stdio: ["ignore", "ignore", "ignore"], detached: true });
	let exit = null;
	child.on("exit", (code, signal) => { exit = { code, signal }; });
	return {
		pid: child.pid,
		exited: () => exit,
		core: () => children(child.pid)[0],
		stop() {
			for (const c of children(child.pid)) { try { process.kill(c, "SIGKILL"); } catch (e) {} }
			try { process.kill(child.pid, "SIGKILL"); } catch (e) {}
		}
	};
}

// a file which the core writes when Scheme asks it, after a pause
const out = name => path.join(root, name);
const saveBody = (file, pause) =>
	`(delayed (:pause ${pause}) (string-save (object->string (tree->stree (buffer-get-body (current-buffer)))) (system->url "${file}")))`;
const read = file => { try { return fs.readFileSync(file, "latin1"); } catch (e) { return null; } };

const tests = [];
const test = (name, run) => tests.push({ name, run });

test("a quit ends both programs", async () => {
	// (the first test also sets the home directory up: it may take long)
	const s = start({ scheme: "(delayed (:pause 3000) (quit-TeXmacs))", waitFirst: 0 });
	try {
		await until("the shell ends", () => s.exited(), 240000);
		check("without an error", s.exited().code === 0, JSON.stringify(s.exited()));
		check("the core ended well", said().includes("the core stopped") && !said().includes("unexpectedly"), said().slice(-300));
	} finally { s.stop(); }
});

test("typing", async () => {
	const file = out("typed.scm");
	const s = start({ scheme: `(begin (new-document) ${saveBody(file, 14000)})`,
		lines: ["click 500 400", "text Typed \\ here"] });
	try {
		await until("the document is written", () => read(file), 40000);
		check("the text with its backslash", /Typed/.test(read(file)) && /here/.test(read(file)), read(file));
	} finally { s.stop(); }
});

test("a new pane takes the keyboard", async () => {
	const file = out("pane.scm");
	const s = start({ scheme: `(begin (new-document*) ${saveBody(file, 14000)})`,
		lines: ["text In the new pane"] });
	try {
		await until("the document is written", () => read(file), 40000);
		check("what is typed goes to the new pane", read(file).includes("In the new pane"), read(file));
	} finally { s.stop(); }
});

test("a dialog: typed, then Ok", async () => {
	const file = out("answer.txt");
	// (Ok: the dialog of the question is the window of the script)
	const s = start({ scheme: `(interactive (lambda (x) (string-save x (system->url "${file}"))) "Text to insert")`,
		lines: ["text Answer \"q\" (x)", 1, "click 426 48"], waitFirst: 35 });
	try {
		await until("the answer is written", () => read(file) !== null, 40000);
		check("the value which was typed", read(file) === 'Answer "q" (x)', read(file));
	} finally { s.stop(); }
});

test("a dialog: typed, then return", async () => {
	// (return in a field of a dialog which asks for values presses Ok, as
	// in the page)
	const file = out("returned.txt");
	const s = start({ scheme: `(interactive (lambda (x) (string-save x (system->url "${file}"))) "Text to insert")`,
		lines: ["text By return", "key return"], waitFirst: 35 });
	try {
		await until("the answer is written", () => read(file) !== null, 40000);
		check("the value which was typed", read(file) === "By return", read(file));
	} finally { s.stop(); }
});

test("answers which are not data", async () => {
	// to the core itself: an answer is read, never evaluated
	const mark = out("pwned-answer");
	const child = spawn(core, [], { env: { ...process.env, TEXMACS_PATH: texmacs, TEXMACS_HOME_PATH: home, TAU_DESKTOP: "1" },
		stdio: ["pipe", "ignore", "pipe"] });
	let err = "";
	child.stderr.on("data", d => { err += d; });
	try {
		await sleep(9000);
		child.stdin.write(`answer 5 (system "touch ${mark}")\nanswer 6 #.(system "touch ${mark}")\nanswer 7 "ok" (list "a" #t 3) ()\n`);
		await until("the core says what it refused", () => (err.match(/not data/g) || []).length >= 2, 20000);
		check("nothing was run", !fs.existsSync(mark));
		check("data is taken", (err.match(/not data/g) || []).length === 2, err.slice(-400));
	} finally { child.kill("SIGKILL"); }
});

test("a document runs nothing", async () => {
	const mark = out("pwned-doc");
	const doc = out("evil.tm");
	fs.writeFileSync(doc, `<TeXmacs|2.1.5>\n\n<style|generic>\n\n<\\body>\n  <use-module|x (system "touch ${mark}")>\n\n` +
		`  <use-module|(utils)) (system (symbol-\\>string (quote touch))) ((a)>\n\n  <use-module|(doc tmdoc-markup)>\n\n  Hello\n</body>\n`);
	const s = start({ scheme: `(load-buffer (system->url "${doc}"))` });
	try {
		await until("the document is typeset", () => said().includes("Not a plugin"), 40000);
		await sleep(1000);
		check("its Scheme was not run", !fs.existsSync(mark));
		check("both are refused", said().includes("Not a module") && said().includes("Not a plugin"), said().slice(-400));
		check("a module by its name is taken", !said().includes("Not a module: (doc tmdoc-markup)"));
	} finally { s.stop(); }
});

test("the scroll bar", async () => {
	const s = start({ debug: true, lines: ["press 994 180", "move 994 260", 1, "move 994 340", 1, "release 994 340", 6] });
	try {
		await until("the bar is told", () => /> bar \d+ 0 [1-9]/.test(said()), 40000);
		await until("the view scrolls", () => /PANT .* scroll 0,[1-9]/.test(said()), 10000);
	} finally { s.stop(); }
});

test("a menu opened while the core is busy", async () => {
	// the menu opens at once (the window does not wait for the core) and
	// its contents come when the core is free again
	const s = start({ debug: true, scheme: '(delayed (:pause 6000) (system "sleep 8"))',
		lines: [5, "click 61 16", 40], waitFirst: 30 });
	try {
		await until("the menu is asked for", () => /> expand (\d+)/.test(said()), 40000);
		const n = said().match(/> expand (\d+)/)[1];
		await until("the menu is filled when they come", () => said().includes("the menu " + n + " is filled"), 30000);
		check("the core was busy meanwhile", said().includes("the core is busy"), said().slice(-300));
	} finally { s.stop(); }
});

test("the core asked to end saves the documents", async () => {
	// (a document without a name goes to a folder of its own, and is opened
	// again at the next start: tau-files.scm)
	const rescued = path.join(home, "texts", "rescued");
	const kept = () => fs.existsSync(rescued) ? fs.readdirSync(rescued) : [];
	const s = start({ scheme: "(new-document)", lines: ["click 500 500", "text Rescue me"] });
	try {
		await until("the text is typed", () => s.core() && said().includes("vue script: wait"), 30000);
		await sleep(13000);
		const pid = s.core();
		check("a core", pid);
		process.kill(pid, "SIGTERM");
		await until("the core ends", () => !alive(pid), 20000);
		await until("the shell says why", () => said().includes("it was asked to end"), 10000);
		const fresh = kept().filter(f => (read(path.join(rescued, f)) || "").includes("Rescue me"));
		check("the document was saved aside", fresh.length > 0, kept().join(" "));
	} finally { s.stop(); }
	const again = start({});
	try {
		await until("it is opened again at the next start", () => kept().length === 0, 30000);
	} finally { again.stop(); }
});

test("the core which crashes is said", async () => {
	const s = start({});
	try {
		await until("a core", () => s.core(), 30000);
		await sleep(8000);
		process.kill(s.core(), "SIGSEGV");
		await until("the shell says it", () => /the core stopped (on the signal|with the status)/.test(said()), 20000);
		check("the shell waits for the user (its box)", alive(s.pid));
	} finally { s.stop(); }
});

test("a core of another version is not used", async () => {
	const fake = out("fake-core.sh"), bad = out("bad-core.sh");
	fs.writeFileSync(fake, '#!/bin/sh\nprintf "HELO\\001\\000\\000\\0009" >&$TAU_FD\nsleep 30\n', { mode: 0o755 });
	fs.writeFileSync(bad, '#!/bin/sh\nprintf "XXXXYYYYgarbage" >&$TAU_FD\nsleep 30\n', { mode: 0o755 });
	for (const [program, why] of [[fake, "speaks version 9"], [bad, "not a frame of Tau"]]) {
		const s = start({ coreProgram: program, waitFirst: 0 });
		try { await until("the shell refuses it (" + why + ")", () => said().includes(why), 20000); }
		finally { s.stop(); }
	}
});

test("the window of a core which does not answer", async () => {
	const s = start({ lines: ["click 500 500", 30, "close"] });
	try {
		await until("a core", () => s.core(), 30000);
		await sleep(10000);
		const pid = s.core();
		process.kill(pid, "SIGSTOP");
		await until("the shell asks whether to wait", () => said().includes("does not answer"), 30000);
		check("the shell is still there", alive(s.pid));
	} finally { s.stop(); }
});

let failed = 0, ran = 0;
for (const t of tests) {
	if (only && !t.name.includes(only)) continue;
	ran++;
	const begin = Date.now();
	let problem = null;
	try { await t.run(); }
	catch (error) { problem = error instanceof Failure ? error.message : String(error && error.stack || error); }
	const time = ((Date.now() - begin) / 1000).toFixed(1) + " s";
	if (problem) { failed++; console.log(`shell: FAILED ${t.name} (${time}): ${problem}`); }
	else console.log(`shell: ${t.name} (${time})`);
	await sleep(500);
}
if (keep) console.log("shell: the home directory is kept in " + root);
else fs.rmSync(root, { recursive: true, force: true });
console.log(failed ? `shell: ${failed} of ${ran} tests failed` : `shell: ${ran} tests pass`);
process.exit(failed ? 1 : 0);
