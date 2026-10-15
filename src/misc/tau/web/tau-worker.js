// The worker of Tau: the core of TeXmacs (tau.js, tau.wasm) and the
// messages between it and the page (docs/tau-design.md, "The protocol").
//
// From the page: { t: "place" | "scroll" | "focus" | "key" | "mouse", view, ... },
//                { t: "text", view, text }, { t: "invoke" | "expand", number }, { t: "file", path },
//                { t: "answer", number, args }, { t: "close", id },
//                { t: "buffer", what, window, name }, { t: "paste", view, text, html },
//                { t: "open", ticket, name, bytes }
//                { t: "forget-home" }, { t: "fs", id, op, ... }, { t: "ping", id }
// To the page:   { t: "ready" | "view" | "paint" | "log" | "status" | "failed", ... },
//                { t: "chrome" | "visible" | "footer" | "contents" | "file", part, ... },
//                { t: "dialog" | "close" | "refresh", part, ... },
//                { t: "buffers" | "clipboard" | "paste-request" | "pick" | "download", ... },
//                { t: "quit", failed }, { t: "stopped", text }, { t: "progress", what, loaded, total },
//                { t: "storage", text }, { t: "saving", on }, { t: "storage-erased" } (tau-pre.js),
//                { t: "forget-blocked" | "forget-failed", text }

"use strict";

// what the scripts of the worker complain of is told to the page too
const consoleError = console.error.bind(console);
console.error = (...args) => { consoleError(...args); postMessage({ t: "log", text: args.map(String).join(" ") }); };

// The progress of the loading, for the panel of the page (app.mjs): the
// bytes of the files of TeXmacs (misc/wasm/packages.js tells them to
// tmProgress when there is one) and those of the program, counted below
// as the browser compiles them.
self.tmProgress = {
	files: (loaded, total) => postMessage({ t: "progress", what: "files", loaded, total }),
	error: text => postMessage({ t: "failed", text })
};
function instantiateWasm(imports, receive) {
	const done = r => receive(r.instance, r.module);
	const fail = error => {
		postMessage({ t: "failed", text: "Cannot load the program of Tau: " + (error && error.message || error) });
		giveUp();
	};
	fetch("tau.wasm", { credentials: "same-origin" }).then(response => {
		if (!response.ok) throw new Error("tau.wasm: " + response.status);
		// (a compressed answer does not say the size of what comes out of it)
		let total = response.headers.get("content-encoding") ? 0 : Number(response.headers.get("content-length")) || 0;
		if (!WebAssembly.instantiateStreaming || typeof TransformStream === "undefined")
			return response.arrayBuffer().then(bytes => WebAssembly.instantiate(bytes, imports));
		let loaded = 0, told = 0;
		const counted = response.body.pipeThrough(new TransformStream({
			transform(chunk, out) {
				loaded += chunk.length;
				if (loaded - told > 262144) { told = loaded; postMessage({ t: "progress", what: "program", loaded, total: Math.max(total, loaded) }); }
				out.enqueue(chunk);
			},
			flush() { postMessage({ t: "progress", what: "program", loaded, total: loaded }); }
		}));
		return WebAssembly.instantiateStreaming(new Response(counted, { headers: { "Content-Type": "application/wasm" } }), imports);
	}).then(done).catch(fail);
	return {};
}

importScripts("tau.js");

let core = null;
const waiting = [];   // the messages which came before the core was ready
let gaveUp = false;   // the program will not come: the requests are answered so

// The program could not be loaded: the requests of the panel of the files
// which wait (a backup asked from the panel of the failure) are answered
// with that, and so are those which come later, instead of waiting forever
function giveUp() {
	if (gaveUp || core) return;
	gaveUp = true;
	for (const m of waiting.splice(0))
		if (m.t === "fs") postMessage({ t: "fs", id: m.id, error: "the program of Tau did not start" });
		else if (m.t === "ping") postMessage({ t: "pong", id: m.id });
}

// A value of the page as data which the core reads (tau_answer: it is
// read, never evaluated): the arguments of the command of an input are
// strings, numbers, booleans and lists of them
function scheme(value) {
	if (typeof value === "string") return '"' + value.replace(/[\\"]/g, "\\$&") + '"';
	if (typeof value === "number") return Number.isFinite(value) ? String(value) : "0";
	if (Array.isArray(value)) return "(" + value.map(scheme).join(" ") + ")";
	return value ? "#t" : "#f";
}

function handle(m) {
	switch (m.t) {
	case "answer":
		core.ccall("tau_answer", null, ["number", "string"],
			[m.number, (m.args || []).map(scheme).join(" ")]);
		break;
	case "close":
		core._tau_closed(m.id);
		break;
	case "fullscreen-left":
		core._tau_fullscreen_left();
		break;
	case "scheme":
		// for the tests: a Scheme command, when the page is opened with ?debug
		if (new URLSearchParams(self.location.search).has("debug"))
			core.ccall("tau_scheme", null, ["string"], [m.code]);
		break;
	case "memory":
		// for the tests: the size of the memory of the program
		if (new URLSearchParams(self.location.search).has("debug")) postMessage({ t: "memory", text: String(core.HEAPU8.length) });
		break;
	case "fonts-arrived":
		// for the tests: a font which the network brought late (packages.js)
		if (new URLSearchParams(self.location.search).has("debug")) core._vue_web_font_arrived();
		break;
	case "dry":
		// for the benchmark of the drawing (misc/tau/test/render-bench.mjs)
		if (new URLSearchParams(self.location.search).has("debug")) core._tau_dry(m.on ? 1 : 0);
		break;
	case "buffer":
		core.ccall("tau_buffer", null, ["string", "number", "string"], [m.what, m.window, m.name || ""]);
		break;
	case "paste":
		core.ccall("tau_paste", null, ["number", "string", "string"], [m.view, m.text || "", m.html || ""]);
		break;
	case "drop":
		// images which were dropped on a view, and are files now
		core.ccall("tau_drop", null, ["number", "number", "number", "string"], [m.view, m.x, m.y, m.paths.join("\n")]);
		break;
	case "open": {
		// a file for the core, with its bytes: they are put in its file
		// system (among the documents of the user or, for a document which
		// is only looked at, aside) and the core is told where
		const dir = m.temp ? "/tmp/web" : USER;
		core.FS.mkdirTree(dir);
		const name = m.name.replace(/[\/\\\u0000-\u001f\u007f]/g, "_") || "file", bytes = new Uint8Array(m.bytes);
		let path = dir + "/" + name;
		// (another file of the user with this name is not written over: what
		// is pasted is always "image.png"; nor a folder of that name)
		const st = fsStat(path);
		if (st && (core.FS.isDir(st.mode) || (!m.temp && !sameBytes(core.FS.readFile(path), bytes)))) {
			const dot = name.lastIndexOf("."), stem = dot > 0 ? name.slice(0, dot) : name, ext = dot > 0 ? name.slice(dot) : "";
			for (let k = 2; fsStat(path); k++) path = dir + "/" + stem + " " + k + ext;
		}
		core.FS.writeFile(path, bytes);
		core.ccall("tau_file", null, ["number", "string"], [m.ticket || 0, path]);
		break;
	}
	case "place":
		core._tau_place(m.view, m.width, m.height, m.density, m.place);
		break;
	case "scroll":
		core._tau_scroll_by(m.view, m.dx, m.dy, m.x || 0, m.y || 0);
		break;
	case "focus":
		core._tau_focus(m.view, m.focus ? 1 : 0);
		break;
	case "key":
		core.ccall("tau_key", null, ["number", "string"], [m.view, m.key]);
		break;
	case "text":
		core.ccall("tau_text", null, ["number", "string"], [m.view, m.text]);
		break;
	case "invoke":
		core._tau_invoke(m.number);
		break;
	case "expand":
		core._tau_expand(m.number);
		break;
	case "file": {
		// a file of the core for the page (an icon): its bytes, or none
		let bytes = null;
		try { bytes = core.FS.readFile(m.path).buffer; } catch (error) {}
		postMessage({ t: "file", path: m.path, bytes }, bytes ? [bytes] : []);
		break;
	}
	case "fs":
		// a request of the panel of the files (files.mjs), which is answered
		fsRequest(m).then(
			r => postMessage({ t: "fs", id: m.id, value: r.value }, r.transfer || []),
			// (restart: the files are not what they were, the page starts again)
			error => postMessage({ t: "fs", id: m.id, error: String(error && error.message || error), restart: frozen }));
		break;
	case "chosen":
		// the file which was chosen for a question of the core (none: an
		// empty path), or a document to open (no ticket)
		core.ccall("tau_file", null, ["number", "string"], [m.ticket || 0, m.path || ""]);
		break;
	case "mouse":
		// (at: when it happened, which the core is told as an age)
		core.ccall("tau_mouse", null, ["number", "string", "number", "number", "number", "number"],
			[m.view, m.kind, m.x, m.y, m.mods, m.at ? Math.round(Date.now() - m.at) : 0]);
		break;
	}
}

// ---------------------------------------------------------------------------
// The file system, for the panel of the files of the page (files.mjs)
// ---------------------------------------------------------------------------

// The home directory (kept in the browser, tau-pre.js) is the user's: the
// page lists it, and brings files in and out. The files of TeXmacs
// (/texmacs) are only read. Nothing else is reached.
const HOME = "/home/tau", SYS = "/texmacs";
const BACKUP_SKIP = [".TeXmacs/system/tmp/", ".TeXmacs/system/cache/"];
let frozen = false; // a backup was restored: the core is not to write over it

function fsClean(path) {
	const parts = String(path).split("/");
	if (parts[0] !== "" || parts.slice(1).some(x => x === "" || x === "." || x === ".."))
		throw new Error("not a path: " + path);
	return path;
}
const under = (path, top) => path === top || path.startsWith(top + "/");
function fsReadable(path) {
	fsClean(path);
	if (!under(path, HOME) && !under(path, SYS)) throw new Error("not a file of the user or of TeXmacs: " + path);
	return path;
}
function fsWritable(path) {
	fsClean(path);
	if (!path.startsWith(HOME + "/")) throw new Error("this cannot be changed: " + path);
	return path;
}
const fsParent = path => path.slice(0, path.lastIndexOf("/")) || "/";
const fsBase = path => path.slice(path.lastIndexOf("/") + 1);
function sameBytes(a, b) {
	if (a.length !== b.length) return false;
	for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
	return true;
}
function fsStat(path) { try { return core.FS.stat(path); } catch (error) { return null; } }
function fsIsDir(path) { const st = fsStat(path); return !!st && core.FS.isDir(st.mode); }
function fsTime(st) { const m = st.mtime; return m instanceof Date ? m.getTime() : +m; }
function fsList(dir) {
	return core.FS.readdir(dir).filter(n => n !== "." && n !== "..").map(name => {
		const path = dir + "/" + name, st = core.FS.stat(path);
		return { name, path, dir: core.FS.isDir(st.mode), size: st.size, time: fsTime(st) };
	});
}
// the files under a directory: [{ rel, path }]; with dirs, the
// directories which are empty too: { rel, path, dir: true }
function fsWalk(dir, rel, out, dirs) {
	for (const e of fsList(dir)) {
		const r = rel ? rel + "/" + e.name : e.name;
		if (!e.dir) out.push({ rel: r, path: e.path });
		else {
			const n = out.length;
			fsWalk(e.path, r, out, dirs);
			if (dirs && out.length === n) out.push({ rel: r, path: e.path, dir: true });
		}
	}
	return out;
}
function fsRemove(path) {
	if (fsIsDir(path)) {
		for (const e of fsList(path)) fsRemove(e.path);
		core.FS.rmdir(path);
	}
	else core.FS.unlink(path);
}
function fsWrite(path, bytes, time) {
	core.FS.mkdirTree(fsParent(path));
	// (a file which is written over gets a time after the one it had, even
	// when what comes says an older one: the browser keeps a file again
	// only when its time changed, and different bytes with the same time
	// were never written)
	const before = fsStat(path);
	core.FS.writeFile(path, new Uint8Array(bytes));
	// (the time the file had: the lists of TeXmacs sort and compare by it)
	if (time > 0) {
		if (before && !core.FS.isDir(before.mode)) time = Math.max(time, fsTime(before) + 1);
		try { core.FS.utime(path, time, time); } catch (error) {}
	}
}
// files with their bytes, which go to the page without a copy
function fsBytes(files) {
	const value = files.map(f => {
		if (f.dir) return { rel: f.rel, dir: true, bytes: new ArrayBuffer(0), time: fsTime(core.FS.stat(f.path)) };
		const data = core.FS.readFile(f.path);
		return { rel: f.rel, bytes: data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength), time: fsTime(core.FS.stat(f.path)) };
	});
	return { value, transfer: value.map(f => f.bytes) };
}

const backupSkips = f => BACKUP_SKIP.some(s => (f.rel + "/").startsWith(s));
// the files of a backup can be written: or an error, before anything is
function restoreCheck(files, exact) {
	const kinds = new Map(); // what the backup makes of a path: "file" or "dir"
	for (const f of files) {
		fsWritable(HOME + "/" + f.rel);
		const parts = f.rel.split("/"), kind = f.dir ? "dir" : "file";
		for (let i = 1; i < parts.length; i++) {
			const d = parts.slice(0, i).join("/");
			if (kinds.get(d) === "file") throw new Error("the backup has " + d + " both as a file and as a folder");
			kinds.set(d, "dir");
		}
		if (kinds.has(f.rel) && kinds.get(f.rel) !== kind) throw new Error("the backup has " + f.rel + " both as a file and as a folder");
		kinds.set(f.rel, kind);
	}
	if (exact) return;
	for (const [rel, kind] of kinds) {
		const st = fsStat(HOME + "/" + rel);
		if (st && core.FS.isDir(st.mode) !== (kind === "dir"))
			throw new Error(rel + " is a " + (kind === "dir" ? "file" : "folder") + " here and a " + (kind === "dir" ? "folder" : "file") +
				" in the backup: rename or delete it first, or replace everything");
	}
}

async function fsRequest(m) {
	if (frozen) throw new Error("Tau starts again");
	switch (m.op) {
	case "list": {
		// (the two directories of the user are always there)
		for (const d of [HOME + "/Documents", HOME + "/.TeXmacs"]) if (!fsStat(d)) core.FS.mkdirTree(d);
		let dir = fsReadable(m.dir);
		if (!fsIsDir(dir)) dir = HOME + "/Documents";
		return { value: { dir, entries: fsList(dir), kept: !!core.tauHomeKept } };
	}
	case "read": {
		// a file, or all the files of a directory (under its name)
		const path = fsReadable(m.path);
		if (!fsIsDir(path)) return fsBytes([{ rel: fsBase(path), path }]);
		return fsBytes(fsWalk(path, fsBase(path), []));
	}
	case "write":
		for (const f of m.files) fsWrite(fsWritable(f.path), f.bytes, f.time);
		return { value: m.files.length };
	case "mkdir":
		core.FS.mkdirTree(fsWritable(m.path));
		return { value: true };
	case "rename":
		if (fsStat(fsWritable(m.to))) throw new Error(fsBase(m.to) + " exists already");
		core.FS.rename(fsWritable(m.from), m.to);
		return { value: true };
	case "remove":
		fsRemove(fsWritable(m.path));
		return { value: true };
	case "copy": {
		// a file or a directory (of TeXmacs) into the home directory
		// A directory is copied into the one which is there: the files of
		// the same name are written over, what the user has besides stays
		// (~/.TeXmacs/progs is not emptied for a copy of the one of TeXmacs)
		const from = fsReadable(m.from), to = fsWritable(m.to);
		const files = fsIsDir(from) ? fsWalk(from, "", []).map(f => ({ from: f.path, to: to + "/" + f.rel })) : [{ from, to }];
		if (fsStat(to) && fsIsDir(to) !== fsIsDir(from))
			throw new Error(fsBase(to) + " is a " + (fsIsDir(to) ? "folder" : "file") + " in your folder: rename or delete it first");
		// (nothing is written when a file would take the place of a folder)
		for (const f of files) {
			if (fsIsDir(f.to)) throw new Error(f.to.slice(HOME.length + 1) + " is a folder in your folder: rename or delete it first");
			for (let d = fsParent(f.to); d.length > to.length; d = fsParent(d))
				if (fsStat(d) && !fsIsDir(d)) throw new Error(d.slice(HOME.length + 1) + " is a file in your folder: rename or delete it first");
		}
		if (fsIsDir(from)) core.FS.mkdirTree(to);
		for (const f of files) fsWrite(f.to, core.FS.readFile(f.from), 0);
		return { value: files.length };
	}
	case "backup":
		// the whole home directory, without what TeXmacs makes again
		// (with the folders which are empty)
		return fsBytes(fsWalk(HOME, "", [], true).filter(f => !backupSkips(f)));
	case "restore": {
		// The files of a backup are written over those of the home
		// directory (or in place of all of them), and kept; the core does
		// nothing more (it would save its preferences over the restored
		// ones) and the page starts again
		if (!core.tauHomeKept) throw new Error("this page does not keep the files: restore the backup in the page of Tau which does");
		// Nothing is touched before all of it is known to fit: a path which
		// is not one of the home directory, a file where there is a folder
		const files = m.files.filter(f => !backupSkips(f));
		restoreCheck(files, m.exact);
		// From here on the files are not what the core knows: it stops (its
		// turns too, which would write its preferences over the restored
		// ones), and the page starts again whatever happens
		frozen = true;
		try { core._tau_freeze(); } catch (error) {}
		// (an exact copy: what the backup has not goes first)
		if (m.exact) for (const e of fsList(HOME)) fsRemove(e.path);
		for (const f of files) {
			if (f.dir) core.FS.mkdirTree(HOME + "/" + f.rel);
			else fsWrite(HOME + "/" + f.rel, f.bytes, f.time);
		}
		const failed = await new Promise(resolve => core.tauSaveHome(resolve));
		if (failed) throw new Error("the browser did not store the files (" + (failed.message || failed) + ")");
		return { value: files.length };
	}
	}
	throw new Error("unknown request: " + m.op);
}

// What is kept in the browser is deleted; the page starts again once it
// is. Another page of Tau which keeps it lets it go (tau-pre.js); one which
// does not (an older one) holds the deletion until it is closed: the page
// says so, and waits.
function forgetHome() {
	const names = ["/home/tau"];
	// (this worker lets the browser go too, without saying it was deleted)
	self.tauForgetting = true;
	Promise.all(names.map(name => new Promise((resolve, reject) => {
		try {
			const r = indexedDB.deleteDatabase(name);
			r.onsuccess = () => resolve();
			r.onerror = () => reject(r.error || new Error("the browser refused"));
			r.onblocked = () => postMessage({ t: "forget-blocked" });
		} catch (error) { reject(error); }
	}))).then(() => postMessage({ t: "quit" }),
		error => { self.tauForgetting = false; postMessage({ t: "forget-failed", text: String(error && error.message || error) }); });
}

onmessage = event => {
	// (what is kept in the browser is forgotten without the program, which
	// may be what cannot start with it)
	if (event.data.t === "forget-home") { forgetHome(); return; }
	// (the page asks whether the worker is there, and is done with what it
	// said before: it is answered after those, as the core on the desktop
	// does. Before the program is ready they wait, the ping with them)
	if (event.data.t === "ping" && (core || gaveUp)) { postMessage({ t: "pong", id: event.data.id }); return; }
	if (!core) {
		if (!gaveUp) waiting.push(event.data);
		else if (event.data.t === "fs") postMessage({ t: "fs", id: event.data.id, error: "the program of Tau did not start" });
		return;
	}
	// (after a backup was restored the page starts again: a request for the
	// files is answered, with that, and the rest is not listened to)
	if (frozen && event.data.t !== "fs") return;
	// (a program which stopped is not called again; its files are still
	// read, for a backup of them)
	if (dead && event.data.t !== "fs") return;
	dispatch(event.data);
};

// a message for the core: an error in it is said, and stops the program
// only when it cannot go on
function dispatch(m) {
	try { handle(m); }
	catch (error) {
		postMessage({ t: "log", text: "error in " + m.t + ": " + (error && error.message || error) });
		console.error(error);
		stopped(error);
	}
}

// The program stopped on an error which it cannot go on after (a trap of
// WebAssembly, the stack of the browser which is too small...): the page
// is told, so that the user does not go on typing into nothing
let dead = false;
// what the program printed last, for the report of a stop
const printed = [];
function lastLines(text) {
	printed.push(String(text).slice(0, 300));
	if (printed.length > 25) printed.shift();
}
function stopped(error) {
	const fatal = error instanceof WebAssembly.RuntimeError || error instanceof RangeError ||
		/RuntimeError|Aborted|OOM|call stack|unreachable|out of bounds|null function|table entry/i.test(String(error && error.message || error));
	if (!fatal || dead) return;
	dead = true;
	// (with where it happened: the functions of the program by their numbers,
	// which stage/tau.js.symbols names, misc/tau/Makefile)
	postMessage({ t: "stopped", text: String(error && error.message || error),
		stack: String(error && error.stack || "").split("\n").slice(0, 40).join("\n") +
			(printed.length ? "\n\nprinted last:\n" + printed.join("\n") : "") });
}
self.addEventListener("error", event => stopped(event.error || event.message));
self.addEventListener("unhandledrejection", event => stopped(event.reason));

// The address of the page names documents to open (?arg=<path>), and
// nothing else: an option of TeXmacs would run what a link says ("-x" a
// command of Scheme, "-i" a file of them), with the documents of the user
// at hand. A document is one which is kept or a file of TeXmacs, as for
// ?file=.
const args = new URLSearchParams(self.location.search).getAll("arg").filter(a =>
	/^\/(texmacs|home\/tau)\//.test(a) && !a.split("/").includes("..") && !/[\0-\x1f]/.test(a));

// The icons of a description go with it: the files which its nodes name
// and which the page has not got yet are read here and added to the
// message (files: path -> bytes), so that the bars come with their icons
// and not before them.
const USER = "/home/tau/Documents";  // where the files of the user are, in the core

const sentFiles = new Set();
function collectFiles(node, files) {
	if (Array.isArray(node)) { for (const x of node) collectFiles(x, files); return; }
	if (!node || typeof node !== "object") return;
	if (node.file && !sentFiles.has(node.file)) {
		sentFiles.add(node.file);
		try { files[node.file] = tauModule.FS.readFile(node.file).buffer; } catch (error) {}
	}
	for (const key in node) if (typeof node[key] === "object") collectFiles(node[key], files);
}
const DESCRIPTIONS = new Set(["chrome", "contents", "dialog", "refresh", "popup"]);
let tauModule = null;
function post(message, transfer) {
	// what Scheme says at the end of a turn comes together
	if (message.t === "batch") { for (const m of message.msgs) post(m); return; }
	if (tauModule && DESCRIPTIONS.has(message.t)) {
		const files = {};
		collectFiles(message, files);
		const buffers = Object.values(files);
		if (buffers.length) { message.files = files; transfer = (transfer || []).concat(buffers); }
	}
	if (tauModule && (message.t === "download" || message.t === "open-pdf")) {
		// a file for the user: its bytes go with the message
		try {
			message.bytes = tauModule.FS.readFile(message.path).buffer;
			transfer = [message.bytes];
		} catch (error) { message.bytes = null; }
	}
	postMessage(message, transfer || []);
}

tauCore({
	arguments: args,
	tauPost: post,
	instantiateWasm,
	preRun: [module => { tauModule = module; }],
	// TeXmacs quits: what it wrote is kept first, then the page is told
	// (and when it could not be: the page does not start again, which would
	// lose it)
	tauQuit: () => tauModule.tauSaveHome(error =>
		postMessage({ t: "quit", failed: error ? String(error && error.message || error) : "" })),
	print: text => { lastLines(text); postMessage({ t: "log", text }); },
	printErr: text => { lastLines(text); postMessage({ t: "log", text }); },
	setStatus: text => { if (text) postMessage({ t: "status", text }); },
	// (the program and the files are there: TeXmacs starts)
	onRuntimeInitialized: () => postMessage({ t: "progress", what: "starting" })
}).then(module => {
	core = module;
	postMessage({ t: "ready", homeKept: !!module.tauHomeKept, homeError: module.tauHomeError || "" });
	// (each as onmessage does it: one which fails does not make the start
	// look failed, nor drop the others)
	for (const m of waiting.splice(0)) {
		if (m.t === "ping") { postMessage({ t: "pong", id: m.id }); continue; }
		if (dead && m.t !== "fs") continue;
		dispatch(m);
	}
}).catch(error => {
	postMessage({ t: "failed", text: String(error && error.message || error) });
	// The program did not start, but its files may be there (the home kept
	// in the browser was read before it): the requests for them which
	// waited, a backup, are answered; the others are dropped
	if (tauModule && tauModule.FS) {
		core = tauModule;
		dead = true;
		for (const m of waiting.splice(0))
			if (m.t === "fs") { try { handle(m); } catch (e) {} }
			else if (m.t === "ping") postMessage({ t: "pong", id: m.id });
	}
	else giveUp();
	throw error;
});
