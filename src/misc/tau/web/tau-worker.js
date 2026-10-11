// The worker of Tau: the core of TeXmacs (tau.js, tau.wasm) and the
// messages between it and the page (docs/tau-design.md, "The protocol").
//
// From the page: { t: "place" | "scroll" | "focus" | "key" | "mouse", view, ... },
//                { t: "text", view, text }, { t: "invoke" | "expand", number }, { t: "file", path },
//                { t: "answer", number, args }, { t: "close", id },
//                { t: "buffer", what, window, name }, { t: "paste", view, text, html },
//                { t: "open", ticket, name, bytes }
// To the page:   { t: "ready" | "view" | "paint" | "log" | "status" | "failed", ... },
//                { t: "chrome" | "visible" | "footer" | "contents" | "file", part, ... },
//                { t: "dialog" | "close" | "refresh", part, ... },
//                { t: "buffers" | "clipboard" | "paste-request" | "pick" | "download", ... },
//                { t: "quit" }, { t: "stopped", text }, { t: "progress", what, loaded, total }

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
	const fail = error => postMessage({ t: "failed", text: "Cannot load the program of Tau: " + (error && error.message || error) });
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

// A value of the page as Scheme reads it: the arguments of the command of
// an input are strings, booleans and lists of them
function scheme(value) {
	if (typeof value === "string") return '"' + value.replace(/[\\"]/g, "\\$&") + '"';
	if (typeof value === "number") return String(value);
	if (Array.isArray(value)) return "(list " + value.map(scheme).join(" ") + ")";
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
	case "forget-home": {
		// what is kept in the browser is deleted; the page starts again
		const names = ["/home/tau"];
		Promise.all(names.map(name => new Promise(resolve => {
			try {
				const r = indexedDB.deleteDatabase(name);
				r.onsuccess = r.onerror = r.onblocked = () => resolve();
			} catch (error) { resolve(); }
		}))).then(() => postMessage({ t: "quit" }));
		break;
	}
	case "fullscreen-left":
		core._tau_fullscreen_left();
		break;
	case "scheme":
		// for the tests: a Scheme command, when the page is opened with ?debug
		if (new URLSearchParams(self.location.search).has("debug"))
			core.ccall("tau_scheme", null, ["string"], [m.code]);
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
		const path = dir + "/" + m.name.replace(/[\/\\]/g, "_");
		core.FS.writeFile(path, new Uint8Array(m.bytes));
		core.ccall("tau_file", null, ["number", "string"], [m.ticket || 0, path]);
		break;
	}
	case "place":
		core._tau_place(m.view, m.width, m.height, m.density, m.place);
		break;
	case "scroll":
		core._tau_scroll_by(m.view, m.dx, m.dy);
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
			error => postMessage({ t: "fs", id: m.id, error: String(error && error.message || error) }));
		break;
	case "chosen":
		// the file which was chosen for a question of the core (none: an
		// empty path), or a document to open (no ticket)
		core.ccall("tau_file", null, ["number", "string"], [m.ticket || 0, m.path || ""]);
		break;
	case "mouse":
		core.ccall("tau_mouse", null, ["number", "string", "number", "number", "number"],
			[m.view, m.kind, m.x, m.y, m.mods]);
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
function fsStat(path) { try { return core.FS.stat(path); } catch (error) { return null; } }
function fsIsDir(path) { const st = fsStat(path); return !!st && core.FS.isDir(st.mode); }
function fsTime(st) { const m = st.mtime; return m instanceof Date ? m.getTime() : +m; }
function fsList(dir) {
	return core.FS.readdir(dir).filter(n => n !== "." && n !== "..").map(name => {
		const path = dir + "/" + name, st = core.FS.stat(path);
		return { name, path, dir: core.FS.isDir(st.mode), size: st.size, time: fsTime(st) };
	});
}
// the files under a directory: [{ rel, path }]
function fsWalk(dir, rel, out) {
	for (const e of fsList(dir)) {
		const r = rel ? rel + "/" + e.name : e.name;
		if (e.dir) fsWalk(e.path, r, out); else out.push({ rel: r, path: e.path });
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
	core.FS.writeFile(path, new Uint8Array(bytes));
	// (the time the file had: the lists of TeXmacs sort and compare by it)
	if (time > 0) try { core.FS.utime(path, time, time); } catch (error) {}
}
// files with their bytes, which go to the page without a copy
function fsBytes(files) {
	const value = files.map(f => {
		const data = core.FS.readFile(f.path);
		return { rel: f.rel, bytes: data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength), time: fsTime(core.FS.stat(f.path)) };
	});
	return { value, transfer: value.map(f => f.bytes) };
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
		const from = fsReadable(m.from), to = fsWritable(m.to);
		if (fsStat(to)) fsRemove(to);
		if (fsIsDir(from)) for (const f of fsWalk(from, "", [])) fsWrite(to + "/" + f.rel, core.FS.readFile(f.path), 0);
		else fsWrite(to, core.FS.readFile(from), 0);
		return { value: true };
	}
	case "backup":
		// the whole home directory, without what TeXmacs makes again
		return fsBytes(fsWalk(HOME, "", []).filter(f => !BACKUP_SKIP.some(s => f.rel.startsWith(s))));
	case "restore": {
		// The files of a backup are written over those of the home
		// directory (or in place of all of them), and kept; the core does
		// nothing more (it would save its preferences over the restored
		// ones) and the page starts again
		if (!core.tauHomeKept) throw new Error("this page does not keep the files: restore the backup in the page of Tau which does");
		frozen = true;
		// (an exact copy: what the backup has not goes first)
		if (m.exact) for (const e of fsList(HOME)) fsRemove(e.path);
		for (const f of m.files) {
			if (BACKUP_SKIP.some(s => f.rel.startsWith(s))) continue;
			fsWrite(fsWritable(HOME + "/" + f.rel), f.bytes, f.time);
		}
		await new Promise(resolve => core.tauSaveHome(resolve));
		return { value: m.files.length };
	}
	}
	throw new Error("unknown request: " + m.op);
}

onmessage = event => {
	if (!core) { waiting.push(event.data); return; }
	if (frozen) return;
	try { handle(event.data); }
	catch (error) {
		postMessage({ t: "log", text: "error in " + event.data.t + ": " + (error && error.message || error) });
		console.error(error);
		stopped(error);
	}
};

// The program stopped on an error which it cannot go on after (a trap of
// WebAssembly, the stack of the browser which is too small...): the page
// is told, so that the user does not go on typing into nothing
let dead = false;
function stopped(error) {
	const fatal = error instanceof WebAssembly.RuntimeError || error instanceof RangeError ||
		/RuntimeError|call stack|unreachable|out of bounds|null function|table entry/i.test(String(error && error.message || error));
	if (!fatal || dead) return;
	dead = true;
	postMessage({ t: "stopped", text: String(error && error.message || error) });
}
self.addEventListener("error", event => stopped(event.error || event.message));
self.addEventListener("unhandledrejection", event => stopped(event.reason));

const args = new URLSearchParams(self.location.search).getAll("arg");

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
	tauQuit: () => tauModule.tauSaveHome(() => postMessage({ t: "quit" })),
	print: text => postMessage({ t: "log", text }),
	printErr: text => postMessage({ t: "log", text }),
	setStatus: text => { if (text) postMessage({ t: "status", text }); },
	// (the program and the files are there: TeXmacs starts)
	onRuntimeInitialized: () => postMessage({ t: "progress", what: "starting" })
}).then(module => {
	core = module;
	postMessage({ t: "ready", homeKept: !!module.tauHomeKept });
	for (const m of waiting.splice(0)) handle(m);
}).catch(error => {
	postMessage({ t: "failed", text: String(error && error.message || error) });
	throw error;
});
