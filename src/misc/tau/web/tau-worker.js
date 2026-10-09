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
	case "open": {
		// a file of the user: its bytes are put in the file system of the
		// core, which is told where
		const path = USER + "/" + m.name.replace(/[\/\\]/g, "_");
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
	case "mouse":
		core.ccall("tau_mouse", null, ["number", "string", "number", "number", "number"],
			[m.view, m.kind, m.x, m.y, m.mods]);
		break;
	}
}

onmessage = event => {
	if (!core) { waiting.push(event.data); return; }
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
