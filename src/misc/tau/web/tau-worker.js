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
//                { t: "buffers" | "clipboard" | "pick" | "download", ... }

"use strict";

// what the scripts of the worker complain of is told to the page too
const consoleError = console.error.bind(console);
console.error = (...args) => { consoleError(...args); postMessage({ t: "log", text: args.map(String).join(" ") }); };

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
	}
};

const args = new URLSearchParams(self.location.search).getAll("arg");

// The icons of a description go with it: the files which its nodes name
// and which the page has not got yet are read here and added to the
// message (files: path -> bytes), so that the bars come with their icons
// and not before them.
const USER = "/user";  // where the files of the user are, in the core

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
	if (tauModule && message.t === "download") {
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
	preRun: [module => { tauModule = module; module.FS.mkdirTree(USER); }],
	print: text => postMessage({ t: "log", text }),
	printErr: text => postMessage({ t: "log", text }),
	setStatus: text => { if (text) postMessage({ t: "status", text }); }
}).then(module => {
	core = module;
	postMessage({ t: "ready" });
	for (const m of waiting.splice(0)) handle(m);
}).catch(error => {
	postMessage({ t: "failed", text: String(error && error.message || error) });
	throw error;
});
