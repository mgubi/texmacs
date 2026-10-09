// The worker of Tau: the core of TeXmacs (tau.js, tau.wasm) and the
// messages between it and the page (docs/tau-design.md, "The protocol").
//
// From the page: { t: "place" | "scroll" | "focus" | "key" | "mouse", view, ... },
//                { t: "invoke" | "expand", number }, { t: "file", path }
// To the page:   { t: "ready" | "view" | "paint" | "log" | "status" | "failed", ... },
//                { t: "chrome" | "visible" | "footer" | "contents" | "file", part, ... }

"use strict";

importScripts("tau.js");

let core = null;
const waiting = [];   // the messages which came before the core was ready

function handle(m) {
	switch (m.t) {
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
const sentFiles = new Set();
function collectFiles(items, files) {
	for (const node of items || []) {
		if (node.file && !sentFiles.has(node.file)) {
			sentFiles.add(node.file);
			try { files[node.file] = tauModule.FS.readFile(node.file).buffer; } catch (error) {}
		}
		if (node.items) collectFiles(node.items, files);
	}
}
let tauModule = null;
function post(message, transfer) {
	if (tauModule && (message.t === "chrome" || message.t === "contents")) {
		const files = {};
		collectFiles(message.items, files);
		const buffers = Object.values(files);
		if (buffers.length) { message.files = files; transfer = (transfer || []).concat(buffers); }
	}
	postMessage(message, transfer || []);
}

tauCore({
	arguments: args,
	tauPost: post,
	preRun: [module => { tauModule = module; }],
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
