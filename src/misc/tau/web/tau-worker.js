// The worker of Tau: the core of TeXmacs (tau.js, tau.wasm) and the
// messages between it and the page (docs/tau-design.md, "The protocol").
//
// From the page: { t: "place" | "scroll" | "focus" | "key" | "mouse", view, ... }
// To the page:   { t: "ready" | "view" | "paint" | "log" | "status" | "failed", ... }

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

tauCore({
	arguments: args,
	tauPost: (message, transfer) => postMessage(message, transfer || []),
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
