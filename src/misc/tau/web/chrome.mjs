// The interface of Tau around the views (step 3 of docs/tau-design.md):
// the menu bar, the icon bars and the footer, made from the descriptions
// which the core sends ("chrome", in the vocabulary of the menu markup of
// TeXmacs: kernel/gui/menu-serial.scm). The contents of a submenu are asked
// when it opens ("expand"), an entry sends the number of its action
// ("invoke").

let send = () => {};        // to the core
let afterAction = () => {}; // gives the keyboard back to the view

const waitingContents = new Map(); // number of a submenu -> resolve
const waitingFiles = new Map();    // path -> { promise, resolve }
const popups = [];                 // the open menus, outermost first

export function init(options) {
	send = options.send;
	afterAction = options.afterAction;
	document.addEventListener("pointerdown", event => {
		if (!event.target.closest(".tau-popup, .tau-bar")) closePopups(0);
	}, true);
	document.addEventListener("keydown", event => {
		if (event.key === "Escape" && popups.length) { closePopups(0); afterAction(); }
	}, true);
}

// The messages of the core which concern the interface
export function handle(m) {
	// the icons which come with a description (tau-worker.js)
	if (m.files) for (const path in m.files) {
		const url = URL.createObjectURL(new Blob([m.files[path]], { type: mime(path) }));
		const w = waitingFiles.get(path);
		if (w) w.resolve(url); else waitingFiles.set(path, { promise: Promise.resolve(url), url });
	}
	switch (m.t) {
	case "chrome": {
		const bar = document.getElementById("tau-" + m.part);
		if (!bar) return true;
		closePopups(0); // the numbers of the old description are forgotten
		bar.replaceChildren(...renderItems(m.items, { bar: true }));
		return true;
	}
	case "visible": {
		const bar = document.getElementById("tau-" + m.part);
		if (bar) bar.hidden = !m.visible;
		return true;
	}
	case "footer": {
		const side = document.getElementById("tau-footer-" + m.part);
		if (side) side.textContent = m.text;
		return true;
	}
	case "contents": {
		const resolve = waitingContents.get(m.part);
		waitingContents.delete(m.part);
		if (resolve) resolve(m.items);
		return true;
	}
	case "file": {
		const w = waitingFiles.get(m.path);
		if (w) w.resolve(m.bytes ? URL.createObjectURL(new Blob([m.bytes], { type: mime(m.path) })) : null);
		return true;
	}
	}
	return false;
}

function mime(path) {
	return path.endsWith(".svg") ? "image/svg+xml" : path.endsWith(".png") ? "image/png" : "application/octet-stream";
}

// An icon: the core found its file, which is in the file system of the
// worker; its bytes are asked once
function iconUrl(path) {
	let w = waitingFiles.get(path);
	if (!w) {
		w = {};
		w.promise = new Promise(resolve => { w.resolve = resolve; });
		waitingFiles.set(path, w);
		send({ t: "file", path });
	}
	return w.promise;
}

function expand(number) {
	return new Promise(resolve => {
		waitingContents.set(String(number), resolve);
		send({ t: "expand", number });
	});
}

function invoke(node) {
	closePopups(0);
	send({ t: "invoke", number: node.action });
	afterAction();
}

function el(tag, className, text) {
	const e = document.createElement(tag);
	if (className) e.className = className;
	if (text !== undefined) e.textContent = text;
	return e;
}

// the label of an entry or of a submenu: an icon, a text, or both
function label(node, context) {
	const parts = [];
	if (node.icon) {
		const img = el("img", "tau-icon");
		img.alt = "";
		img.draggable = false;
		if (node.file) {
			// at once when the file is there already, so that the bar is
			// not drawn without its icons first
			const known = waitingFiles.get(node.file);
			if (known && known.url) img.src = known.url;
			else iconUrl(node.file).then(url => { if (url) img.src = url; });
		}
		parts.push(img);
	}
	if (node.label !== undefined) parts.push(el("span", node.symbol ? "tau-symbol" : "tau-label", node.label));
	if (node.color !== undefined) {
		const c = el("span", "tau-color");
		c.style.background = node.color;
		parts.push(c);
	}
	return parts;
}

function tooltip(node) {
	const parts = [];
	if (node.help) parts.push(node.help);
	else if (node.icon && node.label === undefined) parts.push(node.icon.replace(/^tm_|\.xpm$/g, "").replace(/_/g, " "));
	if (node.shortcut) parts.push("(" + node.shortcut + ")");
	return parts.join(" ");
}

const CHECKS = { v: "✓", o: "●", "*": "●" };

// The elements of a list of nodes, in a bar or in a menu
function renderItems(items, context) {
	const out = [];
	for (const node of items || []) {
		switch (node.kind) {
		case "horizontal": case "vertical": case "hlist": case "vlist":
		case "minibar": case "refreshable":
			out.push(...renderItems(node.items, context));
			break;
		case "tile": {
			const grid = el("div", "tau-tile");
			grid.style.gridTemplateColumns = `repeat(${node.columns || 8}, auto)`;
			grid.append(...renderItems(node.items, { ...context, tile: true }));
			out.push(grid);
			break;
		}
		case "separator":
			if (context.bar) out.push(el("span", "tau-vsep"));
			else if (!node.vertical) out.push(el("div", "tau-hsep"));
			break;
		case "glue":
			if (context.bar && node.hext) out.push(el("span", "tau-glue"));
			break;
		case "group":
			out.push(el(context.bar ? "span" : "div", "tau-group", node.label || ""));
			break;
		case "text":
			out.push(el(context.bar ? "span" : "div", "tau-text", node.label || ""));
			break;
		case "entry": out.push(renderEntry(node, context)); break;
		case "submenu": out.push(renderSubmenu(node, context)); break;
		}
	}
	return out;
}

function renderEntry(node, context) {
	const b = el("button", "tau-entry");
	b.type = "button";
	b.disabled = !node.enabled;
	if (context.bar || context.tile) {
		b.append(...label(node, context));
		if (node.check) b.classList.add("tau-pressed");
		const tip = tooltip(node);
		if (tip) b.title = tip;
	} else {
		b.append(el("span", "tau-check", CHECKS[node.check] || ""), ...label(node, context),
			el("span", "tau-shortcut", node.shortcut || ""));
		if (node.help) b.title = node.help;
	}
	b.addEventListener("click", () => invoke(node));
	b.addEventListener("pointerenter", () => { if (!context.bar) closePopups(context.depth); });
	return b;
}

function renderSubmenu(node, context) {
	const b = el("button", "tau-entry tau-submenu");
	b.type = "button";
	b.disabled = !node.enabled;
	if (context.bar || context.tile) {
		b.append(...label(node, context));
		if (node.icon) b.append(el("span", "tau-arrow", "▾"));
		const tip = tooltip(node);
		if (tip) b.title = tip;
	} else {
		b.append(el("span", "tau-check", ""), ...label(node, context), el("span", "tau-shortcut tau-arrow", "▸"));
	}
	const depth = context.bar ? 0 : context.depth;
	const open = () => openPopup(b, node, depth, context.bar);
	b.addEventListener("click", () => {
		if (b.classList.contains("tau-open")) closePopups(depth); else open();
	});
	// once a menu of a bar is open, the others open under the pointer
	b.addEventListener("pointerenter", () => {
		if (context.bar ? popups.length > 0 && !b.classList.contains("tau-open") : true) {
			if (!b.classList.contains("tau-open")) open();
		}
	});
	return b;
}

function closePopups(depth) {
	while (popups.length > depth) {
		const p = popups.pop();
		p.element.remove();
		p.button.classList.remove("tau-open");
	}
}

async function openPopup(button, node, depth, below) {
	closePopups(depth);
	const popup = el("div", "tau-popup");
	popup.append(el("div", "tau-text", "…"));
	document.body.append(popup);
	button.classList.add("tau-open");
	const entry = { element: popup, button };
	popups.push(entry);
	place(popup, button, below);
	const items = await expand(node.contents);
	if (!popups.includes(entry)) return; // closed meanwhile
	popup.replaceChildren(...renderItems(items, { bar: false, depth: depth + 1 }));
	place(popup, button, below);
}

// a menu under its button (in a bar) or at its right (in a menu), kept in
// the window
function place(popup, button, below) {
	const r = button.getBoundingClientRect();
	let x = below ? r.left : r.right - 2, y = below ? r.bottom : r.top - 4;
	popup.style.maxHeight = (window.innerHeight - 8) + "px";
	const w = popup.offsetWidth, h = popup.offsetHeight;
	if (x + w > window.innerWidth - 4) x = below ? Math.max(4, window.innerWidth - 4 - w) : Math.max(4, r.left - w + 2);
	if (y + h > window.innerHeight - 4) y = Math.max(4, window.innerHeight - 4 - h);
	popup.style.left = x + "px";
	popup.style.top = y + "px";
}
