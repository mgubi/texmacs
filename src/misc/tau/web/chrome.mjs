// The interface of Tau around the views (step 3 of docs/tau-design.md):
// the menu bar, the icon bars and the footer, made from the descriptions
// which the core sends ("chrome", in the vocabulary of the menu markup of
// TeXmacs: kernel/gui/menu-serial.scm). The contents of a submenu are asked
// when it opens ("expand"), an entry sends the number of its action
// ("invoke").
//
// The dialogs (step 4) are descriptions too ("dialog"), shown in windows of
// the page: their inputs send their values ("answer"), their parts which
// change come again ("refresh"), and the core takes them away ("close")
// after the page said that the user closed them.

let send = () => {};        // to the core
let afterAction = () => {}; // gives the keyboard back to the view
let makeView = () => null;   // the canvas of a view in a dialog (tau.mjs)
let keyName = () => null;    // a key in the notation of TeXmacs (tau.mjs)

const waitingContents = new Map(); // number of a submenu -> resolve
const waitingFiles = new Map();    // path -> { promise, resolve }
const popups = [];                 // the open menus, outermost first
let contextAt = { x: 100, y: 100 }; // where the right button was pressed last

export function init(options) {
	send = options.send;
	afterAction = options.afterAction;
	if (options.makeView) makeView = options.makeView;
	if (options.keyName) keyName = options.keyName;
	document.addEventListener("pointerdown", event => {
		if (!event.target.closest(".tau-popup, .tau-bar")) closePopups(0);
	}, true);
	document.addEventListener("keydown", event => {
		if (event.key === "Escape" && popups.length) { closePopups(0); afterAction(); }
	}, true);
	// where the context menu goes: where the right button was pressed
	document.addEventListener("pointerdown", event => {
		if (event.button === 2) contextAt = { x: event.clientX, y: event.clientY };
	}, true);
	initDialogs();
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
		// the tools at the sides and under the views are laid out as dialogs
		const tool = bar.classList.contains("tau-tool");
		bar.replaceChildren(...renderItems(m.items, tool ? { dialog: true, row: false } : { bar: true }));
		if (!tool) fit(bar);
		return true;
	}
	case "visible": {
		const bar = document.getElementById("tau-" + m.part);
		if (bar) bar.hidden = !m.visible;
		// "the header" of TeXmacs is the menu bar with the icon bars
		if (m.part === "menu") document.body.classList.toggle("tau-no-header", !m.visible);
		return true;
	}
	case "tooltip": showTooltip(m); return true;
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
	case "popup": showContextMenu(m.items); return true;
	case "dialog": showDialog(m); return true;
	case "close": removeDialog(m.id); return true;
	case "refresh": {
		const e = document.querySelector(`[data-refresh="${m.number}"]`);
		if (e) e.replaceChildren(...renderItems(m.items, e.tauContext));
		// (a menu which changed size stays in the window)
		const popup = e && e.closest(".tau-popup");
		if (popup) keepInWindow(popup);
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

function invoke(node, context) {
	closePopups(0);
	send({ t: "invoke", number: node.action });
	// (the keyboard stays in a dialog)
	if (!context || !context.dialog) afterAction();
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
		// a colour, or a pattern (a picture of the core, as an icon)
		const c = el("span", "tau-color");
		if (node.color) c.style.background = node.color;
		if (node.width) c.style.width = Math.round(node.width * 0.75) + "px";
		if (node.height) c.style.height = Math.round(node.height * 0.75) + "px";
		if (node.pattern && node.file) {
			const known = waitingFiles.get(node.file);
			const show = url => { if (url) c.style.background = `url("${url}") center / cover`; };
			if (known && known.url) show(known.url); else iconUrl(node.file).then(show);
		}
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
		if (context.dialog && renderDialogItem(node, context, out)) continue;
		switch (node.kind) {
		case "refreshable":
			// in a menu the part is kept, to be described again while the
			// menu is open (the palette of the colour menus); a bar is
			// described again whole
			if (!context.bar) {
				const e = el("div", "tau-refreshable");
				e.dataset.refresh = node.number;
				e.tauContext = context;
				e.append(...renderItems(node.items, context));
				out.push(e);
				break;
			}
			out.push(...renderItems(node.items, context));
			break;
		case "hlist": case "horizontal":
			// a row of a menu (a text and a list to choose from)
			if (!context.bar && !context.tile) {
				const e = el("div", "tau-h tau-menu-row");
				e.append(...renderItems(node.items, { ...context, row: true }));
				out.push(e);
				break;
			}
			out.push(...renderItems(node.items, context));
			break;
		case "enum":
			// a choice in a menu: it does not close the menu
			out.push(labelled(node, renderEnum(node)));
			break;
		case "vertical": case "vlist":
		case "minibar":
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
	const b = el("button", node.button && context.dialog ? "tau-button" : "tau-entry");
	b.type = "button";
	b.disabled = !node.enabled;
	if (context.bar || context.tile || context.dialog) {
		b.append(...label(node, context));
		if (node.check) b.classList.add("tau-pressed");
		const tip = tooltip(node);
		if (tip) b.title = tip;
	} else {
		b.append(el("span", "tau-check", CHECKS[node.check] || ""), ...label(node, context),
			el("span", "tau-shortcut", node.shortcut || ""));
		if (node.help) b.title = node.help;
	}
	b.addEventListener("click", () => invoke(node, context));
	b.addEventListener("pointerenter", () => { if (!context.bar) closePopups(context.depth); });
	return b;
}

function renderSubmenu(node, context) {
	const b = el("button", "tau-entry tau-submenu");
	b.type = "button";
	b.disabled = !node.enabled;
	if (context.bar || context.tile || context.dialog) {
		b.append(...label(node, context));
		if (node.icon || context.dialog) b.append(el("span", "tau-arrow", "▾"));
		const tip = tooltip(node);
		if (tip) b.title = tip;
	} else {
		b.append(el("span", "tau-check", ""), ...label(node, context), el("span", "tau-shortcut tau-arrow", "▸"));
	}
	const depth = context.bar ? 0 : context.depth || 0;
	// the menu of a bar opens under its button, or at its right when the bar
	// is a column (the icon bars at the left)
	const column = () => document.body.classList.contains("tau-bars-left") && !!b.closest("#tau-icons-0, #tau-icons-1");
	const open = () => openPopup(b, node, depth, (context.bar || context.dialog) && !column());
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
	fit(popup);
	place(popup, button, below);
}

// The context menu of a view, which the core describes when the right
// button is pressed there
function showContextMenu(items) {
	closePopups(0);
	const popup = el("div", "tau-popup");
	popup.append(...renderItems(items, { bar: false, depth: 1 }));
	document.body.append(popup);
	popups.push({ element: popup, button: el("span") });
	popup.style.maxHeight = (window.innerHeight - 8) + "px";
	fit(popup);
	const w = popup.offsetWidth, h = popup.offsetHeight;
	popup.style.left = Math.max(4, Math.min(contextAt.x, window.innerWidth - 4 - w)) + "px";
	popup.style.top = Math.max(4, Math.min(contextAt.y, window.innerHeight - 4 - h)) + "px";
}

function keepInWindow(popup) {
	const r = popup.getBoundingClientRect();
	if (r.right > window.innerWidth - 4) popup.style.left = Math.max(4, window.innerWidth - 4 - r.width) + "px";
	if (r.bottom > window.innerHeight - 4) popup.style.top = Math.max(4, window.innerHeight - 4 - r.height) + "px";
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

// ---------------------------------------------------------------------------
// Dialogs
// ---------------------------------------------------------------------------

const dialogs = new Map(); // number -> element
let dialogTop = 20;        // z-index of the dialog in front
let listCounter = 0;

function initDialogs() {
	document.addEventListener("keydown", event => {
		const d = event.target.closest && event.target.closest(".tau-dialog");
		if (!d) return;
		if (event.key === "Escape" && !popups.length) {
			event.preventDefault();
			send({ t: "close", id: Number(d.dataset.id) });
		}
	});
}

// a length of TeXmacs ("120px", "30em", "1w") for the style sheet, or none
function cssSize(s) {
	const m = /^(-?\d+(?:\.\d+)?)(px|em|ex|w|h)?$/.exec(s || "");
	if (!m) return null;
	if (m[2] === "w" || m[2] === "h") return Math.round(100 * Number(m[1])) + "%";
	return m[1] + (m[2] || "px");
}

function answer(node, ...args) {
	send({ t: "answer", number: node.answer, args });
}

function box(className, items, context) {
	const e = el("div", className);
	e.append(...renderItems(items, context));
	return e;
}

function labelled(node, control) {
	if (!node.label) return control;
	const row = el("label", "tau-setting");
	row.append(el("span", "tau-setting-label", node.label), control);
	return row;
}

// The element of the node of a dialog, added to out: the layouts are boxes
// there, where a bar or a menu takes their items in a row
function renderDialogItem(node, context, out) {
	const row = { ...context, row: true }, column = { ...context, row: false };
	switch (node.kind) {
	case "horizontal": case "hlist": case "minibar": case "class":
		out.push(box("tau-h", node.items, row));
		break;
	case "vertical": case "vlist": case "division":
		out.push(box("tau-v", node.items, column));
		break;
	case "hsplit": out.push(box("tau-h tau-split", node.items, row)); break;
	case "vsplit": out.push(box("tau-v tau-split", node.items, column)); break;
	case "scrollable": out.push(box("tau-v tau-scrollable", node.items, column)); break;
	case "resize": {
		const e = box("tau-v tau-resize", node.items, column);
		const w = cssSize(node.width), h = cssSize(node.height);
		if (w && !w.endsWith("%")) e.style.width = w;
		if (h && !h.endsWith("%")) e.style.height = h;
		out.push(e);
		break;
	}
	case "box": {
		const e = el("fieldset", "tau-box");
		if (node.label) e.append(el("legend", "", node.label));
		e.append(...renderItems(node.items, column));
		out.push(e);
		break;
	}
	case "refreshable": {
		const e = el("div", "tau-refreshable");
		e.dataset.refresh = node.number;
		e.tauContext = context;
		e.append(...renderItems(node.items, context));
		out.push(e);
		break;
	}
	case "glue": {
		const e = el("div", "tau-space");
		if (context.row ? node.hext : node.vext) e.style.flex = "1";
		if (node.width) e.style.minWidth = node.width + "px";
		if (node.height) e.style.minHeight = node.height + "px";
		out.push(e);
		break;
	}
	case "separator":
		out.push(el("div", node.vertical ? "tau-vline" : "tau-hsep"));
		break;
	case "text": out.push(el("span", "tau-dialog-text", node.label || "")); break;
	case "group": out.push(el("div", "tau-dialog-group", node.label || "")); break;
	case "aligned": {
		const grid = el("div", "tau-aligned");
		for (const r of node.rows) {
			grid.append(box("tau-h tau-aligned-left", r.left, row), box("tau-h", r.right, row));
		}
		out.push(grid);
		break;
	}
	case "tabs": out.push(renderTabs(node, context)); break;
	case "view": { const c = makeView(node); if (c) out.push(c); break; }
	case "input": out.push(renderInput(node)); break;
	case "enum": out.push(labelled(node, renderEnum(node))); break;
	case "choice": out.push(renderChoice(node)); break;
	case "toggle": {
		const c = el("input", "tau-toggle");
		c.type = "checkbox";
		c.checked = node.on;
		c.disabled = !node.enabled;
		c.addEventListener("change", () => answer(node, c.checked));
		out.push(labelled(node, c));
		break;
	}
	default:
		if (!node.unsupported) return false;
		out.push(el("span", "tau-unsupported", "[" + node.kind + "]"));
	}
	return true;
}

function proposals(input, values) {
	if (!values || values.length < 2) return [];
	const list = el("datalist");
	list.id = "tau-list-" + (++listCounter);
	for (const v of values) { const o = el("option"); o.value = v; list.append(o); }
	input.setAttribute("list", list.id);
	return [list];
}

function renderInput(node) {
	const wrap = el("span", "tau-input-wrap");
	const input = el("input", "tau-input");
	input.type = node.type === "password" ? "password" : "text";
	input.value = node.value;
	input.disabled = !node.enabled;
	input.spellcheck = false;
	input.autocomplete = "off";
	const w = cssSize(node.width);
	if (w) { wrap.style.width = w; if (w.endsWith("%")) wrap.style.flex = "1"; }
	// the value goes when it is validated or left, as in the other
	// interfaces; return also validates a dialog which asks for values
	if (node.continuous) {
		// the text and the key at each key (the search bar): the keys which
		// do not change the text are told by themselves
		input.addEventListener("input", () => answer(node, [input.value, ""]));
		input.addEventListener("keydown", event => {
			const key = keyName(event);
			if (!key || [...key].length === 1 || ["backspace", "delete", "left", "right", "space"].includes(key)) return;
			event.preventDefault();
			event.stopPropagation();
			answer(node, [input.value, key]);
		});
		wrap.append(input);
		return wrap;
	}
	let sent = node.value;
	const commit = () => { if (input.value !== sent) { sent = input.value; answer(node, sent); } };
	input.addEventListener("change", commit);
	input.addEventListener("keydown", event => {
		if (event.key !== "Enter") return;
		sent = input.value;
		answer(node, sent);
		const d = input.closest(".tau-dialog");
		const buttons = d && d.tauSubmit ? d.querySelectorAll(".tau-button") : [];
		if (buttons.length) buttons[buttons.length - 1].click();
	});
	wrap.append(input, ...proposals(input, node.proposals));
	return wrap;
}

function renderEnum(node) {
	if (node.editable) {
		const input = el("input", "tau-input");
		input.value = node.value;
		input.disabled = !node.enabled;
		const wrap = el("span", "tau-input-wrap");
		const w = cssSize(node.width);
		if (w && !w.endsWith("%")) wrap.style.width = w;
		input.addEventListener("change", () => answer(node, input.value));
		wrap.append(input, ...proposals(input, node.values.concat([""])));
		return wrap;
	}
	const select = el("select", "tau-enum");
	select.disabled = !node.enabled;
	for (const v of node.values) {
		const o = el("option", "", v);
		o.value = v;
		o.selected = v === node.value;
		select.append(o);
	}
	const w = cssSize(node.width);
	if (w && !w.endsWith("%")) select.style.width = w;
	select.addEventListener("change", () => answer(node, select.value));
	return select;
}

// a list of which one or several are chosen, with a filter or without
function renderChoice(node) {
	const select = el("select", "tau-choice");
	select.multiple = node.multiple;
	select.disabled = !node.enabled;
	const fill = filter => {
		const shown = node.values.filter(v => !filter || v.toLowerCase().includes(filter.toLowerCase()));
		select.size = Math.max(2, Math.min(shown.length, 12));
		select.replaceChildren(...shown.map(v => {
			const o = el("option", "", v);
			o.value = v;
			o.selected = node.chosen.includes(v);
			return o;
		}));
	};
	if (node.filter === undefined) {
		fill("");
		select.addEventListener("change", () => {
			const chosen = Array.from(select.selectedOptions, o => o.value);
			answer(node, node.multiple ? chosen : chosen[0] || "");
		});
		return select;
	}
	const e = el("div", "tau-v");
	const input = el("input", "tau-input");
	input.value = node.filter;
	input.placeholder = "Filter";
	fill(node.filter);
	input.addEventListener("input", () => fill(input.value));
	select.addEventListener("change", () => answer(node, select.value, input.value));
	e.append(input, select);
	return e;
}

function renderTabs(node, context) {
	const e = el("div", "tau-tabs"), strip = el("div", "tau-tab-strip"), pages = [];
	e.append(strip);
	node.tabs.forEach((tab, i) => {
		const b = el("button", "tau-tab");
		b.type = "button";
		if (tab.icon) b.append(...label({ icon: tab.icon, file: tab.file }, context));
		b.append(...renderItems(tab.label, { ...context, row: true }));
		const page = box("tau-v tau-tab-page", tab.items, { ...context, row: false });
		page.hidden = i !== 0;
		b.classList.toggle("tau-current", i === 0);
		b.addEventListener("click", () => {
			pages.forEach((p, j) => { p.hidden = j !== i; });
			strip.querySelectorAll(".tau-tab").forEach((t, j) => t.classList.toggle("tau-current", j === i));
		});
		strip.append(b);
		pages.push(page);
		e.append(page);
	});
	return e;
}

function showDialog(m) {
	let d = dialogs.get(m.id);
	if (!d) {
		d = el("div", "tau-dialog");
		d.dataset.id = m.id;
		const bar = el("div", "tau-dialog-title"), title = el("span", "tau-dialog-name");
		const close = el("button", "tau-dialog-close", "×");
		close.type = "button";
		close.title = "Close";
		close.addEventListener("click", () => send({ t: "close", id: m.id }));
		bar.append(title, close);
		d.append(bar, el("div", "tau-dialog-body tau-v"));
		document.body.append(d);
		dialogs.set(m.id, d);
		// each new dialog a bit lower than the one before
		const n = dialogs.size - 1;
		d.style.left = `calc(50% + ${24 * n}px)`;
		d.style.top = `${90 + 24 * n}px`;
		d.addEventListener("pointerdown", () => { d.style.zIndex = ++dialogTop; }, true);
		drag(d, bar);
	}
	d.tauSubmit = !!m.submit;
	d.style.zIndex = ++dialogTop;
	d.querySelector(".tau-dialog-name").textContent = m.title;
	const body = d.querySelector(".tau-dialog-body");
	body.replaceChildren(...renderItems(m.items, { dialog: true, row: false }));
	const first = body.querySelector("input:not([type=checkbox]):not(:disabled)");
	if (first) { first.focus(); first.select(); }
	else { d.tabIndex = -1; d.focus(); }
}

function removeDialog(id) {
	const d = dialogs.get(id);
	if (!d) return;
	const focused = d.contains(document.activeElement);
	dialogs.delete(id);
	d.remove();
	if (focused || !dialogs.size) afterAction();
}

// a dialog is moved by its title
function drag(d, bar) {
	bar.addEventListener("pointerdown", event => {
		if (event.target.closest("button")) return;
		const r = d.getBoundingClientRect(), dx = event.clientX - r.left, dy = event.clientY - r.top;
		d.style.transform = "none";
		const move = e => {
			d.style.left = Math.max(0, Math.min(window.innerWidth - 40, e.clientX - dx)) + "px";
			d.style.top = Math.max(0, Math.min(window.innerHeight - 24, e.clientY - dy)) + "px";
		};
		move(event);
		bar.setPointerCapture(event.pointerId);
		bar.addEventListener("pointermove", move);
		bar.addEventListener("pointerup", () => bar.removeEventListener("pointermove", move), { once: true });
	});
}

// ---------------------------------------------------------------------------
// What does not fit
// ---------------------------------------------------------------------------

// A bar or a menu whose items do not fit scrolls (with the wheel too), and
// shows a chevron at each end where there is more: a click moves by most of
// what is seen, holding it goes on, and in a menu the pointer over it is
// enough. The chevrons are items of no size which stick to the ends; they
// are put back when the items are replaced.
const fitted = new WeakMap(); // container -> its two chevrons

function chevron(c, end) {
	const anchor = el("span", "tau-chev tau-chev-" + end), b = el("button", "tau-chevron");
	b.type = "button";
	b.tabIndex = -1;
	anchor.append(b);
	let timer = null;
	const step = amount => {
		const column = c.classList.contains("tau-fit-column"), sign = end === "prev" ? -1 : 1;
		const d = sign * (amount || 0.7 * (column ? c.clientHeight : c.clientWidth));
		c.scrollBy({ left: column ? 0 : d, top: column ? d : 0, behavior: amount ? "auto" : "smooth" });
	};
	const stop = () => { if (timer) { clearInterval(timer); timer = null; } };
	b.addEventListener("pointerdown", event => {
		event.preventDefault();
		event.stopPropagation();
		step();
		stop();
		timer = setInterval(() => step(12), 30);
	});
	b.addEventListener("pointerup", stop);
	b.addEventListener("pointerleave", stop);
	b.addEventListener("click", event => event.stopPropagation());
	// in a menu the pointer over the chevron scrolls
	b.addEventListener("pointerenter", () => {
		if (!c.classList.contains("tau-popup")) return;
		stop();
		timer = setInterval(() => step(8), 30);
	});
	return anchor;
}

function updateFit(c) {
	const f = fitted.get(c);
	if (!f || !f.prev.isConnected) return;
	const column = getComputedStyle(c).flexDirection === "column";
	c.classList.toggle("tau-fit-column", column);
	const at = column ? c.scrollTop : c.scrollLeft;
	const most = column ? c.scrollHeight - c.clientHeight : c.scrollWidth - c.clientWidth;
	f.prev.classList.toggle("tau-shown", at > 1);
	f.next.classList.toggle("tau-shown", at < most - 1);
	f.prev.firstChild.textContent = column ? "▴" : "‹";
	f.next.firstChild.textContent = column ? "▾" : "›";
}

export function fit(c) {
	let f = fitted.get(c);
	if (!f) {
		f = { prev: chevron(c, "prev"), next: chevron(c, "next") };
		fitted.set(c, f);
		c.addEventListener("scroll", () => updateFit(c), { passive: true });
		new ResizeObserver(() => updateFit(c)).observe(c);
		// the wheel moves a row sideways
		c.addEventListener("wheel", event => {
			if (c.classList.contains("tau-fit-column") || !event.deltaY || event.deltaX) return;
			if (c.scrollWidth <= c.clientWidth) return;
			c.scrollLeft += event.deltaY;
			event.preventDefault();
		}, { passive: false });
	}
	f.prev.remove();
	f.next.remove();
	// (a bar with nothing in it stays empty, and is not shown)
	if (!c.firstElementChild) return;
	c.prepend(f.prev);
	c.append(f.next);
	updateFit(c);
}

// ---------------------------------------------------------------------------
// Tooltips
// ---------------------------------------------------------------------------

// A tooltip of a document (the text of a reference, a note...): a view
// which the core draws, shown over the view which has the keyboard, at a
// place counted from its top left corner. The core takes it away.
const tooltips = new Map(); // id -> element

function showTooltip(m) {
	const old = tooltips.get(m.id);
	if (old) { old.remove(); tooltips.delete(m.id); }
	if (!m.view) return;
	const canvas = makeView({ view: m.view, width: m.width, height: m.height, input: false });
	if (!canvas) return;
	const box = el("div", "tau-tooltip");
	box.append(canvas);
	document.body.append(box);
	tooltips.set(m.id, box);
	const pane = document.querySelector(".tau-pane.tau-active .tau-canvas") || document.querySelector(".tau-canvas");
	const r = pane ? pane.getBoundingClientRect() : { left: 0, top: 0 };
	const x = r.left + m.x, y = r.top + m.y;
	box.style.left = Math.max(2, Math.min(x, window.innerWidth - m.width - 8)) + "px";
	box.style.top = Math.max(2, Math.min(y, window.innerHeight - m.height - 8)) + "px";
}
