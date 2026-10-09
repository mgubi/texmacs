// What the page shows of Tau itself, as the page of TeXmacs Vue does
// (misc/wasm/progress.js and frame.js): a panel while it loads, with its
// icon, a few words about it and how far the loading is; and its icon at
// the top left of the page, a button with a menu about Tau and the page.

const SOURCE = "https://github.com/mgubi/texmacs/tree/wip_tau";

function el(tag, className, text) {
	const e = document.createElement(tag);
	if (className) e.className = className;
	if (text !== undefined) e.textContent = text;
	return e;
}

function link(text, href) {
	const a = el("a", "", text);
	a.href = href; a.target = "_blank"; a.rel = "noopener";
	return a;
}

function head() {
	const h = el("div", "tau-app-head"), img = el("img"), names = el("div");
	img.src = "icon.svg"; img.alt = "";
	const title = el("div", "tau-app-title", "Tau");
	title.append(el("span", "tau-app-badge", "experimental"));
	names.append(title, el("div", "tau-app-sub", "GNU TeXmacs in the browser"));
	h.append(img, names);
	return h;
}

// ---------------------------------------------------------------------------
// The panel of the loading
// ---------------------------------------------------------------------------
//
// The worker tells the bytes of the program and of the files of TeXmacs as
// they come ("progress"); once they are there TeXmacs starts, which takes a
// moment without news. The panel goes when the first view is drawn; an
// error stays on it.

let panel = null, bar = null, fill = null, detail = null;
const loaded = { program: [0, 0], files: [0, 0] };
let t0 = performance.now(), over = false;

function build() {
	if (panel || over) return;
	panel = el("div", "tau-loading");
	const about = el("div", "tau-app-text");
	about.append("The editor of TeXmacs runs in a worker of this page, and its interface is the page itself. " +
		"Nothing is installed, and your documents stay in this browser. Its source is on ", link("GitHub", SOURCE), ".");
	bar = el("div", "tau-loading-bar");
	fill = el("div", "tau-loading-fill");
	bar.append(fill);
	detail = el("div", "tau-loading-detail", "Loading…");
	panel.append(head(), about, bar, detail);
	document.body.append(panel);
}

export function progress(m) {
	if (over) return;
	build();
	if (m.what === "program" || m.what === "files") loaded[m.what] = [m.loaded, m.total];
	const total = loaded.program[1] + loaded.files[1], got = loaded.program[0] + loaded.files[0];
	const coming = m.what !== "starting" && (!total || got < total || !loaded.program[1] || !loaded.files[1]);
	if (coming) {
		bar.classList.toggle("tau-busy", !total);
		const share = total ? got / total : 0;
		fill.style.width = total ? Math.min(100, 100 * share).toFixed(1) + "%" : "";
		const spent = performance.now() - t0;
		const left = share > 0.03 && spent > 500 ? spent * (1 - share) / share : 0;
		detail.textContent = "Downloading" + (total ? ": " + Math.floor(100 * share) + "%" : "…") +
			(left ? ", about " + (left < 60000 ? Math.ceil(left / 1000) + " s" : Math.ceil(left / 60000) + " min") + " left" : "");
	} else {
		bar.classList.add("tau-busy");
		fill.style.width = "";
		detail.textContent = "Starting TeXmacs…";
	}
}

export function started() {
	if (over) return;
	over = true;
	if (!panel) return;
	detail.textContent = "Ready";
	bar.classList.remove("tau-busy");
	fill.style.width = "100%";
	const p = panel;
	panel = null;
	p.classList.add("tau-gone");
	setTimeout(() => p.remove(), 900);
}

export function failed(text) {
	if (over) return;
	build();
	panel.classList.add("tau-failed");
	bar.style.display = "none";
	detail.textContent = text + " Reload the page to try again.";
}

// ---------------------------------------------------------------------------
// The icon at the top left, and its menu
// ---------------------------------------------------------------------------

function human(n) {
	return n > 1 << 20 ? (n / (1 << 20)).toFixed(1) + " MB" : Math.ceil(n / 1024) + " KB";
}

let menu = null;
function closeMenu() {
	if (menu) { menu.remove(); menu = null; }
	const b = document.getElementById("tau-app");
	if (b) b.classList.remove("tau-open");
}

function openMenu(button, options) {
	if (menu) { closeMenu(); return; }
	button.classList.add("tau-open");
	menu = el("div", "tau-app-menu");
	const text = (...parts) => { const d = el("div", "tau-app-text"); d.append(...parts); menu.append(d); return d; };
	const item = (label, run) => {
		const b = el("button", "tau-entry", label);
		b.type = "button";
		b.addEventListener("click", () => { closeMenu(); run(); });
		menu.append(b);
	};
	const sep = () => menu.append(el("div", "tau-hsep"));
	menu.append(head());
	text("An experiment with ", link("GNU TeXmacs", "https://www.texmacs.org"),
		", the structured editor for scientists: its editor and its Scheme run in a worker of this page, " +
		"and the menus, the dialogs and the tabs are the page itself. Expect rough edges.");
	text(link("Sources and notes on GitHub", SOURCE), ".");
	sep();
	const kept = text(options.homeKept() ? "Your documents and preferences are kept in the storage of this browser."
		: options.nothingKept ? "Nothing is kept in this page (?nohome in its address)."
		: "Another page of Tau keeps the documents: what is saved in this one is not kept.");
	if (options.homeKept() && navigator.storage && navigator.storage.estimate)
		navigator.storage.estimate().then(e => {
			if (menu) kept.textContent = "Kept in this browser: your documents and preferences (" + human(e.usage || 0) + " used).";
		}).catch(() => {});
	text("In the address of the page: ?bars=top or left (the icon bars), ?nohome (nothing is kept), " +
		"?trace-keys (the keys in the console), ?arg=… (an option of TeXmacs).");
	sep();
	item("Start Tau again", () => options.reload());
	item("Forget everything kept in this browser…", () => {
		if (!confirm("Delete the documents and the preferences which Tau keeps in this browser?")) return;
		options.forget();
	});
	document.body.append(menu);
	const r = button.getBoundingClientRect();
	menu.style.left = Math.max(4, r.left) + "px";
	menu.style.top = (r.bottom + 2) + "px";
}

export function initApp(options) {
	const button = document.getElementById("tau-app");
	if (!button) return;
	button.addEventListener("click", event => { event.stopPropagation(); openMenu(button, options); });
	document.addEventListener("pointerdown", event => {
		if (menu && !event.target.closest(".tau-app-menu, #tau-app")) closeMenu();
	}, true);
	document.addEventListener("keydown", event => { if (menu && event.key === "Escape") closeMenu(); }, true);
}
