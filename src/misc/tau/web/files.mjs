// The files kept in the browser, and their panel.
//
// The home directory of the core, /home/tau, is kept in the storage of the
// browser (tau-pre.js). It is in the worker: the page asks the worker for
// what it shows and changes (the requests "fs" of tau-worker.js). This
// module is the panel which manages it, after the one of the browser build
// of TeXmacs (misc/wasm/files.js):
//
// - it shows the folders of the user and brings files in and out: add
//   files, whole folders (a project with its images) or a zip, make
//   folders, rename, delete, save a copy of a file on the computer, or of
//   a folder as a zip; back up the whole home directory in a zip and
//   restore it, to keep it safe or to move it to another browser;
// - the TeXmacs folder of the user (~/.TeXmacs) is a place of its own;
// - so are the files of TeXmacs (/texmacs: its styles, packages, Scheme
//   code, documentation), which are not changed: they are opened, copied to
//   the computer, or copied into the TeXmacs folder of the user
//   ("customize": ~/.TeXmacs at the same place), where TeXmacs looks first;
// - the same panel is where a file is chosen when TeXmacs asks for one:
//   to open (Load, Insert image...) and to save (Save as, Export...).
//
// Everything that opens a chooser of the system is a control the user
// clicks: the browsers open one only for a click being handled.

const HOME = "/home/tau";
const DOCS = HOME + "/Documents";
const USER_TM = HOME + "/.TeXmacs";
const SYS = "/texmacs";
const under = (p, top) => p === top || p.startsWith(top + "/");

let options = null;   // what the page gives: see initFiles
let current = null;   // the open panel: { close }

function el(tag, name, text) {
	const e = document.createElement(tag);
	if (name) e.className = name;
	if (text !== undefined) e.textContent = text;
	return e;
}
function suffix(name) {
	const i = name.lastIndexOf(".");
	return i < 0 ? "" : name.slice(i + 1).toLowerCase();
}
const join = (dir, name) => dir === "/" ? "/" + name : dir + "/" + name;
const base = p => p.slice(p.lastIndexOf("/") + 1);
// a name of a file which is given by the user, or comes with a file
// (no separator, and no control character: the paths go to the core in lines)
const safe = n => n.replace(/[\/\\\u0000-\u001f\u007f]/g, "_");
function humanSize(n) {
	return n < 1024 ? n + " B" : n < 1048576 ? (n / 1024).toFixed(0) + " KB" : (n / 1048576).toFixed(1) + " MB";
}
function humanTime(ms) {
	if (!(ms > 0)) return "";
	const d = new Date(ms), now = new Date(), two = n => (n < 10 ? "0" : "") + n;
	const day = d.getFullYear() + "-" + two(d.getMonth() + 1) + "-" + two(d.getDate());
	return d.toDateString() === now.toDateString() ? two(d.getHours()) + ":" + two(d.getMinutes()) : day;
}
// a name which is not among names: "name", "name 2", ...
function fresh(names, name) {
	if (!names.includes(name)) return name;
	const i = name.lastIndexOf("."), stem = i > 0 ? name.slice(0, i) : name, ext = i > 0 ? name.slice(i) : "";
	for (let k = 2; ; k++) if (!names.includes(stem + " " + k + ext)) return stem + " " + k + ext;
}

// ---------------------------------------------------------------------------
// Zip files (the compression streams of the browser, no library)
// ---------------------------------------------------------------------------

let crcTable = null;
function crc32(bytes) {
	if (!crcTable) {
		crcTable = new Uint32Array(256);
		for (let n = 0; n < 256; n++) {
			let c = n;
			for (let k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
			crcTable[n] = c >>> 0;
		}
	}
	let crc = 0xFFFFFFFF;
	for (let i = 0; i < bytes.length; i++) crc = crcTable[(crc ^ bytes[i]) & 0xFF] ^ (crc >>> 8);
	return (crc ^ 0xFFFFFFFF) >>> 0;
}
// (at most limit bytes come out: more is an error, before it fills the
// memory of the page, as a zip which inflates without end would)
async function transform(bytes, stream, limit) {
	const reader = new Blob([bytes]).stream().pipeThrough(stream).getReader(), parts = [];
	let size = 0;
	for (;;) {
		const r = await reader.read();
		if (r.done) break;
		size += r.value.length;
		if (limit !== undefined && size > limit) { reader.cancel().catch(() => {}); throw new Error("the zip file is too large once unpacked"); }
		parts.push(r.value);
	}
	const out = new Uint8Array(size);
	let at = 0;
	for (const q of parts) { out.set(q, at); at += q.length; }
	return out;
}
// what a zip may unpack to, in all
const UNZIP_MAX = 1 << 30;
// the time of a file (ms) as the two 16-bit fields of a zip (local time,
// 2 s steps), and back
function dosTime(ms) {
	const d = new Date(ms);
	if (!(ms > 0) || d.getFullYear() < 1980) return [0, 0x21]; // 1980-01-01
	return [(d.getHours() << 11) | (d.getMinutes() << 5) | (d.getSeconds() >> 1),
		((d.getFullYear() - 1980) << 9) | ((d.getMonth() + 1) << 5) | d.getDate()];
}
function fromDosTime(t, d) {
	if (!d) return 0;
	return new Date(1980 + (d >> 9), ((d >> 5) & 15) - 1, d & 31, t >> 11, (t >> 5) & 63, (t & 31) * 2).getTime();
}
// files: [{ rel, bytes, time? }] -> a zip file (a Blob); a rel which ends
// with "/" is a folder
export async function zip(files) {
	const canDeflate = typeof CompressionStream !== "undefined";
	const enc = new TextEncoder(), parts = [], central = [];
	let offset = 0;
	for (const file of files) {
		const name = enc.encode(file.rel), data = new Uint8Array(file.bytes);
		const crc = crc32(data);
		let method = 0, comp = data;
		if (canDeflate && data.length > 64) {
			const d = await transform(data, new CompressionStream("deflate-raw"));
			if (d.length < data.length) { comp = d; method = 8; }
		}
		const dt = dosTime(file.time);
		const h = new DataView(new ArrayBuffer(30));
		h.setUint32(0, 0x04034b50, true); h.setUint16(4, 20, true);
		h.setUint16(6, 0x0800, true); h.setUint16(8, method, true);
		h.setUint16(10, dt[0], true); h.setUint16(12, dt[1], true);
		h.setUint32(14, crc, true); h.setUint32(18, comp.length, true);
		h.setUint32(22, data.length, true); h.setUint16(26, name.length, true);
		parts.push(new Uint8Array(h.buffer), name, comp);
		const c = new DataView(new ArrayBuffer(46));
		c.setUint32(0, 0x02014b50, true); c.setUint16(4, 20, true); c.setUint16(6, 20, true);
		c.setUint16(8, 0x0800, true); c.setUint16(10, method, true);
		c.setUint16(12, dt[0], true); c.setUint16(14, dt[1], true);
		c.setUint32(16, crc, true); c.setUint32(20, comp.length, true);
		c.setUint32(24, data.length, true); c.setUint16(28, name.length, true);
		c.setUint32(42, offset, true);
		central.push(new Uint8Array(c.buffer), name);
		offset += 30 + name.length + comp.length;
	}
	const size = central.reduce((s, a) => s + a.length, 0), n = files.length, tail = [];
	if (offset + size > 0xFFFFFFFF) throw new Error("more than 4 GB: too much for a zip made here");
	if (n >= 0xFFFF) {
		// more files than the end record counts: the one of zip64 before
		// it, and where it is
		const z = new DataView(new ArrayBuffer(76));
		z.setUint32(0, 0x06064b50, true); z.setBigUint64(4, 44n, true);
		z.setUint16(12, 45, true); z.setUint16(14, 45, true);
		z.setBigUint64(24, BigInt(n), true); z.setBigUint64(32, BigInt(n), true);
		z.setBigUint64(40, BigInt(size), true); z.setBigUint64(48, BigInt(offset), true);
		z.setUint32(56, 0x07064b50, true); z.setBigUint64(64, BigInt(offset + size), true);
		z.setUint32(72, 1, true);
		tail.push(new Uint8Array(z.buffer));
	}
	const e = new DataView(new ArrayBuffer(22));
	e.setUint32(0, 0x06054b50, true); e.setUint16(8, Math.min(n, 0xFFFF), true);
	e.setUint16(10, Math.min(n, 0xFFFF), true); e.setUint32(12, size, true);
	e.setUint32(16, offset, true);
	tail.push(new Uint8Array(e.buffer));
	return new Blob(parts.concat(central, tail), { type: "application/zip" });
}
// the bytes of a zip file -> [{ rel, bytes, time }] (stored and deflated
// entries); with dirs, its folders too: { rel, dir: true, ... }. An entry
// whose bytes are not those which were packed is an error
export async function unzip(bytes, dirs) {
	const v = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
	let end = -1;
	for (let i = bytes.length - 22; i >= Math.max(0, bytes.length - 65557); i--)
		if (v.getUint32(i, true) === 0x06054b50) { end = i; break; }
	if (end < 0) throw new Error("not a zip file");
	const dec = new TextDecoder(), out = [];
	let unpacked = 0;
	let n = v.getUint16(end + 10, true), p = v.getUint32(end + 16, true);
	if ((n === 0xFFFF || p === 0xFFFFFFFF) && end >= 20 && v.getUint32(end - 20, true) === 0x07064b50) {
		// (zip64: the count and the place of the directory are in its record)
		const z = Number(v.getBigUint64(end - 12, true));
		if (z + 56 > bytes.length || v.getUint32(z, true) !== 0x06064b50) throw new Error("the zip file is damaged");
		n = Number(v.getBigUint64(z + 32, true));
		p = Number(v.getBigUint64(z + 48, true));
	}
	for (let k = 0; k < n; k++) {
		if (p + 46 > bytes.length || v.getUint32(p, true) !== 0x02014b50) throw new Error("the zip file is damaged");
		const method = v.getUint16(p + 10, true), csize = v.getUint32(p + 20, true), crc = v.getUint32(p + 16, true);
		const time = fromDosTime(v.getUint16(p + 12, true), v.getUint16(p + 14, true));
		const nlen = v.getUint16(p + 28, true), xlen = v.getUint16(p + 30, true);
		const clen = v.getUint16(p + 32, true), local = v.getUint32(p + 42, true);
		const name = dec.decode(bytes.subarray(p + 46, p + 46 + nlen));
		p += 46 + nlen + xlen + clen;
		if (name.startsWith("__MACOSX/")) continue;
		if (name.endsWith("/")) {
			if (dirs) out.push({ rel: name.slice(0, -1), dir: true, bytes: new Uint8Array(0), time });
			continue;
		}
		const start = local + 30 + v.getUint16(local + 26, true) + v.getUint16(local + 28, true);
		let data = bytes.subarray(start, start + csize);
		if (method === 8) data = await transform(data, new DecompressionStream("deflate-raw"), UNZIP_MAX - unpacked);
		else if (method !== 0) continue; // another compression: skipped
		unpacked += data.length;
		if (unpacked > UNZIP_MAX) throw new Error("the zip file is too large once unpacked");
		if (crc32(data) !== crc) throw new Error("the zip file is damaged (" + name + ")");
		out.push({ rel: name, bytes: new Uint8Array(data), time });
	}
	return out;
}

// ---------------------------------------------------------------------------
// Bringing files in and out
// ---------------------------------------------------------------------------

function download(name, blob) {
	if (options.downloaded) options.downloaded(name, blob);
	const a = el("a");
	a.href = URL.createObjectURL(blob);
	a.download = name;
	document.body.append(a);
	a.click();
	a.remove();
	setTimeout(() => URL.revokeObjectURL(a.href), 10000);
}
// a copy for the computer: the file, or the folder as a zip
async function downloadPath(e) {
	const files = await options.request("read", { path: e.path });
	if (!e.dir) download(e.name, new Blob([files[0].bytes]));
	else download(e.name + ".zip", await zip(files));
}

function toast(text) {
	const t = el("div", "tau-toast", text);
	t.setAttribute("role", "status");
	t.setAttribute("aria-live", "polite");
	document.body.append(t);
	setTimeout(() => t.remove(), 4000);
}

// File objects with their relative paths -> written under dir; the paths
// at the top of what was written. What has the name of something which is
// there already is asked about: clash (the names) answers "replace" (the
// files are written over those of the same name), "both" (what comes gets
// another name) or nothing (nothing is written, and null is the answer)
async function importFiles(dir, items, clash) {
	const files = [];
	const add = (rel, bytes, time) => {
		const parts = rel.split("/").filter(x => x && x !== "." && x !== "..").map(safe);
		if (parts.length) files.push({ parts, bytes, time: time || 0 });
	};
	for (const item of items) {
		const bytes = new Uint8Array(await item.file.arrayBuffer());
		if (suffix(item.rel) === "zip" && item.unpack) {
			// a zip of one folder gives that folder; the files of a zip
			// without one go into a folder named after it
			// (maps of names without the names which every object has: a file
			// may be called "constructor" or "toString")
			const entries = await unzip(bytes), first = Object.create(null);
			for (const e of entries) first[e.rel.split("/")[0]] = e.rel.includes("/");
			const names = Object.keys(first), one = names.length === 1 && first[names[0]];
			const into = one ? "" : item.rel.slice(0, item.rel.length - 4) + "/";
			for (const e of entries) add(into + e.rel, e.bytes, e.time);
			continue;
		}
		add(item.rel, bytes, item.file.lastModified);
	}
	if (files.length && clash) {
		const there = (await options.request("list", { dir })).entries.map(e => e.name);
		const taken = [...new Set(files.map(f => f.parts[0]))].filter(n => there.includes(n));
		const answer = taken.length ? await clash(taken) : "replace";
		if (!answer) return null;
		if (answer === "both") {
			const other = Object.create(null);
			for (const n of taken) { other[n] = fresh(there, n); there.push(other[n]); }
			for (const f of files) if (f.parts[0] in other) f.parts[0] = other[f.parts[0]];
		}
	}
	const tops = [...new Set(files.map(f => join(dir, f.parts[0])))];
	if (files.length) {
		// (each file in a buffer of its own, which goes to the worker)
		const written = files.map(f => ({ path: join(dir, f.parts.join("/")), bytes: f.bytes.slice().buffer, time: f.time }));
		await options.request("write", { files: written }, written.map(f => f.bytes));
	}
	return tops;
}
// the entries of a drop, folders included -> [{ rel, file }]
async function dropped(dt) {
	const out = [], entries = [];
	if (dt.items)
		for (const item of dt.items) {
			const en = item.webkitGetAsEntry ? item.webkitGetAsEntry() : null;
			if (en) entries.push(en);
		}
	if (entries.length === 0) { // no entries (a synthetic drop): the plain files
		for (const file of dt.files) out.push({ rel: file.name, file });
		return out;
	}
	async function visit(en, rel) {
		if (en.isFile) {
			const f = await new Promise((ok, ko) => en.file(ok, ko));
			out.push({ rel: rel + en.name, file: f });
		}
		else if (en.isDirectory) {
			const reader = en.createReader();
			let batch;
			do {
				batch = await new Promise((ok, ko) => reader.readEntries(ok, ko));
				for (const x of batch) await visit(x, rel + en.name + "/");
			} while (batch.length > 0);
		}
	}
	for (const en of entries) await visit(en, "");
	return out;
}

// The files which TeXmacs does not open as documents but the browser shows
// (a PDF, an image): they open in a tab of the browser, unless TeXmacs
// asked for such a file (an image to insert)
const TYPES = { pdf: "application/pdf", png: "image/png", jpg: "image/jpeg", jpeg: "image/jpeg", gif: "image/gif",
	webp: "image/webp", svg: "image/svg+xml", bmp: "image/bmp" };
const browserViews = p => suffix(p) in TYPES;
const IMAGES = ["png", "jpg", "jpeg", "gif", "webp", "svg", "bmp", "tif", "tiff", "pdf", "eps", "ps"];
// An image is shown as the picture of a page which is made here, never as
// a document of its own: an SVG file may carry a script, which would run
// as a script of this page and reach all the files kept in the browser. A
// picture runs none. (The PDF viewer of the browser runs none of ours.)
function viewInBrowser(path) {
	// (the tab is opened now, with the click: later the browser would refuse)
	const tab = window.open("", "_blank");
	if (!tab) { toast("The browser did not let a tab open for " + base(path)); return; }
	options.request("read", { path }).then(files => {
		const type = TYPES[suffix(path)], url = URL.createObjectURL(new Blob([files[0].bytes], { type }));
		if (type === "application/pdf") {
			tab.opener = null;
			tab.location = url;
			// (kept while the viewer loads it)
			setTimeout(() => URL.revokeObjectURL(url), 60000);
			return;
		}
		const doc = tab.document, img = doc.createElement("img");
		doc.title = base(path);
		doc.body.style.cssText = "margin:0;min-height:100vh;display:flex;align-items:center;justify-content:center;background:#f4f4f6";
		img.style.cssText = "max-width:100%;max-height:100vh;background:#fff";
		img.alt = base(path);
		img.addEventListener("load", () => URL.revokeObjectURL(url));
		img.addEventListener("error", () => URL.revokeObjectURL(url));
		img.src = url;
		doc.body.append(img);
	}).catch(error => { tab.close(); toast(error.message); });
}

// ---------------------------------------------------------------------------
// Backup: the whole home directory in a zip, and back
// ---------------------------------------------------------------------------

// A backup is a zip of the home directory: the documents of the user and
// ~/.TeXmacs (the preferences, the styles and fonts added), under home/,
// with a note, tau-backup.json. The temporary files and the caches of
// TeXmacs are left out: TeXmacs makes them again. What is not saved yet (a
// document being edited) is not a file, and not in the backup.
const BACKUP_NOTE = "tau-backup.json", BACKUP_ROOT = "home/";
function backupName() {
	const d = new Date(), two = n => (n < 10 ? "0" : "") + n;
	return "tau-backup-" + d.getFullYear() + "-" + two(d.getMonth() + 1) + "-" + two(d.getDate()) + ".zip";
}
// -> { blob, files, bytes }
export async function makeBackup() {
	// (a folder which is empty is an entry too: its name with "/")
	const all = await options.request("backup", {});
	const files = all.map(f => ({ rel: BACKUP_ROOT + f.rel + (f.dir ? "/" : ""), bytes: f.bytes, time: f.time }));
	const bytes = files.reduce((n, f) => n + f.bytes.byteLength, 0);
	const note = { format: 1, application: "Tau", made: new Date().toISOString(),
		address: location.origin + location.pathname, files: all.filter(f => !f.dir).length, bytes };
	files.unshift({ rel: BACKUP_NOTE, time: Date.now(), bytes: new TextEncoder().encode(JSON.stringify(note, null, 1)) });
	return { blob: await zip(files), files: note.files, bytes };
}
// the backup, saved on the computer
export function backup() {
	return makeBackup().then(b => {
		download(backupName(), b.blob);
		toast("Backup: " + b.files + " files, " + humanSize(b.blob.size) + ". Documents which are not saved are not in it.");
		return b;
	}, error => toast("The backup could not be made: " + error.message));
}
// a backup is chosen on the computer and restored, in the panel (which asks first)
export function restore() {
	const input = el("input");
	input.type = "file";
	input.accept = ".zip,application/zip";
	// (off the page rather than hidden, which some browsers open no chooser for)
	input.style.cssText = "position:fixed;left:-1000px;top:0;opacity:0";
	input.addEventListener("change", () => {
		const file = input.files && input.files[0];
		input.remove();
		if (!file) return;
		file.arrayBuffer().then(buffer => restoreBackup(new Uint8Array(buffer))).catch(error => toast("Not restored: " + error.message));
	});
	document.body.append(input);
	input.click();
}
// the bytes of a backup -> { note, entries: [{ rel (in the home), bytes,
// time, dir? }], missing }; an error when they are not a backup. missing:
// how many of the files it was made with are not in it
export async function readBackup(bytes) {
	const all = await unzip(bytes, true), entries = [];
	let note = null;
	for (const e of all) {
		if (e.rel === BACKUP_NOTE) {
			try { note = JSON.parse(new TextDecoder().decode(e.bytes)); } catch (error) {}
		}
		else if (e.rel.startsWith(BACKUP_ROOT)) {
			const rel = e.rel.slice(BACKUP_ROOT.length);
			// (no path which leaves the home directory)
			if (rel === "" || rel.split("/").some(q => q === "" || q === "." || q === "..")) continue;
			entries.push({ rel, bytes: e.bytes, time: e.time, dir: !!e.dir });
		}
	}
	if (!note || note.format !== 1) throw new Error("this zip is not a backup of Tau");
	const missing = Math.max(0, (Number(note.files) || 0) - entries.filter(e => !e.dir).length);
	return { note, entries, missing };
}

// ---------------------------------------------------------------------------
// The panel
// ---------------------------------------------------------------------------

const ICONS = {
	folder: '<svg viewBox="0 0 16 16" width="15" height="15"><path d="M1.5 4a1 1 0 0 1 1-1h3.3l1.4 1.5h6.3a1 1 0 0 1 1 1V12a1 1 0 0 1-1 1h-11a1 1 0 0 1-1-1z" fill="#8fa8dc" stroke="#5670a8" stroke-width="0.8"/></svg>',
	file: '<svg viewBox="0 0 16 16" width="15" height="15"><path d="M3.5 1.5h6l3 3v10h-9z" fill="#fff" stroke="#8a8a98" stroke-width="0.8"/><path d="M9.5 1.5v3h3" fill="none" stroke="#8a8a98" stroke-width="0.8"/></svg>'
};
const TITLES = { browse: "Files in this browser", open: "Open a file", save: "Save as" };

// mode: "browse", "open" or "save"; done (path or null) for open and save
function panel(mode, opts, done) {
	if (current) current.close(null);
	let dir = opts.dir || DOCS, selected = null, entries = [], kept = true, asking = null;
	const veil = el("div", "tau-veil tau-files-veil"), box = el("div", "tau-files");
	box.tabIndex = -1;
	box.dataset.mode = mode;
	veil.append(box);
	const head = el("div", "tau-dialog-title"), closeButton = el("button", "tau-dialog-close", "×");
	closeButton.type = "button";
	closeButton.addEventListener("click", () => close(null));
	head.append(el("span", "tau-dialog-name", opts.title || TITLES[mode]), closeButton);
	const places = el("div", "tau-files-places"), stored = el("span", "tau-files-note");
	const crumbs = el("div", "tau-files-crumbs"), tools = el("div", "tau-files-tools");
	const question = el("div", "tau-files-ask"), listing = el("div", "tau-files-list");
	const foot = el("div", "tau-files-foot");
	question.style.display = "none";
	box.append(head, places, crumbs, tools, question, listing, foot);

	let nameInput = null, hint = null;
	if (mode === "save") {
		nameInput = el("input", "tau-files-name");
		nameInput.type = "text";
		nameInput.spellcheck = false;
		nameInput.value = opts.name || (opts.type === "image" ? "untitled.png" : "untitled.tm");
		foot.append(el("span", "", "Name:"), nameInput);
	}
	else {
		hint = el("span", "tau-files-hint");
		foot.append(hint);
	}

	const button = (text, run, tip) => {
		const b = el("button", "tau-button", text);
		b.type = "button";
		if (tip) b.title = tip;
		b.addEventListener("click", run);
		return b;
	};
	// a button which opens a chooser of the system: a label around an input
	const chooser = (what, text, attrs, onfiles) => {
		const l = el("label", "tau-button", text), i = el("input");
		i.type = "file";
		i.dataset.what = what;
		i.style.display = "none";
		for (const k in attrs) i.setAttribute(k, attrs[k]);
		i.addEventListener("change", () => { onfiles([...i.files]); i.value = ""; });
		l.append(i);
		return l;
	};

	function close(result) {
		if (current !== me) return;
		current = null;
		veil.remove();
		for (const t of ["keydown", "keypress", "keyup"]) document.removeEventListener(t, onkey, true);
		document.removeEventListener("focusin", onfocus, true);
		if (done) done(result);
		if (options.focus) options.focus();
	}
	// The keys typed while the panel is open are not for TeXmacs: they do
	// not go further than here
	function onkey(event) {
		event.stopPropagation();
		// (229: a key of an input method, as its Enter which picks a word)
		if (event.type !== "keydown" || event.isComposing || event.keyCode === 229) return;
		if (event.key === "Escape") {
			event.preventDefault();
			if (asking) asking(null); else close(null);
		}
		else if (event.key === "Enter") {
			// (on a button, the key is the button's)
			if (event.target.closest && event.target.closest("button, label")) return;
			if (asking) { event.preventDefault(); asking(true); }
			else if (mode !== "browse") { event.preventDefault(); accept(); }
		}
	}
	// (nor does the keyboard go back to a view behind the panel)
	function onfocus(event) {
		if (!veil.contains(event.target)) box.focus();
	}

	// A question in the panel: a line with a field (value is a string) or
	// without one (a confirmation). The promise: the text, true, or null
	function ask(text, value, yes, other) {
		if (asking) asking(null);
		return new Promise(resolve => {
			question.textContent = "";
			question.style.display = "";
			tools.style.display = "none";
			const input = value === undefined ? null : el("input", "tau-files-name");
			const finish = result => {
				asking = null;
				question.style.display = "none";
				render();
				resolve(result === true && input ? input.value.trim() : result);
				if (nameInput) nameInput.focus(); else box.focus();
			};
			asking = finish;
			const ok = button(yes || "OK", () => finish(true)), cancel = button("Cancel", () => finish(null));
			ok.classList.add("tau-files-yes");
			question.append(el("span", "tau-files-question", text));
			if (input) { input.type = "text"; input.spellcheck = false; input.value = value; question.append(input); }
			question.append(cancel);
			// (another answer than yes: its name is what is answered)
			if (other) { const b = button(other, () => finish(other)); b.dataset.what = "other"; question.append(b); }
			question.append(ok);
			if (input) { input.focus(); input.select(); } else ok.focus();
		});
	}
	function failed(error) { toast(error && error.message || String(error)); }
	const request = (op, args, transfer) => options.request(op, args, transfer);

	// a file opened (the dialog to open, or open in the panel): for TeXmacs,
	// or for the browser (a PDF or an image, unless TeXmacs asked for one)
	function openChosen(path) {
		const wanted = mode === "open" && opts.type === "image" && IMAGES.includes(suffix(path));
		if (browserViews(path) && !wanted) viewInBrowser(path);
		else if (mode === "open") close(path);
		else { close(null); options.open(path); }
	}
	async function accept() {
		if (mode === "open") {
			const e = entries.find(x => x.path === selected);
			if (e && !e.dir) openChosen(e.path);
		}
		else if (mode === "save") {
			if (under(dir, SYS)) { toast("The files of TeXmacs cannot be changed: save in one of your folders."); return; }
			const n = safe(nameInput.value.trim());
			if (!n) return;
			const e = entries.find(x => x.name === n);
			if (e && e.dir) { go(e.path); return; }
			if (e && !(await ask("Replace " + n + ", which exists already?", undefined, "Replace"))) return;
			close(join(dir, n));
		}
	}

	// a file or folder of TeXmacs into the TeXmacs folder of the user, at the
	// same place (styles/x.ts: .TeXmacs/styles/x.ts), where TeXmacs looks
	// before its own files
	async function customize(e) {
		const rel = e.path.slice(SYS.length + 1), to = join(USER_TM, rel);
		const there = (await request("list", { dir: to.slice(0, to.lastIndexOf("/")) })).entries.some(x => x.path === to);
		// (a folder is copied into the one of the user, whose other files stay)
		if (there && !(await ask(e.dir
			? ".TeXmacs/" + rel + " exists already: write the files of TeXmacs over yours of the same name? Your other files there stay."
			: ".TeXmacs/" + rel + " exists already: replace it by the one of TeXmacs?", undefined, "Replace"))) return;
		await request("copy", { from: e.path, to });
		toast("Copied to .TeXmacs/" + rel + ": TeXmacs uses your copy, which you can edit " +
			"(a style or a package in the next document, Scheme code at the next start)");
	}

	async function go(to, select) {
		// (a question is about this folder: going elsewhere gives it up)
		if (asking && to !== dir) asking(null);
		try {
			const r = await request("list", { dir: to });
			dir = r.dir; entries = r.entries; kept = r.kept;
			selected = select && entries.some(e => e.path === select) ? select : null;
			box.dataset.loaded = "1";
		} catch (error) { failed(error); }
		if (current === me) render();
	}
	const refresh = select => go(dir, select);

	function render() {
		// the three places: the documents of the user, the TeXmacs folder of
		// the user (~/.TeXmacs: the preferences, the styles, packages, fonts
		// and Scheme code added, where TeXmacs looks before its own files)
		// and the files of TeXmacs, which are only shown
		const sys = under(dir, SYS), userTm = !sys && under(dir, USER_TM);
		const place = sys ? "sys" : userTm ? "tm" : "files", top = sys ? SYS : userTm ? USER_TM : HOME;
		box.dataset.dir = dir;
		places.textContent = "";
		for (const [text, where, key] of [["Your files", DOCS, "files"], ["Your TeXmacs folder", USER_TM, "tm"], ["Files of TeXmacs", SYS, "sys"]]) {
			const a = el("a", "tau-files-place" + (key === place ? " tau-current" : ""), text);
			a.dataset.place = key;
			a.addEventListener("click", () => go(where));
			places.append(a);
		}
		// the files are the user's, on this computer: nothing goes to a server
		stored.textContent = kept ? "Stored in this browser only; nothing is sent anywhere."
			: "Not kept: these files are gone when the page is closed.";
		stored.classList.toggle("tau-files-warning", !kept);
		places.append(stored);
		if (!asking) tools.style.display = sys ? "none" : "";
		if (hint) hint.textContent = sys
			? "The files of TeXmacs cannot be changed here: \"customize\" copies one into your TeXmacs folder (.TeXmacs), where you can edit it."
			: userTm
			? "Your TeXmacs folder (.TeXmacs): preferences, and the styles, packages, fonts and Scheme code you add. TeXmacs looks here before its own files."
			: "Drop files or folders here to add them to this folder.";
		crumbs.textContent = "";
		const home = el("a", "tau-files-crumb", sys ? "TeXmacs" : userTm ? ".TeXmacs" : "Home");
		home.addEventListener("click", () => go(top));
		crumbs.append(home);
		let acc = top;
		for (const part of dir.slice(top.length).split("/").filter(Boolean)) {
			acc = acc + "/" + part;
			const target = acc, a = el("a", "tau-files-crumb", part);
			a.addEventListener("click", () => go(target));
			crumbs.append(" / ", a);
		}
		listing.textContent = "";
		// (the TeXmacs folder is a place of its own, not a folder of the home)
		const shown = entries.filter(e => !(place === "files" && dir === HOME && e.name.startsWith(".")))
			.sort((a, b) => a.dir !== b.dir ? (a.dir ? -1 : 1) : a.name.localeCompare(b.name));
		if (shown.length === 0) listing.append(el("div", "tau-files-empty", "This folder is empty."));
		for (const e of shown) {
			const row = el("div", "tau-files-row" + (selected === e.path ? " tau-current" : ""));
			row.dataset.name = e.name;
			const icon = el("span", "tau-files-icon");
			icon.innerHTML = e.dir ? ICONS.folder : ICONS.file;
			row.append(icon, el("span", "tau-files-entry", e.name));
			const acts = el("span", "tau-files-acts");
			const act = (text, run, tip) => {
				const a = el("a", "tau-files-act", text);
				a.dataset.act = text;
				if (tip) a.title = tip;
				a.addEventListener("click", event => { event.stopPropagation(); Promise.resolve(run()).catch(failed); });
				a.addEventListener("dblclick", event => event.stopPropagation());
				acts.append(a);
			};
			if (mode === "browse" && !e.dir)
				act("open", () => openChosen(e.path),
					browserViews(e.path) ? "Open in a tab of the browser (TeXmacs does not open this kind of file)" : "");
			act("save copy", () => downloadPath(e), e.dir ? "Save a copy on your computer, as a zip" : "Save a copy on your computer");
			if (sys) act("customize", () => customize(e), "Copy into your TeXmacs folder, where TeXmacs looks first");
			else {
				act("rename", async () => {
					let n = await ask("Rename " + e.name + " to", e.name, "Rename");
					if (!n || n === e.name) return;
					n = safe(n);
					await request("rename", { from: e.path, to: join(dir, n) });
					await refresh(join(dir, n));
				});
				act("delete", async () => {
					if (!(await ask("Delete " + e.name + (e.dir ? " and everything in it?" : "?"), undefined, "Delete"))) return;
					await request("remove", { path: e.path });
					await refresh();
				});
			}
			row.append(acts, el("span", "tau-files-size", e.dir ? "" : humanSize(e.size)), el("span", "tau-files-time", humanTime(e.time)));
			row.addEventListener("click", () => {
				if (e.dir) { go(e.path); return; }
				selected = e.path;
				if (mode === "save") nameInput.value = e.name;
				render();
			});
			row.addEventListener("dblclick", () => {
				if (e.dir) return;
				if (mode === "save") accept(); else openChosen(e.path);
			});
			listing.append(row);
		}
	}

	const added = tops => { if (tops) refresh(tops.length === 1 ? tops[0] : null); };
	// what comes with a name which is taken in the folder: asked first
	const clash = async names => {
		const answer = await ask((names.length === 1 ? names[0] + " exists" : names.length + " of these exist (" +
			names.slice(0, 4).join(", ") + (names.length > 4 ? "…" : "") + ")") + " already in this folder. Replace, or keep both?",
			undefined, "Replace", "Keep both");
		return answer === true ? "replace" : answer ? "both" : null;
	};
	tools.append(
		chooser("files", "Add files…", { multiple: "" }, fs =>
			importFiles(dir, fs.map(f => ({ rel: f.name, file: f })), clash).then(added, failed)),
		chooser("folder", "Add a folder…", { webkitdirectory: "", multiple: "" }, fs =>
			importFiles(dir, fs.map(f => ({ rel: f.webkitRelativePath || f.name, file: f })), clash).then(added, failed)),
		chooser("zip", "Add a zip…", { accept: ".zip" }, fs =>
			importFiles(dir, fs.map(f => ({ rel: f.name, file: f, unpack: true })), clash)
				.then(added, error => toast("Cannot read the zip: " + error.message))),
		button("New folder", async () => {
			let n = await ask("New folder", fresh(entries.map(e => e.name), "folder"), "Create");
			if (!n) return;
			n = safe(n);
			try { await request("mkdir", { path: join(dir, n) }); } catch (error) { failed(error); }
			await refresh();
		}));

	if (mode === "browse") {
		// the backup of all of it (the home directory): not in the dialogs to
		// open and to save, which a restore, starting the page again, would
		// not end well
		const b1 = button("Save a backup", () => backup(), "Save a zip of all your files and of your TeXmacs folder on your computer");
		b1.dataset.what = "backup";
		const b2 = chooser("restore", "Restore a backup…", { accept: ".zip,application/zip" }, fs => {
			if (fs[0]) fs[0].arrayBuffer().then(buffer => restore(new Uint8Array(buffer))).catch(error => toast("Not restored: " + error.message));
		});
		b2.title = "Bring back the files of a backup, from this or another browser";
		foot.append(b1, b2, button("Close", () => close(null)));
	}
	else {
		const ok = button(mode === "open" ? "Open" : "Save", () => accept());
		ok.classList.add("tau-files-yes");
		ok.dataset.what = "ok";
		foot.append(button("Cancel", () => close(null)), ok);
	}

	// The files of a backup are written over those of the home directory:
	// merged with what is there (a file which the backup has not stays) or
	// in place of it, an exact copy of the backup. Tau starts again with them
	async function restore(bytes) {
		const b = await readBackup(bytes);
		const real = b.entries.filter(e => !e.dir);
		const docs = real.filter(e => !e.rel.startsWith(".TeXmacs/")).length;
		const size = real.reduce((n, e) => n + e.bytes.length, 0);
		const when = String(b.note.made || "").replace("T", " ").slice(0, 16);
		// (programs of Scheme in it run when Tau starts: a backup from someone
		// else may bring them)
		const programs = real.filter(e => /^\.TeXmacs\/(progs|plugins)\//.test(e.rel)).length;
		const what = "the backup made " + when + " (" + docs + " of your files, " + (real.length - docs) +
			" files of preferences and settings, " + humanSize(size) + ")" +
			(programs ? ". It has " + programs + " files of programs (in .TeXmacs/progs or .TeXmacs/plugins), which run when Tau starts: restore it only if it is yours" : "");
		// (a backup which lost files is not put in place of everything)
		const answer = b.missing
			? await ask("This backup is not whole: " + b.missing + " of the " + b.note.files + " files it was made with are not in it. Restore " +
				what + " all the same? Its files are written over those of the same name, the others stay. " +
				"Documents which are not saved are lost: Tau then starts again.", undefined, "Restore")
			: await ask("Restore " + what + "? \"Restore\" writes its files over those of the same name " +
				"and keeps the others; \"Replace everything\" first deletes all that is kept in this browser, for an exact copy of the backup. " +
				"Documents which are not saved are lost: Tau then starts again.",
				undefined, "Restore", "Replace everything");
		if (!answer) return;
		const files = b.entries.map(e => ({ rel: e.rel, dir: e.dir, bytes: e.bytes.slice().buffer, time: e.time }));
		try { await request("restore", { files, exact: answer !== true }, files.map(f => f.bytes)); }
		catch (error) {
			// (refused before anything was written: nothing more to do)
			if (!error.restart) throw error;
			// the files are in between, and the core has stopped
			await ask("The backup could not be restored: " + error.message + ". Tau starts again with what the browser kept.", undefined, "OK");
		}
		options.reload();
	}

	veil.addEventListener("dragover", event => { event.preventDefault(); event.stopPropagation(); });
	veil.addEventListener("drop", event => {
		event.preventDefault(); event.stopPropagation();
		if (!event.dataTransfer) return;
		if (under(dir, SYS)) { toast("The files of TeXmacs cannot be changed: drop in one of your folders"); return; }
		const into = dir;
		dropped(event.dataTransfer).then(items => importFiles(into, items, clash)).then(added, failed);
	});
	veil.addEventListener("mousedown", event => { if (event.target === veil) close(null); });
	for (const t of ["keydown", "keypress", "keyup"]) document.addEventListener(t, onkey, true);
	document.addEventListener("focusin", onfocus, true);
	document.body.append(veil);
	const me = { close, restore, mode };
	current = me;
	render();
	if (nameInput) {
		nameInput.focus();
		// (the name without its suffix, which is seldom what changes)
		const dot = nameInput.value.lastIndexOf(".");
		nameInput.setSelectionRange(0, dot > 0 ? dot : nameInput.value.length);
	}
	else box.focus();
	go(dir);
}

// ---------------------------------------------------------------------------
// For the page
// ---------------------------------------------------------------------------

// options: request (op, args, transfer) -> a promise (the worker answers),
// chosen (ticket, path) the answer to a question of the core, open (path)
// a document to open, reload () the page starts again, focus () the
// keyboard goes back to the views
export function initFiles(opts) { options = opts; }

export function browse(dir) { panel("browse", { dir }, null); }

// The core asks for a file (tau-choose-file): { mode, ticket, title, type,
// name, dir }. It is told what was chosen, or that nothing was
export function choose(m) {
	if (m.mode === "browse") { browse(m.dir); return; }
	panel(m.mode, m, path => options.chosen(m.ticket, path || ""));
}

// Files and folders which are dropped on a view: copied into ~/Documents,
// keeping their structure (a name which is taken gets another one, rather
// than overwriting). The documents are opened; images, when no document
// came with them, are inserted where they fell (insert (paths)); a folder
// is shown in the panel, and its document is opened when it has one.
const DOC_SUFFIXES = ["tm", "tmml", "ts", "tp", "stm", "tex", "html", "htm", "md", "bib", "scm", "txt"];
export async function dropOnView(dataTransfer, insert) {
	const items = await dropped(dataTransfer);
	if (items.length === 0) return;
	const there = (await options.request("list", { dir: DOCS })).entries.map(e => e.name);
	const renamed = Object.create(null);
	for (const item of items) {
		const parts = item.rel.split("/");
		if (!(parts[0] in renamed)) { renamed[parts[0]] = fresh(there, safe(parts[0])); there.push(renamed[parts[0]]); }
		parts[0] = renamed[parts[0]];
		item.rel = parts.join("/");
	}
	await importFiles(DOCS, items);
	const names = Object.values(renamed), tops = (await options.request("list", { dir: DOCS })).entries.filter(e => names.includes(e.name));
	const docs = tops.filter(e => !e.dir && DOC_SUFFIXES.includes(suffix(e.name)));
	const images = tops.filter(e => !e.dir && IMAGES.includes(suffix(e.name)));
	const folders = tops.filter(e => e.dir);
	for (const e of docs) options.open(e.path);
	if (images.length > 0 && docs.length === 0) insert(images.map(e => e.path));
	for (const f of folders) {
		const mains = (await options.request("list", { dir: f.path })).entries.filter(e => !e.dir && suffix(e.name) === "tm");
		if (mains.length === 1) options.open(mains[0].path);
	}
	if (folders.length > 0) browse(folders[0].path);
	// (what is neither a document nor an image, nor in a folder, is only kept)
	if (folders.length > 0 || tops.length > docs.length + images.length || Object.keys(renamed).some(n => renamed[n] !== safe(n)))
		toast("Copied to Documents: " + names.join(", "));
}

export const isOpen = () => !!current;
// a restore of bytes, in the panel to browse, which is opened if it is not
export function restoreBackup(bytes) {
	if (!current || current.mode !== "browse") browse();
	return current.restore(bytes);
}
