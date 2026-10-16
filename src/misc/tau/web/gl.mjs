// The view of a pane drawn by WebGL (?gl): the commands of the core
// (mupdf_record in src/Plugins/MuPDF/mupdf_renderer.cpp) as triangles.
//
// What the view shows is kept in one of two textures: a scroll draws it into
// the other at its new place, and that one is then shown; a frame draws into
// the one shown, within its rectangle (the scissor), and the texture is then
// drawn on the canvas. Rectangles are triangles of a colour; a glyph is a
// rectangle of the coverage of the glyph, kept in one texture of a byte a
// pixel (the atlas: emptied and filled again with what a frame needs when it
// is full), times its colour; a picture a texture of its own. Lines,
// polygons and arcs are drawn by a 2D canvas as the page draws them, and
// given as a picture. A transformation (9 and 10) moves the corners of what
// is drawn; the clips are said in the pixels of the view, as the scissor.
//
// (and, as the GPU path of wip_other_guis does: a thin line along x or y is
// rectangles, of the coverage which the 2D canvas would give each row or
// column; the paths which come one after the other are drawn by the 2D
// canvas together, given once; the canvas is drawn once for a refresh of the
// screen, not for each frame; the textures of the pictures are kept for the
// pictures used last, 512 or 256 MB at most)
//
// makeGLSurface (canvas, shared, lost): null without WebGL 2; else what
// tau.mjs asks of a surface (size, shift, pixels, commands, sync). shared
// has the glyphs and the pictures of the page, drawPath and opSize; lost is
// called when the context of WebGL came back after it was lost, for the
// view to be drawn again.

const VS = `#version 300 es
layout(location = 0) in vec2 p;
layout(location = 1) in vec2 uv;
layout(location = 2) in vec4 col;
layout(location = 3) in float kind;
uniform vec2 size;
out vec2 vuv; out vec4 vcol; out float vkind;
void main () {
	gl_Position = vec4 (p.x / size.x * 2.0 - 1.0, 1.0 - p.y / size.y * 2.0, 0.0, 1.0);
	vuv = uv; vcol = col; vkind = kind;
}`;
// kind 0: a colour; 1: a coverage (red) times a colour; 2: a picture (with
// its alpha premultiplied) times an opacity
const FS = `#version 300 es
precision highp float;
in vec2 vuv; in vec4 vcol; in float vkind;
uniform sampler2D tex;
out vec4 o;
void main () {
	if (vkind < 0.5) o = vcol;
	else if (vkind < 1.5) o = vcol * texture (tex, vuv).r;
	else o = texture (tex, vuv) * vcol.a;
}`;
const FLOATS = 9;     // x, y, u, v, r, g, b, a, kind
const ATLAS = 2048;   // the atlas of the glyphs
const PICTURES = 512, PICTURE_BYTES = 256 << 20;

export function makeGLSurface(canvas, shared, lost) {
	const gl = canvas.getContext("webgl2", { alpha: false, antialias: false, depth: false, stencil: false,
		premultipliedAlpha: true, preserveDrawingBuffer: true });
	if (!gl) return null;
	let R = null;         // what WebGL has: programs, textures (none while the context is lost)
	let W = 0, H = 0;     // the size of the textures of the view
	let verts = new Float32Array(FLOATS * 6 * 2048), nv = 0, bound = null;
	const scratch = new OffscreenCanvas(1, 1), sx = scratch.getContext("2d");
	const pixel = new Uint8Array(4);

	function texture(filter) {
		const t = gl.createTexture();
		gl.bindTexture(gl.TEXTURE_2D, t);
		gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, filter);
		gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, filter);
		gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
		gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
		return t;
	}
	function init() {
		const shader = (type, source) => {
			const s = gl.createShader(type);
			gl.shaderSource(s, source);
			gl.compileShader(s);
			if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error("Tau, WebGL: " + gl.getShaderInfoLog(s));
			return s;
		};
		const program = gl.createProgram();
		gl.attachShader(program, shader(gl.VERTEX_SHADER, VS));
		gl.attachShader(program, shader(gl.FRAGMENT_SHADER, FS));
		gl.linkProgram(program);
		if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error("Tau, WebGL: " + gl.getProgramInfoLog(program));
		gl.useProgram(program);
		const vao = gl.createVertexArray();
		gl.bindVertexArray(vao);
		gl.bindBuffer(gl.ARRAY_BUFFER, gl.createBuffer());
		const stride = 4 * FLOATS;
		for (const [loc, n, at] of [[0, 2, 0], [1, 2, 2], [2, 4, 4], [3, 1, 8]]) {
			gl.enableVertexAttribArray(loc);
			gl.vertexAttribPointer(loc, n, gl.FLOAT, false, stride, 4 * at);
		}
		gl.uniform1i(gl.getUniformLocation(program, "tex"), 0);
		gl.activeTexture(gl.TEXTURE0);
		const atlas = texture(gl.LINEAR);
		gl.texStorage2D(gl.TEXTURE_2D, 1, gl.R8, ATLAS, ATLAS);
		R = { size: gl.getUniformLocation(program, "size"), atlas, frames: null, shown: 0,
			stage: texture(gl.NEAREST), path: texture(gl.LINEAR), pictures: new Map(), pictureBytes: 0,
			glyphs: new Map(), ax: 0, ay: 0, arow: 0 };
		gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
		W = H = 0;
	}
	canvas.addEventListener("webglcontextlost", event => { event.preventDefault(); R = null; });
	canvas.addEventListener("webglcontextrestored", () => { init(); lost(); });
	init();

	// what is drawn: triangles, two for each rectangle, drawn together until
	// the texture or the target changes
	function flush() {
		if (!nv) return;
		gl.bindTexture(gl.TEXTURE_2D, bound || R.atlas);
		gl.bufferData(gl.ARRAY_BUFFER, verts.subarray(0, nv * FLOATS), gl.STREAM_DRAW);
		gl.drawArrays(gl.TRIANGLES, 0, nv);
		nv = 0;
	}
	function use(t) { if (t !== bound) { flush(); bound = t; } }
	// (and no texture of the view stays bound while one is drawn into: WebGL
	// would not draw, a texture which is read and written)
	function into(fb, w, h) {
		flush();
		bound = null;
		gl.bindFramebuffer(gl.FRAMEBUFFER, fb);
		gl.viewport(0, 0, w, h);
		gl.uniform2f(R.size, w, h);
	}
	// (u0, v0) at (x0, y0), (u1, v1) at (x1, y1); m: a DOMMatrix or null
	function quad(x0, y0, x1, y1, u0, v0, u1, v1, r, g, b, a, kind, m) {
		if ((nv + 6) * FLOATS > verts.length) {
			const more = new Float32Array(verts.length * 2);
			more.set(verts);
			verts = more;
		}
		const c = [[x0, y0, u0, v0], [x1, y0, u1, v0], [x1, y1, u1, v1], [x0, y1, u0, v1]];
		if (m) for (const q of c) { const x = q[0], y = q[1]; q[0] = m.a * x + m.c * y + m.e; q[1] = m.b * x + m.d * y + m.f; }
		let k = nv * FLOATS;
		for (const j of [0, 1, 2, 0, 2, 3]) {
			const q = c[j];
			verts[k] = q[0]; verts[k + 1] = q[1]; verts[k + 2] = q[2]; verts[k + 3] = q[3];
			verts[k + 4] = r; verts[k + 5] = g; verts[k + 6] = b; verts[k + 7] = a; verts[k + 8] = kind;
			k += FLOATS;
		}
		nv += 6;
	}
	// a colour of the core (0xRRGGBBAA), premultiplied
	let cr = 0, cg = 0, cb = 0, ca = 0;
	function color(v) {
		const u = v >>> 0;
		ca = (u & 255) / 255;
		cr = (u >>> 24) / 255 * ca; cg = ((u >>> 16) & 255) / 255 * ca; cb = ((u >>> 8) & 255) / 255 * ca;
	}
	// the texture of the view shown, drawn into the target as it is (blend off)
	function copy(from, x, y) {
		gl.disable(gl.BLEND);
		gl.disable(gl.SCISSOR_TEST);
		use(from);
		quad(x, y, x + W, y + H, 0, 1, 1, 0, 1, 1, 1, 1, 2, null);
		flush();
	}
	// (at the next refresh of the screen, or when the canvas is waited for)
	let presenting = 0;
	function presentNow() {
		if (presenting) { cancelAnimationFrame(presenting); presenting = 0; }
		if (!R || !R.frames) return;
		into(null, canvas.width, canvas.height);
		copy(R.frames[R.shown].tex, 0, 0);
	}
	function present() {
		if (!presenting) presenting = requestAnimationFrame(() => { presenting = 0; presentNow(); });
	}

	// the glyphs of a frame in the atlas, before it is drawn
	function placeGlyph(id) {
		const g = shared.glyphCovs.get(id);
		if (!g || g.w > ATLAS || g.h > ATLAS) { R.glyphs.set(id, null); return true; }
		if (R.ax + g.w > ATLAS) { R.ax = 0; R.ay += R.arow + 1; R.arow = 0; }
		if (R.ay + g.h > ATLAS) return false;
		gl.bindTexture(gl.TEXTURE_2D, R.atlas);
		gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
		gl.texSubImage2D(gl.TEXTURE_2D, 0, R.ax, R.ay, g.w, g.h, gl.RED, gl.UNSIGNED_BYTE, g.cov);
		gl.pixelStorei(gl.UNPACK_ALIGNMENT, 4);
		R.glyphs.set(id, { u0: R.ax / ATLAS, v0: R.ay / ATLAS, u1: (R.ax + g.w) / ATLAS, v1: (R.ay + g.h) / ATLAS, w: g.w, h: g.h });
		R.ax += g.w + 1;
		R.arow = Math.max(R.arow, g.h);
		return true;
	}
	function placeGlyphs(o) {
		for (let pass = 0; pass < 2; pass++) {
			let full = false;
			for (let i = 0, k; i < o.length && (k = shared.opSize(o, i)); i += k)
				if (o[i] === 2 && !R.glyphs.has(o[i + 1]) && !placeGlyph(o[i + 1])) { full = true; break; }
			if (!full) return;
			R.glyphs.clear();
			R.ax = R.ay = R.arow = 0;
		}
	}
	// (a number may come again with other pixels: the texture follows them;
	// the map is in the order of use, the first are given back when too many)
	function forget(id, t) {
		gl.deleteTexture(t.tex);
		R.pictureBytes -= 4 * t.w * t.h;
		R.pictures.delete(id);
	}
	function pictureOf(id) {
		const pic = shared.pictures.get(id);
		let t = R.pictures.get(id);
		if (t && t.pic === pic) { R.pictures.delete(id); R.pictures.set(id, t); return t; }
		flush();
		if (t) forget(id, t);
		if (!pic) return null;
		for (const [old, u] of R.pictures) {
			if (R.pictures.size < PICTURES && R.pictureBytes + 4 * pic.width * pic.height <= PICTURE_BYTES) break;
			forget(old, u);
		}
		t = { tex: texture(gl.LINEAR), w: pic.width, h: pic.height, pic };
		gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, true);
		gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, pic);
		gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, false);
		R.pictures.set(id, t);
		R.pictureBytes += 4 * t.w * t.h;
		return t;
	}
	// a line of two points along x or y, 2 pixels wide at most, without a
	// transformation: the rows (or columns) which its width covers, each a
	// rectangle of its colour times what it covers of them, as the 2D canvas
	// (its round ends taken as square, less than half a pixel apart); false
	// for another line (and nothing drawn unless draw)
	function straightLine(o, i, m, draw) {
		if (m || o[i + 1] !== 2 || o[i + 2] > 32) return false;
		const x1 = o[i + 4], y1 = o[i + 5], x2 = o[i + 6], y2 = o[i + 7], w = o[i + 2] / 16;
		const along = y1 === y2;
		if (!along && x1 !== x2 || w <= 0) return false;
		if (!draw) return true;
		// (the ends are fractional too: what they cover of their pixel is
		// taken as all of it, a pixel of the length at most)
		const a1 = Math.floor((along ? Math.min(x1, x2) : Math.min(y1, y2)) - w / 2);
		const a2 = Math.ceil((along ? Math.max(x1, x2) : Math.max(y1, y2)) + w / 2);
		const c = along ? y1 : x1, b1 = c - w / 2, b2 = c + w / 2;
		color(o[i + 3]);
		const band = (s1, s2, k) => {
			if (s2 <= s1 || k <= 0) return;
			if (along) quad(a1, s1, a2, s2, 0, 0, 0, 0, cr * k, cg * k, cb * k, ca * k, 0, null);
			else quad(s1, a1, s2, a2, 0, 0, 0, 0, cr * k, cg * k, cb * k, ca * k, 0, null);
		};
		const f1 = Math.floor(b1), f2 = Math.floor(b2);
		if (f1 === f2) { band(f1, f1 + 1, b2 - b1); return true; }
		band(f1, f1 + 1, f1 + 1 - b1);
		band(f1 + 1, f2, 1);
		band(f2, f2 + 1, b2 - f2);
		return true;
	}
	// the rectangle (within the clip) which a line, a polygon or an arc covers
	function pathBox(o, i, m, clip) {
		const end = i + shared.opSize(o, i), op = o[i];
		const xs = [], ys = [];
		if (op === 5) { xs.push(o[i + 1], o[i + 3], o[i + 1], o[i + 3]); ys.push(o[i + 2], o[i + 2], o[i + 4], o[i + 4]); }
		else for (let p = i + (op === 3 ? 4 : 3); p < end; p += 2) { xs.push(o[p]); ys.push(o[p + 1]); }
		if (m) for (let k = 0; k < xs.length; k++) {
			const x = xs[k], y = ys[k];
			xs[k] = m.a * x + m.c * y + m.e; ys[k] = m.b * x + m.d * y + m.f;
		}
		const width = op === 3 ? o[i + 2] / 16 : op === 5 && !o[i + 9] ? o[i + 7] / 16 : 0;
		const pad = 2 + width * (m ? Math.max(Math.hypot(m.a, m.b), Math.hypot(m.c, m.d)) : 1);
		return [Math.max(clip[0], Math.floor(Math.min(...xs) - pad)), Math.max(clip[1], Math.floor(Math.min(...ys) - pad)),
			Math.min(clip[2], Math.ceil(Math.max(...xs) + pad)), Math.min(clip[3], Math.ceil(Math.max(...ys) + pad))];
	}
	// the lines, polygons and arcs from i on, one after the other (but the
	// straight lines, and those which would make the rectangle given too
	// large): drawn by the 2D canvas into the rectangle which they cover,
	// given as a picture; the index after them
	function paths(o, i, m, clip) {
		const n = o.length, todo = [];
		let box = null, area = 0;
		for (; i < n && (o[i] === 3 || o[i] === 4 || o[i] === 5); i += shared.opSize(o, i)) {
			// (a straight line after paths waits for them to be drawn)
			if (o[i] === 3 && straightLine(o, i, m, !todo.length)) { if (todo.length) break; continue; }
			const b = pathBox(o, i, m, clip);
			if (b[2] <= b[0] || b[3] <= b[1]) continue;
			const u = box ? [Math.min(box[0], b[0]), Math.min(box[1], b[1]), Math.max(box[2], b[2]), Math.max(box[3], b[3])] : b;
			const a = (b[2] - b[0]) * (b[3] - b[1]);
			if (box && (u[2] - u[0]) * (u[3] - u[1]) > Math.max(2 * (area + a), 256 * 256)) break;
			todo.push(i); box = u; area += a;
		}
		if (todo.length) drawPaths(o, todo, m, box);
		return i;
	}
	function drawPaths(o, todo, m, [x1, y1, x2, y2]) {
		const w = x2 - x1, h = y2 - y1;
		if (scratch.width !== w || scratch.height !== h) { scratch.width = w; scratch.height = h; }
		else { sx.setTransform(1, 0, 0, 1, 0, 0); sx.clearRect(0, 0, w, h); }
		sx.setTransform(1, 0, 0, 1, -x1, -y1);
		if (m) sx.transform(m.a, m.b, m.c, m.d, m.e, m.f);
		for (const i of todo) shared.drawPath(sx, o, i);
		flush();
		gl.bindTexture(gl.TEXTURE_2D, R.path);
		gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, true);
		gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, scratch);
		gl.pixelStorei(gl.UNPACK_PREMULTIPLY_ALPHA_WEBGL, false);
		bound = R.path;
		quad(x1, y1, x2, y2, 0, 0, 1, 1, 1, 1, 1, 1, 2, null);
	}

	return {
		size(w, h) {
			if (canvas.width !== w || canvas.height !== h) { canvas.width = w; canvas.height = h; }
			if (!R || (W === w && H === h && R.frames)) return;
			for (const f of R.frames || []) { gl.deleteTexture(f.tex); gl.deleteFramebuffer(f.fb); }
			R.frames = [0, 1].map(() => {
				const tex = texture(gl.NEAREST);
				gl.texStorage2D(gl.TEXTURE_2D, 1, gl.RGBA8, w, h);
				const fb = gl.createFramebuffer();
				gl.bindFramebuffer(gl.FRAMEBUFFER, fb);
				gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, tex, 0);
				gl.clearColor(1, 1, 1, 1);
				gl.clear(gl.COLOR_BUFFER_BIT);
				return { tex, fb };
			});
			R.shown = 0;
			W = w; H = h;
		},
		shift(dx, dy) {
			if (!R || !R.frames) return;
			const to = R.frames[1 - R.shown];
			into(to.fb, W, H);
			copy(R.frames[R.shown].tex, dx, dy);
			R.shown = 1 - R.shown;
		},
		pixels(m) {
			if (!R || !R.frames) return;
			gl.bindTexture(gl.TEXTURE_2D, R.stage);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, m.w, m.h, 0, gl.RGBA, gl.UNSIGNED_BYTE, new Uint8Array(m.pixels));
			into(R.frames[R.shown].fb, W, H);
			gl.disable(gl.BLEND);
			gl.disable(gl.SCISSOR_TEST);
			use(R.stage);
			quad(m.x, m.y, m.x + m.w, m.y + m.h, 0, 0, 1, 1, 1, 1, 1, 1, 2, null);
			flush();
			present();
		},
		// (the rectangle which the worker drew, ?coregl: as the pixels)
		bitmap(m) {
			if (!R || !R.frames) { m.bitmap.close(); return; }
			gl.bindTexture(gl.TEXTURE_2D, R.stage);
			gl.texImage2D(gl.TEXTURE_2D, 0, gl.RGBA, gl.RGBA, gl.UNSIGNED_BYTE, m.bitmap);
			m.bitmap.close();
			into(R.frames[R.shown].fb, W, H);
			gl.disable(gl.BLEND);
			gl.disable(gl.SCISSOR_TEST);
			use(R.stage);
			quad(m.x, m.y, m.x + m.w, m.y + m.h, 0, 0, 1, 1, 1, 1, 1, 1, 2, null);
			flush();
			present();
		},
		commands(m) {
			if (!R || !R.frames) return;
			const o = new Int32Array(m.ops), f = new Float32Array(m.ops), n = o.length;
			placeGlyphs(o);
			into(R.frames[R.shown].fb, W, H);
			gl.enable(gl.BLEND);
			gl.enable(gl.SCISSOR_TEST);
			const frame = [m.x, m.y, m.x + m.w, m.y + m.h];
			let clip = frame;
			const setClip = (x1, y1, x2, y2) => {
				flush();
				x1 = Math.max(x1, frame[0]); y1 = Math.max(y1, frame[1]);
				x2 = Math.max(x1, Math.min(x2, frame[2])); y2 = Math.max(y1, Math.min(y2, frame[3]));
				clip = [x1, y1, x2, y2];
				gl.scissor(x1, H - y2, x2 - x1, y2 - y1);
			};
			setClip(...frame);
			const matrices = [];
			let matrix = null;
			for (let i = 0; i < n;) {
				switch (o[i]) {
				case 9: {
					// (as the 2D canvas: the matrix of PDF, y up, in the pixels of the view)
					const t = new DOMMatrix([f[i + 1], -f[i + 2], -f[i + 3], f[i + 4], f[i + 5], -f[i + 6]]);
					matrices.push(matrix);
					matrix = matrix ? matrix.multiply(t) : t;
					i += 7; break;
				}
				case 10:
					matrix = matrices.length ? matrices.pop() : null;
					i += 1; break;
				case 1:
					color(o[i + 5]);
					quad(o[i + 1], o[i + 2], o[i + 3], o[i + 4], 0, 0, 0, 0, cr, cg, cb, ca, 0, matrix);
					i += 6; break;
				case 2: {
					const g = R.glyphs.get(o[i + 1]);
					if (g) {
						use(R.atlas);
						color(o[i + 4]);
						const x = o[i + 2], y = o[i + 3];
						quad(x, y, x + g.w, y + g.h, g.u0, g.v0, g.u1, g.v1, cr, cg, cb, ca, 1, matrix);
					}
					i += 5; break;
				}
				case 3: case 4: case 5:
					i = paths(o, i, matrix, clip);
					break;
				case 6:
					setClip(o[i + 1], o[i + 2], o[i + 3], o[i + 4]);
					i += 5; break;
				case 7:
					quad(o[i + 1], o[i + 2], o[i + 3], o[i + 4], 0, 0, 0, 0, 0xd8 / 255, 0xd8 / 255, 0xd8 / 255, 1, 0, null);
					i += 5; break;
				case 8: {
					const t = pictureOf(o[i + 1]);
					if (t) {
						use(t.tex);
						const x = o[i + 2], y = o[i + 3], a = o[i + 4] / 255;
						quad(x, y, x + t.w, y + t.h, 0, 0, 1, 1, a, a, a, a, 2, matrix);
					}
					i += 5; break;
				}
				default: i = n;
				}
			}
			flush();
			gl.disable(gl.SCISSOR_TEST);
			present();
		},
		// (a pixel of the view read back: the canvas is drawn at the refresh,
		// as a 2D canvas is put on the screen, and is not waited for)
		sync() {
			if (!R || !R.frames) return;
			into(R.frames[R.shown].fb, W, H);
			gl.readPixels(0, 0, 1, 1, gl.RGBA, gl.UNSIGNED_BYTE, pixel);
		}
	};
}
