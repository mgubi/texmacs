// The test suites of TeXmacs (TeXmacs/progs/check/check-master.scm) run by
// the core of Tau under node. Some cannot pass there (no processes, other
// fonts): they are listed with their reason in suites-expected.txt, and
// the run fails when a suite fails which is not listed, or when one which
// is listed passes (the list is then out of date).
//
//   node misc/tau/test/suites-test.mjs [--core build-tau/out/node/tau.js]
//                                      [--texmacs TeXmacs] [--verbose]

import { spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const here = path.dirname(fileURLToPath(import.meta.url));
const args = process.argv.slice(2);
const opt = (name, dflt) => { const i = args.indexOf(name); return i >= 0 ? args[i + 1] : dflt; };
const core = path.resolve(opt("--core", "build-tau/out/node/tau.js"));
const texmacs = path.resolve(opt("--texmacs", path.join(here, "../../../TeXmacs")));
const verbose = args.includes("--verbose");

const expected = new Map();
for (const line of fs.readFileSync(path.join(here, "suites-expected.txt"), "utf8").split("\n")) {
	const m = /^([^#\s][^\s:]*):\s*(.*)$/.exec(line);
	if (m) expected.set(m[1], m[2]);
}

const home = fs.mkdtempSync(path.join(os.tmpdir(), "tau-suites-"));
const code = '(begin (use-modules (check check-master)) (display* "RESULT " (run-all-tests) "\\n") (quit-TeXmacs))';
const run = spawnSync(process.execPath, [core, "-headless", "-x", code], {
	env: { ...process.env, TEXMACS_PATH: texmacs, TEXMACS_HOME_PATH: path.join(home, ".TeXmacs"), HOME: home },
	encoding: "utf8", maxBuffer: 1 << 28, timeout: 15 * 60 * 1000
});
const out = (run.stdout || "") + (run.stderr || "");
fs.rmSync(home, { recursive: true, force: true });

// the failures of each suite: the lines between its title and the next
const failures = new Map();
let suite = "(start)";
for (const line of out.split("\n")) {
	const t = /^Test suite of (?:the )?([^\s:]+)/.exec(line);
	if (t) suite = t[1];
	if (/^\s*FAILED /.test(line) || /^\s*error in suite/.test(line)) {
		if (!failures.has(suite)) failures.set(suite, []);
		failures.get(suite).push(line.trim());
	}
}
const summary = /^Suites: (\d+), failed: (.*)$/m.exec(out);
if (!summary) {
	console.log("suites: the tests did not run to their end");
	console.log(out.split("\n").slice(-15).join("\n"));
	process.exit(1);
}
const failed = summary[2] === "none" ? [] : summary[2].split(", ");
const unexpected = failed.filter(s => !expected.has(s));
const fixed = Array.from(expected.keys()).filter(s => !failed.includes(s));
if (verbose) for (const [s, lines] of failures) console.log(`${s}: ${lines.length} failures\n  ` + lines.slice(0, 5).join("\n  "));
console.log(`suites: ${summary[1]} run, ${failed.length - unexpected.length} fail as expected` +
	` (${failed.filter(s => expected.has(s)).join(", ")})` + (unexpected.length ? `, ${unexpected.length} more fail` : ""));
for (const s of unexpected) {
	console.log(`suites: ${s} fails and is not expected to:`);
	const lines = Array.from(failures.entries()).filter(([k]) => k.includes(s) || s.includes(k)).flatMap(([, l]) => l);
	console.log("  " + (lines.length ? lines.slice(0, 12).join("\n  ") : "(see the output with --verbose)"));
}
for (const s of fixed) console.log(`suites: ${s} passes now: take it out of suites-expected.txt`);
process.exit(unexpected.length || fixed.length ? 1 : 0);
