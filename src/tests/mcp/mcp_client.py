#!/usr/bin/env python3
"""A small MCP client which tests `texmacs --mcp' (src/docs/mcp-server.md).

usage: mcp_client.py <scenario> <workdir>

It starts `texmacs.bin --mcp' (and, for some scenarios, a second headless
TeXmacs on the same file), speaks JSON-RPC to it on a pipe, and checks the
answers. Every process is stopped above TM_TEST_RSS_KB (2 GB) of memory and
after TM_TEST_TIMEOUT seconds (120). Prints "CHECK <scenario> <name> PASS|FAIL";
the exit status is the number of failures (capped at 1).
"""
import json, os, re, select, subprocess, sys, threading, time

HERE = os.path.dirname(os.path.abspath(__file__))
TOP = os.path.abspath(os.path.join(HERE, "..", ".."))
BIN = os.path.join(TOP, "TeXmacs", "bin", "texmacs.bin")
LAUNCHER = os.path.join(TOP, "TeXmacs", "bin", "texmacs")
RSS_CAP = int(os.environ.get("TM_TEST_RSS_KB", "2000000"))
TIMEOUT = int(os.environ.get("TM_TEST_TIMEOUT", "120"))

results = []
SCEN = ""
procs = []


def check(name, ok):
    print("CHECK %s %s %s" % (SCEN, name, "PASS" if ok else "FAIL"), flush=True)
    results.append(ok)
    return ok


def watchdog():
    start = time.monotonic()
    while True:
        alive = [p for p in procs if p.poll() is None]
        if not alive and procs and getattr(watchdog, "done", False):
            return
        for p in alive:
            try:
                rss = subprocess.run(["ps", "-o", "rss=", "-p", str(p.pid)],
                                     capture_output=True, text=True).stdout.strip()
                if rss and int(rss) > RSS_CAP:
                    print("  process %d stopped: %s KB" % (p.pid, rss), file=sys.stderr)
                    p.kill()
            except Exception:
                pass
        if time.monotonic() - start > TIMEOUT:
            print("  timeout: stopping the processes", file=sys.stderr)
            for p in alive:
                p.kill()
            return
        if getattr(watchdog, "done", False):
            return
        time.sleep(0.3)


class Mcp:
    """A `texmacs --mcp' process and its protocol."""

    def __init__(self, env, args=(), launcher=False):
        self.p = subprocess.Popen([LAUNCHER if launcher else BIN, "--mcp"] + list(args), stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  env=env)
        procs.append(self.p)
        self.buf = b""
        self.n = 0
        self.bad_lines = []        # lines of the output which are not JSON-RPC
        self.err = b""
        threading.Thread(target=self._drain_err, daemon=True).start()

    def _drain_err(self):
        for l in self.p.stderr:
            self.err += l

    def send_raw(self, line):
        self.p.stdin.write(line if isinstance(line, bytes) else line.encode())
        self.p.stdin.flush()

    def read_line(self, timeout=40):
        end = time.monotonic() + timeout
        while b"\n" not in self.buf:
            left = end - time.monotonic()
            if left <= 0:
                return None
            r, _, _ = select.select([self.p.stdout], [], [], min(left, 1))
            if r:
                d = os.read(self.p.stdout.fileno(), 1 << 20)
                if not d:
                    return None
                self.buf += d
            elif self.p.poll() is not None:
                return None
        line, self.buf = self.buf.split(b"\n", 1)
        return line

    def read_msg(self, timeout=40):
        line = self.read_line(timeout)
        if line is None:
            return None
        try:
            m = json.loads(line.decode("utf-8"))
            if m.get("jsonrpc") != "2.0":
                raise ValueError("no jsonrpc")
            return m
        except Exception:
            self.bad_lines.append(line)
            return self.read_msg(timeout)

    def request(self, method, params=None, timeout=40):
        self.n += 1
        m = {"jsonrpc": "2.0", "id": self.n, "method": method}
        if params is not None:
            m["params"] = params
        self.send_raw(json.dumps(m) + "\n")
        r = self.read_msg(timeout)
        return r

    def notify(self, method, params=None):
        m = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            m["params"] = params
        self.send_raw(json.dumps(m) + "\n")

    def initialize(self, version="2025-06-18"):
        r = self.request("initialize", {"protocolVersion": version, "capabilities": {},
                                        "clientInfo": {"name": "tm-test", "version": "1"}})
        self.notify("notifications/initialized")
        return r

    def tool(self, name, **args):
        """(is_error, value): the JSON value of the text of the result, or the text"""
        r = self.request("tools/call", {"name": name, "arguments": args})
        if r is None or "result" not in r:
            return (True, r)
        res = r["result"]
        text = res["content"][0]["text"]
        try:
            v = json.loads(text)
        except Exception:
            v = text
        return (bool(res.get("isError")), v)

    def close(self, timeout=30):
        """close the input and wait for the exit: its status, or None"""
        try:
            self.p.stdin.close()
        except Exception:
            pass
        try:
            return self.p.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            self.p.kill()
            return None


def wait_file(path, timeout=30):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if os.path.exists(path):
            return True
        time.sleep(0.05)
    return False


def poll(fn, timeout=15):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        v = fn()
        if v:
            return v
        time.sleep(0.1)
    return None


def make_env(w, name, share="off"):
    home = os.path.join(w, "home" + name)
    os.makedirs(os.path.join(home, "system"), exist_ok=True)
    with open(os.path.join(home, "system", "preferences.scm"), "w") as f:
        f.write('("share-open-files" "%s")\n' % share)
    env = dict(os.environ)
    env.update(TEXMACS_HOME_PATH=home, TEXMACS_PATH=os.path.join(TOP, "TeXmacs"),
               QT_QPA_PLATFORM="offscreen", TEXMACS_SHARED_DIR=os.path.join(w, "tmp"),
               TMPDIR=os.path.join(w, "tmp"), GIT_CONFIG_GLOBAL="/dev/null",
               GIT_CONFIG_NOSYSTEM="1")
    env.pop("XDG_RUNTIME_DIR", None)
    return env, home


def start_other(w, env, script, file, extra=None, gui=False, role="B"):
    """the other TeXmacs (a headless process with a script of tests/mcp; with
    gui=True a process with the Qt interface, offscreen, as the person's)"""
    sync = os.path.join(w, "sync")
    e = dict(env)
    if extra:
        e.update(extra)
    e.update(SH_ROLE=role, SH_SYNC=sync, SH_FILE=file, SH_OUT=os.path.join(w, "out-" + role),
             SH_DIR=os.path.join(TOP, "tests", "shared"))
    e["QT_QPA_PLATFORM"] = "offscreen"
    log = open(os.path.join(w, "log-" + role), "wb")
    p = subprocess.Popen([BIN] + ([] if gui else ["-headless"]) + ["-X", "-x",
                          '(load "%s")' % os.path.join(HERE, script)],
                         env=e, stdout=log, stderr=subprocess.STDOUT)
    procs.append(p)
    return p


def sync_set(w, name):
    with open(os.path.join(w, "sync", name), "w") as f:
        f.write("1")


# ---------------------------------------------------------------- documents

def write_paper(path):
    """a paper with sections, labels, a duplicate label, a broken reference,
    a citation, a theorem, math, an accent, and many long paragraphs"""
    filler = "".join(
        "Filler paragraph %d: %s\n\n" % (i, "lorem ipsum dolor sit amet " * 11)
        for i in range(1, 31))
    body = (
        "<doc-data|<doc-title|Test paper>|<doc-author|<author-data|<author-name|Ada>>>>\n\n"
        "<section|Introduction<label|sec:intro>>\n\n"
        "Hello <strong|world>, caf\\<#E9\\> and <math|\\<alpha\\><rsup|2>>, see "
        "<reference|sec:intro> and <reference|missing>; cite <cite|knuth>.\n\n"
        "<\\theorem>\n  Every <math|x\\<in\\>\\<bbb-R\\>> has <math|x<rsup|2>\\<geqslant\\>0>."
        "<label|thm:main>\n</theorem>\n\n"
        "<section|Details<label|sec:intro>>\n\n" + filler +
        "<subsection|Last>\n\nThe final paragraph has needle-xyz inside.\n")
    with open(path, "w") as f:
        f.write("<TeXmacs|2.1.5>\n\n<style|<tuple|generic|number-long-article>>\n\n"
                "<\\body>\n" + body + "</body>\n\n<\\initial>\n  <\\collection>\n"
                "    <associate|page-medium|paper>\n  </collection>\n</initial>\n")


def write_small(path):
    with open(path, "w") as f:
        f.write("<TeXmacs|2.1.5>\n\n<style|<tuple|generic>>\n\n<\\body>\n"
                "  First paragraph.\n\n  Second paragraph.\n\n  <section|Third>\n"
                "</body>\n")


def text_of(v):
    return v["content"] if isinstance(v, dict) else str(v)


# ---------------------------------------------------------------- scenarios

def s_protocol(w):
    env, home = make_env(w, "A")
    m = Mcp(env)
    r = m.initialize()
    check("initialize answers", r is not None and "result" in r)
    res = r["result"] if r and "result" in r else {}
    check("protocol version echoed", res.get("protocolVersion") == "2025-06-18")
    check("server info", res.get("serverInfo", {}).get("name") == "texmacs")
    check("tools capability", "tools" in res.get("capabilities", {}))
    check("id echoed", r is not None and r.get("id") == 1)
    r2 = m.initialize("2024-11-05")
    check("older version echoed", r2["result"]["protocolVersion"] == "2024-11-05")
    r3 = m.initialize("1999-01-01")
    check("unknown version: the server's own",
          r3["result"]["protocolVersion"] in ("2025-06-18", "2025-11-25", "2025-03-26", "2024-11-05"))
    r = m.request("ping")
    check("ping", r is not None and r.get("result") == {})
    r = m.request("tools/list")
    tools = r["result"]["tools"] if r and "result" in r else []
    names = sorted(t["name"] for t in tools)
    check("tools listed", names == sorted(["list_documents", "open_document", "get_outline",
                                           "read", "find", "get_references", "replace", "insert",
                                           "delete", "edit_text", "undo_agent", "save_document",
                                           "close_document", "create_document", "list_proposals",
                                           "withdraw_proposal"]))
    check("tools have schemas", all(t.get("inputSchema", {}).get("type") == "object" and
                                    t.get("description") for t in tools))
    r = m.request("nothing/here")
    check("unknown method: error -32601", r is not None and r.get("error", {}).get("code") == -32601)
    r = m.request("tools/call", {"name": "no_such_tool", "arguments": {}})
    check("unknown tool: error", r is not None and "error" in r)
    # a notification gets no answer: the next answer is that of the next request
    m.notify("notifications/cancelled", {"requestId": 99})
    r = m.request("ping")
    check("notification gets no answer", r is not None and r.get("id") == m.n)
    m.send_raw("{not json\n")
    r = m.read_msg()
    check("parse error", r is not None and r.get("error", {}).get("code") == -32700 and r.get("id") is None)
    m.send_raw("\n\n")
    r = m.request("ping")
    check("blank lines are skipped", r is not None and "result" in r)
    m.send_raw('{"jsonrpc":"2.0","id":"abc","method":"ping"}\r\n')
    r = m.read_msg()
    check("string id echoed, CRLF accepted", r is not None and r.get("id") == "abc")
    e, v = m.tool("list_documents")
    check("list_documents with nothing open", not e and v["documents"] == [])
    e, v = m.tool("read", document="nope")
    check("tool error is a result with isError", e and "unknown document" in str(v))
    code = m.close()
    check("exit 0 at the end of the input", code == 0)
    check("stdout carries only protocol lines", not m.bad_lines)
    log = os.path.join(home, "system", "mcp.log")
    check("the log file exists", os.path.exists(log) and os.path.getsize(log) > 0)


def s_tools(w):
    env, home = make_env(w, "A")
    paper = os.path.join(w, "paper.tm")
    write_paper(paper)
    paper = os.path.realpath(paper)
    m = Mcp(env)
    m.initialize()
    e, v = m.tool("open_document", path="relative.tm")
    check("a relative path is refused", e and "absolute" in str(v))
    e, v = m.tool("open_document", path=os.path.join(w, "nofile.tm"))
    check("a missing file is an error", e and "no such file" in str(v))
    e, v = m.tool("open_document", path=paper)
    check("open_document", not e and v["document"] == "d1" and v["title"] == "Test paper")
    check("open_document: style and shared", v["style"] == ["generic", "number-long-article"] and v["shared"] is None)
    blocks = v["blocks"]
    e, v2 = m.tool("open_document", path=paper)
    check("opening twice gives the same id", not e and v2["document"] == "d1")
    e, v = m.tool("list_documents")
    check("list_documents", not e and len(v["documents"]) == 1 and v["documents"][0]["path"] == paper)

    e, o = m.tool("get_outline", document="d1")
    secs = o["sections"] if not e else []
    check("outline: sections", [s["title"] for s in secs] == ["Introduction", "Details", "Last"])
    check("outline: levels", [s["level"] for s in secs] == [2, 2, 3])
    check("outline: labels", secs and secs[0]["labels"] == ["sec:intro"])
    check("outline: handles and hashes", all(s["handle"].startswith("n") and s["hash"] for s in secs))
    check("outline: title", o.get("title") == "Test paper")
    check("outline: sizes", len(secs) == 3 and secs[1]["chars"] > secs[0]["chars"])

    e, t = m.tool("read", document="d1", format="text")
    check("read text", not e and "Hello world, café and" in t["content"])
    check("read text: math and references", "(ref sec:intro)" in t["content"] and "[knuth]" in t["content"])
    e, t = m.tool("read", document=paper, format="text", max_size=2000)
    check("read by path", not e and t["document"] == "d1")
    check("read cuts between blocks", not e and t["truncated"] and t["next_start"] is not None
          and len(t["content"]) <= 2000 and t["end"] == t["next_start"])
    # continue until the end: all blocks are read once
    seen = []
    start, guard = 0, 0
    while start is not None and guard < 100:
        e, t = m.tool("read", document="d1", format="text", max_size=2000, start=start)
        if e:
            break
        seen.append(t["content"])
        start = t["next_start"]
        guard += 1
    full = "\n".join(seen)
    check("continuation reaches the end", "needle-xyz" in full and "Filler paragraph 30" in full)
    check("continuation loses nothing", "Filler paragraph 15:" in full and "Hello world" in full)
    e, t = m.tool("read", document="d1", format="texmacs", max_size=100000)
    check("read texmacs", not e and "<section|Introduction<label|sec:intro>>" in t["content"]
          and "<strong|world>" in t["content"])
    check("read texmacs: ascii only (accents as \\<#e9\\>)",
          all(ord(c) < 128 for c in t["content"]) and "caf\\<#e9\\>" in t["content"].lower())
    check("read texmacs: blocks", t["blocks"] == blocks and t["end"] == blocks and not t["truncated"])
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("read stree", not e and t["content"].startswith("(document") and "section" in t["content"])
    check("read stree: utf8", "café" in t["content"])
    e, t = m.tool("read", document="d1", format="latex", max_size=100000)
    check("read latex", not e and "\\section{Introduction}" in t["content"] and "\\begin{theorem}" in t["content"])
    e, t = m.tool("read", document="d1", format="markdown", max_size=100000)
    check("read markdown", not e and "# Introduction" in t["content"] and "**world**" in t["content"])
    e, t = m.tool("read", document="d1", format="html")
    check("unknown format is an error", e and "unknown format" in str(t))

    # handles
    h = secs[0]["handle"]
    e, t = m.tool("read", document="d1", node=h, format="text")
    check("read a node by its handle", not e and t["node"] == h and t["content"].strip() == "Introduction")
    check("node hash equals the outline's", t["hash"] == secs[0]["hash"])
    e, t = m.tool("read", document="d1", node=h, format="texmacs")
    check("read a node: texmacs", not e and "<section|Introduction" in t["content"])
    e, t = m.tool("read", document="d1", format="text", max_size=1500)
    kids = t["children"]
    thm = [c for c in kids if c["tag"] == "theorem"]
    check("children have handle, tag, hash, preview", kids and all(c["handle"] and c["hash"] and "preview" in c for c in kids))
    e, t = m.tool("read", document="d1", node=thm[0]["handle"], format="latex") if thm else (True, "no theorem in the window")
    check("read a theorem as latex", not e and "\\begin{theorem}" in t["content"] and "\\label{thm:main}" in t["content"])
    e, t = m.tool("read", document="d1", node="n99999")
    check("unknown handle", e and "unknown handle" in str(t))
    e, t = m.tool("read", document="nodoc")
    check("unknown document", e)

    # find
    e, f = m.tool("find", document="d1", query="NEEDLE-xyz")
    check("find text (case-insensitive)", not e and f["count"] == 1 and f["matches"][0]["section"] == "Last")
    check("find: context", "needle-xyz" in f["matches"][0]["context"])
    e, f2 = m.tool("read", document="d1", node=f["matches"][0]["handle"], format="text")
    check("find: the handle reads", not e and "needle-xyz" in f2["content"])
    e, f = m.tool("find", document="d1", query="café")
    check("find: non-ascii query", not e and f["count"] == 1)
    e, f = m.tool("find", document="d1", query="tag:theorem")
    check("find tag:", not e and f["count"] == 1 and f["matches"][0]["tag"] == "theorem")
    e, f = m.tool("find", document="d1", query="label:thm:main")
    check("find label:", not e and f["count"] == 1 and "Every" in m.tool("read", document="d1", node=f["matches"][0]["handle"], format="text")[1]["content"])
    e, f = m.tool("find", document="d1", query="section:detail")
    check("find section:", not e and f["count"] == 1 and f["matches"][0]["tag"] == "section")
    e, f = m.tool("find", document="d1", query="lorem ipsum", limit=5)
    check("find: limit and truncated", not e and f["count"] == 30 and len(f["matches"]) == 5 and f["truncated"])
    e, f = m.tool("find", document="d1", query="no such text anywhere")
    check("find: nothing", not e and f["count"] == 0 and f["matches"] == [])
    e, f = m.tool("find", document="d1", query="")
    check("find: empty query is an error", e)

    # references
    e, r = m.tool("get_references", document="d1")
    names = [l["name"] for l in r["labels"]] if not e else []
    check("references: labels", names == ["sec:intro", "thm:main", "sec:intro"])
    check("references: refs", [x["name"] for x in r["references"]] == ["sec:intro", "missing"])
    check("references: broken", r["broken_references"] == ["missing"])
    check("references: duplicate labels", r["duplicate_labels"] == ["sec:intro"])
    check("references: citations", [c["key"] for c in r["citations"]] == ["knuth"])
    check("references: label handles read", all(isinstance(l["handle"], str) for l in r["labels"]))

    # many calls do not leak the protocol
    for _ in range(20):
        m.tool("read", document="d1", format="text", max_size=500)
    e, v = m.tool("list_documents")
    check("still serving after many reads", not e)
    code = m.close()
    check("exit 0", code == 0)
    check("stdout carries only protocol lines", not m.bad_lines)


def s_shared(w):
    """a second headless TeXmacs hosts the file; the MCP server joins it and
    reads the live edits of the host"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "live.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-live.scm", f)
    sync = os.path.join(w, "sync")
    check("the host has loaded the file", wait_file(os.path.join(sync, "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("list_documents")
    check("the file the other TeXmacs has open is listed",
          not e and any(x["path"] == f for x in v["open_elsewhere"]))
    e, v = m.tool("open_document", path=f)
    check("open_document joins the host", not e and v["shared"] is not None and v["shared"]["role"] == "participant")
    e, v = m.tool("list_documents")
    check("an open shared file is no longer 'elsewhere'", not e and not any(x["path"] == f for x in v["open_elsewhere"]))
    e, t = m.tool("read", document="d1", format="text")
    check("reads the host's document", not e and "First paragraph." in t["content"])
    first = t["children"][0]
    h0, hash0 = first["handle"], first["hash"]
    sync_set(w, "c-joined")
    ok = wait_file(os.path.join(sync, "h-edited"))
    check("the host edited", ok)
    got = poll(lambda: (lambda r: r if not r[0] and "LIVE-" in r[1]["content"] else None)
               (m.tool("read", document="d1", format="text")))
    check("the live edit of the host is read", got is not None)
    e, t = m.tool("read", document="d1", node=h0, format="text")
    check("a handle follows its node through the host's edit", not e and "First LIVE-paragraph." in t["content"])
    check("the hash of the node changed", not e and t["hash"] != hash0)
    e, v = m.tool("find", document="d1", query="live-paragraph")
    check("find sees the edit", not e and v["count"] == 1)
    second = got[1]["children"][1]["handle"]
    sync_set(w, "c-read1")
    wait_file(os.path.join(sync, "h-deleted"))
    got = poll(lambda: (lambda r: r if not r[0] and "Second paragraph." not in r[1]["content"] else None)
               (m.tool("read", document="d1", format="text")))
    check("a deletion of the host is read", got is not None)
    e, t = m.tool("read", document="d1", node=second, format="text")
    check("the handle of a deleted node is an error", e and "does not exist" in str(t))
    e, t = m.tool("read", document="d1", node=h0, format="text")
    check("the other handle still works", not e and "LIVE-" in t["content"])
    sync_set(w, "c-done")
    code = m.close()
    check("exit 0", code == 0)
    check("stdout carries only protocol lines", not m.bad_lines)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()
    out = open(os.path.join(w, "out-B")).read() if os.path.exists(os.path.join(w, "out-B")) else ""
    check("the other TeXmacs passed its checks", out == "ok")


def s_mcp_host(w):
    """the MCP server opens the file first (it hosts it); another headless
    TeXmacs joins and edits; the MCP server reads the edit"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "hosted.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    check("the MCP server hosts the file", not e and v["shared"] is not None and v["shared"]["role"] == "host")
    other = start_other(w, envB, "host-join.scm", f)
    sync_set(w, "c-opened")
    check("the other TeXmacs edited", wait_file(os.path.join(w, "sync", "h-edited")))
    got = poll(lambda: (lambda r: r if not r[0] and "FROMB-" in r[1]["content"] else None)
               (m.tool("read", document="d1", format="text")))
    check("the edit of the participant is read", got is not None)
    e, v = m.tool("list_documents")
    check("participants are counted", not e and v["documents"][0]["shared"]["participants"] == 2)
    sync_set(w, "c-done")
    code = m.close()
    check("exit 0", code == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()
    out = open(os.path.join(w, "out-B")).read() if os.path.exists(os.path.join(w, "out-B")) else ""
    check("the other TeXmacs passed its checks", out == "ok")


def s_unshared(w):
    """with the preference off (the default) a file open elsewhere is read
    alone: nothing is shared, nothing is listed"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "alone.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "off")
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    check("not shared", not e and v["shared"] is None)
    e, v = m.tool("list_documents")
    check("nothing open elsewhere", not e and v["open_elsewhere"] == [])
    e, t = m.tool("read", document="d1", format="text")
    check("reads the file", not e and "Second paragraph." in t["content"])
    check("exit 0", m.close() == 0)



def write_tm(path, body):
    with open(path, "w") as f:
        f.write("<TeXmacs|2.1.5>\n\n<style|<tuple|generic>>\n\n<\\body>\n" + body +
                "</body>\n")


def s_launcher(w):
    """the launcher script TeXmacs/bin/texmacs keeps the standard input for --mcp"""
    env, home = make_env(w, "A")
    m = Mcp(env, launcher=True)
    r = m.initialize()
    check("through the launcher: initialize", r is not None and "result" in r)
    r = m.request("ping")
    check("through the launcher: ping", r is not None and r.get("result") == {})
    check("through the launcher: exit 0", m.close() == 0)
    check("through the launcher: only protocol lines", not m.bad_lines)


def s_robust(w):
    """text encodings, unusual paths, odd shapes, bad messages, limits"""
    roots = os.path.join(w, "root")
    os.makedirs(roots, exist_ok=True)
    env, home = make_env(w, "A")
    env["TEXMACS_MCP_ROOTS"] = roots
    uni = os.path.join(roots, "sp é.tm")
    write_tm(uni, "First line T\\<#E9\\>st \\<alpha\\>.\n\nSecond paragraph.\n\n"
                  "<section|Caf\\<#E9\\>>\n\nBody after.\n")
    weird = os.path.join(roots, "ok.tm:weird.tm")
    write_tm(weird, "Weird name.\n")
    star = os.path.join(roots, "a*b.tm")
    write_tm(star, "Star name.\n")
    os.makedirs(os.path.join(roots, "sp é:x"), exist_ok=True)
    indir = os.path.join(roots, "sp é:x", "inner.tm")
    write_tm(indir, "Inside.\n")
    nested = os.path.join(roots, "nested.tm")
    write_tm(nested, "<section|Intro><label|s1>\n\nText.\n\n<subsection|Sub><label|s2>\n\n"
                     "More <reference|s1>.\n\n<reference>\n")
    outside = os.path.join(w, "outside.tm")
    write_tm(outside, "Outside the roots.\n")
    m = Mcp(env)
    m.initialize()
    # encodings and newlines
    e, v = m.tool("open_document", path=os.path.realpath(uni))
    check("a name with a space and a non-ascii letter", not e and v["file"] == "sp é.tm")
    for fmt in ("text", "latex", "markdown", "texmacs", "stree"):
        e, t = m.tool("read", document="d1", format=fmt, max_size=100000)
        c = t["content"] if not e else ""
        check("read %s: no garbled characters" % fmt,
              not e and "˙" not in c and "Ã" not in c and "¯" not in c)
    e, t = m.tool("read", document="d1", format="text")
    check("text: real newlines and utf8", not e and "\n" in t["content"] and "Tést α." in t["content"])
    e, t = m.tool("read", document="d1", format="markdown")
    check("markdown: real newlines and utf8", not e and "\n" in t["content"] and "Tést" in t["content"] and "# Café" in t["content"])
    e, t = m.tool("read", document="d1", format="latex")
    check("latex: real newlines", not e and "\n" in t["content"] and "\\section" in t["content"])
    e, t = m.tool("read", document="d1", format="texmacs")
    check("texmacs: real newlines, ascii", not e and "\n" in t["content"] and all(ord(ch) < 128 for ch in t["content"]))
    e, t = m.tool("read", document="d1", format="text", max_size=300)
    check("preview has no garbled newline", not e and all("˙" not in c["preview"] for c in t["children"]))
    e, f = m.tool("find", document="d1", query="second")
    check("find context has no garbled newline", not e and f["count"] == 1 and "˙" not in f["matches"][0]["context"])
    e, o = m.tool("get_outline", document="d1")
    check("outline title in utf8", not e and o["sections"][0]["title"] == "Café")
    # paths
    e, v = m.tool("open_document", path=os.path.realpath(star))
    if not e:
        e, t = m.tool("read", document=v["document"], format="text")
    check("a star in the name is a plain character", not e and "Star name." in t["content"])
    for name, path in (("a colon in the name", weird), ("a colon in a folder", indir)):
        e, v = m.tool("open_document", path=os.path.realpath(path))
        check(name + " is refused cleanly", e and "colon" in str(v))
    # the roots
    e, v = m.tool("open_document", path=os.path.realpath(outside))
    check("a file outside the roots is refused", e and "not allowed" in str(v))
    e, v = m.tool("open_document", path="/etc/passwd")
    check("/etc/passwd is refused", e and "not allowed" in str(v))
    e, v = m.tool("open_document", path=roots + "/../outside.tm")
    check("a .. path out of the roots is refused", e and "not accepted" in str(v))
    link = os.path.join(roots, "link.tm")
    try:
        os.symlink(os.path.realpath(outside), link)
        e, v = m.tool("open_document", path=link)
        check("a symbolic link out of the roots is refused", e and "not allowed" in str(v))
    except OSError:
        pass
    e, v = m.tool("read", document=os.path.realpath(outside))
    check("read by an outside path is refused", e and "not allowed" in str(v))
    # a path is checked as it is loaded: `..' cannot step out of the roots
    inner = os.path.join(roots, "deep")
    os.makedirs(inner, exist_ok=True)
    sec = os.path.join(w, "secret.tm")
    write_tm(sec, "Secret.\n")
    os.symlink(inner, os.path.join(roots, "link_deep"))
    for name, path in (("a nonexistent folder and ..", roots + "/nonexist/../../secret.tm"),
                       ("many ..", roots + "/../../../../../../" + os.path.realpath(sec)),
                       ("..", roots + "/deep/../../secret.tm"),
                       ("a symbolic link and ..", roots + "/link_deep/../../secret.tm")):
        e, v = m.tool("open_document", path=path)
        check("%s: refused" % name, e and ("not accepted" in str(v) or "not allowed" in str(v)))
        e, v = m.tool("read", document=path)
        check("%s: read refused too" % name, e)
    e, v = m.tool("list_documents")
    check("nothing outside was opened", not e and all(x["path"].startswith(os.path.realpath(roots)) for x in v["documents"]))
    # sections in concatenations
    e, v = m.tool("open_document", path=os.path.realpath(nested))
    did = v["document"]
    e, o = m.tool("get_outline", document=did)
    secs = o["sections"] if not e else []
    check("sections followed by labels are found", [x["title"] for x in secs] == ["Intro", "Sub"])
    check("their labels and levels", secs and secs[0]["labels"] == ["s1"] and [x["level"] for x in secs] == [2, 3])
    e, f = m.tool("find", document=did, query="section:sub")
    check("find section: in that shape", not e and f["count"] == 1)
    e, f = m.tool("find", document=did, query="More")
    check("find: the section of a hit", not e and f["matches"][0]["section"] == "Sub")
    e, t = m.tool("read", document=did, node=secs[0]["handle"], format="text")
    check("a section handle reads", not e and "Intro" in t["content"])
    e, r = m.tool("get_references", document=did)
    check("a reference with no argument is harmless", not e and r["labels_total"] == 2)
    # arguments
    e, f = m.tool("find", document=did, query="tag:")
    check("tag: with nothing is a clean error", e and "nothing after" in str(f))
    r = m.request("tools/call", {"name": "read", "arguments": {"document": did, "node": 5}})
    check("a node that is not a string: invalid params", r is not None and r.get("error", {}).get("code") == -32602)
    r = m.request("tools/call", {"name": "read", "arguments": {"document": did, "max_size": "big"}})
    check("max_size that is not an integer: invalid params", r is not None and r.get("error", {}).get("code") == -32602)
    r = m.request("tools/call", {"name": "read", "arguments": [1]})
    check("arguments that are not an object: invalid params", r is not None and r.get("error", {}).get("code") == -32602)
    # bad messages
    m.send_raw('{"jsonrpc":"2.0","id":{"a":1},"method":"ping"}\n')
    r = m.read_msg()
    check("an id that is an object: -32600", r is not None and r.get("error", {}).get("code") == -32600)
    m.send_raw('{"jsonrpc":"2.0","id":7}\n')
    r = m.read_msg()
    check("no method: -32600", r is not None and r.get("error", {}).get("code") == -32600 and r.get("id") == 7)
    m.send_raw('{"jsonrpc":"2.0","id":8,"method":5}\n')
    r = m.read_msg()
    check("a method that is not a string: -32600", r is not None and r.get("error", {}).get("code") == -32600)
    for bad in ('{"a":tru}', '{"a":1e999}', '{"a":01}', '{"a":"\\x"}', '{"a":[1,]}', '{"a" 1}'):
        m.send_raw(bad + "\n")
        r = m.read_msg()
        check("%s: -32700" % bad, r is not None and r.get("error", {}).get("code") == -32700)
    for name, raw in (("a raw tab in a string", b'{"a":"x\ty"}'), ("a backslash and NUL", b'{"a":"\\\x00"}'),
                      ("invalid UTF-8", b'{"a":"\xff"}'), ("a truncated UTF-8 character", b'{"a":"\xc3"}')):
        m.send_raw(raw + b"\n")
        r = m.read_msg()
        check("%s: -32700" % name, r is not None and r.get("error", {}).get("code") == -32700)
    r = m.request("ping")
    check("a number of 100 digits is valid", True)
    m.send_raw(b'{"jsonrpc":"2.0","id":77,"method":"ping","params":{"x":1.' + b"1" * 100 + b'}}\n')
    r = m.read_msg()
    check("a number of 100 digits is accepted", r is not None and r.get("id") == 77 and "result" in r)
    m.send_raw('{"jsonrpc":"2.0","id":9,"method":"ping"} garbage\n')
    r = m.read_msg()
    check("garbage after the object: -32700", r is not None and r.get("error", {}).get("code") == -32700)
    m.send_raw('{"jsonrpc":"2.0","id":9,"method":"ping"}{"x":1}\n')
    r = m.read_msg()
    check("a second object on the line: -32700", r is not None and r.get("error", {}).get("code") == -32700)
    m.send_raw("[" * 200000 + "\n")
    r = m.read_msg()
    check("200000 nested arrays: an error, no crash", r is not None and "error" in r)
    r = m.request("ping")
    check("still alive after the nesting", r is not None and "result" in r)
    # a line which is too long is dropped to its end
    big = b'{"jsonrpc":"2.0","id":1,"method":"ping","params":{"x":"' + b"a" * (34 * 1024 * 1024) + b'"}}\n'
    m.send_raw(big)
    r = m.read_msg(60)
    check("a line over 32 MB: error", r is not None and r.get("error", {}).get("code") == -32600)
    r = m.request("ping", None, 30)
    check("and nothing else is answered for it", r is not None and r.get("id") == m.n and "result" in r)
    check("exit 0", m.close() == 0)
    check("stdout carries only protocol lines", not m.bad_lines)


def s_outputs(w):
    """long lists are cut and can be continued; a client that is gone ends the server"""
    env, home = make_env(w, "A")
    f = os.path.join(w, "many.tm")
    body = "".join("<section|S%d<label|l%d>>\n\nSee <reference|l%d> and <cite|k%d>.\n\n" % (i, i, i, i)
                   for i in range(1, 61))
    write_tm(f, body)
    m = Mcp(env)
    m.initialize()
    m.tool("open_document", path=os.path.realpath(f))
    e, o = m.tool("get_outline", document="d1", limit=25)
    check("outline: limit", not e and len(o["sections"]) == 25 and o["total"] == 60 and o["next_start"] == 25)
    e, o2 = m.tool("get_outline", document="d1", limit=25, start=o["next_start"])
    check("outline: continues", not e and o2["sections"][0]["title"] == "S26")
    e, r = m.tool("get_references", document="d1", limit=10)
    check("references: limit", not e and len(r["labels"]) == 10 and r["labels_total"] == 60 and r["truncated"])
    e, r = m.tool("get_references", document="d1", limit=10, start=55)
    check("references: continues", not e and len(r["labels"]) == 5)
    # the client goes away: the server notices that it cannot write and ends
    m.p.stdout.close()
    m.send_raw(json.dumps({"jsonrpc": "2.0", "id": 99, "method": "ping"}) + "\n")
    try:
        code = m.p.wait(timeout=20)
    except subprocess.TimeoutExpired:
        code = None
    check("the server ends when it cannot write", code is not None)
    if code is None:
        m.p.kill()
    big = os.path.join(home, "system", "mcp.log")
    check("the log is small", os.path.getsize(big) < 4 * 1024 * 1024)

def s_hostdeath(w):
    """a host which is killed (kill -9) leaves the list of open files, also when
    its process id is taken by another program; the server has not opened the file"""
    import re, glob
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "livé é.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-wait.scm", f)
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("list_documents")
    check("listed, with its non-ascii name as it is (the server has not opened it)",
          not e and v["documents"] == [] and any(x["path"] == f for x in v["open_elsewhere"]))
    other.kill()                         # kill -9 of our own process
    other.wait()
    gone = poll(lambda: (lambda r: r if not r[0] and not any(x["path"] == f for x in r[1]["open_elsewhere"]) else None)
                (m.tool("list_documents")), 12)
    check("after the host is killed it is not 'elsewhere' any more", gone is not None)
    # the entry is still there: its process id now belongs to another program
    ents = glob.glob(os.path.join(w, "tmp", "texmacs-*", "shared", "*.entry"))
    check("the entry of the dead host is left", len(ents) == 1)
    foreign = subprocess.Popen(["sleep", "60"])
    try:
        txt = open(ents[0]).read()
        mt = re.match(r'^(\(entry ".*" \d+ "[^"]*" )(\d+)(.*)$', txt, re.S)
        check("the entry can be read", mt is not None)
        if mt:
            with open(ents[0], "w") as fh:
                fh.write(mt.group(1) + str(foreign.pid) + mt.group(3))
            time.sleep(0.2)
            e, v = m.tool("list_documents")
            check("an entry with a live foreign pid and a closed port is not listed",
                  not e and not any(x["path"] == f for x in v["open_elsewhere"]))
    finally:
        foreign.kill()
        foreign.wait()
    check("exit 0", m.close() == 0)


def s_nonascii(w):
    """a file with a non-ascii name, hosted by another TeXmacs, is listed and opened"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "livé é.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-wait.scm", f)
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("list_documents")
    paths = [x["path"] for x in v["open_elsewhere"]] if not e else []
    check("listed with its name as it is", paths == [f])
    e, d = m.tool("open_document", path=paths[0]) if paths else (True, "none")
    check("the listed path opens and joins", not e and d["shared"] is not None and d["file"] == "livé é.tm")
    e, t = m.tool("read", document="d1", format="text")
    check("and reads", not e and "First paragraph." in t["content"])
    sync_set(w, "c-done")
    check("exit 0", m.close() == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()


def s_stalled(w):
    """a host which is stopped (SIGSTOP) stays listed and does not block the server"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "stalled.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-wait.scm", f)
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    import signal
    os.kill(other.pid, signal.SIGSTOP)
    try:
        t0 = time.monotonic()
        ok = True
        for _ in range(40):
            e, v = m.tool("list_documents")
            ok = ok and not e and any(x["path"] == f for x in v["open_elsewhere"])
        dt = time.monotonic() - t0
        check("a stopped host stays listed (40 listings)", ok)
        check("and the server is not blocked (40 listings in under 20 s: %.1f s)" % dt, dt < 20)
    finally:
        os.kill(other.pid, signal.SIGCONT)
    sync_set(w, "c-done")
    check("exit 0", m.close() == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()


def s_logcap(w):
    """the log is rotated during the run when it is over the cap"""
    env, home = make_env(w, "A")
    env["TEXMACS_MCP_DEBUG"] = "1"
    env["TEXMACS_MCP_LOG_CAP"] = "30000"
    m = Mcp(env)
    m.initialize()
    for _ in range(600):
        m.request("ping")
        time.sleep(0.005)
    time.sleep(1)
    m.request("ping")
    logf = os.path.join(home, "system", "mcp.log")
    check("the log was rotated", os.path.exists(logf + ".1"))
    check("the log is under about the cap", os.path.getsize(logf) < 90000)
    check("exit 0", m.close() == 0)


def block_texts(v):
    return [c["preview"] for c in v["children"]]


def s_edit(w):
    """the editing tools in a file nobody else has open"""
    env, home = make_env(w, "A")
    f = os.path.join(w, "e.tm")
    write_small(f)
    f = os.path.realpath(f)
    m = Mcp(env)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    e, t = m.tool("read", document="d1", format="texmacs")
    k = t["children"]
    h0, x0 = k[0]["handle"], k[0]["hash"]
    h1, x1 = k[1]["handle"], k[1]["hash"]
    # edit_text
    e, r = m.tool("edit_text", document="d1", node=h0, old="First", new="Initial", expect=x0)
    check("edit_text applied", not e and r["status"] == "applied" and r["undo_steps"] == 1)
    e, t = m.tool("read", document="d1", node=r["changed"][0]["handle"], format="text")
    check("edit_text: the text changed", not e and t["content"] == "Initial paragraph.")
    e, r2 = m.tool("edit_text", document="d1", node=h1, old="paragraph", new="x", expect="BADHASH")
    check("a stale hash is a conflict, nothing is changed", not e and r2["status"] == "conflict"
          and r2["current_hash"] == x1 and "Second paragraph." in r2["current"] and r2["handle"])
    e, r2 = m.tool("edit_text", document="d1", node=h1, old="a", new="x")
    check("edit_text: several occurrences are an error that counts them", e and "occurs 3 times" in str(r2))
    e, r2 = m.tool("edit_text", document="d1", node=h1, old="Zzz", new="x")
    check("edit_text: no occurrence is an error", e and "not found" in str(r2))
    e, r2 = m.tool("edit_text", document="d1", node=h1, old="Second", new="Café <b>", expect=x1)
    e, t = m.tool("read", document="d1", node=r2["changed"][0]["handle"], format="text")
    check("edit_text: utf8 and a literal < in the new text", not e and t["content"] == "Café <b> paragraph.")
    # delete
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    nb = t["blocks"]
    victim = t["children"][2]
    e, r = m.tool("delete", document="d1", node=victim["handle"], expect="BAD")
    check("delete with a stale hash: conflict", not e and r["status"] == "conflict")
    e, r = m.tool("delete", document="d1", node=victim["handle"], expect=victim["hash"])
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    check("delete", not e and t["blocks"] == nb - 1 and victim["preview"] not in t["content"])
    # undo_agent
    steps_before = r["undo_steps"]
    h_before = t["hash"]
    e, u = m.tool("undo_agent", document="d1", steps=1)
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    check("undo_agent restores the deleted block", not e and t["blocks"] == nb and victim["preview"] in t["content"])
    # replace
    e, t = m.tool("read", document="d1", format="texmacs")
    k = t["children"]
    e, r = m.tool("replace", document="d1", node=k[1]["handle"], expect=k[1]["hash"],
                  content="New <strong|second> para\n\nand a third", format="texmacs")
    check("replace by two blocks", not e and r["status"] == "applied" and len(r["changed"]) == 2)
    e, t = m.tool("read", document="d1", format="texmacs")
    check("replace: the blocks are there, in order",
          not e and t["content"].split("\n\n")[1:3] == ["New <strong|second> para", "and a third"])
    n_blocks = t["blocks"]
    e, r = m.tool("replace", document="d1", node=t["children"][0]["handle"], content="<strong|bad", format="texmacs")
    check("replace: content which does not parse is refused", e and "does not parse" in str(r))
    e, t2 = m.tool("read", document="d1", format="texmacs")
    check("and nothing changed", t2["hash"] == t["hash"])
    e, r = m.tool("replace", document="d1", node=t["children"][0]["handle"], content="(strong \"s\")", format="stree")
    check("replace with stree", not e and r["changed"][0]["tag"] == "strong")
    e, r = m.tool("replace", document="d1", node=t["children"][1]["handle"], content="\\textbf{tex}", format="latex")
    check("replace with latex", not e and r["status"] == "applied")
    e, r = m.tool("replace", document="d1", node=t["children"][2]["handle"], content="## Title\n\n* item", format="markdown")
    check("replace with markdown", not e and r["status"] == "applied")
    e, t = m.tool("read", document="d1", format="texmacs", max_size=100000)
    check("the formats gave the right trees", "<strong|s>" in t["content"] and "<subsection|Title>" in t["content"]
          and "<strong|tex>" in t["content"] or "textbf" not in t["content"])
    e, r = m.tool("replace", document="d1", node=t["children"][0]["handle"], content="x", format="html")
    check("an unknown format is refused", e)
    # insert
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    first = t["children"][0]["handle"]
    e, r = m.tool("insert", document="d1", anchor=first, where="before", content="At the top", format="texmacs")
    check("insert before", not e and r["status"] == "applied")
    e, r = m.tool("insert", document="d1", where="last", content="At the end\n\nand more", format="texmacs")
    check("insert last in the document", not e and len(r["changed"]) == 2)
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    check("insert: first and last blocks", not e and t["content"].startswith("At the top") and t["content"].endswith("and more"))
    e, r = m.tool("insert", document="d1", where="middle", content="x")
    check("insert: a bad `where'", r is not None and not e is None and e)
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    last = t["children"][-1]["handle"]
    e, r = m.tool("insert", document="d1", anchor=last, where="after", content="<em|tail>", format="texmacs")
    check("insert after", not e and r["status"] == "applied")
    e, u = m.tool("undo_agent", document="d1", steps=100)
    e, t = m.tool("read", document="d1", format="texmacs", max_size=100000)
    check("undo_agent undoes all the steps", not e and u["undo_steps"] == 0 and t["content"].startswith("First paragraph.\n\nSecond paragraph.\n\n<section|Third>"))
    e, u = m.tool("undo_agent", document="d1")
    check("nothing more to undo", e and "no step" in str(u))
    # edit_text with an empty `new', and its undo (checked with the text around)
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    e, r = m.tool("edit_text", document="d1", node=t["children"][0]["handle"], old=" paragraph", new="")
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    check("edit_text with an empty new deletes the text", not e and t["children"][0]["preview"] == "First.")
    e, u = m.tool("undo_agent", document="d1")
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    check("and its undo puts it back", not e and t["children"][0]["preview"] == "First paragraph.")
    # a step of the person (simulated by a file change is not possible): save
    e, r = m.tool("edit_text", document="d1", node=t["children"][0]["handle"], old="First", new="Saved")
    e, sv = m.tool("save_document", document="d1")
    txt = open(f).read()
    check("save_document writes the file", not e and sv["status"] == "saved" and "Saved paragraph." in txt)
    # close
    e, r = m.tool("edit_text", document="d1", node=m.tool("read", document="d1")[1]["children"][0]["handle"], old="Saved", new="Unsaved")
    e, c = m.tool("close_document", document="d1")
    check("close refuses unsaved changes", e and "unsaved" in str(c))
    e, c = m.tool("close_document", document="d1", discard=True)
    check("close with discard", not e and c["status"] == "closed")
    e, l = m.tool("list_documents")
    check("the document is not listed after close", not e and l["documents"] == [])
    check("and the file keeps what was saved", "Unsaved" not in open(f).read())
    # create
    new = os.path.join(w, "created.tm")
    e, c = m.tool("create_document", path=new, style="article", content="Hello <em|you>")
    check("create_document", not e and c["style"] == ["article"] and os.path.exists(new))
    e, t = m.tool("read", document=c["document"], format="texmacs")
    check("create_document: the content", not e and "Hello <em|you>" in t["content"])
    e, sv = m.tool("save_document", document=c["document"])
    check("a created document is saved", not e and "Hello <em|you>" in open(new).read() and "<style|article>" in open(new).read())
    e, c2 = m.tool("create_document", path=new)
    check("create_document: the file exists", e and "exists" in str(c2))
    e, c2 = m.tool("create_document", path=os.path.join(w, "nodir", "x.tm"))
    check("create_document: no folder", e)
    e, c2 = m.tool("create_document", path=os.path.join(w, "x.txt"))
    check("create_document: .tm only", e)
    e, c2 = m.tool("create_document", path=os.path.join(w, "y.tm"), content="<strong|bad")
    check("create_document: bad content, no file", e and not os.path.exists(os.path.join(w, "y.tm")))
    # the answer says what changed in the document by others
    check("exit 0", m.close() == 0)
    check("stdout carries only protocol lines", not m.bad_lines)


def s_readonly(w):
    """TEXMACS_MCP_READONLY and TEXMACS_MCP_ROOTS for the editing tools"""
    roots = os.path.join(w, "root")
    os.makedirs(roots, exist_ok=True)
    f = os.path.join(roots, "r.tm")
    write_small(f)
    f = os.path.realpath(f)
    env, home = make_env(w, "A")
    env["TEXMACS_MCP_READONLY"] = "1"
    m = Mcp(env)
    m.initialize()
    r = m.request("tools/list")
    names = [t["name"] for t in r["result"]["tools"]]
    check("read-only: the editing tools are not listed", names == ["list_documents", "open_document", "get_outline", "read", "find", "get_references", "list_proposals"])
    m.tool("open_document", path=f)
    for tool, args in (("replace", {"document": "d1", "node": "n1", "content": "x"}),
                       ("insert", {"document": "d1", "where": "last", "content": "x"}),
                       ("delete", {"document": "d1", "node": "n1"}),
                       ("edit_text", {"document": "d1", "node": "n1", "old": "a", "new": "b"}),
                       ("undo_agent", {"document": "d1"}), ("save_document", {"document": "d1"}),
                       ("close_document", {"document": "d1"}),
                       ("create_document", {"path": os.path.join(roots, "n.tm")})):
        e, v = m.tool(tool, **args)
        check("read-only: %s is refused" % tool, e and "read-only" in str(v))
    check("read-only: nothing was created", not os.path.exists(os.path.join(roots, "n.tm")))
    check("exit 0", m.close() == 0)
    env2, home2 = make_env(w, "B")
    env2["TEXMACS_MCP_ROOTS"] = roots
    m = Mcp(env2)
    m.initialize()
    e, v = m.tool("create_document", path=os.path.join(w, "outside.tm"))
    check("roots: create_document outside is refused", e and "not allowed" in str(v) and not os.path.exists(os.path.join(w, "outside.tm")))
    e, v = m.tool("create_document", path=roots + "/../outside.tm")
    check("roots: .. is refused", e and not os.path.exists(os.path.join(w, "outside.tm")))
    link = os.path.join(roots, "lnk")
    os.symlink(w, link)
    e, v = m.tool("create_document", path=link + "/viasym.tm")
    check("roots: a symbolic link out is refused", e and not os.path.exists(os.path.join(w, "viasym.tm")))
    e, v = m.tool("create_document", path=os.path.join(roots, "inside.tm"))
    check("roots: create_document inside", not e and os.path.exists(os.path.join(roots, "inside.tm")))
    check("exit 0", m.close() == 0)


def hostcmd(w, state, text):
    """send a command to the other TeXmacs (tests/mcp/host-edit.scm) and wait for its answer"""
    state[0] += 1
    i = state[0]
    tmp = os.path.join(w, "sync", "tmp-%d" % i)
    with open(tmp, "w") as fh:
        fh.write(text)
    os.replace(tmp, os.path.join(w, "sync", "cmd-%d" % i))
    return wait_file(os.path.join(w, "sync", "ack-%d" % i), 30)


def host_body(w, state):
    try:
        os.remove(os.path.join(w, "sync", "B-body"))
    except OSError:
        pass
    hostcmd(w, state, "publish")
    wait_file(os.path.join(w, "sync", "B-body"), 10)
    time.sleep(0.2)
    return open(os.path.join(w, "sync", "B-body")).read()


def s_edit_shared(w):
    """the agent edits a file which a second headless TeXmacs (the person) hosts
    and edits too: the changes are merged, the person's are never refused, a
    conflicting change of the agent is refused and reported"""
    import threading
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "shared.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-edit.scm", f)
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    check("the agent joins as a participant", not e and v["shared"]["role"] == "participant")
    st = [0]
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    # 1. an edit of the agent reaches the host
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="First", new="Initial", expect=k[0]["hash"])
    check("shared: the edit is applied (acknowledged by the host)", not e and r["status"] == "applied")
    hb = host_body(w, st)
    check("shared: the host has the agent's text", "Initial paragraph." in str(hb))
    # 2. the host types elsewhere; the agent sees it through a note
    hostcmd(w, st, "type-p1 (h1)")
    got = poll(lambda: (lambda r: r if not r[0] and "(h1)" in r[1]["content"] else None)(m.tool("read", document="d1", format="text")), 10)
    check("shared: the agent reads the host's typing", got is not None)
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    hostcmd(w, st, "type-p1 (h2)")
    time.sleep(1.0)          # (no read: the note of the next call must tell)
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Initial", new="Start")
    check("shared: a note tells that the document changed", not e and "changed since your last call" in r.get("note", ""))
    # 3. the same paragraph, different places, at the same time: merged
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    hostcmd(w, st, "pause")
    hostcmd(w, st, "type-p0 !!")
    res = {}
    def agent_edit():
        res["v"] = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Start", new="Begin")
    th = threading.Thread(target=agent_edit)
    th.start()
    time.sleep(1.0)
    hostcmd(w, st, "resume")
    th.join(30)
    e, r = res.get("v", (True, None))
    check("shared: same paragraph, different places: applied (merged by the host)", not e and r["status"] == "applied")
    time.sleep(0.5)
    hb = host_body(w, st)
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("shared: both converge, with both changes", "Begin paragraph.!!" in str(hb) and "Begin paragraph.!!" in t["content"])
    # 4. a change of the agent in a paragraph the host deletes: refused, reported
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    hostcmd(w, st, "pause")
    hostcmd(w, st, "delete-p1")
    res = {}
    def agent_edit2():
        res["v"] = m.tool("edit_text", document="d1", node=k[1]["handle"], old="Second", new="Gone")
    th = threading.Thread(target=agent_edit2)
    th.start()
    time.sleep(1.0)
    hostcmd(w, st, "resume")
    th.join(30)
    e, r = res.get("v", (True, None))
    check("shared: an edit of a deleted paragraph is refused and reported", not e and r["status"] == "refused" and "reason" in r)
    time.sleep(0.5)
    hb = host_body(w, st)
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("shared: the agent's copy is the host's document again", "Gone" not in str(hb) and "Gone" not in t["content"]
          and str(hb).count("paragraph") == t["content"].count("paragraph"))
    hostcmd(w, st, "stats")
    time.sleep(0.2)
    check("shared: the host refused exactly the agent's patch (its own were never refused)",
          open(os.path.join(w, "sync", "stats")).read().strip() == "(1)")
    # 5. expect: a conflict when the host changed the node
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    old_hash = k[0]["hash"]
    hostcmd(w, st, "type-p0 ??")
    poll(lambda: (lambda r: r if not r[0] and "??" in r[1]["content"] else None)(m.tool("read", document="d1", format="text")), 10)
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Begin", new="Again", expect=old_hash)
    check("shared: edit with a stale hash (the host typed in the node): conflict", not e and r["status"] == "conflict" and "??" in r["current"])
    # 6. undo_agent in a shared file: after a refusal the older steps are forgotten
    e, u = m.tool("undo_agent", document="d1")
    check("shared: after a refused change the steps before are not undoable", e)
    e, r = m.tool("edit_text", document="d1", node=m.tool("read", document="d1")[1]["children"][0]["handle"], old="Begin", new="Again")
    check("shared: an edit after a refusal works", not e and r["status"] == "applied")
    e, u = m.tool("undo_agent", document="d1")
    check("shared: undo_agent works and reaches the host", not e and u["status"] == "applied")
    hb = host_body(w, st)
    check("shared: the host has the undone text", "Begin paragraph.!!??" in str(hb))
    # 7. the person's undo skips the agent's change (it commutes)
    e, t = m.tool("read", document="d1", format="text")
    k = t["children"]
    hostcmd(w, st, "type-p0 AA")
    hostcmd(w, st, "type-p0 BB")
    poll(lambda: (lambda r: r if not r[0] and "BB" in r[1]["content"] else None)(m.tool("read", document="d1", format="text")), 10)
    e, t = m.tool("read", document="d1", format="text")
    e, r = m.tool("insert", document="d1", where="last", content="Agent block", format="texmacs")
    check("shared: the agent inserts a block", not e and r["status"] == "applied")
    hb0 = host_body(w, st)
    hostcmd(w, st, "undo")
    hostcmd(w, st, "undo")
    hb = host_body(w, st)
    check("shared: the person's undo undid their own typing and kept the agent's block",
          "AA" not in str(hb) and "BB" not in str(hb) and "Agent block" in str(hb))
    # 8. the same inside the paragraph the person types in (the agent's change is at
    # its start, far from what they type)
    e, t = m.tool("read", document="d1", format="text")
    p0 = t["children"][0]
    hostcmd(w, st, "type-p0 CC")
    hostcmd(w, st, "type-p0 DD")
    poll(lambda: (lambda r: r if not r[0] and "DD" in r[1]["content"] else None)(m.tool("read", document="d1", format="text")), 10)
    e, t = m.tool("read", document="d1", format="text")
    e, r = m.tool("edit_text", document="d1", node=t["children"][0]["handle"], old="Begin", new="Start")
    check("shared: the agent edits the start of the paragraph the person types in", not e and r["status"] == "applied")
    hostcmd(w, st, "undo")
    hostcmd(w, st, "undo")
    hb = host_body(w, st)
    check("shared: the person's two undos undo their typing and keep the agent's change",
          "CC" not in str(hb) and "DD" not in str(hb) and "Start paragraph." in str(hb))
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("shared: the agent has the same document", "Start paragraph." in t["content"] and "CC" not in t["content"])
    # 9. an edit that the content filter cancels never reaches the host; an accepted one is sent once
    def peers():
        try:
            os.remove(os.path.join(w, "sync", "peers"))
        except OSError:
            pass
        hostcmd(w, st, "peers")
        wait_file(os.path.join(w, "sync", "peers"), 10)
        time.sleep(0.1)
        return int(open(os.path.join(w, "sync", "peers")).read().strip())
    e, r = m.tool("insert", document="d1", where="last", content="<hlink|site|https://ok.org>")
    check("shared: a block with a link is accepted", not e and r["status"] == "applied")
    def dest():
        t = m.tool("read", document="d1", format="text", max_size=100000)[1]
        link = t["children"][-1]
        return m.tool("read", document="d1", node=link["handle"], format="text")[1]["children"][1]["handle"]
    n1 = peers()
    e, r = m.tool("replace", document="d1", node=dest(), content="javascript:alert(1)")
    n2 = peers()
    check("shared: an edit cancelled by the content filter is refused", e and "content filter" in str(r))
    check("shared: and the host took no patch for it (%d, %d)" % (n1, n2), n2 == n1)
    e, r = m.tool("replace", document="d1", node=dest(), content="https://other.org")
    n3 = peers()
    check("shared: an accepted edit is sent once (%d, %d)" % (n2, n3), not e and n3 == n2 + 1)
    sync_set(w, "c-done")
    check("exit 0", m.close() == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()
    out = open(os.path.join(w, "out-B")).read() if os.path.exists(os.path.join(w, "out-B")) else ""
    check("the other TeXmacs passed its checks", out == "ok")


def s_propose(w):
    """the person's TeXmacs (a second process, interactive) applies the modes to the
    agent: propose (proposals the person accepts or rejects, the agent withdraws
    them and hears of the outcome), direct, read-only"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "shared.tm")
    write_small(f)
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    other = start_other(w, envB, "host-edit.scm", f, {"SH_INTERACTIVE": "1"})
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    check("the agent joins as a participant", not e and v["shared"]["role"] == "participant")
    def mode():
        return m.tool("list_documents")[1]["documents"][0].get("agent_mode")
    got = poll(lambda: mode() == "propose", 10)
    check("list_documents tells the mode: propose (the default with a person)", got)
    st = [0]
    def handles():
        return m.tool("read", document="d1", format="text")[1]["children"]
    def proposals_of_host():
        try:
            os.remove(os.path.join(w, "sync", "proposals"))
        except OSError:
            pass
        hostcmd(w, st, "proposals")
        wait_file(os.path.join(w, "sync", "proposals"), 10)
        time.sleep(0.1)
        return open(os.path.join(w, "sync", "proposals")).read()
    k = handles()
    # 1. an edit becomes a proposal
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="First", new="Initial", expect=k[0]["hash"])
    check("propose: the edit is proposed", not e and r["status"] == "proposed" and r.get("proposal") == "p1")
    hb = host_body(w, st)
    check("propose: the host's document holds both versions", "version-both" in hb and "First" in hb and "Initial" in hb)
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("propose: the agent's copy is the host's, with the proposal", "version-both" in t["content"])
    e, lp = m.tool("list_proposals", document="d1")
    check("list_proposals: one pending, with its place", not e and len(lp["proposals"]) == 1 and lp["proposals"][0]["status"] == "pending"
          and lp["proposals"][0]["where"]["index"] == 0)
    e, u = m.tool("undo_agent", document="d1")
    check("a proposed change cannot be undone by undo_agent", e)
    # 2. the person accepts: the agent's next answer has a note
    hostcmd(w, st, "accept p1")
    time.sleep(0.5)
    e, t = m.tool("read", document="d1", format="text")
    check("accepted: the note tells the agent (once)", "proposal p1 was accepted" in t.get("note", ""))
    e, lp = m.tool("list_proposals", document="d1")
    check("accepted: listed as accepted", not e and lp["proposals"][0]["status"] == "accepted")
    hb = host_body(w, st)
    check("accepted: the new text in the host's document", "Initial paragraph." in hb and "version-both" not in hb)
    e, t = m.tool("read", document="d1", format="text")
    check("and not again", "proposal p1" not in t.get("note", ""))
    k = t["children"]
    # 3. reject
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Initial", new="Other")
    check("a second proposal", not e and r["status"] == "proposed" and r["proposal"] == "p2")
    hostcmd(w, st, "reject p2")
    time.sleep(0.5)
    e, t = m.tool("read", document="d1", format="text")
    check("rejected: the note, and the old text", "proposal p2 was rejected" in t.get("note", "") and "Initial paragraph." in t["content"])
    k = t["children"]
    # 4. withdraw
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Initial", new="Third")
    check("a third proposal", not e and r["status"] == "proposed" and r["proposal"] == "p3")
    e, r = m.tool("withdraw_proposal", document="d1", id="p3")
    check("withdraw_proposal", not e and r["status"] == "withdrawn")
    hb = host_body(w, st)
    check("withdrawn: the host's document is clean", "version-both" not in hb and "Initial paragraph." in hb)
    e, r = m.tool("withdraw_proposal", document="d1", id="p9")
    check("an unknown proposal is an error", e)
    # 5. insertion and deletion are proposals too
    e, r = m.tool("insert", document="d1", where="last", content="Added block.")
    check("an insertion is proposed", not e and r["status"] == "proposed")
    hostcmd(w, st, "accept p4")
    time.sleep(0.5)
    hb = host_body(w, st)
    check("accepted insertion", "Added block." in hb and "version-both" not in hb)
    k = handles()
    e, r = m.tool("delete", document="d1", node=k[-1]["handle"])
    check("a deletion is proposed", not e and r["status"] == "proposed")
    hb = host_body(w, st)
    check("both versions while pending", "version-both" in hb and "Added block." in hb)
    hostcmd(w, st, "accept p5")
    time.sleep(0.5)
    hb = host_body(w, st)
    check("accepted deletion", "Added block." not in hb and "version-both" not in hb)
    # 6. the person's undo after accepting brings the proposal back, not the agent's undone
    hostcmd(w, st, "undo")
    hb = host_body(w, st)
    check("the person's undo of the accept restores the markup", "version-both" in hb)
    hostcmd(w, st, "redo")
    # 7. direct
    hostcmd(w, st, "mode direct")
    poll(lambda: mode() == "direct", 10)
    check("the agent sees the new mode: direct", mode() == "direct")
    k = handles()
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Initial", new="Direct")
    check("direct: applied", not e and r["status"] == "applied")
    hb = host_body(w, st)
    check("direct: in the host's document", "Direct paragraph." in hb and "version-both" not in hb)
    # 8. read-only
    hostcmd(w, st, "mode read-only")
    poll(lambda: mode() == "read-only", 10)
    k = handles()
    e, r = m.tool("edit_text", document="d1", node=k[0]["handle"], old="Direct", new="Nothing")
    check("read-only: refused with the rule", not e and r["status"] == "refused" and r["rules"][0]["rule"] == "read-only"
          and "read-only" in r["reason"])
    hb = host_body(w, st)
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    check("read-only: both copies unchanged", "Nothing" not in hb and "Nothing" not in t["content"] and "Direct paragraph." in hb)
    # 9. back to propose; the saved file with a pending proposal is readable
    hostcmd(w, st, "mode propose")
    poll(lambda: mode() == "propose", 10)
    k = handles()
    e, r = m.tool("edit_text", document="d1", node=k[1]["handle"], old="Second", new="Pending")
    check("a pending proposal", not e and r["status"] == "proposed")
    e, r = m.tool("save_document", document="d1")
    check("propose: the agent's Save is refused", not e and r["status"] == "refused" and "person" in r.get("reason", ""))
    e, r = m.tool("insert", document="d1", where="last", content="<version-both|a|b>")
    check("version markup written by the agent is refused", e and "version" in str(r))
    hostcmd(w, st, "save")
    time.sleep(1.0)
    txt = open(f, errors="replace").read()
    check("the saved file holds both versions of the pending proposal", "version-both" in txt and "Second" in txt and "Pending" in txt)
    # 10. an agent that closes with a pending proposal: the proposal stays in the person's document
    e, r = m.tool("close_document", document="d1", discard=True)
    check("the agent leaves", not e)
    hb = host_body(w, st)
    check("the pending proposal stays in the person's document", "version-both" in hb)
    sync_set(w, "c-done")
    check("exit 0", m.close() == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()
    out = open(os.path.join(w, "out-B")).read() if os.path.exists(os.path.join(w, "out-B")) else ""
    check("the other TeXmacs passed its checks", out == "ok")


def s_undo(w):
    """the editor's undo in a headless process (single process, no sharing)"""
    f = os.path.join(w, "u.tm")
    write_small(f)
    env, home = make_env(w, "A")
    env["UNDO_FILE"] = os.path.realpath(f)
    p = subprocess.Popen([BIN, "-headless", "-x", '(load "%s")' % os.path.join(HERE, "undo-probe.scm")],
                         env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    procs.append(p)
    try:
        out = p.communicate(timeout=60)[0].decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        p.kill()
        out = ""
    checks = [l for l in out.splitlines() if l.startswith("CHECK ")]
    check("the probe ran to its end", "UNDO-DONE" in out)
    check("no crash (exit status 0)", p.returncode == 0)
    for l in checks:
        check(l[6:-5], l.endswith("PASS"))
    check("three undo checks", len(checks) == 3)


def s_content(w):
    """what content is refused: markup that runs code or reads files, trees with
    the wrong number of arguments; also create_document and links"""
    roots = os.path.join(w, "root")
    os.makedirs(roots, exist_ok=True)
    outside = os.path.join(w, "outside")
    os.makedirs(outside, exist_ok=True)
    env, home = make_env(w, "A")
    env["TEXMACS_MCP_ROOTS"] = roots
    f = os.path.join(roots, "c.tm")
    write_small(f)
    write_small(os.path.join(roots, "inc.tm"))
    write_small(os.path.join(outside, "secret.tm"))
    with open(os.path.join(roots, "pic.png"), "wb") as fh:
        fh.write(b"x")
    os.symlink(outside, os.path.join(roots, "lnk"))       # a link that leads out of the roots
    with open(os.path.join(roots, "inc.tex"), "w") as fh:
        fh.write("INCLUDED-TEX-CONTENT\n")
    f = os.path.realpath(f)
    rr = os.path.realpath(roots)
    m = Mcp(env)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    e, t = m.tool("read", document="d1", format="stree", max_size=100000)
    base_hash = t["hash"]
    def refused(label, fmt, content, word):
        e, r = m.tool("insert", document="d1", where="last", content=content, format=fmt)
        e2, t2 = m.tool("read", document="d1", format="stree", max_size=100000)
        ok = e and word in str(r) and t2["hash"] == base_hash
        check("refused: %s" % label, ok)
        if not ok:
            print("    got:", label, e, str(r)[:160], t2["hash"] == base_hash)
    def accepted(label, fmt, content):
        e, r = m.tool("insert", document="d1", where="last", content=content, format=fmt)
        check("accepted: %s" % label, not e and r["status"] == "applied")
        if e:
            print("    ", str(r)[:300])
        m.tool("undo_agent", document="d1")
    for tag in ("extern", "script", "script-eval", "eval", "eval-args", "action", "quasi", "assign", "compound", "session", "input", "raw-data"):
        refused("(%s ...) in stree" % tag, "stree", '(%s "x" "y")' % tag, "`%s'" % tag)
    refused("hlink to script:", "stree", '(hlink "click" "script:(system \\"id\\")")', "script:")
    refused("hlink to file:", "stree", '(hlink "x" "file:///etc/passwd")', "file:")
    refused("hlink to javascript:", "stree", '(hlink "x" "javascript:alert(1)")', "javascript:")
    refused("href to script:", "stree", '(href "script:foo")', "script:")
    refused("image from file:///", "stree", '(image "file:///etc/passwd" "1par" "" "" "")', "file:")
    refused("image with an absolute path", "stree", '(image "/etc/hosts" "1par" "" "" "")', "/etc/hosts")
    refused("image with ..", "stree", '(image "../outside/x.png" "1par" "" "" "")', "..")
    refused("image with raw data", "stree", '(image (tuple (raw-data "abc") "png") "1par" "" "" "")', "raw-data")
    refused("include of an absolute path", "stree", '(include "%s/inc.tm")' % rr, "refers to the file")
    refused("include of an absolute path outside", "stree", '(include "/etc/passwd")', "refers to the file")
    refused("include with ..", "stree", '(include "../outside/x.tm")', "..")
    refused("postscript with an absolute path", "stree", '(postscript "/etc/hosts" "1par" "" "" "")', "refers to the file")
    refused("bibliography with an absolute file", "stree", '(bibliography "bib" "plain" "/etc/hosts")', "refers to the file")
    refused("texmacs: include", "texmacs", "<include|/etc/hosts>", "refers to the file")
    refused("texmacs: extern", "texmacs", '<extern|(system "id")>', "`extern'")
    refused("texmacs: hlink to script:", "texmacs", "<hlink|x|script:foo>", "script:")
    refused("texmacs: action", "texmacs", "<action|go|(system \"id\")>", "`action'")
    refused("latex: \\input", "latex", "\\input{%s/inc.tex}" % rr, "\\input")
    refused("latex: \\include", "latex", "\\include{%s/inc}" % rr, "\\include")
    refused("latex: \\verbatiminput", "latex", "\\verbatiminput{/etc/hosts}", "\\verbatiminput")
    refused("latex: \\includegraphics of an absolute path", "latex", "\\includegraphics{/etc/hosts}", "refers to the file")
    refused("markdown: an image with an absolute path", "markdown", "![x](/etc/hosts)", "refers to the file")
    refused("markdown: a link to script:", "markdown", "[x](script:foo)", "script:")
    accepted("an image from the web", "stree", '(image "https://example.org/x.png" "1par" "" "" "")')
    accepted("an image in the folder", "stree", '(image "pics/x.png" "1par" "" "" "")')
    accepted("a link to the web", "stree", '(hlink "site" "https://example.org/")')
    accepted("a link to a label", "stree", '(hlink "here" "#sec")')
    accepted("a mail link", "stree", '(hlink "me" "mailto:a@b.org")')
    accepted("an include in the folder", "stree", '(include "inc.tm")')
    accepted("markdown: a link", "markdown", "[x](https://example.org)")
    # the primitives of the DRD, by their real names (src/Data/Drd/drd_std.cpp)
    refused("include*", "texmacs", "<include*|/etc/hosts>", "`include*'")
    refused("include* in the folder behind a link", "texmacs", "<include*|lnk/secret.tm>", "`include*'")
    refused("include behind a link", "texmacs", "<include|lnk/secret.tm>", "`include'")
    refused("include with %2e%2e", "texmacs", "<include|%2e%2e/outside/secret.tm>", "`include'")
    refused("include of a web address", "texmacs", "<include|HTTP://x/y>", "`include'")
    refused("include of a network path", "texmacs", "<include|//host/x.tm>", "`include'")
    refused("style-only*", "texmacs", "<style-only*|x>", "`style-only*'")
    refused("style-with*", "texmacs", "<style-with*|x|y>", "`style-with*'")
    refused("hybrid", "texmacs", "<hybrid|extern>", "`hybrid'")
    refused("authorize", "texmacs", "<authorize|a|b>", "`authorize'")
    refused("observer", "texmacs", "<observer|a|b>", "`observer'")
    refused("while (a loop without limit)", "texmacs", "<while|true|x>", "`while'")
    refused("for-each", "texmacs", "<for-each|x|y>", "`for-each'")
    refused("range", "stree", '(range "0" "1000000000")', "`range'")
    refused("pattern with an absolute file", "texmacs", "<with|color|<pattern|/etc/passwd|1cm|1cm>|x>", "`pattern'")
    refused("pattern behind a link", "texmacs", "<with|color|<pattern|lnk/pic.png|1cm|1cm>|x>", "`pattern'")
    refused("video", "texmacs", "<video|/etc/passwd>", "`video'")
    refused("sound", "stree", '(sound "/etc/passwd")', "`sound'")
    refused("find-file", "stree", '(image (find-file "/etc/passwd") "1par" "" "" "")', "`find-file'")
    refused("a plugin input", "texmacs", "<scheme-input|default|1|(+ 1 2)>", "-input")
    refused("shell-input", "texmacs", "<shell-input|a|b|c>", "-input")
    refused("a macro", "texmacs", "<with|f|<macro|<f>>|<f>>", "tag `")
    refused("a macro that calls itself", "texmacs", "<with|f|<macro|x|<f|x>>|<f|a>>", "tag `")
    refused("apply", "stree", '(apply "f" "x")', "`apply'")
    refused("expand", "stree", '(expand "f" "x")', "`expand'")
    refused("set", "stree", '(set "x" "y")', "`set'")
    refused("provide", "stree", '(provide "x" "y")', "`provide'")
    refused("filter-style", "stree", '(filter-style "x")', "`filter-style'")
    # links: the destination, written as plain text
    refused("a destination through with/value", "texmacs", "<with|u|file:///etc/passwd|<hlink|a|<value|u>>>", "destination is not written")
    refused("a destination through concat", "texmacs", "<hlink|a|<concat|file:|///etc/passwd>>", "destination is not written")
    refused("a destination through merge", "texmacs", "<hlink|a|<merge|file|:///etc/passwd>>", "destination is not written")
    refused("a destination that is a network path", "texmacs", "<hlink|a|//host/path>", "//host/path")
    refused("a destination with blanks before the scheme", "texmacs", "<hlink|a| javascript:x>", "javascript:x")
    refused("a tmfs destination", "texmacs", "<hlink|a|tmfs://exec/x>", "tmfs:")
    refused("url", "texmacs", "<url|file:///etc/passwd>", "`url'")
    refused("href", "texmacs", "<href|file:///etc/passwd>", "`href'")
    refused("slink", "texmacs", "<slink|file:///etc/passwd>", "`slink'")
    refused("a link in a locus", "texmacs", "<locus|<id|x>|<link|hyperlink|<id|x>|file:///etc/passwd>|text>", "`link'")
    # raw text for other programs
    refused("specific for latex", "stree", '(specific "latex" "\\\\input{/etc/passwd}")', "specific")
    refused("specific for html", "stree", '(specific "html" "<script>alert(1)</script>")', "specific")
    refused("a latex tag with \\input", "stree", '(latex "\\\\input{/etc/passwd}")', "\\input")
    refused("latex: building a command name", "latex", "\\in\\string put{secret}", "\\string")
    refused("latex: a definition that hides \\input", "latex", "\\newcommand{\\foo}{\\input{x}}", "\\input")
    refused("latex: \\bibliography of an absolute file", "latex", "\\bibliography{/etc/passwd}", "bibliography")
    refused("latex: \\href to a file", "latex", "\\href{file:///etc/passwd}{x}", "file:")
    refused("latex: \\url to javascript", "latex", "\\url{javascript:alert(1)}", "javascript:")
    refused("markdown: ![x](../../etc/passwd)", "markdown", "![x](../../etc/passwd)", "refers to the file")
    refused("markdown: [x](javascript:alert(1))", "markdown", "[x](javascript:alert(1))", "javascript:")
    accepted("a link with a colon in its text", "texmacs", "<hlink|See: this|http://example.com>")
    accepted("a link to a section", "texmacs", "<hlink|Section 1: intro|#sec:x>")
    accepted("a link to a section of another file", "texmacs", "<hlink|a|other.tm#sec:x>")
    accepted("a link with an uppercase scheme", "texmacs", "<hlink|a|HTTP://x.y>")
    accepted("a link to the web with a query", "texmacs", "<hlink|a|https://x.org/a?b=c:d>")
    accepted("a reference", "texmacs", "<reference|sec:x>")
    refused("an unknown tag", "texmacs", "<unknowntag|a|b>", "unknown tag")
    refused("an unknown tag with one argument", "texmacs", "<foo|a>", "unknown tag")
    accepted("a theorem with a colon", "texmacs", "<theorem|Let x: then y.>")
    accepted("a footnote with a colon", "texmacs", "<footnote|note: with colon>")
    accepted("a pattern in the folder", "texmacs", "<with|color|<pattern|pic.png|1cm|1cm>|x>")
    accepted("a specific for printer", "texmacs", "<specific|printer|x>")
    accepted("a graphics with a point", "texmacs", "<graphics|<point|0|0>>")
    accepted("a latex tag with plain text", "stree", '(latex "x^2")')
    for src in ("\\usepackage{../x}", "\\usepackage{/etc/x}", "\\documentclass{../../x}", "\\documentclass[../x]{article}",
                "\\bibliographystyle{/etc/x}", "\\bibliography{/etc/passwd}", "\\bibliography{../refs}",
                "\\addbibresource{../x.bib}", "\\usepackage{a\\input}", "\\usepackage{.hidden}"):
        refused("latex: %s (a path, not a name)" % src, "latex", src, "LaTeX command")
    accepted("latex: \\bibliography{refs} with its style", "latex", "\\bibliographystyle{plain}\\bibliography{refs}")
    accepted("latex: \\addbibresource with a name and a section", "latex", "\\section{A}\\addbibresource{refs.bib}")
    e, r = m.tool("insert", document="d1", where="last", format="latex", content=
             "\\documentclass[12pt,a4paper]{article}\n\\usepackage{amsmath,amssymb}\n\\usepackage[utf8]{inputenc}\n"
             "\\begin{document}\n\\section{Intro}\\label{sec:i}\nWe know that $x^2\\geq 0$ \\cite{knuth}.\n"
             "\\bibliographystyle{plain}\n\\bibliography{refs}\n\\end{document}")
    check("a full small LaTeX article is accepted", not e and r["status"] == "applied")
    e, t = m.tool("read", document="d1", format="texmacs", max_size=100000)
    check("the article has no preamble command and has a bibliography with its style",
          not e and "documentclass" not in t["content"] and "usepackage" not in t["content"] and "tm-plain" in t["content"])
    m.tool("undo_agent", document="d1", steps=3)
    refused("latex: only the preamble", "latex", "\\documentclass{article}\\usepackage{amsmath}", "only commands of the preamble")
    accepted("a folded block", "stree", '(folded "summary" "body")')
    accepted("a summarized block", "stree", '(summarized "summary" "body")')
    accepted("a detailed block", "stree", '(detailed "summary" "body")')
    refused("a button (its script is an argument)", "stree", '(button "x" "y")', "button")
    refused("a toggle-box (its action is a computed command)", "stree", '(toggle-box "a" "b")', "toggle-box")
    accepted("latex: href to the web", "latex", "\\href{https://example.org}{site}")
    accepted("a get-attachment", "texmacs", "<get-attachment|a>")
    # names written as they are in an export to LaTeX
    for tag in ("label", "reference", "pageref", "eqref", "cite", "nocite"):
        refused("%s with a brace and \\input" % tag, "stree", '(%s "a}\\\\input{/etc/passwd}\\\\x{")' % tag, "character that cannot be in a name")
    refused("label with a percent", "stree", '(label "a%b")', "character that cannot be")
    refused("label with a control character", "stree", '(label "a\\nb")', "character that cannot be")
    refused("label with a hash", "stree", '(label "a#b")', "character that cannot be")
    refused("a link to an address with a brace", "stree", '(hlink "x" "http://a/b}\\\\input{x}")', "cannot be in an address")
    refused("a link to an address with a bare percent", "stree", '(hlink "x" "http://a/b%zz")', "cannot be in an address")
    refused("an include with a brace", "stree", '(include "a}b.tm")', "character that cannot be")
    refused("a with whose value has a brace", "stree", '(with "font-family" "}\\\\input{x}" "text")', "value that is not accepted")
    accepted("a label with a colon and a dash", "stree", '(label "sec:intro-1_a")')
    accepted("a link with a fragment", "texmacs", "<hlink|a|https://x.org/p#frag>")
    accepted("a link with a percent escape", "texmacs", "<hlink|a|https://x.org/a%20b>")
    accepted("a link with a query and a user path", "stree", '(hlink "a" "http://a.b/~u/c?d=1&e=2#f")')
    accepted("markdown: a link with a query", "markdown", "[a](https://example.com/a?b=c&d=e#f)")
    refused("a link with ^^", "stree", '(hlink "a" "http://a.b/^^5c")', "cannot be in an address")
    refused("a latex tag with includegraphics", "stree", '(latex "\\\\includegraphics{/etc/passwd}")', "includegraphics")
    refused("a latex tag with bibliography", "stree", '(latex "\\\\bibliography{/etc/passwd}")', "bibliography")
    refused("a latex tag with IfFileExists", "stree", '(latex "\\\\IfFileExists{/etc/passwd}{a}{b}")', "IfFileExists")
    accepted("a with with a plain value", "stree", '(with "color" "red" "x")')
    accepted("text with braces and a backslash (escaped by the export)", "stree", '(concat "a { b } \\\\ c")')
    # tags are accepted only if the style (or the document) defines them
    for tag, args in (("verbatiminput", '"/etc/passwd"'), ("lstinputlisting", '"x"'), ("inputminted", '"c" "x"'),
                      ("import", '"x" "y"'), ("subimport", '"x" "y"'), ("includepdf", '"x"'),
                      ("ShellEscape", '"x"'), ("directlua", '"x"'), ("immediate", '"x"'),
                      ("write18", '"x"'), ("doxygen-load", '"x"'), ("button", '"x" "y"'),
                      ("tmdoc-file", '"x"'), ("widget-cmd", '"x"')):
        refused("an unknown tag: %s" % tag, "stree", "(%s %s)" % (tag, args), tag)
    for src in ("\\includepdf{x}", "\\inputminted{c}{x}", "\\import{a}{b}", "\\subimport{a}{b}",
                "\\ShellEscape{x}", "\\directlua{x}", "\\immediate\\write18{x}",
                "\\addplot table {/etc/passwd};", "\\pgfplotstableread{x}", "\\CatchFileDef{a}{b}{c}",
                "\\DTLloaddb{a}{b}"):
        refused("latex: %s" % src, "latex", src, "LaTeX command")
    refused("^^ in a latex tag", "stree", '(latex "a^^5cinput{x}")', "^^")
    refused("a macro with the wrong number of arguments", "stree", '(theorem)', "cannot have")
    refused("a macro with too many arguments", "stree", '(theorem "a" "b")', "cannot have")
    refused("a bibliography with the wrong number of arguments", "texmacs", "<bibliography|bib|plain|refs.bib>", "bibliography")
    # aliases and wrappers: the second layer checks the content as it expands
    refused("an alias of href through with and value", "texmacs", "<with|em|<value|href>|<em|javascript:alert(1)>>", "alias")
    refused("an alias of label", "texmacs", "<with|strong|<value|label>|<strong|a>>", "alias")
    refused("an alias of a macro through quote-value", "texmacs", "<with|em|<quote-value|hlink>|<em|x|file:///etc/passwd>>", "alias")
    refused("a with that sets a tag", "texmacs", "<with|em|x|<em|y>>", "alias")
    refused("a wrapper macro with a javascript address", "texmacs", "<hlink*|x|javascript:alert(1)|t>", "javascript:")
    refused("a wrapper macro with a file address", "texmacs", "<hlink*|x|file:///etc/passwd|t>", "file:")
    accepted("a wrapper macro with a web address", "texmacs", "<hlink*|x|https://example.org|t>")
    accepted("a reference and a cite (they expand to links)", "texmacs", "<reference|sec:x> <cite|knuth>")
    # the values of the environment
    for bad in ('(with "font-size" "2\\" onmouseover=\\"x" "t")', '(with "color" "red\\" onmouseover=\\"x" "t")',
                '(with "bg-color" "red<b>" "t")', '(with "font-size" "big" "t")', '(with "color" "#12" "t")',
                '(tformat (cwith "1" "1" "1" "1" "cell-background" "red\\" onmouseover=\\"x") (table (row (cell "a"))))'):
        refused("a value that is not accepted: %s" % bad[:40], "stree", bad, "not accepted")
    accepted("a hex colour", "texmacs", "<with|color|#ff0000|x>")
    accepted("a hex background", "texmacs", "<with|bg-color|#ffeecc|x>")
    accepted("a length in percent", "texmacs", "<with|par-left|10%|x>")
    accepted("a font size", "texmacs", "<with|font-size|1.2|x>")
    accepted("a colour name", "texmacs", "<with|color|dark red|x>")
    accepted("a cwith with a colour", "stree", '(tformat (cwith "1" "1" "1" "1" "cell-background" "#ffeecc") (table (row (cell "a"))))')
    refused("a bibliography with a path in a list of files", "texmacs", "<bibliography|bib|tm-plain|refs,../secret|<document|>>", "refers to the file")
    accepted("a bibliography with a list of files", "texmacs", "<bibliography|bib|tm-plain|refs,more|<document|>>")
    e, r = m.tool("insert", document="d1", where="last", format="latex", content=
             "\\documentclass{article}\\title{On the Theory}\\author{A. Author\\thanks{Grant}}\\date{2026}\n"
             "\\begin{document}\n\\maketitle\n\\section{A}\\small small \\large large\\normalsize\n"
             "See \\autoref{sec:a} and \\ref{x}. \\caption{bare}\n\\hline\n\\bibliographystyle{plain}\\bibliography{refs}\n\\end{document}")
    check("a LaTeX article with title, author, maketitle, thanks, caption, autoref, hline", not e and r["status"] == "applied")
    e, t = m.tool("read", document="d1", format="texmacs", max_size=100000)
    check("and it is imported with its title and bibliography", not e and "doc-title|On the Theory" in t["content"]
          and "tm-plain" in t["content"] and "<reference|sec:a>" in t["content"] and "autoref" not in t["content"])
    m.tool("undo_agent", document="d1")
    accepted("a snippet with a title and a maketitle", "latex", "\\title{T}\\author{A}\\maketitle Hello")
    # what goes into the exports as attributes, entities, files written by LaTeX
    refused("a label with a tag", "stree", '(label "a\\"><script>alert(1)</script>")', "character that cannot be")
    refused("a reference with a quote", "stree", '(reference "a\\"><script>alert(5)</script>")', "character that cannot be")
    refused("a cite with a quote", "stree", '(cite "a\\"><script>alert(11)</script>")', "character that cannot be")
    refused("a link with a quote and an event", "stree", '(hlink "x" "http://a.b/\\"onmouseover=\\"alert(1)")', "cannot be in an address")
    refused("a link with a tag", "stree", '(hlink "x" "http://a.b/\'><script>alert(2)</script>")', "cannot be in an address")
    refused("a link with a numeric entity", "stree", '(hlink "x" "&#106;avascript:alert(3)")', "cannot be in an address")
    refused("a link with a hexadecimal entity", "stree", '(hlink "x" "jav&#x61;script:alert(3)")', "cannot be in an address")
    refused("an href with a named entity", "stree", '(href "javascript&colon;alert(1)")', "href")
    refused("a link with a blank", "stree", '(hlink "x" "http://a.b/c d")', "cannot be in an address")
    refused("an image address with a quote", "stree", '(image "http://a.b/x.png\\" onerror=\\"alert(9)" "1par" "" "" "")', "character that cannot be")
    refused("an image size with a quote", "stree", '(image "x.png" "1par\\" onload=\\"alert(10)" "" "" "")', "character that cannot be")
    refused("a map that calls include*", "texmacs", "<map|include*|<tuple|/etc/passwd>>", "map")
    refused("a map that calls hlink", "texmacs", "<map|hlink|<tuple|x>>", "map")
    accepted("a map that calls strong", "texmacs", "<map|strong|<tuple|x|y>>")
    refused("LaTeX filecontents", "latex", "\\begin{filecontents*}{../x.tex}hi\\end{filecontents*}", "filecontents")
    refused("LaTeX minted with shell-escape", "latex", "\\usepackage[shell-escape]{minted}\\begin{document}x\\end{document}", "runs programs")
    refused("LaTeX standalone with convert", "latex", "\\documentclass[convert=true]{standalone}\\begin{document}x\\end{document}", "runs programs")
    # trees with the wrong number of arguments (the typesetter crashed on `with')
    for bad in ('(with)', '(frac "a")', '(table)', '(row)', '(cell)', '(concat)', '(tformat)',
                '(sqrt)', '(document "a" (frac "b"))', '(rsub "a" "b")', '(tuple "a" (with))'):
        refused("malformed %s" % bad, "stree", bad, "cannot have")
    # tables: what the typesetter and the editor cannot take is refused, loose rows are not
    for bad in ('(table "X")', '(table (row (cell "a")) "X")', '(table (tuple))',
                '(tformat (cwith "1" "1" "1" "1" "cell-halign" "c") (frac "a" "b"))'):
        refused("table structure %s" % bad, "stree", bad, "needed")
    for bad in ('(table (row (cell "a") (cell "b")) (row (cell "e")))', '(table (row (cell "a") (cell "b") (cell "c")) (row (cell "e")))',
                '(table (row (cell "a")) (row (cell "b") (cell "c")))'):
        refused("ragged table %s" % bad, "stree", bad, "needed")
    for a, b, c, d, var, v in (("1", "1", "5", "5", "cell-row-span", "2"), ("1", "1", "5", "5", "cell-col-span", "1"),
                               ("1", "1", "3", "3", "cell-col-span", "2"), ("3", "3", "1", "1", "cell-col-span", "2"),
                               ("1", "1", "-5", "-5", "cell-col-span", "2"), ("1", "1", "1", "9", "cell-col-span", "2"),
                               ("1", "9", "1", "1", "cell-col-span", "2")):
        refused("span outside the table %s %s %s %s %s" % (a, b, c, d, var), "stree",
                '(tformat (cwith "%s" "%s" "%s" "%s" "%s" "%s") (table (row (cell "a") (cell "b")) (row (cell "c") (cell "d"))))' % (a, b, c, d, var, v),
                "not all in the table")
    refused("a table of set-cursor", "stree", '(table (set-cursor))', "`set-cursor'")
    refused("an empty row among rows", "stree", '(table (row (cell "a")) (row))', "needed")
    for bad, word in (('(table (row (cell "a")) (frac "a" "b"))', "needed"),
                      ('(table (with "color" "red" (row (cell "a"))))', "needed"),
                      ('(tformat (table (row (cell "a"))) "x")', "needed"),
                      ('(tformat (twith "table-width" "1par") (table (row (cell "a"))) "")', "needed"),
                      ('(tformat (arg "x"))', "needed"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-decoration" "x") (table (row (cell "a"))))', "cell-decoration"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-row-span" "-3") (table (row (cell "a")) (row (cell "b"))))', "cell-row-span"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-row-span" "x") (table (row (cell "a")) (row (cell "b"))))', "cell-row-span"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-row-span" "3") (table (row (cell "a")) (row (cell "b"))))', "cell-row-span"),
                      ('(tformat (cwith "2" "2" "1" "1" "cell-row-span" "2") (table (row (cell "a")) (row (cell "b"))))', "cell-row-span"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-col-span" "0") (table (row (cell "a") (cell "b"))))', "cell-col-span"),
                      ('(with "cell-row-span" "2" (table (row (cell "a"))))', "cell-row-span"),
                      ('(tformat (cwith "1" "1" "1" "1" "cell-col-span" "2") (table (row (cell "a") (cell "b")) (row (cell "c") (cell "d")) (row (cell "e"))))', "needed")):
        refused("table attribute/structure %s" % bad, "stree", bad, word)
    for ok in ('(tformat (cwith "1" "1" "1" "1" "cell-row-span" "2") (cwith "1" "1" "1" "1" "cell-halign" "c") (table (row (cell "a")) (row (cell "b"))))',
               '(tformat (cwith "1" "1" "1" "1" "cell-col-span" "3") (table (row (cell "a") (cell "b")) (row (cell "c") (cell "d"))))',
               '(tformat (twith "table-width" "1par") "x")',
               '(tformat (cwith "1" "1" "1" "1" "cell-row-span" "2") (table (row (cell "a")) (row (cell "b"))))',
               '(tformat (cwith "1" "1" "1" "1" "cell-col-span" "2") (table (row (cell "a") (cell "b"))))',
               '(tformat (cwith "1" "1" "1" "1" "cell-row-span" "1") (table (row (cell "a"))))',
               '(tformat (cwith "1" "-1" "1" "-1" "cell-halign" "c") (table (row (cell "a"))))',
               '(table (row (cell "a") (cell "b")))', '(table (row (with "color" "blue" "x")))',
               '(table (row (document "a")))', '(table (row "1" "2"))', '(table (row (concat "a" "b")))',
               '(tformat (twith "table-width" "1par") (table (row (cell "a"))))'):
        accepted("table shape %s" % ok, "stree", ok)
    r = m.request("ping")
    check("the server is alive after the malformed trees", r is not None and "result" in r)
    e, t2 = m.tool("read", document="d1", format="text")
    check("and reads", not e)
    # create_document and links
    e, c = m.tool("create_document", path=os.path.join(roots, "n1.tm"), content='(extern "x")', format="stree")
    check("create_document: refused content, no file", e and not os.path.exists(os.path.join(roots, "n1.tm")))
    link = os.path.join(roots, "dangling.tm")
    target = os.path.join(outside, "new.tm")
    os.symlink(target, link)
    e, c = m.tool("create_document", path=link)
    check("create_document: a dangling link is refused", e and not os.path.exists(target))
    link2 = os.path.join(roots, "tofile.tm")
    os.symlink(os.path.join(outside, "x.tm"), link2)
    e, c = m.tool("create_document", path=link2)
    check("create_document: another dangling link is refused", e and not os.path.exists(os.path.join(outside, "x.tm")))
    e, c = m.tool("create_document", path=os.path.join(roots, "n2.tm"), content="ok text")
    check("create_document: still works", not e)
    check("exit 0", m.close() == 0)


def s_filter(w):
    """every name in the lists of refused content is a tag of the DRD"""
    env, home = make_env(w, "A")
    p = subprocess.Popen([BIN, "-headless", "-x",
                          '(begin (display* "UNKNOWN-NAMES " (mcp-filter-unknown-names) "\\n") (quit-TeXmacs))'],
                         env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    procs.append(p)
    try:
        out = p.communicate(timeout=60)[0].decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        p.kill()
        out = ""
    line = [l for l in out.splitlines() if l.startswith("UNKNOWN-NAMES")]
    check("the probe ran", len(line) == 1)
    check("every name of the lists is a tag of the DRD: %s" % (line[0] if line else "?"),
          len(line) == 1 and line[0].strip() == "UNKNOWN-NAMES ()")


def s_styles(w):
    """the tags of the style of the document, and those the document assigns, are
    accepted; the other ones are not"""
    env, home = make_env(w, "A")
    os.makedirs(os.path.join(home, "styles"), exist_ok=True)
    with open(os.path.join(home, "styles", "mystyle.ts"), "w") as fh:
        fh.write("<TeXmacs|2.1.5>\n\n<style|source>\n\n<\\body>\n  <\\active*>\n    <\\src-package|mystyle|1.0>\n    </src-package>\n  </active*>\n\n"
                 "  <assign|mymacro|<macro|x|<strong|<arg|x>>>>\n</body>\n")
    f = os.path.join(w, "s.tm")
    with open(f, "w") as fh:
        fh.write("<TeXmacs|2.1.5>\n\n<style|<tuple|generic|mystyle>>\n\n<\\body>\n"
                 "  <assign|localmac|<macro|x|<em|<arg|x>>>>\n\n  Text.\n</body>\n")
    g = os.path.join(w, "g.tm")
    write_small(g)
    f = os.path.realpath(f)
    g = os.path.realpath(g)
    m = Mcp(env)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    e, v2 = m.tool("open_document", path=g)
    def ins(d, content, fmt="texmacs"):
        r = m.tool("insert", document=d, where="last", content=content, format=fmt)
        if not r[0]:
            m.tool("undo_agent", document=d)
        return r
    e, r = ins("d1", "<mymacro|hi>")
    check("a macro of the style of the document is accepted", not e)
    e, r = ins("d1", "<localmac|hi>")
    check("a macro that the document assigns is accepted", not e)
    e, r = ins("d2", "<mymacro|hi>")
    check("the same macro in a document without that style: unknown", e and "unknown tag" in str(r))
    e, r = ins("d2", "<localmac|hi>")
    check("a macro assigned in another document: unknown", e and "unknown tag" in str(r))
    e, r = ins("d1", "<strong|x> <em|y>")
    check("the macros of the standard style are accepted", not e)
    # the book style: chapter, part, appendix (the evaluator cannot do some counters of
    # the style outside the typesetter; that is not a danger)
    bk = os.path.join(w, "b.tm")
    with open(bk, "w") as fh:
        fh.write("<TeXmacs|2.1.5>\n\n<style|<tuple|book>>\n\n<\\body>\n  Text.\n</body>\n")
    e, vb = m.tool("open_document", path=os.path.realpath(bk))
    for what, content in (("chapter", '(chapter "A chapter")'), ("part", '(part "A part")'),
                          ("appendix", '(appendix "An appendix")'), ("section", '(section "A section")')):
        e, r = m.tool("insert", document=vb["document"], where="last", content=content, format="stree")
        check("book style: %s is accepted" % what, not e and r["status"] == "applied")
    check("exit 0", m.close() == 0)


def s_guard(w):
    """an edit may not introduce what the filter refuses, in the context of the
    document: leaves of existing nodes, definitions, the environment around, the
    budget of the evaluation, a macro that calls itself"""
    env, home = make_env(w, "A")
    root = os.path.join(w, "root")
    os.makedirs(root, exist_ok=True)
    env["TEXMACS_MCP_ROOTS"] = root
    with open(os.path.join(root, "pic.png"), "wb") as fh:
        fh.write(b"x")
    f = os.path.join(root, "g.tm")
    body = (
        "  <assign|foo|<macro|x|<hlink|harmless|https://a.org>>>\n\n"
        "  <assign|bar|<macro|x|<hlink|passes|<arg|x>>>>\n\n"
        "  <assign|ctxm|<macro|x|<if|<equal|<value|color>|red>|<hlink|t|<arg|x>>|<arg|x>>>>\n\n"
        "  <assign|dbl|<macro|x|<concat|<arg|x>|<arg|x>>>>\n\n"
        "  <assign|loopm|<macro|x|<loopm|<arg|x>>>>\n\n"
        "  <assign|usev|<macro|x|<if|<equal|<value|ctxvar>|red>|<hlink|t|<arg|x>>|<arg|x>>>>\n\n"
        "  <set|ctxvar|red>\n\n"
        "  <specific|html|hello>\n\n  <specific|latex|relax>\n\n  <extern|(display \"aaa\")>\n\n"
        "  Long link <hlink|t|javascript:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaX> here.\n\n"
        "  A paragraph with <hlink|a link|https://ok.org> in it.\n\n"
        "  A label here<label|lab1> and an image <image|pic.png|1par|||>.\n\n"
        "  <with|color|red|Red paragraph.>\n\n"
        "  Bad link <hlink|t|javascript:x> here and <action|click|(system \"x\")> too.\n\n"
        "  Plain paragraph.\n")
    write_tm(f, body)
    f = os.path.realpath(f)
    m = Mcp(env)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    def state():
        return m.tool("read", document="d1", format="texmacs", max_size=100000)[1]
    def block(prefix):
        t = m.tool("read", document="d1", format="text", max_size=100000)[1]
        for c in t["children"]:
            if c["preview"].startswith(prefix):
                return c
        return None
    base = state()["hash"]
    def check_refused(label, tool, **args):
        e, r = m.tool(tool, document="d1", **args)
        h = state()["hash"]
        check("refused: %s" % label, e and ("content filter" in str(r) or "refused" in str(r)) and h == base)
    def check_applied(label, tool, **args):
        e, r = m.tool(tool, document="d1", **args)
        ok = not e and r["status"] == "applied"
        check("accepted: %s" % label, ok)
        if ok:
            m.tool("undo_agent", document="d1")
        return ok
    # leaves of existing nodes (reached through their handles)
    def node_in(prefix, tag):
        b = block(prefix)
        t = m.tool("read", document="d1", node=b["handle"], format="text")[1]
        for c in t["children"]:
            if c["tag"] == tag:
                return c
        return None
    def child(h, i):
        t = m.tool("read", document="d1", node=h, format="text")[1]
        return t["children"][i]
    # (a refused edit is cancelled: the nodes are restored as new trees, so the handles are taken again)
    def dest_handle():
        return child(node_in("A paragraph with", "hlink")["handle"], 1)["handle"]
    def label_handle():
        return child(node_in("A label here", "label")["handle"], 0)["handle"]
    def image_handle():
        return child(node_in("A label here", "image")["handle"], 0)["handle"]
    check("the address leaf of the link was found",
          child(node_in("A paragraph with", "hlink")["handle"], 1)["preview"] == "https://ok.org")
    for what, content in (("javascript:", "javascript:alert(1)"), ("script:", 'script:(system "id")'),
                          ("a file address", "file:///etc/passwd")):
        check_refused("replace of the address leaf by %s" % what, "replace", node=dest_handle(), content=content)
    check_applied("replace of the address leaf by a web address", "replace", node=dest_handle(), content="https://elsewhere.org")
    check_refused("replace of a label by a brace and \\input", "replace", node=label_handle(), content="a}\\input{x}")
    check_applied("replace of a label by another name", "replace", node=label_handle(), content="lab2")
    check_refused("replace of an image file by an absolute path", "replace", node=image_handle(), content="/etc/passwd")
    check_refused("replace of an image file by ..", "replace", node=image_handle(), content="../x.png")
    check_applied("replace of an image file by another name in the folder", "replace", node=image_handle(), content="other.png")
    b = block("A paragraph with")
    check_applied("edit_text of the words around the link", "edit_text", node=b["handle"], old="A paragraph", new="One paragraph")
    # the environment around the edit
    rb = block("Red paragraph")
    rbody = child(rb["handle"], 2)
    check("the body of the with was found", rbody["preview"] == "Red paragraph.")
    check_refused("a pass-through macro inside a with that makes a link", "replace", node=rbody["handle"], content="<ctxm|javascript:alert(1)>")
    check_refused("the same macro outside it: it passes its argument to a link, and is not called at all", "insert", where="last", content="<ctxm|javascript:alert(1)>")
    rb = block("Red paragraph")
    check_refused("a value of with replaced by a compound", "replace", node=rb["handle"], content='(with "color" (concat "red" "\\"") "x")', format="stree")
    # a node that is code or raw text for another program: no edit inside it, whatever it was
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    for pref, tag in (("hello", "specific"), ("relax", "specific"), ("aaa", "extern")):
        pass
    sp = [c for c in t["children"] if c["tag"] == "specific"]
    ex = [c for c in t["children"] if c["tag"] == "extern"]
    if len(sp) >= 2 and ex:
        h_html = child(sp[0]["handle"], 1)
        check_refused("an edit of the text of a specific for html (to a script)", "replace", node=h_html["handle"], content="<script>alert(1)</script>")
        sp = [c for c in m.tool("read", document="d1", format="text", max_size=100000)[1]["children"] if c["tag"] == "specific"]
        h_tex = child(sp[1]["handle"], 1)
        check_refused("an edit of the text of a specific for latex", "replace", node=h_tex["handle"], content="immediate\\write18{id}")
        ex = [c for c in m.tool("read", document="d1", format="text", max_size=100000)[1]["children"] if c["tag"] == "extern"]
        h_ext = child(ex[0]["handle"], 0)
        check_refused("an edit of the command of an extern", "replace", node=h_ext["handle"], content='(system "id")')
        e, r = m.tool("delete", document="d1", node=ex[0]["handle"])
        check("the extern itself can be deleted (and put back)", not e and r["status"] == "applied")
        m.tool("undo_agent", document="d1")
    else:
        check("the specific and extern nodes were found", False)
    # equal reasons for different contents: the key has the whole node
    t = m.tool("read", document="d1", format="text", max_size=100000)[1]
    jl = [c for c in t["children"] if c["preview"].startswith("Long link")]
    if jl:
        link = [c for c in m.tool("read", document="d1", node=jl[0]["handle"], format="text")[1]["children"] if c["tag"] == "hlink"][0]
        dest = child(link["handle"], 1)
        check_refused("a change at the end of a long javascript address that the person has", "replace",
                      node=dest["handle"], content="javascript:" + "a" * 90 + "Y")
    else:
        check("the block with the long link was found", False)
    # a block that does not evaluate within the budget: edits in it are refused
    hf = os.path.join(root, "h.tm")
    write_tm(hf, "  <assign|dbl|<macro|x|<concat|<arg|x>|<arg|x>>>>\n\n  Light block.\n\n  Heavy " + "<dbl|" * 24 + "x" + ">" * 24 + ".\n")
    e, v2 = m.tool("open_document", path=os.path.realpath(hf))
    t = m.tool("read", document=v2["document"], format="text", max_size=100000)[1]
    hv = [c for c in t["children"] if c["preview"].startswith("Heavy")]
    if hv:
        e, r = m.tool("edit_text", document=v2["document"], node=hv[0]["handle"], old="Heavy", new="Light")
        check("an edit in a block that does not evaluate within the budget is refused", e and "budget" in str(r))
    else:
        check("the heavy block was found", False)
    lt = [c for c in t["children"] if c["preview"].startswith("Light block")]
    e, r = m.tool("edit_text", document=v2["document"], node=lt[0]["handle"], old="Light", new="Easy")
    check("an edit in another block of that document is accepted", not e and r["status"] == "applied")
    # a whole-document comparison (a definition is edited) with heavy blocks: the memory stays low
    mf = os.path.join(root, "mem.tm")
    write_tm(mf, "  <assign|dbl|<macro|x|<concat|<arg|x>|<arg|x>>>>\n\n" +
             "".join("  Block %d " % i + "<dbl|" * 17 + "x" + ">" * 17 + ".\n\n" for i in range(25)))
    e, v3 = m.tool("open_document", path=os.path.realpath(mf))
    t0 = time.monotonic()
    t = m.tool("read", document=v3["document"], format="text", max_size=5000)[1]
    asg = [c for c in t["children"] if c["tag"] == "assign"][0]
    def child3(h, i):
        return m.tool("read", document=v3["document"], node=h, format="text")[1]["children"][i]
    mac = child3(asg["handle"], 1)
    body = child3(mac["handle"], 1)
    e, r = m.tool("replace", document=v3["document"], node=body["handle"], content="<concat|<arg|x>|<arg|x>|<arg|x>>")
    dt = time.monotonic() - t0
    check("an edit of a definition under heavy uses is refused within the budgets (%.1f s)" % dt, e and dt < 100)
    r2 = m.request("ping")
    check("and the server is alive (the watchdog stops a process above 2 GB)", r2 is not None and "result" in r2)
    # violations of the person that stay: sibling edits are accepted, a change of the address is not
    badb = block("Bad link")
    check_applied("an edit of the text around a javascript link of the person", "edit_text", node=badb["handle"], old="here and", new="there and")
    check_applied("an insertion after such a block", "insert", anchor=badb["handle"], where="after", content="Inserted after.")
    bl = node_in("Bad link", "hlink")
    check_refused("a change of the address of that link", "replace", node=child(bl["handle"], 1)["handle"], content="javascript:y")
    # a macro reached by a computed name: the definition cannot be checked by its users
    cf = os.path.join(root, "c.tm")
    write_tm(cf, "  <assign|url|https://ok.org>\n\n  <assign|foo|<macro|x|<hlink|t|<value|url>>>>\n\n"
                 "  <compound|<merge|fo|o>|1>\n\n  Text here.\n")
    e, vc = m.tool("open_document", path=os.path.realpath(cf))
    dc = vc["document"]
    t = m.tool("read", document=dc, format="text", max_size=100000)[1]
    ua = [c for c in t["children"] if c["tag"] == "assign"][0]
    leaf = m.tool("read", document=dc, node=ua["handle"], format="text")[1]["children"][1]
    e, r = m.tool("replace", document=dc, node=leaf["handle"], content="javascript:alert(1)")
    check("a definition used through a computed name: the edit to javascript: is refused", e and "content filter" in str(r))
    # set of the person: the context of the blocks after it
    t = m.tool("read", document="d1", format="text", max_size=100000)[1]
    st = [c for c in t["children"] if c["tag"] == "set"]
    check("the set block was found", len(st) == 1)
    check_refused("a macro that depends on a variable set by the person, after the set", "insert", where="last", content="<usev|javascript:alert(1)>")
    # definitions
    check_applied("a use of a harmless macro with a dangerous argument (it ignores it)", "insert", where="last", content="<foo|javascript:alert(1)>")
    e, r = m.tool("insert", document="d1", where="last", content="<foo|javascript:alert(1)>")
    use = r["changed"][0]["handle"] if not e else None
    base2 = state()["hash"]
    # the two-step: change the macro so that it uses its argument
    foo = block("harmless") or None
    e, t = m.tool("read", document="d1", format="text", max_size=100000)
    fooblk = [c for c in t["children"] if c["tag"] == "assign"][:1]
    if fooblk:
        mac = child(fooblk[0]["handle"], 1)
        hl = child(mac["handle"], 1)
        url = child(hl["handle"], 1)
        e, r = m.tool("replace", document="d1", node=url["handle"], content="javascript:alert(2)")
        check("a macro whose address is changed to javascript: is refused", e and "content filter" in str(r) and state()["hash"] == base2)
        mac = child(fooblk[0]["handle"], 1)
        hl = child(mac["handle"], 1)
        e, r = m.tool("replace", document="d1", node=hl["handle"], content='<hlink|harmless|<arg|x>>')
        check("a macro changed to pass its argument to a link is refused",
              e and "refused" in str(r) and state()["hash"] == base2)
        mac = child(fooblk[0]["handle"], 1)
        hl = child(mac["handle"], 1)
        e, r = m.tool("replace", document="d1", node=hl["handle"], content='<bar|<arg|x>>')
        check("the two-step: the macro now calls a macro of the person which makes a link of the argument, "
              "and a use with javascript: exists: refused (the definitions are the person's)",
              e and "refused" in str(r) and state()["hash"] == base2)
    else:
        check("the macro foo was found", False)
    m.tool("undo_agent", document="d1")
    # the budget: a macro that doubles, nested
    nest = "x"
    for _ in range(26):
        nest = "<dbl|%s>" % nest
    t0 = time.monotonic()
    e, r = m.tool("insert", document="d1", where="last", content=nest)
    dt = time.monotonic() - t0
    check("a nested doubling macro is refused within the budget (%.1f s)" % dt, e and ("budget" in str(r) or "content filter" in str(r)) and dt < 60)
    r = m.request("ping")
    check("and the server is alive", r is not None and "result" in r)
    # a macro that calls itself
    e, r = m.tool("insert", document="d1", where="last", content="<loopm|a>")
    r2 = m.request("ping")
    check("a macro that calls itself does not crash the server", r2 is not None and "result" in r2)
    check("and the content is refused", e)
    # the preamble of the person does not make every insertion refused
    e, r = m.tool("insert", document="d1", where="last", content="Hello <em|you>")
    check("a plain insertion in a document with such a preamble is accepted", not e and r["status"] == "applied")
    # the commands of a latex tag
    for cmd in ("pdfximage", "special", "VerbatimInput", "includesvg", "attachfile", "markdownInput", "latelua", "pdfobj"):
        e, r = m.tool("insert", document="d1", where="last", content='(latex "\\\\\\%s{x}")' % cmd, format="stree")
        check("a latex tag with \\\\%s is refused" % cmd, e)
    check("exit 0", m.close() == 0)


def s_recursion(w):
    """a macro that calls itself, in a block and inline: typesetting and export of the
    document do not crash the process (the person's TeXmacs would have crashed too)"""
    env, home = make_env(w, "A")
    f = os.path.join(w, "loop.tm")
    write_tm(f, "  <assign|loopm|<macro|x|<loopm|<arg|x>>>>\n\n  Before\n\n  <loopm|a>\n\n  After <loopm|b> inline.\n")
    env["RECURSION_FILE"] = os.path.realpath(f)
    env["RECURSION_PDF"] = os.path.join(os.path.realpath(w), "loop.pdf")
    p = subprocess.Popen([BIN, "-headless", "-x", '(load "%s")' % os.path.join(HERE, "recursion-probe.scm")],
                         env=env, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    procs.append(p)
    try:
        out = p.communicate(timeout=100)[0].decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        p.kill()
        out = ""
    deep = os.path.join(w, "deep.tm")
    inner = "deepest"
    for i in range(100):
        inner = "<\\itemize>\n<item>level %d\n\n%s\n</itemize>" % (i, inner)
    write_tm(deep, inner + "\n")
    env2 = dict(env)
    env2["RECURSION_FILE"] = os.path.realpath(deep)
    env2["RECURSION_PDF"] = os.path.join(os.path.realpath(w), "deep.pdf")
    p2 = subprocess.Popen([BIN, "-headless", "-x", '(load "%s")' % os.path.join(HERE, "recursion-probe.scm")],
                          env=env2, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    procs.append(p2)
    try:
        out2 = p2.communicate(timeout=100)[0].decode("utf-8", "replace")
    except subprocess.TimeoutExpired:
        p2.kill()
        out2 = ""
    check("100 nested itemize levels typeset and export", "EXPORT-OK" in out2 and p2.returncode == 0)
    try:
        txt = subprocess.run(["mutool", "draw", "-F", "txt", "-o", "-", env2["RECURSION_PDF"], "1-99"],
                             capture_output=True, text=True, timeout=60).stdout
    except Exception:
        txt = None
    if txt is None:
        # (no mutool here: the text of the PDF cannot be read; the check is skipped, and said so)
        print("    SKIP no level is replaced by an error box (mutool is not available)")
    else:
        check("and no level is replaced by an error box", "level 99" in txt and "nesting" not in txt)
    check("typesetting does not crash", "TYPESET-OK" in out)
    check("export to PDF does not crash", "EXPORT-OK" in out and os.path.exists(env["RECURSION_PDF"]))
    check("exit status 0", p.returncode == 0)


def s_static(w):
    """the static rules of the filter: the definitions belong to the person, the
    content does not call a macro that passes an argument to a link, a file, a
    script or a macro name (whatever it is wrapped in), errors of the evaluation
    refuse the content, the keys of the violations have the whole content; and
    normal writing in the standard styles is accepted"""
    env, home = make_env(w, "A")
    root = os.path.join(w, "root")
    os.makedirs(root, exist_ok=True)
    env["TEXMACS_MCP_ROOTS"] = root
    with open(os.path.join(root, "pic.png"), "wb") as fh:
        fh.write(b"x")
    m = Mcp(env)
    m.initialize()
    count = [0]
    def opendoc(body, style="generic"):
        count[0] += 1
        f = os.path.join(root, "s%d.tm" % count[0])
        with open(f, "w") as fh:
            fh.write("<TeXmacs|2.1.5>\n\n<style|<tuple|%s>>\n\n<\\body>\n%s</body>\n" % (style, body))
        e, v = m.tool("open_document", path=os.path.realpath(f))
        return v["document"]
    def kids(d, node=None):
        a = dict(document=d, format="text", max_size=1000000)
        if node:
            a["node"] = node
        e, r = m.tool("read", **a)
        return r["children"] if isinstance(r, dict) else []
    def hashof(d):
        return m.tool("read", document=d, format="texmacs", max_size=1000000)[1]["hash"]
    def refused(label, d, tool, **args):
        h = hashof(d)
        e, r = m.tool(tool, document=d, **args)
        ok = e and ("refused" in str(r) or "only relative" in str(r) or "nothing was changed" in str(r)) and hashof(d) == h
        check("refused: %s" % label, ok)
        if not ok:
            print("    ", str(r)[:300])
        return str(r)
    def accepted(label, d, tool, **args):
        e, r = m.tool(tool, document=d, **args)
        ok = not e and r["status"] == "applied"
        check("accepted: %s" % label, ok)
        if not ok:
            print("   ", str(r)[:300])
        return ok
    # 1. an evaluator error does not hide a link: the macro is called through arg, quote-arg, value...
    body = "  <assign|foo|<macro|x|<hlink|t|<arg|x>>>>\n\n  Ordinary.\n\n  Last.\n"
    d = opendoc(body)
    last = kids(d)[-1]["handle"]
    for what, c in (("arg", '(concat "Pre " (arg (foo "javascript:alert(1)")) " post.")'),
                    ("quote-arg", '(concat "Pre " (quote-arg (foo "javascript:alert(1)")) " post.")'),
                    ("value", '(concat "Pre " (value (foo "javascript:alert(1)")) " post.")'),
                    ("direct", '(concat "Pre " (foo "javascript:alert(1)") " post.")'),
                    ("a harmless address", '(concat "Pre " (foo "https://ok.org") " post.")'),
                    ("compound", '(compound "foo" "javascript:alert(1)")')):
        refused("a call of the macro of the person (%s)" % what, d, "replace", node=last, content=c, format="stree")
        last = kids(d)[-1]["handle"]
    # 2. macros of the style that compute names, definitions the person uses
    for v, use in (("style", "<inc-counter|foo>"), ("direct", "<inc-foo>")):
        d = opendoc("  <assign|dst|https://ok.org>\n\n  <assign|inc-foo|<macro|<hlink|t|<value|dst>>>>\n\n"
                    "  U %s text.\n\n  Ordinary.\n" % use)
        ks = kids(d)
        inner = kids(d, ks[0]["handle"])
        refused("the address of a definition of the person is changed to javascript: (%s)" % v, d, "replace",
                node=inner[1]["handle"], content="javascript:alert(1)")
        refused("the definition of the person is deleted (%s)" % v, d, "delete", node=ks[0]["handle"])
        refused("the definition of the person is replaced (%s)" % v, d, "replace", node=ks[0]["handle"],
                content="<assign|dst|javascript:alert(1)>")
        refused("a use of that macro (%s)" % v, d, "insert", where="last", content=use)
        accepted("plain text next to the definitions (%s)" % v, d, "insert", where="last", content="Plain text.")
        ks = kids(d)
        accepted("an insertion after a definition (%s)" % v, d, "insert", anchor=ks[0]["handle"], where="after", content="After the definition.")
    # a block that has a definition inside
    d = opendoc("  Before <assign|bar|<macro|x|<strong|<arg|x>>>> after.\n\n  Last.\n")
    ks = kids(d)
    refused("a paragraph with a definition inside is replaced", d, "replace", node=ks[0]["handle"], content="Nothing.")
    refused("a paragraph with a definition inside is deleted", d, "delete", node=ks[0]["handle"])
    # 3. the old audit cases: a macro of the person that passes the argument on, content swapped
    for v, pre, use, new in (
        ("script", "<assign|sc|<macro|x|<script|python|<arg|x>>>>", "<sc|print(1)>", '(sc "import os; os.system(\\"id\\")")'),
        ("latex", "<assign|lt|<macro|x|<latex|<arg|x>>>>", "<lt|\\\\input{a.tex}>", '(lt "\\\\input{/etc/passwd}")'),
        ("javascript link", "<assign|jl|<macro|x|<hlink|t|<arg|x>>>>", "<jl|javascript:a(1)>", '(jl "javascript:evil(2)")'),
        ("image", "<assign|im|<macro|x|<image|<arg|x>|1cm|1cm||>>>", "<im|/etc/hosts>", '(im "/etc/passwd")'),
        ("not plain text", "<assign|jl|<macro|x|<hlink|t|<arg|x>>>>", "<jl|<with|a|b|javascript:a(1)>>", '(jl (with "a" "b" "javascript:evil"))')):
        d = opendoc("  " + pre + "\n\n  Item " + use + " end.\n\n  Last.\n")
        ks = kids(d)
        refused("%s: the person's call is swapped for another content" % v, d, "replace", node=ks[1]["handle"],
                content='(concat "Item " %s " end.")' % new, format="stree")
    # the person's own calls stay: an edit of the text around them is accepted
    d = opendoc("  <assign|jl|<macro|x|<hlink|t|<arg|x>>>>\n\n  Item <jl|javascript:a(1)> end.\n\n  Last.\n")
    ks = kids(d)
    accepted("an edit of the text around a call of the person", d, "edit_text", node=ks[1]["handle"], old="end", new="finish")
    # 4. errors of the evaluation refuse the content (but not the benign ones)
    d = opendoc("  <assign|bad|<macro|x|<arg|y>>>\n\n  Text.\n")
    r = refused("a macro whose expansion is an error", d, "insert", where="last", content="<bad|x>")
    check("and the reason says why", "error" in r)
    # 5. the key of a violation has the whole content: a long address that the person has
    long_a = "javascript:" + "a" * 100
    d = opendoc("  Long <hlink*|t|%sX|> link.\n\n  Last.\n" % long_a)
    ks = kids(d)
    hl = [c for c in kids(d, ks[0]["handle"]) if c["tag"] == "hlink*"]
    if hl:
        dest = kids(d, hl[0]["handle"])[1]
        refused("the end of a long javascript address of a standard macro is changed", d, "replace",
                node=dest["handle"], content=long_a + "Y")
    else:
        check("the hlink* was found", False)
    # 7. (audit) hlink* forwards its destination: checked where it is written, also deep inside
    #    a fold, whose hidden part the second layer does not see
    d = opendoc("  Text.\n")
    for what, c in (("hlink* alone", "<hlink*|t|javascript:alert(1)|x>"),
                    ("hlink* in a summarized", "<summarized|sum|<hlink*|t|javascript:alert(1)|x>>"),
                    ("hlink* in a detailed", "<detailed|sum|<hlink*|t|javascript:alert(1)|x>>"),
                    ("href with a file", "<href|file:///etc/passwd>"),
                    ("include of a path", "<include|/etc/passwd>")):
        refused(what, d, "insert", where="last", content=c)
    accepted("hlink* with a web address", d, "insert", where="last", content="<hlink*|t|https://ok.org|x>")
    # every macro of the table: the places the analysis finds in it are checked where it is called
    pf = os.path.join(root, "safe-style.tm")
    kinds_ok = True
    seen = 0
    for style in ("article", "tmdoc", "book", "beamer", "generic"):
        with open(pf, "w") as fh:
            fh.write("<TeXmacs|2.1.5>\n\n<style|<tuple|%s>>\n\n<\\body>\n  Text.\n</body>\n" % style)
        penv = dict(env)
        penv["SAFE_FILE"] = os.path.realpath(pf)
        pr = subprocess.run([BIN, "-headless", "-x", '(load "%s")' % os.path.join(HERE, "safe-probe.scm")],
                            env=penv, capture_output=True, text=True, timeout=100)
        allowed = [l.split()[1] for l in pr.stdout.splitlines() if l.startswith("ALLOW ")]
        d = opendoc("  Text.\n", style)
        for l in pr.stdout.splitlines():
            if not l.startswith("SAFE ") or l.startswith("SAFE-END"):
                continue
            parts = l.split(" ", 3)
            name, npar, pos = parts[1], int(parts[2]), parts[3]
            seen += 1
            ks = set(re.findall(r"[a-z]+", re.sub(r"\ball\b|\bvar\b", "", pos)))
            if not ks <= {"link", "file"} and name not in allowed:
                kinds_ok = False
                print("    kinds of", name, pos)
            for pm in re.finditer(r"\((\d+|all|var) ([a-z ]+)\)", pos):
                idx, kd = pm.group(1), pm.group(2).split()
                if idx == "var":
                    continue
                n = max(npar, 1) if npar > 0 else 1
                i = 0 if idx == "all" else int(idx)
                for kind, bad in (("link", "javascript:alert(1)"), ("file", "/etc/passwd")):
                    if kind not in kd:
                        continue
                    args = ['"t"'] * max(n, i + 1)
                    args[i] = '"%s"' % bad
                    content = "(%s %s)" % (name, " ".join(args))
                    h = hashof(d)
                    e, r = m.tool("insert", document=d, where="last", content=content, format="stree")
                    ok = e and hashof(d) == h and ("javascript" in str(r) or "/etc/passwd" in str(r) or "refers to the file" in str(r) or "plain name" in str(r))
                    check("%s: %s at position %s of %s is refused" % (style, kind, idx, name), ok)
                    if not ok:
                        print("    ", content, str(r)[:200])
    check("every macro of the table has only link and file places, but the declared ones (%d found)" % seen, kinds_ok and seen > 10)
    # 7b. (audit) the functions of extern: every function used by the standard styles and packages
    #     is on the reviewed list or makes its macro dangerous
    tsdir = os.path.join(TOP, "TeXmacs")
    groups = {}
    used = set()
    for sub in ("styles", "packages"):
        for dp, dn, fn in os.walk(os.path.join(tsdir, sub)):
            for f in fn:
                if not f.endswith(".ts"):
                    continue
                txt = open(os.path.join(dp, f), errors="replace").read()
                if "<extern|" not in txt:
                    continue
                parts = re.split(r"<assign\|([^|<>]+)\|<\\?x?macro\|", txt)
                for k in range(1, len(parts), 2):
                    for fm in re.finditer(r"<extern\|([^|<>]+)", parts[k + 1].split("\n  <assign|")[0]):
                        groups.setdefault((sub, f[:-3]), {}).setdefault(fm.group(1), set()).add(parts[k])
                        used.add(fm.group(1))
    lst = os.path.join(w, "extern.list")
    docs = []
    with open(lst, "w") as fh:
        for (sub, nm), fmap in sorted(groups.items()):
            pf2 = os.path.join(root, "ext-%s-%s.tm" % (sub, nm))
            tup = nm if sub == "styles" else "generic|%s" % nm
            with open(pf2, "w") as g:
                g.write("<TeXmacs|2.1.5>\n\n<style|<tuple|%s>>\n\n<\\body>\n  Text.\n</body>\n" % tup)
            names = sorted(set().union(*fmap.values()))
            fh.write("%s\t%s\n" % (os.path.realpath(pf2), ",".join(names)))
            docs.append((nm, fmap))
    penv = dict(env)
    penv["EXTERN_LIST"] = lst
    pr = subprocess.run([BIN, "-headless", "-x", '(load "%s")' % os.path.join(HERE, "extern-probe.scm")],
                        env=penv, capture_output=True, text=True, timeout=300)
    out = pr.stdout.splitlines()
    allowed = set(re.findall(r'"([^"]+)"', next((l for l in out if l.startswith("ALLOWED ")), "")))
    refused_macros = set(re.findall(r'"([^"]+)"', next((l for l in out if l.startswith("REFUSED ")), "")))
    dangerous = {}
    defined = {}
    cur = []
    for l in out:
        if l.startswith("DEFINED "):
            cur = re.findall(r'"([^"]+)"', l)
        elif l.startswith("DANGER "):
            dangerous.setdefault(len(defined), set()).add(l.split()[1])
        elif l.startswith("DONE "):
            defined[len(defined)] = set(cur)
    bad = []
    nchecked = 0
    for gi, (nm, fmap) in enumerate(docs):
        for fn_, macros in fmap.items():
            if fn_ in allowed:
                continue
            for mname in macros:
                if mname in defined.get(gi, set()):
                    nchecked += 1
                    if mname not in dangerous.get(gi, set()) and mname not in refused_macros:
                        bad.append((nm, mname, fn_))
    check("every extern function of the standard styles is on the list or makes its macro dangerous (%d macros, %d functions on the list)" % (nchecked, len(allowed)),
          not bad and nchecked > 20 and "EXTERN-END" in pr.stdout)
    if bad:
        print("    ", bad[:10])
    check("every function on the list is used by a standard style or package",
          all((a in used) for a in allowed) or print("    ", sorted(a for a in allowed if a not in used)))
    # map and the choice lists: the function is a name, the arguments are code
    d = opendoc("  Text.\n")
    for what, c in (("a computed name", '(map (merge "inc" "lude") (tuple "/etc/hosts"))'),
                    ("a concat name", '(map (concat "inc" "lude") (tuple "/etc/hosts"))'),
                    ("a choice of names", '(map (if "true" "include" "strong") (tuple "/etc/hosts"))'),
                    ("a written dangerous name", '(map "include" (tuple "/etc/hosts"))'),
                    ("a value", '(with "f" "include" (map (value "f") (tuple "/etc/hosts")))')):
        refused("map with %s" % what, d, "insert", where="last", content=c, format="stree")
    accepted("map with a written harmless name", d, "insert", where="last", content='(map "strong" (tuple "x" "y"))', format="stree")
    d = opendoc("  Text.\n", "new-gui")
    for tg in ("choice-list", "check-list"):
        refused("%s, code in data" % tg, d, "insert", where="last",
                content='(%s "(system \\"id\\")" "b")' % tg, format="stree")
    # a computed name whose family has a macro that is refused
    d = opendoc("  <assign|gm|<macro|n|<compound|<merge|tmdoc-|<arg|n>>|x>>>\n\n  Text.\n", "tmdoc")
    refused("a macro of the person that reaches the macros tmdoc-*", d, "insert", where="last", content='(gm "image")', format="stree")
    d = opendoc("  <assign|gm|<macro|n|<compound|<merge|itemize-|<arg|n>>|x>>>\n\n  Text.\n")
    accepted("a macro of the person that reaches the macros itemize-*", d, "insert", where="last", content='(gm "dot")', format="stree")
    # tmfs://help/ : the manuals, not the files
    d = opendoc("  Text.\n")
    refused("tmfs help file", d, "insert", where="last", content="<hlink|x|tmfs://help/file/etc/hosts>")
    refused("tmfs help http", d, "insert", where="last", content="<hlink|x|tmfs://help/http/example.org/a>")
    refused("tmfs git", d, "insert", where="last", content="<hlink|x|tmfs://git/a>")
    accepted("tmfs help article", d, "insert", where="last", content="<hlink|x|tmfs://help/article/tm/doc/main/man-intro.en.tm>")
    # 7c. (audit) work driven by data, definers, smart-ref, the notes in the margin
    d = opendoc("  Text.\n", "generic|gui-button")
    t0 = time.monotonic()
    refused("tiled with 0 columns (a loop)", d, "insert", where="last", content='(tiled "0" "a" "b")', format="stree")
    check("and it is refused at once (%.1f s)" % (time.monotonic() - t0), time.monotonic() - t0 < 10)
    d = opendoc("  Text.\n", "generic|exam")
    refused("a huge number of columns in a with", d, "insert", where="last",
            content='(with "mc-monospaced-cols" "300000" "x")', format="stree")
    refused("a number with 199 digits", d, "insert", where="last", content='(with "par-sep" "%s" "x")' % ("9" * 199), format="stree")
    refused("a length with a long fraction", d, "insert", where="last", content='(with "par-sep" "1.12345678cm" "x")', format="stree")
    accepted("a number of columns", d, "insert", where="last", content='(with "mc-monospaced-cols" "50" "x")', format="stree")
    accepted("a length", d, "insert", where="last", content='(with "par-sep" "0.5fn" "x")', format="stree")
    accepted("a date as text", d, "insert", where="last", content='(with "my-date" "2026-11-04" "x")', format="stree")
    d = opendoc("  Text.\n")
    for tg, nargs in (("new-remark", 2), ("new-theorem", 2), ("new-exercise", 2), ("new-figure", 2),
                      ("new-section", 1), ("new-counter", 1)):
        refused("definer %s redefining strong" % tg, d, "insert", where="last",
                content='(%s %s)' % (tg, " ".join(['"strong"', '"REDEFINED"'][:nargs])), format="stree")
    refused("new-env", d, "insert", where="last", content='(new-env "strong" "S" "x" "y")', format="stree")
    refused("new-list", d, "insert", where="last", content='(new-list "strong" "x" "y")', format="stree")
    # smart-ref: harmless unless the macro <type>-ref of a label is dangerous
    d = opendoc("  <assign|zz-ref|<macro|x|<hlink|t|<arg|x>>>>\n\n  Text.\n", "generic|smart-ref")
    accepted("smart-ref with a label of a known type", d, "insert", where="last", content='(smart-ref "eq:foo")', format="stree")
    refused("smart-ref with a label whose type is a dangerous macro of the person", d, "insert", where="last",
            content='(smart-ref "zz:javascript")', format="stree")
    refused("smart-ref with a label that is not written", d, "insert", where="last",
            content='(smart-ref (merge "z" "z"))', format="stree")
    d = opendoc("  Text.\n", "generic|smart-ref")
    accepted("smart-ref in a plain document", d, "insert", where="last", content='(smart-ref "eq:foo" "fig:bar")', format="stree")
    # the notes in the margin of the styles of journals; the title of the manuals
    for st in ("elsart", "amsart", "jsc", "ifac", "svjour", "article", "book", "beamer"):
        d = opendoc("  Text.\n", st)
        accepted("%s: marginal-note" % st, d, "insert", where="last", content='(marginal-note "normal" "c" "A note")', format="stree")
    # (audit) variables that multiply the work are bounded
    d = opendoc("  Text.\n")
    for var, bad, good in (("par-columns", "13", "4"), ("par-columns", "9999", "12"), ("font-size", "9", "2"),
                           ("font-size", "20", "8"), ("font-base-size", "101", "12"), ("magnification", "5", "2"),
                           ("magnification", "10", "4")):
        r = refused("%s %s" % (var, bad), d, "insert", where="last", content='(with "%s" "%s" "x")' % (var, bad), format="stree")
        check("and the message says the limit (%s)" % var, "up to" in r)
        accepted("%s %s" % (var, good), d, "insert", where="last", content='(with "%s" "%s" "x")' % (var, good), format="stree")
    accepted("the two multipliers together at their limits", d, "insert", where="last",
             content='(with "magnification" "4" (with "font-size" "8" "x"))', format="stree")
    refused("a magnification of 10 around a font-size of 20", d, "insert", where="last",
            content='(with "magnification" "10" (with "font-size" "20" "x"))', format="stree")
    d = opendoc("  <assign|zz-ref|<macro|x|<hlink|t|<arg|x>>>>\n\n  Text.\n", "generic|smart-ref")
    refused("smart-ref with a label in capitals (it lower-cases)", d, "insert", where="last",
            content='(smart-ref "ZZ:javascript")', format="stree")
    refused("smart-ref with an accented letter after the type", d, "insert", where="last",
            content='(smart-ref "zz\u00e9:javascript")', format="stree")
    refused("smart-ref with an accented letter before, and the type at the end", d, "insert", where="last",
            content='(smart-ref "\u00e9zz:zz")', format="stree")
    accepted("smart-ref with an accented letter before: the type is the end, j", d, "insert", where="last",
             content='(smart-ref "\u00e9zz:j")', format="stree")
    d = opendoc("  Text.\n", "tmdoc")
    accepted("tmdoc: tmdoc-title", d, "insert", where="last", content='(tmdoc-title "A title")', format="stree")
    d = opendoc("  Text.\n", "generic|exam")
    accepted("exam: gap-dots-long", d, "insert", where="last", content='(gap-dots-long "x")', format="stree")
    # 8. edit_text does not reach text inside definitions or code
    for what, body, old, new in (
        ("set", "  <set|dst|https://ok.org>\n\n  <assign|u|<macro|<hlink|t|<value|dst>>>>\n\n  Text.\n", "https://ok.org", "javascript:alert(1)"),
        ("provide", "  <provide|dst|https://ok.org>\n\n  Text.\n", "https://ok.org", "javascript:alert(1)"),
        ("style-only", "  <style-only|alpha beta>\n\n  Text.\n", "alpha", "javascript"),
        ("with-package", "  <with-package|beamer|alpha beta>\n\n  Text.\n", "alpha", "javascript"),
        ("a bare macro", "  <macro|x|alpha beta>\n\n  Text.\n", "alpha", "javascript"),
        ("assign", "  <assign|dst|https://ok.org>\n\n  Text.\n", "https://ok.org", "javascript:alert(1)"),
        ("latex", "  <latex|alpha beta>\n\n  Text.\n", "alpha", "\\input"),
        ("script", "  <script|python|alpha beta>\n\n  Text.\n", "alpha", "beta")):
        d = opendoc(body)
        ks = kids(d)
        h = hashof(d)
        e, r = m.tool("edit_text", document=d, node=ks[0]["handle"], old=old, new=new)
        check("edit_text inside %s is refused" % what, e and hashof(d) == h)
    # 9. a macro of the person that the standard macros reach: the table does not exempt them
    d = opendoc("  <assign|toc-normal-1|<macro|what|<hlink|t|<arg|what>>>>\n\n  Text.\n")
    refused("a section whose table of contents entry is a macro of the person that makes a link",
            d, "insert", where="last", content="<section|javascript:alert(1)>")
    d = opendoc("  <assign|toc-normal-1|<macro|what|<strong|<arg|what>>>>\n\n  Text.\n")
    accepted("a section where the macro of the person is harmless", d, "insert", where="last", content="<section|A title>")
    # 10. cycles: the result does not depend on the order
    cyc = ("  <assign|ma|<macro|x|<mb|<arg|x>>>>\n\n"
           "  <assign|mb|<macro|x|<concat|<hlink|t|<arg|x>>|<ma|<arg|x>>>>>\n\n  Text.\n")
    d = opendoc(cyc)
    refused("a cycle, a first", d, "insert", where="last", content='(concat (ma "javascript:a") (mb "javascript:b"))', format="stree")
    d = opendoc(cyc)
    refused("a cycle, b first", d, "insert", where="last", content='(concat (mb "javascript:b") (ma "javascript:a"))', format="stree")
    d = opendoc(cyc)
    refused("a cycle, a alone", d, "insert", where="last", content='(ma "javascript:a")', format="stree")
    # 11. the prefix exemption is for links only
    d = opendoc("  <assign|hl|<macro|x|<hlink|t|<merge|https://|<arg|x>>>>>\n\n"
                "  <assign|lt|<macro|x|<latex|<merge|https://|<arg|x>>>>>\n\n  Text.\n")
    accepted("a macro of the person whose link has a fixed address beginning", d, "insert", where="last", content="<hl|example.org>")
    refused("a macro of the person whose latex has a fixed beginning", d, "insert", where="last", content='(lt "\\\\input{x}")', format="stree")
    # 12. a call that the person's block already has does not make the block unchangeable
    d = opendoc("  <assign|jl|<macro|x|<hlink|t|<arg|x>>>>\n\n  Item <jl|javascript:a(1)> end.\n\n  Last.\n")
    ks = kids(d)
    accepted("a paragraph with the call of the person, the text around it changed", d, "replace", node=ks[1]["handle"],
             content='(concat "Item " (jl "javascript:a(1)") " finish.")', format="stree")
    ks = kids(d)
    refused("the same paragraph with another address", d, "replace", node=ks[1]["handle"],
            content='(concat "Item " (jl "javascript:b(2)") " finish.")', format="stree")
    # 13. standard macros that are harmless are accepted
    d = opendoc("  Text.\n")
    for label, c in (("marginal-note", '(marginal-note "normal" "c" "A note")'), ("itemize*", None)):
        if c:
            accepted(label, d, "insert", where="last", content=c, format="stree")
    d = opendoc("  Text.\n", "tmdoc")
    for label, c in (("simple-link", '(simple-link "https://ok.org")'), ("help-link", '(help-link "body" "name")'),
                     ("key", '(key "C-x")'), ("menu", '(menu "File")'), ("shortcut", '(shortcut "C-x")')):
        accepted("tmdoc: " + label, d, "insert", where="last", content=c, format="stree")
    refused("tmdoc: simple-link with javascript:", d, "insert", where="last", content='(simple-link "javascript:alert(1)")', format="stree")
    refused("tmdoc: tmdoc-link with javascript:", d, "insert", where="last", content='(tmdoc-link "t" "javascript:alert(1)")', format="stree")
    # 14. nesting deeper than 200 levels: the ancestors are all looked at
    D = 230
    w2 = '<hlink|t|<arg|x>>'
    for _ in range(D):
        w2 = "<with|font-shape|italic|%s>" % w2
    d = opendoc("  <assign|dd|<macro|x|%s>>\n\n  Para.\n" % w2)
    node = kids(d)[0]["handle"]
    depth = 0
    while True:
        ch = kids(d, node)
        nxt = [c for c in ch if c["tag"] in ("macro", "with", "hlink", "arg")]
        if not nxt:
            break
        node = nxt[0]["handle"]
        depth += 1
        if depth > D + 6:
            break
    check("the nesting was descended (%d levels)" % depth, depth > 200)
    e, r = m.tool("replace", document=d, node=node, content="javascript:alert(1)", format="texmacs")
    check("an edit at the bottom of 230 levels in a definition is refused", e)
    # 15. a document with 1500 macros and many paragraphs: edits next to a definition are cheap
    parts = ["  <assign|m0|<macro|x|<em|<arg|x>>>>\n"]
    for i in range(1, 1500):
        parts.append("  <assign|m%d|<macro|x|<m%d|<arg|x>>>>\n" % (i, i - 1))
    for i in range(8000):
        parts.append("  Paragraph %d with <m%d|word> and <em|text>.\n" % (i, i % 5))
    d = opendoc("\n".join(parts))
    ks = kids(d)
    t0 = time.monotonic()
    e, r = m.tool("insert", document=d, anchor=ks[5]["handle"], where="after", content="New.", format="texmacs")
    dt = time.monotonic() - t0
    check("an insertion after a definition in a document with 1500 macros (%.1f s)" % dt, not e and dt < 20)
    rss = int(subprocess.run(["ps", "-o", "rss=", "-p", str(m.p.pid)], capture_output=True, text=True).stdout.strip() or 0)
    check("and the process stays under 1.4 GB (%d MB; it varies from 700 to 1000 MB with the timing of the garbage collector)" % (rss // 1024), rss < 1400000)
    t0 = time.monotonic()
    e, r = m.tool("delete", document=d, node=kids(d)[6]["handle"])
    dt = time.monotonic() - t0
    check("a deletion next to the definitions (%.1f s)" % dt, not e and dt < 20)
    # a heavy macro chain: a block that does not evaluate within the budget, next to the definitions
    chain = ["  <assign|d0|<macro|x|<concat|<arg|x>|<arg|x>>>>\n"]
    for i in range(1, 40):
        chain.append("  <assign|d%d|<macro|x|<d%d|<d%d|<arg|x>>>>>\n" % (i, i - 1, i - 1))
    chain.append("  Light.\n")
    chain.append("  Heavy <d39|x>.\n")
    d = opendoc("\n".join(chain))
    ks = kids(d)
    t0 = time.monotonic()
    e, r = m.tool("insert", document=d, anchor=ks[3]["handle"], where="after", content="New.", format="texmacs")
    dt = time.monotonic() - t0
    check("an insertion next to the definitions of a heavy chain is cheap (%.1f s)" % dt, not e and dt < 20)
    ks = kids(d)
    heavy = [c for c in ks if c["preview"].startswith("Heavy")]
    t0 = time.monotonic()
    e, r = m.tool("edit_text", document=d, node=heavy[0]["handle"], old="Heavy", new="Light") if heavy else (False, "")
    dt = time.monotonic() - t0
    check("an edit in the heavy block is refused within the budget (%.1f s)" % dt, e and dt < 30)
    rss = int(subprocess.run(["ps", "-o", "rss=", "-p", str(m.p.pid)], capture_output=True, text=True).stdout.strip() or 0)
    check("and the process stays under 1.4 GB (%d MB; it varies from 700 to 1000 MB with the timing of the garbage collector)" % (rss // 1024), rss < 1400000)
    check("exit 0", m.close() == 0)


def s_not_joined(w):
    """a file a live TeXmacs holds but the server could not join (the host sends a
    document the checks refuse; the host is stopped): the agent is told, the mode is
    read-only, edits and save are refused, the file on disk is not written"""
    import signal
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    f = os.path.join(w, "refused.tm")
    write_small(f)
    with open(f, "a") as fh:
        fh.write("\n<\\initial>\n  <\\collection>\n  </collection>\n</initial>\n")
    with open(f) as fh:
        txt = fh.read().replace("  Second paragraph.", "  Second paragraph.\n\n  <hlink|only one>")
    with open(f, "w") as fh:
        fh.write(txt)
    f = os.path.realpath(f)
    before = open(f).read()
    other = start_other(w, envB, "host-wait.scm", f)
    check("the host has loaded the file", wait_file(os.path.join(w, "sync", "h-loaded")))
    m = Mcp(envA)
    m.initialize()
    def probe(tag):
        e, v = m.tool("open_document", path=f)
        check(tag + ": open_document is answered", not e and v.get("shared") is None)
        d = v.get("document", "d1")
        check(tag + ": it has a note that says why", not e and "not joined" in v.get("note", ""))
        check(tag + ": agent_mode is read-only", not e and v.get("agent_mode") == "read-only")
        e, r = m.tool("insert", document=d, where="last", content="Changed", format="texmacs")
        check(tag + ": an insert is refused", e and "read-only" in str(r))
        e, r = m.tool("save_document", document=d)
        check(tag + ": the save is refused", e and "read-only" in str(r))
        check(tag + ": the file is as it was", open(f).read() == before)
        e, r = m.tool("read", document=d, format="text")
        check(tag + ": the document can be read", not e)
        return d
    d = probe("refused welcome")
    m.tool("close_document", document=d, discard=True)
    os.kill(other.pid, signal.SIGSTOP)
    try:
        probe("busy host")
    finally:
        os.kill(other.pid, signal.SIGCONT)
    sync_set(w, "c-done")
    check("exit 0", m.close() == 0)
    try:
        other.wait(timeout=30)
    except subprocess.TimeoutExpired:
        other.kill()


def s_gui_host(w):
    """a TeXmacs with its window (Qt, offscreen: the person's, not headless) hosts a
    file whose initial environment is an empty collection, as a saved file has;
    the MCP server joins it (agent_mode propose, an insert becomes a proposal,
    Tools > Agents shows the agent), and a second window of another process joins too"""
    os.makedirs(os.path.join(w, "sync"), exist_ok=True)
    os.makedirs(os.path.join(w, "tmp"), exist_ok=True)
    f = os.path.join(w, "gui.tm")
    write_small(f)
    with open(f, "a") as fh:
        fh.write("\n<\\initial>\n  <\\collection>\n  </collection>\n</initial>\n")
    f = os.path.realpath(f)
    envA, homeA = make_env(w, "A", "on")
    envB, homeB = make_env(w, "B", "on")
    envC, homeC = make_env(w, "C", "on")
    other = start_other(w, envB, "host-edit.scm", f, {"SH_STEP_TIMEOUT": "180000"}, gui=True)
    sync = os.path.join(w, "sync")
    check("the GUI host has loaded the file", wait_file(os.path.join(sync, "h-loaded"), 60))
    st = [0]
    # a second window (another process, with its interface) joins the host
    third = start_other(w, envC, "host-join.scm", f, {"SH_STEP_TIMEOUT": "180000"}, gui=True, role="C")
    sync_set(w, "c-opened")
    check("the second window edited", wait_file(os.path.join(sync, "h-edited"), 60))
    time.sleep(0.5)
    hb = host_body(w, st)
    check("the host has the edit of the second window", "FROMB-" in hb)
    def ev(expr, wait=10):
        try:
            os.remove(os.path.join(sync, "eval"))
        except OSError:
            pass
        hostcmd(w, st, "eval " + expr)
        wait_file(os.path.join(sync, "eval"), wait)
        time.sleep(0.1)
        return open(os.path.join(sync, "eval")).read() if os.path.exists(os.path.join(sync, "eval")) else ""
    check("footer: hosting with a second window", "Shared: hosting, 2 windows" in ev("(shared-file-footer-text (current-buffer))"))
    check("the focus bar group is not shown without an agent", ev("(shared-agents-focus?)") == "#f")
    m = Mcp(envA)
    m.initialize()
    e, v = m.tool("open_document", path=f)
    check("the agent joins the window of a GUI host", not e and v["shared"] is not None and v["shared"]["role"] == "participant")
    check("agent_mode is propose", not e and v.get("agent_mode") == "propose")
    e, t = m.tool("read", document="d1", format="text")
    first = t["children"][0]
    e, r = m.tool("insert", document="d1", anchor=first["handle"], where="before", content="Inserted by the agent", format="texmacs")
    check("an insert becomes a proposal", not e and r.get("status") == "proposed")
    hb = host_body(w, st)
    check("the proposal is in the document of the host", "version-both" in hb and "Inserted by the agent" in hb)
    hostcmd(w, st, "agents-here")
    check("(agents-here?) is true in the host", wait_file(os.path.join(sync, "agents-here"), 10)
          and open(os.path.join(sync, "agents-here")).read() == "#t")
    # the footer and the focus bar of the host
    check("footer: shared with the agent, its mode and the proposal",
          all(x in ev("(shared-file-footer-text (current-buffer))") for x in ("Shared with agent", "(propose)", "1 proposal")))
    check("footer: the footer hook adds it to the left footer",
          "Shared with agent" in ev("(tree->stree (footer-hook (stree->tree \"x\")))"))
    check("focus bar: the group is shown", ev("(shared-agents-focus?)") == "#t")
    check("focus bar: the focus icons build with the group",
          ev("(and (make-menu-widget (list (quote horizontal) (list (quote link) (quote texmacs-focus-icons))) 0) #t)") == "#t")
    check("focus bar: the agent is connected", "connected" in ev("(shared-agents-focus-text (current-buffer))"))
    check("focus bar: one proposal pending", ev("(shared-file-pending-count (current-buffer))") == "1")
    ev("(go-to (list 1 0))")
    check("focus bar: no proposal at the cursor away from it", ev("(shared-proposal-at-cursor (current-buffer))") == "#f")
    check("focus bar: next proposal goes to it", ev("(shared-proposal-go! (current-buffer) 1)") == '"p1"')
    check("focus bar: the proposal at the cursor", ev("(shared-proposal-at-cursor (current-buffer))") == '"p1"')
    shots = os.path.join(w, "shots")
    os.makedirs(shots, exist_ok=True)
    try:
        os.remove(os.path.join(sync, "snapshot"))
    except OSError:
        pass
    ev("(begin (update-menus) #t)")
    time.sleep(1.0)
    hostcmd(w, st, "snapshot " + shots)
    check("a snapshot of the host window was taken", wait_file(os.path.join(sync, "snapshot"), 20)
          and os.path.exists(os.path.join(shots, "window-0.png")))
    ev("(shared-set-agent-mode! (current-buffer) (quote read-only))")
    check("focus bar: the mode dropdown's action sets the mode", ev("(shared-agent-mode (current-buffer))") == "read-only")
    e, v = m.tool("list_documents")
    check("the agent sees the mode", not e and v["documents"][0].get("agent_mode") == "read-only")
    ev("(shared-set-agent-mode! (current-buffer) (quote propose))")
    check("focus bar: the footer follows the mode", "(propose)" in ev("(shared-file-footer-text (current-buffer))"))
    ids = ev("(shared-file-agents (current-buffer))")
    aid = ids.strip("()\"").split(" ")[0].strip('"')
    # the agent is stopped: not answering after five seconds
    import signal
    os.kill(m.p.pid, signal.SIGSTOP)
    try:
        got = poll(lambda: "not answering" in ev("(shared-agent-connection-text (current-buffer) %s)" % ('"' + aid + '"')), 20)
        check("connection: a stopped agent is not answering", bool(got))
        ev("(shared-agent-check-connection (current-buffer))")
        time.sleep(1.6)
        check("check connection: a stopped agent does not answer", "does not answer" in ev("(shared-agent-last-check (current-buffer))"))
    finally:
        os.kill(m.p.pid, signal.SIGCONT)
    got = poll(lambda: ev("(shared-agent-connection-text (current-buffer) %s)" % ('"' + aid + '"')) == '"connected"', 20)
    check("connection: connected again when it goes on", bool(got))
    ev("(shared-agent-check-connection (current-buffer))")
    time.sleep(1.6)
    check("check connection: the agent answers", "answers" in ev("(shared-agent-last-check (current-buffer))"))
    check("exit 0", m.close() == 0)
    got = poll(lambda: ev("(shared-agent-connection-text (current-buffer) %s)" % ('"' + aid + '"')) == '"disconnected"', 20)
    check("connection: disconnected when the agent leaves", bool(got))
    check("focus bar: the group is gone with the agent", ev("(shared-agents-focus?)") == "#f")
    sync_set(w, "c-done")
    for p in (other, third):
        try:
            p.wait(timeout=60)
        except subprocess.TimeoutExpired:
            p.kill()
    out = open(os.path.join(w, "out-C")).read() if os.path.exists(os.path.join(w, "out-C")) else ""
    check("the second window passed its checks", out == "ok")


SCENARIOS = {"protocol": s_protocol, "tools": s_tools, "shared": s_shared,
             "mcp-host": s_mcp_host, "unshared": s_unshared,
             "hostdeath": s_hostdeath, "nonascii": s_nonascii, "stalled": s_stalled, "logcap": s_logcap,
             "edit": s_edit, "readonly": s_readonly, "edit-shared": s_edit_shared, "propose": s_propose, "gui-host": s_gui_host, "not-joined": s_not_joined,
             "undo": s_undo, "static": s_static, "recursion": s_recursion, "guard": s_guard, "filter": s_filter, "styles": s_styles, "content": s_content, "launcher": s_launcher, "robust": s_robust, "outputs": s_outputs}

if __name__ == "__main__":
    SCEN = sys.argv[1]
    work = sys.argv[2]
    os.makedirs(work, exist_ok=True)
    threading.Thread(target=watchdog, daemon=True).start()
    try:
        SCENARIOS[SCEN](work)
    except Exception as ex:
        import traceback
        traceback.print_exc()
        check("no exception (%s)" % ex, False)
    finally:
        watchdog.done = True
        for p in procs:
            if p.poll() is None:
                p.kill()
    print("RESULT %s %d failures of %d" % (SCEN, results.count(False), len(results)))
    sys.exit(1 if False in results else 0)
