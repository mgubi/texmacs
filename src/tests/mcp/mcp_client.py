#!/usr/bin/env python3
"""A small MCP client which tests `texmacs --mcp' (src/docs/mcp-server.md).

usage: mcp_client.py <scenario> <workdir>

It starts `texmacs.bin --mcp' (and, for some scenarios, a second headless
TeXmacs on the same file), speaks JSON-RPC to it on a pipe, and checks the
answers. Every process is stopped above TM_TEST_RSS_KB (2 GB) of memory and
after TM_TEST_TIMEOUT seconds (120). Prints "CHECK <scenario> <name> PASS|FAIL";
the exit status is the number of failures (capped at 1).
"""
import json, os, select, subprocess, sys, threading, time

HERE = os.path.dirname(os.path.abspath(__file__))
TOP = os.path.abspath(os.path.join(HERE, "..", ".."))
BIN = os.path.join(TOP, "TeXmacs", "bin", "texmacs.bin")
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
    start = time.time()
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
        if time.time() - start > TIMEOUT:
            print("  timeout: stopping the processes", file=sys.stderr)
            for p in alive:
                p.kill()
            return
        if getattr(watchdog, "done", False):
            return
        time.sleep(0.3)


class Mcp:
    """A `texmacs --mcp' process and its protocol."""

    def __init__(self, env, args=()):
        self.p = subprocess.Popen([BIN, "--mcp"] + list(args), stdin=subprocess.PIPE,
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
        end = time.time() + timeout
        while b"\n" not in self.buf:
            left = end - time.time()
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
    end = time.time() + timeout
    while time.time() < end:
        if os.path.exists(path):
            return True
        time.sleep(0.05)
    return False


def poll(fn, timeout=15):
    end = time.time() + timeout
    while time.time() < end:
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


def start_other(w, env, script, file):
    """the other TeXmacs (a headless process with a script of tests/mcp)"""
    sync = os.path.join(w, "sync")
    e = dict(env)
    e.update(SH_ROLE="B", SH_SYNC=sync, SH_FILE=file, SH_OUT=os.path.join(w, "out-B"),
             SH_DIR=os.path.join(TOP, "tests", "shared"))
    log = open(os.path.join(w, "log-B"), "wb")
    p = subprocess.Popen([BIN, "-headless", "-X", "-x",
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
                                           "read", "find", "get_references"]))
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


SCENARIOS = {"protocol": s_protocol, "tools": s_tools, "shared": s_shared,
             "mcp-host": s_mcp_host, "unshared": s_unshared}

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
