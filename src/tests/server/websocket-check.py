#!/usr/bin/env python3
"""Checks of the port of the TeXmacs server for the WebSocket clients.

usage: tests/server/websocket-check.py [--bin texmacs.bin] [--home dir]

Starts TeXmacs as a server (no window, QT_QPA_PLATFORM=offscreen) in a
scratch home, once per configuration of the preferences of the WebSocket
port, and talks to it from this script as a WebSocket client on localhost:
the check of the Origin of the handshake, the limit on the size of what a
client sends, text messages, and the ports which cannot be served (out of
range, the port of the server itself, a port in use, not a number). The
server log is read from the output of TeXmacs.

The preferences are those of the scratch home (system/preferences.scm,
written before each start), by default tests/build/server/home: nothing is
read or written in the home of the user. The ports are taken between
20000 and 40000, among those which are free; the processes started are
stopped by their number. Needs a TeXmacs built with the Qt WebSockets module, Qt
6.4 or later; prints the cases and exits with 1 when one of them fails.
"""

import argparse
import base64
import os
import random
import re
import signal
import socket
import struct
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))
TOP = os.path.dirname(os.path.dirname(HERE))

failures = []
count = 0


def check(name, ok, detail=""):
    global count
    count += 1
    if ok:
        print("  ok    %s" % name)
    else:
        print("  FAIL  %s%s" % (name, (": " + detail) if detail else ""))
        failures.append(name)


def port_free(port):
    for family, addr in ((socket.AF_INET, "127.0.0.1"),
                         (socket.AF_INET, "0.0.0.0")):
        s = socket.socket(family, socket.SOCK_STREAM)
        try:
            s.bind((addr, port))
        except OSError:
            return False
        finally:
            s.close()
    return True


def free_port_pair():
    """A port P such that P and P+1 are free, below the ports which the
    system gives to the connections of the clients."""
    for _ in range(200):
        p = random.randrange(20000, 40000)
        if port_free(p) and port_free(p + 1):
            return p
    raise RuntimeError("no free pair of ports")


def listening(port, host="127.0.0.1"):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.settimeout(2)
    try:
        s.connect((host, port))
        return True
    except OSError:
        return False
    finally:
        s.close()


def scm_quote(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


class Server:
    def __init__(self, args, name, prefs):
        self.name = name
        self.home = args.home
        self.log = os.path.join(args.home, "..", "server-%s.log" % name)
        os.makedirs(os.path.join(self.home, "system"), exist_ok=True)
        with open(os.path.join(self.home, "system", "preferences.scm"),
                  "w") as f:
            for k, v in sorted(prefs.items()):
                f.write("(%s %s)\n" % (scm_quote(k), scm_quote(v)))
        env = dict(os.environ)
        env["TEXMACS_PATH"] = os.path.join(TOP, "TeXmacs")
        env["TEXMACS_HOME_PATH"] = self.home
        env["QT_QPA_PLATFORM"] = "offscreen"
        self.out = open(self.log, "w")
        self.proc = subprocess.Popen(
            ["perl", "-e", "alarm shift; exec @ARGV", "300", args.bin,
             "-headless", "-X", "-x", "(server-start)"],
            env=env, stdout=self.out, stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL)
        self.port = int(prefs["server port"])

    def wait(self, timeout=120):
        """Until the server says that it listens. The native port is not
        tried here: the number of the socket of a native client which
        leaves may be closed a second time, and be that of a WebSocket by
        then (pull request #335)."""
        end = time.time() + timeout
        while time.time() < end:
            if self.proc.poll() is not None:
                return False
            if "waiting for connections" in self.text():
                time.sleep(1)  # the WebSocket port comes right after
                return True
            time.sleep(0.5)
        return False

    def text(self):
        self.out.flush()
        with open(self.log, errors="replace") as f:
            return f.read()

    def stop(self):
        if self.proc.poll() is None:
            os.kill(self.proc.pid, signal.SIGTERM)
            try:
                self.proc.wait(10)
            except subprocess.TimeoutExpired:
                os.kill(self.proc.pid, signal.SIGKILL)
                self.proc.wait()
        self.out.close()


class WebSocket:
    """A minimal client: binary or text messages, masked frames."""

    def __init__(self, port, origin=None):
        self.sock = socket.create_connection(("127.0.0.1", port), timeout=10)
        key = base64.b64encode(os.urandom(16)).decode()
        req = ("GET / HTTP/1.1\r\nHost: 127.0.0.1:%d\r\n"
               "Upgrade: websocket\r\nConnection: Upgrade\r\n"
               "Sec-WebSocket-Key: %s\r\nSec-WebSocket-Version: 13\r\n"
               "Sec-WebSocket-Protocol: binary\r\n" % (port, key))
        if origin is not None:
            req += "Origin: %s\r\n" % origin
        self.sock.sendall((req + "\r\n").encode())
        head = b""
        while b"\r\n\r\n" not in head:
            data = self.sock.recv(4096)
            if not data:
                break
            head += data
        self.head, _, self.rest = head.partition(b"\r\n\r\n")
        line = self.head.split(b"\r\n")[0].decode(errors="replace")
        m = re.match(r"HTTP/1.1 (\d+)", line)
        self.status = int(m.group(1)) if m else 0

    def send(self, payload, opcode=2, fin=True):
        head = bytes([(0x80 if fin else 0) | opcode])
        n = len(payload)
        if n < 126:
            head += bytes([0x80 | n])
        elif n < 65536:
            head += bytes([0x80 | 126]) + struct.pack(">H", n)
        else:
            head += bytes([0x80 | 127]) + struct.pack(">Q", n)
        mask = os.urandom(4)
        # masking by whole words of 4 bytes is fast enough for some MB
        k = int.from_bytes(mask * 1024, "big")
        out = bytearray()
        for i in range(0, n, 4096):
            chunk = payload[i:i + 4096]
            m = int.from_bytes(chunk, "big") ^ (k >> (8 * (4096 - len(chunk))))
            out += m.to_bytes(len(chunk), "big")
        self.sock.sendall(head + mask + bytes(out))

    def recv_exact(self, n):
        out = b""
        while len(out) < n:
            if self.rest:
                take = self.rest[:n - len(out)]
                self.rest = self.rest[len(take):]
                out += take
                continue
            data = self.sock.recv(n - len(out))
            if not data:
                raise EOFError
            out += data
        return out

    def recv(self, timeout=10):
        """(opcode, payload) of the next frame; (None, b"") at the end."""
        self.sock.settimeout(timeout)
        try:
            b1, b2 = self.recv_exact(2)
            n = b2 & 0x7f
            if n == 126:
                n = struct.unpack(">H", self.recv_exact(2))[0]
            elif n == 127:
                n = struct.unpack(">Q", self.recv_exact(8))[0]
            return b1 & 0x0f, self.recv_exact(n)
        except (EOFError, ConnectionResetError):
            return None, b""

    def closed(self, timeout=15):
        """Whether the server closes the connection (close frame or end)."""
        end = time.time() + timeout
        while time.time() < end:
            try:
                op, _ = self.recv(max(0.1, end - time.time()))
            except socket.timeout:
                return False
            if op is None or op == 8:
                return True
        return False

    def ping(self, serial=1):
        """A request of the server which any client may send: the server
        answers that the command does not exist."""
        msg = ("(%d (websocket-check-ping))" % serial).encode()
        self.send(str(len(msg)).encode() + b"\n" + msg)
        data = b""
        end = time.time() + 15
        while time.time() < end:
            try:
                op, payload = self.recv(max(0.1, end - time.time()))
            except socket.timeout:
                break
            if op is None or op == 8:
                break
            data += payload
            if b"websocket-check-ping" in data:
                return True
        return False

    def close(self):
        try:
            self.sock.close()
        except OSError:
            pass


def send_big(ws, payload, raw=False, **kw):
    """Sends a message (raw: bytes as they are), and says False when the
    server closed the link meanwhile."""
    try:
        if raw:
            ws.sock.sendall(payload)
        else:
            ws.send(payload, **kw)
        return True
    except (BrokenPipeError, ConnectionResetError, socket.timeout):
        return False


MB = 1024 * 1024


def base_prefs(server_port, **more):
    prefs = {"server port": str(server_port), "tls-server": "off"}
    for k, v in more.items():
        prefs["server websocket " + k.replace("_", " ")] = v
    return prefs


def run(args, name, prefs, cases):
    print("%s:" % name)
    srv = Server(args, name, prefs)
    try:
        if not srv.wait():
            check(name + ": the server starts", False, srv.log)
            return
        try:
            cases(srv)
        except OSError as e:
            check(name + ": the cases run to their end", False, repr(e))
    finally:
        srv.stop()


def default_cases(srv):
    w = srv.port + 1
    log = srv.text()
    check("the port after the one of the server listens", listening(w))
    check("the log names the address of the port",
          ("at 127.0.0.1:%d" % w) in log, "log: " + log[-400:])
    ws = WebSocket(w)
    check("a client without Origin is accepted", ws.status == 101,
          str(ws.status))
    check("it is served", ws.status == 101 and ws.ping())
    ws.close()
    for origin in ("http://localhost:8080", "http://127.0.0.1:3000",
                   "https://localhost", "http://[::1]:8000"):
        ws = WebSocket(w, origin)
        check("a page of this machine (%s) is accepted" % origin,
              ws.status == 101 and ws.ping(), str(ws.status))
        ws.close()
    for origin in ("https://example.org", "http://localhost.example.org",
                   "http://127.0.0.1.example.org:8080", "null",
                   "file://"):
        ws = WebSocket(w, origin)
        check("another page (%s) is refused" % origin, ws.status != 101,
              str(ws.status))
        ws.close()
    # the limit, 16 MB by default
    ws = WebSocket(w)
    ok = send_big(ws, b"x" * (16 * MB + 1))
    check("a message above 16 MB closes the link", not ok or ws.closed())
    ws.close()
    ws = WebSocket(w)
    msg = b"(2 (websocket-check-ping))" + b" " * (4 * MB)
    ws.send(str(len(msg)).encode() + b"\n" + msg)
    op, data = ws.recv(20)
    check("a message of 4 MB is served", op == 2 and b"websocket-check" in data)
    ws.close()
    # a packet which never ends, in messages below the limit
    ws = WebSocket(w)
    ok = send_big(ws, b"999999999\n" + b"x" * MB)
    for i in range(20):
        if not ok:
            break
        ok = send_big(ws, b"x" * MB)
    check("an unfinished packet above 16 MB closes the link",
          not ok or ws.closed())
    ws.close()
    ws = WebSocket(w)
    msg = b"(3 (websocket-check-ping))"
    ws.send(str(len(msg)).encode() + b"\n" + msg, opcode=1)
    check("a text message closes the link", ws.closed())
    ws.close()
    # Qt closes such a client itself; what the client sends after that
    # must not keep the server busy
    ws = WebSocket(w)
    ok = send_big(ws, b"", opcode=3)  # a reserved opcode
    for i in range(8):
        if not ok:
            break
        ok = send_big(ws, b"\x83\x00" * (MB // 2), raw=True)
    ws.close()
    t = time.time()
    try:
        ws = WebSocket(w)
        served = ws.status == 101 and ws.ping()
        ws.close()
    except OSError:
        served = False
    check("after a client which breaks the protocol and goes on sending, "
          "the next one is served at once", served and time.time() - t < 10,
          "%.1f s" % (time.time() - t))
    ws = WebSocket(w)
    check("the port still serves", ws.status == 101 and ws.ping())
    ws.close()


def origins_cases(srv):
    w = srv.port + 1
    for origin, ok in (("https://mgubi.github.io", True),
                       ("HTTPS://MGUBI.GITHUB.IO", True),
                       ("https://mgubi.github.io:443", False),
                       ("https://example.org", False),
                       ("http://localhost:8080", False)):
        ws = WebSocket(w, origin)
        if ok:
            check("a configured origin (%s) is accepted" % origin,
                  ws.status == 101 and ws.ping(), str(ws.status))
        else:
            check("an origin not configured (%s) is refused" % origin,
                  ws.status != 101, str(ws.status))
        ws.close()
    ws = WebSocket(w, "https://mgubi.github.io")
    ok = send_big(ws, b"x" * (MB + 1))
    check("with a limit of 1 MB, a message above it closes the link",
          not ok or ws.closed())
    ws.close()
    ws = WebSocket(w, "https://mgubi.github.io")
    msg = b"(4 (websocket-check-ping))" + b" " * (MB // 2)
    ws.send(str(len(msg)).encode() + b"\n" + msg)
    op, data = ws.recv(20)
    check("with a limit of 1 MB, a message of 0.5 MB is served",
          op == 2 and b"websocket-check" in data)
    ws.close()


def any_origin_cases(srv):
    ws = WebSocket(srv.port + 1, "https://example.org")
    check("with '*', any origin is accepted",
          ws.status == 101 and ws.ping(), str(ws.status))
    ws.close()


def refused_port(wport, why, free=True):
    def cases(srv):
        log = srv.text()
        check("an error in the log (%s)" % why,
              re.search(r"cannot serve WebSocket clients.*%s" % re.escape(
                  str(wport)), log) is not None, "log: " + log[-400:])
        check("no WebSocket port is announced (%s)" % why,
              "waiting for WebSocket clients" not in log)
        check("the server still listens (%s)" % why, listening(srv.port))
        if free and isinstance(wport, int) and 0 < wport % 65536 != srv.port:
            check("nothing listens at %d (%s)" % (wport % 65536, why),
                  not listening(wport % 65536))
    return cases


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", default=os.path.join(TOP, "TeXmacs", "bin",
                                                  "texmacs.bin"))
    ap.add_argument("--home", default=os.path.join(TOP, "tests", "build",
                                                   "server", "home"))
    args = ap.parse_args()
    args.home = os.path.abspath(args.home)
    os.makedirs(args.home, exist_ok=True)

    p = free_port_pair()
    run(args, "default", base_prefs(p), default_cases)
    p = free_port_pair()
    run(args, "origins",
        base_prefs(p, origins="https://mgubi.github.io", limit="1"),
        origins_cases)
    p = free_port_pair()
    run(args, "any-origin", base_prefs(p, origins="*"), any_origin_cases)
    p = free_port_pair()
    run(args, "off", dict(base_prefs(p), **{"server websocket": "off"}),
        lambda srv: check("with 'off' nothing listens",
                          not listening(srv.port + 1)))
    p = free_port_pair()
    q = free_port_pair()
    w = 65536 + q  # as a 16 bit number it would be the free port q
    run(args, "above-65535", base_prefs(p, port=str(w)),
        refused_port(w, "above 65535"))
    p = free_port_pair()
    run(args, "server-port", base_prefs(p, port=str(p)),
        refused_port(p, "the port of the server"))
    p = free_port_pair()
    run(args, "not-a-number", base_prefs(p, port="next"),
        refused_port("next", "not a number"))
    p = free_port_pair()
    q = free_port_pair()
    blocker = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    blocker.bind(("127.0.0.1", q))
    blocker.listen(1)
    try:
        run(args, "in-use", base_prefs(p, port=str(q)),
            refused_port(q, "in use", free=False))
    finally:
        blocker.close()
    if port_free(65535) and port_free(65534):
        run(args, "server-at-65535", base_prefs(65535),
            refused_port(65536, "the server at 65535"))
    else:
        print("server-at-65535: skipped, the port is in use")

    print("%d checks, %d failed" % (count, len(failures)))
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
