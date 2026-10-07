#!/usr/bin/env python3
"""
Lightweight real-time Web UI backend for ghci-session live monitoring & NES Game Player.
Serves static assets, live monitoring SSE/REST APIs, and high-performance WebSocket NES streaming.
"""
import base64
import glob
import hashlib
import http.server
import json
import os
import struct
import subprocess
import sys
import threading
import time
from urllib.parse import parse_qs, urlparse

PORT = 8080
ROOT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
WEB_DIR = os.path.join(ROOT_DIR, "web")
NES_DIR = os.path.join(ROOT_DIR, "nes")
BIN_DIR = os.path.join(ROOT_DIR, ".bin")
NES_RUNNER_BIN = os.path.join(BIN_DIR, "nes-runner")
SESSION_DIR = os.path.join(NES_DIR, ".ghci-session", "nes")

# -----------------------------------------------------------------------------
# WebSocket RFC 6455 Framing Helpers
# -----------------------------------------------------------------------------
def make_ws_frame(payload: bytes, binary: bool = True) -> bytes:
    opcode = 0x82 if binary else 0x81
    length = len(payload)
    if length < 126:
        header = bytes([opcode, length])
    elif length <= 65535:
        header = bytes([opcode, 126]) + struct.pack('>H', length)
    else:
        header = bytes([opcode, 127]) + struct.pack('>Q', length)
    return header + payload

def parse_ws_frame(raw: bytearray):
    if len(raw) < 2:
        return None, 0
    b0 = raw[0]
    b1 = raw[1]
    opcode = b0 & 0x0F
    is_masked = bool(b1 & 0x80)
    length = b1 & 0x7F
    offset = 2
    if length == 126:
        if len(raw) < 4:
            return None, 0
        length = struct.unpack('>H', raw[2:4])[0]
        offset = 4
    elif length == 127:
        if len(raw) < 10:
            return None, 0
        length = struct.unpack('>Q', raw[2:10])[0]
        offset = 10
    mask_key = None
    if is_masked:
        if len(raw) < offset + 4:
            return None, 0
        mask_key = raw[offset:offset+4]
        offset += 4
    if len(raw) < offset + length:
        return None, 0
    payload = raw[offset:offset+length]
    if is_masked:
        payload = bytes([b ^ mask_key[i % 4] for i, b in enumerate(payload)])
    return (opcode, bytes(payload)), offset + length

# -----------------------------------------------------------------------------
# Haskell NES Runner Process Manager
# -----------------------------------------------------------------------------
class NesRunner:
    def __init__(self, bin_path):
        self.bin_path = bin_path
        self.lock = threading.Lock()
        self.proc = None
        self.current_rom_info = {"mapper": "NROM", "name": "Built-in Sprite Demo", "mapperNum": 0}
        self.start()

    def start(self):
        with self.lock:
            if self.proc:
                try:
                    self.proc.terminate()
                except Exception:
                    pass
            if os.path.exists(self.bin_path):
                self.proc = subprocess.Popen(
                    [self.bin_path],
                    stdin=subprocess.PIPE,
                    stdout=subprocess.PIPE,
                    bufsize=0
                )
            else:
                self.proc = None

    def step(self, pad=0):
        with self.lock:
            if not self.proc or self.proc.poll() is not None:
                self.start()
            if not self.proc:
                return None, None
            try:
                self.proc.stdin.write(bytes([ord('F'), pad & 0xFF]))
                self.proc.stdin.flush()
                magic = self.proc.stdout.read(4)
                if magic != b'NES1':
                    return None, None
                vlen = struct.unpack('>I', self.proc.stdout.read(4))[0]
                alen = struct.unpack('>I', self.proc.stdout.read(4))[0]
                video = self.proc.stdout.read(vlen)
                audio = self.proc.stdout.read(alen)
                return video, audio
            except Exception:
                self.start()
                return None, None

    def reset(self):
        with self.lock:
            if not self.proc or self.proc.poll() is not None:
                self.start()
                return True
            try:
                self.proc.stdin.write(b'R')
                self.proc.stdin.flush()
                resp = self.proc.stdout.read(2)
                return resp == b'OK'
            except Exception:
                self.start()
                return False

    def load_demo(self):
        with self.lock:
            if not self.proc or self.proc.poll() is not None:
                self.start()
            try:
                self.proc.stdin.write(b'D')
                self.proc.stdin.flush()
                resp = self.proc.stdout.read(2)
                self.current_rom_info = {"mapper": "NROM", "name": "Built-in Sprite Demo", "mapperNum": 0}
                return resp == b'OK'
            except Exception:
                self.start()
                return False

    def load_rom(self, rom_bytes, filename="Uploaded ROM"):
        with self.lock:
            if not self.proc or self.proc.poll() is not None:
                self.start()
            try:
                header = b'L' + struct.pack('>I', len(rom_bytes))
                self.proc.stdin.write(header + rom_bytes)
                self.proc.stdin.flush()
                resp = self.proc.stdout.read(2)
                if resp == b'OK':
                    self.proc.stdin.write(b'I')
                    self.proc.stdin.flush()
                    iresp = self.proc.stdout.read(2)
                    if iresp == b'IN':
                        ilen = struct.unpack('>H', self.proc.stdout.read(2))[0]
                        info_str = self.proc.stdout.read(ilen).decode('utf-8', errors='ignore')
                        try:
                            info = json.loads(info_str)
                            info["name"] = filename
                            self.current_rom_info = info
                        except Exception:
                            self.current_rom_info = {"name": filename}
                    return True, "Loaded successfully"
                else:
                    ilen = struct.unpack('>H', self.proc.stdout.read(2))[0]
                    err = self.proc.stdout.read(ilen).decode('utf-8', errors='ignore')
                    return False, err
            except Exception as e:
                self.start()
                return False, str(e)

nes_runner = NesRunner(NES_RUNNER_BIN)

# -----------------------------------------------------------------------------
# GhciSession Monitoring Helpers
# -----------------------------------------------------------------------------
def get_latest_task_log():
    pattern = "/Users/wesleymassuda/.gemini/antigravity-ide/brain/*/.system_generated/tasks/task-*.log"
    all_logs = glob.glob(pattern)
    chat_logs = []
    for l in all_logs:
        try:
            with open(l, "r", errors="ignore") as f:
                sample = f.read(4096)
                if "server.py" in sample or "HTTP/1.1" in sample:
                    continue
                if any(w in sample for w in ["chat", "nes", "gba", "Gba", "model", "round", "CHECK-PASS", "cabal"]):
                    chat_logs.append(l)
        except Exception:
            pass
    if chat_logs:
        chat_logs.sort(key=lambda p: os.path.getmtime(p), reverse=True)
        return chat_logs[0]
    return None

GBA_DIR = os.path.join(ROOT_DIR, "gba")
GBA_SESSION_DIR = os.path.join(GBA_DIR, ".ghci-session", "gba")

def get_active_session_info(target=None):
    if target == "gba":
        return "gba", GBA_SESSION_DIR, GBA_DIR
    elif target == "nes":
        return "nes", SESSION_DIR, NES_DIR
    gba_status = os.path.join(GBA_SESSION_DIR, "status.json")
    nes_status = os.path.join(SESSION_DIR, "status.json")
    if os.path.exists(gba_status):
        if not os.path.exists(nes_status) or os.path.getmtime(gba_status) >= os.path.getmtime(nes_status):
            return "gba", GBA_SESSION_DIR, GBA_DIR
    return "nes", SESSION_DIR, NES_DIR

def get_session_status(target=None):
    sname, sdir, _ = get_active_session_info(target)
    status_file = os.path.join(sdir, "status.json")
    if os.path.exists(status_file):
        try:
            with open(status_file, "r") as f:
                data = json.load(f)
                data["active_target"] = sname
                return data
        except Exception:
            pass
    return {"status": {"kind": "UNKNOWN", "verdict": "checking...", "ok": False}, "active_target": sname}

def get_history_tail(limit=80, target=None):
    _, sdir, _ = get_active_session_info(target)
    main_dir = os.path.join(sdir, "history", "main")
    if not os.path.exists(main_dir):
        return []
    day_files = sorted(glob.glob(os.path.join(main_dir, "*.jsonl")))
    messages = []
    for df in day_files:
        try:
            with open(df, "r", encoding="utf-8", errors="replace") as f:
                for line in f:
                    line = line.strip()
                    if line:
                        try:
                            messages.append(json.loads(line))
                        except Exception:
                            pass
        except Exception:
            pass
    return messages[-limit:]

def get_git_diff(target=None):
    sname, _, pdir = get_active_session_info(target)
    try:
        proc_stat = subprocess.run(["git", "-C", sname, "status", "--short"], cwd=ROOT_DIR, capture_output=True, text=True, timeout=2)
        proc_diff = subprocess.run(["git", "-C", sname, "diff", "--stat"], cwd=ROOT_DIR, capture_output=True, text=True, timeout=2)
        return {
            "session": sname,
            "status": proc_stat.stdout.strip(),
            "diff_stat": proc_diff.stdout.strip()
        }
    except Exception as e:
        return {"session": sname, "status": "", "diff_stat": str(e)}

def get_vfs_data(target=None):
    sname, _, pdir = get_active_session_info(target)
    bin_path = os.path.join(ROOT_DIR, ".bin", "ghci-session")
    try:
        proc = subprocess.run([bin_path, "--root", sname, "vfs", "--json"], cwd=ROOT_DIR, capture_output=True, text=True, timeout=3)
        if proc.returncode == 0:
            data = json.loads(proc.stdout)
            data["session"] = sname
            return data
    except Exception as e:
        return {"error": str(e), "files": [], "session": sname}
    return {"files": [], "count": 0, "total_lines": 0, "over_budget": 0, "session": sname}

# -----------------------------------------------------------------------------
# HTTP & WebSocket Handler
# -----------------------------------------------------------------------------
class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=WEB_DIR, **kwargs)

    def address_string(self):
        # Avoid slow reverse-DNS lookups on local loopback
        return self.client_address[0]

    def log_message(self, format, *args):
        # Mute repetitive status / frame polling logs
        if len(args) > 0 and any(p in str(args[0]) for p in ["/api/status", "/api/history", "/api/nes/frame", "/ws/nes", "/api/vfs"]):
            return
        super().log_message(format, *args)

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path

        # Check WebSocket upgrade
        if self.headers.get("Upgrade", "").lower() == "websocket" or path == "/ws/nes":
            self.handle_websocket()
            return

        qs = parse_qs(parsed.query)
        target = qs.get("session", [None])[0]

        if path == "/api/status":
            self.send_json(self.build_status_data(target))
        elif path == "/api/log":
            self.send_json(self.build_log_data(target))
        elif path == "/api/history":
            self.send_json({"messages": get_history_tail(80, target)})
        elif path == "/api/diff":
            self.send_json(get_git_diff(target))
        elif path == "/api/vfs":
            self.send_json(get_vfs_data(target))
        elif path == "/api/stream":
            self.handle_sse(target)
        elif path == "/api/nes/info":
            self.send_json(nes_runner.current_rom_info)
        elif path == "/api/nes/frame":
            # HTTP fallback frame step
            qs = parse_qs(parsed.query)
            pad = int(qs.get("pad", ["0"])[0])
            video, audio = nes_runner.step(pad)
            if video and audio:
                payload = struct.pack('>I', len(audio)) + video + audio
                self.send_response(200)
                self.send_header("Content-Type", "application/octet-stream")
                self.send_header("Content-Length", str(len(payload)))
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(payload)
            else:
                self.send_error(500, "Runner error")
        else:
            super().do_GET()

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path

        if path == "/api/nes/reset":
            ok = nes_runner.reset()
            self.send_json({"ok": ok})
        elif path == "/api/nes/demo":
            ok = nes_runner.load_demo()
            self.send_json({"ok": ok, "info": nes_runner.current_rom_info})
        elif path == "/api/nes/step":
            content_len = int(self.headers.get("Content-Length", 0))
            pad = 0
            if content_len > 0:
                body = self.rfile.read(content_len)
                try:
                    data = json.loads(body.decode("utf-8"))
                    pad = int(data.get("pad", 0))
                except Exception:
                    pass
            video, audio = nes_runner.step(pad)
            if video and audio:
                payload = struct.pack('>I', len(audio)) + video + audio
                self.send_response(200)
                self.send_header("Content-Type", "application/octet-stream")
                self.send_header("Content-Length", str(len(payload)))
                self.send_header("Access-Control-Allow-Origin", "*")
                self.end_headers()
                self.wfile.write(payload)
            else:
                self.send_error(500, "Runner error")
        elif path == "/api/nes/rom":
            content_len = int(self.headers.get("Content-Length", 0))
            if content_len > 0:
                rom_bytes = self.rfile.read(content_len)
                filename = self.headers.get("X-Filename", "custom_game.nes")
                ok, msg = nes_runner.load_rom(rom_bytes, filename)
                self.send_json({"ok": ok, "message": msg, "info": nes_runner.current_rom_info})
            else:
                self.send_error(400, "Empty ROM body")
        else:
            self.send_error(404, "Not found")

    def send_json(self, data):
        body = json.dumps(data).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(body)

    def build_status_data(self, target=None):
        task_log = get_latest_task_log()
        task_running = False
        task_size = 0
        if task_log and os.path.exists(task_log):
            task_size = os.path.getsize(task_log)
            task_running = (time.time() - os.path.getmtime(task_log)) < 45

        status = get_session_status(target)
        git_info = get_git_diff(target)
        return {
            "time": time.time(),
            "target": target or status.get("active_target", "gba"),
            "task_log": os.path.basename(task_log) if task_log else None,
            "task_running": task_running,
            "task_size": task_size,
            "status": status,
            "git": git_info
        }

    def build_log_data(self, target=None):
        task_log = get_latest_task_log()
        lines = []
        if task_log and os.path.exists(task_log):
            try:
                with open(task_log, "r", encoding="utf-8", errors="replace") as f:
                    lines = f.readlines()
            except Exception:
                pass
        return {
            "task_log": os.path.basename(task_log) if task_log else None,
            "total_lines": len(lines),
            "tail": [l.rstrip("\r\n") for l in lines[-200:]]
        }

    def handle_sse(self, target=None):
        self.send_response(200)
        self.send_header("Content-Type", "text/event-stream")
        self.send_header("Cache-Control", "no-cache")
        self.send_header("Connection", "keep-alive")
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()

        task_log = get_latest_task_log()
        last_pos = 0
        if task_log and os.path.exists(task_log):
            last_pos = max(0, os.path.getsize(task_log) - 25000)

        tick = 0
        while True:
            try:
                new_text = ""
                task_log = get_latest_task_log()
                if task_log and os.path.exists(task_log):
                    size = os.path.getsize(task_log)
                    if size < last_pos:
                        last_pos = 0
                    if size > last_pos:
                        with open(task_log, "r", encoding="utf-8", errors="replace") as f:
                            f.seek(last_pos)
                            new_text = f.read()
                            last_pos = f.tell()

                if new_text:
                    payload = json.dumps({
                        "type": "log_chunk",
                        "chunk": new_text,
                        "task": os.path.basename(task_log) if task_log else ""
                    })
                    self.wfile.write(f"data: {payload}\n\n".encode("utf-8"))
                    self.wfile.flush()

                tick += 1
                if tick % 3 == 0:
                    status_payload = json.dumps({
                        "type": "status_update",
                        "status": self.build_status_data(target)
                    })
                    self.wfile.write(f"data: {status_payload}\n\n".encode("utf-8"))
                    self.wfile.flush()

                time.sleep(0.3)
            except (BrokenPipeError, ConnectionResetError):
                break
            except Exception:
                time.sleep(0.5)

    def handle_websocket(self):
        key = self.headers.get("Sec-WebSocket-Key")
        if not key:
            self.send_error(400, "Missing Sec-WebSocket-Key")
            return
        GUID = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
        accept = base64.b64encode(hashlib.sha1((key + GUID).encode()).digest()).decode()
        self.send_response(101, "Switching Protocols")
        self.send_header("Upgrade", "websocket")
        self.send_header("Connection", "Upgrade")
        self.send_header("Sec-WebSocket-Accept", accept)
        self.end_headers()

        sock = self.request
        sock.setblocking(False)
        pad = 0
        running = True
        paused = False
        in_buf = bytearray()
        frame_interval = 1.0 / 60.0

        while running:
            loop_start = time.time()
            try:
                chunk = sock.recv(4096)
                if chunk:
                    in_buf.extend(chunk)
                    while True:
                        frame, consumed = parse_ws_frame(in_buf)
                        if not frame:
                            break
                        in_buf = in_buf[consumed:]
                        opcode, payload = frame
                        if opcode == 0x08:
                            running = False
                            break
                        elif opcode == 0x09:
                            sock.sendall(bytes([0x8A, 0]))
                        elif opcode == 0x02:
                            if len(payload) >= 1:
                                pad = payload[0]
                        elif opcode == 0x01:
                            try:
                                msg = json.loads(payload.decode('utf-8'))
                                t = msg.get("type")
                                if t == "pad":
                                    pad = msg.get("buttons", 0)
                                elif t == "reset":
                                    nes_runner.reset()
                                elif t == "demo":
                                    nes_runner.load_demo()
                                elif t == "pause":
                                    paused = not paused
                            except Exception:
                                pass
                elif chunk == b'':
                    break
            except BlockingIOError:
                pass
            except Exception:
                break

            if not running:
                break

            if not paused:
                video, audio = nes_runner.step(pad)
                if video and audio:
                    packet = struct.pack('>I', len(audio)) + video + audio
                    ws_frame = make_ws_frame(packet, binary=True)
                    try:
                        sock.sendall(ws_frame)
                    except Exception:
                        break

            elapsed = time.time() - loop_start
            sleep_time = frame_interval - elapsed
            if sleep_time > 0.001:
                time.sleep(sleep_time)

def run():
    server = http.server.ThreadingHTTPServer(("0.0.0.0", PORT), Handler)
    print(f"===========================================================")
    print(f"  GhciSession NES Live Arcade & Monitor running at:")
    print(f"  -> http://localhost:{PORT}")
    print(f"===========================================================")
    server.serve_forever()

if __name__ == "__main__":
    run()
