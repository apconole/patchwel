#!/usr/bin/env python3
"""Stdlib-only mock Patchwork API server for patchwel's test suite.

Serves a small, deterministic set of Patchwork-shaped JSON fixtures
(projects/series/patches/comments/checks/events/covers), a real mbox
per patch (produced via `git format-patch`, not hand-written), and an
Anubis-style HTML "challenge" page that can be toggled on for any
path. A `/_control/*` API lets tests mutate server behavior (patch
state, since= format gating, forced HTTP status/delay per path,
challenge toggling) and inspect exactly what was requested.

Usage: mock_patchwork_server.py [--port N]
Prints "PORT=<n>" (and nothing else) to stdout once listening, so a
launcher can read the actual bound port.
"""
import argparse
import copy
import json
import os
import re
import subprocess
import sys
import tempfile
import threading
import time
from datetime import datetime, timedelta, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlparse, parse_qs, quote


def recent(days_ago, hours_ago=0):
    """Return an ISO-8601 timestamp (naive, no timezone marker -- matching
    real Patchwork's own date fields) DAYS_AGO days (and HOURS_AGO hours)
    before now, so fixture data always falls inside any reasonable sync
    lookback window regardless of when the test suite actually runs."""
    dt = datetime.now(timezone.utc) - timedelta(days=days_ago, hours=hours_ago)
    return dt.strftime("%Y-%m-%dT%H:%M:%S")

SINCE_FORMATS = {
    "z": re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$"),
    "naive": re.compile(r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}$"),
    "date": re.compile(r"^\d{4}-\d{2}-\d{2}$"),
}

CHALLENGE_HTML = """<!doctype html>
<html><head><title>Making sure you're not a bot!</title></head>
<body><script>/* proof-of-work challenge would run here */</script>
<p>Checking your browser before accessing the site.</p></body></html>
"""


def build_mboxes(patch_ids):
    """Build a real RFC 2822 mbox (via `git format-patch`) per id in
    PATCH_IDS. Each patch adds its own new, distinct file
    (`patch-<id>.txt`) rather than building on the previous patch's
    change, so any subset/order of them applies cleanly to an
    unrelated repo with no inter-patch dependency -- exactly what
    `patchwork-apply-series'/`-as-commits' need to be able to apply
    several of these together regardless of what else is in the
    target tree."""
    mboxes = {}
    with tempfile.TemporaryDirectory() as d:
        def git(*args):
            subprocess.run(["git", "-C", d, *args], check=True,
                           capture_output=True, text=True)

        git("init", "-q")
        git("config", "user.email", "test@example.com")
        git("config", "user.name", "Test User")
        with open(os.path.join(d, "base.txt"), "w") as f:
            f.write("base\n")
        git("add", "base.txt")
        git("commit", "-q", "-m", "initial commit")
        base_rev = subprocess.run(
            ["git", "-C", d, "rev-parse", "HEAD"],
            check=True, capture_output=True, text=True).stdout.strip()
        for pid in patch_ids:
            git("checkout", "-q", base_rev)
            fname = f"patch-{pid}.txt"
            with open(os.path.join(d, fname), "w") as f:
                f.write(f"content for patch {pid}\n")
            git("add", fname)
            git("commit", "-q", "-m", f"patch {pid}: add {fname}")
            out = subprocess.run(
                ["git", "-C", d, "format-patch", "-1", "--stdout", "HEAD"],
                check=True, capture_output=True, text=True).stdout
            mboxes[pid] = out
    return mboxes


def make_fixtures():
    project = {"id": 1, "url": "/api/projects/1/", "name": "Test Project",
               "link_name": "testproj"}
    project2 = {"id": 2, "url": "/api/projects/2/", "name": "Other Project",
                "link_name": "otherproj"}

    def person(name, email):
        return {"id": abs(hash(email)) % 100000, "name": name, "email": email}

    alice = person("Alice Dev", "alice@example.com")
    bob = person("Bob Reviewer", "bob@example.com")

    def patch(pid, series_id, name, state, position, check="pending",
              delegate=None, project_obj=None):
        return {
            "id": pid,
            "url": f"/api/patches/{pid}/",
            "web_url": f"/patch/{pid}/",
            "project": project_obj or project,
            "msgid": f"<patch-{pid}@example.com>",
            "date": recent(6 - position),
            "name": name,
            "state": state,
            "archived": False,
            "submitter": alice,
            "delegate": delegate,
            "mbox": f"/mbox/{pid}",
            "series": [{"id": series_id}],
            "check": check,
            "content": f"Commit message body for {name}.\n\nSigned-off-by: Alice Dev <alice@example.com>\n",
            "diff": "--- a/file.txt\n+++ b/file.txt\n@@ -1 +1,2 @@\n base\n+new line\n",
            "headers": {
                "Message-ID": f"<patch-{pid}@example.com>",
                "From": "Alice Dev <alice@example.com>",
                "Subject": name,
                "To": "list@example.com",
                "Cc": "Bob Reviewer <bob@example.com>",
                "References": f"<cover-{series_id}@example.com>",
                "In-Reply-To": f"<cover-{series_id}@example.com>",
            },
        }

    patches = {
        2001: patch(2001, 1001, "[PATCH] one patch series", "new", 1),
        2002: patch(2002, 1002, "[PATCH 1/2] first of two", "new", 1),
        2003: patch(2003, 1002, "[PATCH 2/2] second of two", "under-review", 2),
        2004: patch(2004, 1003, "[PATCH] an untouched-by-events series", "new", 1),
        2005: patch(2005, 1004, "[PATCH] a series in the other project", "new", 1,
                    project_obj=project2),
    }

    series = {
        1001: {
            "id": 1001, "url": "/api/series/1001/", "web_url": "/series/1001/",
            "project": project, "name": "A single-patch series",
            "date": recent(5), "submitter": alice,
            "version": 1, "total": 1,
            "mbox": "/mbox-series/1001",
        },
        1002: {
            "id": 1002, "url": "/api/series/1002/", "web_url": "/series/1002/",
            "project": project, "name": "A two-patch series",
            "date": recent(4), "submitter": alice,
            "version": 1, "total": 2,
            "mbox": "/mbox-series/1002",
        },
        1003: {
            "id": 1003, "url": "/api/series/1003/", "web_url": "/series/1003/",
            "project": project, "name": "An untouched-by-events series",
            "date": recent(6), "submitter": alice,
            "version": 1, "total": 1,
            "mbox": "/mbox-series/1003",
        },
        1004: {
            "id": 1004, "url": "/api/series/1004/", "web_url": "/series/1004/",
            "project": project2, "name": "A series in the other project",
            "date": recent(3), "submitter": alice,
            "version": 1, "total": 1,
            "mbox": "/mbox-series/1004",
        },
    }

    comments = {
        2002: [{
            "id": 3001, "web_url": "/comment/3001/",
            "msgid": "<comment-3001@example.com>",
            "date": recent(2),
            "subject": "Re: [PATCH 1/2] first of two",
            "submitter": bob,
            "content": "> some quoted line\nReviewed-by: Bob Reviewer <bob@example.com>\n",
            "headers": {
                "Message-ID": "<comment-3001@example.com>",
                "From": "Bob Reviewer <bob@example.com>",
                "To": "list@example.com",
                "Cc": "Alice Dev <alice@example.com>",
                "References": "<patch-2002@example.com>",
                "In-Reply-To": "<patch-2002@example.com>",
            },
        }],
    }

    checks = {
        2003: [
            {"id": 4001, "date": recent(1), "state": "success",
             "context": "ci/build", "description": "build passed",
             "target_url": "https://ci.example.com/4001", "user": bob},
            {"id": 4002, "date": recent(1, 1), "state": "warning",
             "context": "ci/style", "description": "style nit",
             "target_url": "https://ci.example.com/4002", "user": bob},
        ],
    }

    events = [
        {"id": 5001, "category": "series-created", "project": project,
         "date": recent(4),
         "payload": {"series": {"id": 1002}}},
        {"id": 5002, "category": "patch-created", "project": project,
         "date": recent(5),
         "payload": {"patch": {"id": 2001}}},
        {"id": 5003, "category": "cover-created", "project": project,
         "date": recent(5, 1),
         "payload": {"cover": {"id": 6001}}},
        {"id": 5004, "category": "series-created", "project": project2,
         "date": recent(3),
         "payload": {"series": {"id": 1004}}},
    ]

    covers = {
        6001: {"id": 6001, "series": [{"id": 1001}]},
    }

    return {
        "projects": {1: project, 2: project2},
        "series": series,
        "patches": patches,
        "comments": comments,
        "checks": checks,
        "events": events,
        "covers": covers,
    }


class State:
    def __init__(self):
        self.lock = threading.Lock()
        self.mboxes = build_mboxes(sorted(make_fixtures()["patches"].keys()))
        self.reset()

    def reset(self):
        with self.lock:
            self.data = make_fixtures()
            self.since_mode = "z"
            self.challenge_paths = set()
            self.status_overrides = {}
            self.delay_overrides = {}
            self.required_token = None
            self.reject_events_with_project = False
            self.request_log = []


STATE = State()


def matching_override(overrides, path):
    for prefix, value in overrides.items():
        if prefix == "*" or path.startswith(prefix):
            return value
    return None


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt, *args):
        pass  # keep test output quiet; use /_control/log instead

    def _send_json(self, status, obj, extra_headers=None):
        body = json.dumps(obj).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        for k, v in (extra_headers or {}).items():
            self.send_header(k, v)
        self.end_headers()
        self.wfile.write(body)

    def _send_text(self, status, text, content_type="text/plain"):
        body = text.encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _log_request(self, method, path, qs):
        with STATE.lock:
            STATE.request_log.append({
                "method": method,
                "path": path,
                "query": {k: v[0] for k, v in qs.items()},
                "headers": dict(self.headers.items()),
            })

    def _base_url(self, path):
        host = self.headers.get("Host", "127.0.0.1")
        return f"http://{host}{path}"

    def _paginate(self, items, qs, path):
        per_page = int(qs.get("per_page", ["100"])[0])
        page = int(qs.get("page", ["1"])[0])
        start = (page - 1) * per_page
        end = start + per_page
        page_items = items[start:end]
        headers = {}
        if end < len(items):
            next_qs = dict(qs)
            next_qs["page"] = [str(page + 1)]
            query = "&".join(f"{k}={quote(v[0])}" for k, v in next_qs.items())
            headers["Link"] = f'<{self._base_url(path)}?{query}>; rel="next"'
        return page_items, headers

    def _check_since(self, qs, path):
        """Return an HTTP status to force, or None if the request should
        proceed normally."""
        if path.startswith("/api/events/") and STATE.since_mode == "no-events-api":
            return 404
        if "since" not in qs:
            return None
        if STATE.since_mode in ("no-events-api", "accept-all"):
            return None
        if STATE.since_mode == "reject-all":
            return 400
        pattern = SINCE_FORMATS.get(STATE.since_mode)
        if pattern and not pattern.match(qs["since"][0]):
            return 400
        return None

    def _pre_checks(self, method, path, qs):
        """Apply challenge/status-override/delay/since gating, common to
        every /api/ and /mbox/ request. Returns True if this method
        already fully handled (and sent) the response."""
        self._log_request(method, path, qs)

        if STATE.required_token is not None:
            auth = self.headers.get("Authorization", "")
            if auth != f"Token {STATE.required_token}":
                self._send_json(401, {"detail": "Invalid token"})
                return True

        if matching_override(STATE.challenge_paths_dict(), path):
            self._send_text(200, CHALLENGE_HTML, "text/html")
            return True

        forced_status = matching_override(STATE.status_overrides, path)
        if forced_status is not None:
            self._send_json(forced_status, {"detail": "forced status"})
            return True

        if (STATE.reject_events_with_project and path.startswith("/api/events/")
                and "project" in qs):
            self._send_json(502, {"detail": "simulated project= 502"})
            return True

        delay = matching_override(STATE.delay_overrides, path)
        if delay:
            time.sleep(delay)

        since_status = self._check_since(qs, path)
        if since_status is not None:
            self._send_json(since_status, {"detail": "bad since= format"})
            return True

        return False

    # -- routing --------------------------------------------------------

    def do_GET(self):
        parsed = urlparse(self.path)
        path = parsed.path
        qs = parse_qs(parsed.query)

        if path == "/health":
            self._send_text(200, "ok")
            return

        if path == "/_control/log":
            with STATE.lock:
                self._send_json(200, STATE.request_log)
            return

        if path.startswith("/mbox/"):
            if self._pre_checks("GET", path, qs):
                return
            pid = int(path.rsplit("/", 1)[-1])
            with STATE.lock:
                mbox = STATE.mboxes.get(pid)
            if mbox is None:
                self._send_json(404, {"detail": "no such patch"})
            else:
                self._send_text(200, mbox)
            return

        if not path.startswith("/api/"):
            self._send_json(404, {"detail": "not found"})
            return

        if self._pre_checks("GET", path, qs):
            return

        with STATE.lock:
            data = STATE.data
            if path == "/api/projects/":
                items, headers = self._paginate(list(data["projects"].values()), qs, path)
                self._send_json(200, items, headers)
            elif path == "/api/series/":
                items = list(data["series"].values())
                if "project" in qs:
                    items = [s for s in items if s["project"]["link_name"] == qs["project"][0]]
                items, headers = self._paginate(items, qs, path)
                self._send_json(200, items, headers)
            elif re.match(r"^/api/series/(\d+)/$", path):
                sid = int(re.match(r"^/api/series/(\d+)/$", path).group(1))
                if sid in data["series"]:
                    self._send_json(200, data["series"][sid])
                else:
                    self._send_json(404, {"detail": "not found"})
            elif path == "/api/patches/":
                items = list(data["patches"].values())
                if "series" in qs:
                    sid = int(qs["series"][0])
                    items = [p for p in items if any(s["id"] == sid for s in p["series"])]
                if "project" in qs:
                    items = [p for p in items if p["project"]["link_name"] == qs["project"][0]]
                items, headers = self._paginate(items, qs, path)
                self._send_json(200, items, headers)
            elif re.match(r"^/api/patches/(\d+)/$", path):
                pid = int(re.match(r"^/api/patches/(\d+)/$", path).group(1))
                if pid in data["patches"]:
                    patch_obj = dict(data["patches"][pid])
                    patch_obj["mbox"] = self._base_url(patch_obj["mbox"])
                    self._send_json(200, patch_obj)
                else:
                    self._send_json(404, {"detail": "not found"})
            elif re.match(r"^/api/patches/(\d+)/comments/$", path):
                pid = int(re.match(r"^/api/patches/(\d+)/comments/$", path).group(1))
                items, headers = self._paginate(data["comments"].get(pid, []), qs, path)
                self._send_json(200, items, headers)
            elif re.match(r"^/api/patches/(\d+)/checks/$", path):
                pid = int(re.match(r"^/api/patches/(\d+)/checks/$", path).group(1))
                items, headers = self._paginate(data["checks"].get(pid, []), qs, path)
                self._send_json(200, items, headers)
            elif re.match(r"^/api/covers/(\d+)/$", path):
                cid = int(re.match(r"^/api/covers/(\d+)/$", path).group(1))
                if cid in data["covers"]:
                    self._send_json(200, data["covers"][cid])
                else:
                    self._send_json(404, {"detail": "not found"})
            elif path == "/api/events/":
                items = data["events"]
                if "project" in qs:
                    pass  # every fixture event belongs to the one test project
                items, headers = self._paginate(items, qs, path)
                self._send_json(200, items, headers)
            else:
                self._send_json(404, {"detail": "not found"})

    def do_PATCH(self):
        parsed = urlparse(self.path)
        path = parsed.path
        qs = parse_qs(parsed.query)
        if self._pre_checks("PATCH", path, qs):
            return
        m = re.match(r"^/api/patches/(\d+)/$", path)
        if not m:
            self._send_json(404, {"detail": "not found"})
            return
        pid = int(m.group(1))
        length = int(self.headers.get("Content-Length", 0))
        body = json.loads(self.rfile.read(length) or b"{}")
        with STATE.lock:
            patch = STATE.data["patches"].get(pid)
            if not patch:
                self._send_json(404, {"detail": "not found"})
                return
            patch.update(body)
            self._send_json(200, patch)

    def do_POST(self):
        parsed = urlparse(self.path)
        path = parsed.path
        qs = parse_qs(parsed.query)
        if not path.startswith("/_control/"):
            if self._pre_checks("POST", path, qs):
                return
            self._send_json(404, {"detail": "not found"})
            return

        length = int(self.headers.get("Content-Length", 0))
        raw = self.rfile.read(length) if length else b"{}"
        try:
            body = json.loads(raw or b"{}")
        except ValueError:
            body = {}

        if path == "/_control/reset":
            STATE.reset()
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-patch-state":
            with STATE.lock:
                p = STATE.data["patches"].get(int(body["patch_id"]))
                if p:
                    p["state"] = body["state"]
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-since-mode":
            with STATE.lock:
                STATE.since_mode = body["mode"]
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-challenge":
            with STATE.lock:
                if body.get("on"):
                    STATE.challenge_paths.add(body["path"])
                else:
                    STATE.challenge_paths.discard(body["path"])
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-status":
            with STATE.lock:
                if body.get("status") is None:
                    STATE.status_overrides.pop(body["path"], None)
                else:
                    STATE.status_overrides[body["path"]] = body["status"]
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-delay":
            with STATE.lock:
                if not body.get("seconds"):
                    STATE.delay_overrides.pop(body["path"], None)
                else:
                    STATE.delay_overrides[body["path"]] = body["seconds"]
            self._send_json(200, {"ok": True})
        elif path == "/_control/require-token":
            with STATE.lock:
                STATE.required_token = body.get("token")
            self._send_json(200, {"ok": True})
        elif path == "/_control/set-reject-events-with-project":
            with STATE.lock:
                STATE.reject_events_with_project = bool(body.get("on"))
            self._send_json(200, {"ok": True})
        elif path == "/_control/shutdown":
            self._send_json(200, {"ok": True})
            threading.Thread(target=self.server.shutdown).start()
        else:
            self._send_json(404, {"detail": "not found"})


def _challenge_paths_dict(self):
    return {p: True for p in self.challenge_paths}


State.challenge_paths_dict = _challenge_paths_dict


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", type=int, default=0)
    args = parser.parse_args()

    server = ThreadingHTTPServer(("127.0.0.1", args.port), Handler)
    print(f"PORT={server.server_address[1]}", flush=True)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    main()
