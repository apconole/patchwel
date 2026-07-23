#!/usr/bin/env bash
# Drive patchwork-cron-sync.el once per configured server, all running
# concurrently rather than one process working through them serially --
# see the "Background sync via cron" section of README.org for why (the
# time a sync takes is dominated by serialized network round-trips, not
# by Emacs Lisp itself; the local sqlite db is already opened in WAL mode
# with a busy timeout specifically so several of these can write to it at
# once without "database is locked" errors).
#
# Usage, in a crontab, in place of a plain `emacs --batch -l
# patchwork-cron-sync.el' entry:
#
#   */5 * * * * /path/to/patchwel/patchwork-cron-sync-parallel.sh >> ~/.cache/patchwel/sync.log 2>&1
#
# The server list is never duplicated here -- it's read from
# patchwork-cron-sync.el itself (via --list-servers) every run, so
# editing patchwork-servers in that one file is all that's ever needed.
#
# Each server's sync is individually guarded by its own flock lock file,
# so a slow network making one tick's sync for a server still running
# when the next tick fires just skips that server that tick (logged),
# rather than running a second, overlapping process for it -- see
# "Overlapping runs" in README.org for why that matters even though the
# db itself is already safe against concurrent writers.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYNC_EL="$SCRIPT_DIR/patchwork-cron-sync.el"
EMACS="${EMACS:-emacs}"
LOCK_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/patchwel"

mkdir -p "$LOCK_DIR"

mapfile -t SERVER_URLS < <("$EMACS" -Q --batch -l "$SYNC_EL" -- --list-servers)

if [ "${#SERVER_URLS[@]}" -eq 0 ]; then
  echo "patchwork-cron-sync-parallel: no servers configured in $SYNC_EL" >&2
  exit 1
fi

sync_one() {
  local url="$1"
  local key
  key=$(printf '%s' "$url" | tr -c 'A-Za-z0-9' '_')
  local lock_fd
  exec {lock_fd}>"$LOCK_DIR/cron-sync-$key.lock"
  if ! flock -n "$lock_fd"; then
    echo "patchwork-cron-sync-parallel: sync for $url already running, skipped" >&2
    return 0
  fi
  "$EMACS" -Q --batch -l "$SYNC_EL" -- "$url"
}

pids=()
for url in "${SERVER_URLS[@]}"; do
  sync_one "$url" &
  pids+=("$!")
done

status=0
for pid in "${pids[@]}"; do
  wait "$pid" || status=1
done

exit "$status"
