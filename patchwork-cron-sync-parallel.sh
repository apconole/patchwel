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

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYNC_EL="$SCRIPT_DIR/patchwork-cron-sync.el"
EMACS="${EMACS:-emacs}"

mapfile -t SERVER_URLS < <("$EMACS" -Q --batch -l "$SYNC_EL" -- --list-servers)

if [ "${#SERVER_URLS[@]}" -eq 0 ]; then
  echo "patchwork-cron-sync-parallel: no servers configured in $SYNC_EL" >&2
  exit 1
fi

pids=()
for url in "${SERVER_URLS[@]}"; do
  "$EMACS" -Q --batch -l "$SYNC_EL" -- "$url" &
  pids+=("$!")
done

status=0
for pid in "${pids[@]}"; do
  wait "$pid" || status=1
done

exit "$status"
