#!/usr/bin/env bash
set -euo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_FILE="$1"
exec "${EMACS:-emacs}" -Q --batch \
  -L "$REPO_ROOT" -L "$REPO_ROOT/tests" \
  -l ert -l "$REPO_ROOT/tests/test-helpers.el" -l "$TEST_FILE" \
  -f ert-run-tests-batch-and-exit
