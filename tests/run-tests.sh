#!/bin/bash
# ============================================================================
# Tiqsi Emacs Test Runner
# ============================================================================
#
# Usage:
#   ./tests/run-tests.sh                        Run all test suites
#   ./tests/run-tests.sh tests/test-foo.el      Run a specific test file
#   ./tests/run-tests.sh --eval '(+ 1 2)'       Eval simple elisp against live config
#   ./tests/run-tests.sh --eval-file foo.el      Eval a file of elisp
#   ./tests/run-tests.sh --smoke                 Quick init smoke test
#   ./tests/run-tests.sh --verbose               Show full init loading output
#
# Exit codes:
#   0  All tests passed / eval succeeded
#   1  One or more tests failed / eval errored
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_DIR="$(dirname "$SCRIPT_DIR")"
HARNESS="$SCRIPT_DIR/tiqsi-test-harness.el"
INIT="$REPO_DIR/init-lite.el"

VERBOSE=0
MODE="all"
TARGET=""
EVAL_EXPR=""

# Parse args
while [[ $# -gt 0 ]]; do
  case "$1" in
    --verbose|-v)
      VERBOSE=1
      shift
      ;;
    --smoke)
      MODE="smoke"
      shift
      ;;
    --eval)
      MODE="eval"
      EVAL_EXPR="$2"
      shift 2
      ;;
    --eval-file)
      MODE="eval-file"
      TARGET="$2"
      shift 2
      ;;
    *)
      MODE="file"
      TARGET="$1"
      shift
      ;;
  esac
done

# Filter: suppress hydra/init noise unless --verbose
filter_output() {
  if [[ $VERBOSE -eq 1 ]]; then
    cat
  else
    grep -v -E '^(Loading |Building |Cloning |Configuring |Warning \(emacs\)|Warning \[flymake|modules/|Checking for library|init-lite\.el:|Successfully loaded|OSX laptop|Cleaning up|Disabling|Claude REPL fixes|defhydra macro|After defhydra|M-c is bound|Raw output:|Error in process filter|Company backend.*could not|w32-send-sys|t:\[.*FAILURE.*argl|Tiqsi Claude REPL integration|Warning: The OPENAI|Symbol.s function definition)' || true
  fi
}

case "$MODE" in
  smoke)
    emacs -q -l "$INIT" -l "$HARNESS" \
      --eval '(tiqsi-test-smoke)' \
      --batch 2>&1 | filter_output
    ;;

  eval)
    # Write expr to temp file to avoid shell quoting issues
    TMPFILE="$(mktemp /tmp/tiqsi-eval-XXXXXX.el)"
    trap 'rm -f "$TMPFILE"' EXIT
    printf '(tiqsi-test-eval-and-exit "%s")' \
      "$(printf '%s' "$EVAL_EXPR" | sed 's/\\/\\\\/g; s/"/\\"/g')" \
      > "$TMPFILE"
    emacs -q -l "$INIT" -l "$HARNESS" -l "$TMPFILE" \
      --batch 2>&1 | filter_output
    ;;

  eval-file)
    # Load and evaluate a file of elisp against the live config
    if [[ ! "$TARGET" = /* ]]; then
      TARGET="$REPO_DIR/$TARGET"
    fi
    emacs -q -l "$INIT" -l "$HARNESS" -l "$TARGET" \
      --batch 2>&1 | filter_output
    ;;

  file)
    # Resolve relative paths
    if [[ ! "$TARGET" = /* ]]; then
      TARGET="$REPO_DIR/$TARGET"
    fi
    emacs -q -l "$INIT" -l "$HARNESS" \
      --eval "(progn (tiqsi-test-reset) (tiqsi-test-load-and-run \"$TARGET\") (let ((f 0)) (dolist (s tiqsi-test--suite-results) (cl-incf f (nth 2 s))) (tiqsi-test-summary) (kill-emacs (if (> f 0) 1 0))))" \
      --batch 2>&1 | filter_output
    ;;

  all)
    emacs -q -l "$INIT" -l "$HARNESS" \
      --eval '(tiqsi-test-batch-run-all)' \
      --batch 2>&1 | filter_output
    ;;
esac
