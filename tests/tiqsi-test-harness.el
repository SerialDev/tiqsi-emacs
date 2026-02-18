;;; tiqsi-test-harness.el --- Test harness for tiqsi-emacs development -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; A test harness for tiqsi-emacs that provides:
;;
;; 1. Reliable batch-mode execution against the full tiqsi config
;; 2. A simple assertion framework (tiqsi-test-assert, tiqsi-test-assert-equal, etc.)
;; 3. Structured output parseable by external tools (AI agents, CI, scripts)
;; 4. An eval gateway for ad-hoc elisp evaluation from the shell
;;
;; The harness solves the core problem: init-lite.el assumes an interactive
;; Emacs session, and various modules fail at load time when env vars are
;; missing, services are unavailable, etc. The harness loads init-lite.el
;; with appropriate guards and provides a clean way to run tests against
;; the fully configured environment.
;;
;; Usage from shell (the AI agent REPL):
;;
;;   # Run all registered test suites:
;;   ./tests/run-tests.sh
;;
;;   # Run a specific test file:
;;   ./tests/run-tests.sh tests/test-claude-repl.el
;;
;;   # Evaluate arbitrary elisp against the live config:
;;   ./tests/run-tests.sh --eval '(message "features: %d" (length features))'
;;
;;   # Quick smoke test (just load the config):
;;   ./tests/run-tests.sh --smoke
;;
;;   # Run with verbose init output:
;;   ./tests/run-tests.sh --verbose tests/test-claude-repl.el
;;
;; Usage from Emacs (interactive):
;;
;;   M-x tiqsi-test-run-all    -- run all test files in tests/
;;   M-x tiqsi-test-run-file   -- run a specific test file
;;

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; State
;; ---------------------------------------------------------------------------

(defvar tiqsi-test--pass-count 0 "Total passed assertions.")
(defvar tiqsi-test--fail-count 0 "Total failed assertions.")
(defvar tiqsi-test--skip-count 0 "Total skipped tests.")
(defvar tiqsi-test--errors nil "List of (LABEL . DETAIL) for failures.")
(defvar tiqsi-test--current-suite "" "Name of the currently running suite.")
(defvar tiqsi-test--suite-results nil "Alist of (SUITE-NAME PASS FAIL SKIP).")
(defvar tiqsi-test-verbose nil "When non-nil, print extra diagnostics.")

;; ---------------------------------------------------------------------------
;; Core assertion API
;; ---------------------------------------------------------------------------

(defun tiqsi-test-assert (condition label &optional detail)
  "Assert CONDITION is non-nil. LABEL names the test. DETAIL is optional context."
  (if condition
      (progn
        (cl-incf tiqsi-test--pass-count)
        (message "  PASS  %s" label))
    (cl-incf tiqsi-test--fail-count)
    (push (cons label (or detail "")) tiqsi-test--errors)
    (message "  FAIL  %s%s" label (if detail (format "  [%s]" detail) ""))))

(defun tiqsi-test-assert-equal (expected actual label)
  "Assert EXPECTED equals ACTUAL."
  (tiqsi-test-assert (equal expected actual) label
                     (unless (equal expected actual)
                       (format "expected %S, got %S" expected actual))))

(defun tiqsi-test-assert-match (regexp string label)
  "Assert STRING matches REGEXP."
  (tiqsi-test-assert (and (stringp string) (string-match-p regexp string))
                     label
                     (unless (and (stringp string) (string-match-p regexp string))
                       (format "regexp %S did not match %S" regexp string))))

(defun tiqsi-test-assert-fboundp (symbol label)
  "Assert SYMBOL is a defined function."
  (tiqsi-test-assert (fboundp symbol) label
                     (unless (fboundp symbol) "function not defined")))

(defun tiqsi-test-assert-boundp (symbol label)
  "Assert SYMBOL is a bound variable."
  (tiqsi-test-assert (boundp symbol) label
                     (unless (boundp symbol) "variable not bound")))

(defun tiqsi-test-skip (label &optional reason)
  "Mark test LABEL as skipped with optional REASON."
  (cl-incf tiqsi-test--skip-count)
  (message "  SKIP  %s%s" label (if reason (format "  [%s]" reason) "")))

(defun tiqsi-test-assert-no-error (label &rest body-forms)
  "Evaluate BODY-FORMS and assert no error is signaled.
This is a function, not a macro, so pass a lambda:
  (tiqsi-test-assert-no-error \"my test\" (lambda () (do-thing)))"
  (condition-case err
      (progn
        (dolist (form body-forms) (funcall form))
        (tiqsi-test-assert t label))
    (error
     (tiqsi-test-assert nil label (error-message-string err)))))

;; ---------------------------------------------------------------------------
;; Suite management
;; ---------------------------------------------------------------------------

(defun tiqsi-test-suite (name)
  "Begin a new test suite named NAME."
  ;; Save previous suite results if any
  (when (not (string-empty-p tiqsi-test--current-suite))
    (tiqsi-test--save-suite-result))
  (setq tiqsi-test--current-suite name)
  (setq tiqsi-test--pass-count 0
        tiqsi-test--fail-count 0
        tiqsi-test--skip-count 0
        tiqsi-test--errors nil)
  (message "\n--- %s ---" name))

(defun tiqsi-test--save-suite-result ()
  "Save the current suite's results to `tiqsi-test--suite-results'."
  (push (list tiqsi-test--current-suite
              tiqsi-test--pass-count
              tiqsi-test--fail-count
              tiqsi-test--skip-count)
        tiqsi-test--suite-results))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(defun tiqsi-test-summary ()
  "Print structured summary of all test results. Returns total fail count."
  ;; Save final suite
  (when (not (string-empty-p tiqsi-test--current-suite))
    (tiqsi-test--save-suite-result))

  (let ((total-pass 0) (total-fail 0) (total-skip 0))
    (message "\n========================================")
    (message "  TEST RESULTS")
    (message "========================================")
    (dolist (suite (reverse tiqsi-test--suite-results))
      (let ((name (nth 0 suite))
            (pass (nth 1 suite))
            (fail (nth 2 suite))
            (skip (nth 3 suite)))
        (cl-incf total-pass pass)
        (cl-incf total-fail fail)
        (cl-incf total-skip skip)
        (message "  %-40s %2d pass  %2d fail  %2d skip"
                 name pass fail skip)))
    (message "----------------------------------------")
    (message "  %-40s %2d pass  %2d fail  %2d skip"
             "TOTAL" total-pass total-fail total-skip)
    (message "========================================")
    (when (> total-fail 0)
      (message "\nFailed tests:")
      (dolist (suite (reverse tiqsi-test--suite-results))
        ;; Re-report errors would need storage per suite; for now the inline
        ;; FAIL messages above serve as the detail.
        ))
    (message "\nEXIT_CODE=%d" (if (> total-fail 0) 1 0))
    total-fail))

(defun tiqsi-test-reset ()
  "Reset all test state for a fresh run."
  (setq tiqsi-test--pass-count 0
        tiqsi-test--fail-count 0
        tiqsi-test--skip-count 0
        tiqsi-test--errors nil
        tiqsi-test--current-suite ""
        tiqsi-test--suite-results nil))

;; ---------------------------------------------------------------------------
;; Repo root resolution
;; ---------------------------------------------------------------------------

(defvar tiqsi-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name default-directory))))
  "Root of the tiqsi-emacs repository, derived from harness location.")

(defun tiqsi-test-expand (relative-path)
  "Expand RELATIVE-PATH relative to `tiqsi-test-root'."
  (expand-file-name relative-path tiqsi-test-root))

;; ---------------------------------------------------------------------------
;; File runners
;; ---------------------------------------------------------------------------

(defun tiqsi-test-load-and-run (test-file)
  "Load TEST-FILE and call its standard runner function.
Convention: tests/test-foo.el should define `test-foo-run' that uses the harness API."
  (let ((full-path (if (file-name-absolute-p test-file)
                       test-file
                     (tiqsi-test-expand test-file))))
    (if (not (file-exists-p full-path))
        (progn
          (tiqsi-test-suite (format "MISSING: %s" test-file))
          (tiqsi-test-assert nil (format "File exists: %s" test-file) "file not found"))
      (condition-case err
          (load full-path nil t)
        (error
         (tiqsi-test-suite (format "LOAD-ERROR: %s" test-file))
         (tiqsi-test-assert nil "File loads without error"
                            (error-message-string err)))))))

(defun tiqsi-test-run-all ()
  "Find and run all test-*.el files in the tests/ directory."
  (interactive)
  (tiqsi-test-reset)
  (message "\n========================================")
  (message "  Tiqsi Emacs Test Harness")
  (message "  %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
  (message "  Root: %s" tiqsi-test-root)
  (message "========================================")
  (let* ((test-dir (tiqsi-test-expand "tests/"))
         (test-files (directory-files test-dir t "^test-.*\\.el$")))
    (dolist (f test-files)
      ;; Don't load the harness itself
      (unless (string-match-p "tiqsi-test-harness" f)
        (tiqsi-test-load-and-run f))))
  (tiqsi-test-summary))

(defun tiqsi-test-run-file (file)
  "Run a single test FILE."
  (interactive "fTest file: ")
  (tiqsi-test-reset)
  (tiqsi-test-load-and-run file)
  (tiqsi-test-summary))

;; ---------------------------------------------------------------------------
;; Eval gateway -- for ad-hoc elisp from the shell
;; ---------------------------------------------------------------------------

(defun tiqsi-test-eval-and-exit (expr-string)
  "Evaluate EXPR-STRING, print result, exit 0."
  (condition-case err
      (let ((result (eval (car (read-from-string expr-string)))))
        (message "RESULT: %S" result)
        (kill-emacs 0))
    (error
     (message "ERROR: %s" (error-message-string err))
     (kill-emacs 1))))

;; ---------------------------------------------------------------------------
;; Smoke test -- just verify init-lite.el loads
;; ---------------------------------------------------------------------------

(defun tiqsi-test-smoke ()
  "Quick smoke test: verify the config loaded and report basics."
  (tiqsi-test-reset)
  (tiqsi-test-suite "Smoke Test")
  (tiqsi-test-assert t "Emacs started")
  (tiqsi-test-assert (> (length features) 50)
                     (format "Features loaded: %d" (length features)))
  (tiqsi-test-assert (fboundp 'load-expand)
                     "load-expand defined")
  (tiqsi-test-assert (featurep 'company)
                     "company-mode loaded")
  (tiqsi-test-assert (featurep 'evil)
                     "evil loaded")
  (tiqsi-test-assert (fboundp 'straight-use-package)
                     "straight.el available")
  ;; Check key modules loaded
  (tiqsi-test-assert (featurep 'helm)
                     "helm loaded")
  (tiqsi-test-assert (featurep 'hydra)
                     "hydra loaded")
  (tiqsi-test-assert (fboundp 'tiqsi-claude-hydra)
                     "Claude hydra available")
  (let ((fails (tiqsi-test-summary)))
    (kill-emacs (if (> fails 0) 1 0))))

;; ---------------------------------------------------------------------------
;; Batch entry point
;; ---------------------------------------------------------------------------

(defun tiqsi-test-batch-run-all ()
  "Entry point for batch mode: run all tests, exit with code."
  (tiqsi-test-run-all)
  ;; tiqsi-test-summary already called by run-all; compute exit code
  (let ((total-fail 0))
    (dolist (suite tiqsi-test--suite-results)
      (cl-incf total-fail (nth 2 suite)))
    (kill-emacs (if (> total-fail 0) 1 0))))

(provide 'tiqsi-test-harness)

;;; tiqsi-test-harness.el ends here
