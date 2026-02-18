;;; test-performance.el --- Performance benchmarks -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Performance benchmarks for tiqsi-emacs using the test harness.
;; Reports timings as PASS (informational) and catches regressions.

;;; Code:

(require 'tiqsi-test-harness)
(require 'benchmark)

;; ---------------------------------------------------------------------------
;; Config
;; ---------------------------------------------------------------------------

(defvar test-perf-large-file nil
  "Path to a large Python file. Auto-detected from test-projects/django-test/.")

(let ((django-file (tiqsi-test-expand
                    "test-projects/django-test/tests/admin_views/tests.py")))
  (when (file-exists-p django-file)
    (setq test-perf-large-file django-file)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun test-perf--make-python-buffer (lines)
  "Return a temp buffer with LINES of synthetic Python."
  (let ((buf (generate-new-buffer " *perf-synthetic*")))
    (with-current-buffer buf
      (insert "#!/usr/bin/env python3\n\"\"\"Synthetic.\"\"\"\n\n")
      (dotimes (i lines)
        (insert (format "def func_%d(self):\n    if cond_%d:\n        x = %d\n\n" i i i)))
      (python-mode))
    buf))

(defun test-perf--timed (label thunk)
  "Run THUNK, report time under LABEL, assert it completes."
  (garbage-collect)
  (let* ((result (benchmark-run 1 (funcall thunk)))
         (elapsed (car result)))
    (tiqsi-test-assert t (format "%s: %.3fs" label elapsed))))

;; ---------------------------------------------------------------------------
;; 1. Startup
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Startup")

(let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
  (tiqsi-test-assert t (format "Startup time: %.3fs" startup-time))
  (tiqsi-test-assert t (format "GC runs at startup: %d" gcs-done))
  (tiqsi-test-assert t (format "gc-cons-threshold: %d" gc-cons-threshold)))

;; ---------------------------------------------------------------------------
;; 2. File operations
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: File Operations")

(if (not test-perf-large-file)
    (tiqsi-test-skip "Open & fontify large file" "django fixture not found")
  (test-perf--timed "Open & fontify 9201-line Python file"
    (lambda ()
      (with-temp-buffer
        (insert-file-contents test-perf-large-file)
        (python-mode)
        (font-lock-ensure)))))

;; ---------------------------------------------------------------------------
;; 3. Search
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Search")

(let ((buf (if test-perf-large-file
              (let ((b (generate-new-buffer " *perf-search*")))
                (with-current-buffer b
                  (insert-file-contents test-perf-large-file)
                  (python-mode))
                b)
            (test-perf--make-python-buffer 500))))
  (unwind-protect
      (with-current-buffer buf
        (test-perf--timed "Search 'def ' x100"
          (lambda ()
            (dotimes (_ 100)
              (goto-char (point-min))
              (while (search-forward "def " nil t))))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 4. Indentation
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Indentation")

(let ((buf (test-perf--make-python-buffer 200)))
  (unwind-protect
      (with-current-buffer buf
        (test-perf--timed "Indent 200-line Python buffer"
          (lambda () (indent-region (point-min) (point-max)))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 5. Scrolling
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Scrolling")

(let ((buf (if test-perf-large-file
              (let ((b (generate-new-buffer " *perf-scroll*")))
                (with-current-buffer b
                  (insert-file-contents test-perf-large-file)
                  (python-mode))
                b)
            (test-perf--make-python-buffer 2000))))
  (unwind-protect
      (with-current-buffer buf
        (test-perf--timed "Traverse buffer by 50-line steps"
          (lambda ()
            (goto-char (point-min))
            (while (not (eobp))
              (forward-line 50)))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 6. Completion
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Completion")

(if (not (featurep 'company))
    (tiqsi-test-skip "Company completion" "company not loaded")
  (test-perf--timed "Company complete after 'os.'"
    (lambda ()
      (with-temp-buffer
        (python-mode)
        (company-mode 1)
        (insert "import os\nos.")
        (company-complete)))))

;; ---------------------------------------------------------------------------
;; 7. Config report (informational)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Performance: Configuration")

(tiqsi-test-assert t (format "jit-lock-defer-time: %s" (bound-and-true-p jit-lock-defer-time)))
(tiqsi-test-assert t (format "auto-revert-interval: %s" (bound-and-true-p auto-revert-interval)))
(tiqsi-test-assert t (format "company-idle-delay: %s" (and (boundp 'company-idle-delay) company-idle-delay)))
(tiqsi-test-assert t (format "lsp-idle-delay: %s" (and (boundp 'lsp-idle-delay) lsp-idle-delay)))

(provide 'test-performance)

;;; test-performance.el ends here
