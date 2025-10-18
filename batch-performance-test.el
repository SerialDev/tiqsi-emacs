;;; batch-performance-test.el --- Batch performance testing -*- lexical-binding: t -*-

;;; Commentary:
;; Non-interactive performance testing

;;; Code:

(require 'benchmark)

;; Report startup time
(message "\n=== STARTUP PERFORMANCE ===")
(message "Emacs startup time: %.3f seconds" 
         (float-time (time-subtract after-init-time before-init-time)))
(message "GC runs during startup: %d" gcs-done)

;; Test file operations
(message "\n=== FILE OPERATION TESTS ===")

;; Test 1: Open large Python file
(let ((test-file "test-projects/django-test/tests/admin_views/tests.py"))
  (garbage-collect)
  (let* ((gc-before gcs-done)
         (result (benchmark-run 1 
                   (with-temp-buffer
                     (insert-file-contents test-file)
                     (python-mode)
                     (font-lock-ensure))))
         (gc-after gcs-done))
    (message "Open & fontify large file (9201 lines):")
    (message "  Time: %.3f seconds" (car result))
    (message "  GC runs: %d" (- gc-after gc-before))
    (message "  GC time: %.3f seconds" (nth 1 result))))

;; Test 2: Search performance
(let ((test-file "test-projects/django-test/django/db/models/fields/__init__.py"))
  (with-temp-buffer
    (insert-file-contents test-file)
    (python-mode)
    (goto-char (point-min))
    (garbage-collect)
    (let* ((gc-before gcs-done)
           (result (benchmark-run 100
                     (goto-char (point-min))
                     (while (search-forward "def " nil t))))
           (gc-after gcs-done))
      (message "\nSearch for 'def ' 100 times in 2892-line file:")
      (message "  Time: %.3f seconds" (car result))
      (message "  GC runs: %d" (- gc-after gc-before)))))

;; Test 3: Indentation performance
(message "\n=== INDENTATION TESTS ===")
(with-temp-buffer
  (python-mode)
  (insert "def test_function():\n")
  (dotimes (i 100)
    (insert "    if condition:\n")
    (insert "        x = " (number-to-string i) "\n"))
  (goto-char (point-min))
  (let* ((result (benchmark-run 1
                   (indent-region (point-min) (point-max)))))
    (message "Indent 200 lines of Python:")
    (message "  Time: %.3f seconds" (car result))))

;; Test 4: Company completion (if available)
(when (featurep 'company)
  (message "\n=== COMPLETION TESTS ===")
  (with-temp-buffer
    (python-mode)
    (company-mode 1)
    (insert "import os\nos.")
    (let* ((result (benchmark-run 1
                     (company-complete))))
      (message "Company completion after 'os.':")
      (message "  Time: %.3f seconds" (car result)))))

;; Memory usage
(message "\n=== MEMORY USAGE ===")
(garbage-collect)
(when (fboundp 'memory-info)
  (let ((mem-info (memory-info)))
    (when mem-info
      (message "Memory used: %.1f MB" 
               (/ (car mem-info) 1024.0)))))

;; Configuration summary
(message "\n=== CONFIGURATION ===")
(message "gc-cons-threshold: %d" gc-cons-threshold)
(message "jit-lock-defer-time: %s" jit-lock-defer-time)
(message "auto-revert-interval: %s" auto-revert-interval)
(message "company-idle-delay: %s" (and (boundp 'company-idle-delay) company-idle-delay))
(message "lsp-idle-delay: %s" (and (boundp 'lsp-idle-delay) lsp-idle-delay))

(message "\n=== TEST COMPLETE ===")

(provide 'batch-performance-test)
;;; batch-performance-test.el ends here