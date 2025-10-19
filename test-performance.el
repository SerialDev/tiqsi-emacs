;;; test-performance.el --- Performance testing script -*- lexical-binding: t -*-

;;; Commentary:
;; Script to test Emacs performance with large Python files

;;; Code:

;; Record startup time
(message "Emacs startup time: %.2f seconds" (float-time (time-subtract after-init-time before-init-time)))

;; Enable explain-pause-mode
(when (fboundp 'explain-pause-mode)
  (explain-pause-mode 1))

;; Function to open a file and measure time
(defun test-open-file (file)
  "Open FILE and measure time taken."
  (let ((start-time (current-time)))
    (find-file file)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Opened %s in %.2f seconds" file elapsed)
      elapsed)))

;; Function to test completion
(defun test-completion-performance ()
  "Test completion performance in current buffer."
  (when (derived-mode-p 'python-mode)
    (goto-char (point-min))
    (search-forward "def " nil t)
    (end-of-line)
    (insert "\n    ")
    (let ((start-time (current-time)))
      (company-complete)
      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message "Completion took %.2f seconds" elapsed)))))

;; Function to scroll through buffer
(defun test-scroll-performance ()
  "Test scrolling performance."
  (let ((start-time (current-time))
        (line-count 0))
    (goto-char (point-min))
    (while (not (eobp))
      (forward-line 50)
      (recenter)
      (setq line-count (+ line-count 50)))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Scrolled %d lines in %.2f seconds" line-count elapsed))))

;; Main test function
(defun run-performance-tests ()
  "Run all performance tests."
  (interactive)
  
  ;; Test 1: Open large file
  (message "\n=== TEST 1: Opening large Python file ===")
  (test-open-file "django-test/tests/admin_views/tests.py")
  
  ;; Wait for LSP to initialize
  (sit-for 3)
  
  ;; Test 2: Scrolling
  (message "\n=== TEST 2: Scrolling performance ===")
  (test-scroll-performance)
  
  ;; Test 3: Completion (if available)
  (message "\n=== TEST 3: Completion performance ===")
  (when (bound-and-true-p company-mode)
    (test-completion-performance))
  
  ;; Test 4: Search
  (message "\n=== TEST 4: Search performance ===")
  (let ((start-time (current-time)))
    (goto-char (point-min))
    (while (search-forward "self" nil t))
    (message "Search completed in %.2f seconds" 
             (float-time (time-subtract (current-time) start-time))))
  
  ;; Show explain-pause results
  (when (fboundp 'explain-pause-top)
    (message "\n=== Explain-pause top slowdowns ===")
    (explain-pause-top)))

;; Schedule tests to run after Emacs is fully loaded
(run-with-idle-timer 2 nil 'run-performance-tests)

(provide 'test-performance)
;;; test-performance.el ends here