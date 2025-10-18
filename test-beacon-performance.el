;;; test-beacon-performance.el --- Compare beacon implementations -*- lexical-binding: t -*-

;;; Commentary:
;; Performance comparison between original beacon and ultra-beacon

;;; Code:

(defun test-beacon-original ()
  "Test original beacon performance."
  (beacon-mode -1)
  (require 'beacon)
  (beacon-mode 1)
  (let ((start (current-time))
        (iterations 100))
    (dotimes (i iterations)
      ;; Simulate cursor movement
      (goto-char (point-min))
      (forward-line 10)
      (beacon-blink)
      (sit-for 0.01))
    (beacon-mode -1)
    (message "Original beacon: %d iterations in %.3f seconds"
             iterations
             (float-time (time-subtract (current-time) start)))))

(defun test-beacon-ultra ()
  "Test ultra beacon performance."
  (when (featurep 'beacon)
    (beacon-mode -1))
  (require 'modes-beacon-ultra)
  (ultra-beacon-mode 1)
  (let ((start (current-time))
        (iterations 100))
    (dotimes (i iterations)
      ;; Simulate cursor movement
      (goto-char (point-min))
      (forward-line 10)
      (ultra-beacon-blink)
      (sit-for 0.01))
    (ultra-beacon-mode -1)
    (message "Ultra beacon: %d iterations in %.3f seconds"
             iterations
             (float-time (time-subtract (current-time) start)))))

(defun compare-beacon-implementations ()
  "Compare beacon implementations."
  (interactive)
  (with-temp-buffer
    ;; Create some content
    (dotimes (i 100)
      (insert (format "Line %d with some text to make it realistic\n" i)))
    
    ;; Test ultra beacon first (to avoid any caching effects)
    (test-beacon-ultra)
    
    ;; Small delay
    (sit-for 1)
    
    ;; Test original beacon
    (test-beacon-original)))

;; Run the comparison
(when (called-interactively-p 'interactive)
  (compare-beacon-implementations))

(provide 'test-beacon-performance)
;;; test-beacon-performance.el ends here