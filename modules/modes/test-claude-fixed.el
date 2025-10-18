;;; test-claude-fixed.el --- Test fixed Claude REPL -*- lexical-binding: t -*-

;; Simple test to verify the fixes work
(defun test-claude-fixed ()
  "Test the fixed Claude REPL implementation."
  (interactive)
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  
  ;; Wait for initialization
  (sit-for 1)
  
  ;; Get the REPL buffer
  (let ((buffer (tiqsi-claude-repl--get-or-create-buffer)))
    (with-current-buffer buffer
      ;; Show current state
      (message "Claude REPL started in buffer: %s" (buffer-name))
      (message "JSON processing enabled: %s" tiqsi-claude-repl--json-processing-enabled)
      (message "Current process: %s" tiqsi-claude-repl--current-process)
      
      ;; Test simple input
      (goto-char (point-max))
      (insert "What is 2 + 2?")
      (tiqsi-claude-repl-send-input)
      
      ;; Monitor for output
      (run-with-timer 2 nil
                      (lambda ()
                        (with-current-buffer buffer
                          (message "Buffer size after 2s: %d chars" (buffer-size))
                          (when (> (buffer-size) 200)
                            (message "✅ Claude REPL is working!"))))))))

;; Run the test
(test-claude-fixed)