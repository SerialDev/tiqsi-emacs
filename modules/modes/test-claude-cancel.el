;;; test-claude-cancel.el --- Test Claude REPL cancel functionality -*- lexical-binding: t -*-

;; Test the cancel functionality
(defun test-claude-cancel ()
  "Test Claude REPL cancel with thinking animation."
  (interactive)
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  
  ;; Wait a moment
  (sit-for 0.5)
  
  ;; Get the buffer
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    ;; Show buffer state
    (message "Buffer-local variables:")
    (message "  tiqsi-claude-repl--current-process: %s" tiqsi-claude-repl--current-process)
    (message "  tiqsi-claude-repl--thinking-timer: %s" tiqsi-claude-repl--thinking-timer)
    
    ;; Simulate sending input
    (goto-char (point-max))
    (insert "test input")
    (tiqsi-claude-repl-send-input)
    
    ;; Wait for thinking to start
    (sit-for 0.5)
    
    ;; Check state again
    (message "\nAfter sending input:")
    (message "  tiqsi-claude-repl--current-process: %s" tiqsi-claude-repl--current-process)
    (message "  tiqsi-claude-repl--thinking-timer: %s" tiqsi-claude-repl--thinking-timer)
    
    ;; Try to cancel
    (message "\nAttempting to cancel...")
    (tiqsi-claude-repl-cancel)))

;; Run the test
(test-claude-cancel)