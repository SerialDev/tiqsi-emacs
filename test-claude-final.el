;;; test-claude-final.el --- Final test of Claude REPL -*- lexical-binding: t -*-

(defun test-claude-final ()
  "Final test to ensure Claude REPL works with visible responses."
  (interactive)
  
  ;; Kill any existing Claude REPL
  (when-let ((existing (get-buffer "*Claude REPL (tiqsi-emacs)*")))
    (kill-buffer existing))
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  
  ;; Wait a moment
  (sit-for 1)
  
  ;; Send test messages
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    ;; First message - simple
    (goto-char (point-max))
    (insert "Say hello")
    (call-interactively 'tiqsi-claude-repl-send-input)
    
    ;; Wait for response
    (message "Sent 'Say hello', waiting for response...")
    (sit-for 5)
    
    ;; Second message - test memory
    (goto-char (point-max))
    (insert "Remember the number 42")
    (call-interactively 'tiqsi-claude-repl-send-input)
    
    ;; Wait for response
    (message "Sent 'Remember the number 42', waiting for response...")
    (sit-for 5)
    
    ;; Third message - verify memory
    (goto-char (point-max))
    (insert "What number did I ask you to remember?")
    (call-interactively 'tiqsi-claude-repl-send-input)
    
    ;; Wait for response
    (message "Sent memory check question, waiting for response...")
    (sit-for 5)
    
    ;; Show buffer
    (display-buffer (current-buffer))
    (message "Test complete. Check the Claude REPL buffer for responses.")))

(provide 'test-claude-final)