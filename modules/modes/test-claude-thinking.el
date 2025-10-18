;;; test-claude-thinking.el --- Test Claude thinking traces -*- lexical-binding: t -*-

;; Test function to verify thinking traces are visible
(defun test-claude-thinking-traces ()
  "Test that Claude REPL shows thinking traces."
  (interactive)
  
  ;; Load the patches if not already loaded
  (unless (fboundp 'tiqsi-claude-repl--enhanced-process-filter)
    (load-file "claude-repl/tiqsi-claude-repl-patches.el"))
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  
  ;; Wait a moment
  (sit-for 0.5)
  
  ;; Test with a command that should show thinking
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (goto-char (point-max))
    ;; Test with a complex request that triggers thinking
    (insert "Can you analyze this code and tell me what it does: (defun test (x) (* x x))")
    (tiqsi-claude-repl-send-input)
    
    ;; Monitor for thinking output
    (message "Monitoring for thinking traces...")
    (run-with-timer 2 nil
                    (lambda ()
                      (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
                        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                          (if (string-match-p "THINKING\\|Thinking:" content)
                              (message "✅ Thinking traces are visible!")
                            (message "⚠️  No thinking traces found yet..."))))))))

(test-claude-thinking-traces)