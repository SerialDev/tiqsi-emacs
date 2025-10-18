;;; test-claude-tool-use.el --- Test Claude REPL tool use -*- lexical-binding: t -*-

;; Test function to debug tool use output
(defun test-claude-tool-use ()
  "Test Claude REPL with tool use commands."
  (interactive)
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  ;; Wait a moment
  (sit-for 1)
  ;; Test simple command first
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (goto-char (point-max))
    (insert "exec ! echo 'Testing tool use'")
    (tiqsi-claude-repl-send-input)
    ;; Wait for response
    (sit-for 3)
    ;; Check buffer content
    (message "Buffer content after tool use:\n%s" 
             (buffer-substring-no-properties (point-min) (point-max)))))

;; Run test
(test-claude-tool-use)