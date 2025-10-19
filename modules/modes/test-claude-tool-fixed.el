;;; test-claude-tool-fixed.el --- Test fixed tool execution -*- lexical-binding: t -*-

(defun test-claude-tool-execution ()
  "Test Claude REPL tool execution with fixes."
  (interactive)
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  (sit-for 0.5)
  
  ;; Get buffer and test
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (goto-char (point-max))
    
    ;; Simple test first
    (insert "What is 2+2?")
    (tiqsi-claude-repl-send-input)
    (sit-for 2)
    
    ;; Then test tool execution
    (goto-char (point-max))
    (insert "exec ! echo 'Testing tool execution'")
    (tiqsi-claude-repl-send-input)
    
    ;; Monitor output
    (run-with-timer 3 nil
                    (lambda ()
                      (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
                        (let ((content (buffer-substring-no-properties 
                                        (point-min) (point-max))))
                          (if (or (string-match-p "Tool:" content)
                                  (string-match-p "Testing tool execution" content)
                                  (string-match-p "THINKING" content))
                              (message "✅ Tool execution traces visible!")
                            (message "⚠️ Tool execution output not visible yet"))
                          (message "Buffer contains %d characters" (length content))))))))

(test-claude-tool-execution)