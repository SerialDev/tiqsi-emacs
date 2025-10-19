(require 'tiqsi-claude-repl)

(message "Testing Claude REPL...")

(with-current-buffer (get-buffer-create "*test*")
  (tiqsi-claude-repl-mode)
  (setq-local tiqsi-claude-repl--json-buffer "")
  (setq-local tiqsi-claude-repl--json-processing-enabled nil)
  (setq-local tiqsi-claude-repl--output-start nil) 
  (setq-local tiqsi-claude-repl-show-thinking t)
  (setq-local tiqsi-claude-repl--claude-session-id nil)
  
  (insert "λ test\n")
  
  (let ((proc (make-process :name "test" :buffer (current-buffer) :command '("true"))))
    
    ;; Test JSON parsing
    (tiqsi-claude-repl--process-filter proc 
      "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}\n")
    
    (tiqsi-claude-repl--process-filter proc
      "{\"type\":\"assistant\",\"message\":{\"id\":\"msg\",\"content\":[{\"type\":\"text\",\"text\":\"It works!\"}]}}\n")
    
    (delete-process proc))
  
  (message "\nBuffer:\n%s" (buffer-string))
  
  (goto-char (point-min))
  (if (search-forward "It works!" nil t)
      (message "✅ SUCCESS: JSON parsing works!")
    (message "❌ FAIL: No output"))
  
  (if tiqsi-claude-repl--claude-session-id  
      (message "✅ SUCCESS: Session ID = %s" tiqsi-claude-repl--claude-session-id)
    (message "❌ FAIL: No session ID")))