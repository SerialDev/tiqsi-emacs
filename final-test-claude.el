;;; final-test-claude.el --- Final test of Claude REPL JSON parsing -*- lexical-binding: t -*-

;; First, let's check if the file loads
(condition-case err
    (load-file "modules/modes/tiqsi-claude-repl.el")
  (error (message "ERROR loading file: %s" (error-message-string err))))

;; If it loaded, test JSON parsing
(when (featurep 'tiqsi-claude-repl)
  (message "\n✅ File loaded successfully!")
  
  ;; Test JSON parsing
  (with-current-buffer (get-buffer-create "*test-json*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    (insert "λ test\n⏳ Thinking...\n\n")
    
    ;; Set up variables
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start nil)
    (setq-local tiqsi-claude-repl-show-thinking t)
    (setq-local tiqsi-claude-repl--claude-session-id nil)
    
    ;; Create mock process
    (let ((proc (make-process :name "test" :buffer (current-buffer) :command '("echo"))))
      
      ;; Test system message
      (tiqsi-claude-repl--process-filter proc 
        "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}\n")
      
      ;; Test assistant message  
      (tiqsi-claude-repl--process-filter proc
        "{\"type\":\"assistant\",\"message\":{\"id\":\"msg\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! This works!\"}]}}\n")
      
      (delete-process proc)
      
      ;; Check results
      (message "\n=== BUFFER CONTENTS ===")
      (message "%s" (buffer-string))
      
      (goto-char (point-min))
      (if (search-forward "Hello! This works!" nil t)
          (message "\n✅ SUCCESS: Claude response is visible!")
        (message "\n❌ FAILURE: Claude response NOT found!"))
      
      (if tiqsi-claude-repl--claude-session-id
          (message "✅ SUCCESS: Session ID captured: %s" tiqsi-claude-repl--claude-session-id)
        (message "❌ FAILURE: No session ID captured")))))