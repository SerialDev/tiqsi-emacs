;;; test-claude-batch.el --- Batch test for Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-json-parsing ()
  "Test JSON parsing in batch mode."
  (with-current-buffer (get-buffer-create "*test*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    (insert "λ test\n⏳ Thinking...\n\n")
    
    ;; Initialize variables
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start nil)
    (setq-local tiqsi-claude-repl-show-thinking t)
    (setq-local tiqsi-claude-repl--claude-session-id nil)
    
    ;; Create process
    (let ((proc (make-process 
                 :name "test" 
                 :buffer (current-buffer) 
                 :command (list "echo" "test"))))
      
      ;; Test system message (session init)
      (message "\n=== Testing System Message ===")
      (tiqsi-claude-repl--process-filter proc 
        "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-session-123\"}\n")
      
      ;; Test assistant message
      (message "\n=== Testing Assistant Message ===")
      (tiqsi-claude-repl--process-filter proc 
        "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_123\",\"content\":[{\"type\":\"text\",\"text\":\"Hello from Claude! I can help you.\"}]}}\n")
      
      ;; Clean up
      (delete-process proc)
      
      ;; Show results
      (message "\n=== BUFFER CONTENTS ===")
      (message "%s" (buffer-string))
      
      (message "\n=== SESSION INFO ===")
      (message "Claude session ID captured: %s" 
               (if tiqsi-claude-repl--claude-session-id 
                   tiqsi-claude-repl--claude-session-id
                 "NOT CAPTURED"))
      
      ;; Check if response is visible
      (goto-char (point-min))
      (if (search-forward "Hello from Claude!" nil t)
          (progn
            (message "\n✅ SUCCESS: Claude response is visible!")
            (message "Found at position: %d" (match-beginning 0)))
        (message "\n❌ FAILURE: Claude response NOT found!"))
      
      ;; Check if session ID would be used for resume
      (if tiqsi-claude-repl--claude-session-id
          (message "✅ SUCCESS: Session ID would be used with --resume flag")
        (message "❌ FAILURE: No session ID to resume with")))))

;; Run the test
(test-claude-json-parsing)

(provide 'test-claude-batch)