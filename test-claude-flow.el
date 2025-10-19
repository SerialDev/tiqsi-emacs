;;; test-claude-flow.el --- Test exact flow of Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-complete-flow ()
  "Test the complete flow to see where text gets lost."
  (interactive)
  
  ;; Create a test buffer that mimics the real flow
  (with-current-buffer (get-buffer-create "*test-claude-flow*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    ;; Step 1: Initial prompt
    (insert "λ Say hello\n")
    (message "Step 1 - Initial prompt:\n%s" (buffer-string))
    
    ;; Step 2: Add thinking message (like send-input does)
    (insert (tiqsi-claude-repl--colorize "⏳ Thinking..." 'tiqsi-claude-repl-thinking))
    (insert "\n\n")
    (message "\nStep 2 - After thinking message:\n%s" (buffer-string))
    
    ;; Step 3: Initialize state variables
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start nil)
    (setq-local tiqsi-claude-repl--request-start-time (current-time))
    (setq-local tiqsi-claude-repl-show-thinking t)
    
    ;; Step 4: Simulate first process filter call (sets output start)
    (message "\nStep 4 - First process filter call...")
    (let ((mock-process (make-process
                        :name "test-flow"
                        :buffer (current-buffer)
                        :command '("echo" "test"))))
      
      ;; First call with system init
      (let ((output "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}\n"))
        (tiqsi-claude-repl--process-filter mock-process output))
      
      (message "After first filter - output-start at: %s" 
               (if tiqsi-claude-repl--output-start 
                   (marker-position tiqsi-claude-repl--output-start)
                 "NOT SET"))
      (message "Buffer:\n%s" (buffer-string))
      
      ;; Step 5: Second call with actual message
      (message "\nStep 5 - Second process filter call (assistant message)...")
      (let ((output "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_test\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! I can see your message.\"}]}}\n"))
        (tiqsi-claude-repl--process-filter mock-process output))
      
      (message "After second filter:")
      (message "Buffer size: %d" (buffer-size))
      (message "Buffer:\n%s" (buffer-string))
      
      ;; Step 6: Third call with result
      (message "\nStep 6 - Third process filter call (result)...")
      (let ((output "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":1000}\n"))
        (tiqsi-claude-repl--process-filter mock-process output))
      
      (message "After third filter:")
      (message "Buffer:\n%s" (buffer-string))
      
      ;; Step 7: Process sentinel (post-processing)
      (message "\nStep 7 - Process sentinel...")
      (tiqsi-claude-repl--process-sentinel mock-process "finished\n")
      
      (message "After sentinel:")
      (message "Buffer:\n%s" (buffer-string))
      
      ;; Clean up
      (delete-process mock-process))
    
    ;; Display the buffer
    (display-buffer (current-buffer))))

(provide 'test-claude-flow)