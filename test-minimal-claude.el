;;; test-minimal-claude.el --- Minimal test for Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-minimal-json-processing ()
  "Test minimal JSON processing to isolate the issue."
  (interactive)
  
  ;; Create test buffer
  (with-current-buffer (get-buffer-create "*minimal-json-test*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    ;; Insert a prompt
    (insert "λ test message\n")
    (insert "⏳ Thinking...\n\n")
    
    ;; Set up state
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start (point-marker))
    (setq-local tiqsi-claude-repl-show-thinking t) ; Enable debug messages
    
    ;; Test JSON lines exactly as Claude sends them
    (let ((json-lines
           '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}"
             "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_test\",\"content\":[{\"type\":\"text\",\"text\":\"I'll remember 22 for you!\"}]}}"
             "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":1000}")))
      
      ;; Process each line
      (dolist (line json-lines)
        (message "\n=== Processing line: %s" line)
        
        ;; Create mock process
        (let ((mock-process (make-process
                            :name "minimal-test"
                            :buffer (current-buffer)
                            :command '("echo" "test"))))
          
          ;; Process the line with newline
          (tiqsi-claude-repl--process-filter mock-process (concat line "\n"))
          
          (delete-process mock-process))
        
        ;; Show buffer state
        (message "Buffer after processing:\n%s" (buffer-string))
        (message "Buffer size: %d" (buffer-size))))
    
    ;; Final state
    (message "\n=== Final buffer contents ===")
    (message "%s" (buffer-string))
    
    ;; Check if text was inserted
    (goto-char (point-min))
    (if (search-forward "remember 22" nil t)
        (message "✅ SUCCESS: Claude's response text found!")
      (message "❌ FAILURE: Claude's response text NOT found"))
    
    (display-buffer (current-buffer))))

;; Run the test
(provide 'test-minimal-claude)

;;; test-minimal-claude.el ends here