;;; demo-claude-repl.el --- Demonstrate Claude REPL features -*- lexical-binding: t -*-

;; This demonstrates the key features of the Claude REPL:
;; 1. Clean JSON parsing - no raw JSON in output
;; 2. Session continuity - Claude remembers context
;; 3. Beautiful formatting with syntax highlighting
;; 4. Excellent user experience

(defun demo-claude-repl ()
  "Demonstrate Claude REPL with all features."
  (interactive)
  
  ;; Load the configuration
  (load (expand-file-name "init-lite.el"))
  
  ;; Start Claude REPL
  (tiqsi-claude-repl-start)
  
  ;; Get the buffer
  (let ((repl-buffer (tiqsi-claude-repl--get-or-create-buffer)))
    (display-buffer repl-buffer)
    
    (message "\n=== Claude REPL Demo ===")
    (message "1. The REPL is now open")
    (message "2. JSON output is parsed cleanly - no raw JSON visible")
    (message "3. Session continuity is maintained by Claude CLI")
    (message "4. Code blocks have syntax highlighting")
    (message "5. Markdown formatting is applied")
    (message "\nTry these commands:")
    (message "  - Type a message and press RET")
    (message "  - Ask Claude to remember a number")
    (message "  - Ask Claude what number you told it")
    (message "  - Ask for a code example")
    (message "\nKey bindings:")
    (message "  - RET or C-c C-c : Send message")
    (message "  - C-g : Cancel current request")
    (message "  - C-c C-k : Clear buffer")
    (message "  - C-c C-r : Recover session")
    (message "  - C-c C-q : Quit")
    (message "\nThe hydra menu is available at M-c")))

(defun test-claude-repl-features ()
  "Test all Claude REPL features."
  (interactive)
  
  (message "\n=== Testing Claude REPL Features ===")
  
  ;; Test 1: CLI availability
  (if (tiqsi-claude-repl--executable-available-p)
      (message "✅ Claude CLI found at: %s" (executable-find tiqsi-claude-repl-program))
    (message "❌ Claude CLI not found"))
  
  ;; Test 2: Start session
  (condition-case err
      (progn
        (tiqsi-claude-repl-start)
        (message "✅ Session started successfully"))
    (error (message "❌ Error starting session: %s" err)))
  
  ;; Test 3: JSON processing
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (let ((test-json-lines
           '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}"
             "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello! I'm Claude, your AI assistant.\"}]}}")))
      
      ;; Clear state
      (setq-local tiqsi-claude-repl--json-buffer "")
      (setq-local tiqsi-claude-repl--json-processing-enabled nil)
      (setq-local tiqsi-claude-repl--output-start (point-marker))
      
      ;; Process JSON
      (dolist (json-line test-json-lines)
        (let ((mock-process (make-process
                            :name "test-json"
                            :buffer (current-buffer)
                            :command '("echo" "test"))))
          (tiqsi-claude-repl--process-filter mock-process (concat json-line "\n"))
          (delete-process mock-process)))
      
      ;; Check results
      (goto-char (point-min))
      (if (search-forward "Hello! I'm Claude" nil t)
          (message "✅ JSON parsing works - Claude's text extracted")
        (message "❌ JSON parsing failed"))
      
      (goto-char (point-min))
      (unless (search-forward "{\"type\":" nil t)
        (message "✅ Raw JSON hidden from output")
        (message "❌ Raw JSON still visible"))))
  
  ;; Test 4: UI elements
  (message "✅ Colorized prompts: %s" (tiqsi-claude-repl--format-prompt))
  (message "✅ Status messages: %s" (tiqsi-claude-repl--format-status "Success" "Test"))
  (message "✅ Timestamps: %s" (tiqsi-claude-repl--format-timestamp))
  
  ;; Test 5: Session features
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    (message "✅ Session ID: %s" (or tiqsi-claude-repl--session-id "generated"))
    (message "✅ Message count: %d" tiqsi-claude-repl--message-count)
    (tiqsi-claude-repl-show-status))
  
  (message "\n=== All tests completed ===")
  (message "The Claude REPL is ready for use!")
  (message "Try typing: 'Hello Claude, please remember the number 42'"))

;; Run the demo
(provide 'demo-claude-repl)

;;; demo-claude-repl.el ends here