;;; test-claude-visual.el --- Visual test for Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-visual-demo ()
  "Demonstrate the Claude REPL with simulated responses."
  (interactive)
  
  ;; Kill existing buffer if any
  (when-let ((existing (get-buffer "*Claude REPL Visual Test*")))
    (kill-buffer existing))
  
  ;; Create test buffer
  (with-current-buffer (get-buffer-create "*Claude REPL Visual Test*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    ;; Insert header
    (tiqsi-claude-repl--insert-header)
    
    ;; First interaction
    (insert (tiqsi-claude-repl--format-prompt))
    (insert "hi we are testing you remember 22")
    (insert "\n")
    (insert (tiqsi-claude-repl--colorize "⏳ Thinking... [0.1s]" 'tiqsi-claude-repl-thinking))
    (insert "\n\n")
    
    ;; Simulate the process filter receiving chunks
    (let ((chunks
           '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"b3ec24ec-bc42-45b8-b19e-e115bbb46d51\"}\n"
             "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"I'll help you test. Yes, I can see your message where you mentioned \\\"remember 22\\\". How can I assist you with testing today?\"}]}}\n"
             "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":4542}\n")))
      
      ;; Initialize processing state
      (setq-local tiqsi-claude-repl--json-buffer "")
      (setq-local tiqsi-claude-repl--json-processing-enabled nil)
      (setq-local tiqsi-claude-repl--output-start (point-marker))
      
      ;; Process each chunk
      (dolist (chunk chunks)
        (let ((mock-process (make-process
                            :name "visual-test"
                            :buffer (current-buffer)
                            :command '("echo" "test"))))
          (tiqsi-claude-repl--process-filter mock-process chunk)
          (delete-process mock-process))))
    
    ;; Add completion message
    (goto-char (point-max))
    (insert "\n" (tiqsi-claude-repl--colorize "✅ Completed in 5.4s" 'tiqsi-claude-repl-success) "\n\n")
    
    ;; Add new prompt
    (insert (tiqsi-claude-repl--format-prompt))
    
    ;; Second interaction to test continuity
    (insert "What number did I ask you to remember?")
    (insert "\n")
    (insert (tiqsi-claude-repl--colorize "⏳ Thinking... [0.2s]" 'tiqsi-claude-repl-thinking))
    (insert "\n\n")
    
    ;; Simulate second response
    (let ((response-chunks
           '("{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"You asked me to remember the number 22.\"}]}}\n")))
      
      (setq-local tiqsi-claude-repl--output-start (point-marker))
      
      (dolist (chunk response-chunks)
        (let ((mock-process (make-process
                            :name "visual-test-2"
                            :buffer (current-buffer)
                            :command '("echo" "test"))))
          (tiqsi-claude-repl--process-filter mock-process chunk)
          (delete-process mock-process))))
    
    ;; Add completion
    (goto-char (point-max))
    (insert "\n" (tiqsi-claude-repl--colorize "✅ Completed in 2.1s" 'tiqsi-claude-repl-success) "\n\n")
    (insert (tiqsi-claude-repl--format-prompt))
    
    ;; Display the buffer
    (display-buffer (current-buffer))
    (set-window-point (get-buffer-window (current-buffer)) (point-max))
    
    (message "Visual test completed. Check *Claude REPL Visual Test* buffer for results.")))

(defun test-claude-code-highlighting ()
  "Test code highlighting in Claude responses."
  (interactive)
  
  (with-current-buffer (get-buffer-create "*Claude Code Highlight Test*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    (insert "Testing code highlighting:\n\n")
    
    ;; Test Python code block
    (let ((start (point)))
      (insert "Here's a Python function:\n\n```python\ndef factorial(n):\n    if n <= 1:\n        return 1\n    return n * factorial(n - 1)\n\nprint(factorial(5))  # Output: 120\n```\n\n")
      (tiqsi-claude-repl--post-process-response start (point)))
    
    ;; Test inline code
    (let ((start (point)))
      (insert "Use `print()` to display output and `len()` to get length.\n\n")
      (tiqsi-claude-repl--apply-markdown-formatting start (point)))
    
    ;; Test multiple languages
    (let ((start (point)))
      (insert "JavaScript example:\n\n```javascript\nconst greet = (name) => {\n    console.log(`Hello, ${name}!`);\n};\n```\n\n")
      (tiqsi-claude-repl--post-process-response start (point)))
    
    (display-buffer (current-buffer))
    (message "Code highlighting test completed.")))

(defun test-claude-all-features ()
  "Test all Claude REPL features comprehensively."
  (interactive)
  
  (message "\n=== Starting Comprehensive Claude REPL Tests ===\n")
  
  ;; Test 1: Visual demonstration
  (test-claude-visual-demo)
  (sit-for 2)
  
  ;; Test 2: Code highlighting
  (test-claude-code-highlighting)
  (sit-for 2)
  
  ;; Test 3: Session features
  (with-current-buffer "*Claude REPL Visual Test*"
    ;; Test session summary
    (tiqsi-claude-repl-show-session-summary)
    
    ;; Test status display
    (tiqsi-claude-repl-show-status)
    
    ;; Test history features
    (when tiqsi-claude-repl-save-history
      (tiqsi-claude-repl--save-conversation)
      (message "✅ Conversation saved to history")))
  
  (message "\n=== All tests completed! ===")
  (message "Check the following buffers:")
  (message "  - *Claude REPL Visual Test*")
  (message "  - *Claude Code Highlight Test*"))

(provide 'test-claude-visual)

;;; test-claude-visual.el ends here