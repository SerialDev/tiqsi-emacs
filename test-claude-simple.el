;;; test-claude-simple.el --- Simple test for Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-simple ()
  "Simple test of Claude REPL functionality."
  (interactive)
  
  ;; Start REPL
  (message "\n=== Starting Claude REPL Test ===")
  (tiqsi-claude-repl-start)
  
  ;; Get the buffer
  (let ((repl-buffer (tiqsi-claude-repl--get-or-create-buffer)))
    (with-current-buffer repl-buffer
      ;; Show buffer info
      (message "Buffer: %s" (buffer-name))
      (message "Mode: %s" major-mode)
      (message "Session ID: %s" (or tiqsi-claude-repl--session-id "none"))
      
      ;; Test JSON processing
      (message "\n=== Testing JSON Processing ===")
      (let ((test-json "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello from Claude!\"}]}}")
            (mock-process (make-process
                          :name "test-claude"
                          :buffer (current-buffer)
                          :command '("echo" "test"))))
        (setq-local tiqsi-claude-repl--json-buffer "")
        (setq-local tiqsi-claude-repl--json-processing-enabled nil)
        (setq-local tiqsi-claude-repl--output-start (point-marker))
        
        ;; Process the JSON
        (tiqsi-claude-repl--process-filter mock-process (concat test-json "\n"))
        (delete-process mock-process)
        
        ;; Check output
        (goto-char (point-min))
        (if (search-forward "Hello from Claude!" nil t)
            (message "✅ JSON processing works!")
          (message "❌ JSON processing failed"))
        
        ;; Show buffer contents
        (message "\n=== Buffer Contents ===")
        (message "%s" (buffer-substring-no-properties (point-min) (min (point-max) 500)))))
    
    ;; Display the buffer
    (display-buffer repl-buffer)
    (message "\n=== Test Complete ===")))

(defun test-claude-markdown ()
  "Test markdown rendering safely."
  (interactive)
  (with-current-buffer (get-buffer-create "*Claude Markdown Test*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    ;; Insert some test content
    (insert "Testing markdown rendering:\n\n")
    (insert "This is **bold** text.\n")
    (insert "This is *italic* text.\n")
    (insert "This is inline code: `print('hello')`\n\n")
    
    ;; Apply formatting to the whole buffer
    (condition-case err
        (progn
          (tiqsi-claude-repl--apply-markdown-formatting (point-min) (point-max))
          (message "✅ Markdown formatting applied successfully"))
      (error (message "❌ Markdown formatting error: %s" err)))
    
    (display-buffer (current-buffer))))

;; Run the tests
(provide 'test-claude-simple)

;;; test-claude-simple.el ends here