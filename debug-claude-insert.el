;;; debug-claude-insert.el --- Debug Claude text insertion -*- lexical-binding: t -*-

(defun debug-claude-insert ()
  "Debug why Claude responses aren't showing."
  (interactive)
  
  ;; Create a test buffer
  (with-current-buffer (get-buffer-create "*debug-claude*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    
    ;; Insert initial content
    (insert "λ test\n")
    (insert "⏳ Thinking...\n\n")
    
    ;; Set up state like the real REPL
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start (point-marker))
    (setq-local tiqsi-claude-repl-show-thinking t)
    
    (message "Initial buffer:\n%s" (buffer-string))
    (message "Output start marker at: %d" (marker-position tiqsi-claude-repl--output-start))
    
    ;; Simulate Claude's response
    (let ((json-line "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_test\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! I can see your message.\"}]}}"))
      
      ;; Create a mock process
      (let ((mock-process (make-process
                          :name "debug-test"
                          :buffer (current-buffer)
                          :command '("echo" "test"))))
        
        ;; Call the process filter
        (tiqsi-claude-repl--process-filter mock-process (concat json-line "\n"))
        
        (message "\nAfter process filter:")
        (message "Buffer size: %d" (buffer-size))
        (message "Buffer contents:\n%s" (buffer-string))
        
        ;; Check if post-processing helps
        (when tiqsi-claude-repl--output-start
          (message "\nApplying post-processing from %d to %d..." 
                   (marker-position tiqsi-claude-repl--output-start) 
                   (point-max))
          (tiqsi-claude-repl--post-process-response 
           (marker-position tiqsi-claude-repl--output-start) 
           (point-max)))
        
        (message "\nAfter post-processing:")
        (message "Buffer contents:\n%s" (buffer-string))
        
        ;; Clean up
        (delete-process mock-process)))
    
    ;; Display the buffer
    (display-buffer (current-buffer))))

(provide 'debug-claude-insert)