;;; test-claude-current-state.el --- Test current state of Claude REPL -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-current-json-parsing ()
  "Test if JSON parsing is working in current state."
  (interactive)
  
  (with-current-buffer (get-buffer-create "*test-json-parse*")
    (erase-buffer)
    (tiqsi-claude-repl-mode)
    
    ;; Set up state
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start nil)
    (setq-local tiqsi-claude-repl-show-thinking t)
    
    ;; Insert initial content
    (insert "λ test\n⏳ Thinking...\n\n")
    
    ;; Create mock process
    (let ((mock-process (make-process
                        :name "test-json"
                        :buffer (current-buffer)
                        :command '("echo" "test"))))
      
      ;; Test system message
      (message "\n=== Testing System Message ===")
      (tiqsi-claude-repl--process-filter mock-process 
        "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\"}\n")
      (message "Buffer after system:\n%s" (buffer-string))
      
      ;; Test assistant message
      (message "\n=== Testing Assistant Message ===")
      (tiqsi-claude-repl--process-filter mock-process 
        "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_test\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! This is a test response.\"}]}}\n")
      (message "Buffer after assistant:\n%s" (buffer-string))
      
      ;; Test if text is actually there but invisible
      (message "\n=== Checking for invisible text ===")
      (goto-char (point-min))
      (let ((text-found nil))
        (while (and (not text-found) (< (point) (point-max)))
          (when (looking-at "Hello!")
            (setq text-found t)
            (message "Found 'Hello!' at position %d" (point)))
          (forward-char 1))
        (unless text-found
          (message "Text 'Hello!' not found in buffer")))
      
      ;; Clean up
      (delete-process mock-process))
    
    (display-buffer (current-buffer))))

(provide 'test-claude-current-state)