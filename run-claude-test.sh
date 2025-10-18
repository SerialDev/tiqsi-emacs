#!/bin/bash
# Test Claude REPL functionality

emacs -q -l init-lite.el --eval '
(progn
  (require (quote tiqsi-claude-repl))
  (with-current-buffer (get-buffer-create "*test-claude*")
    (tiqsi-claude-repl-mode)
    (erase-buffer)
    (insert "λ test\n⏳ Thinking...\n\n")
    (setq-local tiqsi-claude-repl--json-buffer "")
    (setq-local tiqsi-claude-repl--json-processing-enabled nil)
    (setq-local tiqsi-claude-repl--output-start nil)
    (setq-local tiqsi-claude-repl-show-thinking t)
    
    (let ((mock-process (make-process
                        :name "test"
                        :buffer (current-buffer)
                        :command (quote ("echo" "test")))))
      ;; Test assistant message
      (tiqsi-claude-repl--process-filter mock-process 
        "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_test\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! Claude is working.\"}]}}\n")
      
      (message "=== RESULT ===")
      (message "%s" (buffer-string))
      
      ;; Check if text exists
      (goto-char (point-min))
      (if (search-forward "Hello! Claude is working." nil t)
          (message "SUCCESS: Claude response found!")
        (message "FAILURE: Claude response NOT found!"))
      
      (delete-process mock-process))))
' -batch 2>&1 | grep -E "RESULT|SUCCESS|FAILURE|Hello"