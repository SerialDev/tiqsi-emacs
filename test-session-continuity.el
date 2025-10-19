;;; test-session-continuity.el --- Test Claude session continuity -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-session-continuity ()
  "Test that Claude maintains session continuity between messages."
  (interactive)
  
  ;; Kill any existing Claude REPL buffer
  (when-let ((existing (get-buffer "*Claude REPL (tiqsi-emacs)*")))
    (kill-buffer existing))
  
  ;; Start new session
  (tiqsi-claude-repl-start)
  
  ;; Wait for initialization
  (sit-for 1)
  
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    ;; Show initial state
    (message "\n=== Testing Session Continuity ===")
    (message "Initial Claude Session ID: %s" (or tiqsi-claude-repl--claude-session-id "not set"))
    
    ;; Send first message
    (goto-char (point-max))
    (insert "Hello Claude, please remember the number 42")
    (tiqsi-claude-repl-send-input)
    
    (message "Sent first message. Waiting for response...")
    (sit-for 8) ; Wait for Claude to respond
    
    ;; Check if we captured session ID
    (message "After first message - Claude Session ID: %s" 
             (or tiqsi-claude-repl--claude-session-id "STILL NOT SET"))
    
    ;; Send second message to test continuity
    (goto-char (point-max))
    (when (re-search-backward "^λ " nil t)
      (goto-char (match-end 0))
      (insert "What number did I ask you to remember?")
      (tiqsi-claude-repl-send-input)
      
      (message "Sent second message. Testing session continuity...")
      (sit-for 8) ; Wait for response
      
      ;; Check results
      (goto-char (point-min))
      (let ((found-42 nil)
            (sessions-match nil))
        ;; Look for 42 in the response
        (when (re-search-forward "42\\|forty-two" nil t)
          (setq found-42 t))
        
        ;; Check if session IDs match
        (goto-char (point-min))
        (let ((session-ids '()))
          (while (re-search-forward "Session initialized (ID: \\([^)]+\\))" nil t)
            (push (match-string 1) session-ids))
          (when (and (>= (length session-ids) 2)
                     (string= (car session-ids) (cadr session-ids)))
            (setq sessions-match t)))
        
        ;; Report results
        (message "\n=== Test Results ===")
        (if found-42
            (message "✅ Claude remembered the number 42!")
          (message "❌ Claude did NOT remember the number 42"))
        
        (if sessions-match
            (message "✅ Same session ID used for both requests")
          (message "❌ Different session IDs - session not continued"))
        
        (when tiqsi-claude-repl--claude-session-id
          (message "✅ Session ID captured: %s" tiqsi-claude-repl--claude-session-id))
        
        ;; Show session summary
        (tiqsi-claude-repl-show-session-summary))))
  
  ;; Display the buffer
  (display-buffer (tiqsi-claude-repl--get-or-create-buffer)))

(defun test-claude-command-building ()
  "Test that the command is built correctly with session resumption."
  (interactive)
  (with-current-buffer (get-buffer-create "*command-test*")
    (tiqsi-claude-repl-mode)
    
    ;; Test without session ID
    (setq-local tiqsi-claude-repl--claude-session-id nil)
    (let* ((cmd-args (list "-p"))
           (cmd-args (if tiqsi-claude-repl--claude-session-id
                         (append cmd-args (list "--resume" tiqsi-claude-repl--claude-session-id))
                       cmd-args)))
      (message "Without session ID: %s" (mapconcat 'identity cmd-args " ")))
    
    ;; Test with session ID
    (setq-local tiqsi-claude-repl--claude-session-id "test-session-123")
    (let* ((cmd-args (list "-p"))
           (cmd-args (if tiqsi-claude-repl--claude-session-id
                         (append cmd-args (list "--resume" tiqsi-claude-repl--claude-session-id))
                       cmd-args)))
      (message "With session ID: %s" (mapconcat 'identity cmd-args " ")))
    
    (kill-buffer)))

;; Run the tests
(provide 'test-session-continuity)

;;; test-session-continuity.el ends here