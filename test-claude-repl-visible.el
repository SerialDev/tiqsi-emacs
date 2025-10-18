;;; test-claude-repl-visible.el --- Test why responses aren't visible -*- lexical-binding: t -*-

(require 'tiqsi-claude-repl)

(defun test-claude-repl-visibility ()
  "Test Claude REPL to understand why responses aren't visible."
  (interactive)
  
  ;; Kill any existing Claude REPL buffer
  (when-let ((existing (get-buffer "*Claude REPL (tiqsi-emacs)*")))
    (kill-buffer existing))
  
  ;; Start new session
  (tiqsi-claude-repl-start)
  
  ;; Wait for initialization
  (sit-for 1)
  
  (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
    ;; Enable debug mode
    (setq-local tiqsi-claude-repl-show-thinking t)
    
    ;; Send a simple message
    (goto-char (point-max))
    (insert "Say hello and confirm you can see this")
    
    ;; Before sending, let's check buffer state
    (message "\n=== BEFORE SENDING ===")
    (message "Buffer size: %d" (buffer-size))
    (message "Buffer contents:\n%s" (buffer-string))
    (message "Point at: %d" (point))
    
    ;; Send the input
    (tiqsi-claude-repl-send-input)
    
    ;; Wait for response
    (message "\n=== WAITING FOR RESPONSE ===")
    (sit-for 10) ; Wait longer for Claude to respond
    
    ;; Check buffer state after response
    (message "\n=== AFTER RESPONSE ===")
    (message "Buffer size: %d" (buffer-size))
    (message "Buffer contents:\n%s" (buffer-string))
    (message "Point at: %d" (point))
    
    ;; Check for any invisible text
    (goto-char (point-min))
    (let ((invisible-regions 0))
      (while (< (point) (point-max))
        (when (get-text-property (point) 'invisible)
          (setq invisible-regions (1+ invisible-regions)))
        (forward-char 1))
      (message "Found %d invisible character positions" invisible-regions))
    
    ;; Check for any text properties that might hide text
    (goto-char (point-min))
    (let ((props-found '()))
      (while (< (point) (point-max))
        (let ((props (text-properties-at (point))))
          (when props
            (dolist (prop props)
              (unless (memq prop props-found)
                (push prop props-found)))))
        (forward-char 1))
      (message "Text properties found: %S" props-found))
    
    ;; Display the buffer
    (display-buffer (current-buffer))))

;; Run the test
(provide 'test-claude-repl-visible)

;;; test-claude-repl-visible.el ends here