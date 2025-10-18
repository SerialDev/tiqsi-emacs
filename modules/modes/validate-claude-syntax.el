;;; validate-claude-syntax.el --- Validate Claude REPL syntax -*- lexical-binding: t -*-

;; Check parentheses balance
(defun check-parens-in-file (file)
  "Check parentheses balance in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (condition-case err
        (progn
          (check-parens)
          (message "✅ %s: Parentheses balanced" (file-name-nondirectory file)))
      (error 
       (message "❌ %s: Parentheses error - %s" 
               (file-name-nondirectory file)
               (error-message-string err))))))

;; Check all files
(dolist (file '("claude-repl/tiqsi-claude-repl.el"
                "claude-repl/tiqsi-claude-repl-ui.el"
                "claude-repl/tiqsi-claude-repl-core.el"
                "claude-repl/tiqsi-claude-repl-features.el"
                "modes-claude.el"))
  (check-parens-in-file file))

(message "\n✅ All syntax checks passed!")