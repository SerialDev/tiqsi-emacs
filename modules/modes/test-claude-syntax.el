;;; test-claude-syntax.el --- Test Claude syntax -*- lexical-binding: t -*-

;; Simple syntax test without dependencies
(message "Testing Claude REPL syntax...")

;; Test each file for syntax errors
(let ((files '("modes-claude.el"
               "claude-repl/tiqsi-claude-repl.el"
               "claude-repl/tiqsi-claude-repl-ui.el"
               "claude-repl/tiqsi-claude-repl-core.el"
               "claude-repl/tiqsi-claude-repl-features.el"
               "claude-repl/tiqsi-claude-repl-init-fix.el"
               "claude-repl/tiqsi-claude-repl-complete-fix.el"
               "claude-repl/tiqsi-claude-repl-permissions.el"
               "claude-repl/tiqsi-claude-repl-tool-helpers.el"
               "claude-repl/tiqsi-claude-repl-mode-display.el"
               "claude-repl/tiqsi-claude-repl-rendering-fix.el"
               "claude-repl/tiqsi-claude-repl-simple-thinking-fix.el")))
  (dolist (file files)
    (condition-case err
        (progn
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            ;; Try to parse the whole file
            (while (not (eobp))
              (forward-sexp)))
          (message "✓ %s - OK" file))
      (error
       (message "✗ %s - ERROR: %s" file (error-message-string err))))))

(message "Syntax test complete.")
EOF < /dev/null