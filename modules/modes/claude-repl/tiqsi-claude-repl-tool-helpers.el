;;; tiqsi-claude-repl-tool-helpers.el --- Helper commands for tool use -*- lexical-binding: t -*-

;;; Commentary:
;; Commands to help with tool execution and permissions

;;; Code:

;; Quick command to grant permission
(defun tiqsi-claude-repl-grant-permission ()
  "Send a message granting permission for tool use."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (goto-char (point-max))
    (insert "yes, you have permission to use tools. proceed.")
    (tiqsi-claude-repl-send-input)))

;; Command to execute with explicit permission
(defun tiqsi-claude-repl-exec-with-permission (command)
  "Execute COMMAND with explicit permission grant."
  (interactive "sCommand to execute: ")
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (goto-char (point-max))
    (insert (format "You have my permission to execute: %s" command))
    (tiqsi-claude-repl-send-input)))

;; Direct execution commands
(defun tiqsi-claude-repl-exec-gs ()
  "Execute git status with permission."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (goto-char (point-max))
    (insert "exec ! git status")
    (tiqsi-claude-repl-send-input)
    ;; Auto-grant permission after a short delay
    (when (eq tiqsi-claude-repl-permission-mode 'auto-accept)
      (run-with-timer 0.5 nil #'tiqsi-claude-repl-grant-permission))))

(defun tiqsi-claude-repl-exec-ls ()
  "Execute ls with permission."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (goto-char (point-max))
    (insert "exec ! ls -la")
    (tiqsi-claude-repl-send-input)))

;; Add common tool shortcuts to the mode map
(with-eval-after-load 'tiqsi-claude-repl-core
  (when (boundp 'tiqsi-claude-repl-mode-map)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c p") 'tiqsi-claude-repl-grant-permission)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c x") 'tiqsi-claude-repl-exec-with-permission)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c g s") 'tiqsi-claude-repl-exec-gs)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c l s") 'tiqsi-claude-repl-exec-ls)))

;; Helper to detect when Claude is asking for permission
(defun tiqsi-claude-repl--detect-permission-request (output)
  "Check if OUTPUT contains a permission request."
  (or (string-match-p "grant permission" output)
      (string-match-p "need.*permission" output)
      (string-match-p "permission.*required" output)
      (string-match-p "please.*allow" output)))

;; Auto-grant permission when detected (if in auto-accept mode)
(defun tiqsi-claude-repl--auto-grant-if-needed (process output)
  "Auto-grant permission if detected and in auto-accept mode."
  (when (and (eq tiqsi-claude-repl-permission-mode 'auto-accept)
             (tiqsi-claude-repl--detect-permission-request output))
    (run-with-timer 0.5 nil
                    (lambda ()
                      (with-current-buffer (process-buffer process)
                        (when (eq major-mode 'tiqsi-claude-repl-mode)
                          (message "Auto-granting permission...")
                          (tiqsi-claude-repl-grant-permission)))))))

;; Hook into the process filter to detect permission requests
(defun tiqsi-claude-repl--enhanced-filter-with-auto-grant (orig-fun process output)
  "Enhanced filter that can auto-grant permissions."
  ;; Call original filter
  (funcall orig-fun process output)
  ;; Check for permission requests
  (tiqsi-claude-repl--auto-grant-if-needed process output))

(when (fboundp 'tiqsi-claude-repl--working-process-filter)
  (advice-add 'tiqsi-claude-repl--working-process-filter :around
              #'tiqsi-claude-repl--enhanced-filter-with-auto-grant))

(provide 'tiqsi-claude-repl-tool-helpers)

;;; tiqsi-claude-repl-tool-helpers.el ends here