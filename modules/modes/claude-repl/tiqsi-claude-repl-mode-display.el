;;; tiqsi-claude-repl-mode-display.el --- Simple mode display -*- lexical-binding: t -*-

;;; Commentary:
;; Simple, elegant permission mode display at bottom of buffer

;;; Code:

;; Faces for mode display
(defface tiqsi-claude-repl-mode-ask
  '((t :background "#8B0000" :foreground "#FFFFFF" :weight bold))
  "Face for ASK mode indicator.")

(defface tiqsi-claude-repl-mode-auto
  '((t :background "#228B22" :foreground "#FFFFFF" :weight bold))
  "Face for AUTO-ACCEPT mode indicator.")

(defface tiqsi-claude-repl-mode-bypass
  '((t :background "#FF8C00" :foreground "#000000" :weight bold))
  "Face for BYPASS mode indicator.")

;; Mode line segment
(defvar tiqsi-claude-repl-mode-line-string nil)

(defun tiqsi-claude-repl--update-mode-line ()
  "Update mode line with current permission mode."
  (setq tiqsi-claude-repl-mode-line-string
        (let* ((mode (if (boundp 'tiqsi-claude-repl-permission-mode)
                         tiqsi-claude-repl-permission-mode
                       'ask))
               (text (pcase mode
                       ('ask " ASK ")
                       ('auto-accept " AUTO ")
                       ('bypass " BYPASS ")))
               (face (pcase mode
                       ('ask 'tiqsi-claude-repl-mode-ask)
                       ('auto-accept 'tiqsi-claude-repl-mode-auto)
                       ('bypass 'tiqsi-claude-repl-mode-bypass))))
          (propertize text 'face face))))

;; Setup function
(defun tiqsi-claude-repl--setup-mode-display ()
  "Setup mode display for Claude REPL."
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    ;; Add to mode line
    (unless (memq 'tiqsi-claude-repl-mode-line-string mode-line-format)
      (setq-local mode-line-format
                  (append mode-line-format
                          '(" " (:eval tiqsi-claude-repl-mode-line-string)))))
    ;; Initial update
    (tiqsi-claude-repl--update-mode-line)
    (force-mode-line-update)))

;; Hook into mode changes
(defun tiqsi-claude-repl--on-mode-change ()
  "Update display when mode changes."
  (tiqsi-claude-repl--update-mode-line)
  (force-mode-line-update)
  ;; Simple message
  (let ((mode-str (pcase tiqsi-claude-repl-permission-mode
                    ('ask "ASK mode - Claude will request permission")
                    ('auto-accept "AUTO mode - Permissions granted automatically")
                    ('bypass "BYPASS mode - Unrestricted tool access"))))
    (message "Permission mode: %s" mode-str)))

;; Hook into Claude REPL mode
(add-hook 'tiqsi-claude-repl-mode-hook #'tiqsi-claude-repl--setup-mode-display)

;; Hook into permission mode changes
(when (fboundp 'tiqsi-claude-repl-cycle-permission-mode)
  (advice-add 'tiqsi-claude-repl-cycle-permission-mode :after
              (lambda (&rest _) (tiqsi-claude-repl--on-mode-change))))

(when (fboundp 'tiqsi-claude-repl-set-permission-mode)
  (advice-add 'tiqsi-claude-repl-set-permission-mode :after
              (lambda (&rest _) (tiqsi-claude-repl--on-mode-change))))

;; Initialize for existing buffers
(dolist (buffer (buffer-list))
  (with-current-buffer buffer
    (when (eq major-mode 'tiqsi-claude-repl-mode)
      (tiqsi-claude-repl--setup-mode-display))))

(provide 'tiqsi-claude-repl-mode-display)

;;; tiqsi-claude-repl-mode-display.el ends here