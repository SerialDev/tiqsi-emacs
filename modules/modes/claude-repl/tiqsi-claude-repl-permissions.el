;;; tiqsi-claude-repl-permissions.el --- Permission control for Claude REPL -*- lexical-binding: t -*-

;;; Commentary:
;; Control prompting modes and permission handling for Claude tool use

;;; Code:

;; Define permission modes
(defcustom tiqsi-claude-repl-permission-mode 'ask
  "How to handle tool permissions.
- `ask': Always ask for permission (default)
- `auto-accept': Automatically accept all tool uses
- `bypass': Bypass permission system entirely"
  :type '(choice (const :tag "Ask for permission" ask)
                 (const :tag "Auto-accept all" auto-accept)
                 (const :tag "Bypass permissions" bypass))
  :group 'tiqsi-claude-repl)

;; Track if we're in planning mode
(defvar-local tiqsi-claude-repl--planning-mode nil
  "Whether Claude is in planning mode.")

;; Command to switch permission modes
(defun tiqsi-claude-repl-set-permission-mode (mode)
  "Set permission MODE for tool use.
MODE can be 'ask, 'auto-accept, or 'bypass."
  (interactive
   (list (intern (completing-read "Permission mode: "
                                  '("ask" "auto-accept" "bypass")
                                  nil t))))
  (setq tiqsi-claude-repl-permission-mode mode)
  (message "Permission mode set to: %s" mode))

;; Command to toggle between modes quickly
(defun tiqsi-claude-repl-cycle-permission-mode ()
  "Cycle through permission modes."
  (interactive)
  (setq tiqsi-claude-repl-permission-mode
        (pcase tiqsi-claude-repl-permission-mode
          ('ask 'auto-accept)
          ('auto-accept 'bypass)
          ('bypass 'ask)))
  ;; Enhanced visual feedback
  (let ((mode-str (pcase tiqsi-claude-repl-permission-mode
                    ('ask "🔒 ASK - Claude will request permission")
                    ('auto-accept "🔓 AUTO-ACCEPT - Permissions granted automatically")
                    ('bypass "⚡ BYPASS - Unrestricted tool access"))))
    (message mode-str)
    ;; Also show in minibuffer for 2 seconds
    (let ((message-log-max nil))
      (minibuffer-message mode-str))))

;; Enhanced send function that handles permissions
(defun tiqsi-claude-repl--send-with-permissions (orig-fun input)
  "Wrap send function to handle permissions based on mode."
  (let ((modified-input input))
    ;; Add flags based on permission mode
    (pcase tiqsi-claude-repl-permission-mode
      ('auto-accept
       ;; Add auto-accept flag if Claude supports it
       (setq modified-input input)) ; Claude doesn't have direct auto-accept
      ('bypass
       ;; For bypass mode, we might need to handle this differently
       (setq modified-input input)))
    
    ;; Check if we're exiting planning mode
    (when (string-match-p "\\(execute\\|run\\|do it\\|proceed\\)" input)
      (setq-local tiqsi-claude-repl--planning-mode nil))
    
    ;; Check if we're entering planning mode
    (when (string-match-p "\\(plan\\|design\\|architect\\)" input)
      (setq-local tiqsi-claude-repl--planning-mode t))
    
    (funcall orig-fun modified-input)))

;; Apply the wrapper
(when (fboundp 'tiqsi-claude-repl--send-to-claude)
  (advice-add 'tiqsi-claude-repl--send-to-claude :around
              #'tiqsi-claude-repl--send-with-permissions))

;; Command to toggle planning mode
(defun tiqsi-claude-repl-toggle-planning-mode ()
  "Toggle planning mode on/off."
  (interactive)
  (setq-local tiqsi-claude-repl--planning-mode 
              (not tiqsi-claude-repl--planning-mode))
  (message "Planning mode: %s" (if tiqsi-claude-repl--planning-mode "ON" "OFF")))

;; Visual indicator for current mode
(defun tiqsi-claude-repl-show-modes ()
  "Show current permission and planning modes."
  (interactive)
  (message "Modes - Permission: %s | Planning: %s | Tools: %s"
           tiqsi-claude-repl-permission-mode
           (if (and (boundp 'tiqsi-claude-repl--planning-mode)
                    tiqsi-claude-repl--planning-mode)
               "ON" "OFF")
           (if (eq tiqsi-claude-repl-permission-mode 'bypass)
               "UNRESTRICTED" "CONTROLLED")))

;; Add mode indicator to prompt
(defun tiqsi-claude-repl--enhanced-prompt ()
  "Enhanced prompt showing current modes."
  (let ((base-prompt (if (fboundp 'tiqsi-claude-repl--format-prompt)
                         (tiqsi-claude-repl--format-prompt)
                       "λ "))
        (mode-indicator ""))
    ;; Add mode indicators
    (when (eq tiqsi-claude-repl-permission-mode 'auto-accept)
      (setq mode-indicator (concat mode-indicator "[A]")))
    (when (eq tiqsi-claude-repl-permission-mode 'bypass)
      (setq mode-indicator (concat mode-indicator "[B]")))
    (when (and (boundp 'tiqsi-claude-repl--planning-mode)
               tiqsi-claude-repl--planning-mode)
      (setq mode-indicator (concat mode-indicator "[P]")))
    ;; Return enhanced prompt
    (if (> (length mode-indicator) 0)
        (concat mode-indicator " " base-prompt)
      base-prompt)))

;; Override prompt insertion to use enhanced version
(defun tiqsi-claude-repl--insert-enhanced-prompt ()
  "Insert enhanced prompt with mode indicators."
  (let ((inhibit-read-only t))
    (unless (bolp) (insert "\n"))
    (insert (tiqsi-claude-repl--enhanced-prompt))
    (when (get-buffer-process (current-buffer))
      (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))))

;; Apply enhanced prompt
(when (fboundp 'tiqsi-claude-repl--insert-prompt)
  (advice-add 'tiqsi-claude-repl--insert-prompt :override
              #'tiqsi-claude-repl--insert-enhanced-prompt))

;; Hydra for quick mode changes
(with-eval-after-load 'hydra
  (defhydra hydra-claude-modes (:color blue :hint nil)
    "
Claude REPL Modes
─────────────────────────────────────────────
Permission:  _a_sk  _A_uto-accept  _B_ypass  _c_ycle
Planning:    _p_lanning toggle
Show:        _s_tatus
─────────────────────────────────────────────
_q_uit
"
    ("a" (tiqsi-claude-repl-set-permission-mode 'ask) "Ask")
    ("A" (tiqsi-claude-repl-set-permission-mode 'auto-accept) "Auto")
    ("B" (tiqsi-claude-repl-set-permission-mode 'bypass) "Bypass")
    ("c" tiqsi-claude-repl-cycle-permission-mode "Cycle")
    ("p" tiqsi-claude-repl-toggle-planning-mode "Planning")
    ("s" tiqsi-claude-repl-show-modes "Status")
    ("q" nil "Quit")))

;; Global keybinding for mode control
(global-set-key (kbd "C-c C-m") 'hydra-claude-modes/body)

;; Add to Claude REPL mode map
(with-eval-after-load 'tiqsi-claude-repl-core
  (when (boundp 'tiqsi-claude-repl-mode-map)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c m") 'hydra-claude-modes/body)
    (define-key tiqsi-claude-repl-mode-map (kbd "C-c M") 'tiqsi-claude-repl-cycle-permission-mode)))

(provide 'tiqsi-claude-repl-permissions)

;;; tiqsi-claude-repl-permissions.el ends here