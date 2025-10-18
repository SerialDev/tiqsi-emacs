;;; test-claude-hydra-interactive.el --- Test Claude hydra interactively -*- lexical-binding: t -*-

;; Instructions:
;; 1. Load this file with: emacs -q -l test-claude-hydra-interactive.el
;; 2. Press M-c to test the hydra
;; 3. Or run M-x tiqsi-claude-hydra

;; Set up minimal environment
(setq tiqsi-core (expand-file-name "~/Documents/workdir/personal/repos/tiqsi-emacs/"))

(defun load-expand (filename)
  "Load FILENAME relative to tiqsi-core directory."
  (load (expand-file-name filename tiqsi-core)))

;; Initialize straight.el
(setq straight-base-dir (expand-file-name "~/.emacs.d/"))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load hydra dependencies
(straight-use-package 'lv)
(straight-use-package 'hydra)
(require 'lv)
(require 'hydra)

;; Load Claude mode
(load-expand "modules/modes/modes-claude.el")

;; Check if hydra is available
(if (fboundp 'hydra-claude/body)
    (progn
      (message "SUCCESS: Claude hydra loaded! Press M-c to test it.")
      ;; Show a helpful message in echo area
      (run-with-timer 1 nil 
                      (lambda () 
                        (message "Claude hydra ready! Press M-c or run M-x tiqsi-claude-hydra"))))
  (error "ERROR: Claude hydra failed to load!"))

;; Make sure the key binding is set
(global-set-key (kbd "M-c") 'tiqsi-claude-hydra)

(provide 'test-claude-hydra-interactive)
;;; test-claude-hydra-interactive.el ends here