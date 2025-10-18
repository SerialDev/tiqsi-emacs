;;; test-claude-loading.el --- Test Claude loading issues -*- lexical-binding: t -*-

;; Diagnostic script to test Claude REPL loading

;; First, set up the Tiqsi environment
(setq tiqsi-core (expand-file-name "~/Documents/workdir/personal/repos/tiqsi-emacs/"))

(defun load-expand (filename)
  "Load FILENAME relative to tiqsi-core directory."
  (let ((full-path (expand-file-name filename tiqsi-core)))
    (message "load-expand: Attempting to load %s" full-path)
    (if (file-exists-p full-path)
        (progn
          (message "load-expand: File exists, loading...")
          (load full-path))
      (message "load-expand: ERROR - File not found: %s" full-path))))

;; Initialize straight.el first
(setq straight-base-dir (expand-file-name "~/.emacs.d/"))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (message "Bootstrapping straight.el...")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)))

;; Load dependencies
(straight-use-package 'lv)
(straight-use-package 'hydra)
(require 'lv)
(require 'hydra)

(message "\n=== Testing hydra availability ===")
(message "featurep 'hydra: %s" (featurep 'hydra))
(message "fboundp 'defhydra: %s" (fboundp 'defhydra))

;; Test loading the Claude REPL files
(message "\n=== Testing Claude REPL loading ===")

;; Check if files exist
(let ((claude-repl-file (expand-file-name "modules/modes/claude-repl/tiqsi-claude-repl.el" tiqsi-core)))
  (message "Claude REPL file exists: %s" (file-exists-p claude-repl-file)))

;; Try loading modes-claude.el
(message "\n=== Loading modes-claude.el ===")
(condition-case err
    (progn
      (load-expand "modules/modes/modes-claude.el")
      (message "Successfully loaded modes-claude.el"))
  (error (message "ERROR loading modes-claude.el: %s" err)))

;; Check if hydra-claude was created
(message "\n=== Checking hydra-claude ===")
(message "fboundp 'hydra-claude/body: %s" (fboundp 'hydra-claude/body))
(message "fboundp 'tiqsi-claude-hydra: %s" (fboundp 'tiqsi-claude-hydra))

;; Check what happened with hydra-claude-modes
(message "\n=== Checking hydra-claude-modes ===")
(message "fboundp 'hydra-claude-modes/body: %s" (fboundp 'hydra-claude-modes/body))

;; List all hydras that were created
(message "\n=== All defined hydras ===")
(mapatoms (lambda (sym)
            (when (and (fboundp sym)
                       (string-match "^hydra-.*\\/body$" (symbol-name sym)))
              (message "Found hydra: %s" sym))))

(provide 'test-claude-loading)
;;; test-claude-loading.el ends here