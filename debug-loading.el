;;; debug-loading.el --- Debug init-lite.el loading sequence -*- lexical-binding: t -*-

;;; Commentary:
;; This file debugs the entire loading sequence to find where things break

;;; Code:

(setq debug-on-error t)
(setq load-prefer-newer t)

(defvar debug-loading-results '()
  "List to store debug results.")

(defun debug-log (step status &optional error-info)
  "Log a debug step with STATUS and optional ERROR-INFO."
  (let ((entry (list :step step 
                     :status status 
                     :time (current-time)
                     :error error-info)))
    (push entry debug-loading-results)
    (message "[DEBUG] %s: %s%s" step status 
             (if error-info (format " - %s" error-info) ""))))

(defun debug-check-function (func-name)
  "Check if FUNC-NAME is available and bound."
  (let ((available (fboundp (intern func-name))))
    (debug-log (format "Function %s" func-name) 
               (if available "AVAILABLE" "MISSING"))
    available))

(defun debug-check-package (package-name)
  "Check if PACKAGE-NAME is loaded."
  (let ((loaded (featurep (intern package-name))))
    (debug-log (format "Package %s" package-name) 
               (if loaded "LOADED" "NOT LOADED"))
    loaded))

(defun debug-check-variable (var-name)
  "Check if VAR-NAME is bound."
  (let ((bound (boundp (intern var-name))))
    (debug-log (format "Variable %s" var-name) 
               (if bound "BOUND" "UNBOUND"))
    bound))

(defun debug-load-file (file-path)
  "Try to load FILE-PATH and catch any errors."
  (condition-case err
      (progn
        (load file-path)
        (debug-log (format "Loading %s" (file-name-nondirectory file-path)) "SUCCESS")
        t)
    (error 
     (debug-log (format "Loading %s" (file-name-nondirectory file-path)) 
                "FAILED" (format "%s" err))
     nil)))

;; Start debugging
(debug-log "DEBUG SESSION START" "STARTING")

;; Test basic Emacs info
(debug-log "Emacs version" (format "%s" emacs-version))
(debug-log "Emacs major version" (format "%d" emacs-major-version))

;; Load init-lite.el step by step
(debug-log "Current directory" default-directory)

;; Check if core files exist
(let ((core-files '("core/core-setup.el" 
                   "core/core-performance.el"
                   "modules/modes/modes-ido.el" 
                   "modules/modes/modes-helm.el")))
  (dolist (file core-files)
    (let ((full-path (expand-file-name file)))
      (debug-log (format "File exists: %s" file)
                 (if (file-exists-p full-path) "YES" "NO")))))

;; Try loading init-lite.el with detailed tracking
(debug-log "About to load init-lite.el" "STARTING")

(condition-case err
    (load "init-lite.el")
  (error (debug-log "Loading init-lite.el" "FAILED" (format "%s" err))))

;; Check critical packages after loading
(debug-log "=== POST-LOAD CHECKS ===" "STARTING")

;; Check straight.el
(debug-check-package "straight")

;; Check IDO
(debug-check-package "ido")
(debug-check-function "ido-switch-buffer")

;; Check smex  
(debug-check-package "smex")
(debug-check-function "smex")
(debug-check-variable "smex-cache")

;; Check helm
(debug-check-package "helm")
(debug-check-function "helm-smex")
(debug-check-function "helm-mini")

;; Check LSP
(debug-check-package "lsp-mode")
(debug-check-function "lsp")
(debug-check-package "lsp-pyright")

;; Check what M-x is actually bound to
(let ((mx-binding (key-binding (kbd "M-x"))))
  (debug-log "M-x bound to" (format "%s" mx-binding)))

;; Check what C-x b is bound to  
(let ((cxb-binding (key-binding (kbd "C-x b"))))
  (debug-log "C-x b bound to" (format "%s" cxb-binding)))

;; Final summary
(debug-log "=== SUMMARY ===" "COMPLETE")
(debug-log "Total steps checked" (format "%d" (length debug-loading-results)))

;; Print all results
(message "\n=== FULL DEBUG REPORT ===")
(dolist (result (reverse debug-loading-results))
  (message "[%s] %s: %s%s"
           (format-time-string "%H:%M:%S" (plist-get result :time))
           (plist-get result :step)
           (plist-get result :status)
           (let ((err (plist-get result :error)))
             (if err (format " (%s)" err) ""))))

(message "=== END DEBUG REPORT ===\n")

;;; debug-loading.el ends here