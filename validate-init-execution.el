;;; validate-init-execution.el --- Validate init-lite.el execution completeness -*- lexical-binding: t -*-

;;; Commentary:
;; Add checkpoint logging throughout init-lite.el to see where execution stops

;;; Code:

;; Create a log file for tracking
(defvar init-execution-log-file "/tmp/emacs-init-execution.log")
(defvar init-checkpoints '())

(defun log-checkpoint (checkpoint-name &optional details)
  "Log a checkpoint during init execution."
  (let ((timestamp (current-time-string))
        (entry (format "[%s] CHECKPOINT: %s%s\n" 
                       timestamp 
                       checkpoint-name 
                       (if details (format " - %s" details) ""))))
    (push (cons checkpoint-name timestamp) init-checkpoints)
    (message "CHECKPOINT: %s" checkpoint-name)
    (append-to-file entry nil init-execution-log-file)))

;; Clear the log file
(when (file-exists-p init-execution-log-file)
  (delete-file init-execution-log-file))

(log-checkpoint "VALIDATION START" "Beginning init execution tracking")

;; Now let's add checkpoints to key points in init-lite.el by temporarily modifying it
;; We'll insert these checkpoints at strategic points

(defun validate-current-state ()
  "Check current state of key variables and functions."
  (log-checkpoint "STATE CHECK" 
                  (format "helm-smex: %s, M-x binding: %s, smex loaded: %s"
                          (if (fboundp 'helm-smex) "AVAILABLE" "MISSING")
                          (key-binding (kbd "M-x"))
                          (if (featurep 'smex) "YES" "NO"))))

;; Add to end of init to verify completion
(add-hook 'after-init-hook 
          (lambda ()
            (log-checkpoint "AFTER-INIT-HOOK" "Emacs initialization complete")
            (validate-current-state)
            (log-checkpoint "FINAL STATE" 
                           (format "Total checkpoints: %d" (length init-checkpoints)))
            
            ;; Print summary
            (message "\n=== INIT EXECUTION SUMMARY ===")
            (dolist (checkpoint (reverse init-checkpoints))
              (message "✓ %s at %s" (car checkpoint) (cdr checkpoint)))
            (message "=== END SUMMARY ===\n")
            
            ;; Write final log
            (append-to-file (format "\n=== EXECUTION COMPLETE ===\nTotal checkpoints: %d\n" 
                                   (length init-checkpoints))
                           nil init-execution-log-file)
            
            ;; Also write to a status file we can check
            (with-temp-file "/tmp/emacs-init-status.txt"
              (insert (format "INIT COMPLETE\nCheckpoints: %d\nM-x bound to: %s\nhelm-smex available: %s\n"
                             (length init-checkpoints)
                             (key-binding (kbd "M-x"))
                             (if (fboundp 'helm-smex) "YES" "NO"))))))

;; Test if we can add checkpoints by advice
(defun add-checkpoint-advice (func-name checkpoint-name)
  "Add advice to FUNC-NAME to log a checkpoint."
  (when (fboundp (intern func-name))
    (advice-add (intern func-name) :before
                (lambda (&rest _args)
                  (log-checkpoint checkpoint-name (format "Calling %s" func-name))))))

;; Add some strategic advice
(log-checkpoint "ADDING ADVICE" "Setting up function call tracking")

;; Track key loading functions
(advice-add 'load :before
            (lambda (file &rest _args)
              (log-checkpoint "LOADING FILE" 
                             (format "Loading: %s" 
                                    (if (stringp file) file (format "%s" file))))))

(advice-add 'require :before
            (lambda (feature &rest _args)
              (log-checkpoint "REQUIRING FEATURE" (format "Requiring: %s" feature))))

;; Track package loading
(advice-add 'straight-use-package :before
            (lambda (recipe &rest _args)
              (log-checkpoint "STRAIGHT PACKAGE" 
                             (format "Installing: %s" 
                                    (if (listp recipe) (car recipe) recipe)))))

(log-checkpoint "VALIDATION SETUP COMPLETE" "Ready to track init execution")

;;; validate-init-execution.el ends here