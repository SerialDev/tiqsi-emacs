;;; test-claude-hydra.el --- Test Claude hydra loading -*- lexical-binding: t -*-

;; Mock the load-expand function
(defun load-expand (file)
  "Mock load-expand function for testing."
  (let ((full-path (expand-file-name file 
                     "/Users/amariscalcloudflare.com/Documents/workdir/personal/repos/tiqsi-emacs/")))
    (if (file-exists-p full-path)
        (load-file full-path)
      (message "Warning: File not found: %s" full-path))))

;; Mock straight-use-package
(defun straight-use-package (package)
  "Mock straight-use-package for testing."
  (message "Mock: Would install package %s" package))

;; Test loading
(condition-case err
    (progn
      ;; Load hydra if available
      (require 'hydra nil t)
      
      ;; Load the claude mode
      (load-file "modes-claude.el")
      
      ;; Check if hydra was created
      (if (fboundp 'hydra-claude/body)
          (progn
            (message "✅ hydra-claude/body is defined!")
            ;; List all the commands in the hydra
            (message "Available commands:")
            (dolist (cmd '(tiqsi-claude-start
                          tiqsi-claude-kill
                          tiqsi-claude-toggle
                          tiqsi-claude-send-region
                          tiqsi-claude-send-function
                          tiqsi-claude-send-buffer
                          tiqsi-claude-ask-question
                          tiqsi-claude-fix-error
                          tiqsi-claude-optimize-code
                          tiqsi-claude-explain-code
                          tiqsi-claude-generate-tests))
              (message "  - %s: %s" cmd 
                      (if (fboundp cmd) "✓ defined" "✗ NOT defined"))))
        (message "❌ hydra-claude/body is NOT defined!")))
  (error 
   (message "❌ Error loading: %s" (error-message-string err))))