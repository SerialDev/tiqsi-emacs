;;; test-claude-hydra.el --- Test Claude hydra loading

;; Test script to verify hydra loading

;; Add Tiqsi core to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/build/"))

;; Initialize straight.el
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

;; Load straight packages
(straight-use-package 'lv)
(straight-use-package 'hydra)

;; Try to load hydra
(require 'lv)
(require 'hydra)

;; Check if defhydra is available
(if (fboundp 'defhydra)
    (progn
      (message "SUCCESS: defhydra is available")
      
      ;; Define a simple test hydra
      (defhydra test-hydra (:color blue)
        "Test"
        ("q" nil "quit"))
      
      (if (fboundp 'test-hydra/body)
          (message "SUCCESS: Test hydra created successfully")
        (message "ERROR: Test hydra failed to create"))
      
      ;; Now test the Claude hydra definition
      (defhydra hydra-claude-test (:color pink :hint nil)
        "
Test Claude Hydra
  _c_: Start   _q_: Quit
"
        ("c" (lambda () (interactive) (message "Start")) "Start")
        ("q" nil "Quit" :exit t))
      
      (if (fboundp 'hydra-claude-test/body)
          (message "SUCCESS: Claude-style hydra created successfully")
        (message "ERROR: Claude-style hydra failed to create")))
  (message "ERROR: defhydra not available"))

(provide 'test-claude-hydra)
;;; test-claude-hydra.el ends here