;;; modes-flycheck.el --- Tiqsi Flycheck configuration

;;; Commentary:
;; 

;reduce how often we run it for large amount of error, mostly useful for .py;
(setq flycheck-highlighting-mode 'lines)


(custom-set-faces
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange")))))

;    Push mark when going to the next error to go back to previous position    ;
(defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
  (push-mark))


;        Make some good use of screen realstate in the windows title bar       ;
(with-eval-after-load 'flycheck
  (flycheck-title-mode))


;-{Custom Warnings}-;

(defcustom flycheck-navigation-minimum-level nil
  "The minimum level of errors to navigate.

If set to an error level, only navigate errors whose error level
is at least as severe as this one.  If nil, navigate all errors."
  :group 'flycheck
  :type '(radio (const :tag "All locations" nil)
                (const :tag "Informational messages" info)
                (const :tag "Warnings" warning)
                (const :tag "Errors" error)
                (symbol :tag "Custom error level"))
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.21"))


;------{Python}-----;

(setq flycheck-flake8-maximum-line-length '99)


(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))


;---{Keybindings}---;

;;(add-hook 'python-mode-hook ' sdev-python-mode-hook)
(define-key python-mode-map (kbd "C-c n") 'flycheck-next-error)
(define-key python-mode-map (kbd "C-c b") 'flycheck-previous-error)
(define-key python-mode-map (kbd "C-c ee") 'flycheck-list-errors)


(provide 'modes-flycheck)

;;; modes-flycheck.el ends here
