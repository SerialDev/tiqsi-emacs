;;; programming-text.el --- Tiqsi text mode handling & configuration

;;; Commentary:
;; 

;;; Code:

(defun tiqsi-big-fun-text-hook ()
  ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)
  ; Newline indents, semi-colon doesn't
  (define-key text-mode-map "\C-m" 'newline-and-indent)
  ; Prevent overriding of alt-s
  (define-key text-mode-map "\es" 'tiqsi-save-buffer)
  )
(add-hook 'text-mode-hook 'tiqsi-big-fun-text-hook)

(provide 'programming-text)

;;; programming-text.el ends here
