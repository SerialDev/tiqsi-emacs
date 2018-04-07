;;; core-completion.el --- Tiqsi completion mode support

;;; Commentary:
;; 


(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; TODO mode specific enable since I use AC alot instead of company
;(eval-after-load 'company
;  '(define-key company-active-map (kbd "C-M-h") #'company-quickhelp-manual-begin))
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0.01)


 (defun my-describe-function (function)
   "Display the full documentation of FUNCTION (a symbol) in tooltip."
   (interactive (list (function-called-at-point)))
   (if (null function)
       (pos-tip-show
        "** You didn't specify a function! **" '("red"))
     (pos-tip-show
      (with-temp-buffer
        (let ((standard-output (current-buffer))
              (help-xref-following t))
          (prin1 function)
          (princ " is ")
          (describe-function-1 function)
          (buffer-string)))
      nil nil nil 0)))


(provide 'core-completion)

;;; core-completion.el ends here
