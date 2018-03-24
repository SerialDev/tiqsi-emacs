;;; core-completion.el --- Tiqsi completion mode support

;;; Commentary:
;; 


(set-default 'ac-sources '(ac-source-abbrev ac-source-words-in-buffer))
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)


(provide 'core-completion)

;;; core-completion.el ends here
