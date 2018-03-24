;;; modes-ag.el --- Tiqsi Silver Searcher code searching Support

;;; Commentary:
;; 

(custom-set-variables
 '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
 '(helm-ag-command-option "--all-text")
 '(helm-ag-insert-at-point 'symbol)
 '(helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))

;--{search with Ag}-;

;; (define-key global-map (kbd "C-c hs") 'helm-ag)


;{search project root};

;; (define-key global-map (kbd "C-c hp") 'helm-ag-project-root)

(provide 'modes-ag)

;;; modes-ag.el ends here
