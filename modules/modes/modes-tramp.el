;;; modes-tramp.el --- Tiqsi tramp configuration

;;; Commentary:
;; 


; (defun python-tramp-hook()
; (setq
;  python-shell-interpreter "ipython"
;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;  python-shell-interpreter-args "--simple-prompt -i"
;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;  python-shell-completion-setup-code
;    "from IPython.core.completerlib import module_completion"
;  python-shell-completion-string-code
;    "';'.join(module_completion('''%s'''))\n"
;  python-shell-completion-string-code
;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
;(add-hook 'python-mode-hook 'python-tramp-hook)


(provide 'modes-tramp)

;;; modes-tramp.el ends here
