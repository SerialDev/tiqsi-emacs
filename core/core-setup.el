;;; core-setup.el --- Tiqsi setup

;;; Commentary:
;; 

(setq straight-use-package-by-default t)
(setq use-package-verbose t
      use-package-always-ensure t)

(defun straight-require (module)
  (straight-use-package module)
  (require module))

(straight-require 'use-package)
(straight-require 'hydra)
(straight-require 'delight)
(straight-require 'beacon)
(straight-require 'scratch)
(straight-require 'auto-complete)
(straight-require 'direx)
(straight-require 'beacon)
(straight-require 'rainbow-mode)
(straight-require 'highlight-indentation)
(straight-require 'highlight-thing)
(straight-require 'highlight-symbol)
(straight-require 'column-enforce-mode)
(straight-require 'region-state)
(straight-require 'diminish)
(straight-require 'all-the-icons)


;================================={From ~lisp}=================================;


;-----{From git}----;

(straight-use-package
 '(volatile-highlights
   :type git
   :host github
   :repo "k-talo/volatile-highlights.el"
))

(provide 'core-setup)

;;; core-setup.el ends here
