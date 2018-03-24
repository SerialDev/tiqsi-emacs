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
(straight-require 'ido)
(straight-require 'delight)
(straight-require 'find-dired)
(straight-require 'beacon)
(straight-require 'scratch)
(straight-require 'auto-complete)
(straight-require 'direx)
(straight-require 'beacon)
(straight-require 'rainbow-mode)
(straight-require 'highlight-indentation)
(straight-require 'highlight-thing)
(straight-require 'highlight-symbol)
(straight-require 'highlight-parentheses)
(straight-require 'paren)
(straight-require 'mic-paren)
(straight-require 'column-enforce-mode)
(straight-require 'region-state)
(straight-require 'diminish)
(straight-require 'all-the-icons)
(straight-require 'stripe-buffer)
(straight-require 'flycheck)
(straight-require 'dumb-jump)
(straight-require 'ivy)
(straight-require 'helm)
(straight-require 'helm-flx)
(straight-require 'helm-smex)
(straight-require 'bind-key)
(straight-require 'avy)
(straight-require 's)
(straight-require 'windmove)
(straight-require 'auto-complete)
(straight-require 'focus)
(straight-require 'drag-stuff)
(straight-require 'undo-tree)
(straight-require 'indent-tools)
(straight-require 'vimish-fold)
(straight-require 'corral)
(straight-require 'xref)
(straight-require 'cc-mode)
(straight-require 'compile)
(straight-require 'helpful)
(straight-require 'flycheck-rust)
(straight-require 'which-key)
(straight-require 'hydra) ;; hydras
(straight-require 'ac-helm) ;; Interactive ac with Helm
(straight-require 'helm-dash) ;; Dash Documentation Support
(straight-require 'helm-pydoc) ;; Helm Python documentation
(straight-require 'helm-swoop) ;; Swoop Editing
(straight-require 'helm-descbinds) ;; Keybindings interactive search
(straight-require 'helm-w32-launcher) ;; Start Menu Support
(straight-require 'helm-chrome) ;; Chrome Bookmarks support
(straight-require 'expand-region) ;; Select a code region
(straight-require 'pcre2el) ;; Regexp syntax converter

;-----{From git}----;

(straight-use-package
 '(volatile-highlights
   :type git
   :host github
   :repo "k-talo/volatile-highlights.el"
))


(straight-use-package
 '(fuzzy-format
   :type git
   :host github
   :repo "emacsmirror/fuzzy-format"
))


(straight-use-package
 '(dired-details
   :type git
   :host github
   :repo "emacsmirror/dired-details"
))

;---{Requirements}--;

(require 'uniquify)
(require 'helm-eshell) ;; Eshell History support


;----{From ~Lisp}---;

(load-relative "../modules/misc/andersl/demax") ;; restore congif when maximized
(try-require 'demax)


(provide 'core-setup)
;;; core-setup.el ends here
