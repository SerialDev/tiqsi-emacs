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
(straight-require 'spaceline)
(straight-require 'neotree)
(straight-require 'origami)
(straight-require 'bug-hunter)
(straight-require 'rtags)
(straight-require 'jdee)
(straight-require 'py-isort)
(straight-require 'py-autopep8)
(straight-require 'typing)
(straight-require 'flymake)
(straight-require 'flymake-easy)
(straight-require 'flymake-rust)
(straight-require 'rust-mode)
(straight-require 'textx-mode)
(straight-require 'bnfc)
(straight-require 'etags-select)
(straight-require 'ctags-update)
(straight-require 'helm-etags-plus) ;; Helm Etags support
(straight-require 'spaceline-all-the-icons)
(straight-require 'hydra)
(straight-require 'ido)
(straight-require 'ido-completing-read+)
(straight-require 'delight)
(straight-require 'find-dired)
(straight-require 'magit)
(straight-require 'git-gutter-fringe)
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
(straight-require 'minimap)
(straight-require 'projectile)
(straight-require 'all-the-icons)
(straight-require 'stripe-buffer)
(straight-require 'flycheck)
(straight-require 'flycheck-title)
(straight-require 'dumb-jump)
(straight-require 'ivy)
(straight-require 'helm)
(straight-require 'helm-flx)
(straight-require 'helm-smex)
(straight-require 'bind-key)
(straight-require 'avy)
(straight-require 'ace-jump-buffer)
(straight-require 'ace-window)
(straight-require 's)
(straight-require 'erc)
(straight-require 'org)
(straight-require 'adaptive-wrap)
(straight-require 'htmlize)
(straight-require 'cider)
(straight-require 'company-quickhelp)
(straight-require 'ob-ipython)
(straight-require 'docker)
(straight-require 'docker-api)
(straight-require 'jedi)
(straight-require 'paredit)
;; (straight-require 'parinfer)
(straight-require 'docker-compose-mode)
(straight-require 'dockerfile-mode)
(straight-require 'camcorder)
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
(straight-require 'semantic)
(straight-require 'stickyfunc-enhance)
(straight-require 'which-key)
(straight-require 'pos-tip)
(straight-require 'disaster)
(straight-require '0xc)
(straight-require 'cl)
(straight-require 'dabbrev)
(straight-require 'elf-mode)
(straight-require 'popup-pos-tip)
(straight-require 'racer)
(straight-require 'cargo)
(straight-require 'clojure-mode)
(straight-require 'cider)
(straight-require 'ac-racer)
(straight-require 'irony) ;; C & CPP completion support
(straight-require 'hydra) ;; hydras
(straight-require 'helm-ag) ;; Interactive Silver Searcher with Helm
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
 '(evil
   :type git
   :host github
   :repo "emacs-evil/evil"
))


;; Asynchronous execution

(straight-use-package
 '(dired-async
   :type git
   :host github
   :repo "jwiegley/emacs-async"
))

(straight-use-package
 '(async-bytecomp
   :type git
   :host github
   :repo "jwiegley/emacs-async"
))

(straight-use-package
 '(async
   :type git
   :host github
   :repo "jwiegley/emacs-async"
))


(straight-use-package
 '(flycheck-rtags
   :type git
   :host github
   :repo "Andersbakken/rtags"
))


;; C & CPP Header completion 
(straight-use-package
 '(company-irony-c-headers
   :type git
   :host github
   :repo "hotpxl/company-irony-c-headers"
))

(straight-use-package
 '(volatile-highlights
   :type git
   :host github
   :repo "k-talo/volatile-highlights.el"
))


(straight-use-package
 '(sdev-mypy
   :type git
   :host github
   :repo "SerialDev/mypy-mode"
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


(straight-use-package
 '(wakatime
   :type git
   :host github
   :repo "wakatime/wakatime-mode"
))


(straight-use-package
 '(ensime
   :type git
   :host github
   :repo "ensime/ensime-emacs"
))

;---{Requirements}--;

(require 'browse-url) ; part of gnu emacs
(require 'uniquify)
(require 'dired-x)
(require 'helm-eshell) ;; Eshell History support


;----{From ~Lisp}---;

(load-relative "../modules/misc/andersl/demax") ;; restore congif when maximized
(try-require 'demax)

(load-relative "../modules/misc/tuhdo/org-recipes") ;; Org mode recipes
(try-require 'org-recipes)

; Does not have a melpa source: http://www.hczim.de/software/gas-mode.html
(load-relative "../modules/misc/hczim/gas-mode") ;; Assembly
(try-require 'gas-mode)


(provide 'core-setup)
;;; core-setup.el ends here
