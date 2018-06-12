;;; core-setup.el --- Tiqsi setup

;;; Commentary:
;;

(setq straight-use-package-by-default t)
(setq use-package-verbose t
      use-package-always-ensure t)

(defun straight-require (module)
  (straight-use-package module)
  (require module))


(defun straight-require-lazy (module)
  (straight-use-package-lazy module)
  (try-require module))


(straight-require 'use-package)

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


;-----------------{UX}-----------------;


;-------{M-x}-------;

(straight-require 'spaceline)
(straight-require 'spaceline-all-the-icons)
(straight-require 'delight)
(straight-require 'diminish)


;-----{Windows}-----;

(straight-require-lazy 'neotree)
(straight-require 'all-the-icons)
(straight-require 'switch-window)
(straight-require 'beacon)
(straight-require 'windmove)


;------{Buffer}-----;

(straight-require 'stickyfunc-enhance)
(straight-require 'stripe-buffer)
(straight-require 'rainbow-mode)
(straight-require 'highlight-indentation)
(straight-require 'highlight-thing)
(straight-require 'highlight-symbol)
(straight-require 'highlight-parentheses)
(straight-require 'mic-paren)

;------{Search}-----;

(straight-require 'ivy)
(straight-require 'hydra)
(straight-require 'ido)
(straight-require 'ido-completing-read+)
(straight-require 'find-dired)
(straight-require 'direx)

;-------{Git}-------;

(straight-require 'magit)
(straight-require 'git-gutter-fringe)

;-------{Misc}------;

(straight-require-lazy 'flyspell)
(straight-require-lazy 'column-enforce-mode)
(straight-require 'scratch)
(straight-require 'region-state)
(straight-require 'bind-key)
(straight-require 'erc)
(straight-require 'org)
(straight-require-lazy 'adaptive-wrap)
(straight-require 'focus)
(straight-require 'camcorder)

;---{helpful libs}--;

(straight-require 'helpful)
(straight-require 'which-key)
(straight-require '0xc)

(straight-require 'pos-tip)
(straight-require 'popup-pos-tip)

;-----{needed?}-----;

(straight-require 'paren)
(straight-require 'color)
(straight-require 'cc-mode)
(straight-require 'compile)

;--{cutting board}--;

;(straight-require 'posframe)
;(straight-require 'company-childframe)
;; (straight-require 'parinfer)

;---------------{Modules}--------------;

;-------{Helm}------;

(straight-require 'helm)
(straight-require 'helm-flx)
(straight-require 'helm-smex)
(straight-require 'helm-etags-plus) ;; Helm Etags support
(straight-require 'helm-ag) ;; Interactive Silver Searcher with Helm
(straight-require 'ac-helm) ;; Interactive ac with Helm
(straight-require 'helm-dash) ;; Dash Documentation Support
(straight-require 'helm-pydoc) ;; Helm Python documentation
(straight-require 'helm-swoop) ;; Swoop Editing
(straight-require 'helm-descbinds) ;; Keybindings interactive search
(straight-require 'helm-w32-launcher) ;; Start Menu Support
(straight-require 'helm-chrome) ;; Chrome Bookmarks support

;-------------{Programming}------------;

;-{elisp libraries}-;

(straight-require 'elisp-slime-nav)
(straight-require 's)
(straight-require 'htmlize)
(straight-require 'docker-api)
(straight-require 'pcre2el) ;; Regexp syntax converter


;---{Major Modes}---;

(straight-require 'clojure-mode)
(straight-require 'cl)
(straight-require 'elf-mode)
(straight-require-lazy 'rust-mode)
(straight-require-lazy 'textx-mode)
(straight-require-lazy 'bnfc)

(straight-require 'docker)
(straight-require 'docker-compose-mode)
(straight-require 'dockerfile-mode)

;----{Platforms}----;

(straight-require 'ob-ipython)
(straight-require-lazy 'ein)
(straight-require 'cider)


;------{tools}------;

(straight-require-lazy 'bug-hunter)

(straight-require 'py-isort)
;; (straight-require 'traad) ;; Not convinced
(straight-require 'py-autopep8)


;---{Intellisense}--;

(straight-require-lazy 'rtags)
(straight-require-lazy 'jdee)
(straight-require 'jedi)
(straight-require 'racer)



(straight-require 'irony) ;; C & CPP completion support
(straight-require 'auto-complete)
(straight-require 'company-statistics)
(straight-require 'company-quickhelp)
(straight-require 'company-jedi)
(straight-require 'dabbrev)

(straight-require-lazy 'ac-racer)


;----{utilities}----;

(straight-require 'disaster)
(straight-require 'cargo)


;-----{Linters}-----;

(straight-require-lazy 'flymake)
(straight-require-lazy 'flymake-easy)
(straight-require-lazy 'flymake-rust)
(straight-require 'flycheck)
(straight-require 'flycheck-title)
(straight-require 'flycheck-rust)
(straight-require 'semantic)


;----{Navigation}---;

(straight-require 'etags-select)
(straight-require 'ctags-update)
(straight-require 'minimap)
(straight-require 'projectile)
(straight-require 'dumb-jump)
(straight-require 'avy)
(straight-require 'ace-jump-buffer)
(straight-require 'ace-window)
(straight-require 'xref)


;---------------{Editing}--------------;

(straight-require-lazy 'origami)
(straight-require 'paredit)
(straight-require 'drag-stuff)
(straight-require 'undo-tree)
(straight-require 'indent-tools)
(straight-require-lazy 'vimish-fold)
(straight-require-lazy 'corral)
(straight-require-lazy 'expand-region) ;; Select a code region by semantic units


;----------------{Misc}----------------;

(straight-require-lazy 'typing)


;-----{From git}----;


(straight-use-package
 '(evil
   :type git
   :host github
   :repo "emacs-evil/evil"
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


;; (straight-use-package
;;  '(wakatime
;;    :type git
;;    :host github
;;    :repo "wakatime/wakatime-mode"
;; ))


(straight-use-package
 '(ensime
   :type git
   :host github
   :repo "ensime/ensime-emacs"
))

;---{Requirements}--;

(try-require 'browse-url) ; part of gnu emacs
(try-require 'uniquify)
(try-require 'dired-x)
(try-require 'helm-eshell) ;; Eshell History support


;----{From ~Lisp}---;

(load-expand "modules/misc/andersl/demax") ;; restore congif when maximized
(try-require 'demax)

(load-expand "modules/misc/tuhdo/org-recipes") ;; Org mode recipes
(try-require 'org-recipes)

; Does not have a melpa source: http://www.hczim.de/software/gas-mode.html
(load-expand "modules/misc/hczim/gas-mode") ;; Assembly
(try-require 'gas-mode)


(ensure-executable "clang")
(ensure-executable "gdb")
(ensure-executable "ctags")
(ensure-executable "cmake")
(ensure-executable "make")
(ensure-executable "rdm")
(ensure-executable "ag")

(provide 'core-setup)
;;; core-setup.el ends here
