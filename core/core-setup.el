;;; core-setup.el --- Tiqsi setup  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;


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

; cfrs.el  Child Frame Read String                                          ;
; ------------------------------------------------------------------------- ;


(straight-use-package
 '(cfrs
   :type git
   :host github
   :repo "Alexander-Miller/cfrs"
))



;(use-package dashboard
;  :straight t
;  :ensure t
;  :config
;  (dashboard-setup-startup-hook))



(use-package ediff
  :straight t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))


;-------{M-x}-------;

(straight-require 'spaceline)
(straight-require 'spaceline-all-the-icons)
(straight-require 'delight)


;-----{Windows}-----;

(straight-require 'all-the-icons)
(straight-require 'switch-window)
(straight-require 'windmove)


;------{Buffer}-----;

(straight-require 'stickyfunc-enhance)
(straight-require 'stripe-buffer)
(straight-require 'rainbow-mode)
(straight-require 'highlight-indentation)
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
(straight-require 'scratch)
(straight-require 'region-state)
(straight-require 'bind-key)
(straight-require 'erc)
(straight-require 'org)
(straight-require 'focus)
(straight-require-lazy 'adaptive-wrap)
(straight-require-lazy 'camcorder)

;---{helpful libs}--;

(straight-require 'helpful)
(straight-require '0xc)
(straight-require 'parsec)

(straight-require 'popup)
(straight-require 'pos-tip)
(straight-require 'popup-pos-tip)

;-----{needed?}-----;

;--{cutting board}--;


(require 'cc-mode)
(require 'compile)
(require 'color)
(require 'paren)

;; (straight-require 'posframe)
;(straight-require 'company-childframe)

;; (straight-require 'company-jedi)
;; (straight-require 'jedi)

;---------------{Modules}--------------;

;-------------{Programming}------------;

(straight-require 'helm)
;-{elisp libraries}-;

(straight-require 'elisp-slime-nav)
;; (straight-require 'slime-docker)

(straight-require 's)
(straight-require 'htmlize)
(straight-require 'docker-api)
(straight-require 'pcre2el) ;; Regexp syntax converter


;---{Major Modes}---;

(straight-require 'clojure-mode)
;; (straight-require 'inf-clojure)
(straight-require 'cl)
(straight-require-lazy 'elm-mode)
(straight-require-lazy 'elf-mode)
(straight-require-lazy 'rust-mode)
(straight-require 'textx-mode)
(straight-require 'bnfc)
(straight-require 'tide) ;; Typescript mode support 
(straight-require 'geiser) ;; Scheme mode support 
(straight-require 'flymd) ;; Markdown mode support live editing

(straight-require 'docker)
(straight-require 'docker-compose-mode)
(straight-require 'dockerfile-mode)

;----{Platforms}----;

(straight-require 'ob-ipython)
(straight-require 'ein)
(straight-require 'cider)


;------{tools}------;

(straight-require-lazy 'bug-hunter)

(straight-require 'py-isort)
;; (straight-require 'traad) ;; Not convinced
(straight-require 'py-autopep8)


;---{Intellisense}--;

(straight-require 'rtags)
;; (straight-require 'jdee)
(straight-require 'racer)



(straight-require 'irony) ;; C & CPP completion support
(straight-require 'auto-complete)
(straight-require 'company-statistics)
(straight-require 'company-quickhelp)
(straight-require 'dabbrev)

(straight-require 'ac-racer)
(straight-require 'slime-company)

;----{utilities}----;

(straight-require 'disaster)
(straight-require 'cargo)


;-----{Linters}-----;

(straight-require 'flymake)
(straight-require 'flymake-easy)
(straight-require 'flymake-rust)
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

(straight-require 'origami)
(straight-require 'paredit)
(straight-require 'drag-stuff)
(straight-require 'undo-tree)
(straight-require 'indent-tools)
(straight-require-lazy 'vimish-fold)
(straight-require 'corral)
(straight-require 'expand-region) ;; Select a code region by semantic units


;----------------{Misc}----------------;

(straight-require 'typing)


;-----{From git}----;


(straight-use-package
 '(evil
   :type git
   :host github
   :repo "emacs-evil/evil"
))

;(straight-use-package
; '(matrix-client
;   :type git
;   :host github
;   :repo "jgkamat/matrix-client-el"
;))


;; (straight-use-package
;;  '(evxcr
;;    :type git
;;    :host github
;;    :repo "serialdev/evxcr-mode"
;;    :config
;;    (add-hook 'rust-mode-hook #'evcxr-minor-mode)
;; ))

;; language server protocol support
;; (straight-use-package
;;  '(lsp-mode
;;    :type git
;;    :host github
;;    :repo "emacs-lsp/lsp-mode"
;;    :ensure t
;;   :preface (setq lsp-enable-flycheck nil
;;                  lsp-enable-indentation nil
;;                  lsp-highlight-symbol-at-point nil)
;;   )
;; )

;(straight-require 'blacken)

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
(try-require 'uniquify) ;; part of gnu emacs
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
(ensure-executable "tree")


(provide 'core-setup)
;;; core-setup.el ends here
