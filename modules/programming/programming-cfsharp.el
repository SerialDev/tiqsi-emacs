;;; programming-cfsharp.el --- Tiqsi C & F Sharp mode support  -*- lexical-binding: t -*-

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


(defun tiqsi-csharp-mode-hook ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)
  ;; (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
)


(straight-use-package
 '(omnisharp
   :type git
   :host github
   :repo "OmniSharp/omnisharp-emacs"
))



(straight-use-package
 '(csharp-mode
   :type git
   :host github
   :repo "josteink/csharp-mode"
))


(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(add-hook 'csharp-mode-hook 'tiqsi-csharp-mode-hook)
(add-hook 'csharp-mode-hook 'tiqsi-csharp-mode-hook t)

(use-package fsharp-mode
  :straight t
  :config (progn
	    (progn
	      (setq inferior-fsharp-program
		    (format "%s --readline-" (tiqsi--remove-newlines
					      (shell-command-to-string "which fsharpi"))))

	      (setq fsharp-compiler
		    (format "%s" (tiqsi--remove-newlines
				  (shell-command-to-string "which fsharpc")))))
	    ))


(use-package polymode
  :straight t
  :config (progn

	    ))


(defcustom pm-host/blazor
  (pm-host-chunkmode :name "csharp-mode"
                     :mode 'csharp-mode)
  "Csharp host chunkmode"
  :group 'poly-hostmodes
  :type 'object)

(defcustom  pm-inner/blazor-html
  (pm-inner-auto-chunkmode :name "html-code"
                           :head-matcher "^@page.*"
                           :tail-matcher "^@functions"
                           :mode-matcher 'html-mode)
  "Html blazor code block."
  :group 'poly-innermodes
  :type 'object)

(defcustom  pm-inner/blazor-csharp
  (pm-inner-auto-chunkmode :name "csharp-code"
                           :head-matcher "^@functions"
                           :tail-matcher "^}"
                           :mode-matcher 'csharp-mode)
  "csharp blazor code block."
  :group 'poly-innermodes
  :type 'object)

(define-polymode poly-blazor-mode
  :hostmode 'pm-host/blazor
  :innermodes '(pm-inner/blazor-html
		pm-inner/blazor-csharp))



(provide 'programming-cfsharp)

;;; programming-cfsharp.el ends here
