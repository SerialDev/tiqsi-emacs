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



(use-package csharp-mode
  :straight t
  :config (progn
	    (add-hook 'csharp-mode-hook 'tiqsi-csharp-mode-hook)
	    (add-hook 'csharp-mode-hook 'tiqsi-csharp-mode-setup t)
	    ))


(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))


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




(provide 'programming-cfsharp)

;;; programming-cfsharp.el ends here
