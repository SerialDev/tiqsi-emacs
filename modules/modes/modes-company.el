;;; modes-company.el --- Tiqsi company mode support  -*- lexical-binding: t -*-

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

(use-package company
  :straight t
  :ensure t
  :init
  (require 'color) ;; -- needed for the theme
  ;; -- autocomplete as we type
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 3)
  ;; -- disable lowercase on all completions
  (setq company-dabbrev-downcase nil)
  ;; -- allow non completion characters after interaction with match
  (setq company-require-match nil)
  ;; -- custom theme


  :bind (:map company-active-map
  	      ("[up]" . company-select-previous)
  	      ("[down]" . company-select-next)
  	      ("\C-w" . nil)
  	      :map company-mode-map
  	      ("<M-SPC>" . company-complete-common))
  :defer nil
  :diminish company-mode)

;; (insert(color-lighten-name "#161616" 10))#2f992f992f99
;; (eval-after-load "company" '(let ((bg (face-attribute 'default :background)))
(eval-after-load "company" '(let ((bg "#161616"))
  (custom-set-faces
   `(company-preview ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-preview-search ((t (:inherit company-preview))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 20)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-template-field ((t (:background "deep sky blue" :foreground "black"))))
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 10)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background ,(color-lighten-name bg 40)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
   `(company-tooltip-annotation ((t (:inherit font-lock-keyword-face))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :background ,(color-lighten-name bg 40)))))
   `(company-tooltip-mouse ((t (:foreground "black"))))
   ;; `(company-tooltip-search ((t (:background "brightwhite" :foreground "black"))))
   )))

(face-attribute 'default :background)

(use-package company-statistics
  :straight t)

(use-package company-quickhelp
  :straight t
  :if (display-graphic-p)
  :init
  (setq pos-tip-foreground-color "#c5c8c6")
  (setq company-quickhelp-delay nil)
  (setq pos-tip-background-color "#3b3e40"))


(defun enable-company ()
  (interactive)
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends))
  (global-company-mode)
  (company-statistics-mode)
    (company-quickhelp-mode))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))


(defun indent-and-complete ()
  (indent-according-to-mode)
  (company-complete-common))

;; TODO Bugged fix it
(defun tiqsi/indent-or-complete (arg)
  (interactive "*P")
  (if (company-manual-begin)
      (indent-and-complete)
    (indent-or-expand arg)))

(enable-company)
(global-auto-complete-mode 0)
(define-key global-map (kbd "M-q") 'company-quickhelp-manual-begin)

;; (define-key global-map (kbd "<tab>") 'tiqsi/indent-or-complete)
;; (define-key global-map (kbd "<tab>") 'indent-or-expand)
(define-key global-map (kbd "<tab>") 'company-indent-or-complete-common)

(rectangle-mark-mode 0)

(provide 'modes-company)

;;; modes-company.el ends here
