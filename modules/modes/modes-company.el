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

;; TODO check this out more
					;(straight-require 'company-childframe)


(use-package company
  :straight t
  :ensure t
  :init
  (require 'color) ;; -- needed for the theme
  ;; -- autocomplete as we type
  (setq company-idle-delay 0.05)
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
  :straight t
  :ensure t)

(use-package company-quickhelp
  :straight t
  :ensure t
  :if (display-graphic-p)
  :config
  ;;  (add-hook 'python-mode-hook 'company-quickhelp)
  :init
  (setq pos-tip-foreground-color "#c5c8c6")
  (setq company-quickhelp-delay 0.05)
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

;; Currently using Corfu
;; (enable-company)


(defun tiqsi-company-debug-backend ()
  (interactive)
  (message "Current company backends: %s" company-backends))

(define-key global-map (kbd "M-q") 'company-quickhelp-manual-begin)

;; (define-key global-map (kbd "<tab>") 'tiqsi/indent-or-complete)
;; (define-key global-map (kbd "<tab>") 'indent-or-expand)
(define-key global-map (kbd "<tab>") 'company-indent-or-complete-common)

(use-package posframe
  :straight t
  :after dashboard
  )

(use-package markdown-mode
  :straight t
  :after dashboard
  )

(use-package yasnippet
  :straight t
  )

;; ;; lsp-bridge: faster lsp server
;; (use-package lsp-bridge
;;   :straight (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
;; 	      :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources"))
;;   :hook(prog-mode . lsp-bridge-mode)
;;   )


(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
               :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
               :build (:not compile))
  :init

  ;; (global-lsp-bridge-mode)

  )


(defun activate-lsp-bridge-with-uv ()
  "Set up lsp-bridge with uv virtual environment for Python files."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (let ((default-directory (file-name-directory buffer-file-name)))
      (setq-local lsp-bridge-python-command
        (string-trim (shell-command-to-string "cd $PWD && uv_source && which python")))
      (setq-local lsp-bridge-python-default-server 'pyright)
      (lsp-bridge-mode 1))))





(defun conditional-xref-lsp-find-definition ()
  "Attempt to jump to definition, pushing to xref stack only if unsuccessful."
  (interactive)
  (condition-case nil
    (lsp-find-definition)   ;; This pushes to xref stack on success
    (error
      (xref-push-marker-stack) ;; Push to stack only if definition fails
      (message "Definition not found."))))


(defun conditional-xref-lsp-find-definition-side-buffer ()
  "Open definition in a side buffer and switch focus to it."
  (interactive)
  (condition-case nil
    (let ((buf (save-window-excursion
                 (lsp-find-definition)
                 (current-buffer))))
      (display-buffer-in-side-window buf '((side . right) (slot . 1) (window-width . 0.5)))
      (select-window (get-buffer-window buf)))
    (error
      (xref-push-marker-stack)
      (message "Definition not found."))))



(defun conditional-xref-lsp-find-definition-side-buffer ()
  "Open definition in a right side buffer, reusing an existing side window if available."
  (interactive)
  (condition-case nil
    (let ((buf (save-window-excursion
                 (lsp-find-definition)
                 (current-buffer)))
           (right-window (or (window-in-direction 'right)
                           (display-buffer-in-side-window buf '((side . right) (slot . 1) (window-width . 0.5))))))
      (set-window-buffer right-window buf)
      (select-window right-window))
    (error
      (xref-push-marker-stack)
      (message "Definition not found."))))


(add-hook 'python-mode-hook 'lsp)
(add-hook 'emacs-lisp-mode-hook 'company-mode)



(define-key python-mode-map (kbd "C-.") 'conditional-xref-lsp-find-definition)       ;; Direct jump to definition
(define-key python-mode-map (kbd "C->") 'conditional-xref-lsp-find-definition-side-buffer) ;; Jump to definition in side buffer


(define-key python-mode-map (kbd "C-`") 'lsp-ui-peek-find-definitions)
(define-key python-mode-map (kbd "C-.") 'conditional-xref-lsp-find-definition)       ;; Direct jump to definition
(define-key python-mode-map (kbd "C-,") 'xref-go-back)      ;; Jump back
(define-key python-mode-map (kbd "C-~") 'lsp-ui-peek-find-references)        ;; Find references


;; Enable xref navigation for Emacs Lisp buffers
(define-key emacs-lisp-mode-map (kbd "C-.") 'xref-find-definitions) ;; Jump to definition
(define-key emacs-lisp-mode-map (kbd "C-,") 'xref-pop-marker-stack) ;; Jump back




(defun install-pyright-in-uv ()
  (interactive)
  (shell-command "uv_source && pip install pyright"))


(rectangle-mark-mode 0)

(provide 'modes-company)

;;; modes-company.el ends here
