;;; Programming-javascript.el --- Tiqsi Emacs base  -*- lexical-binding: t -*-

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
;;; License: MIT

;;; Code:

(straight-require 'tide)


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)
(setq tide-completion-detailed t
  tide-always-show-documentation t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(straight-require 'rjsx-mode)

(use-package rjsx-mode
  :ensure t
  :straight t
  :mode ("\\.jsx?$" . rjsx-mode)
  :config
  (setq js-indent-level 2))

(use-package rjsx-mode
  :interpreter (("node" . rjsx-mode))
  :mode (("\\.js?\\'" . rjsx-mode)
          ("\\.jsx?\\'" . rjsx-mode))
  :config (progn
            (setq js2-basic-offset 2
              js2-highlight-level 3
              js2-bounce-indent-p t
              js2-mode-show-strict-warnings nil)))


(use-package nodejs-repl
  :ensure t
  :straight t
  )


;; (defun nvm-which ()
;;   (let* ((shell (concat (getenv "SHELL") " -l -c 'nvm which'"))
;;          (output (shell-command-to-string shell)))
;;     (cadr (split-string output "[\n]+" t))))


;; (defun nvm-which ()
;;   (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
;;     (if (cl-search "Node Version" output)
;;         (setq output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which current"))
;;     (setq output (cadr (split-string output "[\n]+" t))))
;;   (message output)
;;   output))

;; (setq nodejs-repl-command #'nvm-which)
;; (setq nodejs-repl-command "node --experimental-repl-await ")
;; (setq nodejs-repl-command "NODE_PATH=$(npm root -g) node")
(setq nodejs-repl-command "node")

(setenv "NODE_PATH" "/usr/local/lib/node_modules")

;; NODE_PATH=$(npm root -g)
(add-hook 'js-mode-hook
  (lambda ()
    (define-key js-mode-map (kbd "C-x C-s") 'nodejs-repl-send-last-expression)
    (define-key js-mode-map (kbd "C-c C-a") 'nodejs-repl-send-line)
    (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
    (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
    (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
    (define-key js-mode-map (kbd "C-c C-p") 'nodejs-repl)
    (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)
    ))


(defun comment-jsx
  (start end)
  (interactive "r")
  (let ((current-region (buffer-substring start end)))
    (progn
      (goto-char start)
      (move-beginning-of-line 1)
      (insert "{/* ")
      (goto-char end)
      (move-end-of-line 1)
      (insert " */}")
      )
    )
  )




;; (use-package lsp-mode
;;   :ensure t
;;   :commands lsp
;; :hook (prog-mode . lsp))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config
;; (push 'company-lsp company-backends))

;; (define-key js-mode-map (kbd "C-c \\") 'comment-jsx)

;; (lsp-ui-peek-mode 1)
;; (define-key lsp-mode-map (kbd "C-c d") 'lsp-describe-session)
;; (define-key lsp-mode-map (kbd "C-c i") 'lsp-describe-thing-at-point)
;; (define-key lsp-mode-map (kbd "C-c <right>") 'lsp-rename)
;; (define-key lsp-mode-map (kbd "C-c <up>") 'lsp-goto-implementation)
;; (define-key lsp-mode-map (kbd "C-c <left>") 'lsp-goto-type-definition)
;; (define-key lsp-mode-map (kbd "C-c <down>") 'lsp-find-references)

(provide 'programming-javascript)

;;; programming-javascript.el ends here
