;;; core-functionality.el --- Tiqsi functionality  -*- lexical-binding: t -*-

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


                                        ;{Describe Keybindings};

;;; Code
(defun describe-map (map)
  "Describe the key bindings of MAP.
posted by:
From: Cyprian Laskowski <cyp@swagbelly.net>
"
  (interactive
   (list (intern (completing-read "Describe keymap: " obarray
                                  #'(lambda (e)
                                      (and (boundp e)
                                           (string-match "-map$"
                                                         (symbol-name e))))
                                  t))))
  (let (beg end)
    (with-temp-buffer
      (use-local-map (eval map))
      (describe-bindings))
    (set-buffer "*Help*")
    (rename-buffer (generate-new-buffer-name (concat "*" (symbol-name map) " bindings*")))
    (setq beg (and (re-search-forward "^Major Mode Bindings:$" nil t) (1+ (match-end 0)))
          end (and (re-search-forward "^Global Bindings:$" nil t) (match-beginning 0)))
    (if (and beg end)
        (narrow-to-region beg end)
      (narrow-to-region 1 1)
      (error (concat (symbol-name map) " has no bindings set.")))))


                                        ;{GOTO visible window};

(defun pop-to-buffer-or-window (buffer)
  "Like `pop-to-buffer' BUFFER, but find any visible window."
  (let* (win
         )
    (setq win (get-buffer-window buffer t))
    (if (null win)
        (pop-to-buffer buffer)
      (raise-frame (window-frame win))
      (select-frame (window-frame win))
      (select-window win)
      )))

                                        ;----{M-x short}----;

(defun display-extended-command-shorter (command)
  "Display information on a shorter way to M-x a command."
  (interactive (list (read-extended-command)))
  (message "The command `%s' can be invoked with `M-x %s'"
           command
           (execute-extended-command--shorter command command)))

(use-package stripe-buffer
  :straight t
  :ensure t
  :config (progn
            (add-hook 'dired-mode-hook 'turn-on-stripe-buffer-mode)
            (add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
            (set-face-attribute 'stripe-highlight nil :background "#eee8d5")
            ))


                                        ;{Change transparency};

(defun me-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


                                        ;{distinguish buffers};

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-after-kill-buffer-p t)


(define-minor-mode sk/dubcaps-mode
  "Toggle `sk/dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if sk/dubcaps-mode
      (add-hook 'post-self-insert-hook #'sk/dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'sk/dcaps-to-scaps 'local)))

(add-hook 'global-mode-hook #'sk/dubcaps-mode)

(defun sk/diminish-dubcaps ()
  (interactive)
  (diminish 'sk/dubcaps-mode ""))
(add-hook 'sk/dubcaps-mode-hook 'sk/diminish-dubcaps)

                                        ;----{Lint Code}----;

(add-hook 'after-init-hook #'global-flycheck-mode) ;; flycheck for linting code


                                        ;{Jump to definition};
;; dumb-jump for peek at definitions
(use-package dumb-jump
  :straight t
  :config (setq dumb-jump-selector 'ivy)
  :ensure t)

                                        ;{NO local variables};

(setq enable-local-variables nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions (quote other))
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function (quote ignore))
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (15)))
 '(version-control nil))


(defadvice set-mark-command (after no-bloody-t-m-m activate)
  "Prevent consecutive marks activating bloody `transient-mark-mode'."
  (if transient-mark-mode (setq transient-mark-mode nil)))
(defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
  "Prevent mouse commands activating bloody `transient-mark-mode'."
  (if transient-mark-mode (setq transient-mark-mode nil)))

                                        ;---{Keybindings}---;
(global-set-key (kbd "<f7>") 'me-transparency)
(global-set-key (kbd "M-g o") 'dumb-jump-go-other-window)
(global-set-key (kbd "M-g j") 'dumb-jump-go)
(global-set-key (kbd "M-g p") 'dumb-jump-quick-look)



(provide 'core-functionality)

;;; core-functionality.el ends here
