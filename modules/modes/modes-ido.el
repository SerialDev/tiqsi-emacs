;;; modes-ido.el --- Tiksi IDO configuration  -*- lexical-binding: t -*-

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

;; Ido find file requirements

(use-package ido
  :straight t
  :ensure t
  :config )


(straight-use-package
  '(ido-completing-read+
     :type git
     :host github
     :repo "DarwinAwardWinner/ido-completing-read-plus"
     ))

(ido-mode 1)
;; Enhanced IDO configuration for performance and usability
(setq ido-use-virtual-buffers t) ; Show recent files in buffer switching
(setq ido-enable-flex-matching t) ; Flexible matching
(setq ido-everywhere t) ; Use IDO everywhere
(setq ido-max-prospects 12) ; Show more prospects
(setq ido-max-work-file-list 50) ; Keep more work files in memory
(setq ido-work-directory-list-ignore-regexps '("^/tmp/" "^/var/tmp/")) ; Ignore temp dirs
(setq ido-ignore-buffers '("\\` " "^\*")) ; Ignore internal buffers
(setq ido-use-filename-at-point 'guess) ; Guess filename at point
(setq ido-create-new-buffer 'always) ; Always allow creating new buffers
(setq ido-file-extensions-order '(".py" ".el" ".txt" ".org" ".md" ".js" ".html" ".css")) ; Prioritize file types

;; Configure recentf for better buffer switching
(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 300)
  (setq recentf-auto-cleanup 600)) ; Cleanup every 10 minutes

(use-package smex
  :ensure t
  :straight t
  :init
  (smex-initialize)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items")))

;; Make IDO work vertically
(use-package ido-vertical-mode
  :ensure t
  :straight t
  :init               ; I like up and down arrow keys:
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode 1))


(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
         (symbol-names '()))
    (flet ((addsymbols (symbol-list)
             (when (listp symbol-list)
               (dolist (symbol symbol-list)
                 (let ((name nil) (position nil))
                   (cond
                     ((and (listp symbol) (imenu--subalist-p symbol))
                       (addsymbols symbol))

                     ((listp symbol)
                       (setq name (car symbol))
                       (setq position (cdr symbol)))

                     ((stringp symbol)
                       (setq name symbol)
                       (setq position (get-text-property 1 'org-imenu-marker symbol))))

                   (unless (or (null position) (null name))
                     (add-to-list 'symbol-names name)
                     (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
		(matching-symbols (delq nil (mapcar (lambda (symbol)
                                                      (if (string-match regexp symbol) symbol))
                                              symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
              matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
            (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark (point))
      (goto-char position))))


;; (defun my-switch-to-buffer (buffer)
;;   (interactive
;;    (list (ido-read-buffer "Switch to buffer: ")))
;;   ;; get-buffer-window (with second argument t) will return
;;   ;; nil if the buffer is not open in any window across any
;;   ;; frame
;;   (unless (get-buffer-window buffer 0)
;;     (pop-to-buffer buffer nil t)))
;; Replaced by selectrum




(GNUEmacs25
  (progn
    (use-package ido  :config
      (setq ido-auto-merge-delay-time 99999999)
      (setq ido-virtual-buffers t)
      )

    (use-package flx-ido   :requires ido :config (flx-ido-mode))
    (use-package ido-vertical-mode   :requires ido :config (ido-vertical-mode))
    ;; (use-package ido-ubiquitous   :requires ido :config (ido-ubiquitous-mode))
    (ido-everywhere)
    (defun ido-execute-extended-command ()
      "Use ido' to select and execute a command."
      (interactive)
      (call-interactively
	(intern
	  (ido-completing-read
	    "M-x "
	    (all-completions "" obarray 'commandp)))))

    ))



(defun ido-sort-folders-then-ext-alpha (files)
  (let ((files-copy (copy-sequence files)))
    (sort files-copy
      (lambda (a b)
        (let ((dir-a (string-suffix-p "/" a))
               (dir-b (string-suffix-p "/" b)))
          (cond
            ((and dir-a dir-b) (string< a b))
            (dir-a t)
            (dir-b nil)
            (t (let* ((ext-a (or (file-name-extension a) ""))
                       (ext-b (or (file-name-extension b) "")))
                 (if (string= ext-a ext-b)
                   (string< a b)
                   (string< ext-a ext-b))))))))))

(ignore-errors
  (advice-remove 'ido-file-internal #'ido-sort-by-ext-and-alpha))
(ignore-errors
  (advice-remove 'ido-file-internal #'ido-sort-folders-then-ext-alpha))

(advice-add 'ido-file-internal :filter-return #'ido-sort-folders-then-ext-alpha)

(define-key global-map (kbd "C-x f") 'ido-find-file)
;; Note: This conflicts with helm-smex in modes-helm.el
;; Only one of helm or ido should be active at a time
;; Don't override M-x - use smex as primary, this as alternative
(define-key global-map (kbd "C-x C-x") 'ido-execute-extended-command)


(setq vc-handled-backends nil)
;; These settings are now configured above in the enhanced section
;; (setq ido-enable-flex-matching t) ; Already set above
;; (setq ido-use-faces nil) ; Keep faces for better visual distinction
(setq ido-use-virtual-buffers t) ; OVERRIDE: Enable virtual buffers for recent files
(setq ido-auto-merge-delay-time 99999999)
(setq ido-max-file-prompt-width 0.6)
;; ido-ignore-buffers already set above in enhanced section
(setq ido-ignore-directories '("\\`node_modules\\'" "\\`\\.venv\\'" "\\`__pycache__\\'"))
(setq ido-ignore-files '("\\`\\." "\\.pyc\\'" "\\.o\\'" "\\.elc\\'"))


(provide 'modes-ido)

;;; modes-ido.el ends here