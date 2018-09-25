;;; modes-org.el --- Tiqsi org mode support configuration  -*- lexical-binding: t -*-

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
;; Setting up Org Mode TODO: Fri, 25 Nov 2016, 10:01 alot more editing needed
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; cycle through TODOs C-c C-t followed by entry key
(setq org-todo-keywords
      '((sequence "TODO(t)" "REPORT(r)"   "|" "DONE(d)" "INFO(i)")
        (sequence "EMACS(e)" "BUG(b)" "DATA(a)" "|" "FIXED(f)")
        (sequence "|" "CANCELED(c)")))


(setq org-log-done 'my-insert-time-stamp)
                                        ;(setq org-log-done ')
;; I did this yesterday behaviour with org-mode
(defun org-todo-with-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () my-current-time)))
    (org-todo arg)
    ))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (lisp . t)
   ;; other languages..
   (clojure . t)
                                        ;(sh . t)
   (dot . t)
   (emacs-lisp . t)
   ))


                                        ; Archive done tasks in tree / also change to file or agenda if using those in ORG mode : http://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'tree))

;; Remove the markup characters, i.e., "/text/" becomes (italized) "text"
(setq org-hide-emphasis-markers t)


;; Turn on visual-line-mode for Org-mode only
;; Also install "adaptive-wrap" from elpa
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)


;; Use cider as the Clojure execution backend
(setq org-babel-clojure-backend 'cider)


;; No timeout when executing calls on Cider via nrepl
(setq org-babel-clojure-sync-nrepl-timeout nil)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)

;; Tangle Org files when we save them
(defun tangle-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-babel-tangle)))

(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

;; Enable the auto-revert mode globally. This is quite useful when you have
;; multiple buffers opened that Org-mode can update after tangling.
;; All the buffers will be updated with what changed on the disk.
(global-auto-revert-mode)

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;; make sure that when we export in HTML, that we don't export with inline css.
;; that way the CSS of the HTML theme will be used instead which is better
(setq org-html-htmlize-output-type 'css)

(when (require 'core-secrets nil 'noerror)

  (custom-set-variables
   '(ispell-program-name 'secrets-ispell))
  )
;; Enable Flyspell for text modes
(add-hook 'text-mode-hook 'flyspell-mode)

;; Enable python mode in org-mode
;;(add-hook 'org-mode-hook 'python-mode)

                                        ; Remove autosave and other unnecessary files to see in Dire
                                        ; (setq-default dired-omit-files-p t) ; Buffer-local variable
                                        ; (setq dired-omit-files "^\\.?#")

;; Enable inline image when entering org-mode
;; Make sure you have all the necessary DLL for image display
;; Windows ones can be downloaded from: https://sourceforge.net/projects/ezwinports/files/
(defun turn-on-org-show-all-inline-images ()
  (org-display-inline-images t t))

(add-hook 'org-mode-hook 'turn-on-org-show-all-inline-images)
;; Test correct dlls
                                        ;(print image-library-alist)

                                        ;---{Keybindings}---;

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Useful keybindings when using Clojure from Org
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

(provide 'modes-org)

;;; modes-org.el ends here
