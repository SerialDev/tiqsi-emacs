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

(straight-require 'org-tree-slide)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)


(org-babel-do-load-languages
'org-babel-doload-languages
'(

   ))


(org-babel-do-load-languages
  'org-babel-load-languages '(
   (python . t)
			       ))


(org-babel-do-load-languages
  'org-babel-load-languages '(
   (C . t)
			       ))

(org-babel-do-load-languages
  'org-babel-load-languages '(
   (shell . t)
			       ))

(setq org-babel-python-command "python3")

;; (setq org-babel-shell-command "sh")
(setq org-startup-with-inline-images t)




(straight-use-package
 '(ox-reveal
   :type git
   :require t
   :host github
   :repo "yjwen/org-reveal"
))


(straight-use-package
 '(org-re-reveal
   :type git
   :require t
   :host github
   :repo "emacsmirror/org-re-reveal"
))
(require 'org)
(require 'ox)

(require 'org-re-reveal)


(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
(setq org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")


(defun tiqsi-org-mode-before-save-hook ()
  (when (eq major-mode 'org-mode)
    (progn
      (org-reveal-export-to-html)
      (pos-tip-show "exported")
      )

    ))


;; set variable to allow for inline-image resizing within org-mode for large images
(setq org-image-actual-width t)

;; enables abbreviations such as <s for source code
;; enables easy templates again
(require 'org-tempo)

;; disable newline indentation
(add-hook 'org-mode-hook (lambda ()
                           (electric-indent-mode -1)))

;; -------------------------------------
;; ORG-BABEL
;; -------------------------------------

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (calc . t)
   (latex . t)
   (java . t)
   (ruby . t)
   (lisp . t)
   (scheme . t)
   (shell . t)
   (sqlite . t)
   (js . t)
   (octave . t)
   (emacs-lisp .t)
    (restclient . t)))



(add-hook 'before-save-hook #'tiqsi-org-mode-before-save-hook)

(defun tiqsi-org-reveal-insert-img()
  (interactive)
  (let ((title (read-string "Enter  title:") )
	(image_name (read-string "Enter  image_name:")) )

(insert (message 
"** %s\n\
   :PROPERTIES:\n\
   :reveal_background: images/%s\n\
   :reveal_background_size: 600px\n\
   :reveal_background_trans: slide\n\
   :reveal_background_opacity: 0.2\n\
   :END:\n\

** \n\
   :PROPERTIES:\n\
   :reveal_background: images/%s\n\
   :reveal_background_size: 600px\n\
   :reveal_background_trans: slide\n\
   :reveal_background_opacity: 1\n\
   :END:\n\
" title image_name image_name)

  ))
  )






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






;; Override Markdown Mode's image overlays so the image markdown code and the image are both visible!
(eval-after-load "markdown-mode"
  '(defun markdown-display-inline-images ()
  "Add inline image overlays to image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."
  (interactive)
  (unless (display-images-p)
    (error "Cannot show images"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-link-inline nil t)
        (let ((start (match-beginning 0))
              (end (match-end 0))
              (file (match-string-no-properties 6)))
          (when (file-exists-p file)
            (let* ((abspath (if (file-name-absolute-p file)
                                file
                              (concat default-directory file)))
                   (image
                    (if (and markdown-max-image-size
                             (image-type-available-p 'imagemagick))
                        (create-image
                         abspath 'imagemagick nil
                         :max-width (car markdown-max-image-size)
                         :max-height (cdr markdown-max-image-size))
                      (create-image abspath))))
              (when image
                (setq newStart (+ end ))
                (setq newEnd (+ end 1))
                (let ((ov (make-overlay newStart newEnd)))
                  (message "%s" newEnd)
                  (overlay-put ov 'display image)
                  (overlay-put ov 'face 'default)
                  (overlay-put ov 'before-string "\n\n")
                  (push ov markdown-inline-image-overlays))))))))))
)


                                        ;---{Keybindings}---;

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)

;; Useful keybindings when using Clojure from Org
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)


(org-defkey org-mode-map "\C-c\C-i" 'tiqsi-org-reveal-insert-img)

(provide 'modes-org)

;;; modes-org.el ends here
