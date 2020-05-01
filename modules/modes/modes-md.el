;;; modes-md.el --- Tiqsi md configuration  -*- lexical-binding: t -*-

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


;; (straight-use-package
;;  '(artbollocks-mode
;;    :type git
;;    :host github
;;    :repo "sashac/artbollocks-mode"
;; ))


(straight-use-package
 '(writegood-mode
   :type git
   :host github
   :repo "bnbeckwith/writegood-mode"
))

(defun tiqsi-md-to-html()
  (interactive)
  (async-shell-command (s-prepend (s-prepend "pandoc " (s-prepend (buffer-name) " -o ")) (s-replace ".md" ".html" (buffer-name)))))

(use-package flycheck-vale
  :straight t
  :ensure t)

(add-hook 'markdown-mode-hook '(lambda () (flycheck-mode 1)))
(add-hook 'markdown-mode-hook 'flycheck-vale-toggle-enabled)
(add-hook 'markdown-mode-hook '(lambda () (writegood-mode 1)))

(defun tiqsi-markdown-mode-before-save-hook ()
  (when (eq major-mode 'markdown-mode)
    (pos-tip-show
    (message "%s\n%s"
	     (writegood-grade-level)
	     (writegood-reading-ease) ))
    ))

(add-hook 'before-save-hook #'tiqsi-markdown-mode-before-save-hook)

(defun count-occurences (regex string)
  (recursive-count regex string 0))

(defun recursive-count (regex string start)
  (if (string-match regex string start)
      (+ 1 (recursive-count regex string (match-end 0)))
    0))

(defun tiqsi-pd-to-md-table()
  (interactive)
  (let (
	 (data
	  (s-trim
	   (s-collapse-whitespace
	     (get-selection)  ) )))
     
     (let (( ocurrences (count-occurences " " data)) )
       (insert "| ")
       (insert (s-replace " " " | " data) )
       (insert " |")
       (insert "\n")
       
       (dotimes (i ocurrences)
	 (if (eq i 0)
	     (insert "| ----- |")
	   (if (eq i (- ocurrences 1))
	       (insert " ----:|")
	     (insert ":----:|")))
	 )    (insert "\n")
	      )))


; ------------------------------------------------------------------------- ;
;                               Use pandoc-mm                               ;
; ------------------------------------------------------------------------- ;
;                     Install stack for haskell packages                    ;
;            git clone https://github.com/vzaccaria/pandoc-mm.git           ;
;                                cd pandoc-mm                               ;
;                              stack install .                              ;
; ------------------------------------------------------------------------- ;
;                         npm install markmap-lib -g                        ;
; ------------------------------------------------------------------------- ;


(defun tiqsi-md-to-mindmap()
  (interactive)
  (async-shell-command
   (s-prepend "markmap " (buffer-name) ) ))





(define-key markdown-mode-map (kbd "C-c gg") 'writegood-grade-level)
(define-key markdown-mode-map  (kbd "C-c ge") 'writegood-reading-ease)


(provide 'modes-md)

;;; modes-md.el ends here
