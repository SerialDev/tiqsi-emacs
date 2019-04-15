;;; core-completion.el --- Tiqsi completion mode support  -*- lexical-binding: t -*-

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

;; Completion core

;                                             Completion                                            ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; Get all possible dabbrev expansions                                                               ;
; from https://curiousprogrammer.wordpress.com/2009/04/07/autocomplete-and-dabbrev/                 ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defun ac-source-dabbrev (abbrev)
  (interactive)
  (dabbrev--reset-global-variables)
  (let ((dabbrev-check-all-buffers t))
    (sort (dabbrev--find-all-expansions abbrev t) #'string<)))

(defvar ac-source-dabbrev-words
  '((candidates
     . (lambda () (all-completions ac-target
                                   (ac-source-dabbrev ac-target)))))
  "Get all the completions using dabbrev")


(defun ac-self-insert ()
  (interactive)
  (self-insert-command 1)
  (ac-start))

(defun ac-fix-keymap ()
  (let ((i 32))
    (while (<= i ?z)
      (define-key ac-complete-mode-map
        (make-string 1 i) 'ac-self-insert)
      (incf i))))

(ac-fix-keymap)

(define-key ac-complete-mode-map (kbd "DEL")
  (lambda ()
    (interactive)
    (backward-delete-char-untabify 1)
    (ac-start)))

;; (setq ac-auto-start nil)
(setq tab-always-indent 'complete)

(setq-default ac-sources '(ac-source-dabbrev-words ac-source-abbrev ac-source-words-in-buffer))

(add-to-list 'completion-styles 'initials t)

; ------------------------------------------------------------------------------------------------  ;
;                  TODO mode specific enable since I use AC alot instead of company                 ;
; ------------------------------------------------------------------------------------------------  ;
;; (eval-after-load 'company
;;  '(define-key company-active-map (kbd "C-M-h") #'company-quickhelp-manual-begin))
;; (company-quickhelp-mode 1)
;; (setq company-quickhelp-delay 0.01)


(defun core-completion-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (pos-tip-show
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t))
         (prin1 function)
         (princ " is ")
         (describe-function-1 function)
         (buffer-string)))
     nil nil nil 0)))


(defun describe-thing-in-popup ()
  (interactive)
  (let* ((thing (symbol-at-point))
         (help-xref-following t)
         (description (with-temp-buffer
                        (help-mode)
                        (help-xref-interned thing)
                        (buffer-string))))
    (pos-tip-show description)))

(global-set-key (kbd "M-5") 'describe-thing-in-popup)


(provide 'core-completion)

;;; core-completion.el ends here
