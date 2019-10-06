;;; programming-c-lite.el --- Tiqsi C & CPP programming support based on clang  -*- lexical-binding: t -*-

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
;; pip install compdb
;; apt-get install libclang-dev
;; ninja -t compdb `ninja -t rules | grep 'CXX_COMPILER_'`


(use-package google-c-style
 :straight t
 :ensure t
 :config(progn
	  (add-hook 'c-mode-common-hook 'google-set-c-style)
	  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
	  ))


; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    /¯¯¯ UI ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    ;

(use-package preproc-font-lock
 :straight t
 :ensure t
 :config
    (preproc-font-lock-global-mode 1))

; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \ַַַ UI ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;


; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    /¯¯¯ Completion ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    ;

; ------------------------------------------------------------------------- ;
; C & CPP completion support                                                ;
; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;

(use-package irony
 :straight t
 :ensure t
 :config
 (progn
   (add-hook 'c++-mode-hook 'irony-mode)
   (add-hook 'c-mode-hook 'irony-mode)
   (add-hook 'objc-mode-hook 'irony-mode)

   (defun my-irony-mode-hook ()
     (define-key irony-mode-map [remap completion-at-point]
       'irony-completion-at-point-async)
     (define-key irony-mode-map [remap complete-symbol]
       'irony-completion-at-point-async))

   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


   (use-package company-irony-c-headers
    :straight t
    :ensure t
    :config)

   (eval-after-load 'company
     '(add-to-list
       'company-backends '(company-irony-c-headers company-irony)))

   ;; Windows performance tweaks
   ;;
   (when (boundp 'w32-pipe-read-delay)
     (setq w32-pipe-read-delay 0))
   ;; Set the buffer size to 64K on Windows (from the original 4K)
   (when (boundp 'w32-pipe-buffer-size)
     (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

   ;(define-key c-mode-map (kbd "C-?") 'irony-get-type)
   ;(define-key c-mode-base-map (kbd "C-?") 'irony-get-type)

   )
 )


; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \ַַַ Completion ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;


; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    /¯¯¯ Navigation ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    ;

; ------------------------------------------------------------------------- ;
; Grep for the symbol under the cursor.  Works only for C / C++ / H / RC    ;
; files                                                                     ;
; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;

(defun grep-curdir-symbol-at-point ()
  "Grep current directory for symbol at cursor."
  (interactive)
  (grep
   (concat "grep -n -e "
	   (current-word)
	   " *.c *.cpp *.h *.rc NUL")))


; ------------------------------------------------------------------------- ;
; Toggle between header and implementation                                  ;
; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;

(defun tiqsi-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
        (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun tiqsi-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (tiqsi-find-corresponding-file)
  (other-window -1))


; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \ַַַ Navigation ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;



(provide 'programming-c-lite)

;;; programming-c-lite.el ends here
