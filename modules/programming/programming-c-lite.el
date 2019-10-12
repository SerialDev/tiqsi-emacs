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

; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ  /¯¯¯ Tooling ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ     ;
 ;; valgrind --leak-check=full --track-origins=yes -v



(defun tiqsi--tool-valgrind--run(string)
  (interactive "sString for file_name: ")
  ;; (let ((input (cfrs-read "Text: " "Initial Input")))
    (compile (s-concat "valgrind --leak-check=full --track-origins=yes -v " (current-buffer-path) string ))
  )

(defun tiqsi--tool-strace--run(string)
  (interactive "sString for command: ")
    (compile (s-concat "strace -f " string ))
  )


(defun tiqsi--tool-coz--run(string arguments)
  (interactive "sString file_name for causal profiling:  
sString arguments for causal profiler: ")
    (compile (s-concat "coz run  " " --- " (current-buffer-path) string ))
  )


; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \ַַַ Tooling ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯     ;

; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ   /¯¯¯ Functionality ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ  ;

(use-package google-c-style
 :straight t
 :ensure t
 :config(progn
	  (add-hook 'c-mode-common-hook 'google-set-c-style)
	  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
	  ))

(straight-require 'cc-mode)

; ------------------------------------------------------------------------- ;
;https://stackoverflow.com/questions/3312114/how-to-tell-emacs-to-open-h-file-in-c-mode
; ------------------------------------------------------------------------- ;
; C or C++ when dealing with .H files                                       ;
; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;

;; function decides whether .h file is C or C++ header, sets C++ by
;; default because there's more chance of there being a .h without a
;; .cc than a .h without a .c (ie. for C++ template files)
(defun c-c++-header ()
  "sets either c-mode or c++-mode, whichever is appropriate for
header"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;; and if that doesn't work, a function to toggle between c-mode and
;; c++-mode
(defun c-c++-toggle ()
  "toggles between c-mode and c++-mode"
  (interactive)
  (cond ((string= major-mode "c-mode")
         (c++-mode))
        ((string= major-mode "c++-mode")
         (c-mode))))

; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \ַַַ Functionality ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;

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


;; C & CPP Header completion
(straight-use-package
 '(company-irony-c-headers
   :type git
   :host github
   :ensure t
   :repo "hotpxl/company-irony-c-headers"
))


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

; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ /¯¯¯ Snippets ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ     ;

(defun tiqsi--c-insert-std-version ()
  (interactive)
  (insert
"
#if __STD_VERSION__ == 201710L
    /* It's C18 */
#elif __STD_VERSION__ == 201112L
    /* It's C11 */
#elif __ STD_VERSION__ == 199901L
    /* it's C99 */
#elif __STDC__ == 1
    /* it's C90 */
#else
    /* it's not standard conforming */
#endif
"))

; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \ַַַ Snippets ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯     ;


; ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ    /¯¯¯ Keybindings ¯¯¯\ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ ַ   ;


(eval-after-load 'c-mode
  (progn  
    (define-key c-mode-map [f12] 'tiqsi-find-corresponding-file)
    (define-key c-mode-map [M-f12] 'tiqsi-find-corresponding-file-other-window)
  ))

(define-key c++-mode-map [f12] 'tiqsi-find-corresponding-file)
(define-key c++-mode-map [M-f12] 'tiqsi-find-corresponding-file-other-window)

; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \ַַַ Keybindings ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;


(provide 'programming-c-lite)

;;; programming-c-lite.el ends here
