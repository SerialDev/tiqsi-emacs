;;; core-os.el --- Tiqsi OS dependant functionality  -*- lexical-binding: t -*-

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


;                                            Determine OS                                           ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(setq tiqsi-aquamacs (featurep 'aquamacs))
(setq tiqsi-linux (featurep 'x))
(setq tiqsi-win32 (not (or tiqsi-aquamacs tiqsi-linux)))
(setq tiqsi-console (eq (symbol-value 'window-system) nil))
(setq tiqsi-not-console (eq (eq (symbol-value 'window-system) nil)nil))

;                                          Determine Emacs                                          ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; Emacs type: are we running GNU Emacs?                                                           ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
        (cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
        (cons 'progn body)))

(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
        (cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
        (cons 'progn body)))

;; Emacs version
(GNUEmacs
 (list emacs-version emacs-major-version emacs-minor-version
       system-type system-name system-configuration
       window-system
       (when (boundp 'aquamacs-version) aquamacs-version)))


;                                               Win 32                                              ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-win32
  (setq tiqsi-makescript "build.bat")
  (setq tiqsi-font "PragmataPro")
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


(defun maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (when tiqsi-aquamacs (aquamacs-toggle-full-frame))
  (when tiqsi-win32 (w32-send-sys-command 61488)))


(defun w32-restore-frame ()
  "Restore a minimized frame"
  (interactive)
  (w32-send-sys-command 61728))

(defun w32-maximize-frame ()
  "Maximize the current frame"
  (interactive)
  (w32-send-sys-command 61488))

;                                               Linux                                               ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(if tiqsi-win32
    (setq os-sep "\\")
  (setq os-sep "/"))

(when tiqsi-linux
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (setq tiqsi-makescript "./build.sh")
  (display-battery-mode 1) )

;                                               Mac OS                                              ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-aquamacs
  (cua-mode 0)
  (osx-key-mode 0)
  (tabbar-mode 0)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
  (setq tiqsi-makescript "./build.macosx"))

                                        ; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)


(defun which-os ()
  "Display OS information."
  (interactive)
  (if (eq system-type 'gnu/linux)
      (if (shell-command-to-string "type -p lsb_release > /dev/null")
          (message (concat (substring (shell-command-to-string "lsb_release -sd") 1 (- (length (shell-command-to-string "lsb_release -sd")) 2)) " (" (substring (shell-command-to-string "lsb_release -sr") 0 (- (length (shell-command-to-string "lsb_release -sr")) 1)) " '" (substring (shell-command-to-string "lsb_release -sc") 0 (- (length (shell-command-to-string "lsb_release -sc")) 1)) "'" " release, using the " (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname -r")) " kernel)"))
        (if (shell-command-to-string "type -p guix > /dev/null")
            (message (concat "Guix System " (shell-command-to-string "guix system -V | awk 'NR==1{printf $5}'") " (using the " (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname -r")) " kernel)"))
          (message "Cannot determine distro.")))   
    (message system-type)))


;                 https://oremacs.com/2019/03/24/shell-apt/                 ;
; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;

(advice-add
 'ansi-color-apply-on-region
 :before 'ora-ansi-color-apply-on-region)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
        mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (ora-apt-progress-message
           (substring-no-properties
            (delete-and-extract-region mb (point))
            2 -2)))))))

(defun ora-apt-progress-message (progress)
  (setq mode-line-process
        (if (string-match
             "Progress: \\[ *\\([0-9]+\\)%\\]" progress)
            (list
             (concat ":%s "
                     (match-string 1 progress)
                     "%%%% "))
          '(":%s")))
  (force-mode-line-update))

(defun ora-apt-progress-message (progress)
  (message
   (replace-regexp-in-string
    "%" "%%"
    (ansi-color-apply progress))))

;                                            Keybindings                                            ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(define-key global-map "\ep" 'maximize-frame)
(define-key global-map "\ew" 'other-window)
(provide 'core-os)

;;; core-os.el ends here
