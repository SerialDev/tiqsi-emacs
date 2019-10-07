;;; init-lite.el --- Tiqsi Emacs base  -*- lexical-binding: t -*-

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
;; Bankrupcy highly inspired by Doom Emacs
;; These blocks are not part of GNU Emacs.
;;
;;; License: MIT

;; (package-initialize)
;; (setq package-enable-at-startup nil)

;;; Code:



(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Debug?
(setq debug-on-error t)


;----{About User}---;

(setq user-full-name "C Andres Mariscal"
      user-mail-address "carlos.mariscal.melgar@gmail.com")

;----{Backup Dir}---;

;; change backup so that current directory does not clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(defun load-expand(filename)
  (load(expand-file-name filename)))


;; attempt to load a feature/library, failing silently
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")


(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))



(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if 'require' fails."
  `(require ,feature ,file 'noerror))


;{Ensure Executables};
;; Add any executables that must be found


(defun ensure-executable (exec)
  (unless (executable-find exec)
    (message (concat exec " not found in exec-path"))))


(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))


;;; timestamps in *Messages*
(defun current-time-microseconds ()
  (let* ((nowtime (current-time))
         (now-ms (nth 2 nowtime)))
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%d] " now-ms))))

(defadvice message (before test-symbol activate)
  (if (not (string-equal (ad-get-arg 0) "%s%s"))
      (let ((deactivate-mark nil)
            (inhibit-read-only t))
        (save-excursion
          (set-buffer "*Messages*")
          (goto-char (point-max))
          (if (not (bolp))
              (newline))
          (insert (current-time-microseconds))))))


;{Bootstrap Straight};

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Bootstrap use-package

(setq straight-use-package-by-default t)
(setq use-package-verbose t
      use-package-always-ensure t)

(defun straight-require (module)
  (straight-use-package module)
  (require module))


(defun straight-require-lazy (module)
  (straight-use-package-lazy module)
  (try-require module))


(straight-require 'use-package)


(defun calling-function ()
  (let ((n 6) ;; nestings in this function + 1 to get out of it
        func
        bt)
    (while (and (setq bt (backtrace-frame n))
              (not func))
        (setq n (1+ n)
              func (and bt
                      (nth 0 bt)
                      (nth 1 bt))))
    func))

(defmacro cond-require (item do-this)
  `(if (require ',item nil 'noerror)
       (try! ',do-this)
     (message (format "FAILURE-COND-CHECK %s: %s %s %s %s" ',item (current-time-microseconds) (calling-function) (format-mode-line "%l") buffer-file-name))))

(defun try!(func)
  (if (ignore-errors
	(funcall func))
      (message (format "%s SUCCESS: %s %s %s %s" (current-time-microseconds) func (calling-function) (format-mode-line "%l") buffer-file-name  ))
      (message (format "%s FAILURE: %s %s %s %s" (current-time-microseconds) func (calling-function) (format-mode-line "%l") buffer-file-name))))

(straight-require 'evil)


(load-expand  "core/core-performance.el")

(load-expand  "core/core-os.el")
(load-expand  "core/core-editing.el")
(load-expand  "core/core-ui.el")
(load-expand  "core/core-files.el")
(load-expand  "core/core-navigation.el")
(load-expand  "core/core-functionality.el")
(load-expand  "core/core-functions.el")
(load-expand  "core/core-debug.el")
(load-expand  "core/core-secrets.el")
(load-expand  "core/core-completion.el")
(load-expand  "core/core-comments.el")


(load-expand  "modules/modes/modes-ido.el")
(load-expand  "modules/programming/programming-c-lite.el")

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-c v") 'evil-mode)

;; (load-expand  "core/core-setup.el")


;;; init.el ends here
