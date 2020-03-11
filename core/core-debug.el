;;; core-debug.el --- Tiqsi debugging support  -*- lexical-binding: t -*-

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
;; Debug on error


;;; Code:

(defun core-debug-on-error ()
  "Toggle variable `core-debug-on-error'."
  (interactive)
  (setq core-debug-on-error (not core-debug-on-error))
  (message "core-debug-on-error=%s" core-debug-on-error))




                                        ;--------{Check for paren bugs}--------;

(defun core-debug-pbug ()
  "Check parenthesis bugs or similar horrors.
  Even with Emacs advanced programming facilities, checking mismatching
  parenthesis or missing quote (so called \"pbug\") is no less annoying than
  pointer chasing in C.

  This function divides the buffer into regions and tries evaluating them one
  by one.  It stops at the first region where it fails to evaluate because of
  pbug or any other errors.  It sets point and mark (and highlights if
  variable `transient-mark-mode' is on) on the failing region and center its first
  line.  \"^def\" is used to define regions.  You may also `eval-region'
  right after pbug is done to let Lisp parse pinpoint the bug.

  No more \"End of file during parsing\" horrors!"
  (interactive)
  (let ((point (point))
        (region-regex "^(def..")
        defs beg end)
    (goto-char (point-min))
    (setq defs (loop while (search-forward-regexp region-regex nil t)
                     collect (point-at-bol)))
    ;; so it evals last definition
    (nconc defs (list (point-max)))
    (setq beg (point-min))
    (while defs
      (goto-char beg)
      (setq end (pop defs))
      ;; to be cool, uncomment these to see core-debug-pbug doing step by step
      ;; (message "checking core-debug-pbug from %s to %s..." beg end)
      ;; (sit-for 1)
      (when (eq (condition-case nil
                    (eval-region beg (1- end))
                  (error 'core-debug-pbug-error))
                'core-debug-pbug-error)
        (push-mark end 'nomsg 'activate)
        (goto-char beg)
        (recenter)
        (error "a core-debug-pbug found from %s to %s" beg end))
      (setq beg end))
    (goto-char point)
    (message "no core-debug-pbug found")))

                                        ;-----------{test init file}-----------;

;; http://oremacs.com/page17/ - test emacs init file
(defun core-debug-test-emacs ()
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))


                                        ;-------------{Bug hunter}-------------;

;; Debug startup failures. If your Emacs init file signals an error during startup, but you don’t know why,
;; simply issue M-x bug-hunter-init-file RET RET and The Bug Hunter will find it for you.
                                        ;(use-package bug-hunter
                                        ;  :ensure t)

                                        ;---------{Version bug reports}--------;

;; for automatic version info addition to bug reports
(defun core-debug-insert-debug-version-info ()
  "Insert version of Emacs and 7 characters of the commit hash."
  (interactive)
  (insert
   (format "GNU Emacs %s (commit %s)"
           emacs-version
           (substring (emacs-repository-get-version) 0 7))))

                                        ;-----------------{gdb}----------------;

;; (setq gdb-many-windows nil)
;; (defun set-gdb-layout(&optional c-buffer)
;;   (if (not c-buffer)
;;       (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer
;;   ;; from http://stackoverflow.com/q/39762833/846686
;;   (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
;;   (switch-to-buffer gud-comint-buffer)
;;   (delete-other-windows) ;; clean all
;;   (let (
;;          (w-source (selected-window)) ;; left top
;;          (w-gdb (split-window w-source nil 'right)) ;; right bottom
;;          (w-locals (split-window w-gdb nil 'above)) ;; right middle bottom
;;          (w-stack (split-window w-locals nil 'above)) ;; right middle top
;;          (w-breakpoints (split-window w-stack nil 'above)) ;; right top
;;          (w-io (split-window w-source (floor( 0.9 (window-body-height)))
;;                              'below)) ;; left bottom
;;          )
;;     (set-window-buffer w-io (gdb-get-buffer-create 'gdb-inferior-io))
;;     (set-window-dedicated-p w-io t)
;;     (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
;;     (set-window-dedicated-p w-breakpoints t)
;;     (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
;;     (set-window-dedicated-p w-locals t)
;;     (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
;;     (set-window-dedicated-p w-stack t)
;; (set-window-buffer w-gdb gud-comint-buffer)

;; (select-window w-source)
;; (set-window-buffer w-source c-buffer)
;; ))


;; (defadvice gdb (around args activate)
;;   "Change the way to gdb works."
;;   (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
;;   (let (
;;         (c-buffer (window-buffer (selected-window))) ;; save current buffer
;;         )
;;     ad-do-it
;;     (set-gdb-layout c-buffer))
;;   )

;; (defadvice gdb-reset (around args activate)
;;   "Change the way to gdb exit."
;;   ad-do-it
;; (set-window-configuration global-config-editing))

(global-set-key (kbd "C-c (") 'core-debug-pbug)

(provide 'core-debug)

;;; core-debug.el ends here
