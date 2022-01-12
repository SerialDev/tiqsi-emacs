;;; core-performance.el --- Tiqsi core performance utilities  -*- lexical-binding: t -*-

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
;; Buffer/Line/Usability performance
;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers

;;; Code:

;; Asynchronous execution

(straight-use-package
  '(dired-async
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

(straight-use-package
  '(async-bytecomp
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

(straight-use-package
  '(async
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

;; (autoload 'dired-async-mode "dired-async.el" nil t)
;; (dired-async-mode 1)
;; (async-bytecomp-package-mode 1)

;; Garbage collection

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq gc-cons-threshold 20000000)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 20000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'helm-minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(add-hook 'helm-minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq-default bidi-display-reordering nil)
(setq redisplay-dont-pause t)
(setq togle-truncate-lines t )


(defun toggle-truncate-lines ()
  "Toggle whether to wrap lines at right window border."
  (interactive)
  (if (eq truncate-lines nil)
    (set-variable 'truncate-lines 't)
    (set-variable 'truncate-lines nil)
    ) )

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
    (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    ))


(when (require-soft 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21


                                        ;-----------{Speed up linum}-----------;
;; https://www.reddit.com/r/emacs/comments/3ky8lj/throttle_updating_line_numbers/

(defun fast-count-lines()
  (save-excursion
    (goto-char (point-max))
    (string-to-number (format-mode-line "%l"))))

(with-eval-after-load "linum"
  ;; set `linum-delay' so that linum uses `linum-schedule' to update linums.
  (setq linum-delay t)

  ;; create a new var to keep track of the current update timer.
  (defvar-local my-linum-current-timer nil)

  ;; rewrite linum-schedule so it waits for 1 second of idle time
  ;; before updating, and so it only keeps one active idle timer going
  (defun linum-schedule ()
    (when (timerp my-linum-current-timer)
      (cancel-timer my-linum-current-timer))
    (setq my-linum-current-timer
      (run-with-idle-timer 1 nil #'linum-update-current))))



(defun clear-buffer-long-printouts-line()
  (if (>
        (save-excursion
          (goto-char (point-max))
          (string-to-number (format-mode-line "%l")))
        10000
        )
    (comint-clear-buffer)
    nil))


(defun clear-buffer-long-printouts-col()
  (if (>
        (save-excursion
          (goto-char (point-max))
          (string-to-number (format-mode-line "%c")))
        10000
        )
    (comint-clear-buffer)
    nil))

(defun clear-buffer-long-printouts()
  (interactive)
  (progn
    (clear-buffer-long-printouts-line)
    (clear-buffer-long-printouts-col)))



(defun long-printout-teardown()
  (run-with-timer
    0 5 'clear-buffer-long-printouts))


;;;###autoload
(define-minor-mode clear-huge-repl-mode
  "clear massive printouts to not slow emacs to a crawl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'clear-buffer-long-printouts)
            map))

;;;###autoload
(progn
  (add-hook 'comint-mode-hook 'clear-huge-repl-mode)
  (add-hook 'inferior-python-mode 'clear-huge-repl-mode)
  (add-hook 'clear-huge-repl-mode-hook 'long-printout-teardown)
  )


(setq max-specpdl-size 5000)  ; default is 1000, increase the backtrace level


;; (adaptive-wrap-prefix-mode nil)

;; (defun my-line-count (line)
;;   (let ((count (bound-and-true-p my-line-count)))
;;     (when (or (not count) (> line count))
;;       (setq count ((string-to-number fast-count-lines))
;;       (setq-local my-line-count count)
;;       (linum-schedule))
;;     count))

;; (setq linum-format
;;       (lambda (line)
;;         (let* ((w (length (number-to-string
;;                            (my-line-count line))))
;;                (fmt (concat "%" (number-to-string w) "d"
;;                             (if window-system "" " "))))
;;           (propertize (format fmt line) 'face 'linum))))


(provide 'core-performance)

;;; core-performance.el ends here
