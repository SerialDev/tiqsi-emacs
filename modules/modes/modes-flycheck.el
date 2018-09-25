;;; modes-flycheck.el --- Tiqsi Flycheck configuration  -*- lexical-binding: t -*-

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

                                        ;reduce how often we run it for large amount of error, mostly useful for .py;
(setq flycheck-highlighting-mode 'lines)


(custom-set-faces
 '(flycheck-error ((((class color)) (:underline "Red"))))
 '(flycheck-warning ((((class color)) (:underline "Orange")))))

                                        ;    Push mark when going to the next error to go back to previous position    ;
(defadvice flycheck-next-error (before wh/flycheck-next-error-push-mark activate)
  (push-mark))


                                        ;        Make some good use of screen realstate in the windows title bar       ;
(with-eval-after-load 'flycheck
  (flycheck-title-mode))


                                        ;-{Custom Warnings}-;

(defcustom flycheck-navigation-minimum-level nil
  "The minimum level of errors to navigate.

If set to an error level, only navigate errors whose error level
is at least as severe as this one.  If nil, navigate all errors."
  :group 'flycheck
  :type '(radio (const :tag "All locations" nil)
                (const :tag "Informational messages" info)
                (const :tag "Warnings" warning)
                (const :tag "Errors" error)
                (symbol :tag "Custom error level"))
  :safe #'flycheck-error-level-p
  :package-version '(flycheck . "0.21"))


                                        ;------{Python}-----;

(setq flycheck-flake8-maximum-line-length '99)


(defun flymake-error-at-point ()
  "Show the flymake error in the minibuffer when point is on an invalid line."
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))

(add-hook 'post-command-hook 'flymake-error-at-point)

(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "Red"))))
 '(flymake-warnline ((((class color)) (:underline "Orange")))))



                                        ;--------------{Flyspell}--------------;

                                        ;------{Hydras}-----;

(defhydra hydra-flycheck
  (:pre (progn (setq hydra-lv t) (flycheck-list-errors))
        :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
        :color red
        :hint nil)
  "Flycheck"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("ESC"  nil "Exit"))

                                        ;---{Keybindings}---;

;;(add-hook 'python-mode-hook ' sdev-python-mode-hook)
(define-key python-mode-map (kbd "C-c n") 'flycheck-next-error)
(define-key python-mode-map (kbd "C-c b") 'flycheck-previous-error)
(define-key python-mode-map (kbd "C-c ee") 'flycheck-list-errors)


(provide 'modes-flycheck)

;;; modes-flycheck.el ends here
