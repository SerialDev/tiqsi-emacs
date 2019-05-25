;;; modes-magit.el --- Tiqsi Magit configuration  -*- lexical-binding: t -*-

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

(setq magit-completing-read-function 'ivy-completing-read)
(global-magit-file-mode t)


(use-package git-gutter-fringe
  :straight t
  :init
  (global-git-gutter-mode)
  (setq git-gutter-fr:side 'right-fringe))


                                        ;--{Push upstream}--;

(defun magit-push-arguments-maybe-upstream (magit-push-popup-fun &rest args)
  "Enable --set-upstream switch if there isn't a current upstream."
  (let ((magit-push-arguments
         (if (magit-get-remote) magit-push-arguments
           (cons "--set-upstream" magit-push-arguments))))
    (apply magit-push-popup-fun args)))
(advice-add 'magit-push-popup :around #'magit-push-arguments-maybe-upstream)

;;Combined with ido completion this allows pushing a new branch with P P RET:

;; NOTE: requires ido-completing-read+
(setq magit-completing-read-function #'magit-ido-completing-read)

(defun sdev/magit-stage-and-commit()
  (interactive)
  (magit-stage)
  (switch-to-buffer-other-window)
  (magit-commit-popup 'c))


                                        ;---{Keybindings}---;

;; (define-key magit-mode-map "\C-cc" 'magit-commit)
;; (define-key magit-mode-map "C-x M-g" 'magit-dispatch-popup)
                                        ;(define-key python-mode-map (kbd "C-c c") 'sdev/magit-stage-and-commit)

(provide 'modes-magit)

;;; modes-magit.el ends here
