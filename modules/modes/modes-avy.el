;;; modes-avy.el --- Tiqsi avy configuration  -*- lexical-binding: t -*-

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

(defun count-unique-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list frame)))))

(defun sdev/jump-window (&optional frame)
  (interactive)
  (if
      (> (count-unique-visible-buffers) 4)
      (call-interactively #'ace-window)
    (call-interactively #'other-window)))



                                        ;---{keybindings}---;

(global-set-key (kbd "M-w") 'sdev/jump-window)
(global-set-key (kbd "C-c jj") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c jw") 'ace-window)
(global-set-key (kbd "C-c js") 'ace-swap-window)


(provide 'modes-avy)

;;; modes-avy.el ends here
