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


(defun sdev/other-window (&optional arg)
  "Wrap `other-window' and skip *vterm* buffer."
  (interactive "p")
  (ignore-errors
    (let
      ((win (selected-window))
	(start-win (selected-window)))
      (catch 'done
	(while t
	  (setq win (other-window arg))
	  (when (eq win start-win)
	    (throw 'done nil))
	  (unless (string= (buffer-name (window-buffer win)) "*vterm*")
	    (throw 'done (select-window win))))))))



(defun sdev/set-or-jump-windows ()
  "Run ' if there's more than one window, otherwise set windows."
  (interactive)
  (if (= (count-windows) 1)
    (sdev/set-windows)
    (call-interactively #'sdev/other-window)))


(defun sdev/jump-window (&optional frame)
  (interactive)
  (if
    (> (count-unique-visible-buffers) 4)
    (call-interactively #'ace-window)
    (call-interactively #'sdev/set-or-jump-windows)))


(defun sdev/jump-to-vterm ()
  "Jump to the *vterm* buffer."
  (interactive)
  (let ((win (get-buffer-window "*vterm*")))
    (if win
      (select-window win)
      (error "No *vterm* buffer found"))))




                                        ;---{keybindings}---;

(global-set-key (kbd "M-w") 'sdev/jump-window)
(global-set-key (kbd "C-t") 'sdev/jump-to-vterm)



(global-set-key (kbd "C-c jj") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c jw") 'ace-window)
(global-set-key (kbd "C-c js") 'ace-swap-window)


(provide 'modes-avy)

;;; modes-avy.el ends here
