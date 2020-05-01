;;; programming-zig.el --- Tiqsi Zig mode support  -*- lexical-binding: t -*-

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


(use-package zig-mode
  :straight t
  :config (progn
	    (add-hook 'zig-mode-hook 'zig-file-coding-system)
	    ))


(defun zig-file-coding-system ()
  (with-current-buffer (current-buffer)
    (if (string-match "\\.d?zig\\'" buffer-file-name)
        (setq buffer-file-coding-system 'utf-8-unix)
      nil)
    ))

(defun zig-compile ()
  (interactive)
  (async-shell-command
   (s-prepend "zig build-exe " (buffer-name))
        "*zig-compilation*" "*Zig-error*"
		       ))

(defun zig-test()
  (interactive)
  (async-shell-command
   (s-prepend "zig test " (buffer-name))
   "*zig-test*" "*Zig-error*"))


(defun zig-run()
  (interactive)
  (async-shell-command
   (s-prepend
    (s-prepend "zig build-exe " (buffer-name)) " && ./main")
     "*zig-compilation*" "*Zig-error*"
   )
  )


(define-key zig-mode-map (kbd "C-c C-c") 'zig-compile)
(define-key zig-mode-map (kbd "C-c C-t") 'zig-test)
(define-key zig-mode-map (kbd "C-c C-r") 'zig-run)



(provide 'programming-zig)

;;; programming-zig.el ends here
