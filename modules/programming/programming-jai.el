;;; programming-zig.el --- Tiqsi Jai mode support  -*- lexical-binding: t -*-

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


;; JAI mode
(straight-use-package
  '(jai-mode
     :type git
     :host github
     :ensure t
     :repo "krig/jai-mode"
     ))

(defun set-shell-to-current-buffer-location ()
  "Set the shell's working directory to the current buffer's location."
  (let ((buffer-file-directory (file-name-directory (or (buffer-file-name) default-directory))))
    (setq default-directory buffer-file-directory)))


(defun jai-run-current ()
  "Run the current file's executable using Jai with the current buffer's file name without extension."
  (interactive)
  (set-shell-to-current-buffer-location)
  (let ((buffer-file-name (buffer-file-name)))
    (if buffer-file-name
      (let* ((file-name-sans-extension (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
              (executable-path (concat (file-name-directory buffer-file-name) file-name-sans-extension)))
        (if (file-executable-p executable-path)
          (compile-other (concat "./" file-name-sans-extension))
          (message "Executable file not found: %s" executable-path)))
      (message "Buffer is not visiting a file"))))



(defun jai-compile-current ()
  "Build the current file using Jai with the current buffer's file name."
  (interactive)
  
  (let ((buffer-file-name (buffer-file-name)))
    (if buffer-file-name
      (compile-other (format "jai %s " buffer-file-name))
      (message "Buffer is not visiting a file"))))

(defun jai-compile-and-run-current ()
  "Build and run the current file using Jai with the current buffer's file name."
  (interactive)
  (set-shell-to-current-buffer-location)
  (let ((buffer-file-name (buffer-file-name)))
    (if buffer-file-name
      (let ((file-name-sans-extension (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
        (compile-other (format "jai %s && ./%s"
                         buffer-file-name
                         file-name-sans-extension)))
      (message "Buffer is not visiting a file"))))


(define-key jai-mode-map (kbd "C-c C-c") 'jai-compile-and-run-current)
(define-key jai-mode-map (kbd "C-c C-b") 'jai-compile-current)
(define-key jai-mode-map (kbd "C-c C-r") 'jai-run-current)
;; (define-key zig-mode-map (kbd "M-z") 'hydra-ziggy/body)


(provide 'programming-jai)

;;; programming-zig.el ends here
