;;; programming-text.el --- Tiqsi text mode handling & configuration  -*- lexical-binding: t -*-

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

;;; Code:

(defun tiqsi-big-fun-text-hook ()
                                        ; 4-space tabs
  (setq tab-width 4
        indent-tabs-mode nil)
                                        ; Newline indents, semi-colon doesn't
  (define-key text-mode-map "\C-m" 'newline-and-indent)
                                        ; Prevent overriding of alt-s
  (define-key text-mode-map "\es" 'tiqsi-save-buffer)
  )
(add-hook 'text-mode-hook 'tiqsi-big-fun-text-hook)

(provide 'programming-text)

;;; programming-text.el ends here
