;;; programming-nim.el --- Tiqsi Assembly mode support  -*- lexical-binding: t -*-

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


(use-package nim-mode
  :straight t
  :config (progn
	    (add-hook 'nim-mode-hook 'nimsuggest-mode)
	    (add-hook 'nimsuggest-mode-hook 'company-mode)  ; auto complete package
	    (add-hook 'nimsuggest-mode-hook 'company-mode)  ; auto complete package
	    (add-hook 'nimsuggest-mode-hook 'flycheck-nimsuggest-setup)
	    ))


(provide 'programming-nim)

;;; programming-nim.el ends here
