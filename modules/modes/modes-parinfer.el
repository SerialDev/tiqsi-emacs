;;; modes-parinfer.el --- Tiqsi parinfer support for lispy languages


;; Copyright (C) 2017 Andres Mariscal <carlos.mariscal.melgar@gmail.com>
;;
;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>
;; URL: https://github.com/SerialDev/mypy-mode
;; Created: Sunday 25-03-2018 10:23:05 
;; Version: 1.0
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:
;; 
;; Maintained by Andres Mariscal -- carlos.mariscal.melgar@gmail.com
;;; License:
;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(use-package parinfer
  :ensure t
  :bind
  (("C-," . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            evil           ; If you use Evil.
            lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    (add-hook 'clojure-mode-hook #'parinfer-mode)
    ;;(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
	;;(add-hook 'python-mode-hook #'parinfer-mode)
	))

(provide 'modes-parinfer)

;;; modes-parinfer.el ends here
