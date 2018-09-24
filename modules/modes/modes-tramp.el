;;; modes-tramp.el --- Tiqsi tramp configuration  -*- lexical-binding: t -*-

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


; (defun python-tramp-hook()
; (setq
;  python-shell-interpreter "ipython"
;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;  python-shell-interpreter-args "--simple-prompt -i"
;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;  python-shell-completion-setup-code
;    "from IPython.core.completerlib import module_completion"
;  python-shell-completion-string-code
;    "';'.join(module_completion('''%s'''))\n"
;  python-shell-completion-string-code
;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
;(add-hook 'python-mode-hook 'python-tramp-hook)


(provide 'modes-tramp)

;;; modes-tramp.el ends here
