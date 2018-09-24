;;; modes-dash.el --- Tiqsi dash configuration support  -*- lexical-binding: t -*-

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
;                  TODO compatibility issues with non UNIX OS                  ;

(eval-after-load "helm-dash"
  '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))
  ;; (setq-local helm-dash-docsets-path "../../tools/dash/")

(setq helm-dash-browser-func 'browse-url)
(setq helm-dash-browser-func 'eww)


;(defun go-doc ()
;  (interactive)
;  (setq-local helm-dash-docsets '("Go")))
;; (setq-local helm-dash-docsets-path "C:/Users/Carlos/Desktop/Workspace/emacs_dir/NewEmacs/tools/dash/")
;(add-hook 'go-mode-hook 'go-doc)
;(defun py-doc ()
;  (interactive)
;  (setq-local helm-dash-docsets '("Python_3"))
;  (setq-local helm-dash-docsets '("Pandas"))
;  (setq-local helm-dash-docsets '("Numpy")))
;  
;(require 'helm-dash)

;      helm-dash-min-length 3
;      ;; helm-dash-completing-read-func 'completing-read ; 'completing-read, 'ido-completing-read
;      helm-dash-browser-func 'browse-url ; 'browse-url, 'eww
;      ;; helm-dash-connections
;      helm-dash-common-docsets '(
;	                              "Python_3" "Numpy" "Pandas"
;                           
;                                 )
;      ;; helm-dash-docsets
;      )
	  
;(global-set-key (kbd "C-c d") 'helm-dash-at-point) 
;(add-hook 'python-mode-hook 'py-doc)



(provide 'modes-dash)

;;; modes-dash.el ends here
