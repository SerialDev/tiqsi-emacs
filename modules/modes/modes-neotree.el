;;; modes-neotree.el --- Tiqsi neotree support  -*- lexical-binding: t -*-

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


(defun tiqsi-java-hook()

  (jdee-flycheck-enable-p)
  (neotree-show)
  )
(setq jdee-server-dir "/jdee-server/target/")

(use-package neotree
  :straight t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    (setq neotree-smart-optn t
          neo-window-fixed-size nil)
  ;; Disable linum for neotree
    (add-hook 'jdee-mode-hook 'tiqsi-java-hook )
    ))






                                        ;---{Keybindings}---;

(define-key global-map (kbd "M-<f9>") 'neotree-toggle)


(provide 'modes-neotree)

;;; modes-neotree.el ends here
