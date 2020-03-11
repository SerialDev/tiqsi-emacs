;;; modes-dired.el --- Tiqsi dired configuration  -*- lexical-binding: t -*-

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


;; FROM : https://github.com/howardabrams/dot-files/blob/master/emacs.org
                                        ;This enhancement to dired hides the ugly details until you hit \211(\222 and shows the details with \221 )\222.
                                        ;I also change the [\205 ] to a simple asterisk.

;; (use-package dired-details
;;   :ensure t
;;   :init   (setq dired-details-hidden-string "* ")
;;   :config (dired-details-install))

(setq dired-details-hidden-string "* ")


                                        ; The ability to create a dired buffer based on searching for files in a directory tree with find-name-dired is fantastic
(use-package find-dired
  :straight t
  :ensure t
  :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))


                                        ;The peep project allows you to preview files before loading them into a dedicated buffer:
                                        ; (use-package peep-dired
                                        ;   :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
                                        ;   :bind (:map dired-mode-map
                                        ;               ("P" . peep-dired)))
                                        ;(use-package dired-x)


                                        ; (use-package dired-x
                                        ;   :config
                                        ;   (progn
                                        ; (setq dired-omit-verbose nil)
                                        ; ;; toggle `dired-omit-mode' with C-x M-o
                                        ; (add-hook 'dired-mode-hook #'dired-omit-mode)
                                        ; (setq dired-omit-files
                                        ;       (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))))

(provide 'modes-dired)

;;; modes-dired.el ends here
