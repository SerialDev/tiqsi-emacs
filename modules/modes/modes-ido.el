;;; modes-ido.el --- Tiksi IDO configuration  -*- lexical-binding: t -*-

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

(ido-mode 1)


                                        ; IDO mode
                                        ;(use-package ido
                                        ;  :ensure t
                                        ;  :init  (setq ido-enable-flex-matching t
                                        ;               ido-ignore-extensions t
                                        ;               ido-use-virtual-buffers t
                                        ;               ido-everywhere t)
                                        ;  :config
                                        ;  (ido-mode 1)
                                        ;  (ido-everywhere 1)
                                        ;  (add-to-list 'completion-ignored-extensions ".pyc"))

                                        ;Add to IDO, the FLX package:
                                        ;(use-package flx-ido
                                        ;   :ensure t
                                        ;   :init (setq ido-enable-flex-matching t
                                        ;               ido-use-faces nil)
                                        ;   :config (flx-ido-mode 1))

                                        ; Make IDO work vertically
                                        ;(use-package ido-vertical-mode
                                        ;  :ensure t
                                        ;  :init               ; I like up and down arrow keys:
                                        ;  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
                                        ;  :config
                                        ;  (ido-vertical-mode 1))


(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark (point))
      (goto-char position))))


(provide 'modes-ido)

;;; modes-ido.el ends here
