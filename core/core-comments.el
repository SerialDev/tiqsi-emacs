;;; core-comments.el --- Tiqsi comment defuns  -*- lexical-binding: t -*-

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


(setq sdev/msg-len 76)


(defmacro tiqsi-comment--between ( &rest content)
  `(progn
     (insert (s-trim comment-start))
     (insert " ")
     ,@content
     (insert " ")
     (insert (s-trim comment-start))
     (newline)))

(defun tiqsi-comment--insert-end ()
  (interactive)
  (tiqsi-comment--between
    (insert(sdev/truncate sdev/msg-len (s-repeat 100 "-")))))

(defun tiqsi-comment--insert-sep ()
  (interactive)
  (tiqsi-comment--between
    (insert(sdev/truncate sdev/msg-len (s-repeat 100 "-   ")))))

(defun tiqsi-comment--insert-msg (string)
  (interactive "sString for inside centered message: ")
  (tiqsi-comment--between
    (insert (sdev/truncate sdev/msg-len (s-center (- sdev/msg-len 3) string)))))


(defun tiqsi-comment--insert-msg-right (string)
  (tiqsi-comment--between
    (insert (sdev/truncate sdev/msg-len (s-pad-right (- sdev/msg-len 3) " " string)))))

(defun tiqsi-comment--line-to-msg()
  (interactive)
  (move-beginning-of-line 1)
  (tiqsi-comment--insert-msg-right (s-trim-right (thing-at-point 'line t)))
  (kill-whole-line 1)
  )

(defun tiqsi-comment--line-to-msg-centered()
  (interactive)
  (move-beginning-of-line 1)
  (tiqsi-comment--insert-msg (s-trim-right (thing-at-point 'line t)))
  (kill-whole-line 1)
  )




(defmacro tiqsi-comment--between-nocomment ( &rest content)
  `(progn
     (insert " ")
     ,@content
     (insert " ")
     (insert (s-trim comment-start))
     (sdev/del-end-line)
     (move-beginning-of-line 1)
     (insert (s-trim comment-start))
     ))



(defun tiqsi-comment--line-to-msg-centered-begin()
  (interactive)
  (move-beginning-of-line 1)
  (tiqsi-comment--between-nocomment
    (insert
      (s-replace "    " "_ _ " (sdev/truncate
                                 sdev/msg-len
                                 (s-center
                                   (- sdev/msg-len 3)
                                   (s-prepend "   /¯¯¯"
                                     (s-append " ¯¯¯\\  "
                                       (s-trim-right
                                         (thing-at-point 'line t)))))))
      )
    )

  )

(defun tiqsi-comment--line-to-msg-centered-end()
  (interactive)
  (move-beginning-of-line 1)
  (tiqsi-comment--between-nocomment
    (insert
      (s-replace "    " "¯ ¯ " (sdev/truncate sdev/msg-len
                                 (s-center (- sdev/msg-len 3) (s-prepend "  \\_ _"
                                                                (s-append " _ _/ "    (s-trim-right (thing-at-point 'line t))))))))
    )
  )

(defun tiqsi-comment--enclose (string)
  (interactive "sString to enclose with: ")

  (move-beginning-of-line 1)
  (newline)
  (insert string)
  (move-beginning-of-line 1)
  (tiqsi-comment--line-to-msg-centered-end)
  (avy-pop-mark)
  (move-beginning-of-line 1)
  (insert string)
  (move-beginning-of-line 1)
  (tiqsi-comment--line-to-msg-centered-begin)
  (move-end-of-line 1)
  (newline)
  )

;; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Keybindings ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;

(global-set-key (kbd "C-;") 'tiqsi-comment--line-to-msg)
(global-set-key (kbd "C-:") 'tiqsi-comment--line-to-msg-centered)
(global-set-key (kbd "C-'") 'tiqsi-comment--insert-end)
(global-set-key (kbd "C-@") 'tiqsi-comment--insert-sep)
(global-set-key (kbd "C-c >") 'tiqsi-comment--line-to-msg-centered-end)
(global-set-key (kbd "C-c <") 'tiqsi-comment--line-to-msg-centered-begin)
(global-set-key (kbd "C-M-=") 'sdev/sprintf-debug)
;; (define-key python-mode-map (kbd "C-c t e") 'sdev/py-try-catch)

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Keybindings _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \ַַַ  Keybindings ַַַ/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;


(provide 'core-comments)

;;; core-comments.el ends here
