;;; core-editing.el --- Tiqsi Editing  -*- lexical-binding: t -*-

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




;;                                 Commenting                                ;


(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let ((start (line-beginning-position))
         (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
        end (save-excursion
              (goto-char (region-end))
              (end-of-line)
              (point))))
    (comment-or-uncomment-region start end)))


(with-system darwin
  (global-set-key (kbd "M-\\") (lambda () (interactive) (comment-or-uncomment-region-or-line))))



(defun append-as-kill ()
  "Performs copy-region-as-kill as an append."
  (interactive)
  (append-next-kill)
  (copy-region-as-kill (mark) (point)))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Commenting _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Replacing ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _    ;

(defun tiqsi-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
                    (narrow-to-region (mark) (point))
                    (goto-char (point-min))
                    (replace-string old-word new-word)
                    )))


(defun tiqsi-replace-string (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)))


(defun perlish-fix-regexps (regexp)
  "Simple translation of a perlish REGEXP to an emacs one."
  (let ( (new-pattern regexp) )
    (setq new-pattern (replace-regexp-in-string "(" "\\\\(" new-pattern))
    (setq new-pattern (replace-regexp-in-string ")" "\\\\)" new-pattern))
    (setq new-pattern (replace-regexp-in-string "|" "\\\\|" new-pattern))
    (setq new-pattern (replace-regexp-in-string "\\\\\"" "\"" new-pattern))
    new-pattern))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Replacing _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Saving ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;


(defun tiqsi-save-buffer ()
  "Save the buffer after untabifying it."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (untabify (point-min) (point-max))))
  (save-buffer))

;; ------------------------------------------------------------------------- ;
;;                         Run linters asyncronously                         ;
;; ------------------------------------------------------------------------- ;


(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
    '(npx "prettier"
       "--trailing-comma"  "es5"
       "--bracket-spacing" "true"
       "--single-quote"    "true"
       "--semi"            "false"
       "--print-width"     "80"
       file))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (apheleia-global-mode t))



;; Add a few more formatters modifying 'apheleia-mode-alist and  'apheleia-formatters

;; (setq apheleia-mode-alist
;;       (append apheleia-mode-alist
;;               '(
;;                 (python-mode . (isort black)))))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Saving _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Inserting ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _    ;



(defun md-insert-base64 (b64-name b64-data)
  (interactive "sEnter name:
sEnter b64 data: ")
  (insert
    (s-prepend
      (s-prepend "![" (s-prepend b64-name "]"))
      (s-prepend "(data:image/png;base64," (s-prepend b64-data ")" )))
    )
  )


(defun md-insert-url (link-name link-url)
  (interactive "sEnter name:
sEnter b64 data: ")
  (insert
    (s-prepend
      (s-prepend "[" (s-prepend link-name "]"))
      (s-prepend "(" (s-prepend link-url ")" )))
    )
  )



(defun tiqsi-copy-rectangle (start end)
  "Copy the region-rectangle instead of `kill-rectangle'."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end)))


(defun add-full-stop ()
  "Terminate each line with a full stop."
  (interactive "*")
  (while (re-search-forward "$")
    (insert ".")
    (forward-char )))

(defun sdev/timestamp()
  (interactive)
                                        ; If you want to insert date and time, you can use:
  (insert(format-time-string "%A %d-%m-%Y %H:%M:%S ")))

;;; editing-defuns.el --- Basic text editing defuns -*- lexical-binding: t; -*-
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun new-line-in-between ()
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command))

;;; Still to find a good keymap for this one
(defun new-line-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{" 1) (looking-at "}"))
                           (and (looking-back ">" 1) (looking-at "<"))
                           (and (looking-back "(" 1) (looking-at ")"))
                           (and (looking-back "\\[" 1) (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
    (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
            (end (or end (region-end)))
            (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))


(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
    (let ((beg (region-beginning))
           (end (region-end)))
      (duplicate-region arg beg end)
      (one-shot-keybinding "d" (? (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Inserting _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;

;; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Indentation ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (point-to-register 'o)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
  (jump-to-register 'o))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Indentation _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;


;; _ _ _ _ _ _ _ _ _ _ _ _   /¯¯¯ Modification ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;


;; unrelated, but a nice spot for it
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
        (progn
          (goto-char start)
          (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))


(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))


(defun my-narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (when (fboundp 'evil-exit-visual-state) ; There's probably a nicer way to do this
    (evil-exit-visual-state))
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun tiqsi-goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
         (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
      (cond
        ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
        ((and closest-ahead (not closest-behind)) closest-ahead)
        ((and closest-behind (not closest-ahead)) closest-behind)
        ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
        ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
        (closest-ahead)))))

;; Once you go to the closest number, you might want to change it. These functions are useful for that.

(defun tiqsi-incs (s &optional num)
  (interactive "p")
  (let* ((inc (or num 1))
          (new-number (number-to-string (+ inc (string-to-number s))))
          (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
      (s-pad-left (length s) "0" new-number)
      new-number)))

(defun tiqsi-change-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
            (looking-back "[0-9]"))
    (tiqsi-goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (tiqsi-incs (match-string 0) arg) nil nil)))

(defun tiqsi-subtract-number-at-point (arg)
  (interactive "p")
  (tiqsi-change-number-at-point (- arg)))

(defun transpose-params ()
  "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
  (interactive)
  (let* ((end-of-first (cond
                         ((looking-at ", ") (point))
                         ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                         ((looking-back ", ") (- (point) 2))
                         (t (error "Place point between params to transpose."))))
          (start-of-first (save-excursion
                            (goto-char end-of-first)
                            (move-backward-out-of-param)
                            (point)))
          (start-of-last (+ end-of-first 2))
          (end-of-last (save-excursion
                         (goto-char start-of-last)
                         (move-forward-out-of-param)
                         (point))))
    (transpose-regions start-of-first end-of-first start-of-last end-of-last)))

(defun move-forward-out-of-param ()
  (while (not (looking-at ")\\|, \\| ?}\\| ?\\]"))
    (cond
      ((point-is-in-string-p) (move-point-forward-out-of-string))
      ((looking-at "(\\|{\\|\\[") (forward-list))
      (t (forward-char)))))


(defun move-backward-out-of-param ()
  (while (not (looking-back "(\\|, \\|{ ?\\|\\[ ?"))
    (cond
      ((point-is-in-string-p) (move-point-backward-out-of-string))
      ((looking-back ")\\|}\\|\\]") (backward-list))
      (t (backward-char)))))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \_ _ Modification _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Toggle ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;

;;  http://www.mostlymaths.net/2016/09/more-emacs-configuration-tweaks.html
(try-require 'origami)

(eval-after-load 'origami
  '(progn
     (defun rb-show-only (buffer point)
       (interactive (list (current-buffer) (point)))
       (progn (origami-show-only-node buffer point)
         (minimap-new-minimap)))

     (defun rb-toggle-rec (buffer point)
       (interactive (list (current-buffer) (point)))
       (progn (origami-recursively-toggle-node buffer point)
         (minimap-new-minimap)))
     (global-origami-mode 1)
     ))

(use-package vimish-fold
  :straight t
  :ensure t
  :commands (vimish-fold-toggle
              vimish-fold))

;; Fold indentation https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
;; Quite nice for python mode TODO: fix for using with ipynb buffers in EIN
(defun tiqsi-toggle-indent-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
        (if selective-display nil (or col 1))))))

(add-hook 'python-mode-hook
  (lambda () (define-key python-mode-map (kbd "C-c f") 'tiqsi-toggle-indent-fold)))

;; yas-expand is run first and does what it has to, then it calls malb/indent-or-complete.

;; This function then hopefully does what I want:

;; if a region is active, just indent
;; if we’re looking at a space after a non-whitespace character, we try some company-expansion
;; otherwise call whatever would have been called otherwise.

(defun malb/indent-or-complete (&optional arg)
  (interactive "P")
  (cond
    ;; if a region is active, indent
    ((use-region-p)
      (indent-region (region-beginning)
        (region-end)))
    ;; if the next char is space or eol, but prev char not whitespace
    ((and (not (active-minibuffer-window))
       (or (looking-at " ")
         (looking-at "$"))
       (looking-back "[^[:space:]]")
       (not (looking-back "^")))

      (ac-expand arg))

    ;; no whitespace anywhere
    ((and (not (active-minibuffer-window))
       (looking-at "[^[:space:]]")
       (looking-back "[^[:space:]]")
       (not (looking-back "^")))
      (origami-toggle-node (current-buffer) (point)))

    ;; by default just call whatever was bound
    (t
      (let ((fn (or (lookup-key (current-local-map) (kbd "TAB"))
                  'indent-for-tab-command)))
        (if (not (called-interactively-p 'any))
          (fn arg)
          (setq this-command fn)
          (call-interactively fn))))))

(defun current-quotes-char ()
  (nth 3 (syntax-ppss)))

(defalias 'point-is-in-string-p 'current-quotes-char)

(defun move-point-forward-out-of-string ()
  (while (point-is-in-string-p) (forward-char)))

(defun move-point-backward-out-of-string ()
  (while (point-is-in-string-p) (backward-char)))

(defun alternate-quotes-char ()
  (if (eq ?' (current-quotes-char)) ?\" ?'))

(defun toggle-quotes ()
  (interactive)
  (if (point-is-in-string-p)
    (let ((old-quotes (char-to-string (current-quotes-char)))
           (new-quotes (char-to-string (alternate-quotes-char)))
           (start (make-marker))
           (end (make-marker)))
      (save-excursion
        (move-point-forward-out-of-string)
        (backward-delete-char 1)
        (set-marker end (point))
        (insert new-quotes)
        (move-point-backward-out-of-string)
        (delete-char 1)
        (insert new-quotes)
        (set-marker start (point))
        (replace-string new-quotes (concat "\\" new-quotes) nil start end)
        (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
    (error "Point isn't in a string")))


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Toggle _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Deleting ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _     ;;


(defun sdev/del-beg-line()
  (interactive)
  (let ((beg(point ))) (sk/smarter-move-beginning-of-line())
    (delete-region beg(point))))

(defun sdev/del-end-line()
  (interactive)
  (let ((beg(point ))) (move-end-of-line())
    (delete-region beg(point))))

;; Kill whitespace
(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

(defun remove-blank-lines ()
  "Delete blank lines from the current buffer."
  (interactive "*")
  (while (re-search-forward "^$")
    (kill-line)))

(defun flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.  Restrict to \
region if active."
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Deleting _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯     ;;


;;                                 COLOURIZE                                 ;
;; ------------------------------------------------------------------------- ;

(defun colourize--red ()
  "Insert ASCII codes for red text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[91m' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))



(defun colourize--mustard ()
  "Insert ASCII codes for mustard text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[38;5;172m' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))


(defun colourize--yellow ()
  "Insert ASCII codes for yellow text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[33m*' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))


(defun colourize--orange ()
  "Insert ASCII codes for orange text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[38;5;214m' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))



(defun colourize--blue ()
  "Insert ASCII codes for blue text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[38;5;75m?' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))

(defun colourize--cyan ()
  "Insert ASCII codes for cyan text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[36m' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))


(defun colourize--purple ()
  "Insert ASCII codes for purple text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[35m{' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))


(defun colourize--magenta ()
  "Insert ASCII codes for magenta text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[95m^' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert " + '\\033[0m'"))))))







(defun colourize--green ()
  "Insert ASCII codes for green text around the first quoted string on the current line."
  (interactive)
  (save-excursion
    (let ((start (point))
           (end (point-max)))
      ;; Find the first quote
      (when (re-search-forward "\\(['\"]\\)" end t)
        (backward-char 1)
        (insert "'\\033[32m[' +" )
        (forward-char 6)
        ;; Find the closing quote
        (when (re-search-forward "\\(['\"]\\)" end t)
          (insert "' + \\033[0m'"))))))

					; ------------------------------------------------------------------------- ;



;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Kill Ring ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _    ;;

(use-package undo-tree
  :straight t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(defun pre-process-kill-ring-element (element)
  (replace-regexp-in-string "^[[:space:]]+" ""
    (replace-regexp-in-string "[[:space:]]+$" "" (substring-no-properties element))))

(defun preprocess-kill-ring ()
  (let ((result nil)
         (element nil))
    (dolist (element kill-ring)
      (progn
        (setq element (pre-process-kill-ring-element element))
        (when (not (or
                     (eq 0 (length element))
                     (string-match-p "[\r\n]+" element)))
          (setq result (cons element result)))))
    (reverse result)))

(defun browse-kill-ring ()
  (interactive)
  (insert (completing-read "Pick an element: "
            (preprocess-kill-ring))))


(defun my-kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
      (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

(defun my-kill-word-at-point ()
  "Kill the word at point."
  (interactive)
  (my-kill-thing-at-point 'word))

(defun file-name-no-path()
  (interactive)

  (s-prepend(file-name-base (buffer-file-name))
    (s-prepend "." (file-name-extension (buffer-file-name))))
  )


(defun print-current()
  (interactive)
  (let ((current-data (thing-at-point 'symbol) ))
    (progn (my-kill-word-at-point)
      (insert
        (s-prepend "\""
          (s-prepend
            (s-prepend
              (s-prepend (file-name-no-path) "  :: line " )
              (s-prepend (number-to-string (line-number-at-pos)) " :: var: " ))
            (s-prepend(s-prepend current-data "\", ") current-data)))
        )
      )))



(defun add-hooks (hooks functions)
  (let ((hooks (ensure-list hooks))
	 (functions (if (functionp functions) (list functions) functions)))
    (dolist (hook hooks)
      (dolist (func functions)
	(add-hook hook func)))))



(setq current-data "test")

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Kill Ring _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;;

;; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Keybindings ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;;

(define-key global-map [f8] 'tiqsi-replace-string)


;; (define-key global-map (kbd "\e0B") 'next-line)

(define-key input-decode-map (kbd "\eOA") [down])
(define-key input-decode-map "\eOB" [down])




;; Editting
(define-key global-map (kbd "C-q" )'copy-region-as-kill)
(define-key global-map (kbd "C-f" )'yank)
(define-key global-map "" 'nil)
(define-key global-map "" 'rotate-yank-pointer)
(define-key global-map "\eu" 'undo)
(define-key global-map "\e6" 'upcase-word)
(define-key global-map "\e^" 'captilize-word)
(define-key global-map "\e." 'fill-paragraph)
(define-key global-map "\el" 'tiqsi-replace-in-region)
(define-key global-map "\eo" 'query-replace)
(define-key global-map "\eO" 'tiqsi-replace-string)
;; \377 is alt-backspace
(define-key global-map "\377" 'backward-kill-word)
(define-key global-map [M-delete] 'kill-word)
(define-key global-map "\e[" 'start-kbd-macro)
(define-key global-map "\e]" 'end-kbd-macro)
(define-key global-map "\e'" 'call-last-kbd-macro)
;; Buffers
(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)

(define-key global-map (kbd "S-<down>") 'open-line-below)
(define-key global-map (kbd "S-<up>") 'open-line-above)
(define-key global-map (kbd "S-<right>") 'new-line-in-between)
(define-key global-map (kbd "C-+") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)
(global-set-key (kbd "C-d") 'sdev/del-beg-line)
(define-key global-map "\e#"'comment-or-uncomment-region-or-line)

(define-key global-map (kbd "C-c m") 'origami-toggle-node)
(define-key global-map (kbd "C-c TAB") 'tiqsi-toggle-indent-fold)
(define-key global-map [M-q] 'append-as-kill)

(global-set-key (kbd "C-S-d") 'sdev/del-end-line)
(global-set-key (kbd "C-c q") 'toggle-quotes)
(global-set-key (kbd "M-n") 'tiqsi-goto-closest-number)
(global-set-key (kbd "C-c t t") 'sdev/timestamp)
(global-set-key (kbd "C-c <deletechar>") 'kill-whitespace)
(global-set-key (kbd "C-c <up>") 'drag-stuff-up)
(global-set-key (kbd "C-c <down>") 'drag-stuff-down)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)

;;(bind-key "<tab>" #'malb/indent-or-complete)
;; (global-set-key (kbd "C-S-<left>") 'corral-parentheses-backward)
;; (global-set-key (kbd "C-S-<right>") 'corral-parentheses-forward)

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Keybindings _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;;

(provide 'core-editing)

;;; core-editing.el ends here
