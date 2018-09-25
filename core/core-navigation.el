;;; core-navigation.el --- Tiqsi Navigation defuns  -*- lexical-binding: t -*-

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

                                        ;----{Searching}----;

                                        ; Commands
(set-variable 'grep-command "grep -irHn ")
(when tiqsi-win32
  (set-variable 'grep-command "findstr -s -n -i -l "))


;; find matching parenthesis (% command in vim)
(defun match-paren (arg)
  "Go to the matching parenthesis, if on parenthesis; otherwise,
insert `%'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


                                        ;--{Nav primitives}-;

(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
  )

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
  )


                                        ;----{Nav Shift}----;


(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

(drag-stuff-global-mode 1)

;; Shift the selected region right if distance is positive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))


                                        ;--{Move txt ^ & v}-;

(defun sk/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg)
          (when (and (eval-when-compile
                       '(and (>= emacs-major-version 24)
                             (>= emacs-minor-version 3)))
                     (< arg 0))
            (forward-line -1)))
        (forward-line -1))
      (move-to-column column t)))))


(defun sk/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (sk/move-text-internal arg))
(defun sk/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (sk/move-text-internal (- arg)))

                                        ;--{Find elements}--;

(defun find-assignment ()
  (if (re-search-forward
       "[^<>=!]=\\|\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>="
       (save-excursion (end-of-line) (point)) t)
      (progn
        (goto-char (match-beginning 0))
        (if (looking-at ".==")
            nil
          (if (looking-at "\\+=\\|-=\\|\\*=\\|/=\\|&=\\||=\\|\\^=\\|<<=\\|>>=")
              (set-mark (match-end 0))
            (forward-char 1)
            (set-mark (1+ (point))))
          (delete-horizontal-space)
          t))
    nil))


                                        ;-----{Line Nav}----;

;;Toggle between indentation level and beginning of line

(defun sk/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

                                        ;{Select Current Line};

;; alternative to expand region, depends on smarter-move-beginning-of-line
(defun sk/select-inside-line ()
  "Select the current line"
  (interactive)
  (sk/smarter-move-beginning-of-line 1)
  (set-mark (line-end-position))
  (exchange-point-and-mark))

                                        ;{Select Around Line};

(defun sk/select-around-line ()
  "Select line including the newline character"
  (interactive)
  (sk/select-inside-line)
  (next-line 1)
  (sk/smarter-move-beginning-of-line 1))


                                        ;{Select Python Block};

;; to work with expand-region
(defun sk/mark-inside-python-block ()
  "Mark inside a python block"
  (interactive)
  (er/mark-python-block)
  (next-line 1))


                                        ;--{Efficient nav}--;

(use-package avy
  :straight t
  :ensure t
  ;; :bind ("C-c <SPC>" . avy-goto-word-1) ;; Need to get used to it
  :config (progn
            (setq avy-background t)))
                                        ;(key-chord-define-global "jj"  #'avy-goto-word-1)


                                        ;-----{Buffers}-----;

(defun malb/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

                                        ;------{Window}-----;

;; swap 2 windows
(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))


(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;;;###autoload
(defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


;;;###autoload
(defun buf-move-up ()
  "Swap the current buffer and the buffer above the split.
If there is no split, ie now window above the current one, an
error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
If there is no split, ie now window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
         (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(defun my-switch-to-buffer (buffer)
  (interactive
   (list (ido-read-buffer "Switch to buffer: ")))
  ;; get-buffer-window (with second argument t) will return
  ;; nil if the buffer is not open in any window across any
  ;; frame
  (unless (get-buffer-window buffer 0)
    (pop-to-buffer buffer nil t)))


                                        ;---{Keybindings}---;

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)
(global-set-key (kbd "C-c g") 'my-toggle-window-split)

(global-set-key (kbd "C-x b")  'helm-mini)
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)


(global-set-key (kbd "C-c l") 'buf-move-left)
(global-set-key (kbd "C-c r") 'buf-move-right)
(global-set-key (kbd "C-c u") 'buf-move-up)
(global-set-key (kbd "C-c d") 'buf-move-down)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [home] 'beginning-of-line)
(define-key global-map [end] 'end-of-line)
(define-key global-map [pgup] 'forward-page)
(define-key global-map [pgdown] 'backward-page)
(define-key global-map [C-next] 'scroll-other-window)
(define-key global-map [C-prior] 'scroll-other-window-down)

;; Navigation
(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)
(global-set-key (kbd "C-S-a") 'move-end-of-line)
(define-key global-map (kbd "S-<left>") 'open-rectangle)
(define-key global-map (kbd "C-^") 'what-hexadecimal-value)
(define-key global-map (kbd "C-M-#") 'insert-separator)
(define-key global-map "\e " 'set-mark-command)
(define-key global-map [M-a] 'yank)
(define-key global-map [M-z] 'kill-region)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)
(define-key global-map [M-:] 'View-back-to-mark)
(define-key global-map "\e;" 'exchange-point-and-mark)
(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)
(define-key global-map [M-n] 'next-error)
(define-key global-map [M-N] 'previous-error)
(define-key global-map "\egl" 'goto-line)
(define-key global-map "\ej" 'imenu)
;; remap C-a to `smarter-move-beginning-of-line'

(global-set-key [remap move-beginning-of-line]
                'sk/smarter-move-beginning-of-line)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c w") 'rotate-windows)

(global-set-key (kbd "C-c 3") 'my-switch-to-buffer)
(global-set-key (kbd "C-c 2") 'next-buffer)
(global-set-key (kbd "C-c 1") 'previous-buffer)
                                        ;(global-set-key (kbd "%") 'match-paren)


(provide 'core-navigation)

;;; core-navigation.el ends here
