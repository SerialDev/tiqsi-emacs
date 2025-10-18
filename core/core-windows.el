;;; core-windows.el --- Window management functions  -*- lexical-binding: t -*-

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
;; Consolidated window management functions

;;; Code:

;; ------------------------------------------------------------------------- ;
;;                         Window Comparison Functions                       ;
;; ------------------------------------------------------------------------- ;

(defun search-current-line-in-other-window ()
  "Search for the current line's exact text in the other window's buffer.

Gets the text of the line containing the point (trimmed), switches to the
other window, and searches for that exact text. Reports whether the
line text was found or not found in the other buffer."
  (interactive)
  (let* (;; --- Store current state ---
          (current-win (selected-window))
          (current-buf (current-buffer))
          (current-point (point))
          ;; --- Get line text ---
          (current-line-text
            (buffer-substring-no-properties (line-beginning-position)
              (line-end-position)))
          ;; Trim leading/trailing whitespace for robustness
          (search-text (string-trim current-line-text))
          ;; --- Find the 'other' window ---
          (other-win (next-window current-win nil 'visible)) ; Avoid minibuffer
          (other-buf nil)
          (found-in-other nil))

    ;; --- 1. Basic Validation ---
    (when (string-empty-p search-text)
      (user-error "Current line appears empty or contains only whitespace."))

    (unless other-win
      (user-error "Could not find an 'other' visible window."))

    ;; --- 2. Perform Search in Other Window ---
    (setq other-buf (window-buffer other-win))
    (with-current-buffer other-buf
      (save-excursion       ; Don't permanently move point in other buffer
        (save-restriction ; Search entire buffer even if narrowed
          (widen)
          (goto-char (point-min)) ; Start search from beginning
          ;; Use search-forward for literal string search
          ;; 'nil' means don't signal error if not found
          ;; 't' means search only once
          (if (search-forward search-text nil t)
            (setq found-in-other t)
            (setq found-in-other nil)))))

    ;; --- 3. Report Result (back in original window) ---
    (select-window current-win) ; Restore original window focus

    (if found-in-other
      (message "✅ Found matching line in '%s': \"%s\""
        (buffer-name other-buf)
        search-text)
      (message "❓ Line NOT FOUND in '%s': \"%s\" (Searched from '%s')"
        (buffer-name other-buf)
        search-text
        (buffer-name current-buf)))
    ))

(defun compare-entity-across-windows ()
  (interactive)
  (let* ((defun-size-limit 300000)
          (analysis-search-chars 10000)
          (current-win (selected-window))
          (current-buf (current-buffer))
          (current-point (point))
          (original-start nil)
          (original-end nil)
          (entity-name nil)
          (first-meaningful-line nil)
          (current-line-search-text nil)
          (other-win nil)
          (other-buf nil)
          (found-in-other nil)
          (comparison-mode :line)
          (comparison-result :unknown)
          (message-text nil)
          (target-line-in-other nil))

    ;; Identify current defun boundaries & entity name
    (condition-case nil
      (save-excursion
        (goto-char current-point)
        (beginning-of-defun)
        (setq original-start (point))
        (end-of-defun)
        (setq original-end (point))
        (when (and original-start original-end
                (< original-start current-point)
                (<= (- original-end original-start) defun-size-limit))
          (goto-char original-start)
          (when (re-search-forward "^\\s-*\\(?:async\\s-+\\)?\\(?:def\\|class\\)\\s-+\\(\\w+\\)" original-end t)
            (setq entity-name (match-string 1)))
          (goto-char original-start)
          (while (and (< (point) original-end) (not first-meaningful-line))
            (let ((line-text (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
              (unless (or (string-empty-p line-text) (string-prefix-p "#" line-text))
                (setq first-meaningful-line line-text)))
            (forward-line 1))
          (when (and entity-name first-meaningful-line)
            (setq comparison-mode :defun))))
      (error (setq comparison-mode :error comparison-result :boundary_func_error)))

    ;; Line fallback extraction
    (when (eq comparison-mode :line)
      (setq current-line-search-text (string-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (when (string-empty-p current-line-search-text)
        (setq comparison-mode :error comparison-result :empty_line)))

    ;; Find other window/buffer
    (unless (eq comparison-mode :error)
      (setq other-win (next-window current-win nil 'visible))
      (unless (and other-win (setq other-buf (window-buffer other-win)))
        (setq comparison-mode :error comparison-result :no_other_window)))

    ;; Locate entity or line in other buffer explicitly
    (when (and (not (eq comparison-mode :error)) other-buf)
      (with-current-buffer other-buf
        (goto-char (point-min))
        (setq found-in-other
          (cond ((eq comparison-mode :defun) (search-forward first-meaningful-line nil t))
            ((eq comparison-mode :line) (search-forward current-line-search-text nil t))))
        (unless found-in-other (setq comparison-result :not_found))
        (when found-in-other
          (beginning-of-line)
          (setq target-line-in-other (point)))))

    ;; Precise defun comparison with correct cursor handling
    (when (and (eq comparison-mode :defun) found-in-other)
      (let (other-start other-end orig-lines other-lines line-num diff-found)
        (with-current-buffer other-buf
          (goto-char target-line-in-other)
          (forward-char 1)
          (condition-case nil
            (progn
              (beginning-of-defun) (setq other-start (point))
              (end-of-defun) (setq other-end (point)))
            (error (setq comparison-result :boundary_error_other))))
        (when (and other-start other-end)
          (setq orig-lines (split-string (buffer-substring-no-properties original-start original-end) "\n")
            other-lines (split-string (with-current-buffer other-buf
                                        (buffer-substring-no-properties other-start other-end)) "\n")
            line-num 1 diff-found nil)
          (let ((max-lines (max (length orig-lines) (length other-lines))))
            (while (and (<= line-num max-lines) (not diff-found))
              (let ((o-line (nth (1- line-num) orig-lines))
                     (t-line (nth (1- line-num) other-lines)))
                (unless (string= o-line t-line)
                  (setq diff-found t comparison-result :differ
                    message-text (format "❌ Defun differs at line %d.\nOriginal: \"%s\"\nOther: \"%s\""
                                   line-num (or o-line "<NO LINE>") (or t-line "<NO LINE>")))
                  (with-current-buffer other-buf
                    (goto-char other-start)
                    (forward-line (1- line-num))
                    (setq target-line-in-other (point)))))
                (setq line-num (1+ line-num))))
            (unless diff-found
              (setq comparison-result :match
                message-text (format "✅ Defun matches exactly (%d lines)." (length orig-lines)))
              (with-current-buffer other-buf (goto-char other-start)))))))

    ;; Explicit fallback message handling
    (unless message-text
      (setq message-text
        (cond ((eq comparison-result :no_other_window) "❌ No other visible window.")
          ((eq comparison-result :empty_line) "❌ Current line empty.")
          ((eq comparison-result :boundary_func_error) "❌ Boundary function error.")
          ((eq comparison-result :boundary_error_other) "❌ Boundary error in other buffer.")
          ((eq comparison-result :not_found) (format "❌ Not found (%s) in %s." comparison-mode (buffer-name other-buf)))
          ((and (eq comparison-mode :line) found-in-other) (format "✅ Line matches exactly in %s." (buffer-name other-buf)))
          (t "❓ Comparison inconclusive."))))

    ;; Display final message & explicitly center the other window
    (when (and other-win target-line-in-other)
      (select-window other-win)
      (goto-char target-line-in-other)
      (recenter))
    (select-window current-win)
    (message "%s" message-text))

;; ------------------------------------------------------------------------- ;
;;                         Window Movement Functions                         ;
;; ------------------------------------------------------------------------- ;

(defun close-side-come-back ()
  (interactive)
  (sdev/jump-window)
  (kill-current-buffer)
  (sdev/jump-window))

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

;; ------------------------------------------------------------------------- ;
;;                         Buffer/Window Navigation                          ;
;; ------------------------------------------------------------------------- ;

(defun count-unique-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list frame)))))

(defun sdev/other-window (&optional arg)
  "Wrap `other-window' and skip *vterm* buffer."
  (interactive "p")
  (ignore-errors
    (let
      ((win (selected-window))
	(start-win (selected-window)))
      (catch 'done
	(while t
	  (setq win (other-window arg))
	  (when (eq win start-win)
	    (throw 'done nil))
	  (unless (string= (buffer-name (window-buffer win)) "*vterm*")
	    (throw 'done (select-window win))))))))

(defun sdev/set-windows ()
  "Set up window configuration. This function needs to be defined based on usage."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))

(defun sdev/set-or-jump-windows ()
  "Run ' if there's more than one window, otherwise set windows."
  (interactive)
  (if (= (count-windows) 1)
    (sdev/set-windows)
    (call-interactively #'sdev/other-window)))

(defun sdev/jump-window (&optional frame)
  (interactive)
  (if
    (> (count-unique-visible-buffers) 4)
    (call-interactively #'ace-window)
    (call-interactively #'sdev/set-or-jump-windows)))

(defun sdev/jump-to-vterm ()
  "Jump to the *vterm* buffer."
  (interactive)
  (let ((win (get-buffer-window "*vterm*")))
    (if win
      (select-window win)
      (error "No *vterm* buffer found"))))

;; ------------------------------------------------------------------------- ;
;;                            Keybindings                                    ;
;; ------------------------------------------------------------------------- ;

(global-set-key (kbd "C-c .") 'compare-entity-across-windows)
(global-set-key (kbd "C-c g") 'my-toggle-window-split)
(global-set-key (kbd "C-c l") 'buf-move-left)
(global-set-key (kbd "C-c r") 'buf-move-right)
(global-set-key (kbd "C-c u") 'buf-move-up)
(global-set-key (kbd "C-c d") 'buf-move-down)
(global-set-key (kbd "C-c w") 'rotate-windows)

;; Window navigation keybindings (moved from modes-avy.el)
(global-set-key (kbd "M-w") 'sdev/jump-window)
(global-set-key (kbd "C-x C-w") 'sdev/jump-window)
(global-set-key (kbd "C-t") 'sdev/jump-to-vterm)

;; ------------------------------------------------------------------------- ;
;;                    Frame and Tooltip Management                           ;
;;                  (moved from modes-shell.el)                              ;
;; ------------------------------------------------------------------------- ;

(setq tip-frame-params
      '((minibuffer . nil)
	(name . "*Tip Frame*")
	(lambda () (setq mode-line-format nil))
	(visibility . nil)
	(minibuffer-frame-alist nil)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(line-spacing . 0)
	(unsplittable . t)
	(undecorated . t)
	(mouse-wheel-frame . nil)
	(no-other-frame . t)
	(cursor-type . nil)
	(inhibit-double-buffering . t)
	(drag-internal-border . t)
	(no-special-glyphs . t)
	(no-accept-focus . t)
	(no-focus-on-map . t)
	(internal-border-width . 1)
	(right-fringe . 0)
	(left-fringe . 0)
	(top . -1)
	(desktop-dont-save . t)
	(left . -1)))

(defun frame--set-input-focus (frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame)))

(defun make-tip-frame (tip &rest args)
  (setq tip-frame (make-frame
		   (append (append
			    tip-frame-params
			    `((width . ,(+ (* (/ (frame-char-width) 2) (length tip)) (frame-char-width)))))
			   `((height . ,(*(frame-char-height) 2))))))

    ;; (generate-new-buffer "*Tip Frame Buffer*")

    (set-frame-position tip-frame
			(- (car (window-absolute-pixel-position)) (frame-char-width))
			(+ (cdr (window-absolute-pixel-position)) (frame-char-size)))

    (let ((current-frame (selected-frame) ))
      (make-frame-visible tip-frame)
      (select-frame tip-frame)
      (pop-to-buffer "*Tip Frame*")
      (with-current-buffer "*Tip Frame*"
	(fundamental-mode)
	(setq-local beacon-mode nil)
	(setq mode-line-format nil)
	(set-background-color "#5F55FF")
	(linum-mode -1)
	(insert  tip))
      (frame--set-input-focus current-frame)
      (frame-restack current-frame tip-frame)))

(defun close-tip-frame()
  (with-current-buffer "*Tip Frame*"
    (delete-region (point-min) (point-max)))
  (delete-frame tip-frame))

(defun tooltip-command (shell-command-to-execute)
  (while-no-input
    (let ((command (shell-command-to-string shell-command-to-execute) ))
      (make-tip-frame command)
      (sit-for 3)))
  (close-tip-frame))

(provide 'core-windows)

;;; core-windows.el ends here