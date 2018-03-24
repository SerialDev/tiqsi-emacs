;;; demax.el --- Delete too narrow windows when frame if de-maximized.

;; Copyright (C) 1999 by Anders Lindgren.

;; Author: Anders Lindgren <andersl@andersl.com>
;; Created: 1999-04-17
;; Url: http://www.andersl.com/emacs/
;; Keywords: convenience, frames
;; Version: 1.0  1999-04-21

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Description:
;;
;; Demax mode is a global minor mode that deletes too narrow windows
;; when a frame is de-maximized.
;;
;; When a frame is maximized again the previous window configuration
;; is restored, with the exception that current buffer will still be
;; visible.
;;
;; The function `demax-mode' is used to activate and deactivate the
;; mode.  To automaticaly activate `demax-mode' for future sessions
;; use Customize, or place the following line in your init file:
;;    (demax-mode 1)

;; Underlying design idea:
;;
;; In the current system there is no direct way of acting on maximize
;; and de-maxinize requests.  Also there is no clean way to actually
;; see whether a frame is maxinized or not.  Hence, the implementation
;; of this litte program is not as straight forward and clean as it at
;; first glanse might seem.
;;
;; This package installs a handler function as a hook in the variable
;; `window-size-change-functions'.  The handler function is both
;; responsible to track the state of frames, and to actually
;; performing the windows operations needed to delete narrow windows
;; and to restore window configurations.

;;; Code:

;; Dependecies:

(eval-when-compile (require 'cl))


;; Custom Group:

(defgroup demax nil
  "Delete too narrow windows when frame is de-maxinized."
  :group 'frames)


;; Variables:

(defcustom demax-mode nil
  "Delete too narrow windows when frame is de-maxinized.

Set this variable using \\[customize] only.  Otherwise, use the
command `demax-mode'."
  :group 'demax
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (demax-mode (or value 0)))
  :type 'boolean
  :require 'demax)


(defcustom demax-restore-configuration t
  "Non-nil if `demax-mode' should restore the window configuration."
  :group 'demax
  :type 'boolean)

(defcustom demax-width-treshold 72
  "Windows narrower that this will be deleted by `demax-mode'."
  :group 'demax
  :type 'integer)

(defcustom demax-frame-margin 0
  "Frames missing this many characters in width is considered maximal.

By setting this variable to a positive integer it is possible to use
this package when switching between large and small frames."
  :group 'demax
  :type 'integer)

(defcustom demax-verbose t
  "When nil, Demax mode will not generate any messages."
  :group 'demax
  :type 'boolean)

(defcustom demax-mode-text ""
  "String to display in mode line when Demax is active.

Note that a leading space is recommended when the value is not empty."
  :group 'demax
  :type 'string)

(defcustom demax-mode-hook nil
  "Hook called when Demax mode is activated."
  :group 'demax
  :type 'hook)

(defcustom demax-load-hook nil
  "Hook called when Demax mode is loaded."
  :group 'demax
  :type 'hook)


;; Functions:

;;;###autoload
(defun demax-mode (arg)
  "Minor mode to delete windows when frame is de-maximized.

This minor mode is not bound to any specific buffer.

With arg, turn Demax mode on if and only if arg is positive."
  (interactive "P")
  (setq demax-mode
	(if (null arg)
	    (not demax-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (and demax-verbose
	   (interactive-p))
      (message "Demax mode is now %s."
	       (if demax-mode "on" "off")))
  ;; Ensure that our handler always is installed, even when
  ;; demax mode is off.  This ensures that we record information
  ;; about the state of the frames.
  (add-hook 'window-size-change-functions 'demax-resize-handler)
  (if demax-mode
      (run-hooks 'demax-mode-hook)))


(defun demax-get-frame-window-list (&optional frame)
  "Return list of windows in FRAME."
  (let ((windows (list (frame-first-window frame)))
	window)
    (while (progn
	     (setq window (next-window (car windows)))
	     (not (memq window windows)))
      (push window windows))
    windows))


(defun demax-frame-maximized-p (&optional frame)
  "Non-nil if frame is maximized.

If the frame is almost maximized, and the frame is less than
`demax-frame-margin', the frame is considered maximized.  This way
this package is useful for users switching between a small and a
large, albeit not maximized, frame.

Note that the algorithm used to determine if a frame is maximized is
heuristic, hence it can return the wrong value from time to time..."
  (interactive)
  (let* ((frame-alist (frame-parameters frame))
	 (border-width (cdr-safe (assq 'border-width frame-alist))))
    (unless border-width
      (setq border-width 0))
    (let ((maximized (>= (+ (frame-width)
			    (if (cdr-safe
				 (assq 'vertical-scroll-bars frame-alist))
				border-width 0)
			    demax-frame-margin)
			 (/ (x-display-pixel-width frame)
			    (frame-char-width frame)))))
      (if (interactive-p)
	  (message "frame is %smaximized." (if maximized "" "not ")))
      maximized)))


(defvar demax-frame-configurations '()
  "AList of frames and window configurations.")

(defun demax-expand (frame)
  "Restore the window cofiguration to a maximized window.

If `demax-restore-configuration' is non-nil then the window
configuration is restored, with the exception that the selected window
is still visible."
  (if demax-restore-configuration
      (let ((pair (assq frame demax-frame-configurations)))
	(if pair
	    (let ((buff (window-buffer (frame-selected-window frame))))
	      (set-window-configuration (cdr pair))
	      (let ((wind (get-buffer-window buff frame)))
		(if wind
		    (set-frame-selected-window frame wind)
		  (switch-to-buffer buff))))))))


(defun demax-contract (frame)
  "Remove windows that aren't wide enough.

The variable `demax-width-treshold' defines the width treshold, any
window not this wide (except the selected window) will be deleted.

This function is used by `demax-mode' to automatically remove too narrow
windows after a frame has been de-maxinized."
  ;; Save current window configuration.
  (let* ((conf (current-window-configuration frame))
	 (new-conf-alist (list (cons frame conf))))
    ;; Copy the old list and clear info of dead frames.
    (dolist (pair demax-frame-configurations)
      (if (and (not (eq (car pair) frame))
	       (frame-live-p (car pair)))
	  (push pair new-conf-alist)))
    (setq demax-frame-configurations new-conf-alist))
  ;; Clean up windows
  (let ((windows (demax-get-frame-window-list frame)))
    (dolist (wind windows)
      (if (and (not (eq wind (frame-selected-window frame)))
	       (< (window-width wind) demax-width-treshold)
	       (< (window-width wind) (frame-width frame)))
	  (delete-window wind)))))


;; This is normally installed in `window-size-change-functions',
;; and hence called whenever the size of the frame is changed.

(defvar demax-state-alist '()
  "Alist of frame to demax state.")

(defvar demax-debug nil)
(defvar demax-debug-log '())

(defun demax-resize-handler (frame)
  "For each resize, check if frame has been maximized or de-maximized.

This function is installed in `window-size-change-functions'.  When it
detects that a frame has been maximized or de-maximized the functions
`demax-expand' and `demax-contract' is called, respectively.

The current maximized state is recorded in `demax-state-alist'."
  (let ((frame-alist (frame-parameters frame))
	(state (demax-frame-maximized-p frame))
	;; Here we make a conservative assumption.  If demax has
	;; no knowledge of the state of a frame, we assume that
	;; it's not maximized.  If we were to assume the opposite,
	;; we could start messing with the users windows even when
	;; a normal resize is performed.
	(prev-state (cdr-safe (assq frame demax-state-alist))))
    (if demax-debug
	(push (list frame state prev-state) demax-debug-log))
    (if demax-mode
	(cond ((and state (not prev-state))
	       (demax-expand frame))
	      ((and (not state) prev-state)
	       (demax-contract frame))))
    ;; Keep track of the new state, and clean list of dead frames.
    (let ((new-alist (list (cons frame state))))
      (dolist (pair demax-state-alist)
	(if (and (frame-live-p (car pair))
		 (not (eq (car pair) frame)))
	    (push pair new-alist)))
      (setq demax-state-alist new-alist))))


;; The end:

(unless (assq 'demax-mode minor-mode-alist)
  (push '(demax-mode demax-mode-text)
	minor-mode-alist))

;; Start logging information as soon as possible.
(add-hook 'window-size-change-functions 'demax-resize-handler)

(provide 'demax)

(run-hooks 'demax-load-hook)

;; This makes it possible to set Demax mode from Customize.
(if demax-mode
    (demax-mode 1))

;; demax.el ends here.