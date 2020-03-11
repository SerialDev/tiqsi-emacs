;;; drycleaner.el -- Clean your files from garbage.

;; Copyright (C) 1999 Anders Lindgren.

;; Author: Anders lindgren <andersl@andersl.com>
;; Created: 1999-01-03
;; Version: 0.0
;; Url: http://www.andersl.com/emacs/drycleaner.html

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;{{{ Documentation:

;; Drycleaner is a package designed to remove any visible or invisble
;; stains from your files.  The "stains" that this package can handle
;; are, for example, whitespace at inappropriate places, misplaced or
;; missing tabs.  Other type of stains - either coffe stains on your
;; monitor or badly named variables - you have to deal with yourself.
;;
;; This package is designed to work similarly to the built-in feature
;; "require-final-newline".
;;
;; This package can be used in three ways:
;; * Applied explicitly to a buffer.
;; * A minor mode, drycleaner-mode, could be activated for individual
;;   buffers.  This mode checks the status of the buffer each time
;;   it is saved.
;; * A second minor mode, global-drycleaner-mode, exists that enables
;;   the check every time any buffer is saved.
;;
;; The following type of stains are handled:
;; * Extraneous whitespace at end of lines and the end of the file.
;; * Tabs in files that shouldn't contain tabs.
;; * Missing or misplaced tabs in files that should contain them.
;;
;; The default settings of this package it adopts itself to the minor
;; mode `indent-tabs-mode'.  In practice this means that when the
;; indent-tabs-mode is enabled, the drycleaner package will ensure
;; that all indentaion is done using a perfect combination of tabs and
;; spaces.  When it is disabled this package will make sure that no
;; tabs are used at all.

;; Customizing:
;;
;; The variable `drycleaner-check-list' containg a list of checks that
;; should be performed on the buffer.  Each entry in the list consists
;; of two parts, one guard used to enable or disable the check and
;; one function that actually should perform the test.
;;
;; The guard is the name of a variable whose value is one the
;; following:
;;  nil         -- disabled
;;  t           -- enabled
;;  ask         -- the user is prompted
;;  (eval expr) -- `expr' is evaluated ans should return one of the above.
;;
;; Initially `drycleaner-check-list' contains four checks, where the
;; name of the guard and the name of the function are the same:
;;    drycleaner-check-end-of-line-whitespace
;;    drycleaner-check-end-of-file-whitespace
;;    drycleaner-check-perfect-tabs
;;    drycleaner-check-no-tabs

;; Example:
;;
;; The value of `drycleaner-check-no-tabs' is:
;;   (eval (if indent-tabs-mode nil 'ask))
;;
;; This means that when Dryclener is applied to a buffer (for example
;; when the buffer is saved) the `if' expression is evaluated to `nil'
;; (i.e. don't perform this check) if `indent-tabs-mode' is enabled.
;; The value will be `ask' (i.e. perform the check but ask the user if
;; the file should be cleaned) if `indent-tabs-mode' is not enabled.

;; Compatibility:
;;
;; Great care has been taken to ensure that this package work with
;; narrowed and selective-display:ed buffers in order to make packages
;; like `outline' and `folding' to work.
;;
;; As of this writing, this package is not tested under XEmacs.
;; However, I know of no reason why it shouldn't run straight out of
;; the box.

;; Homepage:
;;
;; The home of Drycleaner is:
;;   www.andersl.com/emacs/drycleaner.html
;;
;; While you're are it, you can take a look at some of my other Emacs
;; applications at:
;;   www.andersl.com/emacs/

;; Reporting bugs:
;;
;;     Out of the last ten bugs you found, how many did you report?
;;
;; When reporting a bug, please:
;;
;; * Send a mail the maintainer of the package, or to the author
;;   if no maintainer exists.
;; * Include the name of the package in the title of the mail, to
;;   simplify for the recipient.
;; * State exactly what you did, what happened, and what you expected
;;   to see when you found the bug.
;; * If possible, include an example that activates the bug.
;; * Should you speculate about the cause of the problem, please
;;   state explicitly that you are guessing.

;;}}}

;;; Code:

;;{{{ Requirements

(eval-when-compile
  (require 'cl))

;;}}}
;;{{{ Variables

;; Custom Group:

(defgroup drycleaner nil
  "Automatically clean files from garbage when saved.

Drycleaner mode can be activated for individual buffer.
Global drycleaner mode applies to all buffers."
  :group 'files)


;; Placed here in order to get this to be the first entry on the
;; customize page.
(defcustom global-drycleaner-mode nil
  "When on, buffers are automatically cleaned from garbage when saved.

Set this variable when using \\[customize] only.  Otherwise, use the
command `global-drycleaner-mode' instead."
  :group 'drycleaner
  :initialize 'custom-initialize-default
  :set '(lambda (symbol value)
	  (global-drycleaner-mode (or value 0)))
  :type 'boolean
  :require 'drycleaner)


(defcustom drycleaner-check-end-of-line-whitespace 'ask
  "Enable if Dryclean should check for whitespace at end of lines.

Possible values:
  nil         -- disabled
  t           -- enabled
  ask         -- the user is prompted
  (eval expr) -- `expr' is evaluated ans should return one of the above."
  :group 'drycleaner
  :type  'sexp)


(defcustom drycleaner-check-end-of-file-whitespace 'ask
  "Enable if Dryclean should check for whitespace at the end of the file.

Possible values:
  nil         -- disabled
  t           -- enabled
  ask         -- the user is prompted
  (eval expr) -- `expr' is evaluated ans should return one of the above."
  :group 'drycleaner
  :type  'sexp)


(defcustom drycleaner-check-perfect-tabs 'ask
  "Enable if Dryclean should check for non-perfect tabs.

A perfect tab is a tab that is not preceded by any non-tab character
on a line.  This also checks if a sequence of characters at the beginning
of a line could be replaced with one or more tabs.

Possible values:
  nil         -- disabled
  t           -- enabled
  ask         -- the user is prompted
  (eval expr) -- `expr' is evaluated ans should return one of the above."
  :group 'drycleaner
  :type  'sexp)


(defcustom drycleaner-check-no-tabs '(eval (if indent-tabs-mode nil 'ask))
  "Enable if Dryclean should check for tabs.

Possible values:
  nil         -- disabled
  t           -- enabled
  ask         -- the user is prompted
  (eval expr) -- `expr' is evaluated ans should return one of the above."
  :group 'drycleaner
  :type  'sexp)


(defcustom drycleaner-load-hook nil
  "Functions to run when Dryclean mode is first loaded."
  :group 'drycleaner
  :type 'hook)


;; Note: We must run the eol check before the eof check since
;; the latter doesn't match "NL SPC NL".

(defvar drycleaner-check-list
  '((drycleaner-check-end-of-line-whitespace
     drycleaner-check-end-of-line-whitespace)
    (drycleaner-check-end-of-file-whitespace
     drycleaner-check-end-of-file-whitespace)
    (drycleaner-check-perfect-tabs
     drycleaner-check-perfect-tabs)
    (drycleaner-check-no-tabs
     drycleaner-check-no-tabs))
  "List describing the type of cleanings performed.

Each entry in the list is itself a list with two elements.  The first
is the name of a variable used to enable (with or with out user
confirmation) one explicit check, the second element is the function
used to remove the stain.")

;;}}}
;;{{{ Drycleaner mode

(defvar drycleaner-mode nil
  "Non-nil when `drycleaner-mode' is enabled.

Do not set this variable explicitly, call the function `drycleaner-mode'
instead.")


(defcustom drycleaner-mode-text " DCln"
  "String to display in the mode line when Drycleaner mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "Drycleaner mode text"           ; To separate it from `global-...'
  :group 'drycleaner
  :type 'string)


(defcustom drycleaner-mode-hook nil
  "Functions to run when Drycleaner mode is activated."
  :tag "Drycleaner mode hook"           ; To separate it from `global-...'
  :group 'drycleaner
  :type 'hook)


(defun turn-on-drycleaner-mode ()
  "Turn on Drycleaner mode.

This function is designed to be added to hooks, for example:
  (add-hook 'c-mode-hook 'turn-on-drycleaner-mode)"
  (drycleaner-mode 1))


;;;###autoload
(defun drycleaner-mode (arg)
  "Minor mode to enable cleaning of current buffer when written to files."
  (interactive "P")
  (make-local-variable 'drycleaner-mode)
  (setq drycleaner-mode
	(if (null arg)
	    (not drycleaner-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if (not drycleaner-mode)
      (remove-hook 'write-contents-hooks 'drycleaner-clean-current-buffer)
    (add-hook 'write-contents-hooks 'drycleaner-clean-current-buffer)
    (run-hooks 'drycleaner-mode-hook)))

;;}}}
;;{{{ Global drycleaner mode

(defcustom global-drycleaner-ignore-modes '()
  "List of major modes Global drycleaner mode should not check."
  :group 'drycleaner
  :type '(repeat sexp))


(defcustom global-drycleaner-mode-text ""
  "String to display when Global drycleaner mode is active.

The default is nothing since when this mode is active this text doesn't
vary neither over time, nor between buffers.  Hence a mode line text
would only waste precious space."
  :group 'drycleaner
  :type 'string)


(defcustom global-drycleaner-mode-hook nil
  "Hook called when Global drycleaner mode is activated."
  :group 'drycleaner
  :type 'hook)


(defvar global-drycleaner-ignore-buffer nil
  "*When non-nil, Global drycleaner mode will not clean this buffer.

This variable becomes buffer local when set in any faishon.")
(make-variable-buffer-local 'global-drycleaner-ignore-buffer)


;;;###autoload
(defun global-drycleaner-mode (arg)
  "Minor mode to enable cleaning of all buffers when written to files."
  (interactive "P")
  (make-local-variable 'global-drycleaner-mode)
  (setq global-drycleaner-mode
	(if (null arg)
	    (not global-drycleaner-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if global-drycleaner-mode
      (remove-hook 'write-file-hooks 'global-drycleaner-mode-handler)
    (add-hook 'write-file-hooks 'global-drycleaner-mode-handler)
    (run-hooks 'global-drycleaner-mode-hook)))


(defun global-drycleaner-mode-handler ()
  "Decide which buffers to clean when Global drycleaner mode is enabled.

This function is added to `write-file-hooks' by `global-drycleaner-mode'."
  (if (and (not (memq major-mode global-drycleaner-ignore-modes))
	   (not global-drycleaner-ignore-buffer)
	   ;; No need to check things twice.
	   (not drycleaner-mode))
      (drycleaner-clean-current-buffer))
  ;; Return nil since we're part of write-file-hooks.
  nil)

;;}}}

;;{{{ Top level function

;;;###autoload
(defun drycleaner-clean-current-buffer ()
  "Clean this buffer."
  (interactive)
  (dolist (entry drycleaner-check-list)
    (let* ((entry-var (nth 0 entry))
	   (entry-var-value (symbol-value entry-var))
	   (entry-func (nth 1 entry)))
      (if (and (consp entry-var-value)
	       (eq (nth 0 entry-var-value) 'eval))
	  (setq entry-var-value (eval (nth 1 entry-var-value))))
      (if entry-var-value
	  (funcall entry-func (eq entry-var-value 'ask)))))
  ;; Always return nil to make it possible to add this function
  ;; to `write-content-hooks'.
  nil)

;;}}}
;;{{{ The explicit cleaner functions

(defun drycleaner-check-end-of-line-whitespace (prompt-p)
  "Remove whitespace at end of lines in the current buffer.

If the argument `prompt-p' is non-nil the user in prompted."
  (when (and
	 ;; Check if we need to remove any tabs.
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (re-search-forward "[ \t]\\([\n\r]\\|$\\)" nil t)))
	 (or (not prompt-p)
	     (y-or-n-p
	      (format
	       "Buffer %s contains space at end of lines.  Remove them? "
	       (buffer-name)))))
    ;; Remove the whitespace at the end of line.
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(while (re-search-forward "\\([ \t]+\\)\\([\n\r]\\|$\\)" nil t)
	  (replace-match "\\2"))))))


(defun drycleaner-check-end-of-file-whitespace (prompt-p)
  "Remove whitespace at the end of the file in the current buffer.

If the argument `prompt-p' is non-nil the user in prompted."
  (when (and
	 ;; Check if we need to remove any tabs.
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-max))
	     (let ((p (point)))
	       (skip-chars-backward "\r\n")
	       (< 1 (- p (point))))))
	 (or (not prompt-p)
	     (y-or-n-p
	      (format "Buffer %s garbage at end of file.  Remove it? "
		      (buffer-name)))))
    ;; Ok, we shall remove the extra lines at the end of the file.
    (save-excursion
      (save-restriction
	(widen)
	;; This is safe since we know that the buffer contains
	;; at least two characters.
	(goto-char (- (point-max) 1))
	(let ((p (point)))
	  (skip-chars-backward "\r\n")
	  (delete-region (point) p))))))

;; The only tabs that we concider perfect are tabs that
;; are placed at the beginning of a line.
;; Tabs that are preceded by normal space characters, well,
;; there is nothing wrong with the tab character, but the
;; space shouldn't be there (or it should be a tab itself).

(defun drycleaner-check-perfect-tabs (prompt-p)
  (when (and
	 (save-excursion
	   (save-restriction
	     (widen)
	     (or
	      ;; If a tab character is preceded by a non-tab character
	      ;; something is wrong...
	      (progn
		(goto-char (point-min))
		(re-search-forward "\\(^\\|\r\\)[^\t\n\r]+\t" nil t))
	      ;; If indent-tabs-mode is enabled, and at least on line
	      ;; could be re-indented using tabs, do that.
	      (and indent-tabs-mode
		   (let ((spaces (make-string tab-width
					      (string-to-char " "))))
		     (goto-char (point-min))
		     (search-forward (concat "^" spaces) nil t))))))
	 ;; If we need to remove tabs, and we need to ask the user
	 ;; then ask him.  (Note that we can't do this inside the
	 ;; save-restriction/widen clause since that would change
	 ;; the apperence of the display.)
	 (or (not prompt-p)
	     (y-or-n-p
	      (format
	       "Buffer %s contains non-perfect indentation. Replace it? "
	       (buffer-name)))))
    (save-excursion
      (save-restriction
	(widen)
	(untabify (point-min) (point-max))
	(if indent-tabs-mode
	    (drycleaner-tabify-buffer))))))

(defun drycleaner-check-no-tabs (prompt-p)
  (when (and
	 ;; Check if we need to remove any tabs.
	 (save-excursion
	   (save-restriction
	     (widen)
	     (goto-char (point-min))
	     (search-forward "\t" nil t)))
	 ;; If we need to remove tabs, and we need to ask the user
	 ;; then ask him.  (Note that we can't do this inside the
	 ;; save-restriction/widen clause since that would change
	 ;; the apperence of the display.)
	 (or (not prompt-p)
	     (y-or-n-p
	      (format "Buffer %s contains tabs.  Remove them? "
		      (buffer-name)))))
    ;; Ok, we need to remove the tabs.  Good ol' untabify does that for us.
    (save-excursion
      (save-restriction
	(widen)
	(untabify (point-min) (point-max))))))

;;}}}
;;{{{ Help functions

;; Like "(tabify (point-min) (point-max))", but introduce only tab
;; characters at the beginning of lines.
(defun drycleaner-tabify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\|\r\\)[ \t]+" nil t)
      (let ((column (current-column))
	    (indent-tabs-mode t))
	(delete-region (match-beginning 0) (point))
	(indent-to column)))))

;;}}}

;;{{{ The End

(unless (assq 'drycleaner-mode minor-mode-alist)
  (push '(drycleaner-mode drycleaner-mode-text)
	minor-mode-alist))
(unless (assq 'global-drycleaner-mode minor-mode-alist)
  (push '(global-drycleaner-mode global-drycleaner-mode-text)
	minor-mode-alist))

(provide 'drycleaner)

(run-hooks 'drycleaner-load-hook)

;; This makes it possible to set Global drycleaner mode from
;; Customize.
(if global-drycleaner-mode
    (global-drycleaner-mode 1))

;;}}}

;;; drycleaner.el ends here.