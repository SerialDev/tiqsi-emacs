;;; core-functions.el --- Tiqsi programatic modifications functions

;;; Commentary:
;; 


;--{Disable modes}--;

(defun global-disable-mode (mode-fn)
  "Disable `MODE-FN' in ALL buffers."
  (interactive "a")
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (funcall mode-fn -1))))

;-----{Strings}-----;

;; truncate if long
(defun sdev/truncate (len s)
  "If S is longer than LEN, cut it down to LEN - 3 and add ... at the end."
  (if (> (length s) len)
      (format "%s" (substring s 0 (- len 3)))
    s))

(defun empty-string-p (string)
  "Return true if the string is empty or nil. Expects string."
  (or (null string)
      (zerop (length (trim string)))))

(defun sdev/center-pad (len padding s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) (string-to-char padding) )
     s
     (make-string (floor extra 2) (string-to-char padding) ))))


 ;; Change from snake_case to camelCase
 (defun sk/replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
 (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))


;;; ----------------------------------------------------------------------
;;;
(defsubst str-left (str count)
  "Use STR and read COUNT chars from left.
If the COUNT exeeds string length or is zero, whole string is returned."
  (if (> count 0)
      (substring str 0 (min (length str) count))
    str))

;;; ----------------------------------------------------------------------
;;;  - You can do this with negative argument to substring, but if you exceed
;;;    the string len, substring will barf and quit with error.
;;;  - This one will never call 'error'.
;;;
(defsubst str-right (str count)
  "Use STR and read COUNT chars from right.
If the COUNT exeeds string length or is zero, whole string is returned."
  (let* ((pos (- (length str)  count))
	 )
    (if (> pos 0)
	(substring str (- 0 count))
      str
      )))

;;; ----------------------------------------------------------------------
;;; - Ever struggled with peeking the lists..?
;;; - I have, and printing the contents of auto-mode-alist into
;;;   the buffer is very easy with this.
;;; - Should be default emacs function.
;;;
(defun list-print (list)
  "Insert content of LIST into current point."
  (interactive "XLisp symbol, list name: ")
  (mapcar
   (function
    (lambda (x) (insert (2str x) "\n")))
   list))

;;; ----------------------------------------------------------------------
;;; 1990, Sebastian Kremer, Institute for Theoretical Physics, West Germany
;;; BITNET: ab027@dk0rrzk0.bitnet
;;;
(defsubst list-to-string (list &optional separator)
  "Convert LIST into string. Optional SEPARATOR defaults to \" \".

Input:

  LIST       '(\"str\" \"str\" ...)
  separator  ' '

Return:
  str"
  (mapconcat
   (function identity)			;returns "as is"
   list
   (or separator " ")
   ))

(defun sk/dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

;-----{bindings}----;

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))     


;-------{Eval}------;

(defun sk/copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (kill-new buffer-file-name))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


;--{Introspection}--;

(defun my/show-functions-in-buffer ()
  "Show the functions defined in the current buffer"
  (interactive)
  (let ((current (current-buffer))
	(buffer (get-buffer-create "*Function Definitions*")))
    (save-excursion
      (goto-char 0)
      (while (re-search-forward "(defun \\([^(|^ ]*\\)" nil t)
	(progn
	  (set-buffer buffer)
	  (insert (format "Found : %s\n" (match-string 1)))
	  (set-buffer current)
	))
      )
    (pop-to-buffer buffer)))

;          TODO Base an implementation on this for Peek at definition          ;


;--{Display defun}--;

(defadvice popup-menu-show-quick-help
  (around pos-tip-popup-menu-show-quick-help () activate)
  "Show quick help using `pos-tip-show'."
  (if (eq window-system 'x)
      (let ((doc (popup-menu-document
		  menu (or item
			   (popup-selected-item menu)))))
	(when (stringp doc)
	  (pos-tip-show doc nil
			(if (popup-hidden-p menu)
			    (or (plist-get args :point)
				(point))
			  (overlay-end (popup-line-overlay
					menu (+ (popup-offset menu)
						(popup-selected-line menu)))))
			nil 0)
	  nil))
    ad-do-it))

(defadvice popup-tip
   (around popup-pos-tip-wrapper (string &rest args) activate)
   (if (eq window-system 'x)
       (apply 'popup-pos-tip string args)
     ad-do-it))

(defun chunyang-elisp-function-or-variable-quickhelp (symbol)
  "Display summary of function or variable at point.

Adapted from `describe-function-or-variable'."
  (interactive
   (let* ((v-or-f (variable-at-point))
          (found (symbolp v-or-f))
          (v-or-f (if found v-or-f (function-called-at-point))))
     (list v-or-f)))
  (if (not (and symbol (symbolp symbol)))
      (message "You didn't specify a function or variable")
    (let* ((fdoc (when (fboundp symbol)
                   (or (documentation symbol t) "Not documented.")))
           (fdoc-short (and (stringp fdoc)
                            (substring fdoc 0 (string-match "\n" fdoc))))
           (vdoc (when  (boundp symbol)
                   (or (documentation-property symbol 'variable-documentation t)
                       "Not documented as a variable.")))
           (vdoc-short (and (stringp vdoc)
                            (substring vdoc 0 (string-match "\n" vdoc)))))
      (and (require 'popup nil 'no-error)
           (popup-tip
            (or
             (and fdoc-short vdoc-short
                  (concat fdoc-short "\n\n"
                          (make-string 30 ?-) "\n" (symbol-name symbol)
                          " is also a " "variable." "\n\n"
                          vdoc-short))
             fdoc-short
             vdoc-short)
            :margin t)))))

;                    TODO make a defun for this with python                    ;


;--{Key completion}-;

(which-key-mode 1)
(setq which-key-idle-delay 0.5)


;---{Keybindings}---;

(with-eval-after-load 'emacs
    (define-key global-map (kbd "C-x C-c") nil)
	(define-key global-map (kbd "C-x C-x") #'save-buffers-kill-terminal)
	)

(global-set-key (kbd "C-M-<return>") 'chunyang-elisp-function-or-variable-quickhelp)
(global-set-key (kbd "M-<return>") 'xref-find-definitions-other-window)
(define-key emacs-lisp-mode-map  (kbd "M-h") 'helpful-at-point)

(provide 'core-functions)

;;; core-functions.el ends here
