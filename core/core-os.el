;;; core-os.el --- Tiqsi OS dependant functionality  -*- lexical-binding: t -*-

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


;;                                            Determine OS                                           ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(with-system darwin
  (setq tiqsi-osx t)
  (message "OSX laptop!"))

(with-system windows
  (message "windows laptop!"))

(with-system gnu/linux
  (if
    (string-match "Microsoft"
      (with-temp-buffer (shell-command "uname -r" t)
        (goto-char (point-max))
        (delete-char -1)
        (buffer-string)))
    (message "Running under Linux subsystem for Windows")
    (message "Not running under Linux subsystem for Windows")
    )
  )


(setq tiqsi-aquamacs (featurep 'aquamacs))
(setq tiqsi-linux (featurep 'x))
(setq tiqsi-win32 (not (or tiqsi-aquamacs tiqsi-linux)))
(setq tiqsi-console (eq (symbol-value 'window-system) nil))
(setq tiqsi-not-console (eq (eq (symbol-value 'window-system) nil)nil))

;; ------------------------------------------------------------------------- ;
;;                           Conditional execution                           ;
;; ------------------------------------------------------------------------- ;

(with-system darwin
  (defmacro if-command-exists(data &rest commands)
    `(let ((command-to-use   (shell-command-to-string (s-append "which" ,data))  ))
       (if (equal command-to-use "") nil ,@commands ))
    )
  )

(with-system gnu/linux
  (defmacro if-command-exists(data &rest commands)
    `(let ((command-to-use   (shell-command-to-string (s-append "which" ,data))  ))
       (if (equal command-to-use "") nil ,@commands ))
    )
  )


(defmacro when-executable (executable-name &rest body)
  `(if ,(executable-find executable-name)
     ,@body
     (message "executable not found: %s" executable-name)))


;;                                          Determine Emacs                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;; Emacs type: are we running GNU Emacs?                                                             ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
    (cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
    (cons 'progn body)))


(defmacro GNUEmacs25 (&rest body)
  (list 'if (string-match "GNU Emacs 25" (version))
    (cons 'progn body)))


(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
    (cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
    (cons 'progn body)))

;; Emacs version
(GNUEmacs
  (list emacs-version emacs-major-version emacs-minor-version
    system-type system-name system-configuration
    window-system
    (when (boundp 'aquamacs-version) aquamacs-version)))



;;                                               Win 32                                              ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-win32
  (setq tiqsi-makescript "build.bat")
  (setq tiqsi-font "PragmataPro")
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))

(if (boundp 'w32-send-sys-command)
  (progn

    (defun maximize-frame ()
      "Maximize the current frame"
      (interactive)
      (when tiqsi-aquamacs (aquamacs-toggle-full-frame))
      (when tiqsi-win32 (w32-send-sys-command 61488)))


    (defun w32-restore-frame ()
      "Restore a minimized frame"
      (interactive)
      (w32-send-sys-command 61728))

    (defun w32-maximize-frame ()
      "Maximize the current frame"
      (interactive)
      (w32-send-sys-command 61488))
    )
  (defun maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (when tiqsi-aquamacs (aquamacs-toggle-full-frame))
    (when tiqsi-win32   (message "w32-send-sys-command not interned"))))


;;                                               Linux                                               ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(if tiqsi-win32
  (setq os-sep "\\")
  (setq os-sep "/"))

(when tiqsi-linux
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (setq tiqsi-makescript "./build.sh")
  (display-battery-mode 1) )

;;                                               Mac OS                                              ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;


(with-system darwin
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'none)
  )

(when tiqsi-aquamacs
  (cua-mode 0)
  (osx-key-mode 0)
  (tabbar-mode 0)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
  (setq tiqsi-makescript "./build.macosx"))

;; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)


(defun which-os ()
  "Display OS information."
  (interactive)
  (if (eq system-type 'gnu/linux)
    (if (shell-command-to-string "type -p lsb_release > /dev/null")
      (message (concat (substring (shell-command-to-string "lsb_release -sd") 1 (- (length (shell-command-to-string "lsb_release -sd")) 2)) " (" (substring (shell-command-to-string "lsb_release -sr") 0 (- (length (shell-command-to-string "lsb_release -sr")) 1)) " '" (substring (shell-command-to-string "lsb_release -sc") 0 (- (length (shell-command-to-string "lsb_release -sc")) 1)) "'" " release, using the " (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname -r")) " kernel)"))
      (if (shell-command-to-string "type -p guix > /dev/null")
        (message (concat "Guix System " (shell-command-to-string "guix system -V | awk 'NR==1{printf $5}'") " (using the " (replace-regexp-in-string "\n$" "" (shell-command-to-string "uname -r")) " kernel)"))
        (message "Cannot determine distro.")))
    (message system-type)))


(defmacro check-exec (exec &rest body)
  "Check if EXEC is an executable in the environment.

If EXEC is found, evaluate the first part of BODY, otherwise evaluate the second part.
BODY should contain two parts: what to do if EXEC is found, and what to do if not found."
  (declare (indent defun))
  `(let ((executable-path (executable-find ,exec)))
     (if executable-path
       (progn ,@(car body))  ; Execute the first part of BODY if the executable is found
       (progn ,@(cadr body))))) ; Execute the second part of BODY if the executable is not found



;;          Patch the missing provided-mode-derived-p for < emacs 26         ;

(GNUEmacs25
  (progn
    (defun merge-ordered-lists (lists &optional error-function)
      "Merge LISTS in a consistent order.
LISTS is a list of lists of elements.
Merge them into a single list containing the same elements (removing
duplicates), obeying their relative positions in each list.
The order of the (sub)lists determines the final order in those cases where
the order within the sublists does not impose a unique choice.
Equality of elements is tested with `eql'.

If a consistent order does not exist, call ERROR-FUNCTION with
a remaining list of lists that we do not know how to merge.
It should return the candidate to use to continue the merge, which
has to be the head of one of the lists.
By default we choose the head of the first list."
      ;; Algorithm inspired from
      ;; [C3](https://en.wikipedia.org/wiki/C3_linearization)
      (let ((result '()))
	(setq lists (remq nil lists)) ;Don't mutate the original `lists' argument.
	(while (cdr (setq lists (delq nil lists)))
	  ;; Try to find the next element of the result. This
	  ;; is achieved by considering the first element of each
	  ;; (non-empty) input list and accepting a candidate if it is
	  ;; consistent with the rests of the input lists.
	  (let* ((next nil)
		  (tail lists))
	    (while tail
	      (let ((candidate (caar tail))
	             (other-lists lists))
		;; Ensure CANDIDATE is not in any position but the first
		;; in any of the element lists of LISTS.
		(while other-lists
		  (if (not (memql candidate (cdr (car other-lists))))
	            (setq other-lists (cdr other-lists))
	            (setq candidate nil)
	            (setq other-lists nil)))
		(if (not candidate)
		  (setq tail (cdr tail))
		  (setq next candidate)
		  (setq tail nil))))
	    (unless next ;; The graph is inconsistent.
	      (setq next (funcall (or error-function #'caar) lists))
	      (unless (assoc next lists #'eql)
		(error "Invalid candidate returned by error-function: %S" next)))
	    ;; The graph is consistent so far, add NEXT to result and
	    ;; merge input lists, dropping NEXT from their heads where
	    ;; applicable.
	    (push next result)
	    (setq lists
	      (mapcar (lambda (l) (if (eql (car l) next) (cdr l) l))
		lists))))
	(if (null result) (car lists) ;; Common case.
	  (append (nreverse result) (car lists)))))



    (defun derived-mode-all-parents (mode &optional known-children)
      "Return all the parents of MODE, starting with MODE.
This includes the parents set by `define-derived-mode' and additional
ones set by `derived-mode-add-parents'.
The returned list is not fresh, don't modify it.
\n(fn MODE)"               ;`known-children' is for internal use only.
      ;; Can't use `with-memoization' :-(
      (let ((ps (get mode 'derived-mode--all-parents)))
	(cond
	  (ps ps)
	  ((memq mode known-children)
	    ;; These things happen, better not get all worked up about it.
	    ;;(error "Cycle in the major mode hierarchy: %S" mode)
	    ;; But do try to return something meaningful.
	    (memq mode (reverse known-children)))
	  (t
	    ;; The mode hierarchy (or DAG, actually), is very static, but we
	    ;; need to react to changes because `parent' may not be defined
	    ;; yet (e.g. it's still just an autoload), so the recursive call
	    ;; to `derived-mode-all-parents' may return an
	    ;; invalid/incomplete result which we'll need to update when the
	    ;; mode actually gets loaded.
	    (let* ((new-children (cons mode known-children))
		    (get-all-parents
		      (lambda (parent)
			;; Can't use `cl-lib' here (nor `gv') :-(
			;;(cl-assert (not (equal parent mode)))
			;;(cl-pushnew mode (get parent 'derived-mode--followers))
			(let ((followers (get parent 'derived-mode--followers)))
			  (unless (memq mode followers)
			    (put parent 'derived-mode--followers
                              (cons mode followers))))
			(derived-mode-all-parents parent new-children)))
		    (parent (or (get mode 'derived-mode-parent)
                              ;; If MODE is an alias, then follow the alias.
                              (let ((alias (symbol-function mode)))
				(and (symbolp alias) alias))))
		    (extras (get mode 'derived-mode-extra-parents))
		    (all-parents
		      (merge-ordered-lists
			(cons (if (and parent (not (memq parent extras)))
				(funcall get-all-parents parent))
			  (mapcar get-all-parents extras)))))
              ;; Cache the result unless it was affected by `known-children'
              ;; because of a cycle.
              (if (and (memq mode all-parents) known-children)
		(cons mode (remq mode all-parents))
		(put mode 'derived-mode--all-parents (cons mode all-parents))))))))

    (defun provided-mode-derived-p (mode &optional modes &rest old-modes)
      "Non-nil if MODE is derived from a mode that is a member of the list MODES.
MODES can also be a single mode instead of a list.
This examines the parent modes set by `define-derived-mode' and also
additional ones set by `derived-mode-add-parents'.
If you just want to check the current `major-mode', use `derived-mode-p'.
We also still support the deprecated calling convention:
\(provided-mode-derived-p MODE &rest MODES)."
      (declare (side-effect-free t)
	(advertised-calling-convention (mode modes) "30.1"))
      (cond
	(old-modes (setq modes (cons modes old-modes)))
	((not (listp modes)) (setq modes (list modes))))
      (let ((ps (derived-mode-all-parents mode)))
	(while (and modes (not (memq (car modes) ps)))
	  (setq modes (cdr modes)))
	(car modes)))

    ))

;;                                                                           ;




;;                 https://oremacs.com/2019/03/24/shell-apt/                 ;
;; -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - ;
;; TODO inspect this
;; (safe-execute (advice-add
;; 		'ansi-color-apply-on-region
;; 		:before 'ora-ansi-color-apply-on-region)
;;   )

(straight-use-package-mode +1)

(defun ora-ansi-color-apply-on-region (begin end)
  "Fix progress bars for e.g. apt(8).
Display progress in the mode line instead."
  (let ((end-marker (copy-marker end))
         mb)
    (save-excursion
      (goto-char (copy-marker begin))
      (while (re-search-forward "\0337" end-marker t)
        (setq mb (match-beginning 0))
        (when (re-search-forward "\0338" end-marker t)
          (ora-apt-progress-message
            (substring-no-properties
              (delete-and-extract-region mb (point))
              2 -2)))))))

(defun ora-apt-progress-message (progress)
  (setq mode-line-process
    (if (string-match
          "Progress: \\[ *\\([0-9]+\\)%\\]" progress)
      (list
        (concat ":%s "
          (match-string 1 progress)
          "%%%% "))
      '(":%s")))
  (force-mode-line-update))

(defun ora-apt-progress-message (progress)
  (message
    (replace-regexp-in-string
      "%" "%%"
      (ansi-color-apply progress))))


;; Auto revert files when they change
(global-auto-revert-mode t)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;;                               Terminal setup                              ;
;; ------------------------------------------------------------------------- ;

(safe-load-package
  (straight-require 'vterm))


(defun copy-cd-command-maybe-next-buffer ()
  "Get the directory path of the next buffer if in *vterm*, or current buffer otherwise, and copy it to the clipboard prepended by 'cd '. Then jump to *vterm* buffer and paste it."
  (interactive)
  (let (path)
    (if (string= (buffer-name) "*vterm*")
      (progn
        (other-window 1)
        (setq path (file-name-directory (buffer-file-name)))
        (other-window 1))
      (setq path (file-name-directory (buffer-file-name))))
    (when path
      (kill-new (concat "cd " path))
      (message "Copied 'cd %s' to clipboard" path))
    (call-interactively #'sdev/jump-to-vterm)
    ))

(add-hook 'shell-mode-hook 'company-mode)
(add-hook 'term-mode-hook 'company-mode)


;;                                            Keybindings                                            ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(safe-execute(progn
	       (define-key vterm-mode-map (kbd "M-w") 'sdev/jump-window)
	       (define-key vterm-mode-map (kbd "M-f") 'ido-find-file)
	       (define-key vterm-mode-map (kbd "C-f") 'yank)
	       (define-key vterm-mode-map (kbd "M-k") 'kill-this-buffer)
	       (define-key vterm-mode-map (kbd "M-k") 'kill-this-buffer)
	       (define-key vterm-mode-map (kbd "S-<up>") 'previous-line)
	       (define-key vterm-mode-map (kbd "M-<up>") 'previous-blank-line)
	       (define-key vterm-mode-map (kbd "S-<down>") 'next-line)
	       (define-key vterm-mode-map (kbd "M-<down>") 'next-blank-line)
	       (define-key vterm-mode-map (kbd "C-p") 'copy-cd-command-maybe-next-buffer)
	       ))

(define-key global-map (kbd "C-p") 'copy-cd-command-maybe-next-buffer)

(define-key global-map "\ep" 'maximize-frame)
(define-key global-map "\ew" 'other-window)
(provide 'core-os)

;;; core-os.el ends here
