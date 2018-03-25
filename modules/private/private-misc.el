;;; private-misc.el --- Tiqsi Miscellaneous defuns

;;; Commentary:
;; 

;; http://ergoemacs.org/emacs/emacs_lookup_ref.html  TODO: Further edit this one so it behaves like dict

;------{Lookup}-----;

(defun my-lookup-wikipedia ()
  "Look up the word under cursor in Wikipedia.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "http://en.wikipedia.org/wiki/" word))
    (eww myUrl) ; emacs's own browser
    ))


(defun my-lookup-google ()
  "Look up the word under cursor in Google.
If there is a text selection (a phrase), use that.
This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat "https://www.google.co.uk/search?q=" word))
    (eww myUrl) ; emacs's own browser
    ))


;-{Insert comments}-;

(defun sdev/insert-msg (string)
  (interactive "sString for inside centered message: ")
  (insert comment-start)
  (insert
    (sdev/truncate 99 (s-center 99 string ) ))
  (insert (s-trim comment-start))
  (newline))

(defun sdev/insert-msg-right (string)
  (insert comment-start)
  (insert
    (sdev/truncate 99 (s-pad-right 99 " " string ) ))
  (insert (s-trim comment-start))
  (newline))

(defun sdev/line-to-msg()
  (interactive)
  (sdev/insert-msg-right (s-trim-right (thing-at-point 'line t)))
  (kill-whole-line 1)
  )


(defun sdev/line-to-msg-centered()
  (interactive)
 (sdev/insert-msg (s-trim-right (thing-at-point 'line t)))
  (kill-whole-line 1)
 )


(defun sdev/insert-end ()
  (interactive )
  (insert comment-start)
  (insert
    (sdev/truncate 99 (s-pad-left 99 "-" "" ) ))
  (insert (s-trim comment-start))
  (newline))

(defun sdev/insert-belongs ()
  (interactive )
  (insert "x'∈X"))

(defun sdev/center-pad (len padding s)
  "If S is shorter than LEN, pad it with spaces so it is centered."
  (let ((extra (max 0 (- len (length s)))))
    (concat
     (make-string (ceiling extra 2) (string-to-char padding) )
     s
     (make-string (floor extra 2) (string-to-char padding) ))))

(defun sdev/insert-sep (string)
  (interactive "sString for \\label and \\nameref: ")
  (newline)
  (insert comment-start)
  (insert
    (s-truncate 83 
      (sdev/center-pad 78 "="
        (s-join string '("{""}"))
    ))
  )
  (insert (s-trim comment-start))
  (newline))

(defun sdev/insert-sep-small (string)
  (interactive "sString for \\label and \\nameref: ")
  (newline)
  (insert comment-start )
  (insert
    (s-truncate 38 
      (sdev/center-pad 38 "-"
        (s-join string '("{""}"))
	))
    )
  (insert (s-trim comment-start))
  (newline))

(defun sdev/insert-sep-mini (string)
  (interactive "sString for \\label and \\nameref: ")
  (newline)
  (insert comment-start )
  (insert
    (s-truncate 23 
      (sdev/center-pad 19 "-"
        (s-join string '("{""}"))
	))
    )
  (insert (s-trim comment-start))
  (newline))

(defun sdev/insert-comment (types function params return  extra doctest result )
					;(interactive "sString for \\f:")
  (interactive "sEnter Type def:
sEnter Function: 
sEnter Params :
sEnter Return value :
sEnter Extra information : 
sEnter Doctest : 
sEnter result of doctest: ")
  (insert "\"\"\"")
  (if (not(equal types ""))
          (progn(newline)
          (insert(sdev/truncate 83
            (s-prepend "    * type-def ::"
              (s-join " ::"
                (s-split "," types)))))  
      ()))

  (if (not(equal function ""))
          (progn(newline)
          (insert "    * ---------------{Function}---------------\n")
          (insert (sdev/truncate 83 (s-join function '("    * "" . . . ")))))
      ())

  (if (not(equal params ""))
      (progn
          (newline)
          (insert "    * ----------------{Params}----------------\n")
          (insert (s-word-wrap 83(s-prepend "    * : "(s-join "\n    * :" (s-split "," params))))))
      ())

  (if (not(equal return ""))
          (progn(newline)
          (insert "    * ----------------{Returns}---------------\n")
          (insert (sdev/truncate 83 (s-join return '("    * "" . . . ")))))
      ())
  
  (if (not(equal extra ""))
          (progn(newline)
          (insert "    * -----------------{Extra}----------------\n")
          (insert (s-word-wrap 83(s-join extra '("    * "" . . . ")))))
      ())

  (if (not(equal doctest ""))
      (progn 
          (newline)
          (insert "    * ----------------{Doctest}---------------\n")
          (insert (s-word-wrap 83 (s-join doctest '("    >>> """))))
          (newline)
          (insert (s-word-wrap 83 (s-join result '("    """))))
          (newline)
          (insert "    \"\"\""))
      (insert "\n    \"\"\"")))

(defun sdev/sprintf-debug ( cast function )
    (interactive "sEnter cast:
sEnter function: ")
    (insert "{")
    (newline)
    (insert "    char buf[1024];")
    (newline)
    (insert (s-concat "    sprintf(buf, \"[%s] " function  "[%s] \"," cast function ");" ))
    (newline)
    (insert "    OutputDebugStringA( buf );")
    (newline)
    (insert"}"))

(add-hook 'c++-mode-hook
          (lambda () (define-key c++-mode-map (kbd "C-c = i") 'sdev/sprintf-debug)))


;;--{inspect python}-

(defun sdev/inspect-module(module flag)
    (interactive "sEnter module: 
sEnter flag: ")
    (insert (s-concat "print(inspect_module(" module ", '" flag "'))")) )

(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c = i") 'sdev/inspect-module)))

;; Transpose a column of chars TODO: make it work with words too 
(defun sdev/insert-column (chars)
  (interactive "sChars to enter: ")
  (insert-rectangle (mapcar 'string (string-to-list chars))))


  
;;; Lines to C-LIST
(defun sdev/lines-to-cslist (start end &optional arg)
  (interactive "r\nP")
  (let ((insertion
         (mapconcat 
          (lambda (x) (format "\"%s\"" x))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)
    (when arg (forward-char (length insertion)))))

;; From http://stackoverflow.com/questions/17922208/emacs-convert-items-on-separate-lines-to-a-comma-separated-list
(defun sdev/csv-to-lines (separator)
  "Converts the current region line, as a csv string, to a set of independent lines, splitting the string based on the provided separator."
  (interactive "sEnter separator character: ")
  (setq current-region-string (buffer-substring-no-properties (region-beginning) (region-end)))
  (insert
   (mapconcat 'identity
              (split-string current-region-string separator)
              "\n")))
			  
			  
(defun sdev/lines-to-csv (separator)
  "Converts the current region lines to a single line, CSV value, separated by the provided separator string."
  (interactive "sEnter separator character: ")
  (setq current-region-string (buffer-substring-no-properties (region-beginning) (region-end)))
  (insert
   (mapconcat 'identity
              (split-string current-region-string "\n")
              separator)))
			  

;; TODO improve internal Shift right behaviour
(defun sdev/py-try-catch(beginning end)
  (interactive "r")
  (goto-char (string-to-number (message "%d" beginning)))
  (newline-and-indent)
  (insert "try:")
  (newline-and-indent)
  (goto-char (string-to-number (message "%d" beginning)))
  (kill-whole-line)
  (goto-char (string-to-number (message "%d" end)))
  (move-end-of-line 1)
  (newline-and-indent)
  (newline-and-indent)
  (insert "except Exception as e:")
  (newline-and-indent)
  (insert "print(traceback.format_exc())")
  (pop-mark))

(defun sdev/py-paren(beginning end)
  (interactive "r")
  (goto-char (string-to-number (message "%d" beginning)))
  (insert "(")
  (goto-char (string-to-number (message "%d" end)))
  (right-char 1)
  (right-char 1)
  (insert ")")
  (pop-mark))

(defun sdev/py-quotes(beginning end)
  (interactive "r")
  (goto-char (string-to-number (message "%d" beginning)))
  (insert "\"\"\"")
  (goto-char (string-to-number (message "%d" end)))
  (right-char 1)
  (right-char 1)
  (insert "\"\"\"")
  (pop-mark))

(use-package typing :disabled t
  :straight t
  :init
  (autoload 'typing-of-emacs "typing" nil t)
  :config
  (progn
    (setq toe-starting-length 6)
    (setq toe-starting-time-per-word 2)
    (setq toe-max-length 20)))

(defun count-chars ()
  (interactive)
  (save-restriction
    (widen)
    (message "%s characters" (1- (point-max)))))

(when casey-tiqsi
(defun repo-stats()
  (interactive)
(async-shell-command "\
git ls-tree -r HEAD | sed -Ee 's/^.{53}//' | \
while read filename; do file \"$filename\"; done | \
grep -E ': .*text' | sed -E -e 's/: .*//' | \
while read filename; do git blame --line-porcelain \"$filename\"; done | \
sed -n 's/^author //p' | \
sort | uniq -c | sort -rn")))


(when (require 'core-secrets nil 'noerror) 

  (defun connect-to-eac()
    (interactive)
    (find-file secrets-eac_path)
    )
  (global-set-key (kbd "C-c C-r e") 'connect-to-eac)
  (defun cd-to-sdev()
    (interactive)
    (cd secrets-dev_path)
    )
  (defun cd-to-config()
    (interactive)
    (cd secrets-config_path)
    )
  
  (global-set-key (kbd "C-c C-r p") 'cd-to-sdev)
  (global-set-key (kbd "C-c C-r c") 'cd-to-config)
)

;---{Keybindings}---;

(global-set-key (kbd "C-;") 'sdev/line-to-msg)
(global-set-key (kbd "C-:") 'sdev/line-to-msg-centered)
(global-set-key (kbd "C-@") 'sdev/insert-msg)
(global-set-key (kbd "M-;") 'sdev/insert-belongs)
(global-set-key (kbd "C-~") 'sdev/insert-sep)
(global-set-key (kbd "C-}") 'sdev/insert-sep-small)
(global-set-key (kbd "C-{") 'sdev/insert-sep-mini)
(global-set-key (kbd "C-M-'") 'sdev/insert-comment)
(global-set-key (kbd "C-M-=") 'sdev/sprintf-debug)
(define-key python-mode-map (kbd "C-c t e") 'sdev/py-try-catch)
(global-set-key (kbd "M-(") 'sdev/py-paren)
(global-set-key (kbd "M-\"") 'sdev/py-quotes)

(global-set-key (kbd "C-'") 'sdev/insert-end)


(provide 'private-misc)

;;; private-misc.el ends here
