;;; validate-claude-repl.el --- Validate Claude REPL structure -*- lexical-binding: t -*-

(require 'cl-lib)

(defun validate-file-structure (filename)
  "Validate the parentheses structure of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (let ((stack nil)
          (issues nil))
      (while (not (eobp))
        (cond
          ;; Track opening parens with context
          ((looking-at "(\\(defun\\|defvar\\|defmacro\\|let\\|when\\|if\\|progn\\|with-\\|save-\\)")
           (push (list (line-number-at-pos) 
                       (buffer-substring-no-properties (point) (min (+ (point) 50) (point-max)))
                       (current-column))
                 stack))
          ;; Track any opening paren
          ((looking-at "(")
           (push (list (line-number-at-pos) "(" (current-column)) stack))
          ;; Check closing parens
          ((looking-at ")")
           (if stack
               (pop stack)
             (push (format "Extra ) at line %d" (line-number-at-pos)) issues))))
        (condition-case err
            (forward-char 1)
          (end-of-buffer nil)))
      ;; Report results
      (when stack
        (dolist (unclosed (reverse stack))
          (push (format "Unclosed at line %d: %s" (car unclosed) 
                        (substring (cadr unclosed) 0 (min 30 (length (cadr unclosed)))))
                issues)))
      (if issues
          (message "Issues found:\n%s" (mapconcat 'identity issues "\n"))
        (message "File structure appears valid")))))

(validate-file-structure "modules/modes/claude-repl/tiqsi-claude-repl.el")