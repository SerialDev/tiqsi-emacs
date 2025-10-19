;;; tiqsi-claude-repl-simple-highlight.el --- Simple working syntax highlighting -*- lexical-binding: t -*-

;;; Commentary:
;; Simple and reliable syntax highlighting for Claude REPL

;;; Code:

(require 'font-lock)

;; Define faces for code blocks
(defface tiqsi-claude-code-block-bg
  '((t :background "#2e3440" :extend t))
  "Background for code blocks.")

(defface tiqsi-claude-code-lang
  '((t :foreground "#88c0d0" :weight bold))
  "Face for language labels.")

(defun tiqsi-claude-highlight-all-code-blocks ()
  "Highlight all code blocks in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^```\\([a-zA-Z0-9_+-]*\\)$" nil t)
        (let* ((lang (match-string 1))
               (lang-line-start (match-beginning 0))
               (lang-line-end (match-end 0))
               (code-start (1+ lang-line-end)))
          ;; Find the closing ```
          (when (re-search-forward "^```$" nil t)
            (let ((code-end (match-beginning 0))
                  (block-end (match-end 0)))
              (setq count (1+ count))
              
              ;; Style the language line
              (add-text-properties lang-line-start lang-line-end
                                 '(face tiqsi-claude-code-lang))
              
              ;; Add background to code block
              (add-text-properties code-start code-end
                                 '(face tiqsi-claude-code-block-bg))
              
              ;; Apply syntax highlighting based on language
              (when (and lang (not (string-empty-p lang)))
                (tiqsi-claude--fontify-code-block code-start code-end lang))))))
      (message "Highlighted %d code blocks" count))))

(defun tiqsi-claude--fontify-code-block (start end lang)
  "Apply syntax highlighting to region from START to END for language LANG."
  (let ((mode (tiqsi-claude--get-mode lang)))
    (when mode
      ;; Remove existing fontification
      (remove-text-properties start end '(face nil font-lock-face nil))
      
      ;; Apply new fontification
      (let ((code (buffer-substring-no-properties start end))
            (orig-buffer (current-buffer)))
        (with-temp-buffer
          ;; Set up the mode and fontify
          (insert code)
          (funcall mode)
          (font-lock-fontify-buffer)
          
          ;; Copy faces back to original buffer
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (let* ((next-change (or (next-property-change pos) (point-max)))
                     (face (get-text-property pos 'face)))
                (when face
                  (with-current-buffer orig-buffer
                    (add-text-properties (+ start (- pos (point-min)))
                                       (+ start (- next-change (point-min)))
                                       `(font-lock-face ,face))))
                (setq pos next-change)))))))))

(defun tiqsi-claude--get-mode (lang)
  "Get Emacs mode for LANG."
  (let ((lang-lower (downcase lang)))
    (cond
     ((member lang-lower '("python" "py")) 'python-mode)
     ((member lang-lower '("rust" "rs")) 'rust-mode)
     ((member lang-lower '("javascript" "js")) 'js-mode)
     ((member lang-lower '("typescript" "ts")) 'typescript-mode)
     ((member lang-lower '("java")) 'java-mode)
     ((member lang-lower '("c")) 'c-mode)
     ((member lang-lower '("c++" "cpp")) 'c++-mode)
     ((member lang-lower '("elisp" "emacs-lisp" "lisp")) 'emacs-lisp-mode)
     ((member lang-lower '("ruby" "rb")) 'ruby-mode)
     ((member lang-lower '("go")) 'go-mode)
     ((member lang-lower '("sh" "bash" "shell")) 'sh-mode)
     ((member lang-lower '("json")) 'js-mode)
     ((member lang-lower '("yaml" "yml")) 'yaml-mode)
     ((member lang-lower '("html")) 'html-mode)
     ((member lang-lower '("css")) 'css-mode)
     ((member lang-lower '("sql")) 'sql-mode)
     (t nil))))

;; Auto-highlight after Claude responds
(defun tiqsi-claude-setup-auto-highlight ()
  "Setup automatic highlighting after Claude responses."
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    ;; Add a post-command hook that checks for new code blocks
    (add-hook 'post-command-hook 
              (lambda ()
                (when (and (eq major-mode 'tiqsi-claude-repl-mode)
                          (not (bound-and-true-p tiqsi-claude-repl--thinking-timer)))
                  ;; Schedule highlighting
                  (run-with-idle-timer 0.5 nil 'tiqsi-claude-highlight-all-code-blocks)))
              nil t)))

;; Add to Claude REPL mode
(add-hook 'tiqsi-claude-repl-mode-hook 'tiqsi-claude-setup-auto-highlight)

;; Provide the feature
(provide 'tiqsi-claude-repl-simple-highlight)

;;; tiqsi-claude-repl-simple-highlight.el ends here