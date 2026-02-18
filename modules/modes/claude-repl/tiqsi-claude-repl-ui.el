;;; tiqsi-claude-repl-ui.el --- Claude REPL UI components -*- lexical-binding: t -*-

;;; Commentary:
;; UI components, faces, and display functions for Claude REPL

;;; Code:

(require 'cl-lib)
(require 'ansi-color)

;;; Faces

(defface tiqsi-claude-repl-code-block
  '((t :background "#2e3440" :extend t))
  "Face for code block backgrounds."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-line-number
  '((t :foreground "#4c566a" :background "#2e3440" :weight normal))
  "Face for line numbers in code blocks."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-code-lang
  '((t :foreground "#88c0d0" :weight bold))
  "Face for code language indicators."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-code-border
  '((t :foreground "#5e81ac" :weight normal))
  "Face for code block borders."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-markdown-header
  '((t :foreground "#81a1c1" :weight bold :height 1.2))
  "Face for markdown headers."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-markdown-bold
  '((t :weight bold))
  "Face for markdown bold text."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-markdown-italic
  '((t :slant italic))
  "Face for markdown italic text."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-success
  '((t :foreground "#a3be8c" :weight bold))
  "Face for success messages."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-error
  '((t :foreground "#bf616a" :weight bold))
  "Face for error messages."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-warning
  '((t :foreground "#ebcb8b" :weight bold))
  "Face for warning messages."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-info
  '((t :foreground "#88c0d0" :weight bold))
  "Face for info messages."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-prompt
  '((t :foreground "#d08770" :weight bold))
  "Face for the REPL prompt."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-input
  '((t :foreground "#eceff4"))
  "Face for user input."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-timestamp
  '((t :foreground "#4c566a" :slant italic))
  "Face for timestamps."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-status
  '((t :foreground "#b48ead" :weight bold))
  "Face for status messages."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-separator
  '((t :foreground "#434c5e" :weight bold :height 0.9))
  "Face for separators."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-thinking
  '((t :foreground "#ebcb8b" :weight bold :slant italic))
  "Face for thinking indicators."
  :group 'tiqsi-claude-repl)

(defface tiqsi-claude-repl-progress
  '((t :foreground "#5e81ac" :weight bold))
  "Face for progress indicators."
  :group 'tiqsi-claude-repl)

;;; Colorized Output Functions

(defun tiqsi-claude-repl--colorize (text face)
  "Colorize TEXT with FACE."
  (propertize text 'face face))

(defun tiqsi-claude-repl--make-separator (&optional width char)
  "Create a separator line of WIDTH using CHAR."
  (let ((w (or width 80))
         (c (or char "━")))
    (tiqsi-claude-repl--colorize (make-string w (string-to-char c)) 'tiqsi-claude-repl-separator)))

(defun tiqsi-claude-repl--format-timestamp (&optional time)
  "Format TIME (or current time) as a timestamp."
  (tiqsi-claude-repl--colorize 
    (format-time-string "[%Y-%m-%d %H:%M:%S]" (or time (current-time)))
    'tiqsi-claude-repl-timestamp))

(defun tiqsi-claude-repl--format-status (status &optional details)
  "Format a STATUS message with optional DETAILS."
  (let* ((icon (cond ((string-match-p "success\\|complete\\|done" (downcase status)) "✅")
                 ((string-match-p "error\\|fail" (downcase status)) "❌")
                 ((string-match-p "warn" (downcase status)) "⚠️")
                 ((string-match-p "info" (downcase status)) "ℹ️")
                 ((string-match-p "thinking\\|process" (downcase status)) "🤔")
                 ((string-match-p "wait" (downcase status)) "⏳")
                 ((string-match-p "start" (downcase status)) "🚀")
                 ((string-match-p "stop\\|cancel" (downcase status)) "🛑")
                 ((string-match-p "save" (downcase status)) "💾")
                 ((string-match-p "load\\|history" (downcase status)) "📜")
                 ((string-match-p "search" (downcase status)) "🔍")
                 ((string-match-p "connect" (downcase status)) "🔌")
                 ((string-match-p "disconnect" (downcase status)) "🔌")
                 (t "•")))
          (face (cond ((string-match-p "success\\|complete\\|done" (downcase status)) 'tiqsi-claude-repl-success)
                  ((string-match-p "error\\|fail" (downcase status)) 'tiqsi-claude-repl-error)
                  ((string-match-p "warn" (downcase status)) 'tiqsi-claude-repl-warning)
                  ((string-match-p "thinking\\|process\\|wait" (downcase status)) 'tiqsi-claude-repl-thinking)
                  (t 'tiqsi-claude-repl-info))))
    (concat icon " " (tiqsi-claude-repl--colorize status face)
      (when details (concat " - " details)))))

(defun tiqsi-claude-repl--format-prompt (&optional prefix)
  "Format the REPL prompt with optional PREFIX."
  (concat (when prefix (concat prefix " "))
    (tiqsi-claude-repl--colorize "λ" 'tiqsi-claude-repl-prompt)
    " "))

(defun tiqsi-claude-repl--insert-header ()
  "Insert the REPL header with colors."
  (let ((header-lines
          (list
            (tiqsi-claude-repl--colorize "Claude REPL - AI Programming Assistant" 'tiqsi-claude-repl-info)
            (tiqsi-claude-repl--make-separator)
            (concat (tiqsi-claude-repl--colorize "Commands: " 'tiqsi-claude-repl-status)
              (tiqsi-claude-repl--colorize "RET/C-c C-c" 'tiqsi-claude-repl-prompt) " Send | "
              (tiqsi-claude-repl--colorize "C-g" 'tiqsi-claude-repl-prompt) " Cancel | "
              (tiqsi-claude-repl--colorize "C-c C-k" 'tiqsi-claude-repl-prompt) " Clear | "
              (tiqsi-claude-repl--colorize "C-c C-r" 'tiqsi-claude-repl-prompt) " Recover | "
              (tiqsi-claude-repl--colorize "C-c C-q" 'tiqsi-claude-repl-prompt) " Quit")
            (concat (tiqsi-claude-repl--colorize "Features: " 'tiqsi-claude-repl-status)
              "Smart context • Syntax highlighting • History tracking • Project awareness")
            (tiqsi-claude-repl--make-separator))))
    (dolist (line header-lines)
      (insert line "\n"))
    (insert "\n")))

(defun tiqsi-claude-repl--clean-unicode (text)
  "Clean problematic Unicode characters from TEXT."
  ;; Just clean ANSI codes - skip the problematic Unicode cleaning for now
  (ansi-color-filter-apply text))

(defun tiqsi-claude-repl--handle-error-output (output)
  "Format error OUTPUT with appropriate styling."
  (let ((cleaned (if tiqsi-claude-repl-clean-output
                   (tiqsi-claude-repl--clean-unicode output)
                   output)))
    (insert (tiqsi-claude-repl--colorize cleaned 'tiqsi-claude-repl-error) "\n")))

(defun tiqsi-claude-repl--thinking-indicator ()
  "Return a thinking indicator string."
  (concat "⏳ " (tiqsi-claude-repl--colorize "Thinking..." 'tiqsi-claude-repl-thinking)))

(defun tiqsi-claude-repl--format-tool-use (tool-name &optional args)
  "Format tool use display for TOOL-NAME with optional ARGS."
  (concat
   (tiqsi-claude-repl--colorize "🔧 Tool: " 'tiqsi-claude-repl-info)
   (tiqsi-claude-repl--colorize tool-name 'tiqsi-claude-repl-success)
   (when args
     (concat " " (tiqsi-claude-repl--colorize args 'tiqsi-claude-repl-timestamp)))))

;; ---------------------------------------------------------------------------
;; Code block syntax highlighting (ported from monolithic tiqsi-claude-repl.el)
;; ---------------------------------------------------------------------------

(declare-function tiqsi-claude-repl--get-language-mode "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--safe-mode-available-p "tiqsi-claude-repl-core")

(defun tiqsi-claude-repl--highlight-code-block (start end lang)
  "Apply syntax highlighting to code block from START to END with language LANG.
Uses a temp buffer with the appropriate major mode to fontify the code,
then copies the fontified text back with line numbers."
  (when tiqsi-claude-repl-highlight-code
    (let* ((mode (tiqsi-claude-repl--get-language-mode lang))
           (code (buffer-substring-no-properties start end))
           (highlighted-code nil))
      (setq highlighted-code
        (with-temp-buffer
          (insert code)
          (when (tiqsi-claude-repl--safe-mode-available-p mode)
            (condition-case _err
              (progn
                (delay-mode-hooks (funcall mode))
                (font-lock-mode 1)
                (setq font-lock-verbose nil)
                (if (fboundp 'font-lock-ensure)
                  (font-lock-ensure)
                  (with-no-warnings (font-lock-fontify-buffer))))
              (error
                (when (fboundp 'prog-mode)
                  (prog-mode)
                  (font-lock-mode 1)
                  (if (fboundp 'font-lock-ensure)
                    (font-lock-ensure)
                    (with-no-warnings (font-lock-fontify-buffer)))))))
          (let ((result "")
                (line-count (count-lines (point-min) (point-max)))
                (line-num 1))
            (when (> line-count 0)
              (let ((max-line-width (length (number-to-string line-count))))
                (goto-char (point-min))
                (while (not (eobp))
                  (let ((line-start (point))
                        (line-end (line-end-position)))
                    (when tiqsi-claude-repl-show-line-numbers
                      (setq result
                        (concat result
                          (propertize (format (concat "%" (number-to-string max-line-width) "d │ ") line-num)
                            'face 'tiqsi-claude-repl-line-number))))
                    (setq result
                      (concat result (buffer-substring line-start line-end)))
                    (forward-line 1)
                    (when (not (eobp))
                      (setq result (concat result "\n")))
                    (setq line-num (1+ line-num))))))
            result)))
      ;; Insert the highlighted code back
      (when highlighted-code
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert highlighted-code)
          (add-face-text-property start (point) 'tiqsi-claude-repl-code-block t)
          (point))))))

(defun tiqsi-claude-repl--highlight-inline-code (start end code-text)
  "Apply basic syntax highlighting to inline CODE-TEXT from START to END.
Highlights common programming constructs: keywords, numbers, strings,
function calls, and comments."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search nil))
      ;; Keywords
      (when (string-match "\\b\\(def\\|function\\|class\\|if\\|else\\|for\\|while\\|return\\|import\\|from\\|const\\|let\\|var\\|async\\|await\\)\\b" code-text)
        (let ((keyword-start (+ start (match-beginning 1)))
              (keyword-end (+ start (match-end 1))))
          (when (and (>= keyword-start start) (<= keyword-end end))
            (add-face-text-property keyword-start keyword-end
              '(:foreground "#81a1c1" :weight bold) t))))
      ;; Numbers
      (when (string-match "\\b[0-9]+\\(?:\\.[0-9]+\\)?\\b" code-text)
        (let ((num-start (+ start (match-beginning 0)))
              (num-end (+ start (match-end 0))))
          (when (and (>= num-start start) (<= num-end end))
            (add-face-text-property num-start num-end
              '(:foreground "#b48ead") t))))
      ;; Strings
      (when (or (string-match "\"[^\"]*\"" code-text)
              (string-match "'[^']*'" code-text))
        (let ((str-start (+ start (match-beginning 0)))
              (str-end (+ start (match-end 0))))
          (when (and (>= str-start start) (<= str-end end))
            (add-face-text-property str-start str-end
              '(:foreground "#a3be8c") t))))
      ;; Function calls
      (when (string-match "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" code-text)
        (let ((func-start (+ start (match-beginning 1)))
              (func-end (+ start (match-end 1))))
          (when (and (>= func-start start) (<= func-end end))
            (add-face-text-property func-start func-end
              '(:foreground "#88c0d0") t))))
      ;; Comments
      (when (string-match "\\(#\\|//\\).*$" code-text)
        (let ((comment-start (+ start (match-beginning 0)))
              (comment-end (+ start (match-end 0))))
          (when (and (>= comment-start start) (<= comment-end end))
            (add-face-text-property comment-start comment-end
              '(:foreground "#4c566a" :slant italic) t)))))))

(provide 'tiqsi-claude-repl-ui)

;;; tiqsi-claude-repl-ui.el ends here