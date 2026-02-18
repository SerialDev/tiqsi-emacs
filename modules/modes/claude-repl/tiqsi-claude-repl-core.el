;;; tiqsi-claude-repl-core.el --- Claude REPL core functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Core functionality, process management, and utilities for Claude REPL

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)

;; Declare functions from other files
(declare-function tiqsi-claude-repl--colorize "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--make-separator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-timestamp "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-status "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-prompt "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--clean-unicode "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--handle-error-output "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--thinking-indicator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--stop-thinking-animation "tiqsi-claude-repl-features")

;;; Variables

(defvar-local tiqsi-claude-repl--current-process nil
  "The current Claude process.")

(defvar-local tiqsi-claude-repl--thinking-timer nil
  "Timer for thinking animation.")

(defvar tiqsi-claude-repl--thinking-animation-state 0
  "State counter for thinking animation.")

(defvar tiqsi-claude-repl--thinking-frames
  '("⏳ Thinking." "⏳ Thinking.." "⏳ Thinking..." "⏳ Thinking...." "⏳ Thinking....." "⏳ Thinking......" "⏳ Thinking....." "⏳ Thinking...." "⏳ Thinking..." "⏳ Thinking..")
  "Animation frames for thinking indicator.")

(defvar tiqsi-claude-repl--claude-session-id nil
  "Current Claude session ID.")

(defvar tiqsi-claude-repl--session-started nil
  "Whether the current session has been started.")

(defvar tiqsi-claude-repl--conversation-id nil
  "Current conversation ID for history tracking.")

(defvar tiqsi-claude-repl--conversation-history nil
  "History of the current conversation.")

(defvar tiqsi-claude-repl--session-start-time nil
  "Time when session started.")

(defvar tiqsi-claude-repl--session-id nil
  "Current session ID.")

(defvar tiqsi-claude-repl--message-count 0
  "Number of messages in current session.")

(defvar tiqsi-claude-repl--last-interaction-time nil
  "Time of last interaction.")

(defvar-local tiqsi-claude-repl--json-buffer ""
  "Buffer for accumulating incomplete JSON lines.")

(defvar-local tiqsi-claude-repl--output-start nil
  "Marker for where Claude's output starts.")

(defvar-local tiqsi-claude-repl--request-start-time nil
  "Time when request was started.")

(defvar-local tiqsi-claude-repl--json-processing-enabled nil
  "Whether JSON processing is enabled for current process.")

(defvar-local tiqsi-claude-repl--current-response ""
  "Buffer to accumulate Claude's current response.")

;;; Core Functions

(defun tiqsi-claude-repl--wrap-long-lines (text)
  "Wrap lines in TEXT that exceed `tiqsi-claude-repl-wrap-column'."
  ;; TEMPORARILY DISABLED - just return text as-is
  text)

(defun tiqsi-claude-repl--get-project-root ()
  "Get the current project root directory."
  (or (when (fboundp 'project-root)
        (when-let ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun tiqsi-claude-repl--get-or-create-buffer ()
  "Get or create the Claude REPL buffer."
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
          (buffer-name (format "*Claude REPL (%s)*" 
                         (file-name-nondirectory (directory-file-name project-root)))))
    (or (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (tiqsi-claude-repl-mode)
        (current-buffer)))))

(defun tiqsi-claude-repl--executable-available-p ()
  "Check if Claude CLI is available."
  (executable-find tiqsi-claude-repl-program))

(defun tiqsi-claude-repl--clean-output (output)
  "Clean OUTPUT from ANSI codes and control characters."
  (let ((cleaned output))
    ;; Remove ANSI color codes
    (setq cleaned (ansi-color-filter-apply cleaned))
    ;; Remove control characters except newlines and tabs
    (setq cleaned (replace-regexp-in-string "[\x00-\x08\x0B\x0C\x0E-\x1F\x7F]" "" cleaned))
    ;; Clean any Unicode issues if enabled
    (when tiqsi-claude-repl-clean-output
      (setq cleaned (tiqsi-claude-repl--clean-unicode cleaned)))
    cleaned))

(defun tiqsi-claude-repl--detect-tool-use (output)
  "Detect if OUTPUT contains tool use information."
  (or (string-match-p "<function_calls>" output)
      (string-match-p "<tool_use>" output)
      (string-match-p "exec!" output)
      (string-match-p "bash\\|shell\\|git" (downcase output))))

(defun tiqsi-claude-repl--process-filter (process output)
  "Process filter for Claude output."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t)
            (inhibit-quit t))
        ;; Update thinking indicator if needed
        ;; Set output start marker if not already set
        (when (not tiqsi-claude-repl--output-start)
          ;; Stop thinking animation
          (when (fboundp 'tiqsi-claude-repl--stop-thinking-animation)
            (tiqsi-claude-repl--stop-thinking-animation))
          (save-excursion
            (goto-char (point-max))
            (condition-case nil
                (when (search-backward "⏳ Thinking" nil t)
                  (beginning-of-line)
                  (let ((line-start (point)))
                    (end-of-line)
                    (delete-region line-start (1+ (point)))))
              (error nil))
            ;; Set marker AFTER updating thinking message
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (setq-local tiqsi-claude-repl--output-start (point-marker))))
        
        ;; Simple text output handling
        (let ((cleaned-output output))
          ;; Debug output to see what we're getting
          (when (tiqsi-claude-repl--detect-tool-use output)
            (message "DEBUG: Tool use detected in output: %s" (substring output 0 (min 100 (length output)))))
          
          (when tiqsi-claude-repl-clean-output
            (setq cleaned-output (tiqsi-claude-repl--clean-output cleaned-output)))
          ;; Insert output - even if it looks empty (might be tool use)
          (save-excursion
            (goto-char (point-max))
            ;; Add "Assistant: " prefix if this is the start of response
            (when (and tiqsi-claude-repl--output-start
                       (= (point) (marker-position tiqsi-claude-repl--output-start))
                       (not (string-match-p "^\\s-*$" cleaned-output)))
              (insert (tiqsi-claude-repl--colorize "Assistant: " 'tiqsi-claude-repl-info)))
            ;; Always insert output, don't filter by string-trim
            ;; Accumulate response for later processing
            (setq-local tiqsi-claude-repl--current-response 
                        (concat (or tiqsi-claude-repl--current-response "") cleaned-output))
            (insert cleaned-output)
            ;; Check if we have complete code blocks to process
            (when (string-match "```" cleaned-output)
              (let ((buffer-end (point-max)))
                (save-excursion
                  (goto-char (or tiqsi-claude-repl--output-start (point-min)))
                  (tiqsi-claude-repl--process-partial-code-blocks (point) buffer-end)))))))))

(defun tiqsi-claude-repl--process-json-output (output)
  "Process JSON OUTPUT from Claude."
  (setq tiqsi-claude-repl--json-buffer (concat tiqsi-claude-repl--json-buffer output))
  ;; Process complete JSON objects line by line
  (let ((lines (split-string tiqsi-claude-repl--json-buffer "\n"))
        (remaining ""))
    (dolist (line lines)
      (let ((trimmed-line (string-trim line)))
        (if (and (> (length trimmed-line) 0)
                 (string-prefix-p "{" trimmed-line)
                 (string-suffix-p "}" trimmed-line))
            ;; Process complete JSON line
            (condition-case err
                (let* ((json-obj (json-parse-string trimmed-line))
                       (type (gethash "type" json-obj)))
                  (cond
                   ;; Handle system initialization
                   ((and (equal type "system") (equal (gethash "subtype" json-obj) "init"))
                    (tiqsi-claude-repl--handle-system-init json-obj))
                   
                   ;; Handle assistant messages (the actual response)
                   ((equal type "assistant")
                    (tiqsi-claude-repl--handle-assistant-message json-obj))
                   
                   ;; Handle content block deltas (streaming text)
                   ((equal type "content_block_delta")
                    (tiqsi-claude-repl--handle-content-delta json-obj))
                   
                   ;; Handle result (completion info) - suppress output
                   ((equal type "result")
                    nil)
                   
                   ;; Handle thinking messages
                   ((equal type "thinking")
                    (when tiqsi-claude-repl-show-thinking
                      (tiqsi-claude-repl--handle-thinking-message json-obj)))
                   
                   ;; Ignore other types
                   (t nil)))
              (json-parse-error 
               ;; If JSON parsing fails, add to remaining
               (setq remaining (concat remaining line "\n"))))
          ;; Not a complete JSON line, keep it
          (setq remaining (concat remaining line "\n")))))
    ;; Update buffer with remaining incomplete JSON
    (setq tiqsi-claude-repl--json-buffer remaining)))

(defun tiqsi-claude-repl--handle-system-init (json-obj)
  "Handle system initialization JSON-OBJ."
  (let ((claude-session-id (gethash "session_id" json-obj)))
    (when claude-session-id
      (setq-local tiqsi-claude-repl--claude-session-id claude-session-id))
    ;; Show system info only if in verbose mode
    (when tiqsi-claude-repl-show-thinking
      (save-excursion
        (goto-char (point-max))
        (insert "\n" 
                (tiqsi-claude-repl--colorize "System: " 'tiqsi-claude-repl-info)
                (format "Session initialized (ID: %s)" 
                        (or claude-session-id "unknown"))
                "\n")))))

(defun tiqsi-claude-repl--handle-assistant-message (json-obj)
  "Handle assistant message JSON-OBJ."
  ;; For debugging
  (message "DEBUG: Handling assistant message")
  (let* ((message (gethash "message" json-obj))
         (content (gethash "content" message)))
    (when content
      (if (vectorp content)
          ;; Handle array of content
          (dotimes (i (length content))
            (let ((part (aref content i)))
              (when (equal (gethash "type" part) "text")
                (let ((text (gethash "text" part)))
                  (when (and text (> (length text) 0))
                    (save-excursion
                      (goto-char (point-max))
                      (insert text "\n")))))))
        ;; Handle direct text content
        (when (stringp content)
          (save-excursion
            (goto-char (point-max))
            (insert content "\n")))))))

(defun tiqsi-claude-repl--handle-content-delta (json-obj)
  "Handle content delta streaming JSON-OBJ."
  (let ((delta (gethash "delta" json-obj)))
    (when delta
      (let ((text (gethash "text" delta "")))
        (when (> (length text) 0)
          (save-excursion
            (goto-char (point-max))
            (insert (tiqsi-claude-repl--wrap-long-lines text))))))))

(defun tiqsi-claude-repl--handle-thinking-message (json-obj)
  "Handle thinking message JSON-OBJ."
  (let ((content (or (gethash "content" json-obj)
                     (gethash "text" json-obj))))
    (when content
      (save-excursion
        (goto-char (point-max))
        (insert "\n"
                (tiqsi-claude-repl--colorize "🤔 Thinking: " 'tiqsi-claude-repl-thinking)
                (tiqsi-claude-repl--colorize content 'tiqsi-claude-repl-info)
                "\n")))))

(defun tiqsi-claude-repl--process-sentinel (process event)
  "Sentinel for Claude process."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          ;; Clean up any remaining thinking indicator
          (condition-case nil
              (when (search-backward "⏳ Thinking" nil t)
                (beginning-of-line)
                (let ((line-start (point)))
                  (end-of-line)
                  (delete-region line-start (1+ (point)))))
            (error nil))
          (goto-char (point-max))
          
          ;; Check if response was empty or just tool use
          (when (and tiqsi-claude-repl--output-start
                     (marker-position tiqsi-claude-repl--output-start))
            (let ((response-length (- (point) (marker-position tiqsi-claude-repl--output-start))))
              (when (< response-length 20) ;; Very short or empty response
                (goto-char tiqsi-claude-repl--output-start)
                (unless (looking-at "Assistant:")
                  (insert (tiqsi-claude-repl--colorize "Assistant: " 'tiqsi-claude-repl-info)))
                (goto-char (point-max))
                (when (and tiqsi-claude-repl--current-response
                           (tiqsi-claude-repl--detect-tool-use tiqsi-claude-repl--current-response))
                  (insert (tiqsi-claude-repl--colorize "[Tool execution completed]" 'tiqsi-claude-repl-info))
                  (insert "\n")))))
          
          (unless (bolp) (insert "\n"))
          
          ;; Post-process the response for markdown formatting
          (when (and tiqsi-claude-repl--output-start
                     (marker-position tiqsi-claude-repl--output-start))
            (let ((start (marker-position tiqsi-claude-repl--output-start))
                  (end (point-max)))
              (when (> end start)
                (tiqsi-claude-repl--highlight-markdown-code-blocks start end))))
          
          ;; Handle different process exit events
          (cond
           ((string-match-p "finished" event)
            (insert (tiqsi-claude-repl--make-separator 60 "─") "\n")
            (setq-local tiqsi-claude-repl--json-processing-enabled nil))
           
           ((string-match-p "killed\\|interrupt" event)
            (tiqsi-claude-repl--handle-error-output "Process interrupted")
            (insert (tiqsi-claude-repl--make-separator 60 "─") "\n"))
           
           ((string-match-p "exited abnormally" event)
            (tiqsi-claude-repl--handle-error-output 
             (format "Process exited abnormally: %s" (string-trim event)))
            (insert (tiqsi-claude-repl--make-separator 60 "─") "\n")))
          
          ;; Insert new prompt
          (tiqsi-claude-repl--insert-prompt)
          (goto-char (point-max))
          
          ;; Reset state
          (setq-local tiqsi-claude-repl--output-start nil)
          (setq-local tiqsi-claude-repl--json-buffer "")
          (setq-local tiqsi-claude-repl--current-response "")
          (setq-local tiqsi-claude-repl--request-start-time nil)))))))

(defun tiqsi-claude-repl--insert-prompt ()
  "Insert the REPL prompt."
  (let ((inhibit-read-only t))
    (unless (bolp) (insert "\n"))
    (insert (tiqsi-claude-repl--format-prompt))
    (when (get-buffer-process (current-buffer))
      (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))))

(defun tiqsi-claude-repl--highlight-markdown-code-blocks (start end)
  "Highlight markdown code blocks between START and END."
  (when tiqsi-claude-repl-highlight-code
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^```\\([a-zA-Z0-9+-]*\\)\n\\(\\(?:[^`]\\|`\\(?!``\\)\\)*\\)\n```" end t)
        (let* ((lang (match-string 1))
               (code (match-string 2))
               (block-start (match-beginning 0))
               (block-end (match-end 0)))
          ;; Replace the markdown block with formatted version
          (delete-region block-start block-end)
          (goto-char block-start)
          (tiqsi-claude-repl--insert-formatted-code-block lang code))))))

(defun tiqsi-claude-repl--process-partial-code-blocks (start end)
  "Process any complete code blocks between START and END."
  (when tiqsi-claude-repl-highlight-code
    ;; We need to process from end to start to avoid position issues
    (let ((blocks '()))
      ;; First, find all code blocks
      (save-excursion
        (goto-char start)
        (while (re-search-forward "```\\([a-zA-Z0-9+-]*\\)\n" end t)
          (let ((lang-start (match-beginning 0))
                (lang (match-string 1))
                (code-start (point)))
            ;; Look for the closing ```
            (when (re-search-forward "\n```" end t)
              (let ((block-end (match-end 0))
                    (code-end (match-beginning 0)))
                ;; Store block info
                (push (list lang-start block-end lang
                            (buffer-substring-no-properties code-start code-end))
                      blocks))))))
      ;; Process blocks in reverse order (from end to start)
      (dolist (block blocks)
        (let ((lang-start (nth 0 block))
              (block-end (nth 1 block))
              (lang (nth 2 block))
              (code (nth 3 block)))
          (save-excursion
            (delete-region lang-start block-end)
            (goto-char lang-start)
            (tiqsi-claude-repl--insert-formatted-code-block lang code)))))))

(defun tiqsi-claude-repl--insert-formatted-code-block (lang code)
  "Insert a beautifully formatted code block with LANG and CODE."
  (let ((inhibit-read-only t)
        (start (point)))
    ;; Top border with language label
    (insert "\n")
    (insert (tiqsi-claude-repl--colorize "╭─── " 'tiqsi-claude-repl-code-border))
    (insert (tiqsi-claude-repl--colorize (upcase (or lang "CODE")) 'tiqsi-claude-repl-code-lang))
    (insert (tiqsi-claude-repl--colorize " " 'tiqsi-claude-repl-code-border))
    (insert (tiqsi-claude-repl--colorize 
             (make-string (- 60 (length (or lang "CODE")) 5) ?─) 
             'tiqsi-claude-repl-code-border))
    (insert (tiqsi-claude-repl--colorize "╮\n" 'tiqsi-claude-repl-code-border))
    
    ;; Insert the code with syntax highlighting
    (let ((code-start (point)))
      (insert code)
      ;; Apply syntax highlighting if we have a language
      (when (and lang (> (length lang) 0))
        (tiqsi-claude-repl--apply-inline-syntax-highlighting code-start (point) lang))
      ;; Apply background to entire code area
      (let ((overlay (make-overlay code-start (point))))
        (overlay-put overlay 'face 'tiqsi-claude-repl-code-block)))
    
    ;; Make sure we end with a newline
    (unless (bolp) (insert "\n"))
    
    ;; Bottom border
    (insert (tiqsi-claude-repl--colorize 
             "╰────────────────────────────────────────────────────────────╯\n" 
             'tiqsi-claude-repl-code-border))))

(defun tiqsi-claude-repl--apply-inline-syntax-highlighting (start end lang)
  "Apply syntax highlighting between START and END for LANG."
  (let ((mode-func (cond
                    ((member lang '("python" "py")) 'python-mode)
                    ((member lang '("rust" "rs")) 'rust-mode)
                    ((member lang '("elisp" "emacs-lisp" "el")) 'emacs-lisp-mode)
                    ((member lang '("lisp" "cl")) 'lisp-mode)
                    ((member lang '("javascript" "js")) 'js-mode)
                    ((member lang '("typescript" "ts")) 'typescript-mode)
                    ((member lang '("c")) 'c-mode)
                    ((member lang '("cpp" "c++" "cc")) 'c++-mode)
                    ((member lang '("java")) 'java-mode)
                    ((member lang '("go")) 'go-mode)
                    ((member lang '("bash" "sh" "shell")) 'sh-mode)
                    ((member lang '("json")) 'json-mode)
                    ((member lang '("yaml" "yml")) 'yaml-mode)
                    ((member lang '("xml")) 'xml-mode)
                    ((member lang '("html" "htm")) 'html-mode)
                    ((member lang '("css")) 'css-mode)
                    ((member lang '("sql")) 'sql-mode)
                    ((member lang '("ruby" "rb")) 'ruby-mode)
                    ((member lang '("perl" "pl")) 'perl-mode)
                    ((member lang '("php")) 'php-mode)
                    (t nil))))
    (when (and mode-func (fboundp mode-func))
      ;; Extract the code text
      (let ((code-text (buffer-substring-no-properties start end)))
        ;; Delete original and re-insert with highlighting
        (delete-region start end)
        (goto-char start)
        ;; Insert highlighted version
        (insert
         (with-temp-buffer
           (insert code-text)
           ;; Apply the language mode
           (delay-mode-hooks (funcall mode-func))
           ;; Enable and apply font-lock
           (font-lock-mode 1)
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (font-lock-fontify-buffer))
           ;; Return the highlighted buffer content
           (buffer-string)))))))


;;; Mode Definition

;; Declare smart dispatchers (defined in tiqsi-claude-repl-opencode.el which
;; loads after this file; the bindings resolve at key-press time, not load time).
(declare-function tiqsi-repl-smart-send-input "tiqsi-claude-repl-opencode")
(declare-function tiqsi-repl-smart-cancel "tiqsi-claude-repl-opencode")
(declare-function tiqsi-repl-smart-clear "tiqsi-claude-repl-opencode")

(defvar tiqsi-claude-repl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; RET / C-c C-c / C-g / C-c C-k use smart dispatchers so the correct
    ;; backend (Claude or OpenCode) handles the request.
    (define-key map (kbd "RET") 'tiqsi-repl-smart-send-input)
    (define-key map (kbd "C-c C-c") 'tiqsi-repl-smart-send-input)
    (define-key map (kbd "C-g") 'tiqsi-repl-smart-cancel)
    (define-key map (kbd "C-c C-k") 'tiqsi-repl-smart-clear)
    (define-key map (kbd "C-c C-q") 'tiqsi-claude-repl-quit)
    (define-key map (kbd "C-c C-h") 'tiqsi-claude-highlight-all-code-blocks)
    (define-key map (kbd "C-c C-l") 'tiqsi-claude-repl-list-sessions)
    (define-key map (kbd "C-c C-H") 'tiqsi-claude-repl-history)
    (define-key map (kbd "C-c C-r") 'tiqsi-claude-repl-recover-session)
    (define-key map (kbd "C-c C-w") 'tiqsi-claude-repl-toggle-line-wrapping)
    (define-key map (kbd "C-c C-W") 'tiqsi-claude-repl-set-wrap-column)
    map)
  "Keymap for Claude REPL mode.")

(define-derived-mode tiqsi-claude-repl-mode fundamental-mode "Claude-REPL"
  "Major mode for Claude REPL interaction."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local tiqsi-claude-repl--session-started nil))

;; ---------------------------------------------------------------------------
;; Language mode mapping (ported from monolithic tiqsi-claude-repl.el)
;; ---------------------------------------------------------------------------

(defun tiqsi-claude-repl--get-language-mode (lang)
  "Get the major mode for language LANG.
Maps language names and file extensions to Emacs major modes."
  (let ((lang-lower (downcase (string-trim (or lang "")))))
    (cond
      ;; Lisp family
      ((member lang-lower '("elisp" "emacs-lisp" "el")) 'emacs-lisp-mode)
      ((member lang-lower '("lisp" "common-lisp" "cl")) 'lisp-mode)
      ((member lang-lower '("clojure" "clj" "cljs")) 'clojure-mode)
      ((member lang-lower '("scheme" "scm")) 'scheme-mode)
      ((member lang-lower '("racket" "rkt")) 'racket-mode)
      ;; Common programming languages
      ((member lang-lower '("python" "py" "python3")) 'python-mode)
      ((member lang-lower '("javascript" "js" "node")) 'js-mode)
      ((member lang-lower '("typescript" "ts" "tsx")) 'typescript-mode)
      ((member lang-lower '("jsx")) 'js-jsx-mode)
      ((member lang-lower '("java")) 'java-mode)
      ((member lang-lower '("c")) 'c-mode)
      ((member lang-lower '("c++" "cpp" "cc" "cxx")) 'c++-mode)
      ((member lang-lower '("csharp" "cs" "c#")) 'csharp-mode)
      ((member lang-lower '("rust" "rs")) 'rust-mode)
      ((member lang-lower '("go" "golang")) 'go-mode)
      ((member lang-lower '("ruby" "rb")) 'ruby-mode)
      ((member lang-lower '("perl" "pl")) 'perl-mode)
      ((member lang-lower '("php")) 'php-mode)
      ((member lang-lower '("swift")) 'swift-mode)
      ((member lang-lower '("kotlin" "kt")) 'kotlin-mode)
      ((member lang-lower '("scala")) 'scala-mode)
      ((member lang-lower '("haskell" "hs")) 'haskell-mode)
      ((member lang-lower '("ocaml" "ml")) 'tuareg-mode)
      ((member lang-lower '("fsharp" "fs" "f#")) 'fsharp-mode)
      ((member lang-lower '("r")) 'ess-mode)
      ((member lang-lower '("julia" "jl")) 'julia-mode)
      ((member lang-lower '("nim")) 'nim-mode)
      ((member lang-lower '("zig")) 'zig-mode)
      ((member lang-lower '("dart")) 'dart-mode)
      ((member lang-lower '("lua")) 'lua-mode)
      ((member lang-lower '("erlang" "erl")) 'erlang-mode)
      ((member lang-lower '("elixir" "ex" "exs")) 'elixir-mode)
      ;; Shell and system
      ((member lang-lower '("shell" "bash" "sh" "zsh")) 'sh-mode)
      ((member lang-lower '("fish")) 'fish-mode)
      ((member lang-lower '("powershell" "ps1")) 'powershell-mode)
      ((member lang-lower '("batch" "bat" "cmd")) 'bat-mode)
      ;; Config and build files
      ((member lang-lower '("dockerfile" "docker")) 'dockerfile-mode)
      ((member lang-lower '("makefile" "make" "mk")) 'makefile-mode)
      ((member lang-lower '("cmake")) 'cmake-mode)
      ((member lang-lower '("gradle")) 'groovy-mode)
      ((member lang-lower '("maven" "pom")) 'nxml-mode)
      ;; Data formats
      ((member lang-lower '("sql" "mysql" "postgresql" "sqlite")) 'sql-mode)
      ((member lang-lower '("json" "jsonc")) 'json-mode)
      ((member lang-lower '("yaml" "yml")) 'yaml-mode)
      ((member lang-lower '("toml")) 'conf-toml-mode)
      ((member lang-lower '("xml")) 'nxml-mode)
      ((member lang-lower '("csv")) 'csv-mode)
      ((member lang-lower '("ini" "conf" "config")) 'conf-mode)
      ;; Web technologies
      ((member lang-lower '("html" "htm")) 'html-mode)
      ((member lang-lower '("css" "scss" "sass" "less")) 'css-mode)
      ((member lang-lower '("vue")) 'vue-mode)
      ((member lang-lower '("svelte")) 'svelte-mode)
      ;; Documentation
      ((member lang-lower '("markdown" "md")) 'markdown-mode)
      ((member lang-lower '("org" "org-mode")) 'org-mode)
      ((member lang-lower '("latex" "tex")) 'latex-mode)
      ((member lang-lower '("rst" "restructuredtext")) 'rst-mode)
      ((member lang-lower '("asciidoc" "adoc")) 'adoc-mode)
      ;; Assembly
      ((member lang-lower '("asm" "assembly" "nasm")) 'asm-mode)
      ((member lang-lower '("mips")) 'mips-mode)
      ;; Other
      ((member lang-lower '("diff" "patch")) 'diff-mode)
      ((member lang-lower '("nginx")) 'nginx-mode)
      ((member lang-lower '("terraform" "tf")) 'terraform-mode)
      ((member lang-lower '("graphql" "gql")) 'graphql-mode)
      ((member lang-lower '("protobuf" "proto")) 'protobuf-mode)
      ;; Fallback
      (t (if (string-match-p "[a-z]+" lang-lower) 'prog-mode 'fundamental-mode)))))

(defun tiqsi-claude-repl--safe-mode-available-p (mode)
  "Check if MODE is available and can be safely called."
  (and mode
    (fboundp mode)
    (or (functionp mode)
      (ignore-errors (autoload-do-load (symbol-function mode) mode)))))

;; ---------------------------------------------------------------------------
;; Enhanced thinking animation
;; ---------------------------------------------------------------------------

(defun tiqsi-claude-repl--animate-thinking ()
  "Animate the thinking indicator with braille spinner and elapsed time."
  (when (and tiqsi-claude-repl--current-process
          (process-live-p tiqsi-claude-repl--current-process))
    (let* ((frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))
           (frame (nth (mod tiqsi-claude-repl--thinking-animation-state (length frames)) frames)))
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "⏳ Thinking\\.\\.\\..*$" nil t)
          (let ((elapsed (if tiqsi-claude-repl--request-start-time
                           (format " [%.1fs]" (float-time (time-since tiqsi-claude-repl--request-start-time)))
                           "")))
            (replace-match (tiqsi-claude-repl--colorize
                             (format "%s Thinking...%s" frame elapsed)
                             'tiqsi-claude-repl-thinking)))))
      (setq tiqsi-claude-repl--thinking-animation-state
        (1+ tiqsi-claude-repl--thinking-animation-state)))))

(provide 'tiqsi-claude-repl-core)

;;; tiqsi-claude-repl-core.el ends here