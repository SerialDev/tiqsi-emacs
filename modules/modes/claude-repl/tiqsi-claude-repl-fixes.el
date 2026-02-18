;;; tiqsi-claude-repl-fixes.el --- All fixes for Claude REPL -*- lexical-binding: t -*-

;;; Commentary:
;; Consolidated fixes for Claude REPL issues including:
;; - JSON processing and display
;; - Thinking indicator management
;; - Code syntax highlighting
;; - Tool output formatting
;; - Process sentinel handling

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Variables for state tracking

(defvar-local tiqsi-claude-repl--json-buffer ""
  "Buffer for accumulating JSON lines.")

(defvar-local tiqsi-claude-repl--output-start nil
  "Marker for output start.")

(defvar-local tiqsi-claude-repl--thinking-cleaned nil
  "Whether thinking has been cleaned for current response.")

(defvar-local tiqsi-claude-repl--assistant-started nil
  "Whether assistant response has started.")

;;; Core JSON Processing

(defun tiqsi-claude-repl--process-json-lines (output)
  "Process OUTPUT containing JSON lines."
  ;; Accumulate output
  (setq tiqsi-claude-repl--json-buffer (concat tiqsi-claude-repl--json-buffer output))
  
  ;; Process line by line (Claude sends one JSON object per line)
  (let ((lines (split-string tiqsi-claude-repl--json-buffer "\n"))
        (remaining ""))
    (dolist (line lines)
      (let ((trimmed (string-trim line)))
        (cond
         ;; Empty line - skip
         ((string-empty-p trimmed) nil)
         
         ;; Looks like complete JSON - try to parse
         ((and (string-prefix-p "{" trimmed)
               (string-suffix-p "}" trimmed))
          (condition-case err
              (tiqsi-claude-repl--handle-json-object trimmed)
            (json-parse-error
             ;; Not complete, save for later
             (setq remaining (concat remaining line "\n")))))
         
         ;; Incomplete line - save for next batch
         (t
          (setq remaining (concat remaining line "\n"))))))
    
    ;; Keep incomplete data for next call
    (setq tiqsi-claude-repl--json-buffer (string-trim remaining))))

(defun tiqsi-claude-repl--handle-json-object (json-string)
  "Handle a single JSON object from JSON-STRING."
  (let* ((json-obj (json-parse-string json-string))
         (type (gethash "type" json-obj))
         (inhibit-read-only t))
    
    ;; Debug logging - always log for now to diagnose
    (let ((repr (prin1-to-string json-obj)))
      (message "Claude JSON: type=%s, obj=%s" type (substring repr 0 (min 100 (length repr)))))
    
    (cond
     ;; System initialization
     ((equal type "system")
      (let ((subtype (gethash "subtype" json-obj)))
        (when (equal subtype "init")
          ;; Don't display system init messages - they clutter the output
          (message "System initialized with session: %s" (gethash "session_id" json-obj)))))
     
     ;; Assistant message - full response (non-streaming format)
     ((equal type "assistant")
      (let* ((message (gethash "message" json-obj))
             (content (when message (gethash "content" message))))
        (when content
          ;; Clean thinking indicator
          (tiqsi-claude-repl--clean-thinking-indicator)
          (save-excursion
            (goto-char (point-max))
            (insert "\nAssistant: ")
            (setq tiqsi-claude-repl--output-start (point-marker))
            ;; Process content
            (if (vectorp content)
                ;; Array of content blocks
                (dotimes (i (length content))
                  (let* ((block (aref content i))
                         (block-type (gethash "type" block))
                         (text (gethash "text" block)))
                    (when (and (equal block-type "text") text)
                      (insert text))))
              ;; Simple string content
              (when (stringp content)
                (insert content)))))))
     
     ;; Content block start
     ((equal type "content_block_start")
      (save-excursion
        (goto-char (point-max))
        (unless tiqsi-claude-repl--assistant-started
          (insert "\nAssistant: ")
          (setq tiqsi-claude-repl--assistant-started t)
          (setq tiqsi-claude-repl--output-start (point-marker)))))
     
     ;; Content block delta - the actual streaming text
     ((equal type "content_block_delta")
      (let* ((delta (gethash "delta" json-obj))
             (text (gethash "text" delta)))
        (when text
          ;; Remove thinking indicator on first real content
          (tiqsi-claude-repl--clean-thinking-indicator)
          ;; Insert the text
          (save-excursion
            (goto-char (point-max))
            (insert text)))))
     
     ;; Tool use
     ((equal type "tool_use")
      (tiqsi-claude-repl--format-tool-use json-obj))
     
     ;; Tool result
     ((equal type "tool_result")
      (save-excursion
        (goto-char (point-max))
        (insert "\n📊 Tool Result:\n")
        (when-let ((content (gethash "content" json-obj)))
          (if (listp content)
              (dolist (block content)
                (when (hash-table-p block)
                  (when-let ((text (gethash "text" block)))
                    (insert text "\n"))))
            (insert content "\n")))))
     
     ;; Content block stop - apply formatting
     ((equal type "content_block_stop")
      (when tiqsi-claude-repl--output-start
        (tiqsi-claude-repl--post-process-response 
         tiqsi-claude-repl--output-start (point-max))))
     
     ;; Message stop - finalize
     ((equal type "message_stop")
      ;; Format the complete response before cleanup
      (when tiqsi-claude-repl--output-start
        (tiqsi-claude-repl--post-process-response 
         tiqsi-claude-repl--output-start (point-max)))
      ;; Clean up any JSON artifacts
      (save-excursion
        (goto-char (point-max))
        ;; Look for common JSON artifact patterns
        (when (re-search-backward "\\(:0}\\|,\"output_tokens\":\\|,\"service_tier\":\\|},\"parent_tool_use_id\":\\)" nil t)
          (delete-region (match-beginning 0) (point-max))))
      ;; Add separator
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n────────────────────────────────────────────────────────────\n"))
      ;; Apply syntax highlighting to code blocks
      (run-with-timer 0.2 nil
                      (lambda ()
                        (when (and (buffer-live-p (current-buffer))
                                   (fboundp 'tiqsi-claude-highlight-all-code-blocks))
                          (with-current-buffer (current-buffer)
                            (tiqsi-claude-highlight-all-code-blocks)))))
      ;; Reset state
      (setq tiqsi-claude-repl--assistant-started nil)
      (setq tiqsi-claude-repl--output-start nil)
      (setq tiqsi-claude-repl--thinking-cleaned nil))
     
     ;; Catch-all for unhandled types
     (t
      (message "Unhandled JSON type: %s" type)
      ;; If it has text content, try to display it
      (when-let ((text (or (gethash "text" json-obj)
                          (gethash "content" json-obj))))
        (save-excursion
          (goto-char (point-max))
          (insert text)))))))

;;; Thinking Indicator Management

(defun tiqsi-claude-repl--clean-thinking-indicator ()
  "Remove thinking indicator from buffer."
  (when (not tiqsi-claude-repl--thinking-cleaned)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (when (search-backward "⏳ Thinking" nil t)
          (beginning-of-line)
          (when (looking-at "⏳ Thinking[^\n]*\n?")
            (delete-region (match-beginning 0) (match-end 0))
            (setq tiqsi-claude-repl--thinking-cleaned t)))))
    ;; Also stop animation timer
    (when (and (boundp 'tiqsi-claude-repl--thinking-timer)
               tiqsi-claude-repl--thinking-timer)
      (cancel-timer tiqsi-claude-repl--thinking-timer)
      (setq tiqsi-claude-repl--thinking-timer nil))))

;;; Tool Formatting

(defun tiqsi-claude-repl--format-tool-use (json-obj)
  "Format tool use from JSON-OBJ."
  (let ((name (gethash "name" json-obj))
        (input (gethash "input" json-obj))
        (inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (insert "\n🔧 " (propertize (format "Using %s" name) 'face 'tiqsi-claude-repl-info) "\n")
      
      ;; Special handling for Write tool
      (when (and (equal name "Write") 
                 (hash-table-p input)
                 (gethash "content" input))
        (let* ((file-path (gethash "file_path" input))
               (content (gethash "content" input))
               (ext (file-name-extension file-path))
               (lang (pcase ext
                       ("py" "python")
                       ("rs" "rust") 
                       ("js" "javascript")
                       ("el" "elisp")
                       (_ ext))))
          (insert "   File: " (propertize file-path 'face 'tiqsi-claude-repl-code-lang) "\n\n")
          (insert (format "```%s\n" (or lang "")))
          (let ((code-start (point)))
            (insert content)
            (unless (string-suffix-p "\n" content) (insert "\n"))
            (insert "```\n")
            ;; Mark for syntax highlighting
            (put-text-property code-start (- (point) 4) 'claude-code-lang lang)))))))

;;; Response Formatting

(defun tiqsi-claude-repl--post-process-response (start end)
  "Post-process and format Claude's response between START and END."
  (message "Post-processing response from %s to %s" start end)
  (save-excursion
    ;; Process code blocks
    (goto-char start)
    (let ((found-code-blocks 0))
      (while (re-search-forward "```\\([a-zA-Z0-9_+-]*\\)\n" end t)
        (setq found-code-blocks (1+ found-code-blocks))
        (let* ((lang (match-string 1))
               (lang-start (match-beginning 0))
               (code-start (point))
               (code-end (when (re-search-forward "```" end t)
                          (match-beginning 0))))
          (message "Found code block %d: lang=%s, start=%s, end=%s" 
                   found-code-blocks lang code-start code-end)
          (when code-end
            ;; Add language label with face
            (save-excursion
              (goto-char lang-start)
              (delete-region lang-start code-start)
              (insert (propertize (format "```%s" lang) 
                                'face 'tiqsi-claude-repl-code-lang) "\n")
              (setq code-start (point)))
            ;; Apply background to entire code block
            (add-text-properties code-start code-end
                               '(face (:background "#2e3440" :extend t)))
            ;; Apply syntax highlighting
            (when (and tiqsi-claude-repl-highlight-code
                       (not (string-empty-p lang)))
              (tiqsi-claude-repl--apply-syntax-highlighting code-start code-end lang))
            ;; Add line numbers
            (when tiqsi-claude-repl-show-line-numbers
              (save-excursion
                (goto-char code-start)
                (tiqsi-claude-repl--add-line-numbers code-start code-end))))))
      (message "Processed %d code blocks" found-code-blocks))
    
    ;; Apply markdown formatting
    (tiqsi-claude-repl--apply-markdown-formatting start end)))

(defun tiqsi-claude-repl--apply-syntax-highlighting (start end lang)
  "Apply syntax highlighting for LANG between START and END."
  (message "Applying syntax highlighting for %s from %s to %s" lang start end)
  (let ((mode-fn (pcase (downcase lang)
                   ((or "python" "py") 'python-mode)
                   ((or "rust" "rs") 'rust-mode)
                   ((or "javascript" "js") 'js-mode)
                   ((or "elisp" "emacs-lisp" "lisp") 'emacs-lisp-mode)
                   ((or "c" "cpp" "c++") 'c++-mode)
                   ((or "java") 'java-mode)
                   ((or "go") 'go-mode)
                   ((or "ruby" "rb") 'ruby-mode)
                   ((or "shell" "bash" "sh") 'sh-mode)
                   (_ nil))))
    (when mode-fn
      (let ((code (buffer-substring-no-properties start end))
            (orig-buffer (current-buffer)))
        ;; Use font-lock to get the highlighting
        (with-temp-buffer
          (insert code)
          (funcall mode-fn)
          (font-lock-ensure)
          ;; Copy the fontified text back
          (let ((fontified-code (buffer-substring (point-min) (point-max))))
            (with-current-buffer orig-buffer
              (save-excursion
                (goto-char start)
                (delete-region start end)
                (insert fontified-code)
                ;; Ensure background is preserved
                (add-text-properties start (point)
                                   '(face (:background "#2e3440" :extend t)))))))))))

(defun tiqsi-claude-repl--add-line-numbers (start end)
  "Add line numbers between START and END."
  (save-excursion
    (goto-char start)
    (let ((line-num 1))
      (while (and (< (point) end) (not (eobp)))
        (beginning-of-line)
        (insert (propertize (format "%3d │ " line-num)
                          'face 'tiqsi-claude-repl-line-number))
        (setq line-num (1+ line-num))
        (forward-line 1)))))

(defun tiqsi-claude-repl--apply-markdown-formatting (start end)
  "Apply markdown formatting between START and END."
  (save-excursion
    ;; Bold text
    (goto-char start)
    (while (re-search-forward "\\*\\*\\([^*\n]+\\)\\*\\*" end t)
      (add-text-properties (match-beginning 1) (match-end 1)
                         '(face (:weight bold))))
    ;; Italic text  
    (goto-char start)
    (while (re-search-forward "_\\([^_\n]+\\)_" end t)
      (add-text-properties (match-beginning 1) (match-end 1)
                         '(face (:slant italic))))
    ;; Headers
    (goto-char start)
    (while (re-search-forward "^\\(#+\\) \\(.+\\)$" end t)
      (add-text-properties (match-beginning 2) (match-end 2)
                         '(face (:weight bold :height 1.2 :foreground "#81a1c1"))))))

;;; Process Filter and Sentinel

(defun tiqsi-claude-repl--fixed-process-filter (process output)
  "Fixed process filter for Claude output."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (condition-case err
            (save-excursion
              (goto-char (point-max))
              
              ;; Debug: show raw output in messages
              (when (< (length output) 200)
                (message "Raw output: %s" (replace-regexp-in-string "\n" "\\\\n" output)))
              
              ;; Check if this is JSON output
              (cond
               ;; JSON streaming format
               ((or (string-match-p "^[[:space:]]*{.*\"type\":" output)
                    (not (string-empty-p tiqsi-claude-repl--json-buffer)))
                (tiqsi-claude-repl--process-json-lines output))
               
               ;; Regular text output
               (t
                (insert output))))
          (error
           (message "Error in process filter: %s" (error-message-string err))
           ;; Try to insert output anyway
           (goto-char (point-max))
           (insert output)))))))

(defun tiqsi-claude-repl--fixed-process-sentinel (process event)
  "Fixed sentinel for Claude process."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          
          ;; Clean up thinking indicator
          (tiqsi-claude-repl--clean-thinking-indicator)
          
          ;; Handle different exit events
          (cond
           ((string-match-p "finished" event)
            (unless (bolp) (insert "\n")))
           ((string-match-p "killed\\|interrupt" event)
            (insert "\n❌ Process interrupted\n"))
           ((string-match-p "exited abnormally" event)
            (insert "\n❌ Process error: " (string-trim event) "\n")))
          
          ;; Insert new prompt
          (unless (bolp) (insert "\n"))
          (insert (if (fboundp 'tiqsi-claude-repl--format-prompt)
                      (tiqsi-claude-repl--format-prompt)
                    "λ "))
          
          ;; Reset state
          (setq tiqsi-claude-repl--output-start nil)
          (setq tiqsi-claude-repl--json-buffer "")
          (setq tiqsi-claude-repl--assistant-started nil)
          (setq tiqsi-claude-repl--thinking-cleaned nil))))))

;;; Apply all fixes

(defun tiqsi-claude-repl--apply-all-fixes ()
  "Apply all fixes to make Claude REPL work."
  ;; Define the sentinel if it doesn't exist
  (unless (fboundp 'tiqsi-claude-repl--process-sentinel)
    (defalias 'tiqsi-claude-repl--process-sentinel 
              #'tiqsi-claude-repl--fixed-process-sentinel))
  
  ;; Remove any existing advice first
  (when (fboundp 'tiqsi-claude-repl--process-filter)
    (advice-remove 'tiqsi-claude-repl--process-filter :override))
  (when (fboundp 'tiqsi-claude-repl--process-sentinel)
    (advice-remove 'tiqsi-claude-repl--process-sentinel :override))
  
  ;; Apply our fixed versions
  (when (fboundp 'tiqsi-claude-repl--process-filter)
    (advice-add 'tiqsi-claude-repl--process-filter :override
                #'tiqsi-claude-repl--fixed-process-filter))
  (when (fboundp 'tiqsi-claude-repl--process-sentinel)
    (advice-add 'tiqsi-claude-repl--process-sentinel :override
                #'tiqsi-claude-repl--fixed-process-sentinel))
  
  (message "Claude REPL fixes applied successfully"))

;; Define sentinel immediately if needed
(unless (fboundp 'tiqsi-claude-repl--process-sentinel)
  (defalias 'tiqsi-claude-repl--process-sentinel 
            #'tiqsi-claude-repl--fixed-process-sentinel))

;; Hook into the mode to apply fixes when Claude REPL starts
(add-hook 'tiqsi-claude-repl-mode-hook #'tiqsi-claude-repl--apply-all-fixes)

;; Also hook into process creation
(defun tiqsi-claude-repl--fix-process-setup (&rest _)
  "Ensure process has proper filter and sentinel."
  (when (and (boundp 'tiqsi-claude-repl--current-process)
             tiqsi-claude-repl--current-process
             (processp tiqsi-claude-repl--current-process))
    (set-process-filter tiqsi-claude-repl--current-process 
                        #'tiqsi-claude-repl--fixed-process-filter)
    (set-process-sentinel tiqsi-claude-repl--current-process 
                          #'tiqsi-claude-repl--fixed-process-sentinel)))

;; Hook into the send function to fix process on each send
(with-eval-after-load 'tiqsi-claude-repl-features
  (when (fboundp 'tiqsi-claude-repl--send-to-claude)
    (advice-add 'tiqsi-claude-repl--send-to-claude :after
                #'tiqsi-claude-repl--fix-process-setup))
  (tiqsi-claude-repl--apply-all-fixes))

(with-eval-after-load 'tiqsi-claude-repl-core
  (tiqsi-claude-repl--apply-all-fixes))

;; Apply immediately if already loaded
(when (fboundp 'tiqsi-claude-repl--process-filter)
  (tiqsi-claude-repl--apply-all-fixes))

(provide 'tiqsi-claude-repl-fixes)

;;; tiqsi-claude-repl-fixes.el ends here