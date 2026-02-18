;;; tiqsi-claude-repl-features.el --- Claude REPL features -*- lexical-binding: t -*-

;;; Commentary:
;; History, context, and interactive commands for Claude REPL

;;; Code:

(require 'cl-lib)
(require 'json)

;; Declare functions from other files
(declare-function tiqsi-claude-repl--colorize "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-timestamp "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-status "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-prompt "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--insert-header "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--thinking-indicator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--get-or-create-buffer "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--get-project-root "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--executable-available-p "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--build-smart-context "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl-mode "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--insert-code-block "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--apply-markdown-formatting "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl--process-filter "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--process-sentinel "tiqsi-claude-repl-core")

;; Declare variables from other files
(defvar tiqsi-claude-repl--current-process)
(defvar tiqsi-claude-repl--conversation-history)
(defvar tiqsi-claude-repl--request-start-time)

;;; History Functions

(defun tiqsi-claude-repl--ensure-history-directory ()
  "Ensure the history directory exists."
  (unless (file-exists-p tiqsi-claude-repl-history-directory)
    (make-directory tiqsi-claude-repl-history-directory t)))

(defun tiqsi-claude-repl--generate-session-id ()
  "Generate a unique session ID."
  (format "%s-%s" 
    (format-time-string "%Y%m%d-%H%M%S")
    (random 10000)))

(defun tiqsi-claude-repl--save-conversation ()
  "Save the current conversation to history."
  (when (and tiqsi-claude-repl-save-history
          tiqsi-claude-repl--conversation-history)
    (tiqsi-claude-repl--ensure-history-directory)
    (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
            (filename (expand-file-name 
                       (format "conversation-%s.json" timestamp)
                       tiqsi-claude-repl-history-directory))
            (conversation-data
              `((id . ,tiqsi-claude-repl--conversation-id)
                (timestamp . ,timestamp)
                (project . ,(tiqsi-claude-repl--get-project-root))
                (messages . ,tiqsi-claude-repl--conversation-history))))
      (with-temp-file filename
        (insert (json-encode conversation-data)))
      (message "Conversation saved to %s" filename))))

(defun tiqsi-claude-repl--load-conversation (filename)
  "Load a conversation from FILENAME."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (let* ((data (json-read))
             (messages (cdr (assoc 'messages data))))
        messages))))

;;; Smart Context Functions

(defun tiqsi-claude-repl--get-file-context (file)
  "Get context for FILE."
  (when (and file (file-exists-p file))
    (format "File: %s\nLanguage: %s\n" 
      file 
      (or (file-name-extension file) "unknown"))))

(defun tiqsi-claude-repl--get-project-context ()
  "Get current project context."
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (git-branch (ignore-errors
                      (string-trim
                       (shell-command-to-string 
                        (format "cd %s && git branch --show-current 2>/dev/null" 
                          project-root)))))
         (readme-file (expand-file-name "README.md" project-root))
         (package-file (or (expand-file-name "package.json" project-root)
                         (expand-file-name "Cargo.toml" project-root)
                         (expand-file-name "setup.py" project-root))))
    (concat
     (format "Project: %s\n" project-name)
     (when (and git-branch (> (length git-branch) 0))
       (format "Git Branch: %s\n" git-branch))
     (when (file-exists-p readme-file)
       "README.md exists\n")
     (when (and package-file (file-exists-p package-file))
       (format "Package file: %s\n" (file-name-nondirectory package-file))))))

(defun tiqsi-claude-repl--build-smart-context ()
  "Build smart context for the current environment."
  (when tiqsi-claude-repl-use-smart-context
    (let ((context-parts (list)))
      ;; Add project context
      (push (tiqsi-claude-repl--get-project-context) context-parts)
      
      ;; Add current file context if applicable
      (when buffer-file-name
        (push (tiqsi-claude-repl--get-file-context buffer-file-name) context-parts))
      
      ;; Add error context if in a compilation buffer
      (when (derived-mode-p 'compilation-mode)
        (push "Context: Viewing compilation errors\n" context-parts))
      
      (string-join (delq nil context-parts) "\n"))))

;;; Interactive Commands

;;;###autoload
(defun tiqsi-claude-repl-start ()
  "Start Claude REPL session."
  (interactive)
  (unless (tiqsi-claude-repl--executable-available-p)
    (error "%s" (tiqsi-claude-repl--format-status "Error" 
                  "Claude CLI not found. Install with: npm install -g @anthropic-ai/claude-code")))
  (let ((repl-buffer (tiqsi-claude-repl--get-or-create-buffer)))
    (with-current-buffer repl-buffer
      (goto-char (point-max))
      (unless (> (buffer-size) 0)
        (setq-local tiqsi-claude-repl--session-start-time (current-time))
        (setq-local tiqsi-claude-repl--session-id (tiqsi-claude-repl--generate-session-id))
        (setq-local tiqsi-claude-repl--message-count 0)
        (setq-local tiqsi-claude-repl--last-interaction-time (current-time))
        (tiqsi-claude-repl--insert-header)
        (tiqsi-claude-repl--insert-prompt)
        (tiqsi-claude-repl--log-operation "Started" 
          (format "Session %s for %s" 
            tiqsi-claude-repl--session-id
            (file-name-nondirectory 
              (directory-file-name 
                (tiqsi-claude-repl--get-project-root)))))))
    (display-buffer repl-buffer)
    (select-window (get-buffer-window repl-buffer))))

;;;###autoload
(defun tiqsi-claude-repl-ask-question (question)
  "Ask QUESTION to Claude."
  (interactive "sAsk Claude: ")
  (let ((repl-buffer (tiqsi-claude-repl--get-or-create-buffer)))
    (with-current-buffer repl-buffer
      (goto-char (point-max))
      (insert (tiqsi-claude-repl--colorize question 'tiqsi-claude-repl-input))
      (tiqsi-claude-repl-send-input))))

;;;###autoload
(defun tiqsi-claude-repl-send-input ()
  "Send current input to Claude."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    ;; First ensure we have a prompt
    (tiqsi-claude-repl--ensure-prompt)
    (let ((input (tiqsi-claude-repl--get-input)))
      (when (and input (> (length (string-trim input)) 0))
        ;; Immediately show feedback
        (tiqsi-claude-repl--prepare-for-response)
        ;; Send in next event loop to ensure UI updates
        (run-at-time 0.01 nil #'tiqsi-claude-repl--send-to-claude input)))))

(defun tiqsi-claude-repl--get-input ()
  "Get current input from buffer."
  (save-excursion
    (goto-char (point-max))
    (let ((end (point)))
      (beginning-of-line)
      (if (re-search-forward "λ " nil t)
          (string-trim (buffer-substring-no-properties (point) end))
        ""))))

(defun tiqsi-claude-repl--prepare-for-response ()
  "Prepare the buffer for Claude's response with immediate feedback."
  ;; Move to end of buffer
  (goto-char (point-max))
  
  ;; Add "You: " prefix to the input line
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^λ ")
      (forward-char 2)
      (insert (tiqsi-claude-repl--colorize "You: " 'tiqsi-claude-repl-prompt))))
  
  ;; Insert a newline after the input
  (insert "\n")
  
  ;; Insert thinking indicator immediately
  (insert (tiqsi-claude-repl--thinking-indicator) "\n")
  
  ;; Set request start time
  (setq-local tiqsi-claude-repl--request-start-time (current-time))
  
  ;; Force display update
  (sit-for 0)
  
  ;; Start thinking animation if enabled
  (when tiqsi-claude-repl-animate-thinking
    (tiqsi-claude-repl--start-thinking-animation)))

(defun tiqsi-claude-repl--send-to-claude (input)
  "Send INPUT to Claude process."
  ;; Add to conversation history
  (push (cons 'user input) tiqsi-claude-repl--conversation-history)
  
  ;; Kill any existing process
  (when (and tiqsi-claude-repl--current-process
          (process-live-p tiqsi-claude-repl--current-process))
    (delete-process tiqsi-claude-repl--current-process))
  
  ;; Start Claude process - use continue for maintaining context
  (let* ((context (tiqsi-claude-repl--build-smart-context))
         (full-input (if (> (length context) 0)
                       (format "%s\n\n%s" context input)
                       input))
         ;; Build base arguments
         (base-args (if tiqsi-claude-repl--session-started
                        (list "--continue" "--print")
                      (list "--print")))
         ;; Add model if specified
         (model-args (if tiqsi-claude-repl-model
                         (append base-args (list "--model" tiqsi-claude-repl-model))
                       base-args))
         ;; Add output format and verbose flags for thinking traces
         (output-args (append model-args 
                              (list "--output-format" "stream-json"
                                    "--verbose")))
         ;; Add the input as the last argument
         (args (append output-args (list full-input)))
         (process (apply 'start-process
                         "claude-repl"
                         (current-buffer)
                         tiqsi-claude-repl-program
                         args)))
    ;; Make sure process is buffer-local
    (setq-local tiqsi-claude-repl--current-process process)
    (setq-local tiqsi-claude-repl--session-started t)
    (set-process-filter process 'tiqsi-claude-repl--process-filter)
    (set-process-sentinel process 'tiqsi-claude-repl--process-sentinel)
    ;; Debug process output
    (set-process-query-on-exit-flag process nil)))

;;;###autoload
(defun tiqsi-claude-repl-send-region (start end)
  "Send region to Claude."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
         (length (- end start)))
    (tiqsi-claude-repl--log-operation "Region Sent" 
      (format "%d characters from %s" length (buffer-name)))
    (tiqsi-claude-repl-ask-question text)))

;;;###autoload
(defun tiqsi-claude-repl-send-function ()
  "Send current function to Claude."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point))
           (func-name (save-excursion
			(beginning-of-defun)
			(when (re-search-forward "defun\\s-+\\([^ (]+\\)" (line-end-position) t)
                          (match-string 1)))))
      (end-of-defun)
      (tiqsi-claude-repl--log-operation "Function Sent" 
        (format "Function: %s" (or func-name "<unnamed>")))
      (tiqsi-claude-repl-send-region start (point)))))

;;;###autoload
(defun tiqsi-claude-repl-send-buffer ()
  "Send entire buffer to Claude."
  (interactive)
  (tiqsi-claude-repl--log-operation "Buffer Sent" 
    (format "File: %s (%d lines)" 
      (or (buffer-file-name) (buffer-name))
      (count-lines (point-min) (point-max))))
  (tiqsi-claude-repl-send-region (point-min) (point-max)))

;;;###autoload
(defun tiqsi-claude-repl-send-paragraph ()
  "Send current paragraph to Claude."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
           (end (progn (forward-paragraph) (point))))
      (tiqsi-claude-repl--log-operation "Paragraph Sent" 
        (format "%d characters" (- end start)))
      (tiqsi-claude-repl-send-region start end))))

;;;###autoload
(defun tiqsi-claude-repl-explain-code ()
  "Ask Claude to explain code."
  (interactive)
  (tiqsi-claude-repl--log-operation "Explain Code" "Requesting code explanation")
  (if (region-active-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (tiqsi-claude-repl-ask-question 
        (format "Please explain what this code does:\n\n%s" text)))
    (tiqsi-claude-repl-send-function)))

;;;###autoload
(defun tiqsi-claude-repl-optimize-code ()
  "Ask Claude to optimize code."
  (interactive)
  (tiqsi-claude-repl--log-operation "Optimize Code" "Requesting code optimization")
  (if (region-active-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (tiqsi-claude-repl-ask-question 
        (format "Please optimize this code:\n\n%s" text)))
    (tiqsi-claude-repl-send-function)))

;;;###autoload
(defun tiqsi-claude-repl-fix-error-at-point ()
  "Ask Claude to fix error."
  (interactive)
  (tiqsi-claude-repl--log-operation "Fix Error" "Analyzing code for errors")
  (let ((text (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
                (save-excursion
                  (beginning-of-defun)
                  (let ((start (point)))
		    (end-of-defun)
		    (buffer-substring-no-properties start (point)))))))
    (tiqsi-claude-repl-ask-question 
      (format "Please fix any errors in this code:\n\n%s" text))))

;;;###autoload
(defun tiqsi-claude-repl-generate-tests ()
  "Ask Claude to generate tests."
  (interactive)
  (tiqsi-claude-repl--log-operation "Generate Tests" "Requesting test generation")
  (if (region-active-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (tiqsi-claude-repl-ask-question 
        (format "Please generate tests for this code:\n\n%s" text)))
    (tiqsi-claude-repl-send-function)))

;;;###autoload
(defun tiqsi-claude-repl-cancel ()
  "Cancel current Claude request."
  (interactive)
  (if (eq major-mode 'tiqsi-claude-repl-mode)
      ;; Check for either active process or thinking animation
      (cond
       ;; Case 1: Active process
       ((and tiqsi-claude-repl--current-process
             (process-live-p tiqsi-claude-repl--current-process))
        ;; Kill the process
        (delete-process tiqsi-claude-repl--current-process)
        (setq tiqsi-claude-repl--current-process nil)
        ;; Stop thinking animation
        (tiqsi-claude-repl--stop-thinking-animation)
        ;; Clean up and show cancelled message
        (tiqsi-claude-repl--cleanup-cancelled-request)
        (message "🛑 Claude request cancelled"))
       
       ;; Case 2: Thinking animation is running (process might be starting)
       (tiqsi-claude-repl--thinking-timer
        ;; Stop thinking animation
        (tiqsi-claude-repl--stop-thinking-animation)
        ;; Clean up thinking indicator
        (condition-case nil
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "⏳ Thinking" nil t)
                (beginning-of-line)
                (delete-region (point) (point-max))))
          (error nil))
        ;; Clean up and show cancelled message
        (tiqsi-claude-repl--cleanup-cancelled-request)
        (message "🛑 Claude request cancelled (early stage)"))
       
       ;; Case 3: Nothing to cancel
       (t
        (tiqsi-claude-repl--ensure-prompt)
        (message "ℹ️  No active Claude request to cancel")))
    ;; Not in claude repl mode
    (message "Not in Claude REPL mode")))

(defun tiqsi-claude-repl--cleanup-cancelled-request ()
  "Clean up after cancelling a request."
  ;; First try to clean up any thinking message
  (save-excursion
    (goto-char (point-max))
    (condition-case nil
        (when (search-backward "⏳ Thinking" nil t)
          (beginning-of-line)
          (let ((start (point)))
            (end-of-line)
            (delete-region start (1+ (point)))))
      (error nil)))
  ;; Now add the cancellation message
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (insert (tiqsi-claude-repl--make-separator 60 "✖") "\n")
  (insert (tiqsi-claude-repl--format-status "Cancelled" "Request interrupted by user") "\n")
  (insert (tiqsi-claude-repl--make-separator 60 "─") "\n")
  ;; Always ensure prompt
  (tiqsi-claude-repl--ensure-prompt)
  ;; Reset state variables
  (setq-local tiqsi-claude-repl--output-start nil)
  (setq-local tiqsi-claude-repl--json-buffer "")
  (setq-local tiqsi-claude-repl--json-processing-enabled nil)
  (setq-local tiqsi-claude-repl--request-start-time nil)
  (setq-local tiqsi-claude-repl--current-response ""))

;;;###autoload
(defun tiqsi-claude-repl-clear ()
  "Clear the Claude REPL buffer."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (tiqsi-claude-repl--insert-header)
      (tiqsi-claude-repl--insert-prompt)
      (setq-local tiqsi-claude-repl--conversation-history nil))))

;;;###autoload
(defun tiqsi-claude-repl-quit ()
  "Quit Claude REPL."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    ;; Save conversation history before quitting
    (tiqsi-claude-repl--save-conversation)
    (when (and tiqsi-claude-repl--current-process
	    (process-live-p tiqsi-claude-repl--current-process))
      (delete-process tiqsi-claude-repl--current-process))
    (kill-buffer)))

;;;###autoload
(defun tiqsi-claude-repl-kill ()
  "Kill Claude REPL for current project."
  (interactive)
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
          (buffer-name (format "*Claude REPL (%s)*" 
                         (file-name-nondirectory (directory-file-name project-root)))))
    (when-let* ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        ;; Save conversation history before killing
        (tiqsi-claude-repl--save-conversation)
        (when (and tiqsi-claude-repl--current-process
                (process-live-p tiqsi-claude-repl--current-process))
          (delete-process tiqsi-claude-repl--current-process)))
      (kill-buffer buffer)
      (message "Killed %s" buffer-name))))

;;;###autoload
(defun tiqsi-claude-repl-list-sessions ()
  "List all Claude REPL sessions."
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (buf)
                    (with-current-buffer buf
                      (eq major-mode 'tiqsi-claude-repl-mode)))
                  (buffer-list))))
    (if buffers
        (let ((sessions (mapcar
                         (lambda (buf)
                           (format "%s" (buffer-name buf)))
                         buffers)))
          (message "Active Claude sessions: %s" (string-join sessions ", ")))
      (message "No active Claude sessions"))))

;;;###autoload
(defun tiqsi-claude-repl-history ()
  "Browse Claude conversation history."
  (interactive)
  (tiqsi-claude-repl--ensure-history-directory)
  (let ((history-files (directory-files tiqsi-claude-repl-history-directory 
                                      t "conversation-.*\\.json$")))
    (if history-files
        (let ((file (completing-read "Load conversation: " history-files)))
          (when file
            (let ((messages (tiqsi-claude-repl--load-conversation file)))
              (message "Loaded %d messages from %s" 
                      (length messages) 
                      (file-name-nondirectory file)))))
      (message "No conversation history found"))))

;;;###autoload
(defun tiqsi-claude-repl-recover-prompt ()
  "Recover the REPL prompt if it's missing."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (tiqsi-claude-repl--ensure-prompt)
    (message "✅ Prompt recovered")))

;;; Animation Functions

(defun tiqsi-claude-repl--start-thinking-animation ()
  "Start the thinking animation."
  (when tiqsi-claude-repl--thinking-timer
    (cancel-timer tiqsi-claude-repl--thinking-timer))
  (setq tiqsi-claude-repl--thinking-animation-state 0)
  (setq-local tiqsi-claude-repl--thinking-timer
        (run-with-timer 0.2 0.2 #'tiqsi-claude-repl--update-thinking-animation)))

(defun tiqsi-claude-repl--stop-thinking-animation ()
  "Stop the thinking animation."
  (when tiqsi-claude-repl--thinking-timer
    (cancel-timer tiqsi-claude-repl--thinking-timer)
    (setq-local tiqsi-claude-repl--thinking-timer nil)))

(defun tiqsi-claude-repl--update-thinking-animation ()
  "Update the thinking animation frame."
  (when (and (eq major-mode 'tiqsi-claude-repl-mode)
             (buffer-live-p (current-buffer)))
    (save-excursion
      (goto-char (point-max))
      (when (search-backward "⏳ Thinking" nil t)
        (let ((inhibit-read-only t)
              (start (match-beginning 0))
              (frame (nth (mod tiqsi-claude-repl--thinking-animation-state
                              (length tiqsi-claude-repl--thinking-frames))
                         tiqsi-claude-repl--thinking-frames)))
          (end-of-line)
          (delete-region start (point))
          (insert (tiqsi-claude-repl--colorize frame 'tiqsi-claude-repl-thinking))
          ;; Add elapsed time
          (when tiqsi-claude-repl--request-start-time
            (insert (format " [%.1fs]" 
                           (float-time (time-since tiqsi-claude-repl--request-start-time)))))
          (setq tiqsi-claude-repl--thinking-animation-state
                (1+ tiqsi-claude-repl--thinking-animation-state)))))))

;;; Additional utility functions

(defun tiqsi-claude-repl--ensure-prompt ()
  "Ensure there's a prompt at the end of the buffer."
  (save-excursion
    (goto-char (point-max))
    ;; Check if we already have a prompt
    (beginning-of-line)
    (unless (looking-at "^λ ")
      ;; No prompt found, add one
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (tiqsi-claude-repl--format-prompt))))
  ;; Move point after the prompt
  (goto-char (point-max)))

(defun tiqsi-claude-repl--log-operation (operation &optional details)
  "Log an OPERATION with optional DETAILS."
  (declare-function tiqsi-claude-repl--get-or-create-buffer "tiqsi-claude-repl-core")
  (when (get-buffer (tiqsi-claude-repl--get-or-create-buffer))
    (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
      (save-excursion
        (goto-char (point-max))
        (beginning-of-line)
        (when (looking-at "^λ ")
          (forward-line -1))
        (end-of-line)
        (insert "\n" (tiqsi-claude-repl--format-timestamp) " "
          (tiqsi-claude-repl--format-status operation details) "\n")))))

;;; Code block handling

(defun tiqsi-claude-repl--insert-code-block (lang code)
  "Insert a formatted code block with LANG and CODE."
  (let ((inhibit-read-only t)
        (block-start (point)))
    ;; Insert language indicator
    (when (> (length lang) 0)
      (insert (tiqsi-claude-repl--colorize (format "▶ %s\n" lang) 'tiqsi-claude-repl-code-lang)))
    
    ;; Insert code with syntax highlighting if available
    (if (and tiqsi-claude-repl-highlight-code
             (> (length lang) 0)
             (fboundp 'intern)
             (intern-soft (concat lang "-mode")))
        (let ((mode-func (intern (concat lang "-mode"))))
          (with-temp-buffer
            (insert code)
            (delay-mode-hooks (funcall mode-func))
            (font-lock-ensure)
            (let ((highlighted-code (buffer-string)))
              (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
                (insert highlighted-code)))))
      ;; Fallback: insert without highlighting
      (insert code))
    
    ;; Apply background
    (put-text-property block-start (point) 'face 'tiqsi-claude-repl-code-block)
    (unless (bolp) (insert "\n"))))

(defun tiqsi-claude-repl--apply-markdown-formatting (start end)
  "Apply markdown formatting between START and END."
  (save-excursion
    (goto-char start)
    ;; Bold text
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" end t)
      (replace-match (propertize (match-string 1) 'face 'tiqsi-claude-repl-markdown-bold) t t))
    
    (goto-char start)
    ;; Italic text
    (while (re-search-forward "\\*\\([^*]+\\)\\*" end t)
      (replace-match (propertize (match-string 1) 'face 'tiqsi-claude-repl-markdown-italic) t t))
    
    (goto-char start)
    ;; Headers
    (while (re-search-forward "^\\(#+\\) \\(.+\\)$" end t)
      (replace-match (propertize (match-string 2) 'face 'tiqsi-claude-repl-markdown-header) t t))))

;; ---------------------------------------------------------------------------
;; Context file management (ported from monolithic tiqsi-claude-repl.el)
;; ---------------------------------------------------------------------------

(defvar tiqsi-claude-repl-context-files nil
  "List of files to include as context for Claude.")

(defun tiqsi-claude-repl-add-context-file (file)
  "Add FILE to the context for Claude."
  (interactive "fAdd file to context: ")
  (add-to-list 'tiqsi-claude-repl-context-files file)
  (tiqsi-claude-repl--log-operation "Context Added"
    (format "File: %s" (file-name-nondirectory file)))
  (message "%s" (tiqsi-claude-repl--format-status "Success"
                  (format "Added %s to context" file))))

(defun tiqsi-claude-repl-clear-context-files ()
  "Clear all context files."
  (interactive)
  (let ((count (length tiqsi-claude-repl-context-files)))
    (setq tiqsi-claude-repl-context-files nil)
    (tiqsi-claude-repl--log-operation "Context Cleared"
      (format "%d files removed" count))
    (message "%s" (tiqsi-claude-repl--format-status "Success" "Context files cleared"))))

;; ---------------------------------------------------------------------------
;; History browsing and search
;; ---------------------------------------------------------------------------

(defun tiqsi-claude-repl--generate-history-filename ()
  "Generate a unique filename for saving conversation history."
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (timestamp (format-time-string "%Y%m%d-%H%M%S")))
    (format "%s-%s.claude" project-name timestamp)))

(defun tiqsi-claude-repl--list-history-files ()
  "List all history files, sorted by date (newest first)."
  (when (file-exists-p tiqsi-claude-repl-history-directory)
    (sort (directory-files tiqsi-claude-repl-history-directory t "\\.claude$")
      'file-newer-than-file-p)))

;;;###autoload
(defun tiqsi-claude-repl-browse-history ()
  "Browse Claude conversation history."
  (interactive)
  (let ((history-files (tiqsi-claude-repl--list-history-files)))
    (if history-files
      (let* ((files-info (mapcar (lambda (f)
                                   (cons (format "%s - %s"
                                           (file-name-nondirectory f)
                                           (format-time-string "%Y-%m-%d %H:%M"
                                             (nth 5 (file-attributes f))))
                                     f))
                           history-files))
             (selected (completing-read "Select conversation: " files-info nil t))
             (file (cdr (assoc selected files-info))))
        (when file
          (find-file-read-only file)
          (tiqsi-claude-repl-history-mode)))
      (message "No conversation history found"))))

;;;###autoload
(defun tiqsi-claude-repl-search-history (query)
  "Search through Claude conversation history for QUERY."
  (interactive "sSearch history for: ")
  (let ((history-files (tiqsi-claude-repl--list-history-files))
        (results '()))
    (dolist (file history-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (search-forward query nil t)
          (push (cons file (count-matches query (point-min) (point-max))) results))))
    (if results
      (let* ((sorted-results (sort results (lambda (a b) (> (cdr a) (cdr b)))))
             (choices (mapcar (lambda (r)
                                (format "%s (%d matches)"
                                  (file-name-nondirectory (car r))
                                  (cdr r)))
                        sorted-results))
             (selected (completing-read "Select result: " choices nil t))
             (index (cl-position selected choices :test 'string=))
             (file (car (nth index sorted-results))))
        (find-file-read-only file)
        (goto-char (point-min))
        (search-forward query nil t)
        (tiqsi-claude-repl-history-mode))
      (message "No results found for: %s" query))))

;; History viewing mode
(define-derived-mode tiqsi-claude-repl-history-mode special-mode "Claude-History"
  "Major mode for viewing Claude conversation history."
  (setq-local buffer-read-only t)
  (local-set-key (kbd "q") 'quit-window)
  (local-set-key (kbd "n") 'tiqsi-claude-repl-history-next)
  (local-set-key (kbd "p") 'tiqsi-claude-repl-history-previous))

(defun tiqsi-claude-repl-history-next ()
  "Go to next conversation in history."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (history-files (tiqsi-claude-repl--list-history-files))
         (current-index (cl-position current-file history-files :test 'string=)))
    (when (and current-index (< (1+ current-index) (length history-files)))
      (find-file-read-only (nth (1+ current-index) history-files))
      (tiqsi-claude-repl-history-mode))))

(defun tiqsi-claude-repl-history-previous ()
  "Go to previous conversation in history."
  (interactive)
  (let* ((current-file (buffer-file-name))
         (history-files (tiqsi-claude-repl--list-history-files))
         (current-index (cl-position current-file history-files :test 'string=)))
    (when (and current-index (> current-index 0))
      (find-file-read-only (nth (1- current-index) history-files))
      (tiqsi-claude-repl-history-mode))))

;; ---------------------------------------------------------------------------
;; Session management
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-claude-repl-toggle ()
  "Toggle Claude REPL window visibility."
  (interactive)
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (buffer-name (format "*Claude REPL (%s)*"
                        (file-name-nondirectory (directory-file-name project-root)))))
    (if-let* ((window (get-buffer-window buffer-name)))
      (delete-window window)
      (tiqsi-claude-repl-start))))

;;;###autoload
(defun tiqsi-claude-repl-new-session ()
  "Start a new Claude REPL session (kills existing one if any)."
  (interactive)
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (buffer-name (format "*Claude REPL (%s)*"
                        (file-name-nondirectory (directory-file-name project-root)))))
    (when-let* ((buffer (get-buffer buffer-name)))
      (with-current-buffer buffer
        (when (and tiqsi-claude-repl--current-process
                (process-live-p tiqsi-claude-repl--current-process))
          (delete-process tiqsi-claude-repl--current-process)))
      (kill-buffer buffer)))
  (tiqsi-claude-repl-start)
  (message "Started new Claude REPL session"))

;;;###autoload
(defun tiqsi-claude-repl-recover-session ()
  "Recover session state after a disconnection."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (unless tiqsi-claude-repl--session-id
      (setq-local tiqsi-claude-repl--session-id
        (tiqsi-claude-repl--generate-session-id)))
    (unless tiqsi-claude-repl--session-start-time
      (setq-local tiqsi-claude-repl--session-start-time (current-time)))
    (setq-local tiqsi-claude-repl--last-interaction-time (current-time))
    (tiqsi-claude-repl--ensure-prompt)
    (goto-char (point-max))
    (tiqsi-claude-repl--log-operation "Session Recovered"
      (format "Session %s restored" tiqsi-claude-repl--session-id))
    (message "%s" (tiqsi-claude-repl--format-status "Success" "Session recovered"))))

;; ---------------------------------------------------------------------------
;; Toggle commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-claude-repl-toggle-thinking ()
  "Toggle showing Claude's thinking process."
  (interactive)
  (setq tiqsi-claude-repl-show-thinking (not tiqsi-claude-repl-show-thinking))
  (message "Claude thinking process: %s"
    (if tiqsi-claude-repl-show-thinking "enabled" "disabled")))

;;;###autoload
(defun tiqsi-claude-repl-toggle-syntax-highlighting ()
  "Toggle syntax highlighting in Claude responses."
  (interactive)
  (setq tiqsi-claude-repl-highlight-code (not tiqsi-claude-repl-highlight-code))
  (message "Claude syntax highlighting: %s"
    (if tiqsi-claude-repl-highlight-code "enabled" "disabled")))

;;;###autoload
(defun tiqsi-claude-repl-toggle-line-wrapping ()
  "Toggle automatic line wrapping in Claude responses."
  (interactive)
  (setq tiqsi-claude-repl-auto-wrap (not tiqsi-claude-repl-auto-wrap))
  (message "Claude line wrapping: %s (at column %d)"
    (if tiqsi-claude-repl-auto-wrap "enabled" "disabled")
    tiqsi-claude-repl-wrap-column))

;;;###autoload
(defun tiqsi-claude-repl-set-wrap-column (column)
  "Set the COLUMN at which to wrap long lines."
  (interactive "nWrap at column: ")
  (setq tiqsi-claude-repl-wrap-column column)
  (message "Claude line wrapping set to column %d" column))

;; ---------------------------------------------------------------------------
;; Status and summary
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-claude-repl-show-session-summary ()
  "Show a summary of the current Claude session."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (let* ((total-chars (buffer-size))
           (prompts (count-matches "^λ " (point-min) (point-max)))
           (responses (/ prompts 2))
           (runtime (if tiqsi-claude-repl--session-start-time
                      (format-seconds "%h hours, %m minutes, %s seconds"
                        (float-time (time-subtract (current-time)
                          tiqsi-claude-repl--session-start-time)))
                      "N/A"))
           (idle-time (if tiqsi-claude-repl--last-interaction-time
                        (format-seconds "%m minutes, %s seconds"
                          (float-time (time-subtract (current-time)
                            tiqsi-claude-repl--last-interaction-time)))
                        "N/A")))
      (message "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
        (tiqsi-claude-repl--format-status "Session Summary" "")
        (format "  Internal Session ID: %s" (or tiqsi-claude-repl--session-id "unknown"))
        (format "  Claude Session ID: %s" (or tiqsi-claude-repl--claude-session-id "not yet initialized"))
        (format "  Total characters: %d" total-chars)
        (format "  Total prompts: %d" prompts)
        (format "  Messages exchanged: %d" tiqsi-claude-repl--message-count)
        (format "  Estimated responses: %d" responses)
        (format "  Session runtime: %s" runtime)
        (format "  Idle for: %s" idle-time)))))

;;;###autoload
(defun tiqsi-claude-repl-show-status ()
  "Show the current status of Claude REPL."
  (interactive)
  (let ((status-items
          (list
            (cons "Claude CLI" (if (tiqsi-claude-repl--executable-available-p)
                                 (tiqsi-claude-repl--colorize "Available" 'tiqsi-claude-repl-success)
                                 (tiqsi-claude-repl--colorize "Not found" 'tiqsi-claude-repl-error)))
            (cons "Active sessions" (format "%d" (length (cl-remove-if-not
                                                           (lambda (buf)
                                                             (with-current-buffer buf
                                                               (eq major-mode 'tiqsi-claude-repl-mode)))
                                                           (buffer-list)))))
            (cons "Thinking mode" (if tiqsi-claude-repl-show-thinking
                                    (tiqsi-claude-repl--colorize "Enabled" 'tiqsi-claude-repl-success)
                                    (tiqsi-claude-repl--colorize "Disabled" 'tiqsi-claude-repl-warning)))
            (cons "Syntax highlighting" (if tiqsi-claude-repl-highlight-code
                                          (tiqsi-claude-repl--colorize "Enabled" 'tiqsi-claude-repl-success)
                                          (tiqsi-claude-repl--colorize "Disabled" 'tiqsi-claude-repl-warning)))
            (cons "History saving" (if tiqsi-claude-repl-save-history
                                     (tiqsi-claude-repl--colorize "Enabled" 'tiqsi-claude-repl-success)
                                     (tiqsi-claude-repl--colorize "Disabled" 'tiqsi-claude-repl-warning)))
            (cons "Context files" (format "%d" (length tiqsi-claude-repl-context-files))))))
    (message "%s\n%s"
      (tiqsi-claude-repl--format-status "Claude REPL Status" "")
      (mapconcat (lambda (item)
                   (format "  %s: %s" (car item) (cdr item)))
        status-items "\n"))))

;; ---------------------------------------------------------------------------
;; Buffer reformatting
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-claude-repl-reformat-buffer ()
  "Re-apply formatting to the entire Claude REPL buffer."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (save-excursion
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max) '(face nil invisible nil))
        (goto-char (point-min))
        (while (re-search-forward "^> .+\n\n" nil t)
          (let ((response-start (point))
                (response-end (if (re-search-forward "^> " nil t)
                                (progn (beginning-of-line) (point))
                                (point-max))))
            (when (< response-start response-end)
              (tiqsi-claude-repl--apply-markdown-formatting response-start response-end))
            (goto-char response-end)))))
    (message "Claude REPL buffer reformatted")))

;;;###autoload
(defun tiqsi-claude-repl-test-highlighting ()
  "Test syntax highlighting with a sample response."
  (interactive)
  (tiqsi-claude-repl-start)
  (tiqsi-claude-repl--log-operation "Test Mode" "Running syntax highlighting test")
  (let ((test-input "Show me a Python function with markdown formatting"))
    (with-current-buffer (tiqsi-claude-repl--get-or-create-buffer)
      (goto-char (point-max))
      (insert test-input)
      (tiqsi-claude-repl-send-input))))

(provide 'tiqsi-claude-repl-features)

;;; tiqsi-claude-repl-features.el ends here