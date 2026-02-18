;;; tiqsi-claude-repl-opencode.el --- OpenCode backend for Claude REPL -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; OpenCode (https://opencode.ai) backend integration for the Tiqsi Claude REPL
;; system. This module provides:
;;
;;   - OpenCode process management (launch, send, cancel)
;;   - JSON streaming parser for OpenCode's event format
;;   - Session management (continue, fork, export, stats)
;;   - Model selection and agent support
;;   - File attachment
;;   - TLS bypass for corporate environments
;;   - Dedicated buffer and commands alongside the Claude REPL
;;
;; OpenCode JSON event types:
;;   step_start  — marks the beginning of a response step
;;   text        — contains the actual response text
;;   tool_use    — tool invocation (file edits, shell commands, etc.)
;;   tool_result — result of a tool invocation
;;   step_finish — marks the end of a step with cost/token info
;;
;; Launch requirements:
;;   export NODE_TLS_REJECT_UNAUTHORIZED=0 && opencode
;;
;; Usage:
;;   M-x tiqsi-opencode-start       Start an OpenCode REPL
;;   M-x tiqsi-opencode-ask         Ask a question
;;   M-x tiqsi-opencode-send-region Send region
;;   M-x tiqsi-opencode-switch      Switch backend (Claude <-> OpenCode)

;;; Code:

(require 'cl-lib)
(require 'json)

;; Declare functions from other REPL modules
(declare-function tiqsi-claude-repl--colorize "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--make-separator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-timestamp "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-status "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-prompt "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--insert-header "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--thinking-indicator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--get-project-root "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--get-language-mode "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--safe-mode-available-p "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl--apply-markdown-formatting "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl--log-operation "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl--build-smart-context "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl-mode "tiqsi-claude-repl-core")

;; ---------------------------------------------------------------------------
;; Custom variables
;; ---------------------------------------------------------------------------

(defgroup tiqsi-opencode nil
  "OpenCode backend for Tiqsi REPL."
  :group 'tiqsi-claude-repl
  :prefix "tiqsi-opencode-")

(defcustom tiqsi-opencode-program "opencode"
  "Path to the OpenCode CLI executable."
  :type 'string
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-model nil
  "Model to use for OpenCode sessions.
Format: provider/model (e.g. \"anthropic/claude-sonnet-4-20250514\").
Set to nil to use the default model."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Provider/model"))
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-agent nil
  "Agent to use for OpenCode sessions.
Set to nil to use the default agent."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Agent name"))
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-variant nil
  "Model variant for reasoning effort.
Provider-specific: e.g. \"high\", \"max\", \"minimal\"."
  :type '(choice (const :tag "Default" nil)
                 (string :tag "Variant"))
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-show-thinking nil
  "Whether to show thinking blocks in OpenCode output."
  :type 'boolean
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-show-tool-use t
  "Whether to display tool use events (file edits, commands, etc.)."
  :type 'boolean
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-show-cost t
  "Whether to show cost and token usage after each response."
  :type 'boolean
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-tls-bypass t
  "Whether to set NODE_TLS_REJECT_UNAUTHORIZED=0.
Required for corporate environments with custom CA certificates."
  :type 'boolean
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-auto-approve t
  "Whether to auto-approve tool permissions in the REPL subprocess.
When non-nil (the default), injects OPENCODE_PERMISSION={\"*\":\"allow\"}
into the process environment so that OpenCode does not stall on
permission prompts (edit, bash, etc.) that cannot be answered from
a non-interactive subprocess.

Set to nil to keep the default permission config (mostly \"ask\"),
which will cause tool-use requests to be silently rejected since
there is no TTY for approval."
  :type 'boolean
  :group 'tiqsi-opencode)

;; ---------------------------------------------------------------------------
;; Faces
;; ---------------------------------------------------------------------------

(defface tiqsi-opencode-tool-use
  '((t :foreground "#88c0d0" :weight bold))
  "Face for OpenCode tool use events."
  :group 'tiqsi-opencode)

(defface tiqsi-opencode-tool-result
  '((t :foreground "#a3be8c"))
  "Face for OpenCode tool results."
  :group 'tiqsi-opencode)

(defface tiqsi-opencode-cost
  '((t :foreground "#4c566a" :slant italic))
  "Face for cost and token information."
  :group 'tiqsi-opencode)

(defface tiqsi-opencode-step-marker
  '((t :foreground "#5e81ac" :weight bold))
  "Face for step start/finish markers."
  :group 'tiqsi-opencode)

;; ---------------------------------------------------------------------------
;; State variables
;; ---------------------------------------------------------------------------

(defvar-local tiqsi-opencode--current-process nil
  "The current OpenCode process.")

(defvar-local tiqsi-opencode--force-killed nil
  "Non-nil when the current process was force-killed for replacement.
The sentinel checks this to avoid inserting a spurious completion message.")

(defvar-local tiqsi-opencode--session-id nil
  "Current OpenCode session ID (captured from JSON events).")

(defvar-local tiqsi-opencode--json-buffer ""
  "Buffer for accumulating incomplete JSON lines from OpenCode.")

(defvar-local tiqsi-opencode--request-start-time nil
  "Time when the current request started.")

(defvar-local tiqsi-opencode--output-start nil
  "Marker for where OpenCode's current output starts.")

(defvar-local tiqsi-opencode--message-count 0
  "Number of messages exchanged in current session.")

(defvar-local tiqsi-opencode--total-cost 0.0
  "Total cost accumulated in this session.")

(defvar-local tiqsi-opencode--total-tokens 0
  "Total tokens used in this session.")

(defvar-local tiqsi-opencode--attached-files nil
  "List of files attached to the next message.")

(defvar-local tiqsi-opencode--attach-url nil
  "URL of a running OpenCode server to attach to.
When set, each `run' invocation passes `--attach URL'.")

;; Global active backend selector
(defvar tiqsi-repl-backend 'claude
  "Active REPL backend. Either `claude' or `opencode'.")

;; ---------------------------------------------------------------------------
;; Utility functions
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode--executable-available-p ()
  "Check if OpenCode CLI is available on PATH."
  (executable-find tiqsi-opencode-program))

(defun tiqsi-opencode--build-env ()
  "Build process environment for OpenCode.
Includes TLS bypass if `tiqsi-opencode-tls-bypass' is set, and
auto-approve permissions if `tiqsi-opencode-auto-approve' is set.

The permission override is critical for non-interactive usage:
OpenCode's `run --format json' mode has no TTY, so any tool whose
permission is set to \"ask\" (edit, bash, webfetch) will be silently
rejected — the user never sees a prompt and OpenCode reports
\"you rejected the permission\".  Setting OPENCODE_PERMISSION to
allow-all scopes this to the Emacs subprocess only."
  (let ((env (copy-sequence process-environment)))
    (when tiqsi-opencode-tls-bypass
      (push "NODE_TLS_REJECT_UNAUTHORIZED=0" env))
    (when tiqsi-opencode-auto-approve
      (push "OPENCODE_PERMISSION={\"*\":\"allow\"}" env))
    env))

(defun tiqsi-opencode--buffer-name ()
  "Generate the OpenCode REPL buffer name for the current project."
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (project-name (file-name-nondirectory
                        (directory-file-name project-root))))
    (format "*OpenCode REPL (%s)*" project-name)))

(defun tiqsi-opencode--get-or-create-buffer ()
  "Get or create the OpenCode REPL buffer."
  (let ((buffer-name (tiqsi-opencode--buffer-name)))
    (or (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (tiqsi-claude-repl-mode)
          (current-buffer)))))

;; ---------------------------------------------------------------------------
;; JSON event processing
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode--process-json-line (line)
  "Process a single JSON LINE from OpenCode's stream output."
  (condition-case err
      (let* ((json-obj (json-parse-string line))
             (type (gethash "type" json-obj))
             (inhibit-read-only t))
        (cond
         ;; Text event — the actual response content
         ((equal type "text")
          (let* ((part (gethash "part" json-obj))
                 (text (gethash "text" part "")))
            ;; Capture session ID from first event
            (unless tiqsi-opencode--session-id
              (let ((sid (gethash "sessionID" json-obj)))
                (when sid (setq-local tiqsi-opencode--session-id sid))))
            (when (> (length text) 0)
              (save-excursion
                (goto-char (point-max))
                (insert text)))))

         ;; Step start — beginning of a response step
         ((equal type "step_start")
          (unless tiqsi-opencode--session-id
            (let ((sid (gethash "sessionID" json-obj)))
              (when sid (setq-local tiqsi-opencode--session-id sid))))
          ;; Clear thinking indicator and set output start
          (save-excursion
            (goto-char (point-max))
            (when (re-search-backward "⏳ Thinking\\.\\.\\." nil t)
              (let ((elapsed (if tiqsi-opencode--request-start-time
                               (format " [%.1fs]"
                                       (float-time (time-since
                                                    tiqsi-opencode--request-start-time)))
                               "")))
                (replace-match (format "🧠 Processing%s" elapsed))))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (setq-local tiqsi-opencode--output-start (point-marker))))

         ;; Step finish — end of response with cost info
         ((equal type "step_finish")
          (let* ((part (gethash "part" json-obj))
                 (cost (gethash "cost" part 0))
                 (tokens-obj (gethash "tokens" part))
                 (total-tokens (if tokens-obj (gethash "total" tokens-obj 0) 0))
                 (input-tokens (if tokens-obj (gethash "input" tokens-obj 0) 0))
                 (output-tokens (if tokens-obj (gethash "output" tokens-obj 0) 0))
                 (cache-obj (if tokens-obj (gethash "cache" tokens-obj) nil))
                 (cache-read (if cache-obj (gethash "read" cache-obj 0) 0)))
            ;; Accumulate totals
            (cl-incf tiqsi-opencode--total-cost cost)
            (cl-incf tiqsi-opencode--total-tokens total-tokens)
            ;; Display cost info
            (when tiqsi-opencode-show-cost
              (save-excursion
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert "\n"
                        (tiqsi-claude-repl--colorize
                         (format "── $%.4f │ %d tok (in:%d out:%d cache:%d) ──"
                                 cost total-tokens input-tokens output-tokens cache-read)
                         'tiqsi-opencode-cost)
                        "\n")))))

         ;; Tool use — file edits, shell commands, etc.
         ((equal type "tool_use")
          (when tiqsi-opencode-show-tool-use
            (let* ((part (gethash "part" json-obj))
                   (tool-name (or (gethash "name" part)
                                  (gethash "tool" part)
                                  "unknown"))
                   (tool-input (gethash "input" part)))
              (save-excursion
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (insert (tiqsi-claude-repl--colorize
                         (format "🔧 %s" tool-name) 'tiqsi-opencode-tool-use))
                (when tool-input
                  (let ((input-str (if (hash-table-p tool-input)
                                       (let ((repr (json-serialize tool-input)))
                                         (substring repr 0 (min 200 (length repr))))
                                     (format "%s" tool-input))))
                    (insert " " (tiqsi-claude-repl--colorize
                                 input-str 'tiqsi-opencode-cost))))
                (insert "\n")))))

         ;; Tool result
         ((equal type "tool_result")
          (when tiqsi-opencode-show-tool-use
            (let* ((part (gethash "part" json-obj))
                   (result (or (gethash "result" part)
                               (gethash "output" part))))
              (when result
                (save-excursion
                  (goto-char (point-max))
                  (let ((result-str (if (stringp result)
                                        (substring result 0 (min 300 (length result)))
                                      (format "%s" result))))
                    (insert (tiqsi-claude-repl--colorize
                             (format "  ↳ %s" result-str) 'tiqsi-opencode-tool-result)
                            "\n")))))))

         ;; Thinking event
         ((equal type "thinking")
          (when tiqsi-opencode-show-thinking
            (let ((content (or (gethash "content" (gethash "part" json-obj nil))
                               (gethash "text" (gethash "part" json-obj nil)))))
              (when content
                (save-excursion
                  (goto-char (point-max))
                  (insert (tiqsi-claude-repl--colorize "🤔 " 'tiqsi-claude-repl-thinking)
                          (tiqsi-claude-repl--colorize
                           (substring content 0 (min 200 (length content)))
                           'tiqsi-claude-repl-info)
                          "\n"))))))

         ;; Ignore other event types silently
         (t nil)))
    (json-parse-error
     (message "OpenCode JSON parse error: %s" (error-message-string err)))))

(defun tiqsi-opencode--process-filter (process output)
  "Process filter for OpenCode JSON stream OUTPUT from PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (setq tiqsi-opencode--json-buffer
              (concat tiqsi-opencode--json-buffer output))
        ;; Process complete newline-terminated lines; keep the last
        ;; (potentially incomplete) fragment for next call.
        (let ((lines (split-string tiqsi-opencode--json-buffer "\n")))
          ;; The last element of split-string is everything after the
          ;; final \n — either "" (if output ended with \n) or a partial
          ;; line still accumulating.
          (setq tiqsi-opencode--json-buffer (car (last lines)))
          ;; Process all complete lines (everything except the last)
          (setq lines (butlast lines))
          (dolist (line lines)
            (let ((trimmed (string-trim line)))
              (when (and (> (length trimmed) 0)
                         (string-prefix-p "{" trimmed)
                         (string-suffix-p "}" trimmed))
                (tiqsi-opencode--process-json-line trimmed)))))))))

(defun tiqsi-opencode--process-sentinel (process event)
  "Process sentinel for OpenCode PROCESS completion EVENT.
When `tiqsi-opencode--force-killed' is set, skips the completion
message to avoid garbled output when a new request replaces the old."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t))
        (when (string-match-p "finished\\|exited" event)
          (if tiqsi-opencode--force-killed
              ;; Force-killed: just clean up state, no "Completed" output
              (progn
                (setq-local tiqsi-opencode--force-killed nil)
                (setq-local tiqsi-opencode--current-process nil)
                (setq-local tiqsi-opencode--output-start nil)
                (setq-local tiqsi-opencode--request-start-time nil)
                (setq-local tiqsi-opencode--json-buffer "")
                (setq-local tiqsi-opencode--attached-files nil))
            ;; Normal completion
            ;; Apply markdown formatting to the response
            (when tiqsi-opencode--output-start
              (tiqsi-claude-repl--apply-markdown-formatting
               tiqsi-opencode--output-start (point-max)))
            ;; Show completion time
            (when tiqsi-opencode--request-start-time
              (let ((elapsed (float-time (time-since tiqsi-opencode--request-start-time))))
                (goto-char (point-max))
                (insert "\n" (tiqsi-claude-repl--colorize
                              (format "✅ Completed in %.1fs" elapsed)
                              'tiqsi-claude-repl-success) "\n")))
            ;; Clear state
            (setq-local tiqsi-opencode--current-process nil)
            (setq-local tiqsi-opencode--output-start nil)
            (setq-local tiqsi-opencode--request-start-time nil)
            (setq-local tiqsi-opencode--json-buffer "")
            (setq-local tiqsi-opencode--attached-files nil)
            ;; Add prompt
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n" (tiqsi-claude-repl--format-prompt "oc"))
            (goto-char (point-max))))))))

;; ---------------------------------------------------------------------------
;; Send to OpenCode
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode--send (input)
  "Send INPUT to OpenCode as a non-interactive run command.
Builds the full argument list from current settings, including:
  --session, --model, --agent, --variant, --thinking, --file,
  --fork, --dir.
When --file is used, `--' separates flags from the positional
message to prevent yargs from consuming the message as a file."
  ;; Kill any existing process — set flag so sentinel skips "Completed" message
  (when (and tiqsi-opencode--current-process
             (process-live-p tiqsi-opencode--current-process))
    (setq-local tiqsi-opencode--force-killed t)
    (delete-process tiqsi-opencode--current-process))

  (let* (;; Build arguments
         (args (list "run" "--format" "json"))
         ;; Continue session if we have one
         (args (if tiqsi-opencode--session-id
                   (append args (list "--session" tiqsi-opencode--session-id))
                 args))
         ;; Model
         (args (if tiqsi-opencode-model
                   (append args (list "--model" tiqsi-opencode-model))
                 args))
         ;; Agent
         (args (if tiqsi-opencode-agent
                   (append args (list "--agent" tiqsi-opencode-agent))
                 args))
         ;; Variant (reasoning effort)
         (args (if tiqsi-opencode-variant
                   (append args (list "--variant" tiqsi-opencode-variant))
                 args))
         ;; Thinking
         (args (if tiqsi-opencode-show-thinking
                   (append args (list "--thinking"))
                 args))
         ;; Attach to running server
         (args (if tiqsi-opencode--attach-url
                   (append args (list "--attach" tiqsi-opencode--attach-url))
                 args))
         ;; Attached files
         (has-files tiqsi-opencode--attached-files)
         (args (if has-files
                   (cl-reduce (lambda (acc f) (append acc (list "--file" f)))
                              tiqsi-opencode--attached-files
                              :initial-value args)
                 args))
         ;; When --file is used, insert "--" separator so the message
         ;; positional is not consumed as another --file value.
         (args (if has-files
                   (append args (list "--" input))
                 (append args (list input))))
         ;; Build environment with TLS bypass
         (process-environment (tiqsi-opencode--build-env))
         ;; Start the process
         (process (apply 'start-process
                         "opencode-repl"
                         (current-buffer)
                         tiqsi-opencode-program
                         args)))
    ;; Configure process
    (setq-local tiqsi-opencode--current-process process)
    (setq-local tiqsi-opencode--request-start-time (current-time))
    (cl-incf tiqsi-opencode--message-count)
    (set-process-filter process 'tiqsi-opencode--process-filter)
    (set-process-sentinel process 'tiqsi-opencode--process-sentinel)
    (set-process-query-on-exit-flag process nil)))

;; ---------------------------------------------------------------------------
;; Interactive commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-start ()
  "Start an OpenCode REPL session."
  (interactive)
  (unless (tiqsi-opencode--executable-available-p)
    (error "OpenCode CLI not found. Install with: curl -fsSL https://opencode.ai/install | bash"))
  (let ((buf (tiqsi-opencode--get-or-create-buffer)))
    (with-current-buffer buf
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          ;; Insert header
          (insert (tiqsi-claude-repl--colorize
                   "OpenCode REPL - AI Coding Agent" 'tiqsi-claude-repl-info) "\n")
          (insert (tiqsi-claude-repl--make-separator) "\n")
          (insert (concat
                   (tiqsi-claude-repl--colorize "Backend: " 'tiqsi-claude-repl-status)
                   (tiqsi-claude-repl--colorize "OpenCode" 'tiqsi-claude-repl-success)
                   (when tiqsi-opencode-model
                     (concat " │ Model: "
                             (tiqsi-claude-repl--colorize
                              tiqsi-opencode-model 'tiqsi-claude-repl-info)))
                   (when tiqsi-opencode-agent
                     (concat " │ Agent: "
                             (tiqsi-claude-repl--colorize
                              tiqsi-opencode-agent 'tiqsi-claude-repl-info)))
                    (when tiqsi-opencode-tls-bypass
                      (concat " │ "
                              (tiqsi-claude-repl--colorize
                               "TLS bypass" 'tiqsi-claude-repl-warning)))
                    (when tiqsi-opencode-auto-approve
                      (concat " │ "
                              (tiqsi-claude-repl--colorize
                               "auto-approve" 'tiqsi-claude-repl-success))))
                  "\n")
          (insert (tiqsi-claude-repl--make-separator) "\n\n")
          (insert (tiqsi-claude-repl--format-prompt "oc"))))
      ;; Set backend
      (setq tiqsi-repl-backend 'opencode))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))

;;;###autoload
(defun tiqsi-opencode-send-input ()
  "Send current input to OpenCode.
Works identically to `tiqsi-claude-repl-send-input' — reads from the
prompt line, shows thinking indicator, and dispatches."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (let ((input (tiqsi-opencode--get-input)))
      (when (and input (> (length (string-trim input)) 0))
        (tiqsi-opencode--prepare-for-response)
        (run-at-time 0.01 nil #'tiqsi-opencode--send input)))))

(defun tiqsi-opencode--get-input ()
  "Get current input from the prompt line (mirrors Claude's `--get-input')."
  (save-excursion
    (goto-char (point-max))
    (let ((end (point)))
      (beginning-of-line)
      (if (re-search-forward "λ " nil t)
          (string-trim (buffer-substring-no-properties (point) end))
        ""))))

(defun tiqsi-opencode--prepare-for-response ()
  "Prepare buffer for OpenCode response (mirrors Claude's `--prepare-for-response')."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    ;; Prefix the input line with "You: "
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\(oc \\)?λ ")
        (goto-char (match-end 0))
        (insert (tiqsi-claude-repl--colorize "You: " 'tiqsi-claude-repl-prompt))))
    (insert "\n")
    ;; Thinking indicator
    (insert (tiqsi-claude-repl--thinking-indicator) "\n")
    (setq-local tiqsi-opencode--request-start-time (current-time))
    (sit-for 0)))

;;;###autoload
(defun tiqsi-opencode-ask (question)
  "Ask QUESTION via OpenCode.
Ensures the REPL buffer exists, writes the question to the prompt line,
then sends — exactly like `tiqsi-claude-repl-ask-question'."
  (interactive "sAsk OpenCode: ")
  (let ((buf (tiqsi-opencode--get-or-create-buffer)))
    ;; Make sure the REPL window is visible
    (unless (get-buffer-window buf)
      (tiqsi-opencode-start))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        ;; Write the question onto the current prompt line
        (insert (tiqsi-claude-repl--colorize question 'tiqsi-claude-repl-input))
        ;; Now send it
        (tiqsi-opencode-send-input)))))

;;;###autoload
(defun tiqsi-opencode-send-region (start end)
  "Send region from START to END to OpenCode."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (tiqsi-opencode-ask text)))

;;;###autoload
(defun tiqsi-opencode-send-buffer ()
  "Send entire buffer to OpenCode."
  (interactive)
  (tiqsi-opencode-send-region (point-min) (point-max)))

;;;###autoload
(defun tiqsi-opencode-send-function ()
  "Send current function to OpenCode."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((start (point)))
      (end-of-defun)
      (tiqsi-opencode-send-region start (point)))))

;;;###autoload
(defun tiqsi-opencode-send-paragraph ()
  "Send current paragraph to OpenCode."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (tiqsi-opencode-send-region start end))))

;;;###autoload
(defun tiqsi-opencode-explain-code ()
  "Ask OpenCode to explain code."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (tiqsi-opencode-ask (format "Please explain what this code does:\n\n%s" text)))
    (save-excursion
      (beginning-of-defun)
      (let ((start (point)))
        (end-of-defun)
        (tiqsi-opencode-ask
         (format "Please explain what this code does:\n\n%s"
                 (buffer-substring-no-properties start (point))))))))

;;;###autoload
(defun tiqsi-opencode-optimize-code ()
  "Ask OpenCode to optimize code."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (tiqsi-opencode-ask (format "Please optimize this code:\n\n%s" text)))
    (save-excursion
      (beginning-of-defun)
      (let ((start (point)))
        (end-of-defun)
        (tiqsi-opencode-ask
         (format "Please optimize this code:\n\n%s"
                 (buffer-substring-no-properties start (point))))))))

;;;###autoload
(defun tiqsi-opencode-fix-error ()
  "Ask OpenCode to fix errors in code."
  (interactive)
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (save-excursion
                  (beginning-of-defun)
                  (let ((start (point)))
                    (end-of-defun)
                    (buffer-substring-no-properties start (point)))))))
    (tiqsi-opencode-ask (format "Please fix any errors in this code:\n\n%s" text))))

;;;###autoload
(defun tiqsi-opencode-generate-tests ()
  "Ask OpenCode to generate tests."
  (interactive)
  (if (region-active-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (tiqsi-opencode-ask (format "Please generate tests for this code:\n\n%s" text)))
    (save-excursion
      (beginning-of-defun)
      (let ((start (point)))
        (end-of-defun)
        (tiqsi-opencode-ask
         (format "Please generate tests for this code:\n\n%s"
                 (buffer-substring-no-properties start (point))))))))

;;;###autoload
(defun tiqsi-opencode-cancel ()
  "Cancel the current OpenCode request."
  (interactive)
  (when (and tiqsi-opencode--current-process
             (process-live-p tiqsi-opencode--current-process))
    (delete-process tiqsi-opencode--current-process)
    (setq-local tiqsi-opencode--current-process nil)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n" (tiqsi-claude-repl--colorize "[Request cancelled]"
                                                 'tiqsi-claude-repl-error)
              "\n\n" (tiqsi-claude-repl--format-prompt "oc"))
      (goto-char (point-max)))
    (message "OpenCode request cancelled")))

;;;###autoload
(defun tiqsi-opencode-kill ()
  "Kill the OpenCode REPL buffer."
  (interactive)
  (let ((buf-name (tiqsi-opencode--buffer-name)))
    (when-let* ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (when (and tiqsi-opencode--current-process
                   (process-live-p tiqsi-opencode--current-process))
          (delete-process tiqsi-opencode--current-process)))
      (kill-buffer buf)
      (message "Killed %s" buf-name))))

;;;###autoload
(defun tiqsi-opencode-clear ()
  "Clear the OpenCode REPL buffer."
  (interactive)
  (when (eq major-mode 'tiqsi-claude-repl-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (tiqsi-claude-repl--format-prompt "oc"))
      (setq-local tiqsi-opencode--session-id nil)
      (setq-local tiqsi-opencode--message-count 0)
      (setq-local tiqsi-opencode--total-cost 0.0)
      (setq-local tiqsi-opencode--total-tokens 0))))

;; ---------------------------------------------------------------------------
;; OpenCode-specific features
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-attach-file (file)
  "Attach FILE to the next OpenCode message."
  (interactive "fAttach file: ")
  (push file tiqsi-opencode--attached-files)
  (message "Attached %s (total: %d files)"
           (file-name-nondirectory file)
           (length tiqsi-opencode--attached-files)))

;;;###autoload
(defun tiqsi-opencode-clear-attachments ()
  "Clear all attached files."
  (interactive)
  (let ((count (length tiqsi-opencode--attached-files)))
    (setq-local tiqsi-opencode--attached-files nil)
    (message "Cleared %d attachments" count)))

;;;###autoload
(defun tiqsi-opencode-set-model (model)
  "Set the MODEL for OpenCode (format: provider/model)."
  (interactive "sModel (provider/model): ")
  (setq tiqsi-opencode-model (if (string-empty-p model) nil model))
  (message "OpenCode model: %s" (or tiqsi-opencode-model "default")))

;;;###autoload
(defun tiqsi-opencode-set-agent (agent)
  "Set the AGENT for OpenCode."
  (interactive "sAgent name (empty for default): ")
  (setq tiqsi-opencode-agent (if (string-empty-p agent) nil agent))
  (message "OpenCode agent: %s" (or tiqsi-opencode-agent "default")))

;;;###autoload
(defun tiqsi-opencode-session-stats ()
  "Show session statistics."
  (interactive)
  (message "OpenCode Session: %s\n  Messages: %d │ Cost: $%.4f │ Tokens: %d"
           (or tiqsi-opencode--session-id "not started")
           tiqsi-opencode--message-count
           tiqsi-opencode--total-cost
           tiqsi-opencode--total-tokens))

;;;###autoload
(defun tiqsi-opencode-global-stats ()
  "Show OpenCode global usage statistics."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
     (concat (shell-quote-argument tiqsi-opencode-program) " stats")
     "*OpenCode Stats*")))

;;;###autoload
(defun tiqsi-opencode-list-models ()
  "List available OpenCode models."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
     (concat (shell-quote-argument tiqsi-opencode-program) " models")
     "*OpenCode Models*")))

;;;###autoload
(defun tiqsi-opencode-list-sessions ()
  "List OpenCode sessions."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
     (concat (shell-quote-argument tiqsi-opencode-program) " session list")
     "*OpenCode Sessions*")))

;;;###autoload
(defun tiqsi-opencode-export-session ()
  "Export the current OpenCode session as JSON."
  (interactive)
  (if tiqsi-opencode--session-id
      (let ((process-environment (tiqsi-opencode--build-env)))
        (async-shell-command
         (format "%s export %s"
                 (shell-quote-argument tiqsi-opencode-program)
                 (shell-quote-argument tiqsi-opencode--session-id))
         "*OpenCode Export*"))
    (message "No active session to export")))

;; ---------------------------------------------------------------------------
;; Variant / reasoning effort
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-set-variant (variant)
  "Set the model VARIANT (reasoning effort: high, max, minimal)."
  (interactive "sVariant (high/max/minimal, empty for default): ")
  (setq tiqsi-opencode-variant (if (string-empty-p variant) nil variant))
  (message "OpenCode variant: %s" (or tiqsi-opencode-variant "default")))

;; ---------------------------------------------------------------------------
;; Session management
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-fork-session ()
  "Fork the current session and continue from the fork."
  (interactive)
  (if tiqsi-opencode--session-id
      (let ((buf (tiqsi-opencode--get-or-create-buffer)))
        (with-current-buffer buf
          (let ((inhibit-read-only t)
                (process-environment (tiqsi-opencode--build-env)))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (tiqsi-claude-repl--colorize
                     (format "Forking session %s..." tiqsi-opencode--session-id)
                     'tiqsi-claude-repl-info) "\n")
            ;; Start a forked session with a greeting to capture the new session ID
            (let* ((args (list "run" "--format" "json"
                               "--fork"
                               "--session" tiqsi-opencode--session-id
                               "Continue from this fork."))
                   (process (apply 'start-process
                                   "opencode-fork"
                                   (current-buffer)
                                   tiqsi-opencode-program
                                   args)))
              (setq-local tiqsi-opencode--current-process process)
              (setq-local tiqsi-opencode--request-start-time (current-time))
              ;; Clear session ID so it gets re-captured from the forked response
              (setq-local tiqsi-opencode--session-id nil)
              (set-process-filter process 'tiqsi-opencode--process-filter)
              (set-process-sentinel process 'tiqsi-opencode--process-sentinel)
              (set-process-query-on-exit-flag process nil)))))
    (message "No active session to fork")))

;;;###autoload
(defun tiqsi-opencode-import-session (file)
  "Import a session from JSON FILE."
  (interactive "fImport session file: ")
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s import %s"
              (shell-quote-argument tiqsi-opencode-program)
              (shell-quote-argument file))
     "*OpenCode Import*")))

;;;###autoload
(defun tiqsi-opencode-share-session ()
  "Share the current session."
  (interactive)
  (if tiqsi-opencode--session-id
      (let ((process-environment (tiqsi-opencode--build-env)))
        ;; Use run --share --session to share
        (async-shell-command
         (format "%s run --share --session %s \"\""
                 (shell-quote-argument tiqsi-opencode-program)
                 (shell-quote-argument tiqsi-opencode--session-id))
         "*OpenCode Share*"))
    (message "No active session to share")))

;; ---------------------------------------------------------------------------
;; GitHub PR review
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-pr (number)
  "Review GitHub PR NUMBER with OpenCode."
  (interactive "nPR number: ")
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s pr %d" (shell-quote-argument tiqsi-opencode-program) number)
     (format "*OpenCode PR #%d*" number))))

;; ---------------------------------------------------------------------------
;; Server mode (serve + attach)
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-serve (&optional port)
  "Start OpenCode in headless server mode on PORT (random if nil)."
  (interactive "P")
  (let* ((process-environment (tiqsi-opencode--build-env))
         (port-arg (if port (format " --port %d" (prefix-numeric-value port)) ""))
         (cmd (format "%s serve%s" (shell-quote-argument tiqsi-opencode-program) port-arg)))
    (async-shell-command cmd "*OpenCode Server*")
    (message "OpenCode server starting...%s"
             (if port (format " on port %d" (prefix-numeric-value port)) ""))))

;;;###autoload
(defun tiqsi-opencode-attach (url)
  "Attach to a running OpenCode server at URL."
  (interactive "sServer URL (e.g. http://localhost:4096): ")
  (let ((buf (tiqsi-opencode--get-or-create-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (tiqsi-claude-repl--colorize
                 (format "Attaching to %s..." url)
                 'tiqsi-claude-repl-info) "\n")
        ;; For attached mode, we pass --attach to each run command.
        ;; Store it as buffer-local so --send picks it up.
        (setq-local tiqsi-opencode--attach-url url)
        (insert (tiqsi-claude-repl--format-prompt "oc"))))
    (display-buffer buf)
    (select-window (get-buffer-window buf))))

;;;###autoload
(defun tiqsi-opencode-web ()
  "Start OpenCode server and open web interface."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
     (format "%s web" tiqsi-opencode-program)
     "*OpenCode Web*")))

;; ---------------------------------------------------------------------------
;; Agent management
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-agent-list ()
  "List available OpenCode agents."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s agent list" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode Agents*")))

;;;###autoload
(defun tiqsi-opencode-agent-create ()
  "Create a new OpenCode agent."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s agent create" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode Agent Create*")))

;; ---------------------------------------------------------------------------
;; MCP (Model Context Protocol) servers
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-mcp-list ()
  "List MCP servers and their status."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s mcp list" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode MCP*")))

;;;###autoload
(defun tiqsi-opencode-mcp-add (name command)
  "Add MCP server NAME with COMMAND."
  (interactive "sMCP server name: \nsMCP server command: ")
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s mcp add %s -- %s"
             (shell-quote-argument tiqsi-opencode-program)
             (shell-quote-argument name)
             (shell-quote-argument command))
     "*OpenCode MCP Add*")))

;; ---------------------------------------------------------------------------
;; GitHub integration
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-github-install ()
  "Install the OpenCode GitHub agent."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s github install" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode GitHub*")))

;; ---------------------------------------------------------------------------
;; Debug / Auth
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-debug ()
  "Open OpenCode debugging tools."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s debug" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode Debug*")))

;;;###autoload
(defun tiqsi-opencode-auth ()
  "Manage OpenCode credentials."
  (interactive)
  (let ((process-environment (tiqsi-opencode--build-env)))
    (async-shell-command
      (format "%s auth" (shell-quote-argument tiqsi-opencode-program))
     "*OpenCode Auth*")))

;; ---------------------------------------------------------------------------
;; Smart RET dispatch for the shared REPL mode
;; ---------------------------------------------------------------------------

;; Forward declarations for server transport
(declare-function tiqsi-opencode-server-active-p "tiqsi-claude-repl-opencode-server")
(declare-function tiqsi-opencode-server-send-input "tiqsi-claude-repl-opencode-server")
(declare-function tiqsi-opencode-server-cancel "tiqsi-claude-repl-opencode-server")

(defun tiqsi-repl-smart-send-input ()
  "Send input from current REPL buffer using the correct backend.
OpenCode REPL buffers (named *OpenCode REPL*) route to the server
transport if active, otherwise to the run-based transport.
Everything else goes to `tiqsi-claude-repl-send-input'."
  (interactive)
  (if (string-match-p "\\*OpenCode REPL" (buffer-name))
      (if (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-server-send-input)
        (tiqsi-opencode-send-input))
    (tiqsi-claude-repl-send-input)))

(defun tiqsi-repl-smart-cancel ()
  "Cancel the current request in the correct backend."
  (interactive)
  (if (string-match-p "\\*OpenCode REPL" (buffer-name))
      (if (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-server-cancel)
        (tiqsi-opencode-cancel))
    (tiqsi-claude-repl-cancel)))

(defun tiqsi-repl-smart-clear ()
  "Clear the REPL buffer via the correct backend."
  (interactive)
  (if (string-match-p "\\*OpenCode REPL" (buffer-name))
      (tiqsi-opencode-clear)
    (tiqsi-claude-repl-clear)))

;; ---------------------------------------------------------------------------
;; Backend switching
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-switch ()
  "Switch between Claude and OpenCode backends."
  (interactive)
  (setq tiqsi-repl-backend
        (if (eq tiqsi-repl-backend 'claude) 'opencode 'claude))
  (message "REPL backend switched to: %s"
           (if (eq tiqsi-repl-backend 'claude) "Claude" "OpenCode")))

;; Forward declaration for server start
(declare-function tiqsi-opencode-server-start "tiqsi-claude-repl-opencode-server")

;;;###autoload
(defun tiqsi-repl-smart-start ()
  "Start a REPL using the currently selected backend.
For OpenCode, uses the server transport (opencode serve + SSE)
which supports interactive permission prompts."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (condition-case err
          (tiqsi-opencode-server-start)
        (error
         ;; Fall back to run-based transport
         (message "Server transport failed (%s), using run mode"
                  (error-message-string err))
         (tiqsi-opencode-start)))
    (tiqsi-claude-repl-start)))

;; Forward declarations for server ask
(declare-function tiqsi-opencode-server-send "tiqsi-claude-repl-opencode-server")

;;;###autoload
(defun tiqsi-repl-smart-ask (question)
  "Ask QUESTION using the currently selected backend.
For OpenCode, uses the server transport if active."
  (interactive "sAsk: ")
  (if (eq tiqsi-repl-backend 'opencode)
      (if (tiqsi-opencode-server-active-p)
          (progn
            (tiqsi-opencode-server--prepare-for-response)
            (tiqsi-opencode-server-send question))
        (tiqsi-opencode-ask question))
    (tiqsi-claude-repl-ask-question question)))

;;;###autoload
(defun tiqsi-repl-smart-send-region (start end)
  "Send region from START to END using the currently selected backend."
  (interactive "r")
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-send-region start end)
    (tiqsi-claude-repl-send-region start end)))

(provide 'tiqsi-claude-repl-opencode)

;;; tiqsi-claude-repl-opencode.el ends here
