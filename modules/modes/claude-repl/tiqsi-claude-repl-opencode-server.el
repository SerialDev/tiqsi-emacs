;;; tiqsi-claude-repl-opencode-server.el --- Server transport for OpenCode -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Server-based transport layer for the OpenCode REPL integration.
;;
;; Instead of spawning `opencode run --format json` per message (which
;; cannot handle interactive permission prompts), this module starts
;; `opencode serve` as a persistent background process and communicates
;; via HTTP + SSE (Server-Sent Events).
;;
;; Architecture:
;;
;;   ┌─────────────────┐     HTTP POST          ┌──────────────────┐
;;   │  Emacs REPL buf  │ ──────────────────────▶│  opencode serve  │
;;   │                  │ ◀──────────────────────│  (port 14096)    │
;;   │                  │     SSE event stream   │                  │
;;   └─────────────────┘                         └──────────────────┘
;;
;; Key capabilities over the `run` transport:
;;   - Interactive permission prompts (y/n/always via `y-or-n-p')
;;   - Streaming text deltas for real-time output
;;   - Session persistence across messages (no cold start per message)
;;   - Undo/redo via server API
;;   - Full access to server API (files, symbols, diagnostics)
;;
;; SSE event types we handle:
;;   message.part.delta    — streaming text (insert delta into buffer)
;;   message.part.updated  — part complete (step-start, text, step-finish, tool-use)
;;   message.updated       — message metadata (cost, tokens, finish reason)
;;   session.status        — busy/idle transitions
;;   session.idle          — response complete
;;   session.error         — error during processing
;;   permission.asked      — tool needs approval → prompt user
;;   permission.replied    — confirmation of our reply
;;
;; Usage:
;;   (tiqsi-opencode-server-start)       ; Start serve + connect SSE
;;   (tiqsi-opencode-server-send "msg")  ; Send via HTTP
;;   (tiqsi-opencode-server-stop)        ; Tear down

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

;; Declarations from sibling modules
(declare-function tiqsi-claude-repl--colorize "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--make-separator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-timestamp "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-status "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--format-prompt "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--thinking-indicator "tiqsi-claude-repl-ui")
(declare-function tiqsi-claude-repl--apply-markdown-formatting "tiqsi-claude-repl-features")
(declare-function tiqsi-claude-repl--get-project-root "tiqsi-claude-repl-core")
(declare-function tiqsi-claude-repl-mode "tiqsi-claude-repl-core")

;; Forward declarations for variables from opencode.el
(defvar tiqsi-opencode-program)
(defvar tiqsi-opencode-model)
(defvar tiqsi-opencode-agent)
(defvar tiqsi-opencode-tls-bypass)
(defvar tiqsi-opencode-show-cost)
(defvar tiqsi-opencode-show-tool-use)
(defvar tiqsi-opencode-show-thinking)
(defvar tiqsi-opencode--attached-files)

;; ---------------------------------------------------------------------------
;; Custom variables
;; ---------------------------------------------------------------------------

(defcustom tiqsi-opencode-server-port 14096
  "Port for the OpenCode server.
A random port is used if this port is unavailable."
  :type 'integer
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-server-host "127.0.0.1"
  "Hostname for the OpenCode server."
  :type 'string
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-server-startup-timeout 15
  "Seconds to wait for the OpenCode server to become healthy."
  :type 'integer
  :group 'tiqsi-opencode)

(defcustom tiqsi-opencode-permission-prompt 'ask
  "How to handle permission requests from OpenCode.
  `ask'    — prompt with `y-or-n-p' for each request (default)
  `always' — auto-approve all requests
  `reject' — auto-reject all requests"
  :type '(choice (const :tag "Ask interactively" ask)
                 (const :tag "Auto-approve all" always)
                 (const :tag "Auto-reject all" reject))
  :group 'tiqsi-opencode)

;; ---------------------------------------------------------------------------
;; State variables
;; ---------------------------------------------------------------------------

(defvar tiqsi-opencode-server--process nil
  "The `opencode serve' background process.")

(defvar tiqsi-opencode-server--sse-process nil
  "The SSE event stream connection (a network process via url-retrieve).")

(defvar tiqsi-opencode-server--sse-buffer nil
  "Buffer accumulating SSE data from the event stream.")

(defvar tiqsi-opencode-server--base-url nil
  "Base URL of the running OpenCode server (e.g. \"http://127.0.0.1:14096\").")

(defvar tiqsi-opencode-server--session-id nil
  "Current session ID on the server.")

(defvar tiqsi-opencode-server--repl-buffer nil
  "The REPL buffer that displays output.")

(defvar tiqsi-opencode-server--busy nil
  "Non-nil when the server is processing a message.")

(defvar tiqsi-opencode-server--request-start-time nil
  "Time when the current request started.")

(defvar tiqsi-opencode-server--total-cost 0.0
  "Total cost accumulated across the server session.")

(defvar tiqsi-opencode-server--total-tokens 0
  "Total tokens used across the server session.")

(defvar tiqsi-opencode-server--message-count 0
  "Number of messages exchanged via the server.")

(defvar tiqsi-opencode-server--sse-partial ""
  "Partial SSE data line being accumulated.")

(defvar tiqsi-opencode-server--sse-reconnect-count 0
  "Number of consecutive SSE reconnect attempts.")

(defvar tiqsi-opencode-server--sse-max-reconnects 10
  "Maximum consecutive SSE reconnect attempts before giving up.")

(defvar tiqsi-opencode-server--current-text-part-id nil
  "Part ID of the current text part being streamed.")

(defvar tiqsi-opencode-server--output-start nil
  "Marker for where the current response output starts in the REPL buffer.")

(defvar tiqsi-opencode-server--tool-calls (make-hash-table :test 'equal)
  "Cache of active tool calls, keyed by callID.
Each value is a plist with :tool (name), :input (hash-table),
:status (string).  Populated from `tool' part updates so that
`permission.asked' events can display rich context.")

(defvar tiqsi-opencode-server--permission-grants nil
  "List of permission grants made during the current session.
Each entry is a hash-table with keys: permission, pattern, action, time.
Populated when we reply \"always\" or \"once\" to a permission.asked event.
This is the client-side cache — the server API does not expose
interactive grants, only upfront overrides from session creation.")

;; ---------------------------------------------------------------------------
;; Internal faces (reuse from opencode.el where possible)
;; ---------------------------------------------------------------------------

(defvar tiqsi-opencode-tool-use)   ;; face
(defvar tiqsi-opencode-tool-result) ;; face
(defvar tiqsi-opencode-cost)       ;; face

;; ---------------------------------------------------------------------------
;; Utility: HTTP requests
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--url (path)
  "Build full URL for PATH on the OpenCode server."
  (concat tiqsi-opencode-server--base-url path))

(defun tiqsi-opencode-server--http-request (method path &optional body callback)
  "Make an HTTP METHOD request to PATH with optional JSON BODY.
If CALLBACK is non-nil, call it with the parsed JSON response.
If CALLBACK is nil, request is synchronous and returns parsed JSON."
  (let* ((url-request-method method)
         (url-request-extra-headers
          '(("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data
          (when body (encode-coding-string (json-serialize body) 'utf-8)))
         (url (tiqsi-opencode-server--url path)))
    (if callback
        ;; Async
        (url-retrieve url
                      (lambda (status cb)
                        (if (plist-get status :error)
                            (progn
                              (message "OpenCode HTTP error: %s"
                                       (plist-get status :error))
                              (kill-buffer (current-buffer)))
                           (let ((json-response
                                  (when url-http-end-of-headers
                                    (goto-char url-http-end-of-headers)
                                    (condition-case nil
                                        (json-parse-buffer)
                                      (error nil)))))
                             (kill-buffer (current-buffer))
                             (when cb (funcall cb json-response)))))
                      (list callback)
                      t)  ; silent
      ;; Sync
      (let ((buf (url-retrieve-synchronously url t nil 10)))
        (if (null buf)
            nil
          (with-current-buffer buf
            (if (null url-http-end-of-headers)
                (prog1 nil (kill-buffer))
              (goto-char url-http-end-of-headers)
              (prog1
                  (condition-case nil
                      (json-parse-buffer)
                    (error nil))
                (kill-buffer)))))))))

(defun tiqsi-opencode-server--health-check ()
  "Check if the OpenCode server is healthy. Returns t or nil."
  (condition-case nil
      (let ((resp (tiqsi-opencode-server--http-request
                   "GET" "/global/health")))
        (and resp (eq (gethash "healthy" resp) t)))
    (error nil)))

;; ---------------------------------------------------------------------------
;; Server process management
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--build-env ()
  "Build process environment for the OpenCode server."
  (let ((env (copy-sequence process-environment)))
    (when tiqsi-opencode-tls-bypass
      (push "NODE_TLS_REJECT_UNAUTHORIZED=0" env))
    env))

(defun tiqsi-opencode-server--start-process ()
  "Start `opencode serve' as a background process.
Returns the process object."
  (let* ((process-environment (tiqsi-opencode-server--build-env))
         (buf (get-buffer-create " *opencode-server*"))
         (proc (start-process "opencode-server" buf
                              tiqsi-opencode-program
                              "serve"
                              "--port" (number-to-string tiqsi-opencode-server-port)
                              "--hostname" tiqsi-opencode-server-host)))
    (set-process-query-on-exit-flag proc nil)
    (setq tiqsi-opencode-server--process proc)
    (setq tiqsi-opencode-server--base-url
          (format "http://%s:%d"
                  tiqsi-opencode-server-host
                  tiqsi-opencode-server-port))
    proc))

(defun tiqsi-opencode-server--wait-for-healthy ()
  "Wait until the server responds healthy, or timeout.
Uses `sit-for' to allow redisplay during the wait.
Returns t on success, nil on timeout."
  (let ((deadline (+ (float-time) tiqsi-opencode-server-startup-timeout))
        (healthy nil))
    (while (and (not healthy) (< (float-time) deadline))
      (setq healthy (tiqsi-opencode-server--health-check))
      (unless healthy (sit-for 0.5)))
    healthy))

(defun tiqsi-opencode-server--ensure-running ()
  "Ensure the OpenCode server is running and healthy.
Starts it if needed.  Returns t on success, signals error on failure."
  (if (and tiqsi-opencode-server--process
           (process-live-p tiqsi-opencode-server--process)
           (tiqsi-opencode-server--health-check))
      t
    ;; Server not running or unhealthy — (re)start
    (tiqsi-opencode-server--stop-process)
    (tiqsi-opencode-server--start-process)
    (if (tiqsi-opencode-server--wait-for-healthy)
        t
      (error "OpenCode server failed to start within %ds"
             tiqsi-opencode-server-startup-timeout))))

(defun tiqsi-opencode-server--stop-process ()
  "Stop the OpenCode server process if running."
  (when (and tiqsi-opencode-server--process
             (process-live-p tiqsi-opencode-server--process))
    (delete-process tiqsi-opencode-server--process))
  (setq tiqsi-opencode-server--process nil)
  ;; Also kill SSE
  (tiqsi-opencode-server--sse-disconnect)
  ;; Kill server buffer
  (when-let* ((buf (get-buffer " *opencode-server*")))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; SSE event stream
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--sse-connect ()
  "Connect to the OpenCode SSE event stream.
Spawns a background curl process reading from /event."
  (tiqsi-opencode-server--sse-disconnect)
  (let* ((process-environment (tiqsi-opencode-server--build-env))
         (buf (get-buffer-create " *opencode-sse*"))
         (url (tiqsi-opencode-server--url "/event"))
         (proc (start-process "opencode-sse" buf
                              "curl" "-s" "-N"
                              "-H" "Accept: text/event-stream"
                              url)))
    (setq tiqsi-opencode-server--sse-process proc)
    (setq tiqsi-opencode-server--sse-buffer buf)
    (setq tiqsi-opencode-server--sse-partial "")
    (setq tiqsi-opencode-server--sse-reconnect-count 0)
    (set-process-filter proc #'tiqsi-opencode-server--sse-filter)
    (set-process-sentinel proc #'tiqsi-opencode-server--sse-sentinel)
    (set-process-query-on-exit-flag proc nil)
    proc))

(defun tiqsi-opencode-server--sse-disconnect ()
  "Disconnect the SSE event stream."
  (when (and tiqsi-opencode-server--sse-process
             (process-live-p tiqsi-opencode-server--sse-process))
    (delete-process tiqsi-opencode-server--sse-process))
  (setq tiqsi-opencode-server--sse-process nil)
  (when (and tiqsi-opencode-server--sse-buffer
             (buffer-live-p tiqsi-opencode-server--sse-buffer))
    (kill-buffer tiqsi-opencode-server--sse-buffer))
  (setq tiqsi-opencode-server--sse-buffer nil))

(defun tiqsi-opencode-server--sse-sentinel (process event)
  "Handle SSE PROCESS termination EVENT.  Reconnect if unexpected.
Gives up after `tiqsi-opencode-server--sse-max-reconnects' consecutive
failures to avoid infinite reconnect loops."
  (when (string-match-p "\\(finished\\|exited\\|connection broken\\)" event)
    (message "OpenCode SSE stream disconnected: %s" (string-trim event))
    ;; Auto-reconnect if server is still alive and we haven't exceeded retries
    (if (and tiqsi-opencode-server--process
             (process-live-p tiqsi-opencode-server--process)
             (< tiqsi-opencode-server--sse-reconnect-count
                tiqsi-opencode-server--sse-max-reconnects))
        (progn
          (cl-incf tiqsi-opencode-server--sse-reconnect-count)
          (let ((delay (min 5 tiqsi-opencode-server--sse-reconnect-count)))
            (message "SSE reconnecting in %ds (attempt %d/%d)..."
                     delay
                     tiqsi-opencode-server--sse-reconnect-count
                     tiqsi-opencode-server--sse-max-reconnects)
            (run-at-time delay nil #'tiqsi-opencode-server--sse-connect)))
      (when (>= tiqsi-opencode-server--sse-reconnect-count
                tiqsi-opencode-server--sse-max-reconnects)
        (message "OpenCode SSE: max reconnect attempts reached (%d). Use `R' to restart."
                 tiqsi-opencode-server--sse-max-reconnects)))))

(defun tiqsi-opencode-server--sse-filter (process output)
  "Process filter for SSE stream OUTPUT from PROCESS.
SSE format: `data: {json}\\n\\n' — each event is prefixed with `data: '."
  (setq tiqsi-opencode-server--sse-partial
        (concat tiqsi-opencode-server--sse-partial output))
  ;; Split on double-newlines (SSE event boundaries)
  (let ((events (split-string tiqsi-opencode-server--sse-partial "\n\n")))
    ;; Last element is either "" or partial — save it
    (setq tiqsi-opencode-server--sse-partial (car (last events)))
    (setq events (butlast events))
    (dolist (raw-event events)
      (let ((data-line (string-trim raw-event)))
        (when (string-prefix-p "data: " data-line)
          (let ((json-str (substring data-line 6)))
            (condition-case err
                (let ((event (json-parse-string json-str)))
                  (tiqsi-opencode-server--handle-event event))
              (json-parse-error
               (message "OpenCode SSE JSON error: %s" (error-message-string err))))))))))

;; ---------------------------------------------------------------------------
;; Event dispatch
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--handle-event (event)
  "Dispatch a parsed SSE EVENT object by type."
  (let ((type (gethash "type" event))
        (props (gethash "properties" event)))
    (cond
     ;; --- Streaming text delta ---
     ((equal type "message.part.delta")
      (tiqsi-opencode-server--on-text-delta props))

     ;; --- Part updated (step-start, text complete, step-finish, tool-use) ---
     ((equal type "message.part.updated")
      (tiqsi-opencode-server--on-part-updated props))

     ;; --- Message metadata updated (cost, tokens) ---
     ((equal type "message.updated")
      (tiqsi-opencode-server--on-message-updated props))

     ;; --- Session status ---
     ((equal type "session.status")
      (tiqsi-opencode-server--on-session-status props))

     ;; --- Session idle (response complete) ---
     ((equal type "session.idle")
      (tiqsi-opencode-server--on-session-idle props))

     ;; --- Session error ---
     ((equal type "session.error")
      (tiqsi-opencode-server--on-session-error props))

     ;; --- Permission asked ---
     ((equal type "permission.asked")
      (tiqsi-opencode-server--on-permission-asked props))

     ;; --- Permission replied (confirmation) ---
     ((equal type "permission.replied")
      nil) ;; Informational, no action needed

     ;; --- Ignore other events ---
     (t nil))))

;; ---------------------------------------------------------------------------
;; Event handlers
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--repl-insert (&rest strings)
  "Insert STRINGS at the end of the REPL buffer."
  (when (and tiqsi-opencode-server--repl-buffer
             (buffer-live-p tiqsi-opencode-server--repl-buffer))
    (with-current-buffer tiqsi-opencode-server--repl-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (apply #'insert strings))))))

(defun tiqsi-opencode-server--on-text-delta (props)
  "Handle streaming text delta.
PROPS has sessionID, messageID, partID, field, delta."
  (when (and props
             (equal (gethash "field" props) "text"))
    (let ((delta (gethash "delta" props))
          (session-id (gethash "sessionID" props)))
      ;; Filter to our session
      (when (and delta
                 (or (null tiqsi-opencode-server--session-id)
                     (equal session-id tiqsi-opencode-server--session-id)))
        (tiqsi-opencode-server--repl-insert delta)))))

(defun tiqsi-opencode-server--on-part-updated (props)
  "Handle part updated events.
PROPS contains `part' with type, text, cost, tokens, etc."
  (let* ((part (gethash "part" props))
         (part-type (gethash "type" part))
         (session-id (gethash "sessionID" part)))
    ;; Filter to our session
    (when (or (null tiqsi-opencode-server--session-id)
              (equal session-id tiqsi-opencode-server--session-id))
      (cond
       ;; Step start
       ((equal part-type "step-start")
        (tiqsi-opencode-server--repl-insert-clear-thinking)
        (when (and tiqsi-opencode-server--repl-buffer
                   (buffer-live-p tiqsi-opencode-server--repl-buffer))
          (with-current-buffer tiqsi-opencode-server--repl-buffer
            (let ((inhibit-read-only t))
              (save-excursion
                (goto-char (point-max))
                (unless (bolp) (insert "\n"))
                (setq tiqsi-opencode-server--output-start (point-marker)))))))

       ;; Step finish (cost/tokens)
       ((equal part-type "step-finish")
        (when tiqsi-opencode-show-cost
          (let* ((cost (gethash "cost" part 0))
                 (tokens (gethash "tokens" part))
                 (total-tok (if tokens (gethash "total" tokens 0) 0))
                 (input-tok (if tokens (gethash "input" tokens 0) 0))
                 (output-tok (if tokens (gethash "output" tokens 0) 0))
                 (cache (if tokens (gethash "cache" tokens) nil))
                 (cache-read (if cache (gethash "read" cache 0) 0)))
            (cl-incf tiqsi-opencode-server--total-cost cost)
            (cl-incf tiqsi-opencode-server--total-tokens total-tok)
            (tiqsi-opencode-server--repl-insert
             "\n"
             (tiqsi-claude-repl--colorize
              (format "── $%.4f │ %d tok (in:%d out:%d cache:%d) ──"
                      cost total-tok input-tok output-tok cache-read)
              'tiqsi-opencode-cost)
             "\n"))))

       ;; Tool part — from the server API (type="tool" with callID, tool name, state)
       ;; Cache the tool call details for permission prompts, and render progress.
       ((equal part-type "tool")
        (let* ((call-id (gethash "callID" part))
               (tool-name (or (gethash "tool" part) "unknown"))
               (state (gethash "state" part))
               (status (when state (gethash "status" state)))
               (input (when state (gethash "input" state))))
          ;; Cache tool call details by callID
          (when call-id
            (puthash call-id
                     (list :tool tool-name
                           :input input
                           :status status
                           :part-id (gethash "id" part))
                     tiqsi-opencode-server--tool-calls))
          ;; Render tool use in REPL when status changes to "running"
          (when (and tiqsi-opencode-show-tool-use
                     (equal status "running"))
            (tiqsi-opencode-server--repl-insert
             "\n"
             (tiqsi-claude-repl--colorize
              (format "🔧 %s" tool-name) 'tiqsi-opencode-tool-use))
            (when input
              (let ((input-str (tiqsi-opencode-server--format-tool-input
                                tool-name input)))
                (when (> (length input-str) 0)
                  (tiqsi-opencode-server--repl-insert
                   " "
                   (tiqsi-claude-repl--colorize input-str 'tiqsi-opencode-cost)))))
            (tiqsi-opencode-server--repl-insert "\n"))
          ;; Render tool result when complete
          (when (and tiqsi-opencode-show-tool-use
                     (equal status "completed"))
            (let* ((output (when state (gethash "output" state)))
                   (result-str (cond
                                ((stringp output)
                                 (substring output 0 (min 300 (length output))))
                                ((hash-table-p output)
                                 (let ((s (json-serialize output)))
                                   (substring s 0 (min 300 (length s)))))
                                (t nil))))
              (when result-str
                (tiqsi-opencode-server--repl-insert
                 (tiqsi-claude-repl--colorize
                  (format "  ↳ %s" result-str) 'tiqsi-opencode-tool-result)
                 "\n"))))))

       ;; Legacy tool-use / tool_use events (from run-based transport format)
       ((or (equal part-type "tool-use") (equal part-type "tool_use"))
        (when tiqsi-opencode-show-tool-use
          (let ((tool-name (or (gethash "name" part)
                               (gethash "tool" part)
                               "unknown")))
            (tiqsi-opencode-server--repl-insert
             "\n"
             (tiqsi-claude-repl--colorize
              (format "🔧 %s" tool-name) 'tiqsi-opencode-tool-use)
             "\n"))))

       ;; Legacy tool-result / tool_result events
       ((or (equal part-type "tool-result") (equal part-type "tool_result"))
        (when tiqsi-opencode-show-tool-use
          (let ((result (or (gethash "result" part)
                            (gethash "output" part))))
            (when (and result (stringp result))
              (tiqsi-opencode-server--repl-insert
               (tiqsi-claude-repl--colorize
                (format "  ↳ %s" (substring result 0 (min 300 (length result))))
                'tiqsi-opencode-tool-result)
               "\n")))))

       ;; Thinking
       ((equal part-type "thinking")
        (when tiqsi-opencode-show-thinking
          (let ((content (gethash "text" part)))
            (when content
              (tiqsi-opencode-server--repl-insert
               (tiqsi-claude-repl--colorize "🤔 " 'tiqsi-claude-repl-thinking)
               (tiqsi-claude-repl--colorize
                (substring content 0 (min 200 (length content)))
                'tiqsi-claude-repl-info)
               "\n")))))

       ;; Text part complete — no action (delta already streamed it)
       ((equal part-type "text") nil)

       ;; Other
       (t nil)))))

(defun tiqsi-opencode-server--repl-insert-clear-thinking ()
  "Clear the thinking indicator and show processing time."
  (when (and tiqsi-opencode-server--repl-buffer
             (buffer-live-p tiqsi-opencode-server--repl-buffer))
    (with-current-buffer tiqsi-opencode-server--repl-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (when (re-search-backward "⏳ Thinking\\.\\.\\." nil t)
            (let ((elapsed (if tiqsi-opencode-server--request-start-time
                              (format " [%.1fs]"
                                      (float-time (time-since
                                                   tiqsi-opencode-server--request-start-time)))
                            "")))
              (replace-match (format "🧠 Processing%s" elapsed)))))))))

(defun tiqsi-opencode-server--on-message-updated (props)
  "Handle message updated event.
PROPS has `info' with cost, tokens, finish, etc."
  (let* ((info (gethash "info" props))
         (session-id (gethash "sessionID" info))
         (role (gethash "role" info)))
    ;; Only process assistant messages for our session
    (when (and (equal role "assistant")
               (or (null tiqsi-opencode-server--session-id)
                   (equal session-id tiqsi-opencode-server--session-id)))
      ;; Update cost/tokens from the final message
      (let ((cost (gethash "cost" info 0))
            (tokens (gethash "tokens" info)))
        (when (and cost (> cost 0))
          ;; Cost is already accumulated from step-finish events,
          ;; so we don't double-count here
          nil)))))

(defun tiqsi-opencode-server--on-session-status (props)
  "Handle session status change.
PROPS has sessionID and status (with type: busy|idle)."
  (let* ((session-id (gethash "sessionID" props))
         (status (gethash "status" props))
         (status-type (when status (gethash "type" status))))
    (when (or (null tiqsi-opencode-server--session-id)
              (equal session-id tiqsi-opencode-server--session-id))
      (cond
       ((equal status-type "busy")
        (setq tiqsi-opencode-server--busy t))
       ((equal status-type "idle")
        (setq tiqsi-opencode-server--busy nil))))))

(defun tiqsi-opencode-server--on-session-idle (props)
  "Handle session idle event — response is complete.
PROPS has sessionID."
  (let ((session-id (gethash "sessionID" props)))
    (when (or (null tiqsi-opencode-server--session-id)
              (equal session-id tiqsi-opencode-server--session-id))
      (setq tiqsi-opencode-server--busy nil)
      ;; Apply markdown formatting
      (when (and tiqsi-opencode-server--repl-buffer
                 (buffer-live-p tiqsi-opencode-server--repl-buffer)
                 tiqsi-opencode-server--output-start)
        (with-current-buffer tiqsi-opencode-server--repl-buffer
          (let ((inhibit-read-only t))
            (tiqsi-claude-repl--apply-markdown-formatting
             tiqsi-opencode-server--output-start (point-max)))))
      ;; Show completion time and add prompt
      (when tiqsi-opencode-server--request-start-time
        (let ((elapsed (float-time (time-since tiqsi-opencode-server--request-start-time))))
          (tiqsi-opencode-server--repl-insert
           "\n"
           (tiqsi-claude-repl--colorize
            (format "✅ Completed in %.1fs" elapsed)
            'tiqsi-claude-repl-success)
           "\n")))
      ;; Clear state
      (setq tiqsi-opencode-server--request-start-time nil)
      (setq tiqsi-opencode-server--output-start nil)
      (setq tiqsi-opencode-server--current-text-part-id nil)
      ;; Clear tool call cache (no longer needed after response complete)
      (clrhash tiqsi-opencode-server--tool-calls)
      ;; Add prompt
      (tiqsi-opencode-server--repl-insert
       "\n" (tiqsi-claude-repl--format-prompt "oc"))
      ;; Move point to end
      (when (and tiqsi-opencode-server--repl-buffer
                 (buffer-live-p tiqsi-opencode-server--repl-buffer))
        (with-current-buffer tiqsi-opencode-server--repl-buffer
          (goto-char (point-max)))))))

(defun tiqsi-opencode-server--on-session-error (props)
  "Handle session error event.
PROPS has sessionID and error."
  (let* ((session-id (gethash "sessionID" props))
         (err (gethash "error" props))
         (err-msg (if (hash-table-p err)
                      (or (gethash "message" err)
                          (json-serialize err))
                    (format "%s" err))))
    (when (or (null tiqsi-opencode-server--session-id)
              (equal session-id tiqsi-opencode-server--session-id))
      (setq tiqsi-opencode-server--busy nil)
      (tiqsi-opencode-server--repl-insert
       "\n"
       (tiqsi-claude-repl--colorize
        (format "❌ Error: %s" err-msg)
        'tiqsi-claude-repl-error)
       "\n\n"
       (tiqsi-claude-repl--format-prompt "oc"))
      (setq tiqsi-opencode-server--request-start-time nil))))

;; ---------------------------------------------------------------------------
;; Permission handling
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--on-permission-asked (props)
  "Handle a permission request from the server.
PROPS is a PermissionRequest object with id, sessionID, permission,
patterns, metadata, always, tool."
  (let* ((request-id (gethash "id" props))
         (session-id (gethash "sessionID" props))
         (permission (gethash "permission" props))
         (patterns (gethash "patterns" props))
         (metadata (gethash "metadata" props))
         (always-patterns (gethash "always" props))
         ;; Look up cached tool call details for rich context
         (tool-obj (gethash "tool" props))
         (call-id (when (hash-table-p tool-obj)
                    (gethash "callID" tool-obj)))
         (tool-info (when call-id
                      (gethash call-id tiqsi-opencode-server--tool-calls))))
    ;; Only handle for our session
    (when (or (null tiqsi-opencode-server--session-id)
              (equal session-id tiqsi-opencode-server--session-id))
      (let ((desc (tiqsi-opencode-server--format-permission-desc
                   permission patterns metadata tool-info)))
        ;; Show the permission request and options in the REPL buffer
        ;; BEFORE prompting, so the user can see what they're deciding on
        (tiqsi-opencode-server--repl-insert
         "\n"
         (tiqsi-claude-repl--make-separator) "\n"
         (tiqsi-claude-repl--colorize
          (format "🔐 Permission requested: %s" desc)
          'tiqsi-opencode-tool-use)
         "\n"
         (tiqsi-claude-repl--colorize
          "   [y] Allow once  [a] Allow always  [n] Reject"
          'tiqsi-claude-repl-info)
         "\n"))

      ;; Determine response based on user preference.
      ;; For 'ask mode, we defer the interactive prompt out of the
      ;; process filter using `run-at-time' to avoid blocking SSE
      ;; data processing while the user is in the minibuffer.
      (pcase tiqsi-opencode-permission-prompt
        ('always
         (tiqsi-opencode-server--repl-insert
          (tiqsi-claude-repl--colorize
           "   (auto-approve mode)" 'tiqsi-claude-repl-info)
          "\n")
         (tiqsi-opencode-server--finalize-permission
          request-id "always" permission patterns))
        ('reject
         (tiqsi-opencode-server--repl-insert
          (tiqsi-claude-repl--colorize
           "   (auto-reject mode)" 'tiqsi-claude-repl-error)
          "\n")
         (tiqsi-opencode-server--finalize-permission request-id "reject"))
        ('ask
         ;; Defer interactive prompt out of the process filter
         (run-at-time 0 nil
                      #'tiqsi-opencode-server--deferred-permission-prompt
                      request-id permission patterns metadata always-patterns))))))

(defun tiqsi-opencode-server--deferred-permission-prompt
    (request-id permission patterns metadata always-patterns)
  "Prompt user for permission interactively (deferred from SSE filter).
REQUEST-ID is the server permission ID to reply to."
  (let ((response (tiqsi-opencode-server--prompt-permission
                   permission patterns metadata always-patterns)))
    (tiqsi-opencode-server--finalize-permission
     request-id response permission patterns)))

(defun tiqsi-opencode-server--finalize-permission (request-id response
                                                   &optional perm-type patterns)
  "Send RESPONSE for REQUEST-ID to server and display decision in REPL.
PERM-TYPE and PATTERNS are the permission name and pattern list from
the original request, used to cache the grant locally."
  (tiqsi-opencode-server--reply-permission request-id response)
  ;; Cache the grant so `show-permissions' can display it
  (when (and (member response '("always" "once"))
             perm-type)
    (let ((entry (make-hash-table :test 'equal)))
      (puthash "permission" perm-type entry)
      (puthash "pattern"
               (or (and (sequencep patterns)
                        (> (length patterns) 0)
                        (if (vectorp patterns)
                            (mapconcat #'identity (append patterns nil) ", ")
                          (mapconcat #'identity patterns ", ")))
                   "*")
               entry)
      (puthash "action" (if (equal response "always") "allow" "allow-once") entry)
      (puthash "time" (format-time-string "%H:%M:%S") entry)
      (push entry tiqsi-opencode-server--permission-grants)))
  (tiqsi-opencode-server--repl-insert
   (tiqsi-claude-repl--colorize
    (format "  → %s"
            (pcase response
              ("once"   "✓ Allowed (once)")
              ("always" "✓ Allowed (always)")
              ("reject" "✗ Rejected")
              (_ response)))
    (if (equal response "reject")
        'tiqsi-claude-repl-error
      'tiqsi-claude-repl-success))
   "\n"
   (tiqsi-claude-repl--make-separator)
   "\n"))

(defun tiqsi-opencode-server--format-tool-input (tool-name input)
  "Format INPUT hash-table for TOOL-NAME into a human-readable string.
Returns a concise summary of the tool's arguments."
  (cond
   ;; File operations — show filePath
   ((and (member tool-name '("read" "edit" "write" "glob"))
         (hash-table-p input))
    (let ((path (gethash "filePath" input
                         (gethash "path" input
                                  (gethash "pattern" input nil)))))
      (if path (format "%s" path) "")))
   ;; Bash — show command
   ((and (equal tool-name "bash")
         (hash-table-p input))
    (let ((cmd (gethash "command" input nil)))
      (if cmd
          (let ((truncated (if (> (length cmd) 120)
                               (concat (substring cmd 0 117) "...")
                             cmd)))
            (format "$ %s" truncated))
        "")))
   ;; Grep / content search — show pattern + include
   ((and (equal tool-name "grep")
         (hash-table-p input))
    (let ((pat (gethash "pattern" input ""))
          (inc (gethash "include" input nil)))
      (if inc
          (format "\"%s\" in %s" pat inc)
        (format "\"%s\"" pat))))
   ;; Web fetch — show URL
   ((and (equal tool-name "webfetch")
         (hash-table-p input))
    (let ((url (gethash "url" input nil)))
      (if url (format "%s" url) "")))
   ;; Task / agent — show description or prompt snippet
   ((and (equal tool-name "task")
         (hash-table-p input))
    (let ((desc (gethash "description" input
                         (gethash "prompt" input nil))))
      (if desc
          (let ((truncated (if (> (length desc) 80)
                               (concat (substring desc 0 77) "...")
                             desc)))
            (format "%s" truncated))
        "")))
   ;; Generic fallback — serialize and truncate
   ((hash-table-p input)
    (let ((s (json-serialize input)))
      (if (> (length s) 120)
          (concat (substring s 0 117) "...")
        s)))
   (t "")))

(defun tiqsi-opencode-server--format-permission-desc (permission patterns metadata &optional tool-info)
  "Format a human-readable description of a PERMISSION request.
PERMISSION is the permission type, PATTERNS is the list of patterns,
METADATA contains additional context.  TOOL-INFO, when non-nil, is
a plist from the tool-calls cache (:tool NAME :input HASH :status S)
and is used to produce richer output."
  (let ((pattern-str (if (and patterns (sequencep patterns) (> (length patterns) 0))
                         (mapconcat #'identity
                                    (cl-coerce patterns 'list)
                                    ", ")
                       ""))
        (tool-name (when tool-info (plist-get tool-info :tool)))
        (tool-input (when tool-info (plist-get tool-info :input))))
    ;; Base description from permission type
    (let ((base-desc
           (cond
            ((equal permission "edit")
             (format "edit file: %s" pattern-str))
            ((equal permission "bash")
             (format "run command: %s" pattern-str))
            ((equal permission "read")
             (format "read file: %s" pattern-str))
            ((equal permission "webfetch")
             (format "fetch URL: %s" pattern-str))
            ((equal permission "external_directory")
             (format "access external path: %s" pattern-str))
            (t
             (format "%s: %s" permission pattern-str)))))
      ;; Enrich with tool call details if available
      (if (and tool-name tool-input)
          (let ((input-str (tiqsi-opencode-server--format-tool-input
                            tool-name tool-input)))
            (if (> (length input-str) 0)
                (format "%s\n      Tool: %s\n      Input: %s"
                        base-desc tool-name input-str)
              (format "%s\n      Tool: %s" base-desc tool-name)))
        base-desc))))

(defun tiqsi-opencode-server--prompt-permission (permission patterns metadata always-patterns)
  "Prompt the user for a permission decision.
Shows a clear selection in the minibuffer.  Returns \"once\",
\"always\", or \"reject\"."
  (let* ((pattern-str (if (and patterns (> (length patterns) 0))
                          (mapconcat #'identity
                                     (cl-coerce patterns 'list)
                                     ", ")
                        ""))
         (choice (completing-read
                  (format "🔐 %s %s → " permission pattern-str)
                  '("Allow once" "Allow always" "Reject")
                  nil t nil nil "Allow once")))
    (cond
     ((string-match-p "once" choice) "once")
     ((string-match-p "always" choice) "always")
     ((string-match-p "Reject" choice) "reject")
     (t "once"))))

(defun tiqsi-opencode-server--reply-permission (request-id response)
  "Send a permission RESPONSE for REQUEST-ID to the server.
RESPONSE is \"once\", \"always\", or \"reject\"."
  (tiqsi-opencode-server--http-request
   "POST"
   (format "/permission/%s/reply" request-id)
   `(:reply ,response)
   (lambda (_resp)
     ;; Response handled; nothing more to do
     nil)))

;; ---------------------------------------------------------------------------
;; Session management
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--create-session (&optional title)
  "Create a new session on the server, optionally with TITLE.
Returns the session ID."
  (let* ((body (if title `(:title ,title) (make-hash-table)))
         (resp (tiqsi-opencode-server--http-request "POST" "/session" body)))
    (when resp
      (let ((id (gethash "id" resp)))
        (setq tiqsi-opencode-server--session-id id)
        id))))

(defun tiqsi-opencode-server--send-message (text)
  "Send TEXT as a message to the current session.
Uses prompt_async so it returns immediately; response streams via SSE."
  (unless tiqsi-opencode-server--session-id
    (error "No active server session"))
  (let* ((parts (vector `(:type "text" :text ,text)))
         (body `(:parts ,parts)))
    ;; Add model if set
    (when tiqsi-opencode-model
      ;; Parse provider/model format
      (let* ((parts-split (split-string tiqsi-opencode-model "/"))
             (provider (car parts-split))
             (model (cadr parts-split)))
        (when (and provider model)
          (setq body (plist-put body :model
                               `(:providerID ,provider :modelID ,model))))))
    ;; Add agent if set
    (when tiqsi-opencode-agent
      (setq body (plist-put body :agent tiqsi-opencode-agent)))
    ;; Send async (fire and forget — response comes via SSE)
    (tiqsi-opencode-server--http-request
     "POST"
     (format "/session/%s/prompt_async" tiqsi-opencode-server--session-id)
     body
     (lambda (_resp) nil))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

;;;###autoload
(defun tiqsi-opencode-server-start ()
  "Start the OpenCode server transport and open the REPL buffer.
Starts `opencode serve', connects SSE, creates a session."
  (interactive)
  ;; Ensure server is running
  (tiqsi-opencode-server--ensure-running)
  ;; Connect SSE
  (tiqsi-opencode-server--sse-connect)
  ;; Create session
  (let ((project-name (file-name-nondirectory
                       (directory-file-name
                        (tiqsi-claude-repl--get-project-root)))))
    (tiqsi-opencode-server--create-session
     (format "emacs: %s" project-name)))
  ;; Create REPL buffer
  (let* ((project-root (tiqsi-claude-repl--get-project-root))
         (project-name (file-name-nondirectory
                        (directory-file-name project-root)))
         (buf-name (format "*OpenCode REPL (%s)*" project-name))
         (buf (get-buffer-create buf-name)))
    (setq tiqsi-opencode-server--repl-buffer buf)
    (with-current-buffer buf
      (tiqsi-claude-repl-mode)
      (when (= (buffer-size) 0)
        (let ((inhibit-read-only t))
          (insert (tiqsi-claude-repl--colorize
                   "OpenCode REPL - Server Mode" 'tiqsi-claude-repl-info) "\n")
          (insert (tiqsi-claude-repl--make-separator) "\n")
          (insert (concat
                   (tiqsi-claude-repl--colorize "Backend: " 'tiqsi-claude-repl-status)
                   (tiqsi-claude-repl--colorize "OpenCode (serve)" 'tiqsi-claude-repl-success)
                   (format " │ %s" tiqsi-opencode-server--base-url)
                   (when tiqsi-opencode-model
                     (concat " │ Model: "
                             (tiqsi-claude-repl--colorize
                              tiqsi-opencode-model 'tiqsi-claude-repl-info)))
                   (when tiqsi-opencode-agent
                     (concat " │ Agent: "
                             (tiqsi-claude-repl--colorize
                              tiqsi-opencode-agent 'tiqsi-claude-repl-info)))
                   (concat " │ "
                           (tiqsi-claude-repl--colorize
                            (format "permissions: %s" tiqsi-opencode-permission-prompt)
                            (if (eq tiqsi-opencode-permission-prompt 'ask)
                                'tiqsi-claude-repl-warning
                              'tiqsi-claude-repl-success))))
                  "\n")
          (insert (tiqsi-claude-repl--make-separator) "\n\n")
          (insert (tiqsi-claude-repl--format-prompt "oc")))))
    (display-buffer buf)
    (select-window (get-buffer-window buf))
    (message "OpenCode server REPL ready (session: %s)"
             tiqsi-opencode-server--session-id)))

;;;###autoload
(defun tiqsi-opencode-server-send (text)
  "Send TEXT to OpenCode via the server transport."
  (unless tiqsi-opencode-server--session-id
    (tiqsi-opencode-server-start))
  (setq tiqsi-opencode-server--request-start-time (current-time))
  (cl-incf tiqsi-opencode-server--message-count)
  (setq tiqsi-opencode-server--busy t)
  (tiqsi-opencode-server--send-message text))

;;;###autoload
(defun tiqsi-opencode-server-send-input ()
  "Send current input from the REPL buffer via server transport."
  (interactive)
  (when (and tiqsi-opencode-server--repl-buffer
             (buffer-live-p tiqsi-opencode-server--repl-buffer))
    (with-current-buffer tiqsi-opencode-server--repl-buffer
      (let ((input (tiqsi-opencode-server--get-input)))
        (when (and input (> (length (string-trim input)) 0))
          (tiqsi-opencode-server--prepare-for-response)
          (tiqsi-opencode-server-send input))))))

(defun tiqsi-opencode-server--get-input ()
  "Get current input from the prompt line."
  (save-excursion
    (goto-char (point-max))
    (let ((end (point)))
      (beginning-of-line)
      (if (re-search-forward "λ " nil t)
          (string-trim (buffer-substring-no-properties (point) end))
        ""))))

(defun tiqsi-opencode-server--prepare-for-response ()
  "Prepare the REPL buffer for a response."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    ;; Tag the input line
    (save-excursion
      (beginning-of-line)
      (when (looking-at "^\\(oc \\)?λ ")
        (goto-char (match-end 0))
        (insert (tiqsi-claude-repl--colorize "You: " 'tiqsi-claude-repl-prompt))))
    (insert "\n")
    (insert (tiqsi-claude-repl--thinking-indicator) "\n")
    (setq tiqsi-opencode-server--request-start-time (current-time))
    (sit-for 0)))

;;;###autoload
(defun tiqsi-opencode-server-stop ()
  "Stop the OpenCode server and clean up."
  (interactive)
  (tiqsi-opencode-server--sse-disconnect)
  (tiqsi-opencode-server--stop-process)
  (setq tiqsi-opencode-server--session-id nil)
  (setq tiqsi-opencode-server--busy nil)
  (setq tiqsi-opencode-server--total-cost 0.0)
  (setq tiqsi-opencode-server--total-tokens 0)
  (setq tiqsi-opencode-server--message-count 0)
  (setq tiqsi-opencode-server--permission-grants nil)
  (message "OpenCode server stopped"))

;;;###autoload
(defun tiqsi-opencode-server-cancel ()
  "Abort the current request on the server."
  (interactive)
  (when tiqsi-opencode-server--session-id
    (tiqsi-opencode-server--http-request
     "POST"
     (format "/session/%s/abort" tiqsi-opencode-server--session-id)
     nil
     (lambda (_resp)
       (setq tiqsi-opencode-server--busy nil)
       (tiqsi-opencode-server--repl-insert
        "\n"
        (tiqsi-claude-repl--colorize "[Request cancelled]"
                                      'tiqsi-claude-repl-error)
        "\n\n"
        (tiqsi-claude-repl--format-prompt "oc"))
       (message "OpenCode request cancelled")))))

;;;###autoload
(defun tiqsi-opencode-server-active-p ()
  "Return non-nil if the server transport is active and healthy."
  (and tiqsi-opencode-server--process
       (process-live-p tiqsi-opencode-server--process)
       tiqsi-opencode-server--session-id
       t))

;;;###autoload
(defun tiqsi-opencode-server-session-stats ()
  "Show server session statistics."
  (interactive)
  (message "OpenCode Server Session: %s\n  Messages: %d │ Cost: $%.4f │ Tokens: %d │ %s"
           (or tiqsi-opencode-server--session-id "not started")
           tiqsi-opencode-server--message-count
           tiqsi-opencode-server--total-cost
           tiqsi-opencode-server--total-tokens
           (if tiqsi-opencode-server--busy "BUSY" "IDLE")))

;;;###autoload
(defun tiqsi-opencode-cycle-permission-prompt ()
  "Cycle the permission prompt mode: ask → always → reject → ask."
  (interactive)
  (setq tiqsi-opencode-permission-prompt
        (pcase tiqsi-opencode-permission-prompt
          ('ask 'always)
          ('always 'reject)
          ('reject 'ask)
          (_ 'ask)))
  (message "OpenCode permissions: %s" tiqsi-opencode-permission-prompt))

;; ---------------------------------------------------------------------------
;; Permission introspection
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--get-session-permissions (&optional session-id)
  "Get the permission rules for SESSION-ID from the session list.
If SESSION-ID is nil, uses the current session.
Returns a list of alists with keys: permission, pattern, action.
Note: the permission field is only available from the session list
endpoint (GET /session), not from the individual session detail."
  (let* ((target-id (or session-id tiqsi-opencode-server--session-id))
         (sessions (tiqsi-opencode-server--list-sessions))
         (session (cl-find-if
                   (lambda (s) (equal (gethash "id" s) target-id))
                   sessions)))
    (when session
      (let ((perms (gethash "permission" session)))
        (cond
         ((vectorp perms) (append perms nil))
         ((listp perms) perms)
         (t nil))))))

(defun tiqsi-opencode-server--format-permission-rule (rule)
  "Format a single permission RULE hash-table into a display string.
RULE has keys: permission, pattern, action."
  (when (hash-table-p rule)
    (let ((perm (gethash "permission" rule "?"))
          (pattern (gethash "pattern" rule "*"))
          (action (gethash "action" rule "?")))
      (format "  %-20s %-8s %s"
              perm
              (if (equal action "allow") "ALLOW" "DENY")
              pattern))))

(defun tiqsi-opencode-server--format-permission-summary (perms)
  "Format a short summary of PERMS list for inline display.
Returns something like: '3 allow, 2 deny' or 'no rules'."
  (if (or (null perms) (= (length perms) 0))
      "no rules"
    (let ((allow 0) (deny 0))
      (dolist (rule perms)
        (when (hash-table-p rule)
          (let ((action (gethash "action" rule "")))
            (cond
             ((string-match-p "allow" action) (cl-incf allow))
             ((equal action "deny") (cl-incf deny))))))
      (cond
       ((and (> allow 0) (> deny 0))
        (format "%d allow, %d deny" allow deny))
       ((> allow 0) (format "%d allow" allow))
       ((> deny 0) (format "%d deny" deny))
       (t "no rules")))))

;;;###autoload
(defun tiqsi-opencode-server-show-permissions ()
  "Display the permission rules for the current session.
Merges server-side rules (from session creation) with client-side
grants (from interactive permission prompts during this session)."
  (interactive)
  (unless (tiqsi-opencode-server-active-p)
    (error "OpenCode server is not running"))
  (unless tiqsi-opencode-server--session-id
    (error "No active session"))
  (let* ((server-perms (tiqsi-opencode-server--get-session-permissions))
         (client-perms tiqsi-opencode-server--permission-grants)
         (has-server (and server-perms (> (length server-perms) 0)))
         (has-client (and client-perms (> (length client-perms) 0))))
    (if (not (or has-server has-client))
        (message "No permission rules for current session (%s)"
                 tiqsi-opencode-server--session-id)
      ;; Show in REPL buffer
      (when (and tiqsi-opencode-server--repl-buffer
                 (buffer-live-p tiqsi-opencode-server--repl-buffer))
        (with-current-buffer tiqsi-opencode-server--repl-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (tiqsi-claude-repl--make-separator) "\n")
            (insert (tiqsi-claude-repl--colorize
                     (format "Permission rules for session %s:"
                             (substring tiqsi-opencode-server--session-id
                                        0 (min 18 (length tiqsi-opencode-server--session-id))))
                     'tiqsi-claude-repl-info) "\n")
            (insert (tiqsi-claude-repl--colorize
                     (format "  %-20s %-12s %-8s %s" "TOOL" "SOURCE" "ACTION" "PATTERN")
                     'tiqsi-claude-repl-status) "\n")
            (insert (tiqsi-claude-repl--colorize
                     (make-string 58 ?─)
                     'tiqsi-claude-repl-status) "\n")
            ;; Server-side rules (from session creation / config)
            (when has-server
              (dolist (rule server-perms)
                (when (hash-table-p rule)
                  (let* ((perm (gethash "permission" rule "?"))
                         (pattern (gethash "pattern" rule "*"))
                         (action (gethash "action" rule "?"))
                         (line (format "  %-20s %-12s %-8s %s"
                                       perm "config" (upcase action) pattern))
                         (face (if (equal action "allow")
                                   'tiqsi-claude-repl-success
                                 'tiqsi-claude-repl-error)))
                    (insert (tiqsi-claude-repl--colorize line face) "\n")))))
            ;; Client-side grants (from interactive prompts this session)
            (when has-client
              (dolist (grant (reverse client-perms))
                (when (hash-table-p grant)
                  (let* ((perm (gethash "permission" grant "?"))
                         (pattern (gethash "pattern" grant "*"))
                         (action (gethash "action" grant "?"))
                         (time (gethash "time" grant ""))
                         (source (if (> (length time) 0)
                                     (format "granted@%s" time)
                                   "granted"))
                         (line (format "  %-20s %-12s %-8s %s"
                                       perm source (upcase action) pattern))
                         (face (if (string-match-p "allow" action)
                                   'tiqsi-claude-repl-success
                                 'tiqsi-claude-repl-warning)))
                    (insert (tiqsi-claude-repl--colorize line face) "\n")))))
            (insert (tiqsi-claude-repl--make-separator) "\n")
            (let* ((all-perms (append (or server-perms '()) (or client-perms '())))
                   (summary (tiqsi-opencode-server--format-permission-summary all-perms)))
              (insert (tiqsi-claude-repl--colorize
                       (format "  Local mode: %s │ Total: %s"
                               tiqsi-opencode-permission-prompt summary)
                       'tiqsi-claude-repl-status) "\n\n"))
            (goto-char (point-max)))))
      (let* ((all-perms (append (or server-perms '()) (or client-perms '())))
             (summary (tiqsi-opencode-server--format-permission-summary all-perms)))
        (message "Session permissions: %s (%d rules)"
                 summary (length all-perms))))))

;; ---------------------------------------------------------------------------
;; Session browser
;; ---------------------------------------------------------------------------

(defun tiqsi-opencode-server--list-sessions ()
  "Fetch all sessions from the server.  Returns a list of hash-tables.
Each session has keys: id, slug, title, directory, time, summary, etc."
  (let ((resp (tiqsi-opencode-server--http-request "GET" "/session")))
    (cond
     ((vectorp resp) (append resp nil))      ;; JSON array → list
     ((listp resp) resp)                      ;; already a list
     (t nil))))

(defun tiqsi-opencode-server--format-session-age (created-ms)
  "Format CREATED-MS (Unix milliseconds) as a relative age string."
  (if (or (null created-ms) (= created-ms 0))
      "?"
    (let* ((secs (- (float-time) (/ (float created-ms) 1000.0)))
           (mins (/ secs 60.0))
           (hours (/ mins 60.0))
           (days (/ hours 24.0)))
      (cond
       ((< mins 1)   "just now")
       ((< mins 60)  (format "%dm ago" (floor mins)))
       ((< hours 24) (format "%dh ago" (floor hours)))
       ((< days 7)   (format "%dd ago" (floor days)))
       (t            (format "%dw ago" (floor (/ days 7.0))))))))

(defun tiqsi-opencode-server--format-session-entry (session)
  "Format a SESSION hash-table into a display string for the picker.
Returns a string like:
  ses_abc... │ My Session Title │ 2h ago │ +100/-50 (3 files)"
  (let* ((id (gethash "id" session ""))
         (title (or (gethash "title" session) (gethash "slug" session) "untitled"))
         (time-obj (gethash "time" session))
         (created (when (hash-table-p time-obj) (gethash "created" time-obj)))
         (updated (when (hash-table-p time-obj) (gethash "updated" time-obj)))
         (age (tiqsi-opencode-server--format-session-age (or updated created)))
         (summary (gethash "summary" session))
         (summary-str
          (if (hash-table-p summary)
              (let ((adds (gethash "additions" summary 0))
                    (dels (gethash "deletions" summary 0))
                    (files (gethash "files" summary 0)))
                (if (> (+ adds dels files) 0)
                    (format "+%d/-%d (%d files)" adds dels files)
                  ""))
            ""))
         (parent (gethash "parentID" session))
         (current-marker (if (equal id tiqsi-opencode-server--session-id) " *" ""))
         ;; Permission summary
         (perms-raw (gethash "permission" session))
         (perms (cond ((vectorp perms-raw) (append perms-raw nil))
                      ((listp perms-raw) perms-raw)
                      (t nil)))
         (perms-str (if perms
                        (tiqsi-opencode-server--format-permission-summary perms)
                      ""))
         ;; Truncate title to keep the line reasonable
         (title-display (if (> (length title) 50)
                            (concat (substring title 0 47) "...")
                          title)))
    (format "%-18s │ %-50s │ %8s │ %s%s%s%s"
            (substring id 0 (min 18 (length id)))
            title-display
            age
            summary-str
            (if parent " (fork)" "")
            (if (> (length perms-str) 0) (format " [%s]" perms-str) "")
            current-marker)))

(defun tiqsi-opencode-server--session-entry-to-id (entry sessions)
  "Extract the session ID from ENTRY string matched against SESSIONS."
  ;; The first 18 chars of the entry are the (possibly truncated) session ID.
  ;; Match it against the full IDs from the sessions list.
  (let ((prefix (string-trim (car (split-string entry "│")))))
    (cl-find-if (lambda (s)
                  (string-prefix-p prefix (gethash "id" s "")))
                sessions)))

;;;###autoload
(defun tiqsi-opencode-server-list-sessions ()
  "Browse and select from all OpenCode sessions on the server.
Shows a completing-read picker with session title, age, and stats.
Selecting a session switches to it in the REPL."
  (interactive)
  (unless (tiqsi-opencode-server-active-p)
    (error "OpenCode server is not running"))
  (let* ((sessions (tiqsi-opencode-server--list-sessions))
         (entries (mapcar #'tiqsi-opencode-server--format-session-entry sessions))
         (choice (completing-read
                  (format "Sessions (%d) [current: %s]: "
                          (length sessions)
                          (or tiqsi-opencode-server--session-id "none"))
                  entries nil t)))
    (when choice
      (let ((selected (tiqsi-opencode-server--session-entry-to-id
                       choice sessions)))
        (if selected
            (let ((id (gethash "id" selected))
                  (title (or (gethash "title" selected) "untitled")))
              (tiqsi-opencode-server-switch-session id)
              (message "Switched to session: %s (%s)" title id))
          (message "Could not find session for selection"))))))

;;;###autoload
(defun tiqsi-opencode-server-switch-session (session-id)
  "Switch the active server session to SESSION-ID.
Updates the REPL buffer header and resets per-session counters."
  (interactive
   (list (read-string "Session ID: " tiqsi-opencode-server--session-id)))
  (setq tiqsi-opencode-server--session-id session-id)
  ;; Reset per-session state
  (setq tiqsi-opencode-server--busy nil)
  (setq tiqsi-opencode-server--total-cost 0.0)
  (setq tiqsi-opencode-server--total-tokens 0)
  (setq tiqsi-opencode-server--message-count 0)
  (setq tiqsi-opencode-server--request-start-time nil)
  (setq tiqsi-opencode-server--output-start nil)
  (setq tiqsi-opencode-server--permission-grants nil)
  (clrhash tiqsi-opencode-server--tool-calls)
  ;; Fetch session info for the title
  (let* ((info (condition-case nil
                   (tiqsi-opencode-server--http-request
                    "GET" (format "/session/%s" session-id))
                 (error nil)))
         (title (when (hash-table-p info)
                  (or (gethash "title" info) (gethash "slug" info))))
         (messages (condition-case nil
                       (tiqsi-opencode-server--http-request
                        "GET" (format "/session/%s/message" session-id))
                     (error nil)))
         (msg-count (cond
                     ((vectorp messages) (length messages))
                     ((listp messages) (length messages))
                     (t 0))))
    ;; Update REPL buffer
    (when (and tiqsi-opencode-server--repl-buffer
               (buffer-live-p tiqsi-opencode-server--repl-buffer))
      (with-current-buffer tiqsi-opencode-server--repl-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert (tiqsi-claude-repl--make-separator) "\n")
          (insert (tiqsi-claude-repl--colorize
                   (format "⤷ Switched to session: %s" (or title session-id))
                   'tiqsi-claude-repl-info) "\n")
          (when (> msg-count 0)
            (insert (tiqsi-claude-repl--colorize
                     (format "  %d messages in history" msg-count)
                     'tiqsi-claude-repl-status) "\n"))
          ;; Render the last few messages as context
          (when (and messages (> msg-count 0))
            (tiqsi-opencode-server--render-session-history messages))
          (insert (tiqsi-claude-repl--make-separator) "\n\n")
          (insert (tiqsi-claude-repl--format-prompt "oc"))
          (goto-char (point-max)))))))

(defun tiqsi-opencode-server--render-session-history (messages)
  "Render the last few MESSAGES from session history in the REPL.
MESSAGES is a vector or list of message objects from the server API."
  (let* ((msg-list (if (vectorp messages) (append messages nil) messages))
         ;; Show last 6 messages (3 exchanges) as context
         (recent (last msg-list (min 6 (length msg-list)))))
    (insert (tiqsi-claude-repl--colorize
             "  Recent history:" 'tiqsi-claude-repl-status) "\n")
    (dolist (msg recent)
      (let* ((info (gethash "info" msg))
             (role (when (hash-table-p info) (gethash "role" info)))
             (parts (gethash "parts" msg))
             (parts-list (if (vectorp parts) (append parts nil) parts))
             ;; Find first text part
             (text-part (cl-find-if
                         (lambda (p)
                           (and (hash-table-p p)
                                (equal (gethash "type" p) "text")))
                         parts-list))
             (text (when text-part (gethash "text" text-part)))
             ;; Truncate for preview
             (preview (when (and text (> (length text) 0))
                        (let ((clean (replace-regexp-in-string "[\n\r]+" " " text)))
                          (if (> (length clean) 100)
                              (concat (substring clean 0 97) "...")
                            clean)))))
        (when preview
          (insert "  "
                  (tiqsi-claude-repl--colorize
                   (format "%s: " (or role "?"))
                   (if (equal role "user")
                       'tiqsi-claude-repl-prompt
                     'tiqsi-claude-repl-info))
                  (tiqsi-claude-repl--colorize
                   preview 'tiqsi-claude-repl-status)
                  "\n"))))))

;;;###autoload
(defun tiqsi-opencode-server-delete-session (session-id)
  "Delete SESSION-ID from the server.  Prompts for confirmation."
  (interactive
   (let* ((sessions (tiqsi-opencode-server--list-sessions))
          (entries (mapcar #'tiqsi-opencode-server--format-session-entry sessions))
          (choice (completing-read "Delete session: " entries nil t))
          (selected (tiqsi-opencode-server--session-entry-to-id choice sessions)))
     (if selected
         (list (gethash "id" selected))
       (error "No session selected"))))
  (when (yes-or-no-p (format "Delete session %s? " session-id))
    (let ((resp (tiqsi-opencode-server--http-request
                 "DELETE" (format "/session/%s" session-id))))
      ;; If we deleted the current session, clear it
      (when (equal session-id tiqsi-opencode-server--session-id)
        (setq tiqsi-opencode-server--session-id nil)
        (message "Deleted current session — use 'l' to pick a new one"))
      (message "Session %s deleted" session-id))))

;;;###autoload
(defun tiqsi-opencode-server-new-session (&optional title)
  "Create a new session on the server and switch to it.
Optionally provide a TITLE."
  (interactive (list (read-string "Session title (empty for default): ")))
  (unless (and tiqsi-opencode-server--process
               (process-live-p tiqsi-opencode-server--process))
    (error "OpenCode server is not running"))
  (let* ((project-name (file-name-nondirectory
                        (directory-file-name
                         (tiqsi-claude-repl--get-project-root))))
         (session-title (if (and title (> (length (string-trim title)) 0))
                            title
                          (format "emacs: %s" project-name)))
         (id (tiqsi-opencode-server--create-session session-title)))
    (when id
      ;; Reset per-session state
      (setq tiqsi-opencode-server--busy nil)
      (setq tiqsi-opencode-server--total-cost 0.0)
      (setq tiqsi-opencode-server--total-tokens 0)
      (setq tiqsi-opencode-server--message-count 0)
      (setq tiqsi-opencode-server--permission-grants nil)
      (clrhash tiqsi-opencode-server--tool-calls)
      ;; Update REPL buffer
      (when (and tiqsi-opencode-server--repl-buffer
                 (buffer-live-p tiqsi-opencode-server--repl-buffer))
        (with-current-buffer tiqsi-opencode-server--repl-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert (tiqsi-claude-repl--make-separator) "\n")
            (insert (tiqsi-claude-repl--colorize
                     (format "✦ New session: %s (%s)" session-title id)
                     'tiqsi-claude-repl-success) "\n")
            (insert (tiqsi-claude-repl--make-separator) "\n\n")
            (insert (tiqsi-claude-repl--format-prompt "oc"))
            (goto-char (point-max)))))
      (message "Created new session: %s (%s)" session-title id))))

;; ---------------------------------------------------------------------------
;; Tabulated session browser
;; ---------------------------------------------------------------------------

(defvar tiqsi-opencode-session-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'tiqsi-opencode-session-browser--switch)
    (define-key map (kbd "o")   #'tiqsi-opencode-session-browser--switch)
    (define-key map (kbd "d")   #'tiqsi-opencode-session-browser--delete)
    (define-key map (kbd "D")   #'tiqsi-opencode-session-browser--delete)
    (define-key map (kbd "n")   #'tiqsi-opencode-session-browser--new)
    (define-key map (kbd "g")   #'tiqsi-opencode-session-browser--refresh)
    (define-key map (kbd "p")   #'tiqsi-opencode-session-browser--perms)
    (define-key map (kbd "q")   #'quit-window)
    map)
  "Keymap for the OpenCode session browser.")

(define-derived-mode tiqsi-opencode-session-browser-mode tabulated-list-mode
  "OC-Sessions"
  "Major mode for browsing OpenCode sessions.
\\<tiqsi-opencode-session-browser-mode-map>
\\[tiqsi-opencode-session-browser--switch]  Switch to session
\\[tiqsi-opencode-session-browser--delete]  Delete session
\\[tiqsi-opencode-session-browser--new]     New session
\\[tiqsi-opencode-session-browser--refresh] Refresh list
\\[tiqsi-opencode-session-browser--perms]   Show permissions
\\[quit-window]                             Quit"
  (setq tabulated-list-format
        [("" 2 nil)                          ; current marker
         ("Title" 40 t)
         ("Age" 10 t)
         ("Changes" 20 t)
         ("Perms" 16 t)
         ("ID" 22 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key '("Age" . nil))
  (tabulated-list-init-header))

(defun tiqsi-opencode-session-browser--entries ()
  "Build tabulated-list entries from server sessions."
  (let ((sessions (tiqsi-opencode-server--list-sessions)))
    (mapcar
     (lambda (session)
       (let* ((id (gethash "id" session ""))
              (title (or (gethash "title" session)
                         (gethash "slug" session) "untitled"))
              (time-obj (gethash "time" session))
              (created (when (hash-table-p time-obj) (gethash "created" time-obj)))
              (updated (when (hash-table-p time-obj) (gethash "updated" time-obj)))
              (age (tiqsi-opencode-server--format-session-age (or updated created)))
              (summary (gethash "summary" session))
              (summary-str
               (if (hash-table-p summary)
                   (let ((adds (gethash "additions" summary 0))
                         (dels (gethash "deletions" summary 0))
                         (files (gethash "files" summary 0)))
                     (if (> (+ adds dels files) 0)
                         (format "+%d/-%d (%d files)" adds dels files)
                       ""))
                 ""))
              (parent (gethash "parentID" session))
              (perms-raw (gethash "permission" session))
              (perms (cond ((vectorp perms-raw) (append perms-raw nil))
                           ((listp perms-raw) perms-raw)
                           (t nil)))
              (perms-str (if perms
                             (tiqsi-opencode-server--format-permission-summary perms)
                           ""))
              (current-p (equal id tiqsi-opencode-server--session-id))
              (marker (if current-p "*" ""))
              (title-display (concat
                              (if (> (length title) 38)
                                  (concat (substring title 0 35) "...")
                                title)
                              (if parent " (fork)" ""))))
         (list id (vector marker title-display age summary-str perms-str
                          (substring id 0 (min 22 (length id)))))))
     sessions)))

(defun tiqsi-opencode-session-browser--get-id ()
  "Get the session ID at point."
  (tabulated-list-get-id))

(defun tiqsi-opencode-session-browser--switch ()
  "Switch to the session at point."
  (interactive)
  (let ((id (tiqsi-opencode-session-browser--get-id)))
    (when id
      (tiqsi-opencode-server-switch-session id)
      (quit-window)
      (message "Switched to session: %s" id))))

(defun tiqsi-opencode-session-browser--delete ()
  "Delete the session at point."
  (interactive)
  (let ((id (tiqsi-opencode-session-browser--get-id)))
    (when (and id (yes-or-no-p (format "Delete session %s? " id)))
      (tiqsi-opencode-server--http-request
       "DELETE" (format "/session/%s" id))
      (when (equal id tiqsi-opencode-server--session-id)
        (setq tiqsi-opencode-server--session-id nil))
      (tiqsi-opencode-session-browser--refresh)
      (message "Deleted session %s" id))))

(defun tiqsi-opencode-session-browser--new ()
  "Create a new session from the browser."
  (interactive)
  (let ((title (read-string "Session title: ")))
    (tiqsi-opencode-server-new-session title)
    (tiqsi-opencode-session-browser--refresh)))

(defun tiqsi-opencode-session-browser--refresh ()
  "Refresh the session list."
  (interactive)
  (setq tabulated-list-entries (tiqsi-opencode-session-browser--entries))
  (tabulated-list-print t)
  (message "Refreshed (%d sessions)" (length tabulated-list-entries)))

(defun tiqsi-opencode-session-browser--perms ()
  "Show permissions for the session at point."
  (interactive)
  (let* ((id (tiqsi-opencode-session-browser--get-id))
         (perms (when id (tiqsi-opencode-server--get-session-permissions id))))
    (if (and perms (> (length perms) 0))
        (message "%s" (mapconcat
                       (lambda (rule)
                         (if (hash-table-p rule)
                             (format "%s: %s (%s)"
                                     (gethash "permission" rule "?")
                                     (gethash "action" rule "?")
                                     (gethash "pattern" rule "*"))
                           ""))
                       perms " │ "))
      (message "No permission rules for session %s" (or id "?")))))

;;;###autoload
(defun tiqsi-opencode-session-browser ()
  "Open the OpenCode session browser in a dedicated buffer.
Provides a tabulated view of all sessions with keybindings for
switching, deleting, and creating sessions."
  (interactive)
  (unless (tiqsi-opencode-server-active-p)
    (error "OpenCode server is not running"))
  (let ((buf (get-buffer-create "*OpenCode Sessions*")))
    (with-current-buffer buf
      (tiqsi-opencode-session-browser-mode)
      (setq tabulated-list-entries (tiqsi-opencode-session-browser--entries))
      (tabulated-list-print t))
    (pop-to-buffer buf)
    (message "RET=switch  d=delete  n=new  p=perms  g=refresh  q=quit")))

(provide 'tiqsi-claude-repl-opencode-server)

;;; tiqsi-claude-repl-opencode-server.el ends here
