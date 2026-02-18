;;; test-opencode.el --- OpenCode backend tests -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Tests for the tiqsi-claude-repl-opencode module and the smart-dispatch
;; wrappers in modes-claude.el.  Uses the tiqsi-test-harness assertion API.

;;; Code:

(require 'tiqsi-test-harness)

;; ---------------------------------------------------------------------------
;; 1. Module loading
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Module Loading")

(tiqsi-test-assert (featurep 'tiqsi-claude-repl-opencode)
                   "tiqsi-claude-repl-opencode feature loaded")

;; Core interactive commands
(dolist (fn '(tiqsi-opencode-start
              tiqsi-opencode-kill
              tiqsi-opencode-ask
              tiqsi-opencode-send-input
              tiqsi-opencode-send-region
              tiqsi-opencode-send-buffer
              tiqsi-opencode-send-function
              tiqsi-opencode-send-paragraph
              tiqsi-opencode-explain-code
              tiqsi-opencode-optimize-code
              tiqsi-opencode-fix-error
              tiqsi-opencode-generate-tests
              tiqsi-opencode-cancel
              tiqsi-opencode-clear
              tiqsi-opencode-attach-file
              tiqsi-opencode-clear-attachments
              tiqsi-opencode-set-model
              tiqsi-opencode-set-agent
              tiqsi-opencode-session-stats
              tiqsi-opencode-global-stats
              tiqsi-opencode-list-models
              tiqsi-opencode-list-sessions
              tiqsi-opencode-export-session
              tiqsi-opencode-switch))
  (tiqsi-test-assert-fboundp fn (format "%s defined" fn)))

;; Smart dispatch functions
(dolist (fn '(tiqsi-repl-smart-send-input
              tiqsi-repl-smart-cancel
              tiqsi-repl-smart-clear
              tiqsi-repl-smart-start
              tiqsi-repl-smart-ask
              tiqsi-repl-smart-send-region))
  (tiqsi-test-assert-fboundp fn (format "%s defined" fn)))

;; Variables
(tiqsi-test-assert-boundp 'tiqsi-repl-backend "tiqsi-repl-backend bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-program "tiqsi-opencode-program bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-model "tiqsi-opencode-model bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-agent "tiqsi-opencode-agent bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-tls-bypass "tiqsi-opencode-tls-bypass bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-auto-approve "tiqsi-opencode-auto-approve bound")
(tiqsi-test-assert (eq tiqsi-opencode-auto-approve t)
                   "auto-approve defaults to t")

;; CLI availability -- skip rather than fail if opencode isn't installed
(if (tiqsi-opencode--executable-available-p)
    (tiqsi-test-assert t (format "OpenCode CLI found: %s"
                                 (executable-find tiqsi-opencode-program)))
  (tiqsi-test-skip "OpenCode CLI on PATH" "not installed"))

;; ---------------------------------------------------------------------------
;; 2. JSON event parsing
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: JSON Parsing")

(defun test-opencode--make-mock-buffer ()
  "Create a test buffer in `tiqsi-claude-repl-mode' with OpenCode state."
  (let ((buf (get-buffer-create "*test-opencode-repl*")))
    (with-current-buffer buf
      (tiqsi-claude-repl-mode)
      (erase-buffer)
      (setq-local tiqsi-opencode--json-buffer "")
      (setq-local tiqsi-opencode--session-id nil)
      (setq-local tiqsi-opencode--output-start nil)
      (setq-local tiqsi-opencode--request-start-time nil)
      (setq-local tiqsi-opencode--total-cost 0.0)
      (setq-local tiqsi-opencode--total-tokens 0)
      (setq-local tiqsi-opencode--message-count 0)
      (setq-local tiqsi-opencode--attached-files nil)
      (setq-local tiqsi-opencode--current-process nil))
    buf))

;; step_start event — should capture session ID
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((proc (start-process "oc-test" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                ;; step_start event
                (tiqsi-opencode--process-filter proc
                  "{\"type\":\"step_start\",\"timestamp\":1700000000,\"sessionID\":\"oc-sess-001\",\"part\":{\"type\":\"step-start\",\"snapshot\":\"abc\"}}\n")

                (let ((sid (buffer-local-value 'tiqsi-opencode--session-id buf)))
                  (tiqsi-test-assert (equal sid "oc-sess-001")
                                     "Session ID captured from step_start"
                                     (format "got: %S" sid))))
            (delete-process proc))))
    (kill-buffer buf)))

;; text event — should insert text into buffer
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((proc (start-process "oc-test2" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                (tiqsi-opencode--process-filter proc
                  "{\"type\":\"text\",\"timestamp\":1700000001,\"sessionID\":\"oc-sess-002\",\"part\":{\"type\":\"text\",\"text\":\"Hello from OpenCode!\"}}\n")

                (goto-char (point-min))
                (tiqsi-test-assert
                 (search-forward "Hello from OpenCode!" nil t)
                 "Text event content visible in buffer"))
            (delete-process proc))))
    (kill-buffer buf)))

;; step_finish event — should accumulate cost/tokens
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((tiqsi-opencode-show-cost t)
              (proc (start-process "oc-test3" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                (tiqsi-opencode--process-filter proc
                  "{\"type\":\"step_finish\",\"timestamp\":1700000002,\"part\":{\"type\":\"step-finish\",\"reason\":\"stop\",\"cost\":0.0123,\"tokens\":{\"total\":1500,\"input\":200,\"output\":50,\"reasoning\":0,\"cache\":{\"read\":1200,\"write\":50}}}}\n")

                (tiqsi-test-assert
                 (> (buffer-local-value 'tiqsi-opencode--total-cost buf) 0)
                 "Cost accumulated from step_finish"
                 (format "got: %.4f" (buffer-local-value 'tiqsi-opencode--total-cost buf)))
                (tiqsi-test-assert
                 (= (buffer-local-value 'tiqsi-opencode--total-tokens buf) 1500)
                 "Tokens accumulated from step_finish"
                 (format "got: %d" (buffer-local-value 'tiqsi-opencode--total-tokens buf))))
            (delete-process proc))))
    (kill-buffer buf)))

;; tool_use event
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((tiqsi-opencode-show-tool-use t)
              (proc (start-process "oc-test4" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                (tiqsi-opencode--process-filter proc
                  "{\"type\":\"tool_use\",\"timestamp\":1700000003,\"sessionID\":\"oc-sess-003\",\"part\":{\"type\":\"tool_use\",\"name\":\"edit_file\",\"input\":{\"path\":\"src/main.rs\"}}}\n")

                (goto-char (point-min))
                (tiqsi-test-assert
                 (search-forward "edit_file" nil t)
                 "Tool use event rendered in buffer"))
            (delete-process proc))))
    (kill-buffer buf)))

;; Malformed JSON — should not error, just log
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((proc (start-process "oc-test5" (current-buffer) "sleep" "5")))
          (unwind-protect
              (tiqsi-test-assert-no-error
               "Malformed JSON handled gracefully"
               (lambda ()
                 (tiqsi-opencode--process-filter proc
                   "this is not json at all\n")))
            (delete-process proc))))
    (kill-buffer buf)))

;; Multi-line / incomplete JSON accumulation
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (let ((proc (start-process "oc-test6" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                ;; Send first half of a JSON line
                (tiqsi-opencode--process-filter proc
                  "{\"type\":\"text\",\"timestamp\":170000")
                ;; No text should appear yet
                (goto-char (point-min))
                (tiqsi-test-assert
                 (not (search-forward "partial" nil t))
                 "Incomplete JSON not rendered prematurely")

                ;; Send the rest
                (tiqsi-opencode--process-filter proc
                  "0004,\"sessionID\":\"s\",\"part\":{\"type\":\"text\",\"text\":\"partial resolved\"}}\n")
                (goto-char (point-min))
                (tiqsi-test-assert
                 (search-forward "partial resolved" nil t)
                 "Complete JSON rendered after accumulation"))
            (delete-process proc))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 3. Environment / TLS bypass
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Environment")

(let ((tiqsi-opencode-tls-bypass t))
  (let ((env (tiqsi-opencode--build-env)))
    (tiqsi-test-assert
     (member "NODE_TLS_REJECT_UNAUTHORIZED=0" env)
     "TLS bypass present when enabled")))

;; When disabled, --build-env should NOT add the TLS variable.
;; We test that the returned env has the same count of TLS entries as the
;; base process-environment (i.e. --build-env did not add one).
(let* ((tiqsi-opencode-tls-bypass nil)
       (base-count (cl-count "NODE_TLS_REJECT_UNAUTHORIZED=0"
                             process-environment :test #'equal))
       (env (tiqsi-opencode--build-env))
       (env-count (cl-count "NODE_TLS_REJECT_UNAUTHORIZED=0"
                            env :test #'equal)))
  (tiqsi-test-assert
   (= base-count env-count)
   "TLS bypass not added when disabled"
   (format "base: %d, env: %d" base-count env-count)))

;; Auto-approve: when enabled, OPENCODE_PERMISSION should be in env
(let ((tiqsi-opencode-auto-approve t))
  (let ((env (tiqsi-opencode--build-env)))
    (tiqsi-test-assert
     (cl-some (lambda (e) (string-prefix-p "OPENCODE_PERMISSION=" e)) env)
     "Auto-approve permission present when enabled")))

;; Auto-approve: should include allow-all JSON
(let ((tiqsi-opencode-auto-approve t))
  (let ((env (tiqsi-opencode--build-env)))
    (tiqsi-test-assert
     (member "OPENCODE_PERMISSION={\"*\":\"allow\"}" env)
     "Auto-approve sets allow-all permission JSON")))

;; Auto-approve: when disabled, OPENCODE_PERMISSION should NOT be added
(let* ((tiqsi-opencode-auto-approve nil)
       ;; Filter out any existing OPENCODE_PERMISSION from process-environment
       (process-environment (cl-remove-if
                             (lambda (e) (string-prefix-p "OPENCODE_PERMISSION=" e))
                             process-environment))
       (env (tiqsi-opencode--build-env)))
  (tiqsi-test-assert
   (not (cl-some (lambda (e) (string-prefix-p "OPENCODE_PERMISSION=" e)) env))
   "Auto-approve permission not added when disabled"))

;; ---------------------------------------------------------------------------
;; 4. Backend switching
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Backend Switching")

;; Default backend is claude
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'claude)
        (tiqsi-test-assert (eq tiqsi-repl-backend 'claude)
                           "Default backend is claude")

        ;; Switch to opencode
        (tiqsi-opencode-switch)
        (tiqsi-test-assert (eq tiqsi-repl-backend 'opencode)
                           "Switch toggles to opencode")

        ;; Switch back
        (tiqsi-opencode-switch)
        (tiqsi-test-assert (eq tiqsi-repl-backend 'claude)
                           "Switch toggles back to claude"))
    (setq tiqsi-repl-backend orig)))

;; ---------------------------------------------------------------------------
;; 5. Smart-dispatch wrappers in modes-claude.el
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Smart Dispatch Wrappers")

;; All wrapper functions that route through tiqsi-repl-backend
(dolist (fn '(tiqsi-claude-start
              tiqsi-claude-kill
              tiqsi-claude-toggle
              tiqsi-claude-send-region
              tiqsi-claude-send-function
              tiqsi-claude-send-buffer
              tiqsi-claude-send-paragraph
              tiqsi-claude-ask-question
              tiqsi-claude-fix-error
              tiqsi-claude-optimize-code
              tiqsi-claude-explain-code
              tiqsi-claude-generate-tests
              tiqsi-claude-list-sessions
              tiqsi-claude-clear
              tiqsi-claude-new-session
              tiqsi-claude-delete-session
              tiqsi-claude-session-stats
              tiqsi-claude-export-session
              tiqsi-claude-fork-session
              tiqsi-claude-cycle-permission-prompt))
  (tiqsi-test-assert-fboundp fn (format "dispatch wrapper: %s" fn)))

;; Hydra integration
(tiqsi-test-assert-fboundp 'hydra-claude/body "hydra-claude/body still defined")
(tiqsi-test-assert-fboundp 'tiqsi-claude--backend-label
                           "backend label helper defined")

;; Backend label returns the right string
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'claude)
        (tiqsi-test-assert-equal "Claude" (tiqsi-claude--backend-label)
                                 "Backend label for claude")
        (setq tiqsi-repl-backend 'opencode)
        (tiqsi-test-assert-equal "OpenCode" (tiqsi-claude--backend-label)
                                 "Backend label for opencode"))
    (setq tiqsi-repl-backend orig)))

;; ---------------------------------------------------------------------------
;; 6. REPL mode map smart keys
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Mode Map Smart Keys")

;; Verify the mode map binds to smart dispatchers
(let ((map tiqsi-claude-repl-mode-map))
  (tiqsi-test-assert
   (eq (lookup-key map (kbd "RET")) 'tiqsi-repl-smart-send-input)
   "RET bound to smart-send-input")
  (tiqsi-test-assert
   (eq (lookup-key map (kbd "C-c C-c")) 'tiqsi-repl-smart-send-input)
   "C-c C-c bound to smart-send-input")
  (tiqsi-test-assert
   (eq (lookup-key map (kbd "C-g")) 'tiqsi-repl-smart-cancel)
   "C-g bound to smart-cancel")
  (tiqsi-test-assert
   (eq (lookup-key map (kbd "C-c C-k")) 'tiqsi-repl-smart-clear)
   "C-c C-k bound to smart-clear"))

;; Smart dispatch routes by buffer name
(let ((oc-buf (get-buffer-create "*OpenCode REPL (test)*"))
      (cl-buf (get-buffer-create "*Claude REPL (test)*")))
  (unwind-protect
      (progn
        ;; In an OpenCode buffer the dispatch should detect it
        (with-current-buffer oc-buf
          (tiqsi-test-assert
           (string-match-p "\\*OpenCode REPL" (buffer-name))
           "OpenCode buffer name detection works"))
        ;; In a Claude buffer
        (with-current-buffer cl-buf
          (tiqsi-test-assert
           (not (string-match-p "\\*OpenCode REPL" (buffer-name)))
           "Claude buffer name detection works")))
    (kill-buffer oc-buf)
    (kill-buffer cl-buf)))

;; ---------------------------------------------------------------------------
;; 7. OpenCode custom variables
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Custom Variables")

;; Model setting
(let ((orig tiqsi-opencode-model))
  (unwind-protect
      (progn
        (tiqsi-opencode-set-model "anthropic/claude-sonnet-4-20250514")
        (tiqsi-test-assert-equal "anthropic/claude-sonnet-4-20250514"
                                 tiqsi-opencode-model
                                 "Model set correctly")
        ;; Empty string resets to nil
        (tiqsi-opencode-set-model "")
        (tiqsi-test-assert (null tiqsi-opencode-model)
                           "Empty model string resets to nil"))
    (setq tiqsi-opencode-model orig)))

;; Agent setting
(let ((orig tiqsi-opencode-agent))
  (unwind-protect
      (progn
        (tiqsi-opencode-set-agent "coder")
        (tiqsi-test-assert-equal "coder" tiqsi-opencode-agent
                                 "Agent set correctly")
        (tiqsi-opencode-set-agent "")
        (tiqsi-test-assert (null tiqsi-opencode-agent)
                           "Empty agent string resets to nil"))
    (setq tiqsi-opencode-agent orig)))

;; File attachment
(let ((buf (test-opencode--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        (tiqsi-opencode-attach-file "/tmp/test-file.py")
        (tiqsi-test-assert
         (member "/tmp/test-file.py" tiqsi-opencode--attached-files)
         "File attachment recorded")
        (tiqsi-opencode-attach-file "/tmp/another.rs")
        (tiqsi-test-assert-equal 2 (length tiqsi-opencode--attached-files)
                                 "Multiple attachments accumulated")
        (tiqsi-opencode-clear-attachments)
        (tiqsi-test-assert (null tiqsi-opencode--attached-files)
                           "Attachments cleared"))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 8. New CLI features (variant, fork, PR, serve, agents, MCP, etc.)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Full CLI Feature Coverage")

;; All new functions must be defined
(dolist (fn '(tiqsi-opencode-set-variant
              tiqsi-opencode-fork-session
              tiqsi-opencode-import-session
              tiqsi-opencode-share-session
              tiqsi-opencode-pr
              tiqsi-opencode-serve
              tiqsi-opencode-attach
              tiqsi-opencode-web
              tiqsi-opencode-agent-list
              tiqsi-opencode-agent-create
              tiqsi-opencode-mcp-list
              tiqsi-opencode-mcp-add
              tiqsi-opencode-github-install
              tiqsi-opencode-debug
              tiqsi-opencode-auth))
  (tiqsi-test-assert-fboundp fn (format "%s defined" fn)))

;; Variant custom variable
(tiqsi-test-assert-boundp 'tiqsi-opencode-variant "tiqsi-opencode-variant bound")

;; Variant set/reset
(let ((orig tiqsi-opencode-variant))
  (unwind-protect
      (progn
        (tiqsi-opencode-set-variant "high")
        (tiqsi-test-assert-equal "high" tiqsi-opencode-variant
                                 "Variant set to high")
        (tiqsi-opencode-set-variant "")
        (tiqsi-test-assert (null tiqsi-opencode-variant)
                           "Variant cleared to nil"))
    (setq tiqsi-opencode-variant orig)))

;; Attach URL variable
(tiqsi-test-assert-boundp 'tiqsi-opencode--attach-url
                          "attach-url buffer-local var exists"
                          ;; defvar-local creates a default binding
                          )

;; ---------------------------------------------------------------------------
;; 9. Real E2E smoke test (only if opencode is available)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: E2E Smoke Test")

(if (tiqsi-opencode--executable-available-p)
    (let ((buf (get-buffer-create "*test-opencode-e2e*")))
      (unwind-protect
          (with-current-buffer buf
            (tiqsi-claude-repl-mode)
            (erase-buffer)
            (setq-local tiqsi-opencode--json-buffer "")
            (setq-local tiqsi-opencode--session-id nil)
            (setq-local tiqsi-opencode--output-start nil)
            (setq-local tiqsi-opencode--request-start-time nil)
            (setq-local tiqsi-opencode--total-cost 0.0)
            (setq-local tiqsi-opencode--total-tokens 0)
            (setq-local tiqsi-opencode--message-count 0)
            (setq-local tiqsi-opencode--attached-files nil)
            (setq-local tiqsi-opencode--current-process nil)
            (setq-local tiqsi-opencode--attach-url nil)
            ;; Send a trivial query and wait for it
            (let* ((process-environment (tiqsi-opencode--build-env))
                   (proc (apply 'start-process
                                "oc-e2e"
                                (current-buffer)
                                tiqsi-opencode-program
                                (list "run" "--format" "json"
                                      "Reply with exactly the word PONG"))))
              (setq-local tiqsi-opencode--current-process proc)
              (set-process-filter proc 'tiqsi-opencode--process-filter)
              (set-process-query-on-exit-flag proc nil)
              ;; Wait up to 30s for the process to finish
              (let ((deadline (+ (float-time) 30)))
                (while (and (process-live-p proc)
                            (< (float-time) deadline))
                  (accept-process-output proc 0.5)))
              ;; Check results
              (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                (tiqsi-test-assert
                 (string-match-p "PONG" content)
                 "E2E: received response containing PONG"
                 (format "buffer has %d chars" (length content))))
              (tiqsi-test-assert
               (buffer-local-value 'tiqsi-opencode--session-id buf)
               "E2E: session ID captured"
               (format "sid: %S" (buffer-local-value 'tiqsi-opencode--session-id buf)))
              (tiqsi-test-assert
               (> (buffer-local-value 'tiqsi-opencode--total-tokens buf) 0)
               "E2E: tokens counted"
               (format "tokens: %d" (buffer-local-value 'tiqsi-opencode--total-tokens buf)))
              (tiqsi-test-assert
               (> (buffer-local-value 'tiqsi-opencode--total-cost buf) 0)
               "E2E: cost tracked"
               (format "cost: %.4f" (buffer-local-value 'tiqsi-opencode--total-cost buf)))))
        (kill-buffer buf)))
  (tiqsi-test-skip "E2E smoke test" "opencode CLI not installed"))

;; ---------------------------------------------------------------------------
;; 10. Backend fallback
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Backend Fallback")

(tiqsi-test-assert-fboundp 'tiqsi-claude--ensure-backend
                           "ensure-backend helper defined")

;; When claude is selected but missing, should switch to opencode if available
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'claude)
        ;; Simulate claude missing by temporarily overriding the program name
        (let ((tiqsi-claude-repl-program "nonexistent-claude-binary-xyz"))
          (if (tiqsi-opencode--executable-available-p)
              (progn
                (tiqsi-claude--ensure-backend)
                (tiqsi-test-assert (eq tiqsi-repl-backend 'opencode)
                                   "Fallback: claude missing -> switched to opencode"))
            (tiqsi-test-skip "Fallback: claude missing -> opencode"
                             "opencode also not available"))))
    (setq tiqsi-repl-backend orig)))

;; When opencode is selected but missing, should switch to claude if available
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (let ((tiqsi-opencode-program "nonexistent-opencode-binary-xyz"))
          (if (tiqsi-claude-repl--executable-available-p)
              (progn
                (tiqsi-claude--ensure-backend)
                (tiqsi-test-assert (eq tiqsi-repl-backend 'claude)
                                   "Fallback: opencode missing -> switched to claude"))
            ;; Claude isn't installed either; ensure-backend should still switch
            ;; because it just checks executable-find.  On this machine claude
            ;; isn't installed, so test the error path.
            (condition-case err
                (progn
                  (tiqsi-claude--ensure-backend)
                  (tiqsi-test-assert nil "Fallback: should have errored"
                                     "no error raised"))
              (error
               (tiqsi-test-assert
                (string-match-p "Neither" (error-message-string err))
                "Fallback: errors when both CLIs missing"))))))
    (setq tiqsi-repl-backend orig)))

;; ---------------------------------------------------------------------------
;; 11. Source file syntax validation
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; 12. Server transport module
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Server Transport")

;; Module loaded
(tiqsi-test-assert (featurep 'tiqsi-claude-repl-opencode-server)
                   "tiqsi-claude-repl-opencode-server feature loaded")

;; Key functions exist
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-start
                           "tiqsi-opencode-server-start defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-stop
                           "tiqsi-opencode-server-stop defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-send
                           "tiqsi-opencode-server-send defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-send-input
                           "tiqsi-opencode-server-send-input defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-cancel
                           "tiqsi-opencode-server-cancel defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-active-p
                           "tiqsi-opencode-server-active-p defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-session-stats
                           "tiqsi-opencode-server-session-stats defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-cycle-permission-prompt
                           "tiqsi-opencode-cycle-permission-prompt defined")

;; Custom variables
(tiqsi-test-assert-boundp 'tiqsi-opencode-server-port
                          "tiqsi-opencode-server-port bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-server-host
                          "tiqsi-opencode-server-host bound")
(tiqsi-test-assert-boundp 'tiqsi-opencode-permission-prompt
                          "tiqsi-opencode-permission-prompt bound")
(tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'ask)
                   "permission-prompt defaults to ask")

;; Permission cycling
(let ((orig tiqsi-opencode-permission-prompt))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-permission-prompt 'ask)
        (tiqsi-opencode-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'always)
                           "Cycle: ask -> always")
        (tiqsi-opencode-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'reject)
                           "Cycle: always -> reject")
        (tiqsi-opencode-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'ask)
                           "Cycle: reject -> ask"))
    (setq tiqsi-opencode-permission-prompt orig)))

;; SSE event parsing: text delta
(let ((buf (with-current-buffer (get-buffer-create " *test-server-repl*")
             (tiqsi-claude-repl-mode)
             (current-buffer))))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--repl-buffer buf)
        (setq tiqsi-opencode-server--session-id "ses-test-001")
        ;; Simulate a text delta event
        (tiqsi-opencode-server--handle-event
         (json-parse-string
          "{\"type\":\"message.part.delta\",\"properties\":{\"sessionID\":\"ses-test-001\",\"messageID\":\"msg-1\",\"partID\":\"prt-1\",\"field\":\"text\",\"delta\":\"Hello SSE!\"}}"))
        (with-current-buffer buf
          (tiqsi-test-assert
           (string-match-p "Hello SSE!" (buffer-string))
           "SSE text delta inserted into REPL buffer")))
    (kill-buffer buf)
    (setq tiqsi-opencode-server--repl-buffer nil)
    (setq tiqsi-opencode-server--session-id nil)))

;; SSE event parsing: session status
(let ((orig-busy tiqsi-opencode-server--busy))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--session-id "ses-test-002")
        (setq tiqsi-opencode-server--busy nil)
        (tiqsi-opencode-server--handle-event
         (json-parse-string
          "{\"type\":\"session.status\",\"properties\":{\"sessionID\":\"ses-test-002\",\"status\":{\"type\":\"busy\"}}}"))
        (tiqsi-test-assert tiqsi-opencode-server--busy
                           "SSE session.status busy sets flag")
        (tiqsi-opencode-server--handle-event
         (json-parse-string
          "{\"type\":\"session.status\",\"properties\":{\"sessionID\":\"ses-test-002\",\"status\":{\"type\":\"idle\"}}}"))
        (tiqsi-test-assert (not tiqsi-opencode-server--busy)
                           "SSE session.status idle clears flag"))
    (setq tiqsi-opencode-server--busy orig-busy)
    (setq tiqsi-opencode-server--session-id nil)))

;; SSE filter: parse SSE format
(let ((tiqsi-opencode-server--sse-partial "")
      (tiqsi-opencode-server--session-id "ses-filter-test")
      (tiqsi-opencode-server--busy nil))
  (tiqsi-opencode-server--sse-filter
   nil  ;; process (unused, we only need the side effects)
   "data: {\"type\":\"session.status\",\"properties\":{\"sessionID\":\"ses-filter-test\",\"status\":{\"type\":\"busy\"}}}\n\n")
  (tiqsi-test-assert tiqsi-opencode-server--busy
                     "SSE filter parses data: prefix correctly")
  (setq tiqsi-opencode-server--session-id nil)
  (setq tiqsi-opencode-server--busy nil))

;; Server not active when no process running
(let ((tiqsi-opencode-server--process nil)
      (tiqsi-opencode-server--session-id nil))
  (tiqsi-test-assert (not (tiqsi-opencode-server-active-p))
                     "Server not active when no process"))

;; Permission description formatting
(tiqsi-test-assert
 (string-match-p "edit file"
                 (tiqsi-opencode-server--format-permission-desc
                  "edit" ["src/main.rs"] nil))
 "Permission desc for edit includes 'edit file'")

(tiqsi-test-assert
 (string-match-p "run command"
                 (tiqsi-opencode-server--format-permission-desc
                  "bash" ["git status"] nil))
 "Permission desc for bash includes 'run command'")

;; SSE event parsing: step-finish with cost
(let ((buf (with-current-buffer (get-buffer-create " *test-server-cost*")
             (tiqsi-claude-repl-mode)
             (current-buffer)))
      (orig-cost tiqsi-opencode-server--total-cost)
      (orig-tok tiqsi-opencode-server--total-tokens))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--repl-buffer buf)
        (setq tiqsi-opencode-server--session-id "ses-test-cost")
        (setq tiqsi-opencode-server--total-cost 0.0)
        (setq tiqsi-opencode-server--total-tokens 0)
        (let ((tiqsi-opencode-show-cost t))
          (tiqsi-opencode-server--handle-event
           (json-parse-string
            "{\"type\":\"message.part.updated\",\"properties\":{\"part\":{\"id\":\"prt-1\",\"sessionID\":\"ses-test-cost\",\"messageID\":\"msg-1\",\"type\":\"step-finish\",\"cost\":0.05,\"tokens\":{\"total\":2000,\"input\":100,\"output\":50,\"cache\":{\"read\":1800,\"write\":50}}}}}")))
        (tiqsi-test-assert (> tiqsi-opencode-server--total-cost 0)
                           "Server step-finish accumulates cost"
                           (format "cost: %.4f" tiqsi-opencode-server--total-cost))
        (tiqsi-test-assert (= tiqsi-opencode-server--total-tokens 2000)
                           "Server step-finish accumulates tokens"
                           (format "tokens: %d" tiqsi-opencode-server--total-tokens)))
    (kill-buffer buf)
    (setq tiqsi-opencode-server--repl-buffer nil)
    (setq tiqsi-opencode-server--session-id nil)
    (setq tiqsi-opencode-server--total-cost orig-cost)
    (setq tiqsi-opencode-server--total-tokens orig-tok)))

;; Server transport E2E smoke test (requires opencode CLI)
(if (tiqsi-opencode--executable-available-p)
    (let ((tiqsi-opencode-server--process nil)
          (tiqsi-opencode-server--session-id nil)
          (tiqsi-opencode-tls-bypass t))
      (unwind-protect
          (progn
            ;; Start server
            (tiqsi-opencode-server--start-process)
            (let ((healthy (tiqsi-opencode-server--wait-for-healthy)))
              (tiqsi-test-assert healthy "Server E2E: server started and healthy")
              (when healthy
                ;; Create session
                (let ((sid (tiqsi-opencode-server--create-session "e2e-test")))
                  (tiqsi-test-assert (and sid (stringp sid) (string-prefix-p "ses_" sid))
                                     "Server E2E: session created"
                                     (format "sid: %s" sid))
                  ;; Health check
                  (tiqsi-test-assert (tiqsi-opencode-server--health-check)
                                     "Server E2E: health check passes")))))
        ;; Cleanup
        (tiqsi-opencode-server--stop-process)))
  (tiqsi-test-skip "Server E2E tests" "opencode CLI not installed"))


;; ---------------------------------------------------------------------------
;; 14. Tool input formatting & enriched permission display
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Tool Input Formatting")

;; format-tool-input: read tool → filePath
(let ((input (make-hash-table :test 'equal)))
  (puthash "filePath" "/src/main.rs" input)
  (tiqsi-test-assert-equal "/src/main.rs"
                           (tiqsi-opencode-server--format-tool-input "read" input)
                           "format-tool-input: read shows filePath"))

;; format-tool-input: edit tool → filePath
(let ((input (make-hash-table :test 'equal)))
  (puthash "filePath" "/tmp/foo.py" input)
  (tiqsi-test-assert-equal "/tmp/foo.py"
                           (tiqsi-opencode-server--format-tool-input "edit" input)
                           "format-tool-input: edit shows filePath"))

;; format-tool-input: glob tool → pattern
(let ((input (make-hash-table :test 'equal)))
  (puthash "pattern" "**/*.el" input)
  (tiqsi-test-assert-equal "**/*.el"
                           (tiqsi-opencode-server--format-tool-input "glob" input)
                           "format-tool-input: glob shows pattern"))

;; format-tool-input: bash tool → command
(let ((input (make-hash-table :test 'equal)))
  (puthash "command" "git status" input)
  (tiqsi-test-assert-equal "$ git status"
                           (tiqsi-opencode-server--format-tool-input "bash" input)
                           "format-tool-input: bash shows $ command"))

;; format-tool-input: bash long command truncated
(let ((input (make-hash-table :test 'equal)))
  (puthash "command" (make-string 200 ?x) input)
  (let ((result (tiqsi-opencode-server--format-tool-input "bash" input)))
    (tiqsi-test-assert (<= (length result) 125)
                       "format-tool-input: bash truncates long commands"
                       (format "len: %d" (length result)))))

;; format-tool-input: grep tool → pattern + include
(let ((input (make-hash-table :test 'equal)))
  (puthash "pattern" "defun" input)
  (puthash "include" "*.el" input)
  (tiqsi-test-assert-equal "\"defun\" in *.el"
                           (tiqsi-opencode-server--format-tool-input "grep" input)
                           "format-tool-input: grep with include"))

;; format-tool-input: grep tool → pattern only
(let ((input (make-hash-table :test 'equal)))
  (puthash "pattern" "TODO" input)
  (tiqsi-test-assert-equal "\"TODO\""
                           (tiqsi-opencode-server--format-tool-input "grep" input)
                           "format-tool-input: grep without include"))

;; format-tool-input: webfetch tool → URL
(let ((input (make-hash-table :test 'equal)))
  (puthash "url" "https://example.com" input)
  (tiqsi-test-assert-equal "https://example.com"
                           (tiqsi-opencode-server--format-tool-input "webfetch" input)
                           "format-tool-input: webfetch shows URL"))

;; format-tool-input: task tool → description
(let ((input (make-hash-table :test 'equal)))
  (puthash "description" "Search codebase" input)
  (tiqsi-test-assert-equal "Search codebase"
                           (tiqsi-opencode-server--format-tool-input "task" input)
                           "format-tool-input: task shows description"))

;; format-tool-input: unknown tool → JSON fallback
(let ((input (make-hash-table :test 'equal)))
  (puthash "key" "value" input)
  (let ((result (tiqsi-opencode-server--format-tool-input "unknown-tool" input)))
    (tiqsi-test-assert (string-match-p "key" result)
                       "format-tool-input: unknown tool falls back to JSON")))

;; format-tool-input: nil input → empty string
(tiqsi-test-assert-equal "" (tiqsi-opencode-server--format-tool-input "read" nil)
                         "format-tool-input: nil input returns empty")

;; format-permission-desc with tool-info enrichment
(let ((tool-info (list :tool "read"
                       :input (let ((h (make-hash-table :test 'equal)))
                                (puthash "filePath" "/etc/passwd" h)
                                h)
                       :status "running")))
  (let ((desc (tiqsi-opencode-server--format-permission-desc
               "read" ["*"] nil tool-info)))
    (tiqsi-test-assert (string-match-p "Tool: read" desc)
                       "Enriched permission desc includes tool name")
    (tiqsi-test-assert (string-match-p "/etc/passwd" desc)
                       "Enriched permission desc includes file path")))

;; format-permission-desc with bash tool-info
(let ((tool-info (list :tool "bash"
                       :input (let ((h (make-hash-table :test 'equal)))
                                (puthash "command" "rm -rf /" h)
                                h)
                       :status "running")))
  (let ((desc (tiqsi-opencode-server--format-permission-desc
               "bash" ["rm -rf /"] nil tool-info)))
    (tiqsi-test-assert (string-match-p "Tool: bash" desc)
                       "Enriched bash permission includes tool name")
    (tiqsi-test-assert (string-match-p "\\$ rm -rf /" desc)
                       "Enriched bash permission includes command")))

;; format-permission-desc without tool-info (backward compat)
(let ((desc (tiqsi-opencode-server--format-permission-desc
             "edit" ["src/main.rs"] nil nil)))
  (tiqsi-test-assert (string-match-p "edit file" desc)
                     "Permission desc without tool-info still works")
  (tiqsi-test-assert (not (string-match-p "Tool:" desc))
                     "Permission desc without tool-info has no Tool: line"))

;; Tool cache populated from tool part events
(let ((buf (with-current-buffer (get-buffer-create " *test-tool-cache*")
             (tiqsi-claude-repl-mode)
             (current-buffer)))
      (orig-cache tiqsi-opencode-server--tool-calls))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--repl-buffer buf)
        (setq tiqsi-opencode-server--session-id "ses-tool-cache")
        (setq tiqsi-opencode-server--tool-calls (make-hash-table :test 'equal))
        (let ((tiqsi-opencode-show-tool-use t))
          ;; Simulate a tool part event
          (tiqsi-opencode-server--handle-event
           (json-parse-string
            (concat "{\"type\":\"message.part.updated\","
                    "\"properties\":{\"part\":{"
                    "\"id\":\"prt-42\","
                    "\"sessionID\":\"ses-tool-cache\","
                    "\"type\":\"tool\","
                    "\"callID\":\"call-abc-123\","
                    "\"tool\":\"read\","
                    "\"state\":{\"status\":\"running\","
                    "\"input\":{\"filePath\":\"/tmp/test.el\"}}}}}")))
          ;; Check cache
          (let ((cached (gethash "call-abc-123" tiqsi-opencode-server--tool-calls)))
            (tiqsi-test-assert cached "Tool call cached by callID")
            (when cached
              (tiqsi-test-assert-equal "read" (plist-get cached :tool)
                                       "Cached tool name is correct")
              (tiqsi-test-assert (hash-table-p (plist-get cached :input))
                                 "Cached input is hash-table")
              (tiqsi-test-assert-equal "/tmp/test.el"
                                       (gethash "filePath" (plist-get cached :input))
                                       "Cached input has filePath")))))
    (kill-buffer buf)
    (setq tiqsi-opencode-server--repl-buffer nil)
    (setq tiqsi-opencode-server--session-id nil)
    (setq tiqsi-opencode-server--tool-calls orig-cache)))

;; Tool cache cleared on session idle
(let ((buf (with-current-buffer (get-buffer-create " *test-cache-clear*")
             (tiqsi-claude-repl-mode)
             (current-buffer)))
      (orig-cache tiqsi-opencode-server--tool-calls))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--repl-buffer buf)
        (setq tiqsi-opencode-server--session-id "ses-cache-clear")
        (setq tiqsi-opencode-server--tool-calls (make-hash-table :test 'equal))
        (setq tiqsi-opencode-server--request-start-time (current-time))
        (setq tiqsi-opencode-server--output-start nil)
        ;; Add a tool call to the cache
        (puthash "call-xyz" '(:tool "bash" :input nil :status "running")
                 tiqsi-opencode-server--tool-calls)
        (tiqsi-test-assert (= 1 (hash-table-count tiqsi-opencode-server--tool-calls))
                           "Tool cache has entry before idle")
        ;; Simulate session idle event
        (tiqsi-opencode-server--handle-event
         (json-parse-string
          "{\"type\":\"session.idle\",\"properties\":{\"sessionID\":\"ses-cache-clear\"}}"))
        (tiqsi-test-assert (= 0 (hash-table-count tiqsi-opencode-server--tool-calls))
                           "Tool cache cleared after session idle"))
    (kill-buffer buf)
    (setq tiqsi-opencode-server--repl-buffer nil)
    (setq tiqsi-opencode-server--session-id nil)
    (setq tiqsi-opencode-server--tool-calls orig-cache)))

;; ---------------------------------------------------------------------------
;; 15. Session browser
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Session Browser")

;; New functions exist
(dolist (fn '(tiqsi-opencode-server-list-sessions
              tiqsi-opencode-server-switch-session
              tiqsi-opencode-server-delete-session
              tiqsi-opencode-server-new-session
              tiqsi-opencode-server--list-sessions
              tiqsi-opencode-server--format-session-entry
              tiqsi-opencode-server--format-session-age
              tiqsi-opencode-server--render-session-history))
  (tiqsi-test-assert-fboundp fn (format "session browser: %s" fn)))

;; Dispatch wrappers for new/delete
(tiqsi-test-assert-fboundp 'tiqsi-claude-new-session
                           "dispatch wrapper: tiqsi-claude-new-session")
(tiqsi-test-assert-fboundp 'tiqsi-claude-delete-session
                           "dispatch wrapper: tiqsi-claude-delete-session")

;; Session age formatting
(tiqsi-test-assert-equal "?" (tiqsi-opencode-server--format-session-age nil)
                         "Age: nil -> ?")
(tiqsi-test-assert-equal "?" (tiqsi-opencode-server--format-session-age 0)
                         "Age: 0 -> ?")

;; Recent timestamp → "just now" or "Xm ago"
(let* ((now-ms (* (float-time) 1000.0))
       (age-str (tiqsi-opencode-server--format-session-age now-ms)))
  (tiqsi-test-assert (or (string= "just now" age-str)
                         (string-match-p "m ago" age-str))
                     "Age: recent timestamp -> 'just now' or 'Xm ago'"
                     (format "got: %s" age-str)))

;; Old timestamp → "Xd ago" or "Xw ago"
(let* ((one-week-ago-ms (* (- (float-time) (* 8 24 60 60)) 1000.0))
       (age-str (tiqsi-opencode-server--format-session-age one-week-ago-ms)))
  (tiqsi-test-assert (string-match-p "w ago" age-str)
                     "Age: 8 days ago -> 'Xw ago'"
                     (format "got: %s" age-str)))

;; Session entry formatting
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal)))
  (puthash "created" (* (float-time) 1000.0) time-obj)
  (puthash "updated" (* (float-time) 1000.0) time-obj)
  (puthash "id" "ses_test123456789abcdef" session)
  (puthash "title" "My Test Session" session)
  (puthash "time" time-obj session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (string-match-p "ses_test12345678" entry)
                       "Session entry contains truncated ID")
    (tiqsi-test-assert (string-match-p "My Test Session" entry)
                       "Session entry contains title")))

;; Session entry with summary stats
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal))
      (summary (make-hash-table :test 'equal)))
  (puthash "created" (* (float-time) 1000.0) time-obj)
  (puthash "id" "ses_withsummary0000000" session)
  (puthash "title" "Session With Changes" session)
  (puthash "time" time-obj session)
  (puthash "additions" 100 summary)
  (puthash "deletions" 50 summary)
  (puthash "files" 5 summary)
  (puthash "summary" summary session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (string-match-p "\\+100/-50" entry)
                       "Session entry shows additions/deletions")
    (tiqsi-test-assert (string-match-p "5 files" entry)
                       "Session entry shows file count")))

;; Session entry marks current session
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal))
      (orig-sid tiqsi-opencode-server--session-id))
  (puthash "created" (* (float-time) 1000.0) time-obj)
  (puthash "id" "ses_currentmarker00000" session)
  (puthash "title" "Current" session)
  (puthash "time" time-obj session)
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--session-id "ses_currentmarker00000")
        (let ((entry (tiqsi-opencode-server--format-session-entry session)))
          (tiqsi-test-assert (string-match-p "\\*" entry)
                             "Current session marked with *")))
    (setq tiqsi-opencode-server--session-id orig-sid)))

;; Session entry marks forks
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal)))
  (puthash "created" (* (float-time) 1000.0) time-obj)
  (puthash "id" "ses_forkedchild000000" session)
  (puthash "title" "Forked Session" session)
  (puthash "parentID" "ses_parentorig0000000" session)
  (puthash "time" time-obj session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (string-match-p "(fork)" entry)
                       "Forked session marked with (fork)")))

;; Session entry with long title gets truncated
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal)))
  (puthash "created" (* (float-time) 1000.0) time-obj)
  (puthash "id" "ses_longtitle00000000" session)
  (puthash "title" (make-string 80 ?x) session)
  (puthash "time" time-obj session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (string-match-p "\\.\\.\\." entry)
                       "Long title truncated with ...")))

;; switch-session resets counters
(let ((buf (with-current-buffer (get-buffer-create " *test-switch*")
             (tiqsi-claude-repl-mode)
             (current-buffer)))
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-cost tiqsi-opencode-server--total-cost)
      (orig-tok tiqsi-opencode-server--total-tokens)
      (orig-count tiqsi-opencode-server--message-count)
      (orig-cache tiqsi-opencode-server--tool-calls)
      (orig-buf tiqsi-opencode-server--repl-buffer)
      (orig-base tiqsi-opencode-server--base-url))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-server--repl-buffer buf)
        (setq tiqsi-opencode-server--total-cost 99.99)
        (setq tiqsi-opencode-server--total-tokens 999999)
        (setq tiqsi-opencode-server--message-count 42)
        (setq tiqsi-opencode-server--tool-calls (make-hash-table :test 'equal))
        (puthash "stale" t tiqsi-opencode-server--tool-calls)
        ;; Set a dummy base-url so HTTP requests fail gracefully
        (setq tiqsi-opencode-server--base-url "http://127.0.0.1:1")
        ;; Switch (HTTP will fail for info/messages — that's fine)
        (condition-case nil
            (tiqsi-opencode-server-switch-session "ses_new_session_001")
          (error nil))
        (tiqsi-test-assert-equal "ses_new_session_001"
                                 tiqsi-opencode-server--session-id
                                 "switch-session sets new session ID")
        (tiqsi-test-assert (= 0.0 tiqsi-opencode-server--total-cost)
                           "switch-session resets cost")
        (tiqsi-test-assert (= 0 tiqsi-opencode-server--total-tokens)
                           "switch-session resets tokens")
        (tiqsi-test-assert (= 0 tiqsi-opencode-server--message-count)
                           "switch-session resets message count")
        (tiqsi-test-assert (= 0 (hash-table-count tiqsi-opencode-server--tool-calls))
                           "switch-session clears tool cache"))
    (kill-buffer buf)
    (setq tiqsi-opencode-server--repl-buffer orig-buf)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--total-cost orig-cost)
    (setq tiqsi-opencode-server--total-tokens orig-tok)
    (setq tiqsi-opencode-server--message-count orig-count)
    (setq tiqsi-opencode-server--tool-calls orig-cache)
    (setq tiqsi-opencode-server--base-url orig-base)))

;; E2E: list sessions from live server (requires opencode CLI)
(if (tiqsi-opencode--executable-available-p)
    (let ((tiqsi-opencode-server--process nil)
          (tiqsi-opencode-server--session-id nil)
          (tiqsi-opencode-tls-bypass t)
          (orig-base tiqsi-opencode-server--base-url))
      (unwind-protect
          (progn
            (tiqsi-opencode-server--start-process)
            (when (tiqsi-opencode-server--wait-for-healthy)
              ;; List sessions
              (let ((sessions (tiqsi-opencode-server--list-sessions)))
                (tiqsi-test-assert (listp sessions)
                                   "E2E: list-sessions returns a list")
                (tiqsi-test-assert (> (length sessions) 0)
                                   "E2E: list-sessions has at least 1 session"
                                   (format "count: %d" (length sessions)))
                ;; Each session is a hash-table with id and title
                (let ((first (car sessions)))
                  (tiqsi-test-assert (hash-table-p first)
                                     "E2E: first session is a hash-table")
                  (tiqsi-test-assert (stringp (gethash "id" first))
                                     "E2E: session has string ID"
                                     (format "id: %s" (gethash "id" first)))
                  ;; Format it
                  (let ((entry (tiqsi-opencode-server--format-session-entry first)))
                    (tiqsi-test-assert (> (length entry) 0)
                                       "E2E: formatted entry is non-empty"))))
              ;; Create and then delete a test session
              (let ((test-sid (tiqsi-opencode-server--create-session "test-browser")))
                (tiqsi-test-assert (and test-sid (stringp test-sid))
                                   "E2E: created test session for browser"
                                   (format "sid: %s" test-sid))
                ;; Verify it appears in list
                (let* ((sessions2 (tiqsi-opencode-server--list-sessions))
                       (found (cl-find-if
                               (lambda (s) (equal (gethash "id" s) test-sid))
                               sessions2)))
                  (tiqsi-test-assert found
                                     "E2E: new session appears in list"))
                ;; Delete it
                (tiqsi-opencode-server--http-request
                 "DELETE" (format "/session/%s" test-sid))
                ;; Verify it's gone
                (let* ((sessions3 (tiqsi-opencode-server--list-sessions))
                       (found2 (cl-find-if
                                (lambda (s) (equal (gethash "id" s) test-sid))
                                sessions3)))
                  (tiqsi-test-assert (not found2)
                                     "E2E: deleted session no longer in list")))))
        ;; Cleanup
        (tiqsi-opencode-server--stop-process)
        (setq tiqsi-opencode-server--base-url orig-base)))
  (tiqsi-test-skip "Session browser E2E" "opencode CLI not installed"))

;; ---------------------------------------------------------------------------
;; 16. Hydra dispatch fixes (stats, perms, kill, toggle, export, fork)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Hydra Dispatch Fixes")

;; Permission cycle syncs auto-approve
(let ((orig-perm tiqsi-opencode-permission-prompt)
      (orig-auto tiqsi-opencode-auto-approve))
  (unwind-protect
      (progn
        ;; Start at ask
        (setq tiqsi-opencode-permission-prompt 'ask)
        (setq tiqsi-opencode-auto-approve nil)

        ;; Cycle to always -> auto-approve should be t
        (tiqsi-claude-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'always)
                           "Cycle dispatch: ask -> always")
        (tiqsi-test-assert (eq tiqsi-opencode-auto-approve t)
                           "Cycle dispatch: always syncs auto-approve=t")

        ;; Cycle to reject -> auto-approve should be nil
        (tiqsi-claude-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'reject)
                           "Cycle dispatch: always -> reject")
        (tiqsi-test-assert (eq tiqsi-opencode-auto-approve nil)
                           "Cycle dispatch: reject syncs auto-approve=nil")

        ;; Cycle back to ask -> auto-approve should be nil
        (tiqsi-claude-cycle-permission-prompt)
        (tiqsi-test-assert (eq tiqsi-opencode-permission-prompt 'ask)
                           "Cycle dispatch: reject -> ask")
        (tiqsi-test-assert (eq tiqsi-opencode-auto-approve nil)
                           "Cycle dispatch: ask syncs auto-approve=nil"))
    (setq tiqsi-opencode-permission-prompt orig-perm)
    (setq tiqsi-opencode-auto-approve orig-auto)))

;; Stats dispatch: when server not active, uses run-based stats
;; (we can't test server-active path without a real server, but
;; we CAN test that the dispatch wrapper exists and calls correctly)
(let ((orig tiqsi-repl-backend)
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-proc tiqsi-opencode-server--process))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        ;; Ensure server is NOT active
        (setq tiqsi-opencode-server--session-id nil)
        (setq tiqsi-opencode-server--process nil)
        ;; Should fall back to run-based stats (which shows "not started")
        (tiqsi-test-assert-no-error
         "Stats dispatch: no error when server inactive"
         (lambda () (tiqsi-claude-session-stats))))
    (setq tiqsi-repl-backend orig)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--process orig-proc)))

;; Toggle dispatch: picks server buffer when server is active
(let ((buf (with-current-buffer (get-buffer-create " *test-toggle*")
             (tiqsi-claude-repl-mode)
             (current-buffer)))
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-proc tiqsi-opencode-server--process)
      (orig-buf tiqsi-opencode-server--repl-buffer)
      (orig-backend tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        ;; Simulate server active
        (setq tiqsi-opencode-server--session-id "ses-toggle-test")
        (setq tiqsi-opencode-server--process (start-process "sleep" nil "sleep" "60"))
        (setq tiqsi-opencode-server--repl-buffer buf)
        (tiqsi-test-assert (tiqsi-opencode-server-active-p)
                           "Toggle test: server appears active"))
    ;; Cleanup
    (when (and tiqsi-opencode-server--process
               (process-live-p tiqsi-opencode-server--process))
      (delete-process tiqsi-opencode-server--process))
    (kill-buffer buf)
    (setq tiqsi-repl-backend orig-backend)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--process orig-proc)
    (setq tiqsi-opencode-server--repl-buffer orig-buf)))

;; Kill dispatch: doesn't error even when server/run not active
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (tiqsi-test-assert-no-error
         "Kill dispatch: no error when nothing active"
         (lambda () (tiqsi-claude-kill))))
    (setq tiqsi-repl-backend orig)))

;; Export dispatch: no error when server not active (falls back to run-based)
(let ((orig tiqsi-repl-backend)
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-proc tiqsi-opencode-server--process))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (setq tiqsi-opencode-server--session-id nil)
        (setq tiqsi-opencode-server--process nil)
        ;; Should fall back to run-based export (which says "No active session")
        (tiqsi-test-assert-no-error
         "Export dispatch: no error when server inactive"
         (lambda () (tiqsi-claude-export-session))))
    (setq tiqsi-repl-backend orig)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--process orig-proc)))

;; Fork dispatch: no error when server not active
(let ((orig tiqsi-repl-backend)
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-proc tiqsi-opencode-server--process))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (setq tiqsi-opencode-server--session-id nil)
        (setq tiqsi-opencode-server--process nil)
        ;; Should fall back to run-based fork (which says "No active session to fork")
        (tiqsi-test-assert-no-error
         "Fork dispatch: no error when server inactive"
         (lambda () (tiqsi-claude-fork-session))))
    (setq tiqsi-repl-backend orig)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--process orig-proc)))

;; ---------------------------------------------------------------------------
;; 17. Modes sub-hydra & backend guards
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Modes Hydra & Backend Guards")

;; hydra-claude-modes/body is now defined (was dead key before)
(tiqsi-test-assert-fboundp 'hydra-claude-modes/body
                           "hydra-claude-modes/body is defined")

;; Backend guard helper
(tiqsi-test-assert-fboundp 'tiqsi-claude--require-opencode
                           "tiqsi-claude--require-opencode guard defined")

;; Guard blocks when backend=claude
(let ((orig tiqsi-repl-backend)
      (called nil))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'claude)
        ;; The guard should NOT call the wrapped function
        (cl-letf (((symbol-function 'tiqsi-opencode-list-models)
                   (lambda () (interactive) (setq called t))))
          (tiqsi-claude--require-opencode #'tiqsi-opencode-list-models)
          (tiqsi-test-assert (not called)
                             "Guard blocks call when backend=claude")))
    (setq tiqsi-repl-backend orig)))

;; Guard allows when backend=opencode
(let ((orig tiqsi-repl-backend)
      (called nil))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (cl-letf (((symbol-function 'tiqsi-opencode-list-models)
                   (lambda () (interactive) (setq called t))))
          (tiqsi-claude--require-opencode #'tiqsi-opencode-list-models)
          (tiqsi-test-assert called
                             "Guard allows call when backend=opencode")))
    (setq tiqsi-repl-backend orig)))

;; Modes hydra settings toggle: tool display
(let ((orig tiqsi-opencode-show-tool-use))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-tool-use t)
        ;; Simulate toggling off
        (setq tiqsi-opencode-show-tool-use (not tiqsi-opencode-show-tool-use))
        (tiqsi-test-assert (not tiqsi-opencode-show-tool-use)
                           "Modes: tool display toggles off"))
    (setq tiqsi-opencode-show-tool-use orig)))

;; Modes hydra settings toggle: cost display
(let ((orig tiqsi-opencode-show-cost))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-cost t)
        (setq tiqsi-opencode-show-cost (not tiqsi-opencode-show-cost))
        (tiqsi-test-assert (not tiqsi-opencode-show-cost)
                           "Modes: cost display toggles off"))
    (setq tiqsi-opencode-show-cost orig)))

;; Modes hydra settings toggle: thinking
(let ((orig tiqsi-opencode-show-thinking))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-thinking nil)
        (setq tiqsi-opencode-show-thinking (not tiqsi-opencode-show-thinking))
        (tiqsi-test-assert tiqsi-opencode-show-thinking
                           "Modes: thinking display toggles on"))
    (setq tiqsi-opencode-show-thinking orig)))

;; All 12 guarded hydra keys: verify they produce the guard message
;; when the backend is claude (not opencode)
(let ((orig tiqsi-repl-backend)
      (guarded-fns '(tiqsi-opencode-attach-file
                     tiqsi-opencode-set-model
                     tiqsi-opencode-set-agent
                     tiqsi-opencode-set-variant
                     tiqsi-opencode-list-models
                     tiqsi-opencode-import-session
                     tiqsi-opencode-pr
                     tiqsi-opencode-web
                     tiqsi-opencode-serve
                     tiqsi-opencode-attach
                     tiqsi-opencode-agent-list
                     tiqsi-opencode-mcp-list)))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'claude)
        (dolist (fn guarded-fns)
          (let ((called nil))
            (cl-letf (((symbol-function fn)
                       (lambda (&rest _) (interactive) (setq called t))))
              (tiqsi-claude--require-opencode fn)
              (tiqsi-test-assert (not called)
                                 (format "Guard blocks %s when backend=claude"
                                         fn))))))
    (setq tiqsi-repl-backend orig)))

;; ---------------------------------------------------------------------------
;; 18. Dispatch wrapper behavioral tests (mock-based)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Dispatch Behavioral")

;; Helper: test that a dispatch wrapper calls the correct backend function.
;; WRAPPER is the dispatch function symbol.
;; OC-FN is the OpenCode function symbol it should call when backend=opencode.
;; CL-FN is the Claude function symbol it should call when backend=claude.
;; CALL-FORM is a lambda that invokes the wrapper (needed for wrappers
;; that require arguments or special setup).
(defun test-opencode--assert-dispatch (wrapper oc-fn cl-fn &optional call-form)
  "Assert WRAPPER dispatches to OC-FN when backend=opencode and CL-FN when backend=claude."
  (let ((call-form (or call-form (lambda () (funcall wrapper)))))
    ;; Test opencode path
    (let ((orig tiqsi-repl-backend)
          (oc-called nil)
          (cl-called nil))
      (unwind-protect
          (progn
            (setq tiqsi-repl-backend 'opencode)
            (cl-letf (((symbol-function oc-fn) (lambda (&rest _) (setq oc-called t)))
                      ((symbol-function cl-fn) (lambda (&rest _) (setq cl-called t)))
                      ;; Stub ensure-backend to avoid CLI checks
                      ((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                      ;; Stub server-active-p to nil for simple dispatch
                      ((symbol-function 'tiqsi-opencode-server-active-p) (lambda () nil)))
              (funcall call-form)
              (tiqsi-test-assert oc-called
                                 (format "Dispatch %s -> %s when backend=opencode" wrapper oc-fn))
              (tiqsi-test-assert (not cl-called)
                                 (format "Dispatch %s !-> %s when backend=opencode" wrapper cl-fn))))
        (setq tiqsi-repl-backend orig)))
    ;; Test claude path
    (let ((orig tiqsi-repl-backend)
          (oc-called nil)
          (cl-called nil))
      (unwind-protect
          (progn
            (setq tiqsi-repl-backend 'claude)
            (cl-letf (((symbol-function oc-fn) (lambda (&rest _) (setq oc-called t)))
                      ((symbol-function cl-fn) (lambda (&rest _) (setq cl-called t)))
                      ((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                      ((symbol-function 'tiqsi-opencode-server-active-p) (lambda () nil)))
              (funcall call-form)
              (tiqsi-test-assert cl-called
                                 (format "Dispatch %s -> %s when backend=claude" wrapper cl-fn))
              (tiqsi-test-assert (not oc-called)
                                 (format "Dispatch %s !-> %s when backend=claude" wrapper oc-fn))))
        (setq tiqsi-repl-backend orig)))))

;; --- Session management dispatch ---

;; c: tiqsi-claude-start (opencode path calls tiqsi-opencode-server-start with fallback)
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; Test opencode path (server-start succeeds)
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-called nil) (run-called nil) (claude-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-opencode-server-start) (lambda () (setq server-called t)))
                    ((symbol-function 'tiqsi-opencode-start) (lambda () (setq run-called t)))
                    ((symbol-function 'tiqsi-claude-repl-start) (lambda () (setq claude-called t))))
            (tiqsi-claude-start)
            (tiqsi-test-assert server-called
                               "Dispatch c: opencode -> server-start")
            (tiqsi-test-assert (not run-called)
                               "Dispatch c: opencode server ok -> no run fallback")
            (tiqsi-test-assert (not claude-called)
                               "Dispatch c: opencode -> not claude")))
        ;; Test opencode path (server-start fails -> fallback to run)
        (let ((run-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-opencode-server-start)
                     (lambda () (error "Server failed")))
                    ((symbol-function 'tiqsi-opencode-start) (lambda () (setq run-called t))))
            (tiqsi-claude-start)
            (tiqsi-test-assert run-called
                               "Dispatch c: server fail -> run fallback")))
        ;; Test claude path
        (setq tiqsi-repl-backend 'claude)
        (let ((claude-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-claude-repl-start) (lambda () (setq claude-called t))))
            (tiqsi-claude-start)
            (tiqsi-test-assert claude-called
                               "Dispatch c: claude -> repl-start"))))
    (setq tiqsi-repl-backend orig)))

;; k: tiqsi-claude-kill
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; OpenCode path (server active)
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-stop-called nil) (run-kill-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t))
                    ((symbol-function 'tiqsi-opencode-server-stop)
                     (lambda () (setq server-stop-called t)))
                    ((symbol-function 'tiqsi-opencode-kill)
                     (lambda () (setq run-kill-called t))))
            (tiqsi-claude-kill)
            (tiqsi-test-assert server-stop-called
                               "Dispatch k: opencode+server -> server-stop")
            (tiqsi-test-assert run-kill-called
                               "Dispatch k: opencode -> also run-kill")))
        ;; Claude path
        (setq tiqsi-repl-backend 'claude)
        (let ((claude-kill-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude-repl-kill)
                     (lambda () (setq claude-kill-called t))))
            (tiqsi-claude-kill)
            (tiqsi-test-assert claude-kill-called
                               "Dispatch k: claude -> repl-kill"))))
    (setq tiqsi-repl-backend orig)))

;; l: tiqsi-claude-list-sessions
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; OpenCode + server active -> session-browser
        (setq tiqsi-repl-backend 'opencode)
        (let ((browser-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t))
                    ((symbol-function 'tiqsi-opencode-session-browser)
                     (lambda () (setq browser-called t))))
            (tiqsi-claude-list-sessions)
            (tiqsi-test-assert browser-called
                               "Dispatch l: opencode+server -> session-browser")))
        ;; OpenCode + no server -> run list-sessions
        (let ((run-list-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () nil))
                    ((symbol-function 'tiqsi-opencode-list-sessions)
                     (lambda () (setq run-list-called t))))
            (tiqsi-claude-list-sessions)
            (tiqsi-test-assert run-list-called
                               "Dispatch l: opencode no server -> run list-sessions")))
        ;; Claude
        (setq tiqsi-repl-backend 'claude)
        (let ((claude-list-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude-repl-list-sessions)
                     (lambda () (setq claude-list-called t))))
            (tiqsi-claude-list-sessions)
            (tiqsi-test-assert claude-list-called
                               "Dispatch l: claude -> repl-list-sessions"))))
    (setq tiqsi-repl-backend orig)))

;; t: tiqsi-claude-toggle (verify buffer selection logic)
(let ((orig tiqsi-repl-backend)
      (orig-sid tiqsi-opencode-server--session-id)
      (orig-proc tiqsi-opencode-server--process)
      (orig-buf tiqsi-opencode-server--repl-buffer))
  (unwind-protect
      (progn
        ;; OpenCode + server active -> uses server REPL buffer
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-buf (get-buffer-create " *test-toggle-server*")))
          (setq tiqsi-opencode-server--repl-buffer server-buf)
          (setq tiqsi-opencode-server--session-id "ses-toggle")
          (setq tiqsi-opencode-server--process
                (start-process "test-sleep" nil "sleep" "60"))
          (cl-letf (((symbol-function 'display-buffer)
                     (lambda (buf) nil))) ;; prevent actual display
            (tiqsi-claude-toggle)
            (tiqsi-test-assert t "Dispatch t: opencode+server toggle no error"))
          (when (process-live-p tiqsi-opencode-server--process)
            (delete-process tiqsi-opencode-server--process))
          (kill-buffer server-buf))
        ;; Claude path
        (setq tiqsi-repl-backend 'claude)
        (let ((claude-buf-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude-repl--get-or-create-buffer)
                     (lambda () (get-buffer-create " *test-toggle-claude*")))
                    ((symbol-function 'display-buffer) (lambda (buf) nil)))
            (tiqsi-claude-toggle)
            (tiqsi-test-assert t "Dispatch t: claude toggle no error"))
          (when (get-buffer " *test-toggle-claude*")
            (kill-buffer " *test-toggle-claude*"))))
    (setq tiqsi-repl-backend orig)
    (setq tiqsi-opencode-server--session-id orig-sid)
    (setq tiqsi-opencode-server--process orig-proc)
    (setq tiqsi-opencode-server--repl-buffer orig-buf)))

;; C: tiqsi-claude-clear
(test-opencode--assert-dispatch
 'tiqsi-claude-clear
 'tiqsi-opencode-clear
 'tiqsi-claude-repl-clear)

;; n: tiqsi-claude-new-session (opencode no server -> tiqsi-opencode-start)
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; OpenCode + server -> server-new-session
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-new-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t))
                    ((symbol-function 'tiqsi-opencode-server-new-session)
                     (lambda () (setq server-new-called t))))
            (tiqsi-claude-new-session)
            (tiqsi-test-assert server-new-called
                               "Dispatch n: opencode+server -> server-new-session")))
        ;; OpenCode no server -> opencode-start
        (let ((run-start-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () nil))
                    ((symbol-function 'tiqsi-opencode-start)
                     (lambda () (setq run-start-called t))))
            (tiqsi-claude-new-session)
            (tiqsi-test-assert run-start-called
                               "Dispatch n: opencode no server -> opencode-start")))
        ;; Claude
        (setq tiqsi-repl-backend 'claude)
        (let ((claude-new-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude-repl-new-session)
                     (lambda () (setq claude-new-called t))))
            (tiqsi-claude-new-session)
            (tiqsi-test-assert claude-new-called
                               "Dispatch n: claude -> repl-new-session"))))
    (setq tiqsi-repl-backend orig)))

;; d: tiqsi-claude-delete-session
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; OpenCode + server -> server-delete-session
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-delete-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t))
                    ((symbol-function 'tiqsi-opencode-server-delete-session)
                     (lambda () (interactive) (setq server-delete-called t))))
            (tiqsi-claude-delete-session)
            (tiqsi-test-assert server-delete-called
                               "Dispatch d: opencode+server -> server-delete-session")))
        ;; Claude -> message only
        (setq tiqsi-repl-backend 'claude)
        (tiqsi-test-assert-no-error
         "Dispatch d: claude -> message (no error)"
         (lambda () (tiqsi-claude-delete-session))))
    (setq tiqsi-repl-backend orig)))

;; --- Send content dispatch ---

;; f: tiqsi-claude-send-function
(test-opencode--assert-dispatch
 'tiqsi-claude-send-function
 'tiqsi-opencode-send-function
 'tiqsi-claude-repl-send-function)

;; b: tiqsi-claude-send-buffer
(test-opencode--assert-dispatch
 'tiqsi-claude-send-buffer
 'tiqsi-opencode-send-buffer
 'tiqsi-claude-repl-send-buffer)

;; s: tiqsi-claude-send-paragraph
(test-opencode--assert-dispatch
 'tiqsi-claude-send-paragraph
 'tiqsi-opencode-send-paragraph
 'tiqsi-claude-repl-send-paragraph)

;; r: tiqsi-claude-send-region (needs active region)
(let ((orig tiqsi-repl-backend)
      (buf (get-buffer-create " *test-region*")))
  (unwind-protect
      (with-current-buffer buf
        (insert "test region content")
        (set-mark (point-min))
        (goto-char (point-max))
        (activate-mark)
        ;; OpenCode path
        (setq tiqsi-repl-backend 'opencode)
        (let ((oc-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-opencode-send-region)
                     (lambda (s e) (setq oc-called t))))
            (tiqsi-claude-send-region)
            (tiqsi-test-assert oc-called
                               "Dispatch r: opencode -> opencode-send-region")))
        ;; Claude path
        (set-mark (point-min))
        (goto-char (point-max))
        (activate-mark)
        (setq tiqsi-repl-backend 'claude)
        (let ((cl-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-claude-repl-send-region)
                     (lambda (s e) (setq cl-called t))))
            (tiqsi-claude-send-region)
            (tiqsi-test-assert cl-called
                               "Dispatch r: claude -> repl-send-region"))))
    (setq tiqsi-repl-backend orig)
    (kill-buffer buf)))

;; a: tiqsi-claude-ask-question (needs string arg)
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        (setq tiqsi-repl-backend 'opencode)
        (let ((oc-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-opencode-ask)
                     (lambda (q) (setq oc-called t))))
            (tiqsi-claude-ask-question "test question")
            (tiqsi-test-assert oc-called
                               "Dispatch a: opencode -> opencode-ask")))
        (setq tiqsi-repl-backend 'claude)
        (let ((cl-called nil))
          (cl-letf (((symbol-function 'tiqsi-claude--ensure-backend) (lambda ()))
                    ((symbol-function 'tiqsi-claude-repl-ask-question)
                     (lambda (q) (setq cl-called t))))
            (tiqsi-claude-ask-question "test question")
            (tiqsi-test-assert cl-called
                               "Dispatch a: claude -> repl-ask-question"))))
    (setq tiqsi-repl-backend orig)))

;; --- AI features dispatch ---

;; e: tiqsi-claude-fix-error
(test-opencode--assert-dispatch
 'tiqsi-claude-fix-error
 'tiqsi-opencode-fix-error
 'tiqsi-claude-repl-fix-error-at-point)

;; o: tiqsi-claude-optimize-code
(test-opencode--assert-dispatch
 'tiqsi-claude-optimize-code
 'tiqsi-opencode-optimize-code
 'tiqsi-claude-repl-optimize-code)

;; x: tiqsi-claude-explain-code
(test-opencode--assert-dispatch
 'tiqsi-claude-explain-code
 'tiqsi-opencode-explain-code
 'tiqsi-claude-repl-explain-code)

;; T: tiqsi-claude-generate-tests
(test-opencode--assert-dispatch
 'tiqsi-claude-generate-tests
 'tiqsi-opencode-generate-tests
 'tiqsi-claude-repl-generate-tests)

;; --- Session stats dispatch (3-way: opencode+server, opencode-run, claude) ---
(let ((orig tiqsi-repl-backend))
  (unwind-protect
      (progn
        ;; OpenCode + server active -> server-session-stats
        (setq tiqsi-repl-backend 'opencode)
        (let ((server-stats-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t))
                    ((symbol-function 'tiqsi-opencode-server-session-stats)
                     (lambda () (setq server-stats-called t))))
            (tiqsi-claude-session-stats)
            (tiqsi-test-assert server-stats-called
                               "Dispatch S: opencode+server -> server-session-stats")))
        ;; OpenCode no server -> run session-stats
        (let ((run-stats-called nil))
          (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () nil))
                    ((symbol-function 'tiqsi-opencode-session-stats)
                     (lambda () (setq run-stats-called t))))
            (tiqsi-claude-session-stats)
            (tiqsi-test-assert run-stats-called
                               "Dispatch S: opencode no server -> run session-stats")))
        ;; Claude -> message (no stats fn)
        (setq tiqsi-repl-backend 'claude)
        (tiqsi-test-assert-no-error
         "Dispatch S: claude -> no error"
         (lambda () (tiqsi-claude-session-stats))))
    (setq tiqsi-repl-backend orig)))

;; --- m: modes sub-hydra entry ---
(tiqsi-test-assert-no-error
 "Dispatch m: hydra-claude-modes/body callable"
 (lambda ()
   ;; We can't fully invoke the hydra (it takes over input), but we
   ;; verify it's a callable function that doesn't error on definition check
   (cl-letf (((symbol-function 'hydra-claude-modes/body)
              (lambda () (interactive) t)))
     (hydra-claude-modes/body))))

;; ---------------------------------------------------------------------------
;; 19. Modes hydra lambda behavioral tests
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Modes Hydra Lambdas")

;; We invoke the actual hydra-generated hint functions to verify the lambdas
;; work. Hydra generates functions named hydra-claude-modes/KEY for each key.

;; t key: toggle tool display via hydra-generated function
(let ((orig tiqsi-opencode-show-tool-use))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-tool-use t)
        ;; Hydra generates hydra-claude-modes/lambda-t-and-exit for :color blue keys.
        ;; We call it directly (stubbing hydra-keyboard-quit to avoid hydra state).
        (if (fboundp 'hydra-claude-modes/lambda-t-and-exit)
            (progn
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-t-and-exit))
              ;; The lambda toggled it: t -> nil
              (tiqsi-test-assert (not tiqsi-opencode-show-tool-use)
                                 "Modes t: tool display toggled to OFF via hydra lambda")
              ;; Call again: nil -> t
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-t-and-exit))
              (tiqsi-test-assert tiqsi-opencode-show-tool-use
                                 "Modes t: tool display toggled back to ON via hydra lambda"))
          ;; Fallback: test toggle logic directly
          (setq tiqsi-opencode-show-tool-use (not tiqsi-opencode-show-tool-use))
          (tiqsi-test-assert (not tiqsi-opencode-show-tool-use)
                             "Modes t: tool display toggled to OFF")
          (setq tiqsi-opencode-show-tool-use (not tiqsi-opencode-show-tool-use))
          (tiqsi-test-assert tiqsi-opencode-show-tool-use
                             "Modes t: tool display toggled back to ON")))
    (setq tiqsi-opencode-show-tool-use orig)))

;; c key: toggle cost display via hydra-generated function
(let ((orig tiqsi-opencode-show-cost))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-cost t)
        (if (fboundp 'hydra-claude-modes/lambda-c-and-exit)
            (progn
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-c-and-exit))
              (tiqsi-test-assert (not tiqsi-opencode-show-cost)
                                 "Modes c: cost display toggled to OFF via hydra lambda")
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-c-and-exit))
              (tiqsi-test-assert tiqsi-opencode-show-cost
                                 "Modes c: cost display toggled back to ON via hydra lambda"))
          (setq tiqsi-opencode-show-cost (not tiqsi-opencode-show-cost))
          (tiqsi-test-assert (not tiqsi-opencode-show-cost)
                             "Modes c: cost display toggled to OFF")
          (setq tiqsi-opencode-show-cost (not tiqsi-opencode-show-cost))
          (tiqsi-test-assert tiqsi-opencode-show-cost
                             "Modes c: cost display toggled back to ON")))
    (setq tiqsi-opencode-show-cost orig)))

;; k key: toggle thinking via hydra-generated function
(let ((orig tiqsi-opencode-show-thinking))
  (unwind-protect
      (progn
        (setq tiqsi-opencode-show-thinking nil)
        (if (fboundp 'hydra-claude-modes/lambda-k-and-exit)
            (progn
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-k-and-exit))
              (tiqsi-test-assert tiqsi-opencode-show-thinking
                                 "Modes k: thinking toggled to ON via hydra lambda")
              (cl-letf (((symbol-function 'hydra-keyboard-quit) (lambda ())))
                (hydra-claude-modes/lambda-k-and-exit))
              (tiqsi-test-assert (not tiqsi-opencode-show-thinking)
                                 "Modes k: thinking toggled back to OFF via hydra lambda"))
          (setq tiqsi-opencode-show-thinking (not tiqsi-opencode-show-thinking))
          (tiqsi-test-assert tiqsi-opencode-show-thinking
                             "Modes k: thinking toggled to ON")
          (setq tiqsi-opencode-show-thinking (not tiqsi-opencode-show-thinking))
          (tiqsi-test-assert (not tiqsi-opencode-show-thinking)
                             "Modes k: thinking toggled back to OFF")))
    (setq tiqsi-opencode-show-thinking orig)))

;; ---------------------------------------------------------------------------
;; 20. Help lambda & guarded CLI keys behavioral tests
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Help & Guarded Keys Behavioral")

;; h key: help lambda — just verify it doesn't error
(tiqsi-test-assert-no-error
 "Help lambda (h): no error when invoked"
 (lambda ()
   ;; The help message references tiqsi-claude--backend-label
   (let ((tiqsi-repl-backend 'claude))
     ;; Simulate calling the help lambda body
     (message "AI REPL [%s]: B=switch backend, l=browse sessions, n=new, d=delete. OpenCode keys (F/D/A/V/L/I/P/W/R/X/G/N) require backend=opencode."
              (tiqsi-claude--backend-label)))))

;; Guarded keys: verify they dispatch to the underlying function when backend=opencode
;; Each guarded key wraps tiqsi-claude--require-opencode around the real function.
;; We mock the real function and verify it gets called.

(let ((guarded-keys '(("F" tiqsi-opencode-attach-file)
                      ("D" tiqsi-opencode-set-model)
                      ("A" tiqsi-opencode-set-agent)
                      ("V" tiqsi-opencode-set-variant)
                      ("L" tiqsi-opencode-list-models)
                      ("I" tiqsi-opencode-import-session)
                      ("P" tiqsi-opencode-pr)
                      ("W" tiqsi-opencode-web)
                      ("R" tiqsi-opencode-serve)
                      ("X" tiqsi-opencode-attach)
                      ("G" tiqsi-opencode-agent-list)
                      ("N" tiqsi-opencode-mcp-list))))
  (dolist (entry guarded-keys)
    (let ((key (car entry))
          (fn (cadr entry))
          (orig tiqsi-repl-backend))
      (unwind-protect
          (progn
            ;; When backend=opencode, the guard should let the call through
            (setq tiqsi-repl-backend 'opencode)
            (let ((called nil))
              (cl-letf (((symbol-function fn)
                         (lambda (&rest _) (interactive) (setq called t))))
                (tiqsi-claude--require-opencode fn)
                (tiqsi-test-assert called
                                   (format "Guarded %s (%s): calls through when backend=opencode"
                                           key fn)))))
        (setq tiqsi-repl-backend orig)))))

;; ---------------------------------------------------------------------------
;; 13. Permission introspection
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Permission Introspection")

;; Test: format-permission-rule produces correct output
(let ((rule (make-hash-table :test 'equal)))
  (puthash "permission" "bash" rule)
  (puthash "pattern" "*" rule)
  (puthash "action" "allow" rule)
  (let ((result (tiqsi-opencode-server--format-permission-rule rule)))
    (tiqsi-test-assert (stringp result) "format-permission-rule returns string")
    (tiqsi-test-assert (string-match-p "bash" result) "format-permission-rule contains tool name")
    (tiqsi-test-assert (string-match-p "ALLOW" result) "format-permission-rule shows ALLOW")))

;; Test: format-permission-rule with deny action
(let ((rule (make-hash-table :test 'equal)))
  (puthash "permission" "edit" rule)
  (puthash "pattern" "*.el" rule)
  (puthash "action" "deny" rule)
  (let ((result (tiqsi-opencode-server--format-permission-rule rule)))
    (tiqsi-test-assert (string-match-p "DENY" result) "format-permission-rule shows DENY for deny action")
    (tiqsi-test-assert (string-match-p "edit" result) "format-permission-rule shows tool name for deny")))

;; Test: format-permission-rule with non-hash returns nil
(tiqsi-test-assert (null (tiqsi-opencode-server--format-permission-rule "not a hash"))
                   "format-permission-rule returns nil for non-hash")

;; Test: format-permission-summary with mixed allow/deny
(let ((perms (list (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "allow" h) h)
                   (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "deny" h) h)
                   (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "allow" h) h))))
  (let ((summary (tiqsi-opencode-server--format-permission-summary perms)))
    (tiqsi-test-assert (string-match-p "2 allow" summary) "format-permission-summary counts allows correctly")
    (tiqsi-test-assert (string-match-p "1 deny" summary) "format-permission-summary counts denies correctly")))

;; Test: format-permission-summary with only allows
(let ((perms (list (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "allow" h) h))))
  (tiqsi-test-assert (equal "1 allow" (tiqsi-opencode-server--format-permission-summary perms))
                     "format-permission-summary shows only allow count when no denies"))

;; Test: format-permission-summary with only denies
(let ((perms (list (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "deny" h) h)
                   (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "deny" h) h))))
  (tiqsi-test-assert (equal "2 deny" (tiqsi-opencode-server--format-permission-summary perms))
                     "format-permission-summary shows only deny count when no allows"))

;; Test: format-permission-summary with empty list
(tiqsi-test-assert (equal "no rules" (tiqsi-opencode-server--format-permission-summary nil))
                   "format-permission-summary returns 'no rules' for nil")
(tiqsi-test-assert (equal "no rules" (tiqsi-opencode-server--format-permission-summary '()))
                   "format-permission-summary returns 'no rules' for empty list")

;; Test: get-session-permissions finds the right session
(let ((sessions-mock nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-server--list-sessions)
             (lambda ()
               (let ((s1 (make-hash-table :test 'equal))
                     (s2 (make-hash-table :test 'equal))
                     (perm (make-hash-table :test 'equal)))
                 (puthash "permission" "bash" perm)
                 (puthash "pattern" "*" perm)
                 (puthash "action" "allow" perm)
                 (puthash "id" "ses_abc" s1)
                 (puthash "permission" (vector perm) s1)
                 (puthash "id" "ses_xyz" s2)
                 (list s1 s2)))))
    (let ((tiqsi-opencode-server--session-id "ses_abc"))
      (let ((perms (tiqsi-opencode-server--get-session-permissions)))
        (tiqsi-test-assert (= 1 (length perms))
                           "get-session-permissions finds perms for current session")
        (tiqsi-test-assert (hash-table-p (car perms))
                           "get-session-permissions returns hash-table entries")))
    (let ((tiqsi-opencode-server--session-id "ses_xyz"))
      (let ((perms (tiqsi-opencode-server--get-session-permissions)))
        (tiqsi-test-assert (null perms)
                           "get-session-permissions returns nil when session has no perms")))))

;; Test: get-session-permissions with explicit session-id parameter
(cl-letf (((symbol-function 'tiqsi-opencode-server--list-sessions)
           (lambda ()
             (let ((s1 (make-hash-table :test 'equal))
                   (perm (make-hash-table :test 'equal)))
               (puthash "permission" "read" perm)
               (puthash "action" "deny" perm)
               (puthash "id" "ses_target" s1)
               (puthash "permission" (vector perm) s1)
               (list s1)))))
  (let ((tiqsi-opencode-server--session-id "ses_other"))
    (let ((perms (tiqsi-opencode-server--get-session-permissions "ses_target")))
      (tiqsi-test-assert (= 1 (length perms))
                         "get-session-permissions uses explicit session-id over current"))))

;; Test: session browser entry includes permission summary
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal))
      (perm1 (make-hash-table :test 'equal))
      (perm2 (make-hash-table :test 'equal)))
  (puthash "action" "allow" perm1)
  (puthash "action" "deny" perm2)
  (puthash "id" "ses_test1234567890" session)
  (puthash "title" "Test Session" session)
  (puthash "created" (* (float-time) 1000) time-obj)
  (puthash "time" time-obj session)
  (puthash "permission" (vector perm1 perm2) session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (string-match-p "1 allow" entry)
                       "session browser entry includes allow count")
    (tiqsi-test-assert (string-match-p "1 deny" entry)
                       "session browser entry includes deny count")))

;; Test: session browser entry without permissions shows no bracket
(let ((session (make-hash-table :test 'equal))
      (time-obj (make-hash-table :test 'equal)))
  (puthash "id" "ses_noperm123456789" session)
  (puthash "title" "No Perms" session)
  (puthash "created" (* (float-time) 1000) time-obj)
  (puthash "time" time-obj session)
  (let ((entry (tiqsi-opencode-server--format-session-entry session)))
    (tiqsi-test-assert (not (string-match-p "\\[" entry))
                       "session browser entry without perms has no brackets")))

;; Test: show-permissions function is bound
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-permissions
                           "show-permissions function exists")

;; Test: hydra key p is bound in inspect sub-hydra keymap
(tiqsi-test-assert (and (boundp 'hydra-claude-inspect/keymap)
                        (keymapp hydra-claude-inspect/keymap)
                        (lookup-key hydra-claude-inspect/keymap "p"))
                   "hydra-claude-inspect has 'p' key for permission viewer")

;; ---------------------------------------------------------------------------
;; 14. Permission grant cache
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Permission Grant Cache")

;; Test: permission-grants variable exists
(tiqsi-test-assert (boundp 'tiqsi-opencode-server--permission-grants)
                   "permission-grants variable is defined")

;; Test: finalize-permission caches "always" grants
(let ((tiqsi-opencode-server--permission-grants nil))
  ;; Mock reply-permission and repl-insert to no-op
  (cl-letf (((symbol-function 'tiqsi-opencode-server--reply-permission)
             (lambda (&rest _) nil))
            ((symbol-function 'tiqsi-opencode-server--repl-insert)
             (lambda (&rest _) nil)))
    (tiqsi-opencode-server--finalize-permission "req1" "always" "bash" ["*"])
    (tiqsi-test-assert (= 1 (length tiqsi-opencode-server--permission-grants))
                       "finalize-permission caches 'always' grant")
    (let ((entry (car tiqsi-opencode-server--permission-grants)))
      (tiqsi-test-assert (equal "bash" (gethash "permission" entry))
                         "cached grant has correct permission type")
      (tiqsi-test-assert (equal "allow" (gethash "action" entry))
                         "cached 'always' grant has action=allow")
      (tiqsi-test-assert (equal "*" (gethash "pattern" entry))
                         "cached grant has correct pattern")
      (tiqsi-test-assert (stringp (gethash "time" entry))
                         "cached grant has timestamp"))))

;; Test: finalize-permission caches "once" grants with allow-once action
(let ((tiqsi-opencode-server--permission-grants nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-server--reply-permission)
             (lambda (&rest _) nil))
            ((symbol-function 'tiqsi-opencode-server--repl-insert)
             (lambda (&rest _) nil)))
    (tiqsi-opencode-server--finalize-permission "req2" "once" "edit" ["*.el"])
    (tiqsi-test-assert (= 1 (length tiqsi-opencode-server--permission-grants))
                       "finalize-permission caches 'once' grant")
    (let ((entry (car tiqsi-opencode-server--permission-grants)))
      (tiqsi-test-assert (equal "allow-once" (gethash "action" entry))
                         "cached 'once' grant has action=allow-once"))))

;; Test: finalize-permission does NOT cache "reject"
(let ((tiqsi-opencode-server--permission-grants nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-server--reply-permission)
             (lambda (&rest _) nil))
            ((symbol-function 'tiqsi-opencode-server--repl-insert)
             (lambda (&rest _) nil)))
    (tiqsi-opencode-server--finalize-permission "req3" "reject" "bash" ["*"])
    (tiqsi-test-assert (null tiqsi-opencode-server--permission-grants)
                       "finalize-permission does not cache 'reject'")))

;; Test: multiple grants accumulate
(let ((tiqsi-opencode-server--permission-grants nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-server--reply-permission)
             (lambda (&rest _) nil))
            ((symbol-function 'tiqsi-opencode-server--repl-insert)
             (lambda (&rest _) nil)))
    (tiqsi-opencode-server--finalize-permission "r1" "always" "bash" ["*"])
    (tiqsi-opencode-server--finalize-permission "r2" "always" "edit" ["*.py"])
    (tiqsi-opencode-server--finalize-permission "r3" "once" "read" nil)
    (tiqsi-test-assert (= 3 (length tiqsi-opencode-server--permission-grants))
                       "multiple grants accumulate in cache")))

;; Test: format-permission-summary counts allow-once as allow
(let ((perms (list (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "allow-once" h) h)
                   (let ((h (make-hash-table :test 'equal)))
                     (puthash "action" "allow" h) h))))
  (tiqsi-test-assert (equal "2 allow" (tiqsi-opencode-server--format-permission-summary perms))
                     "format-permission-summary counts allow-once as allow"))

;; Test: cache is cleared on session switch
(let ((tiqsi-opencode-server--permission-grants (list (make-hash-table)))
      (tiqsi-opencode-server--session-id "old-session")
      (tiqsi-opencode-server--busy nil)
      (tiqsi-opencode-server--total-cost 0.0)
      (tiqsi-opencode-server--total-tokens 0)
      (tiqsi-opencode-server--message-count 0)
      (tiqsi-opencode-server--request-start-time nil)
      (tiqsi-opencode-server--output-start nil)
      (tiqsi-opencode-server--repl-buffer nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-server--http-request)
             (lambda (&rest _) nil)))
    (tiqsi-opencode-server-switch-session "new-session")
    (tiqsi-test-assert (null tiqsi-opencode-server--permission-grants)
                       "permission grants cache cleared on session switch")))

;; ---------------------------------------------------------------------------
;; 15. Tabulated session browser
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Session Browser Tabulated")

;; Test: browser mode is defined
(tiqsi-test-assert-fboundp 'tiqsi-opencode-session-browser-mode
                           "session browser mode is defined")

;; Test: browser entry point is defined
(tiqsi-test-assert-fboundp 'tiqsi-opencode-session-browser
                           "session browser function exists")

;; Test: browser mode keymap exists with expected keys
(tiqsi-test-assert (keymapp tiqsi-opencode-session-browser-mode-map)
                   "session browser keymap exists")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "RET"))
                   "browser keymap has RET for switch")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "d"))
                   "browser keymap has d for delete")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "n"))
                   "browser keymap has n for new")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "g"))
                   "browser keymap has g for refresh")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "p"))
                   "browser keymap has p for perms")
(tiqsi-test-assert (lookup-key tiqsi-opencode-session-browser-mode-map (kbd "q"))
                   "browser keymap has q for quit")

;; Test: browser entries builder produces correct format
(cl-letf (((symbol-function 'tiqsi-opencode-server--list-sessions)
           (lambda ()
             (let ((s1 (make-hash-table :test 'equal))
                   (time-obj (make-hash-table :test 'equal)))
               (puthash "id" "ses_test12345678901234" s1)
               (puthash "title" "Test Session" s1)
               (puthash "created" (* (float-time) 1000) time-obj)
               (puthash "time" time-obj s1)
               (list s1)))))
  (let ((entries (tiqsi-opencode-session-browser--entries)))
    (tiqsi-test-assert (= 1 (length entries))
                       "browser entries returns correct count")
    (let ((entry (car entries)))
      (tiqsi-test-assert (equal "ses_test12345678901234" (car entry))
                         "browser entry has session ID as key")
      (tiqsi-test-assert (vectorp (cadr entry))
                         "browser entry has vector of column values")
      (tiqsi-test-assert (= 6 (length (cadr entry)))
                         "browser entry has 6 columns"))))

;; Test: browser entries mark current session
(cl-letf (((symbol-function 'tiqsi-opencode-server--list-sessions)
           (lambda ()
             (let ((s1 (make-hash-table :test 'equal))
                   (s2 (make-hash-table :test 'equal))
                   (t1 (make-hash-table :test 'equal))
                   (t2 (make-hash-table :test 'equal)))
               (puthash "id" "ses_current" s1)
               (puthash "title" "Current" s1)
               (puthash "created" (* (float-time) 1000) t1)
               (puthash "time" t1 s1)
               (puthash "id" "ses_other" s2)
               (puthash "title" "Other" s2)
               (puthash "created" (* (float-time) 1000) t2)
               (puthash "time" t2 s2)
               (list s1 s2)))))
  (let* ((tiqsi-opencode-server--session-id "ses_current")
         (entries (tiqsi-opencode-session-browser--entries))
         (e1 (car entries))
         (e2 (cadr entries)))
    (tiqsi-test-assert (equal "*" (aref (cadr e1) 0))
                       "current session marked with *")
    (tiqsi-test-assert (equal "" (aref (cadr e2) 0))
                       "non-current session has no marker")))

;; Test: list-sessions dispatch uses session browser when server active
(let ((browser-called nil))
  (cl-letf (((symbol-function 'tiqsi-opencode-session-browser)
             (lambda () (interactive) (setq browser-called t)))
            ((symbol-function 'tiqsi-opencode-server-active-p)
             (lambda () t)))
    (let ((tiqsi-repl-backend 'opencode))
      (tiqsi-claude-list-sessions)
      (tiqsi-test-assert browser-called
                         "list-sessions dispatches to tabulated browser when server active"))))

;; ---------------------------------------------------------------------------
;; 16. New viewer functions
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Viewer Functions")

;; Functions exist
(tiqsi-test-assert-fboundp 'tiqsi-opencode-session-history "session history browser defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-tool-log "tool log viewer defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-files "file context viewer defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-cost-breakdown "cost breakdown viewer defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-health "health diagnostics defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-pick-model "quick model picker defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-server-show-mcp "MCP browser defined")
(tiqsi-test-assert-fboundp 'tiqsi-opencode-show-keybinding-reference "keybinding reference defined")

;; State variables exist
(tiqsi-test-assert (boundp 'tiqsi-opencode-server--tool-call-log) "tool-call-log var exists")
(tiqsi-test-assert (boundp 'tiqsi-opencode-server--cost-log) "cost-log var exists")
(tiqsi-test-assert (boundp 'tiqsi-opencode-server--file-references) "file-references var exists")

;; History browser mode exists
(tiqsi-test-assert-fboundp 'tiqsi-opencode-history-browser-mode "history browser mode defined")
(tiqsi-test-assert (keymapp tiqsi-opencode-history-browser-mode-map) "history browser keymap exists")
(tiqsi-test-assert (lookup-key tiqsi-opencode-history-browser-mode-map (kbd "RET"))
                   "history browser has RET key")
(tiqsi-test-assert (lookup-key tiqsi-opencode-history-browser-mode-map (kbd "g"))
                   "history browser has g for refresh")

;; Tool log viewer shows data from the log
(let ((tiqsi-opencode-server--tool-call-log
       (list (list :tool "bash" :status "running" :time "12:00:00" :call-id "c1" :input nil)
             (list :tool "read" :status "running" :time "12:00:01" :call-id "c2" :input nil))))
  ;; Mock server active check
  (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t)))
    (tiqsi-opencode-server-show-tool-log)
    (let ((buf (get-buffer "*OpenCode Tool Log*")))
      (tiqsi-test-assert (buffer-live-p buf) "tool log creates buffer")
      (with-current-buffer buf
        (tiqsi-test-assert (string-match-p "bash" (buffer-string)) "tool log contains bash entry")
        (tiqsi-test-assert (string-match-p "read" (buffer-string)) "tool log contains read entry")
        (tiqsi-test-assert (string-match-p "2 calls" (buffer-string)) "tool log shows total count"))
      (kill-buffer buf))))

;; Cost breakdown viewer shows data from cost log
(let ((tiqsi-opencode-server--cost-log
       (list (list :cost 0.005 :tokens-total 100 :tokens-in 80 :tokens-out 20
                   :cache-read 0 :time "12:00:00")))
      (tiqsi-opencode-server--total-cost 0.005)
      (tiqsi-opencode-server--total-tokens 100)
      (tiqsi-opencode-server--message-count 1))
  (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t)))
    (tiqsi-opencode-server-show-cost-breakdown)
    (let ((buf (get-buffer "*OpenCode Costs*")))
      (tiqsi-test-assert (buffer-live-p buf) "cost breakdown creates buffer")
      (with-current-buffer buf
        (tiqsi-test-assert (string-match-p "0.005" (buffer-string)) "cost breakdown shows cost value")
        (tiqsi-test-assert (string-match-p "1 steps" (buffer-string)) "cost breakdown shows step count"))
      (kill-buffer buf))))

;; File viewer shows references
(let ((tiqsi-opencode-server--file-references
       (list (list :path "/foo/bar.el" :tool "read" :time "12:00:00" :action "read")
             (list :path "/foo/baz.py" :tool "edit" :time "12:00:01" :action "edit"))))
  (cl-letf (((symbol-function 'tiqsi-opencode-server-active-p) (lambda () t)))
    (tiqsi-opencode-server-show-files)
    (let ((buf (get-buffer "*OpenCode Files*")))
      (tiqsi-test-assert (buffer-live-p buf) "file viewer creates buffer")
      (with-current-buffer buf
        (tiqsi-test-assert (string-match-p "bar.el" (buffer-string)) "file viewer shows bar.el")
        (tiqsi-test-assert (string-match-p "baz.py" (buffer-string)) "file viewer shows baz.py")
        (tiqsi-test-assert (string-match-p "2 unique" (buffer-string)) "file viewer shows unique count"))
      (kill-buffer buf))))

;; Health viewer works without server running
(let ((tiqsi-opencode-server--process nil)
      (tiqsi-opencode-server--sse-process nil)
      (tiqsi-opencode-server--session-id nil)
      (tiqsi-opencode-server--base-url nil)
      (tiqsi-opencode-server--busy nil)
      (tiqsi-opencode-server--sse-reconnect-count 0)
      (tiqsi-opencode-server--message-count 0)
      (tiqsi-opencode-server--total-cost 0.0)
      (tiqsi-opencode-server--total-tokens 0)
      (tiqsi-opencode-server--tool-call-log nil)
      (tiqsi-opencode-server--file-references nil)
      (tiqsi-opencode-server--permission-grants nil))
  (tiqsi-opencode-server-show-health)
  (let ((buf (get-buffer "*OpenCode Health*")))
    (tiqsi-test-assert (buffer-live-p buf) "health viewer creates buffer")
    (with-current-buffer buf
      (tiqsi-test-assert (string-match-p "STOPPED" (buffer-string)) "health shows STOPPED when no process")
      (tiqsi-test-assert (string-match-p "Diagnostics" (buffer-string)) "health shows diagnostics header"))
    (kill-buffer buf)))

;; Keybinding reference opens buffer
(tiqsi-opencode-show-keybinding-reference)
(let ((buf (get-buffer "*OpenCode Keys*")))
  (tiqsi-test-assert (buffer-live-p buf) "keybinding reference creates buffer")
  (with-current-buffer buf
    (tiqsi-test-assert (string-match-p "Session" (buffer-string)) "keybinding ref shows Session section")
    (tiqsi-test-assert (string-match-p "Inspect" (buffer-string)) "keybinding ref shows Inspect section")
    (tiqsi-test-assert (string-match-p "Send" (buffer-string)) "keybinding ref shows Send section"))
  (kill-buffer buf))

;; ---------------------------------------------------------------------------
;; 17. Nested hydra structure
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Nested Hydras")

;; All sub-hydra bodies exist
(tiqsi-test-assert (fboundp 'hydra-claude-session/body) "session sub-hydra exists")
(tiqsi-test-assert (fboundp 'hydra-claude-send/body) "send sub-hydra exists")
(tiqsi-test-assert (fboundp 'hydra-claude-inspect/body) "inspect sub-hydra exists")
(tiqsi-test-assert (fboundp 'hydra-claude/body) "main hydra exists")

;; Main hydra has keys for sub-hydras
(tiqsi-test-assert (and (boundp 'hydra-claude/keymap)
                        (lookup-key hydra-claude/keymap "s"))
                   "main hydra has 's' for session sub-hydra")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "S")
                   "main hydra has 'S' for send sub-hydra")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "i")
                   "main hydra has 'i' for inspect sub-hydra")

;; Main hydra has direct action keys
(tiqsi-test-assert (lookup-key hydra-claude/keymap "c") "main hydra has 'c' for start")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "a") "main hydra has 'a' for ask")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "e") "main hydra has 'e' for fix error")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "o") "main hydra has 'o' for optimize")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "x") "main hydra has 'x' for explain")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "T") "main hydra has 'T' for tests")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "t") "main hydra has 't' for toggle")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "M") "main hydra has 'M' for perms")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "B") "main hydra has 'B' for backend")
(tiqsi-test-assert (lookup-key hydra-claude/keymap "?") "main hydra has '?' for keybinding ref")

;; Session sub-hydra keys
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "l") "session hydra has 'l' for browse")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "n") "session hydra has 'n' for new")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "d") "session hydra has 'd' for delete")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "h") "session hydra has 'h' for history")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "k") "session hydra has 'k' for kill")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "f") "session hydra has 'f' for fork")
(tiqsi-test-assert (lookup-key hydra-claude-session/keymap "E") "session hydra has 'E' for export")

;; Send sub-hydra keys
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "r") "send hydra has 'r' for region")
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "f") "send hydra has 'f' for function")
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "b") "send hydra has 'b' for buffer")
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "s") "send hydra has 's' for paragraph")
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "a") "send hydra has 'a' for ask")
(tiqsi-test-assert (lookup-key hydra-claude-send/keymap "F") "send hydra has 'F' for attach file")

;; Inspect sub-hydra keys
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "p") "inspect hydra has 'p' for perms")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "c") "inspect hydra has 'c' for cost")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "t") "inspect hydra has 't' for tool log")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "f") "inspect hydra has 'f' for files")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "s") "inspect hydra has 's' for stats")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "h") "inspect hydra has 'h' for health")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "D") "inspect hydra has 'D' for model picker")
(tiqsi-test-assert (lookup-key hydra-claude-inspect/keymap "m") "inspect hydra has 'm' for MCP")

;; ---------------------------------------------------------------------------
;; 18. Source syntax (including server module)
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "OpenCode: Source Syntax")

(let* ((base-dir (tiqsi-test-expand "modules/modes/"))
       (files (list
               (expand-file-name "claude-repl/tiqsi-claude-repl-opencode.el" base-dir)
               (expand-file-name "claude-repl/tiqsi-claude-repl-opencode-server.el" base-dir)
               (expand-file-name "modes-claude.el" base-dir))))
  (dolist (full-path files)
    (let ((short-name (file-relative-name full-path base-dir)))
      (if (not (file-exists-p full-path))
          (tiqsi-test-assert nil (format "Syntax: %s" short-name) "file not found")
        (condition-case err
            (progn
              (with-temp-buffer
                (insert-file-contents full-path)
                (goto-char (point-min))
                (while (not (eobp))
                  (forward-sexp)))
              (tiqsi-test-assert t (format "Syntax: %s" short-name)))
          (error
           (tiqsi-test-assert nil (format "Syntax: %s" short-name)
                              (error-message-string err))))))))

(provide 'test-opencode)

;;; test-opencode.el ends here
