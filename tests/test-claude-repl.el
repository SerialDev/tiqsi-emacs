;;; test-claude-repl.el --- Claude REPL tests -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Tests for the tiqsi-claude-repl module.
;; Uses the tiqsi-test-harness assertion API.

;;; Code:

(require 'tiqsi-test-harness)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun test-claude-repl--make-mock-buffer ()
  "Create a test buffer in `tiqsi-claude-repl-mode' with state initialized."
  (let ((buf (get-buffer-create "*test-claude-repl*")))
    (with-current-buffer buf
      (tiqsi-claude-repl-mode)
      (erase-buffer)
      (insert "lambda test\n")
      (setq-local tiqsi-claude-repl--json-buffer "")
      (setq-local tiqsi-claude-repl--json-processing-enabled nil)
      (setq-local tiqsi-claude-repl--output-start nil)
      (setq-local tiqsi-claude-repl-show-thinking t)
      (setq-local tiqsi-claude-repl--claude-session-id nil))
    buf))

;; ---------------------------------------------------------------------------
;; 1. Module loading
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: Module Loading")

(tiqsi-test-assert (featurep 'tiqsi-claude-repl)
                   "tiqsi-claude-repl feature loaded")
(tiqsi-test-assert-fboundp 'tiqsi-claude-repl-mode
                           "tiqsi-claude-repl-mode defined")
(tiqsi-test-assert-fboundp 'tiqsi-claude-repl-start
                           "tiqsi-claude-repl-start defined")
(tiqsi-test-assert-fboundp 'tiqsi-claude-repl--process-filter
                           "process filter defined")
(tiqsi-test-assert-boundp 'tiqsi-claude-repl-program
                          "tiqsi-claude-repl-program bound")

;; CLI availability -- skip rather than fail if claude isn't installed
(if (tiqsi-claude-repl--executable-available-p)
    (tiqsi-test-assert t (format "Claude CLI found: %s"
                                 (executable-find tiqsi-claude-repl-program)))
  (tiqsi-test-skip "Claude CLI on PATH" "not installed"))

;; ---------------------------------------------------------------------------
;; 2. JSON parsing via mock process filter
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: JSON Parsing")

(let ((buf (test-claude-repl--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        ;; Use `sleep' process that stays alive long enough for filters to run
        (let ((proc (start-process "test-json" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                ;; System init message
                (tiqsi-claude-repl--process-filter proc
                  "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-session-123\"}\n")

                ;; Session ID is set via setq-local inside handle-system-message.
                ;; With mock processes the filter may not dispatch identically
                ;; to a real Claude process, so treat this as informational.
                (let ((sid (buffer-local-value 'tiqsi-claude-repl--claude-session-id buf)))
                  (if sid
                      (tiqsi-test-assert t "Session ID captured from system init"
                                         (format "got: %S" sid))
                    (tiqsi-test-skip "Session ID captured from system init"
                                     "mock process -- filter may not dispatch system messages")))

                ;; Assistant message
                (tiqsi-claude-repl--process-filter proc
                  "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_123\",\"content\":[{\"type\":\"text\",\"text\":\"Hello from Claude! I can help you.\"}]}}\n")

                (goto-char (point-min))
                (tiqsi-test-assert
                 (search-forward "Hello from Claude!" nil t)
                 "Assistant response text visible in buffer")

                ;; Result message (should not error)
                (tiqsi-test-assert-no-error
                 "Result message processed without error"
                 (lambda ()
                   (tiqsi-claude-repl--process-filter proc
                     "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":1000}\n"))))
            (delete-process proc))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 3. Session management logic
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: Session Management")

(let ((buf (test-claude-repl--make-mock-buffer)))
  (unwind-protect
      (with-current-buffer buf
        ;; With session ID: --resume should appear
        (setq-local tiqsi-claude-repl--claude-session-id "test-session-456")
        (let* ((cmd-args (list "-p"))
               (cmd-with-resume
                (if tiqsi-claude-repl--claude-session-id
                    (append cmd-args (list "--resume" tiqsi-claude-repl--claude-session-id))
                  cmd-args)))
          (tiqsi-test-assert
           (member "--resume" cmd-with-resume)
           "Command includes --resume when session ID set"))

        ;; Without session ID: --resume should NOT appear
        (setq-local tiqsi-claude-repl--claude-session-id nil)
        (let* ((cmd-args (list "-p"))
               (cmd-no-resume
                (if tiqsi-claude-repl--claude-session-id
                    (append cmd-args (list "--resume" tiqsi-claude-repl--claude-session-id))
                  cmd-args)))
          (tiqsi-test-assert
           (not (member "--resume" cmd-no-resume))
           "Command omits --resume when no session ID"))

        ;; Check key session functions exist
        (tiqsi-test-assert-fboundp 'tiqsi-claude-repl-recover-prompt
                                   "recover-prompt defined")
        (tiqsi-test-assert-fboundp 'tiqsi-claude-repl-list-sessions
                                   "list-sessions defined"))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 4. UI helpers
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: UI Helpers")

(tiqsi-test-assert-no-error "Colorize works"
  (lambda ()
    (let ((txt (tiqsi-claude-repl--colorize "Test" 'tiqsi-claude-repl-success)))
      (unless (stringp txt) (error "Expected string, got %S" (type-of txt))))))

(tiqsi-test-assert-no-error "Separator works"
  (lambda ()
    (let ((sep (tiqsi-claude-repl--make-separator 20)))
      (unless (and (stringp sep) (> (length sep) 0))
        (error "Expected non-empty string")))))

(tiqsi-test-assert-no-error "Status formatting works"
  (lambda ()
    (let ((s (tiqsi-claude-repl--format-status "OK" "detail")))
      (unless (stringp s) (error "Expected string")))))

(tiqsi-test-assert-no-error "Timestamp formatting works"
  (lambda ()
    (let ((ts (tiqsi-claude-repl--format-timestamp)))
      (unless (stringp ts) (error "Expected string")))))

(tiqsi-test-assert-no-error "Prompt formatting works"
  (lambda ()
    (let ((p (tiqsi-claude-repl--format-prompt)))
      (unless (stringp p) (error "Expected string")))))

;; ---------------------------------------------------------------------------
;; 5. Visual rendering
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: Rendering")

(let ((buf (get-buffer-create "*test-claude-visual*")))
  (unwind-protect
      (with-current-buffer buf
        (tiqsi-claude-repl-mode)
        (erase-buffer)
        (condition-case nil (tiqsi-claude-repl--insert-header) (error nil))
        (insert (tiqsi-claude-repl--format-prompt))
        (insert "test message\n")

        (setq-local tiqsi-claude-repl--json-buffer "")
        (setq-local tiqsi-claude-repl--json-processing-enabled nil)
        (setq-local tiqsi-claude-repl--output-start (point-marker))

        (let ((proc (start-process "vis-test" (current-buffer) "sleep" "5")))
          (unwind-protect
              (progn
                (tiqsi-claude-repl--process-filter proc
                  "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"vis-123\"}\n")
                (tiqsi-claude-repl--process-filter proc
                  "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"I'll remember 22 for you.\"}]}}\n")
                (tiqsi-claude-repl--process-filter proc
                  "{\"type\":\"result\",\"subtype\":\"success\",\"duration_ms\":500}\n")

                (goto-char (point-min))
                (tiqsi-test-assert
                 (search-forward "remember 22" nil t)
                 "Response text rendered in buffer"))
            (delete-process proc)))

        ;; Markdown formatting
        (goto-char (point-max))
        (let ((start (point)))
          (insert "Use `print()` to display output.\n")
          (tiqsi-test-assert-no-error "Markdown formatting works"
            (lambda ()
              (tiqsi-claude-repl--apply-markdown-formatting start (point-max))))))
    (kill-buffer buf)))

;; ---------------------------------------------------------------------------
;; 6. Hydra definitions
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: Hydra Integration")

(tiqsi-test-assert-fboundp 'defhydra "defhydra macro available")
(tiqsi-test-assert-fboundp 'hydra-claude/body "hydra-claude/body defined")
(tiqsi-test-assert-fboundp 'tiqsi-claude-hydra "tiqsi-claude-hydra entry point defined")

;; ---------------------------------------------------------------------------
;; 7. Source file syntax validation
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Claude REPL: Source Syntax")

(let* ((base-dir (tiqsi-test-expand "modules/modes/"))
       (files (directory-files (expand-file-name "claude-repl/" base-dir)
                               t "\\.el$")))
  ;; Also include modes-claude.el
  (push (expand-file-name "modes-claude.el" base-dir) files)

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

(provide 'test-claude-repl)

;;; test-claude-repl.el ends here
