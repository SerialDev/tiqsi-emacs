;;; modes-claude.el --- Advanced Claude AI integration wrapper  -*- lexical-binding: t -*-

;; Copyright (C) 2025-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: ai, claude, repl, tools
;; Version: 2.0.0
;; Package-Requires: ((cl-lib "0.5") (emacs "30"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Advanced Claude AI integration for Tiqsi Emacs
;; 
;; This is now a wrapper around the new tiqsi-claude-repl.el which provides
;; a million times better integration with proper REPL functionality,
;; minor mode support, and seamless integration like python-mode.
;;
;; Features:
;; - Proper minor mode that works in any buffer
;; - REPL-style interaction with persistent context
;; - Smart context extraction and language awareness
;; - Real-time streaming responses
;; - Project-aware conversation history
;; - Inline code suggestions and fixes
;;

;;; Code:

;; Declare function to avoid warnings
(declare-function tiqsi-claude-repl--get-or-create-buffer "tiqsi-claude-repl-core")

;; Define when-available macro if not already defined
(unless (fboundp 'when-available)
  (defmacro when-available (package &rest body)
    "Execute BODY if PACKAGE is available."
    `(when (require ,package nil 'noerror)
       ,@body)))

;; Load the new and improved Claude REPL integration
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl.el")


;; Ensure required dependencies are available (fallback for older systems)
(straight-use-package
  '(transient
     :type git
     :host github
     :repo "magit/transient"
     ))

(straight-use-package
  '(hydra
     :type git
     :host github
     :repo "abo-abo/hydra"
     ))

;; ---------------------------------------------------------------------------
;; Backend availability and fallback
;; ---------------------------------------------------------------------------

(defun tiqsi-claude--ensure-backend ()
  "Ensure the selected backend's CLI is available.
If not, switch to the other backend automatically.  Signals an
error only when neither CLI is found."
  (cond
   ((and (eq tiqsi-repl-backend 'opencode)
         (not (tiqsi-opencode--executable-available-p)))
    (if (tiqsi-claude-repl--executable-available-p)
        (progn (message "OpenCode CLI not found — switching to Claude")
               (setq tiqsi-repl-backend 'claude))
      (error "Neither OpenCode nor Claude CLI found")))
   ((and (eq tiqsi-repl-backend 'claude)
         (not (tiqsi-claude-repl--executable-available-p)))
    (if (tiqsi-opencode--executable-available-p)
        (progn (message "Claude CLI not found — switching to OpenCode")
               (setq tiqsi-repl-backend 'opencode))
      (error "Neither Claude nor OpenCode CLI found")))))

;; ---------------------------------------------------------------------------
;; Smart dispatch wrappers
;; ---------------------------------------------------------------------------
;;
;; Every wrapper checks `tiqsi-repl-backend' and routes to either Claude or
;; OpenCode.  All existing keybindings (hydra, mode hooks, flycheck) call
;; these wrappers, so switching backend is seamless — change the variable
;; and every keybinding follows.

;;;###autoload
(defun tiqsi-claude-start ()
  "Start AI REPL session (dispatches by `tiqsi-repl-backend').
If the selected backend's CLI is not installed, automatically falls
back to the other backend.  For OpenCode, uses the server transport
(opencode serve + SSE) which supports interactive permission prompts."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (condition-case err
          (tiqsi-opencode-server-start)
        (error
         (message "Server transport failed (%s), falling back to run mode"
                  (error-message-string err))
         (tiqsi-opencode-start)))
    (tiqsi-claude-repl-start)))

;;;###autoload
(defun tiqsi-claude-kill ()
  "Kill AI REPL session (dispatches by `tiqsi-repl-backend').
For OpenCode, stops the server transport if active, then the run
transport."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (progn
        ;; Stop server transport if active
        (when (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-server-stop))
        ;; Also kill run-based process if any
        (tiqsi-opencode-kill))
    (tiqsi-claude-repl-kill)))

;;;###autoload
(defun tiqsi-claude-toggle ()
  "Toggle AI REPL window (dispatches by `tiqsi-repl-backend').
For OpenCode, prefers the server transport REPL buffer if active."
  (interactive)
  (let* ((repl-buffer
          (cond
           ((and (eq tiqsi-repl-backend 'opencode)
                 (tiqsi-opencode-server-active-p)
                 tiqsi-opencode-server--repl-buffer
                 (buffer-live-p tiqsi-opencode-server--repl-buffer))
            tiqsi-opencode-server--repl-buffer)
           ((eq tiqsi-repl-backend 'opencode)
            (tiqsi-opencode--get-or-create-buffer))
           (t
            (tiqsi-claude-repl--get-or-create-buffer))))
         (repl-window (get-buffer-window repl-buffer)))
    (if repl-window
        (delete-window repl-window)
      (display-buffer repl-buffer))))

;;;###autoload
(defun tiqsi-claude-send-region ()
  "Send region to AI REPL (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (if (region-active-p)
      (progn
        (tiqsi-claude--ensure-backend)
        (if (eq tiqsi-repl-backend 'opencode)
            (tiqsi-opencode-send-region (region-beginning) (region-end))
          (tiqsi-claude-repl-send-region (region-beginning) (region-end))))
    (message "No region selected")))

;;;###autoload
(defun tiqsi-claude-send-function ()
  "Send function to AI REPL (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-send-function)
    (tiqsi-claude-repl-send-function)))

;;;###autoload
(defun tiqsi-claude-send-buffer ()
  "Send buffer to AI REPL (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-send-buffer)
    (tiqsi-claude-repl-send-buffer)))

;;;###autoload
(defun tiqsi-claude-send-paragraph ()
  "Send paragraph to AI REPL (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-send-paragraph)
    (tiqsi-claude-repl-send-paragraph)))

;;;###autoload
(defun tiqsi-claude-ask-question (question)
  "Ask a question to AI REPL (dispatches by `tiqsi-repl-backend').
Falls back to the other backend if the selected one is unavailable."
  (interactive (list (read-string
                      (format "Ask %s: "
                              (if (eq tiqsi-repl-backend 'opencode)
                                  "OpenCode" "Claude")))))
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-ask question)
    (tiqsi-claude-repl-ask-question question)))

;;;###autoload
(defun tiqsi-claude-fix-error ()
  "Fix error at point (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-fix-error)
    (tiqsi-claude-repl-fix-error-at-point)))

;;;###autoload
(defun tiqsi-claude-optimize-code ()
  "Optimize code (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-optimize-code)
    (tiqsi-claude-repl-optimize-code)))

;;;###autoload
(defun tiqsi-claude-explain-code ()
  "Explain code (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-explain-code)
    (tiqsi-claude-repl-explain-code)))

;;;###autoload
(defun tiqsi-claude-generate-tests ()
  "Generate tests (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (tiqsi-claude--ensure-backend)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-generate-tests)
    (tiqsi-claude-repl-generate-tests)))

;;;###autoload
(defun tiqsi-claude-list-sessions ()
  "List and browse sessions (dispatches by `tiqsi-repl-backend').
When the OpenCode server transport is active, opens a tabulated
session browser with keybindings for switch/delete/new/perms.
Otherwise falls back to `opencode session list' CLI or Claude's
buffer-based session list."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (if (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-session-browser)
        (tiqsi-opencode-list-sessions))
    (tiqsi-claude-repl-list-sessions)))

;;;###autoload
(defun tiqsi-claude-new-session ()
  "Create a new AI REPL session (dispatches by `tiqsi-repl-backend').
For OpenCode server transport, creates a new session via API.
For Claude, starts a fresh REPL."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (if (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-server-new-session)
        (tiqsi-opencode-start))
    (tiqsi-claude-repl-new-session)))

;;;###autoload
(defun tiqsi-claude-delete-session ()
  "Delete an AI session (dispatches by `tiqsi-repl-backend').
Only available for OpenCode server transport."
  (interactive)
  (if (and (eq tiqsi-repl-backend 'opencode)
           (tiqsi-opencode-server-active-p))
      (call-interactively #'tiqsi-opencode-server-delete-session)
    (message "Session deletion requires OpenCode server transport")))

;;;###autoload
(defun tiqsi-claude-session-stats ()
  "Show session statistics (dispatches by `tiqsi-repl-backend').
For OpenCode, uses server transport stats when the server is active,
otherwise falls back to the run-based transport stats."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (if (tiqsi-opencode-server-active-p)
          (tiqsi-opencode-server-session-stats)
        (tiqsi-opencode-session-stats))
    ;; Claude backend — show session summary if available
    (if (fboundp 'tiqsi-claude-repl-show-session-summary)
        (tiqsi-claude-repl-show-session-summary)
      (message "No stats available for Claude backend"))))

;;;###autoload
(defun tiqsi-claude-export-session ()
  "Export session (dispatches by `tiqsi-repl-backend').
For OpenCode server transport, uses the server session ID."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (if (and (tiqsi-opencode-server-active-p)
               tiqsi-opencode-server--session-id)
          ;; Export using server session ID via CLI
          (let ((process-environment (tiqsi-opencode--build-env)))
             (async-shell-command
              (format "%s export %s"
                      (shell-quote-argument tiqsi-opencode-program)
                      (shell-quote-argument tiqsi-opencode-server--session-id))
              "*OpenCode Export*"))
        (tiqsi-opencode-export-session))
    (message "Export not available for Claude backend")))

;;;###autoload
(defun tiqsi-claude-fork-session ()
  "Fork session (dispatches by `tiqsi-repl-backend').
For OpenCode server transport, uses the server session ID."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (if (and (tiqsi-opencode-server-active-p)
               tiqsi-opencode-server--session-id)
          ;; Fork via CLI using server session ID, then switch to it
          (let ((process-environment (tiqsi-opencode--build-env))
                (old-sid tiqsi-opencode-server--session-id))
            (message "Forking server session %s..." old-sid)
            ;; Create a new session on the server as the fork destination
            (let ((new-sid (tiqsi-opencode-server--create-session
                            (format "fork of %s" old-sid))))
              (when new-sid
                (tiqsi-opencode-server-switch-session new-sid)
                (tiqsi-opencode-server-send
                 (format "Continue from session %s. This is a forked session."
                         old-sid))
                (message "Forked to new session: %s" new-sid))))
        (tiqsi-opencode-fork-session))
    (message "Fork not available for Claude backend")))

;;;###autoload
(defun tiqsi-claude-cycle-permission-prompt ()
  "Cycle the OpenCode permission prompt mode and sync both transports.
Cycles: ask -> always -> reject -> ask.
For the server transport, this controls interactive permission prompts.
For the run transport, this syncs `tiqsi-opencode-auto-approve'."
  (interactive)
  ;; Cycle the server transport permission mode
  (tiqsi-opencode-cycle-permission-prompt)
  ;; Sync run transport auto-approve
  (setq tiqsi-opencode-auto-approve
        (eq tiqsi-opencode-permission-prompt 'always))
  ;; Provide clear feedback about what each transport will do
  (let ((mode tiqsi-opencode-permission-prompt))
    (message "Permissions: %s │ Server: %s │ Run: %s"
             mode
             (pcase mode
               ('ask "interactive prompts")
               ('always "auto-approve all")
               ('reject "auto-reject all"))
             (if tiqsi-opencode-auto-approve
                 "auto-approve (OPENCODE_PERMISSION)"
               "auto-reject (no OPENCODE_PERMISSION)"))))

;;;###autoload
(defun tiqsi-claude-clear ()
  "Clear REPL buffer (dispatches by `tiqsi-repl-backend')."
  (interactive)
  (if (eq tiqsi-repl-backend 'opencode)
      (tiqsi-opencode-clear)
    (tiqsi-claude-repl-clear)))

;; Ensure hydra is loaded before defining the hydra
(unless (featurep 'hydra)
  (straight-use-package 'hydra))

;; Ensure lv (hydra dependency) is also loaded
(unless (featurep 'lv)
  (straight-use-package 'lv))

;; Now require hydra
(require 'hydra nil t)

;; Debug: Check if defhydra is available
(if (fboundp 'defhydra)
    (message "defhydra macro is available")
  (error "ERROR: defhydra macro is NOT available! Cannot proceed without hydra."))


;; ---------------------------------------------------------------------------
;; Helper for backend indicator in hydra hint
;; ---------------------------------------------------------------------------

(defun tiqsi-claude--backend-label ()
  "Return a label for the current backend (for hydra hint)."
  (if (eq tiqsi-repl-backend 'opencode) "OpenCode" "Claude"))

;; ---------------------------------------------------------------------------
;; Backend guard helper
;; ---------------------------------------------------------------------------

(defun tiqsi-claude--require-opencode (fn)
  "Call FN interactively if the active backend is opencode.
Otherwise display a message that the key requires the OpenCode backend."
  (if (eq tiqsi-repl-backend 'opencode)
      (call-interactively fn)
    (message "Requires OpenCode backend (current: %s). Press B to switch."
             (tiqsi-claude--backend-label))))

;; ---------------------------------------------------------------------------
;; Sub-hydra: Session management
;; ---------------------------------------------------------------------------

(defhydra hydra-claude-session (:color blue :hint nil)
  "
╭──────────────────────────────────────────╮
│  Session Management                      │
├──────────────────────────────────────────┤
│  _l_: Browse sessions    _n_: New        │
│  _d_: Delete session     _h_: History    │
│  _k_: Kill session       _C_: Clear buf  │
│  _f_: Fork session       _E_: Export     │
│  _I_: Import session                     │
│  _q_: Back                               │
╰──────────────────────────────────────────╯
"
  ("l" tiqsi-claude-list-sessions "Browse")
  ("n" tiqsi-claude-new-session "New")
  ("d" tiqsi-claude-delete-session "Delete")
  ("h" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-session-history))
       "History")
  ("k" tiqsi-claude-kill "Kill")
  ("C" tiqsi-claude-clear "Clear")
  ("f" tiqsi-claude-fork-session "Fork")
  ("E" tiqsi-claude-export-session "Export")
  ("I" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-import-session))
       "Import")
  ("q" nil "Back"))

;; ---------------------------------------------------------------------------
;; Sub-hydra: Send content
;; ---------------------------------------------------------------------------

(defhydra hydra-claude-send (:color blue :hint nil)
  "
╭──────────────────────────────────────────╮
│  Send Content                            │
├──────────────────────────────────────────┤
│  _r_: Region             _f_: Function   │
│  _b_: Entire buffer      _s_: Paragraph  │
│  _a_: Ask (freeform)     _F_: Attach file│
│  _q_: Back                               │
╰──────────────────────────────────────────╯
"
  ("r" tiqsi-claude-send-region "Region")
  ("f" tiqsi-claude-send-function "Function")
  ("b" tiqsi-claude-send-buffer "Buffer")
  ("s" tiqsi-claude-send-paragraph "Paragraph")
  ("a" tiqsi-claude-ask-question "Ask")
  ("F" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-attach-file))
       "Attach file")
  ("q" nil "Back"))

;; ---------------------------------------------------------------------------
;; Sub-hydra: Inspect / Status
;; ---------------------------------------------------------------------------

(defhydra hydra-claude-inspect (:color blue :hint nil)
  "
╭──────────────────────────────────────────╮
│  Inspect / Status                        │
├──────────────────────────────────────────┤
│  _p_: Permissions        _c_: Cost       │
│  _t_: Tool call log      _f_: Files      │
│  _s_: Session stats      _h_: Health     │
│  _D_: Pick model         _m_: MCP        │
│  _G_: Agents/tools                       │
│  _q_: Back                               │
╰──────────────────────────────────────────╯
"
  ("p" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-permissions))
       "Permissions")
  ("c" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-cost-breakdown))
       "Cost breakdown")
  ("t" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-tool-log))
       "Tool log")
  ("f" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-files))
       "Files")
  ("s" tiqsi-claude-session-stats "Stats")
  ("h" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-health))
       "Health")
  ("D" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-pick-model))
       "Pick model")
  ("m" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-server-show-mcp))
       "MCP servers")
  ("G" (lambda () (interactive)
         (tiqsi-claude--require-opencode #'tiqsi-opencode-agent-list))
       "Agents")
  ("q" nil "Back"))

;; ---------------------------------------------------------------------------
;; Main AI REPL Hydra
;; ---------------------------------------------------------------------------
;; Top-level dispatcher. Frequently used actions are direct keys,
;; everything else is organized into sub-hydras.

(defhydra hydra-claude (:color pink :hint nil)
    "
╭─────────────────────────────────────────────────────────────╮
│          Tiqsi AI REPL  [%(tiqsi-claude--backend-label)]  %(if tiqsi-opencode-server--busy \"BUSY\" \"idle\")
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  _c_: Start/connect       _a_: Ask (freeform)              │
│  _s_: Session...          _S_: Send content...              │
│  _i_: Inspect/status...   _t_: Toggle REPL                 │
│                                                             │
│  AI Actions               Settings                         │
│  ──────────               ──────────                       │
│  _e_: Fix error           _M_: Perms (%`tiqsi-opencode-permission-prompt)
│  _o_: Optimize code       _B_: Backend                     │
│  _x_: Explain code        _1_: Tools %s(if tiqsi-opencode-show-tool-use \"ON\" \"off\")
│  _T_: Generate tests      _2_: Cost %s(if tiqsi-opencode-show-cost \"ON\" \"off\")
│                           _3_: Thinking %s(if tiqsi-opencode-show-thinking \"ON\" \"off\")
│                                                             │
│  _?_: Key reference       _q_: Quit                        │
╰─────────────────────────────────────────────────────────────╯
"
    ;; Core actions
    ("c" tiqsi-claude-start "Start")
    ("a" tiqsi-claude-ask-question "Ask")
    ("t" tiqsi-claude-toggle "Toggle")

    ;; Sub-hydras
    ("s" hydra-claude-session/body "Session..." :exit t)
    ("S" hydra-claude-send/body "Send..." :exit t)
    ("i" hydra-claude-inspect/body "Inspect..." :exit t)

    ;; AI features (direct — these are used constantly)
    ("e" tiqsi-claude-fix-error "Fix Error")
    ("o" tiqsi-claude-optimize-code "Optimize")
    ("x" tiqsi-claude-explain-code "Explain")
    ("T" tiqsi-claude-generate-tests "Tests")

    ;; Settings (direct — quick toggles)
    ("M" tiqsi-claude-cycle-permission-prompt "Cycle perms")
    ("B" tiqsi-opencode-switch "Switch backend")
    ("1" (lambda () (interactive)
           (setq tiqsi-opencode-show-tool-use (not tiqsi-opencode-show-tool-use))
           (message "Tool display: %s" (if tiqsi-opencode-show-tool-use "ON" "OFF")))
         "Toggle tools")
    ("2" (lambda () (interactive)
           (setq tiqsi-opencode-show-cost (not tiqsi-opencode-show-cost))
           (message "Cost display: %s" (if tiqsi-opencode-show-cost "ON" "OFF")))
         "Toggle cost")
    ("3" (lambda () (interactive)
           (setq tiqsi-opencode-show-thinking (not tiqsi-opencode-show-thinking))
           (message "Thinking blocks: %s" (if tiqsi-opencode-show-thinking "ON" "OFF")))
         "Toggle thinking")

    ;; Help / Quit
    ("?" tiqsi-opencode-show-keybinding-reference "Key reference" :exit t)
    ("q" nil "Quit" :exit t))

;; Debug: Check if hydra was created
(message "After defhydra - hydra-claude/body exists: %s" (fboundp 'hydra-claude/body))

;; Wrapper function for debugging
(defun tiqsi-claude-hydra ()
  "Launch the AI REPL hydra."
  (interactive)
  (if (fboundp 'hydra-claude/body)
      (hydra-claude/body)
    (message "ERROR: hydra-claude/body is not defined! Check *Messages* buffer.")))

;; Global keybindings for AI REPL integration
(global-set-key (kbd "M-c") 'tiqsi-claude-hydra)
(global-set-key (kbd "C-c a") 'hydra-claude/body) ; Alternative (as documented in CLAUDE.md)
(global-set-key (kbd "C-c i") 'hydra-claude/body) ; Alternative
(global-set-key (kbd "<f8>") 'hydra-claude/body)  ; Function key option

;; Enable the minor mode globally for programming buffers
(when (fboundp 'global-tiqsi-claude-repl-mode)
  (global-tiqsi-claude-repl-mode 1))

;; Integration with programming modes - seamless like python-mode
;; All hooks go through the smart-dispatch wrappers, so the active
;; backend determines whether Claude or OpenCode handles the request.
;; Use `with-eval-after-load' with the correct feature names (not mode
;; names) to ensure hooks are added when the mode is actually loaded.
(with-eval-after-load 'python
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

;; Integration with flycheck for error explanations
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c C-e") 'tiqsi-claude-fix-error))

;; Startup info
(if (fboundp 'hydra-claude/body)
    (message "Tiqsi AI REPL loaded [backend: %s]. Use M-c for hydra menu."
             (tiqsi-claude--backend-label))
  (message "ERROR: hydra-claude/body is not defined!"))

(provide 'modes-claude)

;;; modes-claude.el ends here