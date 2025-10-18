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

;; New Claude REPL Integration - Backwards compatible wrapper functions

;;;###autoload
(defun tiqsi-claude-start ()
  "Start Claude REPL session (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-start))

;;;###autoload
(defun tiqsi-claude-kill ()
  "Kill Claude REPL session (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-kill))

;;;###autoload  
(defun tiqsi-claude-toggle ()
  "Toggle Claude REPL window."
  (interactive)
  (let* ((repl-buffer (tiqsi-claude-repl--get-or-create-buffer))
         (repl-window (get-buffer-window repl-buffer)))
    (if repl-window
        (delete-window repl-window)
      (display-buffer repl-buffer))))

;;;###autoload
(defun tiqsi-claude-send-region ()
  "Send region to Claude (wrapper for new implementation)."
  (interactive)
  (if (region-active-p)
      (tiqsi-claude-repl-send-region (region-beginning) (region-end))
    (message "No region selected")))

;;;###autoload
(defun tiqsi-claude-send-function ()
  "Send function to Claude (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-send-function))

;;;###autoload
(defun tiqsi-claude-send-buffer ()
  "Send buffer to Claude (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-send-buffer))

;;;###autoload
(defun tiqsi-claude-ask-question (question)
  "Ask Claude a question (wrapper for new implementation)."
  (interactive "sAsk Claude: ")
  (tiqsi-claude-repl-ask-question question))

;;;###autoload
(defun tiqsi-claude-fix-error ()
  "Fix error at point (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-fix-error-at-point))

;;;###autoload
(defun tiqsi-claude-optimize-code ()
  "Optimize code (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-optimize-code))

;;;###autoload
(defun tiqsi-claude-explain-code ()
  "Explain code (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-explain-code))

;;;###autoload
(defun tiqsi-claude-generate-tests ()
  "Generate tests (wrapper for new implementation)."
  (interactive)
  (tiqsi-claude-repl-generate-tests))

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


;; Enhanced Tiqsi Claude Hydra - now using the new REPL system
(defhydra hydra-claude (:color pink :hint nil)
    "
╭──────────────────────────────────────────────────────────╮
│                 Tiqsi Claude REPL Assistant              │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Session      │  Send Content     │  AI Features        │
│  ──────────   │  ─────────────    │  ─────────────      │
│  _c_: Start   │  _r_: Region      │  _e_: Fix Error     │
│  _k_: Kill    │  _f_: Function    │  _o_: Optimize      │
│  _l_: List    │  _b_: Buffer      │  _x_: Explain       │
│  _t_: Toggle  │  _a_: Ask         │  _T_: Tests         │
│  _C_: Clear   │  _s_: Paragraph   │                     │
│               │                   │  Settings           │
│               │                   │  _m_: Modes menu    │
│               │                   │  _M_: Cycle perms   │
│                                                          │
│  _q_: Quit    │  _h_: Help                              │
╰──────────────────────────────────────────────────────────╯
"
    ("c" tiqsi-claude-start "Start Claude")
    ("k" tiqsi-claude-kill "Kill Session")
    ("l" tiqsi-claude-repl-list-sessions "List Sessions")
    ("t" tiqsi-claude-toggle "Toggle Window")
    ("C" tiqsi-claude-repl-clear "Clear Buffer")
    ("r" tiqsi-claude-send-region "Send Region")
    ("f" tiqsi-claude-send-function "Send Function")
    ("b" tiqsi-claude-send-buffer "Send Buffer")
    ("a" tiqsi-claude-ask-question "Ask Question")
    ("s" tiqsi-claude-repl-send-paragraph "Send Paragraph")
    ("e" tiqsi-claude-fix-error "Fix Error")
    ("o" tiqsi-claude-optimize-code "Optimize Code")
    ("x" tiqsi-claude-explain-code "Explain Code")
    ("T" tiqsi-claude-generate-tests "Generate Tests")
    ("m" (lambda () 
           (interactive)
           (if (fboundp 'hydra-claude-modes/body)
               (hydra-claude-modes/body)
             (message "Modes hydra not yet loaded. Try again in a moment.")))
         "Modes" :exit t)
    ("M" tiqsi-claude-repl-cycle-permission-mode "Cycle Perms")
    ("h" (lambda () (interactive) 
           (message "Claude REPL: Enhanced with history, syntax highlighting, and more!"))
         "Help" :exit t)
    ("q" nil "Quit" :exit t))

;; Debug: Check if hydra was created
(message "After defhydra - hydra-claude/body exists: %s" (fboundp 'hydra-claude/body))

;; Wrapper function for debugging
(defun tiqsi-claude-hydra ()
  "Launch Claude hydra with debugging."
  (interactive)
  (if (fboundp 'hydra-claude/body)
      (hydra-claude/body)
    (message "ERROR: hydra-claude/body is not defined! Check *Messages* buffer.")))

;; Global keybindings for Claude integration
(global-set-key (kbd "M-c") 'tiqsi-claude-hydra)
(global-set-key (kbd "C-c a") 'hydra-claude/body) ; Alternative (as documented in CLAUDE.md)
(global-set-key (kbd "C-c i") 'hydra-claude/body) ; Alternative
(global-set-key (kbd "<f8>") 'hydra-claude/body)  ; Function key option

;; Enable the minor mode globally for programming buffers
(when (fboundp 'global-tiqsi-claude-repl-mode)
  (global-tiqsi-claude-repl-mode 1))

;; Define when-available if not already defined
(unless (fboundp 'when-available)
  (defmacro when-available (feature &rest body)
    "When FEATURE is available, evaluate BODY."
    `(when (require ,feature nil 'noerror)
       ,@body)))

;; Integration with programming modes - seamless like python-mode
(when-available 'python-mode
  (add-hook 'python-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

(when-available 'rust-mode
  (add-hook 'rust-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

(when-available 'c-mode
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

(when-available 'c++-mode
  (add-hook 'c++-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-e") 'tiqsi-claude-fix-error)
              (local-set-key (kbd "C-c C-a") 'tiqsi-claude-fix-error))))

;; Integration with flycheck for error explanations
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c C-e") 'tiqsi-claude-fix-error))

;; Debug: Check if hydra was defined
(if (fboundp 'hydra-claude/body)
    (message "Tiqsi Claude REPL integration loaded. Hydra defined. Use M-c for hydra menu.")
  (message "ERROR: hydra-claude/body is not defined!"))

;; Debug: Check keybinding
(message "M-c is bound to: %s" (key-binding (kbd "M-c")))

(provide 'modes-claude)

;;; modes-claude.el ends here