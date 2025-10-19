;;; tiqsi-claude-repl.el --- Claude REPL integration main file -*- lexical-binding: t -*-

;;; Commentary:
;; Main entry point for Claude REPL integration in Tiqsi Emacs
;; This file loads all the components of the Claude REPL system

;;; Code:

;; First, define the custom group and variables that other files depend on
(defgroup tiqsi-claude-repl nil
  "Claude REPL integration for Tiqsi Emacs."
  :group 'tiqsi
  :prefix "tiqsi-claude-repl-")

(defcustom tiqsi-claude-repl-program "claude"
  "Path to the Claude CLI executable."
  :type 'string
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-clean-output t
  "Whether to clean Unicode/ANSI from Claude output."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-show-thinking t
  "Whether to show Claude's thinking process."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-highlight-code t
  "Whether to syntax highlight code blocks in responses."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-highlight-inline-code t
  "Whether to syntax highlight inline code in responses."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-show-line-numbers t
  "Whether to show line numbers in code blocks."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-history-directory
  (expand-file-name "~/.tiqsi-claude-history/")
  "Directory to store Claude conversation history."
  :type 'directory
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-save-history t
  "Whether to save conversation history."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-history-max-size 1000
  "Maximum number of conversations to keep in history."
  :type 'integer
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-use-smart-context t
  "Whether to automatically include project context."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-animate-thinking t
  "Whether to animate the thinking indicator."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-auto-recover-prompt t
  "Whether to automatically recover missing prompts after responses."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-wrap-column 100
  "Column at which to wrap long lines in Claude responses."
  :type 'integer
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-auto-wrap t
  "Whether to automatically wrap long lines in Claude responses."
  :type 'boolean
  :group 'tiqsi-claude-repl)

(defcustom tiqsi-claude-repl-model nil
  "Model to use for Claude sessions.
Options include 'sonnet', 'opus', or full model names.
Set to nil to use the default model."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "Sonnet" "sonnet")
                 (const :tag "Opus" "opus")
                 (string :tag "Custom model name"))
  :group 'tiqsi-claude-repl)

;; Load the component files
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-ui.el")
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-core.el")
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-features.el")

;; Load permission control system
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-permissions.el")

;; Load tool helpers
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-tool-helpers.el")

;; Load mode display (simple colored indicator)
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-mode-display.el")

;; Load all consolidated fixes
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-fixes.el")

;; Load simple syntax highlighting
(load-expand "modules/modes/claude-repl/tiqsi-claude-repl-simple-highlight.el")

;; Provide the main feature
(provide 'tiqsi-claude-repl)

;;; tiqsi-claude-repl.el ends here