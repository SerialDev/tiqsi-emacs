;;; core-performance.el --- Tiqsi core performance utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

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
;; Buffer/Line/Usability performance
;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers

;;; Code:

;; Asynchronous execution

(straight-use-package
  '(dired-async
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

(straight-use-package
  '(async-bytecomp
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

(straight-use-package
  '(async
     :type git
     :host github
     :repo "jwiegley/emacs-async"
     ))

;; (autoload 'dired-async-mode "dired-async.el" nil t)
;; (dired-async-mode 1)
;; (async-bytecomp-package-mode 1)

;; Garbage collection

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq gc-cons-threshold 20000000)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 20000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'helm-minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
(add-hook 'helm-minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq-default bidi-display-reordering nil)
(setq redisplay-dont-pause t)

;; Performance optimizations for large files
(setq-default line-move-visual nil) ; Faster line movement
(setq auto-window-vscroll nil) ; Faster scrolling

;; Auto-revert optimizations (balanced)
(setq auto-revert-interval 2) ; Check every 2 seconds (balanced)
(setq auto-revert-check-vc-info nil) ; Don't check VC (performance)
(setq auto-revert-use-notify t) ; Use file system notifications
(setq global-auto-revert-non-file-buffers t) ; Keep buffers like dired updated

;; Font-lock optimizations
(setq jit-lock-defer-time 0.25) ; Defer fontification
(setq jit-lock-stealth-time 5) ; Fontify when idle
(setq jit-lock-chunk-size 1000) ; Smaller chunks
(setq jit-lock-stealth-nice 0.5)

;; Additional font-lock optimizations for large files
(setq font-lock-maximum-decoration '((t . 2))) ; Reduce decoration level
(setq inhibit-compacting-font-caches t) ; Don't compact font caches during GC

;; Disable expensive features
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)

;; LSP optimizations - balanced for excellent functionality and performance
(with-eval-after-load 'lsp-mode
  ;; Core performance settings - keep LSP responsive but functional
  (setq lsp-idle-delay 0.3) ; Quick response for good UX
  (setq lsp-log-io nil) ; No I/O logging for performance
  (setq lsp-print-performance nil) ; No performance logging
  (setq lsp-inhibit-message nil) ; Keep important messages
  (setq lsp-report-if-no-buffer t) ; Keep buffer reports for debugging
  
  ;; Network/communication optimizations - fix lsp--get-body-length slowdown without breaking functionality
  (setq lsp-response-timeout 30) ; Reasonable timeout - don't kill slow operations
  (setq lsp-tcp-connection-timeout 5) ; Quick connection timeout
  (setq lsp-before-save-edits t) ; Keep formatting on save - important feature
  
  ;; File watching - disable only the most expensive parts
  (setq lsp-enable-file-watchers nil) ; Major performance impact - disable
  (setq lsp-server-trace nil) ; No server tracing
  (setq lsp-semantic-tokens-enable t) ; Keep semantic tokens - very useful for syntax highlighting
  
  ;; UI/Display optimizations - keep useful features
  (setq lsp-enable-folding t) ; Keep folding - useful
  (setq lsp-enable-text-document-color t) ; Keep color - useful for CSS/etc
  (setq lsp-enable-indentation t) ; Keep indentation - essential
  (setq lsp-enable-on-type-formatting nil) ; Disable - can be slow during typing
  
  ;; Modeline/UI elements - keep essential ones
  (setq lsp-modeline-code-actions-enable t) ; Keep code actions - very useful
  (setq lsp-modeline-diagnostics-enable t) ; Keep diagnostics - essential
  (setq lsp-headerline-breadcrumb-enable t) ; Keep breadcrumbs - useful navigation
  (setq lsp-lens-enable nil) ; Disable lens - often slow and not essential
  
  ;; Signature and documentation - keep the most useful parts
  (setq lsp-signature-auto-activate t) ; Keep signatures - very useful
  (setq lsp-signature-render-documentation t) ; Keep signature docs
  (setq lsp-eldoc-enable-hover t) ; Keep hover - essential for development
  (setq lsp-eldoc-render-all nil) ; Don't render everything - performance
  
  ;; Completion optimizations - keep LSP completion but optimize
  (setq lsp-completion-provider :capf) ; Use LSP completion - essential feature
  (setq lsp-prefer-capf t) ; Prefer capf for better integration
  
  ;; Diagnostics - keep full functionality
  (setq lsp-diagnostics-provider :flycheck) ; Use flycheck
  (setq lsp-flycheck-live-reporting t) ; Keep live reporting - useful
  
  ;; Features to keep for excellent development experience
  (setq lsp-enable-snippet t) ; Keep snippets - very useful
  (setq lsp-enable-symbol-highlighting t) ; Keep highlighting - useful
  (setq lsp-enable-links t) ; Keep links - useful for navigation
  (setq lsp-enable-imenu t) ; Keep imenu - useful for navigation
  
  ;; Workspace management - keep alive for better performance
  (setq lsp-keep-workspace-alive t) ; Keep alive - better for multi-file projects
  (setq lsp-restart 'auto-restart) ; Auto-restart for reliability
  (setq lsp-auto-guess-root t) ; Auto-guess for convenience
  
  ;; JSON parsing optimizations for lsp--get-body-length - performance only
  (setq lsp-use-plists t) ; Use plists - faster parsing
  
  ;; Auto-configuration - keep enabled but optimized
  (setq lsp-auto-configure t) ; Keep auto-configure - essential for ease of use
  (setq lsp-enable-suggest-server-download t)) ; Keep suggestions - helpful

;; LSP-UI optimizations - balanced for functionality and performance
(with-eval-after-load 'lsp-ui
  ;; Sideline - keep but optimize timing
  (setq lsp-ui-sideline-enable t) ; Keep sideline - very useful
  (setq lsp-ui-sideline-show-hover nil) ; Don't show hover in sideline - reduces noise
  (setq lsp-ui-sideline-show-diagnostics t) ; Show diagnostics - essential
  (setq lsp-ui-sideline-show-code-actions t) ; Show code actions - very useful
  (setq lsp-ui-sideline-delay 0.3) ; Quick delay for responsiveness
  (setq lsp-ui-sideline-update-mode 'line) ; Update per line for performance
  
  ;; Documentation - keep but optimize
  (setq lsp-ui-doc-enable t) ; Keep documentation - essential
  (setq lsp-ui-doc-delay 0.3) ; Quick delay for good UX
  (setq lsp-ui-doc-include-signature t) ; Include signatures - very useful
  (setq lsp-ui-doc-position 'at-point) ; Show at point
  (setq lsp-ui-doc-max-width 60) ; Limit width for performance
  (setq lsp-ui-doc-max-height 20) ; Limit height for performance
  
  ;; Peek functionality - keep all, very useful
  (setq lsp-ui-peek-enable t) ; Keep peek - excellent for navigation
  (setq lsp-ui-peek-always-show t) ; Always show peek
  
  ;; Integrations - keep useful ones
  (setq lsp-ui-imenu-enable t) ; Keep imenu - useful for navigation
  (setq lsp-ui-flycheck-enable t)) ; Keep flycheck - essential for diagnostics

;; Company mode optimizations
(with-eval-after-load 'company
  (setq company-idle-delay 0.5)
  (setq company-minimum-prefix-length 3))

;; Python specific optimizations - prevent blocking during file operations
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-indent-guess-indent-offset nil) ; Don't guess indentation
  (setq python-shell-completion-native-enable nil) ; Disable native completion
  (setq python-shell-prompt-detect-enabled nil) ; Don't detect prompts
  (setq python-eldoc-setup-code nil) ; Don't setup eldoc
  (setq python-eldoc-string-code nil)) ; No eldoc strings

;; Disable semantic mode (often causes performance issues)
(setq semantic-mode nil)

;; File system monitoring optimizations - major cause of slowdowns in large projects
(setq auto-revert-use-notify nil) ; Don't use file system notifications
(setq auto-revert-avoid-polling t) ; Avoid polling when possible
(setq auto-revert-check-vc-info nil) ; Don't check VC info
(setq auto-revert-interval 10) ; Increase check interval

;; Disable global file watching features that block in large projects
(setq create-lockfiles nil) ; Don't create lock files
(setq make-backup-files nil) ; Don't create backup files during operations
(setq version-control nil) ; No version control backup interference

;; Large file optimizations
(defun my/optimize-large-file-performance ()
  "Optimize performance for large files."
  (when (> (buffer-size) (* 1024 1024)) ; > 1MB
    (setq-local bidi-display-reordering nil)
    (setq-local cursor-type nil)
    (setq-local line-move-visual nil)
    (setq-local auto-hscroll-mode 'current-line)
    (when (> (buffer-size) (* 5 1024 1024)) ; > 5MB
      (setq-local font-lock-mode nil)
      (setq-local show-trailing-whitespace nil))))

(add-hook 'find-file-hook 'my/optimize-large-file-performance)

;; Project and directory scanning optimizations - prevent blocking during file operations
(with-eval-after-load 'projectile
  (setq projectile-enable-caching t) ; Use caching
  (setq projectile-indexing-method 'alien) ; Use external tools
  (setq projectile-generic-command "find . -type f -print0") ; Simple find
  (setq projectile-git-command "git ls-files -zco --exclude-standard") ; Simple git
  (setq projectile-require-project-root nil)) ; Don't require project root

;; Helm optimizations (very conservative to ensure functionality)
(with-eval-after-load 'helm
  (setq helm-idle-delay 0.01) ; Small delay
  (setq helm-input-idle-delay 0.1) ; Allow typing
  (setq helm-candidate-number-limit 200) ; Reasonable number
  (setq helm-full-frame nil) ; Don't use full frame
  (setq helm-split-window-default-side 'below) ; Predictable window split
  ;; Smex specific settings
  (setq helm-M-x-fuzzy-match t) ; Fuzzy matching
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-recentf-fuzzy-match t)
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (setq smex-history-length 50) ; Reasonable history
  (setq recentf-max-saved-items 200)) ; Reasonable number of recent files

;; IDO optimizations (conservative - don't break functionality)
(with-eval-after-load 'ido
  ;; Keep original settings - the slowdown is likely not IDO itself
  (setq ido-save-directory-list-file (concat user-emacs-directory "ido.last"))
  (setq ido-auto-merge-work-directories-length -1) ; Disable auto-merge
  (setq ido-max-directory-size 300000)) ; Handle larger directories

;; Beacon optimizations
(with-eval-after-load 'beacon
  (setq beacon-blink-duration 0.3) ; Shorter blink
  (setq beacon-blink-delay 0.3) ; Shorter delay
  (setq beacon-size 20) ; Smaller beacon
  (setq beacon-dont-blink-major-modes '(t))) ; Disable in certain modes

;; Flycheck optimizations
(with-eval-after-load 'flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-idle-change-delay 4) ; Wait longer before checking
  (setq flycheck-display-errors-delay 0.3)) ; Faster error display
(setq toggle-truncate-lines t)


(defun toggle-truncate-lines ()
  "Toggle whether to wrap lines at right window border."
  (interactive)
  (if (eq truncate-lines nil)
    (set-variable 'truncate-lines 't)
    (set-variable 'truncate-lines nil)
    ) )

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
    (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    ))


(when (require-soft 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21

;; ------------------------------------------------------------------------- ;

(defmacro safe-load-package (package-code)
  `(let ((debug-on-error nil)
          (inhibit-debugger t)) ;; Ensure the debugger is inhibited
     (condition-case err
       (progn
         ,package-code
         (message "Successfully loaded or executed: %s" ',package-code))
       (error (message "Error loading package or executing code: %s" err)))))

(defmacro safe-execute (code)
  `(condition-case nil
     ,code
     (error (message "Failed to execute: %s, but continuing..." ',code))))


(safe-load-package
  (straight-use-package
    '(explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")))


(safe-execute (explain-pause-mode))


;; ------------------------------------------------------------------------- ;


(defun clear-buffer-long-printouts-line()
  (if (>
        (save-excursion
          (goto-char (point-max))
          (string-to-number (format-mode-line "%l")))
        10000
        )
    (comint-clear-buffer)
    nil))


(defun clear-buffer-long-printouts-col()
  (if (>
        (save-excursion
          (goto-char (point-max))
          (string-to-number (format-mode-line "%c")))
        10000
        )
    (comint-clear-buffer)
    nil))

(defun clear-buffer-long-printouts()
  (interactive)
  (progn
    (clear-buffer-long-printouts-line)
    (clear-buffer-long-printouts-col)))



(defun long-printout-teardown()
  (run-with-timer
    0 5 'clear-buffer-long-printouts))


;;;###autoload
(define-minor-mode clear-huge-repl-mode
  "clear massive printouts to not slow emacs to a crawl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c f") 'clear-buffer-long-printouts)
            map))

;;;###autoload
(progn
  (add-hook 'comint-mode-hook 'clear-huge-repl-mode)
  (add-hook 'inferior-python-mode 'clear-huge-repl-mode)
  (add-hook 'clear-huge-repl-mode-hook 'long-printout-teardown)
  )


(setq font-lock-verbose nil)


(setq max-specpdl-size 5000)  ; default is 1000, increase the backtrace level


;; ── 1.  *Always* use the OS notify backend if available ─────────────────────
(setq auto-revert-use-notify t             ; prefer inotify / FSEvents / kqueue
  auto-revert-verbose nil              ; no echo area spam
  auto-revert-interval 5)              ; polling fallback every 5 s

;; ── 2.  Revert only real files; skip VC status & remote buffers ─────────────
(setq auto-revert-check-vc-info nil)       ; don’t ask Git each cycle
(setq auto-revert-remote-files nil)        ; TRAMP polling is expensive

;; Optional: keep non-file buffers in sync (e.g. *vc-dir*), set to t if you need it
(setq global-auto-revert-non-file-buffers nil)

;; ── 3.  Skip specific modes / buffers that are large or unimportant ─────────
(defun my/auto-revert-ignore-buffer-p (buf)
  "Return non-nil if BUF should be ignored by auto-revert."
  (with-current-buffer buf
    (or (string-match-p "\\*helm" (buffer-name))      ; Helm temps
      (string-match-p "\\*Messages\\*" (buffer-name))
      (> (buffer-size) 5e6))))                      ; >5 MB → skip
;(add-to-list 'global-auto-revert-ignore-buffer 'my/auto-revert-ignore-buffer-p)

;; ── 4.  Silence unnecessary modeline updates ────────────────────────────────
(setq auto-revert-stop-on-user-input nil)  ; keep running during idle typing


;;                                BEACON perf                                ;
;; ------------------------------------------------------------------------- ;

;; ------------------------------------------------------------------------- ;


(provide 'core-performance)

;;; core-performance.el ends here
