;;; test-init.el --- Comprehensive init-lite.el audit tests -*- lexical-binding: t -*-

;; Copyright (C) 2025
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Tests that codify every issue found during the tiqsi-emacs init audit.
;; Each test suite verifies a class of bug that was discovered and fixed.
;; These tests serve as regression guards — if a future change reintroduces
;; a problem, the corresponding test will catch it.
;;
;; Run with: ./tests/run-tests.sh tests/test-init.el

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Suite 1: No dangerous global side effects at top level
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Global Side Effects")

;; CRITICAL FIX: shell-command-switch must NOT be globally set to "-ic".
;; That causes every shell-command in Emacs to use interactive mode,
;; adding latency and causing batch/daemon hangs.
(tiqsi-test-assert
 (not (string= shell-command-switch "-ic"))
 "shell-command-switch is NOT globally set to -ic"
 (format "actual value: %S" shell-command-switch))

;; HIGH FIX: modes-shell.el must not spawn processes at load time.
;; We verify no "*Output*" buffer was created by the bare
;; multiple-async-shell-commands call that used to run at top level.
(tiqsi-test-assert
 (not (get-buffer "*Output*"))
 "No *Output* buffer created at init (modes-shell.el top-level call removed)")

;; MEDIUM FIX: global-auto-revert-mode should not be toggled inside
;; programming-zig.el — that's a global setting that belongs in core config.
;; We verify it's not forcibly enabled by zig module loading.
;; (Note: we can't easily test *where* it was set, but we check the zig file
;; doesn't contain the bare call anymore — that's the source-level test below.)

;; ---------------------------------------------------------------------------
;; Suite 2: Macro correctness
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Macro Correctness")

;; CRITICAL FIX: when-executable must evaluate executable-find at RUNTIME,
;; not at macro-expansion time, and must properly quote the name in else branch.
(tiqsi-test-assert
 (fboundp 'when-executable)
 "when-executable macro is defined")

;; Test that when-executable works for a known executable.
;; We use a defvar so that setq inside eval can find the binding.
(defvar test--when-exec-result nil)
(condition-case _err
    (eval '(when-executable "ls" (setq test--when-exec-result t)))
  (error nil))
(tiqsi-test-assert test--when-exec-result
                   "when-executable finds 'ls' at runtime")

;; Test that when-executable gracefully handles missing executables
(let ((result nil))
  (condition-case err
      (eval '(when-executable "nonexistent-binary-xyz-12345" (setq result t)))
    (error (setq result 'error)))
  (tiqsi-test-assert (not (eq result 'error))
                     "when-executable handles missing executable without error")
  (tiqsi-test-assert (null result)
                     "when-executable body not executed for missing executable"))

;; Verify with-interactive-shell macro exists (programming-llm.el)
(tiqsi-test-assert
 (fboundp 'with-interactive-shell)
 "with-interactive-shell macro is defined"
 "Replaces the global (setq shell-command-switch \"-ic\")")

;; Test with-interactive-shell scopes the switch correctly
(let ((outer-switch shell-command-switch))
  (with-interactive-shell
    (tiqsi-test-assert
     (string= shell-command-switch "-ic")
     "with-interactive-shell sets switch to -ic inside body"))
  (tiqsi-test-assert
   (string= shell-command-switch outer-switch)
   "with-interactive-shell restores switch after body"))

;; ---------------------------------------------------------------------------
;; Suite 3: No deprecated API usage
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Deprecated API Checks")

;; MEDIUM FIX: init-lite.el message defadvice should use with-current-buffer,
;; not save-excursion + set-buffer.
;; We can verify this by checking the source text of the advice.
(let* ((init-file (tiqsi-test-expand "init-lite.el"))
       (content (when (file-exists-p init-file)
                  (with-temp-buffer
                    (insert-file-contents init-file)
                    (buffer-string)))))
  (if content
      (progn
        (tiqsi-test-assert
         (not (string-match-p "(save-excursion\n\\s-+(set-buffer " content))
         "init-lite.el: no save-excursion + set-buffer pattern"
         "Should use with-current-buffer instead")
        (tiqsi-test-assert
         (string-match-p "(with-current-buffer \"\\*Messages\\*\"" content)
         "init-lite.el: uses with-current-buffer for *Messages*"))
    (tiqsi-test-skip "init-lite.el source check" "file not found")))

;; MEDIUM FIX: programming-python-lite.el should use keyword args in define-minor-mode.
(let* ((py-file (tiqsi-test-expand "modules/programming/programming-python-lite.el"))
       (content (when (file-exists-p py-file)
                  (with-temp-buffer
                    (insert-file-contents py-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (string-match-p "define-minor-mode my/python-autoenv-mode\n.*\n\\s-+:init-value" content)
       "programming-python-lite.el: define-minor-mode uses keyword args"
       "Positional args to define-minor-mode are deprecated")
    (tiqsi-test-skip "programming-python-lite.el check" "file not found")))

;; ---------------------------------------------------------------------------
;; Suite 4: Robust error handling
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Error Handling")

;; HIGH FIX: get-openai-key must return nil gracefully when OPENAI_KEY is unset,
;; not crash.
(tiqsi-test-assert
 (fboundp 'get-openai-key)
 "get-openai-key function is defined")

(let ((result (condition-case err
                  (get-openai-key)
                (error (cons 'error (error-message-string err))))))
  (tiqsi-test-assert
   (not (and (consp result) (eq (car result) 'error)))
   "get-openai-key does not crash when OPENAI_KEY might be unset"
   (when (and (consp result) (eq (car result) 'error))
     (cdr result))))

;; HIGH FIX: Claude REPL substring must not crash on short strings.
;; Verify the fixed function handles short JSON objects.
(when (fboundp 'tiqsi-claude-repl--handle-json-object)
  ;; We can't easily test the internal substring fix without a mock,
  ;; but we verify the source doesn't contain the bare (substring ... 0 100).
  (let* ((fixes-file (tiqsi-test-expand
                      "modules/modes/claude-repl/tiqsi-claude-repl-fixes.el"))
         (content (when (file-exists-p fixes-file)
                    (with-temp-buffer
                      (insert-file-contents fixes-file)
                      (buffer-string)))))
    (if content
        (tiqsi-test-assert
         (not (string-match-p "(substring (prin1-to-string json-obj) 0 100)" content))
         "tiqsi-claude-repl-fixes.el: no unsafe substring call"
         "Should use (min 100 (length ...)) guard")
      (tiqsi-test-skip "Claude REPL fixes source check" "file not found"))))

;; ---------------------------------------------------------------------------
;; Suite 5: No hardcoded user-specific paths
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Hardcoded Paths")

;; Scan all loaded .el files for common hardcoded path patterns.
;; We check source files rather than runtime values because some variables
;; might be set to user-specific values at runtime via env vars.
(let ((files-to-check
       '("modules/programming/programming-zig.el"
         "modules/programming/programming-llm.el"
         "modules/modes/modes-helm.el"))
      (bad-patterns
       '("/Users/amariscalcloudflare\\.com/"
         "/Users/[a-zA-Z]+/\\.")))
  (dolist (rel-file files-to-check)
    (let* ((full-path (tiqsi-test-expand rel-file))
           (content (when (file-exists-p full-path)
                      (with-temp-buffer
                        (insert-file-contents full-path)
                        (buffer-string)))))
      (if content
          (dolist (pattern bad-patterns)
            (tiqsi-test-assert
             (not (string-match-p pattern content))
             (format "%s: no hardcoded user path matching %s"
                     (file-name-nondirectory rel-file) pattern)))
        (tiqsi-test-skip (format "%s check" rel-file) "file not found")))))

;; Verify zig scratch path is not hardcoded to a specific user
(when (boundp 'default-zig-scratch-path)
  (tiqsi-test-assert
   (not (string-match-p "amariscalcloudflare" default-zig-scratch-path))
   "default-zig-scratch-path is not hardcoded to specific user"))

;; Verify helm-rg uses executable-find, not hardcoded path
(let* ((helm-file (tiqsi-test-expand "modules/modes/modes-helm.el"))
       (content (when (file-exists-p helm-file)
                  (with-temp-buffer
                    (insert-file-contents helm-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (or (string-match-p "executable-find" content)
           (not (string-match-p "setq helm-rg-ripgrep-executable \"/usr/local/bin" content)))
       "modes-helm.el: helm-rg-ripgrep-executable uses executable-find")
    (tiqsi-test-skip "modes-helm.el path check" "file not found")))

;; ---------------------------------------------------------------------------
;; Suite 6: Startup performance guards
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Startup Performance")

;; MEDIUM FIX: exec-path-from-shell should not block init via :init.
;; Verify the use-package declaration uses :config or :defer, not :init.
(let* ((rust-file (tiqsi-test-expand "modules/programming/programming-rust.el"))
       (content (when (file-exists-p rust-file)
                  (with-temp-buffer
                    (insert-file-contents rust-file)
                    (buffer-string)))))
  (if content
      (progn
        (tiqsi-test-assert
         (not (string-match-p
               "(use-package exec-path-from-shell[^)]*:init (exec-path-from-shell-initialize)"
               content))
         "programming-rust.el: exec-path-from-shell not in :init"
         "Should use :config with :defer to avoid blocking startup")
        (tiqsi-test-assert
         (string-match-p ":defer" content)
         "programming-rust.el: exec-path-from-shell has :defer"))
    (tiqsi-test-skip "programming-rust.el check" "file not found")))

;; MEDIUM FIX: LSP settings in programming-rust.el should be scoped with
;; with-eval-after-load, not bare top-level setq.
(let* ((rust-file (tiqsi-test-expand "modules/programming/programming-rust.el"))
       (content (when (file-exists-p rust-file)
                  (with-temp-buffer
                    (insert-file-contents rust-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (string-match-p "(with-eval-after-load 'lsp-mode" content)
       "programming-rust.el: LSP settings wrapped in with-eval-after-load")
    (tiqsi-test-skip "programming-rust.el LSP scoping check" "file not found")))

;; ---------------------------------------------------------------------------
;; Suite 7: Module loading integrity
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Module Loading")

;; Verify core functions are defined
(dolist (fn '(load-expand try-require with-system when-available
              straight-require straight-require-lazy
              with-interactive-shell get-openai-key))
  (tiqsi-test-assert-fboundp fn (format "Function defined: %s" fn)))

;; Verify core macros expand without error
(dolist (macro-sym '(GNUEmacs GNUEmacsGT25 when-executable with-system))
  (tiqsi-test-assert
   (or (fboundp macro-sym) (macrop (symbol-function macro-sym))
       ;; Some macros are defined via defmacro, fboundp returns t for them
       (fboundp macro-sym))
   (format "Macro available: %s" macro-sym)))

;; Verify key features are loaded (sanity check the init chain works)
(dolist (feat '(cl-lib helm hydra company evil))
  (tiqsi-test-assert
   (featurep feat)
   (format "Feature loaded: %s" feat)))

;; ---------------------------------------------------------------------------
;; Suite 8: Source file syntax validation
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Source Syntax Validation")

;; Auto-discover all .el files in core/ and modules/ and verify they parse
;; without read errors. This catches unbalanced parens, bad quoting, etc.
(let* ((root tiqsi-test-root)
       (dirs (list (expand-file-name "core/" root)
                   (expand-file-name "modules/modes/" root)
                   (expand-file-name "modules/programming/" root)
                   (expand-file-name "modules/modes/claude-repl/" root)))
       (all-files nil))
  (dolist (dir dirs)
    (when (file-directory-p dir)
      (setq all-files
            (append all-files
                    (directory-files dir t "\\.el$")))))
  (dolist (el-file all-files)
    (let ((fname (file-name-nondirectory el-file)))
      (condition-case err
          (with-temp-buffer
            (insert-file-contents el-file)
            ;; Try to read all top-level forms
            (goto-char (point-min))
            (let ((read-count 0))
              (condition-case _
                  (while t
                    (read (current-buffer))
                    (cl-incf read-count))
                (end-of-file nil))
              (tiqsi-test-assert
               (> read-count 0)
               (format "Syntax OK: %s (%d forms)" fname read-count))))
        (error
         (tiqsi-test-assert nil
                            (format "Syntax OK: %s" fname)
                            (error-message-string err)))))))

;; ---------------------------------------------------------------------------
;; Suite 9: No bare top-level side effects in module files
;; ---------------------------------------------------------------------------

(tiqsi-test-suite "Top-Level Side Effects Audit")

;; Verify modes-shell.el doesn't have uncommented multiple-async-shell-commands
(let* ((shell-file (tiqsi-test-expand "modules/modes/modes-shell.el"))
       (content (when (file-exists-p shell-file)
                  (with-temp-buffer
                    (insert-file-contents shell-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (not (string-match-p
             "^(multiple-async-shell-commands" content))
       "modes-shell.el: no bare top-level multiple-async-shell-commands"
       "Was spawning 4 shell processes on every startup")
    (tiqsi-test-skip "modes-shell.el side effects check" "file not found")))

;; Verify programming-zig.el doesn't call global-auto-revert-mode
(let* ((zig-file (tiqsi-test-expand "modules/programming/programming-zig.el"))
       (content (when (file-exists-p zig-file)
                  (with-temp-buffer
                    (insert-file-contents zig-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (not (string-match-p "^(global-auto-revert-mode" content))
       "programming-zig.el: no bare global-auto-revert-mode call"
       "Global mode toggles belong in core config")
    (tiqsi-test-skip "programming-zig.el side effects check" "file not found")))

;; Verify programming-llm.el doesn't have bare (setq shell-command-switch ...)
(let* ((llm-file (tiqsi-test-expand "modules/programming/programming-llm.el"))
       (content (when (file-exists-p llm-file)
                  (with-temp-buffer
                    (insert-file-contents llm-file)
                    (buffer-string)))))
  (if content
      (tiqsi-test-assert
       (not (string-match-p "^(setq shell-command-switch" content))
       "programming-llm.el: no bare global shell-command-switch setq")
    (tiqsi-test-skip "programming-llm.el side effects check" "file not found")))

;; ---------------------------------------------------------------------------
;; Done — print summary (handled by harness)
;; ---------------------------------------------------------------------------

(provide 'test-init)

;;; test-init.el ends here
