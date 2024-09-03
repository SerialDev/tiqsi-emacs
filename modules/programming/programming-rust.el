;;; programming-rust.el --- Tiqsi Rust programming support  -*- lexical-binding: t -*-

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
;;

(straight-require 'hydra)
(straight-require 'rust-mode)
(straight-require 'cargo)
(straight-require 'racer)
(straight-require 'parsec)

(straight-use-package
  '(evcxr
     :type git
     :host github
     :repo "serialdev/evcxr-mode"
     :config
     (add-hook 'rust-mode-hook #'evcxr-minor-mode)
     ))

;; RACER HAS BEEN DEPRECATED
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; (add-hook
;;   'rust-mode-hook
;;   '(lambda ()
;;      (setq tab-width 2)
;;      ;; (setq racer-cmd (concat (getenv "HOME") "/cargo/bin/racer")) ;; Rustup binaries PATH
;;      ;; (setq racer-rust-src-path (concat (getenv "HOME") (shell-command-to-string "echo `rustc --print sysroot`/lib/rustlib/src/rust/src")))
;;      (setq company-tooltip-align-annotations t)
;;      (add-hook 'rust-mode-hook #'racer-mode)
;;      (add-hook 'racer-mode-hook #'eldoc-mode)
;;      (add-hook 'racer-mode-hook #'company-mode)
;;      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;;      (add-hook 'rust-mode-hook 'cargo-minor-mode)
;;      (setq rust-format-on-save t)
;;      (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
;;      (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;; (with-system gnu/linux

;;   (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "which cargo") ) )))
;;   (setq racer-rust-src-path
;;     (concat (string-trim
;;               (shell-command-to-string "rustc --print sysroot"))
;;       "/lib/rustlib/src/rust/src"))
;;   (setq racer-cmd (s-prepend (file-name-directory (shell-command-to-string "which racer")) "racer")  )

;;   )




;; (when tiqsi-win32
;;   (setq racer-cmd (s-prepend (file-name-directory (shell-command-to-string "where racer")) "racer.exe")  )
;;   (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "where cargo"))) ))
;;   (setq racer-rust-src-path
;;     (concat (string-trim
;;               (shell-command-to-string "rustc --print sysroot"))
;;       "/lib/rustlib/src/rust/src"))
;;   )


;; (with-system darwin
;;   (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "which cargo") ) )))
;;   (setq racer-rust-src-path
;;     (concat (string-trim
;;               (shell-command-to-string "rustc --print sysroot"))
;;       "/lib/rustlib/src/rust/src"))

;;   (if-command-exists "racer"
;;     (setq racer-cmd
;;       (s-prepend (file-name-directory
;;                    (shell-command-to-string "which racer")) "racer")  )
;;     ))

                                        ;-{Static checking}-;


(setq flymake-rust-use-cargo 1)

(defun tiqsi/cargo-doc-tree()
  "Get the mccabe complexity for this buffer."
  (interactive)
  (message
    (shell-command-to-string(message "tree -d %starget/doc -L 1 " (projectile-project-root)))))


(defun cargo-process-run-optimized()
  (interactive)
  (compile "cargo run --release"))

(defun cargo-process-build-optimized()
  (interactive)
  (compile "cargo build --release"))



                                        ;-----{Hydras }-----;

(defhydra hydra-rust (:color pink :hint nil)
  "
^Rust Cargo commands^
----------------------------------------------------------------------------------
_r_: Run          _i_: Init          _u_: Update               _+r_: Release O
_x_: Run-example  _n_: New           _c_: Repeat               _+b_: Build O
_b_: Build        _f_: Current-test  _e_: Bench
_l_: Clean        _s_: Search        _o_: Current-file-tests
_d_: Doc          _t_: Test          _m_: Fmt
_|_: Doc Tree     _k_: Check         _q_: Clippy
"
  ("e"   cargo-process-bench :color blue)
  ("b"   cargo-process-build :color blue)
  ("l"   cargo-process-clean :color blue)
  ("d"   cargo-process-doc :color blue)
  ("n"   cargo-process-new :color blue)
  ("i"   cargo-process-init :color blue)
  ("r"   cargo-process-run :color blue)
  ("x"   cargo-process-run-example :color blue)
  ("s"   cargo-process-search :color blue)
  ("t"   cargo-process-test :color blue)
  ("u"   cargo-process-update :color blue)
  ("c"   cargo-process-repeat :color blue)
  ("f"   cargo-process-current-test :color blue)
  ("o"   cargo-process-current-file-tests :color blue)
  ("m"   cargo-process-fmt :color blue)
  ("+r" cargo-process-run-optimized :color blue)
  ("+b" cargo-process-build-optimized :color blue)
  ("|"   tiqsi/cargo-doc-tree :color blue)
  ("k"   cargo-process-check color: red)
  ("q" cargo-process-clippy :color blue)
  ("ESC" nil "Exit"))



;; (defun current-line ()
;;   (string-to-number (car (cdr(s-split " " (what-line)))))
;;   )

;; (format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name)))

;; (defun tiqsi--rust-print-src()
;;   (interactive)
;;   (let ((match-end  (s-split "END"   (shell-command-to-string (format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name))) ))))
;;     (if (s-equals? (car match-end) "")
;;       1
;;       (let ((match-match (s-trim(car(cdr(s-split "MATCH" (car match-end)))))  ))
;;         (cl-multiple-value-bind
;;           (name row col path type extra)
;;           (s-split ","  match-match )

;;           (with-temp-buffer
;;             (insert-file-contents path)
;;             (goto-line (string-to-number row))
;;             (print (buffer-substring-no-properties (point)
;;                      (search-forward "}" )))
;;             )
;;           )
;;         ))))



;; (defun tiqsi--racer-insert-struct-point()
;;   (interactive)
;;   (let (  (b-name (buffer-name)) (match-end  (s-split "END"   (shell-command-to-string (format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name))) ))))
;;     ;; (let ((match-end  (s-split "END"  ttt )) (b-name (buffer-name)))
;;     (if (s-equals? (car match-end) "")
;;       1
;;       (let ((match-match (s-trim(car(cdr(s-split "MATCH" (car match-end)))))  ))
;;         (cl-multiple-value-bind
;;           (name row col path type extra)
;;           (s-split ","  match-match )

;;           (with-temp-buffer
;;             (insert-file-contents path)
;;             (goto-line (string-to-number row))

;;             (let ((mm
;;                     (buffer-substring-no-properties (point)
;;                       (search-forward "}" )) ))
;;               (with-current-buffer b-name
;;                 (backward-kill-word 1)
;;                 (insert name)
;;                 (insert "{")
;;                 (newline)
;;                 (cl-loop for k in
;;                   (cl-loop for kv in
;;                     (cl-remove-if  (lambda(item) (string= "" item)) (mapcar 's-trim (s-split ","  (cadr(s-split "{" (car(s-split "}" mm)))))))
;;                     collect (car (s-split ":" kv))
;;                     )
;;                   do (progn
;;                        (insert k)
;;                        (insert ":")
;;                        (insert "  ,")
;;                        (newline)
;;                        )
;;                   )
;;                 (insert "}")
;;                 ))
;;             ))))))

;; (string-to-number (format-mode-line "%l"))

;; (string-to-number (format-mode-line "%c"))



;; (defun racer-ui-tooltip ()
;;   "Show the docstring in a tooltip.
;; The tooltip's face is `racer-tooltip'
;; See `racer-describe'."
;;   (interactive)
;;   (-some-> (symbol-at-point)
;;     (symbol-name)
;;     (racer--describe)
;;     (with-current-buffer (concat "\n" (buffer-string) "\n\n"))
;;     (message 'racer-tooltip nil nil 1000)))

(defun slot/lsp-show-type-signature ()
  "Show the type signature for the thing at
point.  This is essentially what
`lsp-clients-extract-signature-on-hover'
does, just as an extra function."
  (interactive)
  (message
    (slot/syntax-highlight-string
      (slot/lsp-get-type-signature-at-point)
      major-mode)))


(advice-add #'lsp-eldoc-function :after (lambda (&rest _) (setq lsp--hover-saved-bounds nil)))
;; extract and show short signature for rust-analyzer
(cl-defmethod lsp-clients-extract-signature-on-hover (contents (_server-id (eql rust-analyzer)))
  "Get LANGs type signature in STR.
Original implementation from https://github.com/emacs-lsp/lsp-mode/pull/1740."
  (let* ((value (if lsp-use-plists (plist-get contents :value) (gethash "value" contents)))
          (groups (--partition-by (s-blank? it) (s-lines (s-trim value))))
          (mod-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-third-item groups))
                       ((s-equals? "```rust" (car (-third-item groups))) (-first-item groups))
                       (t nil)))
          (cmt (if (null mod-group) "" (concat " // " (cadr mod-group))))
          (sig-group (cond ((s-equals? "```rust" (car (-fifth-item groups))) (-fifth-item groups))
                       ((s-equals? "```rust" (car (-third-item groups))) (-third-item groups))
                       (t (-first-item groups))))
          (sig (->> sig-group
                 (--drop-while (s-equals? "```rust" it))
                 (--take-while (not (s-equals? "```" it)))
                 (--map (s-replace-regexp "//.*" "" it))
                 (--map (s-trim it))
                 (s-join " "))))
    (lsp--render-element (concat "```rust\n" sig cmt "\n```"))))

(defun slot/lsp-get-type-signature (lang str)
  (let* ((start (concat "```" lang))
          (groups (--filter (s-equals? start (car it))
                    (-partition-by #'s-blank? (s-lines (s-trim str)))))
          (name-at-point (symbol-name (symbol-at-point)))
          (type-sig-group (car
                            (--filter (s-contains? name-at-point (cadr it))
                              groups))))
    (->> (or type-sig-group (car groups))
      (-drop 1)                    ; ``` LANG
      (-drop-last 1)               ; ```
      (-map #'s-trim)
      (s-join " "))))




(defun slot/syntax-highlight-string (str mode)
  "Syntax highlight STR in MODE."
  (with-temp-buffer
    (insert str)
    ;; We definitely don't want to call certain modes, so delay the mode's
    ;; hooks until we have removed them.
    (delay-mode-hooks (funcall mode))
    (-map #'funcall
      (--remove (-contains? '(lsp-mode lsp-deferred) it)
        (-mapcat #'symbol-value delayed-mode-hooks)))
    ;; Now we can propertise the string.
    (font-lock-ensure)
    (buffer-string)))

(cl-defmethod lsp-clients-extract-signature-on-hover
  (contents (_server-id (eql rust-analyzer))) ; Only for Rust.
  "Display the type signature of the function at point."
  (slot/syntax-highlight-string
    (slot/lsp-get-type-signature "rust" (plist-get contents :value))
    'rustic-mode))

(defun slot/lsp-get-type-signature-at-point (&optional lang)
  "Get LANGs type signature at point.
LANG is not given, get it from `lsp--buffer-language'."
  (interactive)
  (-some->> (lsp--text-document-position-params)
    (lsp--make-request "textDocument/hover")
    lsp--send-request
    lsp:hover-contents
    (funcall (-flip #'plist-get) :value)
    (slot/lsp-get-type-signature (or lang lsp--buffer-language))))

;; For debugging
(defun display-active-modes-and-functions ()
  "Display the active modes and functions called on cursor hover."
  (let ((major-mode-str (format "Major mode: %s" major-mode))
         (minor-modes-str (format "Active minor modes: %s" 
                            (mapconcat 'prin1-to-string minor-mode-list ", "))))
    (message "%s | %s" major-mode-str minor-modes-str)))

;; ;; Add the function to the post-command-hook
;; (add-hook 'post-command-hook 'display-active-modes-and-functions)
;; (remove-hook 'post-command-hook 'display-active-modes-and-functions)

(defvar hover-debug-mode nil "Flag to track if hover debug mode is active.")


(defun hover-debug-toggle ()
  "Toggle the hover debug mode on and off."
  (interactive)
  (if hover-debug-mode
    (progn
      (remove-hook 'post-command-hook 'display-active-modes-functions-and-funcalls)
      (setq hover-debug-mode nil)
      (message "Hover Debug Mode: OFF"))
    (progn
      (add-hook 'post-command-hook 'display-active-modes-functions-and-funcalls)
      (setq hover-debug-mode t)
      (message "Hover Debug Mode: ON"))))

(defun display-active-modes-functions-and-funcalls ()
  "Display the active modes and any relevant function calls on cursor hover."
  (when hover-debug-mode
    (let ((major-mode-str (format "Major mode: %s" major-mode))
           (minor-modes-str (format "Active minor modes: %s" 
                              (mapconcat 'prin1-to-string minor-mode-list ", "))))
      (message "%s | %s" major-mode-str minor-modes-str))))

					; ------------------------------------------------------------------------- ;
					; ------------------------------------------------------------------------- ;
;; https://web.archive.org/web/20230124093451/https://robert.kra.hn/posts/rust-emacs-setup/
;; (straight-require 'rustic)


(straight-require 'orderless)
(straight-require 'corfu)
(straight-require 'cape)

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  :config
  
  (setq corfu-auto t
    corf-auto-prefix 2
    corfu-quit-no-match t
    corfu-auto-delay  0.05
    completion-cycle-threshold 3
    corfu-quit-at-boundary 'separator
    corfu-popupinfo-delay 0.3
    corfu-echo-delay nil
    )
  ;; (define-key corfu-map (kbd "SPC") #'corfu-insert-separator)

  )

(add-hook 'corfu-mode-hook #'corfu-popupinfo-mode)
(add-hook 'rust-mode-hook #'corfu-mode)
;; (add-hook 'python-mode-hook #'corfu-mode)


(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down) ;; corfu-next
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)  ;; corfu-previous
(define-key corfu-map (kbd "M-.") #'corfu-doc-toggle)


(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
          ("M-j" . lsp-ui-imenu)
          ("M-?" . lsp-find-references)
          ("C-c C-c l" . flycheck-list-errors)
          ("C-c C-c a" . lsp-execute-code-action)
          ("C-c C-c r" . lsp-rename)
          ("C-c C-c q" . lsp-workspace-restart)
          ("C-c C-c Q" . lsp-workspace-shutdown)
          ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  (add-hook 'rust-mode-hook '(lambda () rustic-doc-mode nil))

  ;; LSP bridge
  ;; (setq rustic-lsp-client nil)
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)


  (use-package orderless
    :init
    ;; Tune the global completion style settings to your liking!
    ;; This affects the minibuffer and non-lsp completion at point.
    (setq completion-styles '(orderless partial-completion basic)
      completion-category-defaults nil
      completion-category-overrides nil))

  (use-package lsp-mode
    :ensure
    :commands lsp
    :custom
    ;; what to use when checking on-save. "check" is default, I prefer clippy
    (lsp-rust-analyzer-cargo-watch-command "clippy")
    (lsp-eldoc-render-all t)
    (lsp-idle-delay 0.05)
    ;; enable / disable the hints as you prefer:
    (lsp-rust-analyzer-server-display-inlay-hints t)
    (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
    (lsp-rust-analyzer-display-chaining-hints t)
    (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
    (lsp-rust-analyzer-display-closure-return-type-hints t)
    (lsp-rust-analyzer-display-parameter-hints nil)
    (lsp-rust-analyzer-display-reborrow-hints nil)
    (lsp-completion-provider :none) ;; we use Corfu!
    :init
    (defun my/orderless-dispatch-flex-first (_pattern index _total)
      (and (eq index 0) 'orderless-flex))

    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))
      ;; Optionally configure the first word as flex filtered.
      (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
      ;; Optionally configure the cape-capf-buster.
      (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

    :config
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    (add-hook 'rust-mode-hook 'lsp-mode)
    :hook
    (lsp-completion-mode . my/lsp-mode-setup-completion))
  )

(defun sdev/brew-install (formula)
  "Install a Homebrew formula asynchronously using `brew install'.

FORMULA is a string specifying the formula to install.

Display a success message in the `*Messages*' buffer if the installation is successful."
  (interactive "sEnter formula to install: ")
  (let ((process-name (format "install-%s" formula))
         (command "brew"))
    (async-start-process process-name command
      (lambda (proc)
        (if (= 0 (process-exit-status proc))
          (message "Successfully installed %s" formula)
          (message "Failed to install %s" formula)))
      "install" formula)))



(setq lsp-eldoc-enable-hover nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-modeline-diagnostics-enable nil)

(setq lsp-lens-enable nil)
(setq lsp-signature-auto-activate nil) ;; you could manually request them via `lsp-signature-activate`
(setq lsp-signature-render-documentation nil)
(setq lsp-rust-all-features t)
(setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-macro-call"])
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-parameter-hints t)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-ui-doc-show-with-cursor nil)
;; Enable cache busting, depending on if your server returns
;; sufficiently many candidates in the first place.
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)



;; (use-package company
;;   :ensure
;;   :custom
;;   (company-idle-delay 0.5) ;; how long to wait until popup
;;   ;; (company-begin-commands nil) ;; uncomment to disable popup
;;   :bind
;;   (:map company-active-map
;; 	      ("C-n". company-select-next)
;; 	      ("C-p". company-select-previous)
;; 	      ("M-<". company-select-first)
;; 	      ("M->". company-select-last)))

;; (use-package yasnippet
;;   :ensure
;;   :config
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook 'yas-minor-mode)
;;   (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package flycheck :ensure)

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
    "Rust::LLDB Run Configuration"
    (list :type "lldb"
      :request "launch"
      :name "LLDB::Run"
      :gdbpath "rust-lldb"
      :target nil
      :cwd nil)))


;; (straight-use-package
;;   '(acm-terminal :host github :repo "twlz0ne/acm-terminal"))

;; (unless (package-installed-p 'yasnippet)
;;   (straight-require 'yasnippet)
;;   )




;; (use-package lsp-bridge
;;   :diminish
;;   :defer t
;;   :after (yasnippet orderless)
;;   :straight (:type git :host github :repo "manateelazycat/lsp-bridge"
;; 		:files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;               :includes (acm
;;                           core
;;                           langserver
;;                           multiserver
;;                           resources))
;;   :config

;;   ;; disable tabnine: it's not open source
;;   ;; (setq acm-enable-tabnine nil)

;;   ;; enable signature help in posframe
;;   (setq lsp-bridge-enable-signature-help t)
;;   (setq lsp-bridge-signature-help-fetch-idle 0.3)
;;   (setq lsp-bridge-signature-show-function 'lsp-bridge-signature-show-with-frame)
;;   (setq lsp-bridge-signature-show-with-frame-position 'point)

;;   ;; combine lsp-bridge with orderless
;;   ;; (setq acm-candidate-match-function 'orderless-flex)
;;   (setq acm-backend-lsp-candidate-min-length 1)

;;   (setq lsp-bridge-enable-log   nil
;;     acm-enable-icon         t
;;     acm-enable-doc          t
;;     acm-enable-tabnine      nil
;;     acm-enable-quick-access t)

;;   ;; small QoL
;;   (setq acm-enable-quick-access t)

;;   ;; language servers
;;   (setq lsp-bridge-c-lsp-server "ccls")
;;   (setq lsp-bridge-python-lsp-server "pyright")
;;   (global-lsp-bridge-mode))

;; ;; needed for terminal, there is a visual bug
;; ;; if loading them in graphics mode
;; (unless (or (display-graphic-p) (daemonp))
;;   (require 'popon-setup)
;;   (require 'acm-terminal-setup))



;; Enable lsp-bridge.
;; (add-hooks (list
;; 	     'emacs-lisp-mode
;; 	     'c-mode-hook
;; 	     'c++-mode-hook
;; 	     'python-mode-hook
;; 	     'rust-mode-hook
;; 	     'go-mode-hook
;; 	     'haskell-mode-hook
;; 	     'js2-mode-hook
;; 	     'js-mode-hook
;; 	     'lua-mode-hook
;; 	     )
;;   #'lsp-bridge-mode
;;   )




;; ​;​; For Rust. 
;; ​(​straight-use-package​ ​'rust-mode​) 
;; ​(​add-to-list​ ​'eglot-server-programs​ '((​rust-mode​) ​.​ (​"​rust-analyzer​"​))) 
;; ​(​add-hook​ ​'rust-mode-hook​ ​'eglot-ensure​)

;; ​;​; Go colorful with Tree-sitter. 
;; ​(​straight-use-package​ ​'tree-sitter​) 
;; ​(​straight-use-package​ ​'tree-sitter-langs​) 
;; ​(global-tree-sitter-mode)  
;; ​(​add-hook​ ​'tree-sitter-after-on-hook​ ​#​'tree-sitter-hl-mode​)

(define-key lsp-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

;; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Keybindings ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;


(define-key rust-mode-map (kbd "C-c C-c") 'hydra-rust/body )
(define-key rust-mode-map (kbd "C-t") 'racer-ui-tooltip )

(define-key rust-mode-map (kbd "M-p") 'tiqsi--rust-print-src )
(define-key rust-mode-map (kbd "M-i") 'tiqsi--racer-insert-struct-point )

(define-key rust-mode-map (kbd "C-c c") 'tiqsi-compile--no-message)
(define-key rust-mode-map (kbd "C-c C-r") 'tiqsi-compile--reset-string)
(define-key rust-mode-map (kbd "C-c n") 'flymake-goto-next-error)


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Keybindings _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;

(provide 'programming-rust)

;;; programming-rust.el ends here
