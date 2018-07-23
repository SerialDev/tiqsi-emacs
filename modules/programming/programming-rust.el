;;; programming-rust.el --- Tiqsi Rust programming support

;;; Commentary:
;; 


(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'company-mode)
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(setq rust-format-on-save t)

;; (custom-set-variables
;;  '(racer-cmd (expand-file-name "/cargo/bin/racer"))
;;  '(racer-rust-src-path (expand-file-name "/rust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")))

(add-hook 'rust-mode-hook
          '(lambda ()
             (setq tab-width 2)
						 (setq racer-cmd (expand-file-name "/cargo/bin/racer"))
						 (setq racer-rust-src-path (expand-file-name "/rust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src"))
             ;; (setq racer-cmd (concat (getenv "HOME") "/cargo/bin/racer")) ;; Rustup binaries PATH
             ;; (setq racer-rust-src-path (concat (getenv "HOME") (shell-command-to-string "echo `rustc --print sysroot`/lib/rustlib/src/rust/src")))
             (setq company-tooltip-align-annotations t)
         (add-hook 'rust-mode-hook #'racer-mode)
             (add-hook 'racer-mode-hook #'eldoc-mode)
             (add-hook 'racer-mode-hook #'company-mode)
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
         (add-hook 'rust-mode-hook 'cargo-minor-mode)
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))


;-{Static checking}-;


(setq flymake-rust-use-cargo 1)


;-----{Hydras }-----;

(defhydra hydra-rust (:color pink :hint nil)
  "
^Rust Cargo commands^
------------------------------------------------
_r_: Run          _i_: Init          _u_: Update
_x_: Run-example  _n_: New           _c_: Repeat
_b_: Build        _f_: Current-test  _e_: Bench
_l_: Clean        _s_: Search        _o_: Current-file-tests
_d_: Doc          _t_: Test          _m_: Fmt
                _k_: Check         _q_: Clippy
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
  ("k"   cargo-process-check color: red)
  ("q" cargo-process-clippy :color blue)
  ("ESC" nil "Exit"))



(defun racer-ui-tooltip ()
  "Show the docstring in a tooltip.
The tooltip's face is `racer-tooltip'
See `racer-describe'."
  (interactive)
  (-some-> (symbol-at-point)
           (symbol-name)
           (racer--describe)
           (with-current-buffer (concat "\n" (buffer-string) "\n\n"))
            (message 'racer-tooltip nil nil 1000)))

(define-key rust-mode-map (kbd "C-c C-c") 'hydra-rust/body )
(define-key rust-mode-map (kbd "C-t") 'racer-ui-tooltip )

(provide 'programming-rust)

;;; programming-rust.el ends here
