;;; programming-rust.el --- Tiqsi Rust programming support

;;; Commentary:
;; 


(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'company-mode)
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(setq rust-format-on-save t)

(custom-set-variables
 '(racer-cmd (expand-file-name "/cargo/bin/racer"))
 '(racer-rust-src-path (expand-file-name "/rust/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")))

(add-hook 'rust-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq racer-cmd (concat (getenv "HOME") "/cargo/bin/racer")) ;; Rustup binaries PATH
             (setq racer-rust-src-path (concat (getenv "HOME") (shell-command-to-string "echo `rustc --print sysroot`/lib/rustlib/src/rust/src")))
             (setq company-tooltip-align-annotations t)
         (add-hook 'rust-mode-hook #'racer-mode)
             (add-hook 'racer-mode-hook #'eldoc-mode)
             (add-hook 'racer-mode-hook #'company-mode)
             (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
         (add-hook 'rust-mode-hook 'cargo-minor-mode)
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))


;-{Static checking}-;

(add-hook 'rust-mode-hook 'flymake-rust-load)

(setq flymake-rust-use-cargo 1)


(provide 'programming-rust)

;;; programming-rust.el ends here
