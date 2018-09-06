;;; programming-lisp.el --- Tiqsi Lisp with SLIME support

;;; Commentary:
;; Standard using SBCL
;; Elisp functions will go on this one too


(when tiqsi-linux
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;; Replace "sbcl" with the path to your implementation
;; Do some standard SLIME configuration.
(slime-setup '(slime-fancy slime-tramp))
;; Set the default lisp you want to use (here it's SBCL).
(setq inferior-lisp-program "sbcl")

;; (setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))


;---{Keybindings}---;

(define-key emacs-lisp-mode-map (kbd "C-c C-s") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-.") 'elisp-slime-nav-describe-elisp-thing-at-point)
(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'programming-lisp)

;;; programming-lisp.el ends here
