;;; programming-lisp.el --- Tiqsi Lisp with SLIME support

;;; Commentary:
;; Standard using SBCL
;; 


(when tiqsi-linux
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(define-key emacs-lisp-mode-map (kbd "C-c C-s") 'eval-last-sexp)

(provide 'programming-lisp)

;;; programming-lisp.el ends here
