;;; programming-lisp.el --- Tiqsi Lisp with SLIME support

;;; Commentary:
;; Standard using SBCL
;; 


(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(define-key emacs-lisp-mode-map (kbd "C-c C-s") 'eval-last-sexp)

(provide 'programming-lisp)

;;; programming-lisp.el ends here
