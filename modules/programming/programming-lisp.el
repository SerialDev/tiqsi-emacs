;;; programming-lisp.el --- Tiqsi Lisp with SLIME support

;;; Commentary:
;; Standard using SBCL
;; Elisp functions will go on this one too


;; Replace "sbcl" with the path to your implementation
;; Do some standard SLIME configuration.
(slime-setup '(slime-fancy slime-tramp))
;; Set the default lisp you want to use (here it's SBCL).

(when tiqsi-linux
   (progn
     (load (expand-file-name "~/quicklisp/slime-helper.el"))
     (setq inferior-lisp-program "sbcl")))

(when tiqsi-win32
  (progn
    (load (expand-file-name "C:/Users/andre/.roswell/helper.el"))
    (setq inferior-lisp-program "ros -Q run")))


;; (setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(slime-setup '(slime-fancy slime-company))


(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
(eval-after-load 'elisp-slime-nav '(diminish 'elisp-slime-nav-mode))

(defhydra hydra-slime (:color pink :hint nil)
  "
^Slime useful commands^
----------------------------------------------------------------------------------
_c_: Compile defun  _x_: Export Symbol       _u_: Undefine defun               _<_: List Callers
_p_: Start Slime   _a_: GOTO beg defun       _:_: Interactive eval             _>_: List Callees
_/_: Eval defun    _Sa_: GOTO end defun      _t_: Trace                        _s_: eval last sexpr
_c_: Call defun    _TAB_: Complete at point  _v_: Edit Value                   _r_: Eval in repl
_q_: Close parens  _RET_: Expand-1           _I_: Inspect                      _ESC_: Exit
_kl_: Load/Compile Buffer-File   _kc_: Compile Buffer-File (no load)  _l_: Load File
"
  ("c"   slime-compile-defun :color blue)
  ("p"   slime :color blue)
  ("/"   slime-eval-defun :color blue)
  ("c"   slime-call-defun :color blue)
  ("q"   slime-close-all-parens-in-sexp :color blue)

  ("x"   slime-export-symbol-at-point :color blue)
  ("a"   slime-beginning-of-defun :color blue)
  ("Sa"   slime-end-of-defun :color blue)
  ("TAB"   slime-completion-at-point :color blue)
  ("RET"   slime-expand-1 :color blue)

  ("u"   slime-undefine-function :color blue)
  (":"   slime-interactive-eval :color blue)
  ("r"   slime-eval-region :color blue)
  ("t"   slime-toggle-fancy-trace :color blue)
  ("v"   slime-edit-value :color blue)
  ("I"   slime-inspect :color blue)

  ("<"   slime-list-callers :color blue)
  (">"   slime-list-callees :color blue)
  ("s"   slime-eval-last-expression :color blue)
  ("r"   slime-eval-last-expression-in-repl :color blue)

  ("l"   slime-load-file :color blue)
  ("kl"   slime-compile-and-load-file :color blue)
  ("kc"   slime-compile-file :color blue)
  ("ESC" nil "Exit"))

;;TODO reindent defun or new line on enter to guarantee constant indentation

(defun insert-let()
  (interactive)
  (insert "(let ((x ))
  )")
  (search-backward "x")
  (delete-forward-char 1)
  )


(straight-use-package
 '(coleslaw
   :type git
   :host github
   :repo "equwal/coleslaw"
   :config
   (coleslaw-setup)
   ))


;; (straight-use-package
;;  '(sly
;;    :type git
;;    :host github
;;    :repo "joaotavora/sly"
;;    ))

;; (add-hook 'lisp-mode-hook (lambda () (slime-mode nil)))

;; TODO resolve issues with installation through straight
;; (package-install 'sly)
;; (package-install 'sly-repl-ansi-color)
;; (package-install 'sly-macrostep)
;; (package-install 'sly-quicklisp)
;; (require-soft 'sly)
;; (require-soft 'sly-repl-ansi-color)
;; (require-soft 'sly-macrostep)
;; (require-soft 'sly-quicklisp)


;; (straight-use-package
;;  '(sly-quicklisp
;;    :type git
;;    :host github
;;    :repo "joaotavora/sly-quicklisp"
;; ))

(define-advice eval-print-last-sexp (:around (old-fun &rest args) add-prefix)
  "Prepend ;; =>."
  (let ((op (point)))
    (apply old-fun args)
    (save-excursion
      (goto-char op)
      (forward-line 1)
      (insert ";; => "))))




(define-advice elisp--preceding-sexp (:around (old-fun) multiline-comment)
  "Support sexp in multiline comment."
  (condition-case err
      (funcall old-fun)
    (scan-error
     (if (nth 4 (syntax-ppss))
         (let ((work-buffer (current-buffer))
               (temp-buffer (generate-new-buffer " *temp*"))
               found sexp error)
           (with-current-buffer temp-buffer
             (delay-mode-hooks (emacs-lisp-mode)))
           (save-excursion
             (comment-normalize-vars)
             (while (and (comment-beginning)
                         (not found))
               (let ((code (buffer-substring-no-properties
                            (point) (line-end-position))))
                 (with-current-buffer temp-buffer
                   (goto-char (point-min))
                   (insert code ?\n)
                   (goto-char (point-max))
                   (condition-case err
                       (setq sexp (funcall old-fun)
                             found t)
                     (scan-error (setq error err)))))
               (when (= -1 (forward-line -1))
                 (error "elisp--preceding-sexp@multiline-comment error"))
               (goto-char (line-end-position))))
           (cond (found sexp)
                 (error (signal (car error) (cdr error)))
                 (t (error "elisp--preceding-sexp@multiline-comment error"))))
       (signal (car err) (cdr err))))))

(when tiqsi-linux
  (add-to-list 'load-path (expand-file-name "~/quicklisp/slime-helper.el")))

;; (require 'sly-quicklisp-autoloads)

;; (sly-setup)
;; (sly-enable-contrib 'sly-quicklisp)
;; (sly-enable-contrib 'sly-indentation)
;; (sly-enable-contrib 'sly-fancy)
;; (sly-enable-contrib 'sly-fancy-trace)
;; (sly-enable-contrib 'sly-trace-dialog)
;; (sly-enable-contrib 'sly-repl-ansi-color)

;; (push 'sly-repl-ansi-color sly-contribs)
;; (add-hook 'lisp-mode-hook (lambda () (company-quickhelp-local-mode t)))
;; (add-hook 'lisp-mode-hook (lambda () (sly-mode t)))

(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

                                        ;---{Keybindings}---;

;; Slime
(define-key slime-mode-map (kbd "M-c") 'hydra-slime/body )
(define-key slime-mode-map (kbd "C-l") 'insert-let)
(define-key slime-mode-map (kbd "C-c C-s") 'slime-eval-last-expression)
(define-key slime-mode-map (kbd "C-c s") 'slime-eval-last-expression-in-repl)
(define-key slime-mode-map (kbd "C-c C-p") 'slime)
(define-key slime-mode-map (kbd "C-r") 'slime-reindent-defun)

;; Sly
;; (define-key sly-editing-mode-map (kbd "C-l") 'insert-let)
;; (define-key sly-mode-map (kbd "C-l") 'insert-let)
;; (define-key sly-editing-mode-map (kbd "C-c C-s") 'sly-eval-last-expression)
;; (define-key sly-mode-map (kbd "C-c s") 'sly-eval-last-expression)



(define-key emacs-lisp-mode-map (kbd "C-c C-s") 'eval-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-.") 'elisp-slime-nav-describe-elisp-thing-at-point)
(define-key emacs-lisp-mode-map (kbd "M-.") 'elisp-slime-nav-find-elisp-thing-at-point)

(provide 'programming-lisp)

;;; programming-lisp.el ends here
