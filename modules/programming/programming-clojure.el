;;; programming-clojure.el --- Tiqsi clojure support

;;; Commentary:
;;

;------{Cider}------;

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; Turn on auto-completion with Company-Mode
(add-hook 'clojure-mode-hook 'company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; Replace return key with newline-and-indent when in cider mode.
(add-hook 'cider-mode-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))


(defun lein-start-repl()
  "Start Leiningen repl."
  (interactive)
  (async-shell-command "lein repl :start :port 46061" )

  ;; ;; TODO fix with continuation passing
  ;; (async-start
  ;;  (lambda()
  ;;    (async-shell-command "lein repl :start :port 46061" )
  ;;    46061)
  ;;  (lambda(result)
  ;;    (cider-connect "127.0.0.1" (message "%s" result))))
  )

(defun lein-connect-repl()
  "Start Leiningen repl."
  (interactive)
  (cider-connect "127.0.0.1" "46061" )
  )



(defun lein-compile-uberjar()
  "Compile lein to jar."
  (interactive)
  (message
   (async-shell-command(message "lein uberjar"))))


(defun lein-run-uberjar()
  "Compile lein to jar."
  (interactive)
  (message
   (async-shell-command(message "java -jar target/uberjar/%s" (lein-project-file)))))


(defun lein-project-file()
  "get project-file jar name."
  (interactive)

  (s-append "standalone.jar"
	    (s-replace "\"" "-"
		       (s-replace " \"" "-"
				  (s-replace "defproject " ""
					     (regex-match "defproject .*"
							  (my-file-contents (concat projectile-project-root "project.clj")) 0))))))

(defun my-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun regex-match ( regex-string string-search match-num )
  (string-match regex-string string-search)
  (match-string match-num string-search))

;; REPL history file
(setq cider-repl-history-file "~/.emacs.d/cider-history")


;; nice pretty printing
(setq cider-repl-use-pretty-printing t)

;; nicer font lock in REPL
(setq cider-repl-use-clojure-font-lock t)

;; result prefix for the REPL
(setq cider-repl-result-prefix ";; => ")

;; never ending REPL history
(setq cider-repl-wrap-history t)

;; looong history
(setq cider-repl-history-size 3000)

;; eldoc for clojure
(add-hook 'cider-mode-hook #'eldoc-mode)


;; error buffer not popping up
(setq cider-show-error-buffer nil)

(lambda()  )

(defun move-to-close-paren()
  )
  (looking-at ")")

;; TODO Dynamic dispatch all possible <> [] {} ()

(defun move-forward-paren (&optional arg)
  "Move forward parenthesis"
  (interactive "P")
  (if (looking-at ")") (forward-char 1))
  (while (not (looking-at ")")) (forward-char 1))
  )

(defun move-backward-paren (&optional arg)
  "Move backward parenthesis"
  (interactive "P")
  (if (looking-at "(") (forward-char -1))
  (while (not (looking-at "(")) (backward-char 1))
)


(defun move-forward-sqrParen (&optional arg)
  "Move forward square brackets"
  (interactive "P")
  (if (looking-at "]") (forward-char 1))
  (while (not (looking-at "]")) (forward-char 1))
  )

(defun move-backward-sqrParen (&optional arg)
  "Move backward square brackets"
  (interactive "P")
  (if (looking-at "[[]") (forward-char -1))
  (while (not (looking-at "[[]")) (backward-char 1))
  )

(defun move-forward-curlyParen (&optional arg)
  "Move forward curly brackets"
  (interactive "P")
  (if (looking-at "}") (forward-char 1))
  (while (not (looking-at "}")) (forward-char 1))
  )

(defun move-backward-curlyParen (&optional arg)
  "Move backward curly brackets"
  (interactive "P")
  (if (looking-at "{") (forward-char -1))
  (while (not (looking-at "{")) (backward-char 1))
  )



(defhydra hydra-clojure-usage (:color pink
				      :hint nil)
"
                                                                                           ╭──────┐
 Move              ^ ^        Jump      ^ ^                                           	   │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
_C-<up>_: move fwd paren    	_M-<up>_ wrap around
_C-<down>_: move back paren     _M-<down>_ splice sexp
_M-<right>_: fwd slurp paren	_C-<right>_: back barf paren
_M-<left>_: fwd barf paren  	_C-<left>_: back slurp paren  
_C-s_: eval last sexp           ^ ^
"

  ("C-<up>" move-forward-paren :color red)
  ("C-<down>" move-backward-paren :color red)
  ("M-<right>" paredit-forward-slurp-sexp :color red)
  ("C-<right>" paredit-backward-barf-sexp :color red)
  ("M-<left>" paredit-forward-barf-sexp :color red)
  ("C-<left>" paredit-backward-slurp-sexp :color red)
  ("C-s" cider-eval-last-sexp :color red)
  ("M-<up>" paredit-wrap-round :color red)
  ("M-<down>" paredit-splice-sexp :color red)
  ("<escape>" nil "cancel" :color blue)
  ("<f1>" lein-start-repl "start repl" :color red)
  ("<f2>" lein-connect-repl "connect repl" :color red)
  ("<f3>" lein-compile-uberjar "compile uberjar" :color blue)
  ("<f3>" lein-run-uberjar "run uberjar" :color blue)
)


;; (define-key clojure-mode-map (kbd "C-c <up>") 'move-forward-paren)
;; (define-key clojure-mode-map (kbd "C-c <down>") 'move-backward-paren)
;; (define-key clojure-mode-map (kbd "C-c <right>") 'paredit-forward-slurp-sexp)
;; (define-key clojure-mode-map (kbd "C-c <left>") 'paredit-forward-barf-sexp)
;; (define-key clojure-mode-map (kbd "C-c C-s") 'cider-eval-last-sexp)
;; (define-key clojure-mode-map (kbd "C-c (") 'paredit-wrap-round)
;; (define-key clojure-mode-map (kbd "C-c )") 'paredit-splice-sexp)
(define-key clojure-mode-map (kbd "M-c") 'hydra-clojure-usage/body)


(provide 'programming-clojure)

;;; programming-clojure.el ends here
