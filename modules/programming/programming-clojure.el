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

(define-key clojure-mode-map (kbd "C-c C-s") 'cider-eval-last-sexp)


(provide 'programming-clojure)

;;; programming-clojure.el ends here
