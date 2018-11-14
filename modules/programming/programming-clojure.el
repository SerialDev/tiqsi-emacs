;;; programming-clojure.el --- Tiqsi clojure support  -*- lexical-binding: t -*-

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
;; - They both require the installation of version -> ([cider/piggieback "0.3.9"])
;; - They both require to enable cider-nrepl -> (:plugins [[cider/cider-nrepl "0.18.0"]])
;; Current Issues and solutions
;;   lein new figwheel project-name
;;   lein figwheel
;;
;; Alternatively
;;   lein repl
;;   (use 'figwheel-sidecar.repl-api)
;;   (start-figwheel!)


                                        ;------{Cider}------;

;; Setup cider for clojurescript / figwheel dev.

(defmacro with-minibuffer-input (form &rest inputs)
  (declare (indent 1))
  `(minibuffer-with-setup-hook
       (lambda ()
         (minibuffer-input-provider ',inputs))
     ,form))

;; -*- lexical-binding: t -*-
(defun minibuffer-input-provider (inputs)
  (let ((hook (make-symbol "hook")))
    (fset hook (lambda ()
                 (remove-hook 'post-command-hook hook)
                 (when inputs
                   (when (= 0 (minibuffer-depth))
                     (error "Too many inputs"))
                   (when (cdr inputs)
                     (add-hook 'post-command-hook hook))
                   (insert (pop inputs))
                   (exit-minibuffer))))
    (add-hook 'post-command-hook hook)))

(use-package with-simulated-input
  :straight t
  :ensure t
)

(setq nrepl-hide-special-buffers t)
(setq cider-cljs-repl-type "figwheel")
(setq cider-default-cljs-repl "figwheel")
(setq cider-default-repl-command "lein")


(setq cider-cljs-lein-repl
      "(cond
  (and (resolve 'user/run) (resolve 'user/browser-repl)) ;; Chestnut projects
  (eval '(do (user/run)
             (user/browser-repl)))

  (try
    (require 'figwheel-sidecar.repl-api)
    (resolve 'figwheel-sidecar.repl-api/start-figwheel!)
    (catch Throwable _))
  (eval '(do (figwheel-sidecar.repl-api/start-figwheel!)
             (figwheel-sidecar.repl-api/cljs-repl)))

  (try
    (require 'cemerick.piggieback)
    (resolve 'cemerick.piggieback/cljs-repl)
    (catch Throwable _))
  (eval '(cemerick.piggieback/cljs-repl (cljs.repl.rhino/repl-env)))

  :else
  (throw (ex-info \"Failed to initialize CLJS repl. Add com.cemerick/piggieback and optionally figwheel-sidecar to your project.\" {})))")

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
  (let* ((output-buffer (generate-new-buffer "temp-clj"))
	 (proc (progn
		 (async-shell-command "lein repl :start :port 6666" output-buffer)
		 (get-buffer-process output-buffer))))))

  ;; ;; TODO fix with continuation passing
  ;; (async-start
  ;;  (lambda()
  ;;    (async-shell-command "lein repl :start :port 6666" )
  ;;    46061)
  ;;  (lambda(result)
  ;;    (cider-connect "127.0.0.1" (message "%s" result))))
  

(defun lein-connect-repl()
  "Start Leiningen repl."
  (interactive)
  (with-simulated-input "127.0.0.1 RET 6666 RET" (call-interactively 'cider-connect))
  )

(defun lein-figwheel()
  (interactive)
  (progn
    (set-buffer "temp-clj")
    (insert "(use 'figwheel-sidecar.repl-api)")
    (comint-send-input)
    (insert "(start-figwheel!)")
    (comint-send-input)))

(defun lein-connect-cljs-repl()
  (interactive) 
    (with-simulated-input "127.0.0.1 RET 6666 RET figwheel RET" (call-interactively 'cider-connect-cljs)))

(defun figwheel-repl ()
  (interactive)
  (inf-clojure "lein figwheel"))

;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

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

;; Use clojure-mode for Clojurescript.

;; (add-auto-mode 'clojure-mode "\\.cljs\\'")


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
 Move              ^ ^        Jump      ^ ^                                                │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
_C-<up>_: move fwd paren        _M-<up>_ wrap around
_C-<down>_: move back paren     _M-<down>_ splice sexp
_M-<right>_: fwd slurp paren    _C-<right>_: back barf paren
_M-<left>_: fwd barf paren      _C-<left>_: back slurp paren
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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

(define-key clojure-mode-map (kbd "TAB") 'company-indent-or-complete-common)

;; (define-key clojure-mode-map (kbd "C-c <up>") 'move-forward-paren)
;; (define-key clojure-mode-map (kbd "C-c <down>") 'move-backward-paren)
;; (define-key clojure-mode-map (kbd "C-c <right>") 'paredit-forward-slurp-sexp)
;; (define-key clojure-mode-map (kbd "C-c <left>") 'paredit-forward-barf-sexp)
(define-key clojure-mode-map (kbd "C-c s") 'cider-eval-last-sexp)
;; (define-key clojure-mode-map (kbd "C-c (") 'paredit-wrap-round)
;; (define-key clojure-mode-map (kbd "C-c )") 'paredit-splice-sexp)
(define-key clojure-mode-map (kbd "M-c") 'hydra-clojure-usage/body)


(provide 'programming-clojure)

;;; programming-clojure.el ends here
