;;; programming-python.el --- Tiqsi python programming support

;;; Commentary:
;;

;;; Code:

;---------------------------------------------------------------------------------------------------;
;Fixes upstream bug <25.2RC                                                                         ;
;---------------------------------------------------------------------------------------------------;


;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25753#44

(when (version< emacs-version "25.2")
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
(setq python-shell-prompt-detect-failure-warning nil)

; /end fixes                                                                                        ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                    Python repl setup procedures                                   ;
;---------------------------------------------------------------------------------------------------;

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--matplotlib=qt5"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 )


(defun sdev-use-ipython (&optional ipython)
  "Set defaults to use IPython instead of the standard interpreter.
With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "IPython command: "))))
  (when (not ipython)
    (setq ipython "ipython"))
  (when (not (executable-find ipython))
    (error "Command %S not found" ipython))
  ;; Needed for IPython 5+
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (cond
   ;; Emacs 24 until 24.3
   ((boundp 'python-python-command)
    (setq python-python-command ipython))
   ;; Emacs 24.3
   ((and (version<= "24.3" emacs-version)
         (not (boundp 'python-shell-interpreter-interactive-arg)))
    ;; This is from the python.el commentary.
    ;; Settings for IPython 0.11:
    (setq python-shell-interpreter ipython
          python-shell-interpreter-args "--pylab"
          python-shell-prompt-regexp "In \\[[0-9]+\\]: "
          python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
          python-shell-completion-setup-code
          "from IPython.core.completerlib import module_completion"
          python-shell-completion-module-string-code
          "';'.join(module_completion('''%s'''))\n"
          python-shell-completion-string-code
          "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))
   ;; Emacs 24.4
   ((boundp 'python-shell-interpreter-interactive-arg)
    (setq python-shell-interpreter ipython
          python-shell-interpreter-args "-i")
    ;; Windows requires some special handling here, see #422
    (let ((exe "C:\\Python27\\python.exe")
          (ipython_py "C:\\Python27\\Scripts\\ipython-script.py"))
      (when (and (eq system-type 'windows-nt)
                 (file-exists-p exe)
                 (file-exists-p ipython_py))
        (setq python-shell-interpreter exe
              python-shell-interpreter-args "-i " + ipython_py))))
   (t
    (error "I don't know how to set ipython settings for this Emacs"))))


(defun sdev-use-cpython (&optional cpython)
  "Set defaults to use the standard interpreter instead of IPython.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Python command: "))))
  (when (not cpython)
    (setq cpython "python"))
  (when (not (executable-find cpython))
    (error "Command %S not found" cpython))
  (cond
   ;; Emacs 24 until 24.3
   ((boundp 'python-python-command)
    (setq python-python-command cpython))
   ;; Emacs 24.3 and onwards.
   ((and (version<= "24.3" emacs-version)
         (not (boundp 'python-shell-interpreter-interactive-arg)))
    (setq python-shell-interpreter cpython
          python-shell-interpreter-args "-i"
          python-shell-prompt-regexp ">>> "
          python-shell-prompt-output-regexp ""
          python-shell-completion-setup-code
          "try:
    import readline
except ImportError:
    def __COMPLETER_all_completions(text): []
else:
    import rlcompleter
    readline.set_completer(rlcompleter.Completer().complete)
    def __COMPLETER_all_completions(text):
        import sys
        completions = []
        try:
            i = 0
            while True:
                res = readline.get_completer()(text, i)
                if not res: break
                i += 1
                completions.append(res)
        except NameError:
            pass
        return completions"
          python-shell-completion-module-string-code ""
          python-shell-completion-string-code
          "';'.join(__COMPLETER_all_completions('''%s'''))\n"))
   ;; Emacs 24.4
   ((boundp 'python-shell-interpreter-interactive-arg)
    (setq python-shell-interpreter cpython
          python-shell-interpreter-args "-i"))
   (t
    (error "I don't know how to set ipython settings for this Emacs"))))

(defun sdev-use-remote (&optional ipython)
  (interactive)
  (setq python-shell-interpreter  "/tiqsi-emacs/modules/programming/remote-python.sh"
        python-shell-interpreter-args "-i"
        python-shell-prompt-regexp ">>> "
        python-shell-prompt-output-regexp ""))

; /end repl                                                                                         ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                           Imenu merging                                           ;
;---------------------------------------------------------------------------------------------------;

;; Python mode
(defun my-merge-imenu ()
  (interactive)
  (let ((mode-imenu (imenu-default-create-index-function))
        (custom-imenu (imenu--generic-function imenu-generic-expression)))
    (append mode-imenu custom-imenu)))


(defun my-python-menu-hook()
  (interactive)
  (add-to-list
   'imenu-generic-expression
   '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
  (setq imenu-create-index-function 'my-merge-imenu)
  ;; (eval-after-load "company"
  ;;     '(progn
  ;;         (unless (member 'company-jedi (car company-backends))
  ;;             (setq comp-back (car company-backends))
  ;;             (push 'company-jedi comp-back)
  ;;             (setq company-backends (list comp-back)))))
  )


(add-hook 'python-mode-hook 'my-python-menu-hook)

; /end Imenu                                                                                        ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                         McCabe Complexity                                         ;
;---------------------------------------------------------------------------------------------------;
; requires pip install mccabe                                                                       ;


(defun sdev/py-mccabe()
  "Get the mccabe complexity for this buffer."
  (interactive)
  (message
   (shell-command-to-string(message "python -m mccabe --min 3 %s" buffer-file-name))))

; /end McCabe                                                                                       ;
;---------------------------------------------------------------------------------------------------;


;---------------------------------------------------------------------------------------------------;
;Debugging                                                                                          ;
;---------------------------------------------------------------------------------------------------;
; Highlight the call to ipdb                                                                        ;
; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/                           ;


(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "ipdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun ipdb-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(defun ipdb-cleanup ()
  (interactive)
  (save-excursion
    (replace-regexp ".*ipdb.set_trace().*\n" "" nil (point-min) (point-max))
    ;; (save-buffer)
    ))

; / end debugging                                                                                   ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                              Linting                                              ;
;---------------------------------------------------------------------------------------------------;

(flycheck-define-checker
    python-mypy ""
    :command ("mypy"
              "--ignore-missing-imports"
              "--python-version" "3.6"
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ": error:" (message) line-end))
    :modes python-mode)

(add-to-list 'flycheck-checkers 'python-mypy t)
(flycheck-add-next-checker 'python-pylint 'python-mypy t)

; /end lint                                                                                         ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                           LSP mode setup                                          ;
;---------------------------------------------------------------------------------------------------;

(use-package lsp-mode
  :commands lsp
  :hook (prog-mode . lsp))

;; TODO: make the window disappear/behave normally && hide line numbers
(defun my/hide-frame-line-numbers (frame _window)
  "Hides line nunmbers from a specific frame in a winow."
  (select-frame frame)
  (display-line-numbers-mode -1))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)                                                                                                                          
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)                                                                                                                            
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
(setq lsp-ui-doc-enable t
   lsp-ui-doc-use-childframe t
   lsp-ui-doc-position 'top
   lsp-ui-doc-include-signature t
   lsp-ui-sideline-enable t
   lsp-ui-flycheck-enable t
   lsp-ui-sideline-ignore-duplicate t
   lsp-ui-sideline-show-flycheck t
   lsp-ui-flycheck-list-position 'right
   lsp-ui-flycheck-live-reporting t
   lsp-ui-peek-enable t
   lsp-ui-peek-list-width 60
   lsp-ui-peek-peek-height 25)

  ;; (add-hook 'lsp-ui-doc-frame-hook #'my/hide-frame-line-numbers)
  )

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates 'auto
        company-lsp-enable-recompletion t))


;; With use-package:
(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(progn
 (push 'company-lsp company-backends)
 (setq company-transformers nil
       company-lsp-async t
       company-lsp-cache-candidates nil)
 (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
 )
(add-hook 'python-mode-hook 'lsp)
(add-hook 'python-mode-hook 'flycheck-mode)


(use-package lsp-python-ms
  :ensure nil
  :hook (python-mode . lsp)
  :config

  ;; for dev build of language server
  ;; (setq lsp-python-ms-dir
  ;;       (expand-file-name "~/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  (setq lsp-python-ms-executable
        "~/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

(require 'lsp-ui)
;; make sure we have lsp-imenu everywhere we have LSP
(require 'lsp-ui-imenu)
(add-hook 'lsp-after-open-hook 'lsp-ui-imenu)
(add-hook 'lsp-after-open-hook 'lsp-ui-mode)
(add-hook 'lsp-after-open-hook 'lsp-ui-flycheck-list--view)
(flycheck-mode 1)
(lsp-ui-mode 1)


(straight-use-package
 '(helm-lsp
   :type git
   :host github
   :repo "emacs-lsp/helm-lsp"
   :config
   (progn

     )))

(defun netrom/helm-lsp-workspace-symbol-at-point ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-lsp-workspace-symbol)))

(defun netrom/helm-lsp-global-workspace-symbol-at-point ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-lsp-global-workspace-symbol)))

(setq netrom--general-lsp-hydra-heads
      '(;; Xref
	("d" xref-find-definitions "Definitions" :column "Xref")
	("D" xref-find-definitions-other-window "-> other win")
	("r" xref-find-references "References")
	("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
	("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search")

	;; Peek
	("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
	("C-r" lsp-ui-peek-find-references "References")
	("C-i" lsp-ui-peek-find-implementation "Implementation")

	;; LSP
	("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
	("C-a" lsp-execute-code-action "Execute code action")
	("R" lsp-rename "Rename")
	("t" lsp-goto-type-definition "Type definition")
	("i" lsp-goto-implementation "Implementation")
	("f" helm-imenu "Filter funcs/classes (Helm)")
	("C-c" lsp-describe-session "Describe session")

	;; Flycheck
	("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

      netrom--misc-lsp-hydra-heads
      '(;; Misc
	("q" nil "Cancel" :column "Misc")
	("b" pop-tag-mark "Back")))

;; Create general hydra.
(eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
	 ,@(append
	    netrom--general-lsp-hydra-heads
	    netrom--misc-lsp-hydra-heads)))

(add-hook 'lsp-mode-hook
	  (lambda () (local-set-key (kbd "C-c C-g") 'netrom/lsp-hydra/body)))



(straight-use-package
 '(lsp-treemacs
   :type git
   :host github
   :repo "emacs-lsp/lsp-treemacs"
   :commands lsp-treemacs-errors-list
   :config
   (progn
     )
))


(use-package lsp-java
  :straight t)

(use-package dap-mode
  :straight t
  :config
  (progn
    (add-hook 'python-mode-hook (lambda () (progn
					     (require 'dap-mode)
					     (require 'dap-ui)
					     (require 'dap-python)
					     (dap-mode 1)
					     (dap-ui-mode 1))
				  ))
    ))

(defun dap-set-windows()
  (interactive)
  (dap-ui-locals)
  (dap-ui-repl)
 (dap-ui-sessions)
  )

(eval-after-load "dap-mode"
  '(progn
     (dap-register-debug-provider "python" 'dap-python--populate-start-file-args)
     (dap-register-debug-template "Python :: Run Configuration"
				 (list :type "python"
				       :args ""
				       :cwd nil
				       :target-module nil
				       :request "launch"
				       :name "Python :: Run Configuration"))))


;; (use-package dap-python
;;   :straight t)

; /end lsp                                                                                          ;
;---------------------------------------------------------------------------------------------------;

;---------------------------------------------------------------------------------------------------;
;                                           Miscellaneous                                           ;
;---------------------------------------------------------------------------------------------------;

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)


(defun sdev/py-sort-imports ()
  (interactive)
  (mark-whole-buffer)
  (py-isort-region))



(provide 'programming-python)

;;; programming-python.el ends here
