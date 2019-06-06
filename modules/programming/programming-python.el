;;; programming-python.el --- Tiqsi python programming support
q
;;; Commentary:
;;


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
              "--ignore-missing-imports" "--fast-parser"
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
  :straight t
  :hook (python-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package company-lsp
  :straight t
  :commands company-lsp)
(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode
  :straight t  )

(use-package dap-python
  :straight t)

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
