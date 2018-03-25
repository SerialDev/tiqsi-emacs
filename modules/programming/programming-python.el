;;; programming-python.el --- Tiqsi python programming support

;;; Commentary:
;; 

 (setq jedi:setup-keys nil)
 (setq jedi:tooltip-method nil)
 (autoload 'jedi:setup "jedi" nil t)
 (add-hook 'python-mode-hook 'jedi:setup)

(defvar jedi:goto-stack '())

(defun jedi:jump-to-definition ()
  (interactive)
  (add-to-list 'jedi:goto-stack
               (list (buffer-name) (point)))
  (jedi:goto-definition))

(defun jedi:jump-back ()
  (interactive)
  (let ((p (pop jedi:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))


(defmacro pyt--eval-after-load (file-or-files &rest form)
  "Execute FORM when all the files in FILE-OR-FILES are loaded.
FORM is checked at compile time."
  (declare (debug (form form &rest form))
           (indent 1))
  (when (stringp file-or-files)
    (setq file-or-files (list file-or-files)))
  (let ((code `(progn ,@form)))
    (mapc (lambda (file)
            (setq code `(eval-after-load ',file ',code)))
          file-or-files)
    code))

(defvar pyt--run-once-used-keys nil)

(defmacro pyt--run-once (key &rest body)
  (declare (indent defun))
  (assert (symbolp (eval key)) nil "Key must be a symbol")
  `(unless (member ,key pyt--run-once-used-keys)
     (add-to-list 'pyt--run-once-used-keys ,key)
     ,@body))


;-----{Fixes upstream bug <25.2RC}-----;

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

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "--matplotlib=qt5"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 )


(setq python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 )

;;; smartrep
(pyt--eval-after-load (smartrep python)
  (smartrep-define-key
      python-mode-map
      "C-c"
    '(("C-p" . beginning-of-defun)
      ("C-n" . end-of-defun)
      (">"   . python-indent-shift-right)
      ("<"   . python-indent-shift-left))))

(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=100"))


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


(defun send-py-line ()
  (interactive)
  (setq-local py-temp (string-to-number(message "%d" (point))))
  (next-line 1 1)
  (python-shell-send-region py-temp (point) nil t)) 

;-----------{I-menu merging}-----------;

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

;-----------{Mccabe python }-----------;
;; requires pip install mccabe

(defun sdev/py-mccabe()
  "Get the mccabe complexity for this buffer."
  (interactive)
  (message
  (shell-command-to-string(message "python -m mccabe --min 3 %s" buffer-file-name))))

  
;;; Indentation for python

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)

(defun sdev/py-sort-imports ()
  (interactive)
  (mark-whole-buffer)
  (py-isort-region))


; DISABLED to work with Ipython3 and prevent Inline matplotlib issues
;; (setq ein:use-auto-complete-superpack t
;;       ein:use-smartrep t)

(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

(defun my-ac-jedi-setup ()
  (jedi:setup)
  ;; override `ac-sources':
  (setq ac-sources '(ac-source-jedi-direct)))

(add-hook 'python-mode-hook 'my-ac-jedi-setup)
(add-hook 'inferior-python-mode-hook 'my-ac-jedi-setup)
(inferior-python-mode)

(setq ac-auto-show-menu    0)
(setq ac-delay             0)
(setq ac-menu-height       5)
(setq ac-auto-start t)
(setq ac-show-menu-immediately-on-auto-complete 0)
(setq ac-quick-help-delay 0)
;-----------{Code generation}----------;

;; (package-install 'pygen)
;; (try-require 'pygen)
;; (add-hook 'python-mode-hook 'pygen-mode)
;(shell-command "pip install rope")



;---{Keybindings}---;

;; redefine jedi's C-. (jedi:goto-definition)
;; to remember position, and set C-, to jump back
(define-key python-mode-map (kbd "C-.") 'jedi:jump-to-definition)
(define-key python-mode-map (kbd "C-,") 'jedi:jump-back)
(define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)
(define-key python-mode-map (kbd "C-<tab>") 'jedi:complete)

(define-key python-mode-map (kbd "C-c t s") 'sdev/py-sort-imports)

;; (define-key python-mode-map (kbd "C-c em") 'elpy-multiedit-python-symbol-at-point)
;; (define-key python-mode-map (kbd "C-c er") 'elpy-refactor)
;; (define-key python-mode-map (kbd "C-c ef") 'elpy-format-code)

(define-key python-mode-map (kbd "C-c C-x r") 'python-shell-send-region)
(define-key python-mode-map (kbd "C-c C-a") 'send-py-line)


(provide 'programming-python)

;;; programming-python.el ends here
