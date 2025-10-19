;;; programming-python-lite.el --- Tiqsi python programming support  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; _ _ _ _ _ _ _ _    /¯¯¯ Fixes upstream bug <25.2RC ¯¯¯\_ _ _ _ _ _ _ _    ;



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


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Fixes upstream bug <25.2RC _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;


;; (setq tiqsi-python-buffer "*shell*")
(setq tiqsi-python-buffer "*Python*")

(defun send-py-line ()
  (interactive)
  (let ((py-temp (thing-at-point 'line t)) )
    (comint-send-string tiqsi-python-buffer py-temp)))



(defun send-py-line-p ()
  (interactive)
  (let ((py-temp
          (thing-at-point 'line t)) )

    (comint-send-string tiqsi-python-buffer
      (s-prepend
        py-temp
        (s-prepend
          (s-prepend
            "; print( "py-temp)
          " )) ")))))



(defun send-py-region(begin end)
  (interactive "r")
  (comint-send-string tiqsi-python-buffer
    (buffer-substring-no-properties begin end))
  (comint-send-string tiqsi-python-buffer "\n")
  )


(defun send-py-region (begin end)
  "Send the selected region from BEGIN to END to the Python process.
This function sends the region as a single block to ensure that multiline statements
and definitions are treated correctly."
  (interactive "r")
  (let ((code (buffer-substring-no-properties begin end)))
    ;; Ensure the code ends with a newline to execute it
    (unless (string-suffix-p "\n" code)
      (setq code (concat code "\n")))
    ;; Use python-shell-send-string instead of comint-send-string if available
    (if (fboundp 'python-shell-send-string)
      (python-shell-send-string code)
      (comint-send-string tiqsi-python-buffer code))))



(defun extract-python-functions-to-clipboard (start end)
  "Extract function names from the selected region and copy them to the clipboard in the desired import format."
  (interactive "r")
  (let ((region-content (buffer-substring-no-properties start end))
         (function-names '())
         (buffer-file-name (file-name-nondirectory (buffer-file-name))))
    (with-temp-buffer
      (insert region-content)
      (goto-char (point-min))
      (while (re-search-forward "^def \\([a-zA-Z0-9_]+\\)\\s-*(" nil t)
        (push (match-string 1) function-names)))
    (let ((import-string (concat "from " buffer-file-name " import ("
                           (mapconcat 'identity (reverse function-names) ", ") ")")))
      (kill-new import-string)
      (message "Copied to clipboard: %s" import-string))))


(defun tiqsi-compile (compile-string)
  (interactive "sString to compile: ")
  (let* ((buffer-dir (or (and (boundp 'default-directory)
                           default-directory)
                       (file-name-directory buffer-file-name)))
          (compile-command (concat "cd " buffer-dir " && " compile-string))
          (executable (tiqsi-compile-extract-executable compile-string))
          (current-window (selected-window))
          (other-window (next-window current-window nil t)))
    (setq tiqsi-compile--command compile-string)
    (setq tiqsi-compile--executable executable)
    (message "Compiling executable: %s" executable)
    (with-selected-window other-window
      (let ((compilation-buffer-name-function (lambda (mode) "*tiqsi-compile*"))
             (display-buffer-alist
               `(("*tiqsi-compile*" . ((display-buffer-reuse-window
					 display-buffer-same-window))))))
        (compile compile-command)
	(tiqsi-compile--utils--setmode "*shell*")
	))))


(defun tiqsi-uv-compile (compile-string)
  (interactive (list (read-string "String to compile: " "source .venv/bin/activate && ")))
  (let* ((buffer-dir (or (and (boundp 'default-directory)
                           default-directory)
                       (file-name-directory buffer-file-name)))
          (compile-command (concat "cd " buffer-dir " && " compile-string))
          (executable (tiqsi-compile-extract-executable compile-string))
          (current-window (selected-window))
          (other-window (next-window current-window nil t)))
    (setq tiqsi-compile--command compile-string)
    (setq tiqsi-compile--executable executable)
    (message "Compiling executable: %s" executable)
    (with-selected-window other-window
      (let ((compilation-buffer-name-function (lambda (mode) "*tiqsi-uv-compile*"))
             (display-buffer-alist
               `(("*tiqsi-uv-compile*" . ((display-buffer-reuse-window
                                            display-buffer-same-window))))))
        (compile compile-command)
        ;;(tiqsi-compile--utils--setmode "*shell*")
	))))


(defun custom-compile-go-to-error ()
  "Automatically navigate to the file and line of the compilation error under the cursor,
   but keep the focus on the compilation buffer."
  (interactive)
  (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (pattern "^\\([^:]+\\):\\([0-9]+\\):.*$")
          match file line-num orig-buffer)
    ;; Store the original buffer
    (setq orig-buffer (current-buffer))
    (when (string-match pattern line)
      (setq file (match-string 1 line))
      (setq line-num (string-to-number (match-string 2 line)))
      (when (and file (file-exists-p file) line-num)
        ;; Open the file in another window without switching to it
        (save-selected-window
          (find-file-other-window file)
          (goto-char (point-min))
          (forward-line (1- line-num))
          (recenter))))
    ;; Reselect the original buffer to return the focus there
    (select-window (get-buffer-window orig-buffer))))




(defun python-find-functions-without-docstrings-ag (directory)
  "Use ag to search DIRECTORY for Python functions potentially without docstrings and display results in a compilation-mode buffer."
  (interactive "DDirectory: ")
  (let* ((output-buffer (get-buffer-create "*Python Functions Without Docstrings*"))
          (full-directory (shell-quote-argument (expand-file-name directory)))
          (ag-command "ag")
          (ag-arguments `("--vimgrep" "--python"
                           "def\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s*\\([^)]*\\):\\s*\\n\\s*(?![\\s\\t]*(?:'''|\"\"\"))"
                           ,full-directory)))
    ;; Initialize the output buffer and temporarily disable read-only mode
    (with-current-buffer output-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Searching for Python functions without docstrings in " directory "...\n"))

    ;; Start the ag process
    (let ((process (apply 'start-file-process "ag-search" output-buffer ag-command ag-arguments)))
      (set-process-sentinel process
        (lambda (p e)
          (when (eq (process-status p) 'exit)
            (with-current-buffer (process-buffer p)
              (goto-char (point-max))
              (if (= (process-exit-status p) 0)
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward "^\\([a-zA-Z0-9_./-]+\\.py\\):\\([0-9]+\\):\\([0-9]+\\):" nil t)
                    (insert "\nSearch completed. Issues found.\n")
                    (insert "\nSearch completed. No functions without docstrings found.\n")))
                (insert (format "\nSearch failed with error code %d.\n" (process-exit-status p))))
              ;; Enable compilation-mode after writing is done
              (compilation-mode)
              (setq buffer-read-only t)
              (display-buffer (process-buffer p)))))))))


;; ------------------------------------------------------------------------- ;


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


(defun sdev-use-cpython-3 (&optional cpython)
  "Set defaults to use the standard interpreter instead of IPython.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Python command: "))))
  (when (not cpython)
    (setq cpython "python3"))
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

(defun sdev-use-venv (&optional cpython)
  "Set Emacs to use a Python interpreter from a virtual environment in the current directory.

With prefix arg, prompt for the command to use."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Python command: " (concat (expand-file-name default-directory) ".venv/bin/"))))
    (when (not cpython)
      (setq cpython (concat (expand-file-name default-directory) ".venv/bin/python")))
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
	(error "I don't know how to set ipython settings for this Emacs")))))


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


(defun sdev-custom-venv (&optional ipython)
  "Switch to a Python interpreter in the current directory or specify a custom script."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Path to Python shell script: " default-directory "remote-python.sh"))))
  ;; If IPython is not provided, look for 'remote-python.sh' in the current directory
  (setq ipython (or ipython (concat default-directory "remote-python.sh")))

  ;; Check if the specified interpreter exists, otherwise throw an error
  (unless (file-executable-p ipython)
    (error "Script %S not found or not executable" ipython))

  (setq python-shell-interpreter ipython
    python-shell-interpreter-args "-i"
    python-shell-prompt-regexp ">>> "
    python-shell-prompt-output-regexp ""))


(defun sdev-use-venv (&optional ipython)
  (interactive)
  (let ((script-path (concat (file-name-directory (buffer-file-name))
                       "remote-python.sh")))
    (unless (file-exists-p script-path) ; Check if the script file exists
      (with-temp-file script-path ; Create and write to the file
        (insert "#!/bin/bash\n"
          "# Activate the virtual environment\n"
          "source .venv/bin/activate\n"
          "# Hand off to the Python interpreter\n"
          "exec python -c 'import IPython; IPython.terminal.ipapp.launch_new_instance()' \"$@\"\n"))
      (set-file-modes script-path #o755) ; Make the script executable
      (message "remote-python.sh did not exist and was created."))
    (setq python-shell-interpreter script-path
      python-shell-interpreter-args "-i"
      python-shell-prompt-regexp ">>> "
      python-shell-prompt-output-regexp "")))




(defun sdev-use-remote (&optional ipython)
  (interactive)
  (setq python-shell-interpreter  "/tiqsi-emacs/modules/programming/remote-python.sh"
    python-shell-interpreter-args "-i"
    python-shell-prompt-regexp ">>> "
    python-shell-prompt-output-regexp ""))



(defun sdev-use-hetzner (&optional ipython)
  (interactive)
  (setq python-shell-interpreter  "ssh root@135.181.198.90 -t \"/opt/conda/bin/python $@\""
    python-shell-interpreter-args "-i"
    python-shell-prompt-regexp ">>> "
    python-shell-prompt-output-regexp ""))


;; ------------------------------------------------------------------------- ;


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Python Repl _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;



;; (straight-require 'blacken)
;; (straight-require 'pyimpsort)

;; ;; (straight-require 'company-quickhelp)


;; ;; (popwin-mode 1)

;; (defun tiqsi-py-view-plt(image_name)
;;   (interactive "sImage to view: ")
;;   (let ((image  image_name))

;;     (progn
;;       (comint-send-string "*Python*" (s-prepend
;;                                        (s-prepend "matplotlib.pyplot.savefig(\"" image) "\")\n") )
;;       (popwin:find-file image)
;;       )))


;; ;; (push '(".*.png" :regexp t :height 40 :width 15) popwin:special-display-config)

;; ;; (global-set-key (kbd))

;; (setq
;;   python-shell-interpreter "ipython"
;;   python-shell-interpreter-args "--matplotlib=qt5"
;;   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;   python-shell-completion-setup-code
;;   "from IPython.core.completerlib import module_completion"
;;   python-shell-completion-string-code
;;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
;;   python-shell-completion-module-string-code
;;   "';'.join(module_completion('''%s'''))\n"
;;   )



;; ;; ------------------------------------------------------------------------- ;
;; ;;                            Truncate Huge lines                            ;
;; ;; ------------------------------------------------------------------------- ;

;; ;; (defvar python-shell-output-chunks nil)

;; ;; (defun python-shell-filter-long-lines (string)
;; ;;   (push string python-shell-output-chunks)
;; ;;   (if (not (string-match comint-prompt-regexp string))
;; ;;       ""
;; ;;     (let* ((out (mapconcat #'identity (nreverse python-shell-output-chunks) ""))
;; ;;            (split-str (split-string out "\n"))
;; ;;            (max-len (* 2 (window-width)))
;; ;;            (disp-left (round (* (/ 1.0 3) (window-width))))
;; ;;            (disp-right disp-left)
;; ;;            (truncated (mapconcat
;; ;;                        (lambda (x)
;; ;;                          (if (> (length x) max-len)
;; ;;                              (concat (substring x 0 disp-left) " ... (*TRUNCATED*) ... " (substring x (- disp-right)))
;; ;;                            x))
;; ;;                        split-str "\n")))
;; ;;       (setq python-shell-output-chunks nil)
;; ;;       truncated)))

;; ;; TODO IMPROVE THIS IS AWESOME
;; ;; ;; TODO Further edit this to handle different regexp matches , ps! loving the
;; ;; ;; performance boost from not hitting gap buffer limitations
;; ;; (defun python-shell-filter-long-lines (string)
;; ;;   (push string python-shell-output-chunks)
;; ;;     (if (not (string-match comint-prompt-regexp string))
;; ;;       ""
;; ;;       (let ((out (mapconcat #'identity (nreverse python-shell-output-chunks) ""))
;; ;;          (max-len (window-width))
;; ;;          )
;; ;;      (setq python-shell-output-chunks nil)
;; ;;      (if (> (length out) max-len)
;; ;;          ;; (mapconcat '(lambda (x) (s-word-wrap 90 x) )(s-split "\s+" out) "")
;; ;;          (s-prepend "\n" (s-word-wrap 80 (s-trim out)))
;; ;;        out)
;; ;; )))

;; ;; (add-hook 'comint-preoutput-filter-functions #'python-shell-filter-long-lines)

;; ;; ------------------------------------------------------------------------- ;




;; ;; ------------------------------------------------------------------------- ;



;; ;; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Python Repl ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;



;; ;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \_ _ IMenu _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;
;; ;; ;; Python mode
;; ;; (defun my-merge-imenu ()
;; ;;   (interactive)
;; ;;   (let ((mode-imenu (imenu-default-create-index-function))
;; ;;          (custom-imenu (imenu--generic-function imenu-generic-expression)))
;; ;;     (append mode-imenu custom-imenu)))


;; ;; (defun my-python-menu-hook()
;; ;;   (interactive)
;; ;;   (add-to-list
;; ;;     'imenu-generic-expression
;; ;;     '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))
;; ;;   (setq imenu-create-index-function 'my-merge-imenu)
;; ;;   ;; (eval-after-load "company"
;; ;;   ;;     '(progn
;; ;;   ;;         (unless (member 'company-jedi (car company-backends))
;; ;;   ;;             (setq comp-back (car company-backends))
;; ;;   ;;             (push 'company-jedi comp-back)
;; ;;   ;;             (setq company-backends (list comp-back)))))
;; ;;   )


;; ;; (add-hook 'python-mode-hook 'my-python-menu-hook)

;; ;; _ _ _ _ _ _ _ _ _ _ _ _ _ _   /¯¯¯ IMenu ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;

;; ;; _ _ _ _ _ _ _ _ _ _ _ _ _ _   /¯¯¯ Tools ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;


;; (defun sdev/py-mccabe()
;;   "Get the mccabe complexity for this buffer.
;;    ; requires pip install mccabe                                                                       ;
;;   "
;;   (interactive)
;;   (message
;;     (shell-command-to-string(message "python -m mccabe --min 3 %s" buffer-file-name))))


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \_ _ Tools _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Debugging _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;
;; ;; Highlight the call to ipdb                                                                        ;
;; ;; src http://pedrokroger.com/2010/07/configuring-emacs-as-a-python-ide-2/                           ;


;; ;; (defun annotate-pdb ()
;; ;;   (interactive)
;; ;;   (highlight-lines-matching-regexp "import ipdb")
;; ;;   (highlight-lines-matching-regexp "ipdb.set_trace()"))
;; ;; (add-hook 'python-mode-hook 'annotate-pdb)

;; ;; (defun ipdb-add-breakpoint ()
;; ;;   "Add a break point"
;; ;;   (interactive)
;; ;;   (newline-and-indent)
;; ;;   (insert "import ipdb; ipdb.set_trace()")
;; ;;   (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

;; ;; (defun ipdb-cleanup ()
;; ;;   (interactive)
;; ;;   (save-excursion
;; ;;     (replace-regexp ".*ipdb.set_trace().*\n" "" nil (point-min) (point-max))
;; ;;     ;; (save-buffer)
;; ;;     ))

;; ;; ------------------------------------------------------------------------- ;
;; ;;                             TODO: nice feature                            ;
;; ;; ------------------------------------------------------------------------- ;
;; ;;                         Automatic printf debugging                        ;
;; ;; ------------------------------------------------------------------------- ;
;; ;;                  Automatically generate a print command:                  ;
;; ;;                 -> Under every for with print(num: {for_i})               ;
;; ;;               -> Under every if with print(num {if_var_test})             ;
;; ;;                              - include elif & else                        ;
;; ;;                       -> Maybe an extra string to put.                    ;
;; ;; ------------------------------------------------------------------------- ;
;; ;; def parse_value(value, fn:list, resource:list):                           ;
;; ;;     for name in value.keys():                                             ;
;; ;;         if type(value[name]) == list:                                     ;
;; ;;             temp = []                                                     ;
;; ;;             params = []                                                   ;
;; ;;             for parameter in value[name]:                                 ;
;; ;;                 if type(parameter) == str:                                ;
;; ;;                     params.append((parameter, None))                      ;
;; ;;                 else:                                                     ;
;; ;;                     parameter_l = list(parameter.keys())[0]               ;
;; ;;                     params.append((parameter_l, parameter[parameter_l]))  ;
;; ;;             temp.append((name, params))                                   ;
;; ;;             fn.append(temp)                                               ;
;; ;;             return fn, resource                                           ;
;; ;;         elif type(value[name] == dict):                                   ;
;; ;;             if type(value[name]) == dict:                                 ;
;; ;;                 if value[name] is not dict:                               ;
;; ;;                     fn.append((name, (None, None)))                       ;
;; ;;                     print('empty')                                        ;
;; ;;                     return fn, resource                                   ;
;; ;;                 elif type(value[name]) == str:                            ;
;; ;;                     fn.append((name, (value[name], None)))                ;
;; ;;                     return fn, resource                                   ;
;; ;;         else:                                                             ;
;; ;;             resource.append(name)                                         ;
;; ;;             return fn, resource                                           ;
;; ;; ------------------------------------------------------------------------- ;



;; (setq test_t "def parse_value(value, fn:list, resource:list):
;;     for name in value.keys():
;;         if type(value[name]) == list:
;;             temp = []
;;             params = []
;;             for parameter in value[name]:
;;                 if type(parameter) == str:
;;                     params.append((parameter, None))
;;                 else:
;;                     parameter_l = list(parameter.keys())[0]
;;                     params.append((parameter_l, parameter[parameter_l]))
;;             temp.append((name, params))
;;             fn.append(temp)
;;             return fn, resource
;;         elif type(value[name] == dict):
;;             if type(value[name]) == dict:
;;                 if value[name] is not dict:
;;                     fn.append((name, (None, None)))
;;                     print('empty')
;;                     return fn, resource
;;                 elif type(value[name]) == str:
;;                     fn.append((name, (value[name], None)))
;;                     return fn, resource
;;         else:
;;             resource.append(name)
;;             return fn, resource")


;; ;; ------------------------------------------------------------------------- ;
;; ;;         TODO pprint pandas dataframes when in python inferior mode        ;
;; ;; ------------------------------------------------------------------------- ;


;; ;; Out[1188]:
;; ;;                                                   email            role                              shist
;; ;; 1514  8544ac18bb8509e055de298ed5d135b77fa7a31e897b8d...  Trust & Safety  (Trust & Safety, Empty histogram)


;; (defun tag-word-or-region (tag)
;;   "Surround current word or region with a given tag."
;;   (interactive "sEnter tag (without <>): ")
;;   (let (pos1 pos2 bds start-tag end-tag)
;;     (setq start-tag (concat "<" tag ">"))
;;     (setq end-tag (concat "</" tag ">"))
;;     (if (and transient-mark-mode mark-active)
;;       (progn
;;         (goto-char (region-end))
;;         (insert end-tag)
;;         (goto-char (region-beginning))
;;         (insert start-tag))
;;       (progn
;;         (setq bds (bounds-of-thing-at-point 'symbol))
;;         (goto-char (cdr bds))
;;         (insert end-tag)
;;         (goto-char (car bds))
;;         (insert start-tag)))))



;; ;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Debugging ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _    ;

;; ;; _ _ _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Linting ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _     ;


;; ;; (flycheck-define-checker
;; ;;     python-mypy ""
;; ;;     :command ("mypy"
;; ;;               "--ignore-missing-imports"
;; ;;               "--python-version" "3.6"
;; ;;               source-original)
;; ;;     :error-patterns
;; ;;     ((error line-start (file-name) ":" line ": error:" (message) line-end))
;; ;;     :modes python-mode)

;; ;; (add-to-list 'flycheck-checkers 'python-mypy t)
;; ;; (flycheck-add-next-checker 'python-pylint 'python-mypy t)


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Linting _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯     ;

;; ;; _ _ _ _ _ _ _ _ _ _ _ _ /¯¯¯ Jupyter Notebooks ¯¯¯\_ _ _ _ _ _ _ _ _ _    ;



;; (use-package ein
;;   :straight t
;;   :ensure t
;;   :config
;;   (progn

;;     ;; (add-hook 'poly-ein-mode-hook '(lambda ()
;;     ;;                    (set (make-local-variable 'linum-mode) nil)))

;;     ;; (define-key poly-ein-mode-map (kbd "C-n") 'ein:worksheet-goto-next-input-km)
;;     ;; (define-key poly-ein-mode-map (kbd "C-b") 'ein:worksheet-goto-prev-input-km)

;;     ))

;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ \_ _ Jupyter Notebooks _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;

;; ;; _ _ _ _ _ _ _ _ _ _ _ _   /¯¯¯ Miscellaneous ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _  ;

;; ;; Enter key executes newline-and-indent
;; (defun set-newline-and-indent ()
;;   "Map the return key with `newline-and-indent'"
;;   (local-set-key (kbd "RET") 'newline-and-indent))
;; (add-hook 'python-mode-hook 'set-newline-and-indent)


;; (defun sdev/py-sort-imports ()
;;   (interactive)
;;   (mark-whole-buffer)
;;   (py-isort-region))

;; (use-package hy-mode
;;   :straight t
;;   :ensure t
;;   :config(progn))

;; (with-system darwin
;;   (sdev-use-ipython))


;; ;; _ _ _ _ _ _ _ _ _ _ _ _   /¯¯¯ Documentation ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _  ;


;; (defun insert-param-info (var-string)
;;   (let (
;;          (var-name (car(s-split ":" var-string)))
;;          (var-type (car(cdr(s-split ":" var-string))))
;;          (var-desc (cdr(cdr(s-split ":" var-string))))
;;          )
;;     (insert (s-prepend var-name " : "))
;;     (insert (s-prepend var-type "\n"))
;;     (insert (s-prepend (s-prepend "   " (format "%s" (car var-desc))) "\n") )
;;     (newline)
;;     )
;;   )

;; (defun insert-val-desc (var-string)
;;   (let (
;;          (var-name (car(s-split ":" var-string)))
;;          (var-desc (cdr(s-split ":" var-string)))
;;          )
;;     (insert (s-prepend var-name "\n"))
;;     (insert (s-prepend (s-prepend "    " (format "%s" (car var-desc))) "\n") )
;;     (newline)
;;     )
;;   )

;; (defun tiqsi-numpydoc (description params return raises doctest result)
;;   """     infer column types using pandas

;;     Parameters
;;     ----------

;;     df : pandas.DataFrame
;;         the dataframe from which column types will be extracted

;;     Returns
;;     -------

;;     Dictionary
;;         A python dictionary containing the type information of each column
;;    """
;;   (interactive "sEnter Description:
;; sEnter param list :
;; sEnter return type info :
;; sEnter exception info :
;; sEnter Doctest :
;; sEnter Doctest result: ")
;;   (insert "\"\"\"")
;;   (if (not(equal description ""))
;;     (progn(newline)
;;       (insert (s-prepend description "\n")))
;;     ())
;;   (if (not(equal params ""))
;;     (progn
;;       (newline)
;;       (insert "Parameters\n")
;;       (insert "----------\n")
;;       (newline)
;;       (-map 'insert-param-info  (s-split ", " params))

;;       )
;;     ())

;;   (if (not(equal return ""))
;;     (progn
;;       (insert "Returns\n")
;;       (insert "-------\n")

;;       (newline)
;;       (insert-val-desc return)
;;       )
;;     ())

;;   (if (not(equal raises ""))
;;     (progn
;;       (insert "Raises\n")
;;       (insert "------\n")
;;       (newline)
;;       (insert-val-desc raises)
;;       )
;;     ())

;;   (if (not(equal doctest ""))
;;     (progn
;;       (newline)
;;       (insert "Doctest\n")
;;       (insert "-------\n")
;;       (insert  (s-join doctest '(">>> """)))
;;       (newline)
;;       (insert (s-join result '("""")))
;;       (newline)
;;       (insert "\"\"\""))
;;     (insert "\n    \"\"\""))
;;   )


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \_ _ Documentation _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;

;; (defun get-cwd()
;;   (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))

;; (defun deploy-gcloud()
;;   (interactive)
;;   (let ((current-command  (s-prepend
;;                             (s-prepend "gcloud functions deploy "
;;                               (s-replace "-" "_" (get-cwd)))
;;                             " --runtime python37 --ingress-settings internal-only --trigger-http ") ))
;;     (pos-tip-show current-command)
;;     (async-shell-command current-command)))

;; (defun deploy-gcloud-local()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend
;;             (s-prepend "functions-framework --target "
;;               (s-replace "-" "_" (get-cwd)))
;;             "  ") ))
;;     (pos-tip-show current-command)
;;     (shell-command current-command)))


;; (defun deploy-gcloud-u()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend
;;             (s-prepend "gcloud functions deploy "
;;               (s-replace "-" "_" (get-cwd)))
;;             " --service-account detect-np-misuse@cloudflare-detection-response.iam.gserviceaccount.com --runtime python37  --trigger-http --allow-unauthenticated") ))
;;     (pos-tip-show current-command)
;;     (kill-new current-command)
;;     (async-shell-command current-command)))


;; ;; TODO fix
;; (defun deploy-gcloud-topic-u()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend
;;             (s-prepend "gcloud functions deploy schedule-np-detect --trigger-topic "
;;               (s-replace "-" "_" (get-cwd)))
;;             " --runtime python37 --allow-unauthenticated") ))
;;     (pos-tip-show current-command)
;;     (insert current-command)
;;     (async-shell-command current-command)))


;; (defun describe-gcloud()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend "gcloud functions describe "
;;             (s-replace "-" "_" (get-cwd)))))
;;     (pos-tip-show current-command)
;;     (async-shell-command current-command)))


;; (defun list-logs-gcloud()
;;   (interactive)
;;   (let ((current-command  "gcloud logging logs list "
;;           ))
;;     (pos-tip-show current-command)
;;     (async-shell-command current-command)))



;; (defun list-current-cfun-log-gcloud()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend
;;             (s-prepend "gcloud logging read \"resource.type=cloud_function AND resource.labels.function_name="
;;               (s-replace "-" "_" (get-cwd))) "\" --freshness=10M --order=desc --format=json --limit=10")  ))
;;     (pos-tip-show current-command)
;;     (async-shell-command current-command)))



;; (defun list-current-cfun-log-error-gcloud()
;;   (interactive)
;;   (let ((current-command
;;           (s-prepend
;;             (s-prepend "gcloud logging read \"severity>=ERROR AND resource.type=cloud_function AND resource.labels.function_name="
;;               (s-replace "-" "_" (get-cwd))) "\" --freshness=10M --order=desc --format=json --limit=10")  ))
;;     (pos-tip-show current-command)
;;     (async-shell-command current-command)))


;; ;; resource.type="cloud_function"
;; ;; resource.labels.function_name="detect_np_stats"
;; ;; resource.labels.region="us-central1"
;; ;; logName="projects/endpoint-forensics-collector/logs/cloudfunctions.googleapis.com%2Fcloud-functions"


;; ;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   \_ _ Miscellaneous _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;


;; ;; (eval-after-load 'python-mode
;; ;;   (progn
;; ;;     (define-key python-mode-map (kbd "C-c n") 'flycheck-next-error)
;; ;;     (define-key python-mode-map (kbd "C-c p") 'flycheck-previous-error)
;; ;;     (define-key python-mode-map (kbd "C-c l") 'flycheck-list-errors)
;; ;;     ))

;; ;; ------------------------------------------------------------------------- ;

;; ;; Optionally delete echoed input (after checking it).
;; (when (and comint-process-echoes (not artificial))
;;   (let ((echo-len (- comint-last-input-end
;;                     comint-last-input-start)))
;;     ;; Wait for all input to be echoed:
;;     (while (and (> (+ comint-last-input-end echo-len)
;;                   (point-max))
;;              (accept-process-output proc)
;;              (zerop
;;                (compare-buffer-substrings
;;                  nil comint-last-input-start
;;                  (- (point-max) echo-len)
;;                  ;; Above difference is equivalent to
;;                  ;; (+ comint-last-input-start
;;                  ;;    (- (point-max) comint-last-input-end))
;;                  nil comint-last-input-end (point-max)))))
;;     (if (and
;;           (<= (+ comint-last-input-end echo-len)
;;             (point-max))
;;           (zerop
;;             (compare-buffer-substrings
;;               nil comint-last-input-start comint-last-input-end
;;               nil comint-last-input-end
;;               (+ comint-last-input-end echo-len))))
;;       ;; Certain parts of the text to be deleted may have
;;       ;; been mistaken for prompts.  We have to prevent
;;       ;; problems when `comint-prompt-read-only' is non-nil.
;;       (let ((inhibit-read-only t))
;;         (delete-region comint-last-input-end
;;           (+ comint-last-input-end echo-len))
;;         (when comint-prompt-read-only
;;           (save-excursion
;;             (goto-char comint-last-input-end)
;;             (comint-update-fence)))))))

;; (defun comint-run-thing-process (process command)
;;   "Send COMMAND to PROCESS."
;;   (let ((output-buffer " *Comint Redirect Work Buffer*"))
;;     (with-current-buffer (get-buffer-create output-buffer)
;;       (erase-buffer)
;;       (comint-redirect-send-command-to-process command
;;         output-buffer process nil t)
;;       ;; Wait for the process to complete
;;       (set-buffer (process-buffer process))
;;       (while (and (null comint-redirect-completed)
;;                (accept-process-output process)))
;;       ;; Collect the output
;;       (set-buffer output-buffer)
;;       (goto-char (point-min))
;;       ;; Skip past the command, if it was echoed
;;       (and (looking-at command)
;;         (forward-line))
;;       ;; Grab the rest of the buffer
;;       (buffer-substring-no-properties (point) (- (point-max) 1)))))

;; ;; ------------------------------------------------------------------------- ;



;; (defun toggle-camelcase-underscores ()
;;   "Toggle between camelcase and underscore notation for the symbol at point."
;;   (interactive)
;;   (save-excursion
;;     (let* ((bounds (bounds-of-thing-at-point 'symbol))
;;             (start (car bounds))
;;             (end (cdr bounds))
;;             (currently-using-underscores-p (progn (goto-char start)
;;                                              (re-search-forward "_" end t))))
;;       (if currently-using-underscores-p
;;         (progn
;;           (upcase-initials-region start end)
;;           (replace-string "_" "" nil start end)
;;           (downcase-region start (1+ start)))
;;         (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
;;         (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))



;; ;; ------------------------------------------------------------------------- ;
;; ;; ------------------------------------------------------------------------- ;

;; (straight-require 'py-isort)

;; (defun sdev/py-sort-imports ()
;;   (interactive)
;;   (mark-whole-buffer)
;;   (py-isort-region))

;; (defun paste-to-python-list ()
;;   "Convert clipboard contents to Python list format."
;;   (interactive)
;;   (let* ((raw-text (current-kill 0))
;;           (items (split-string raw-text)))
;;     (kill-new (format "['%s']" (mapconcat 'identity items "', '"))))
;;   (yank))








;; ;; ------------------------------------------------------------------------- ;
;; ;;                               FASTHTML stuff                              ;
;; ;; ------------------------------------------------------------------------- ;

;; ;; (fset 'fh-html-to-link
;; ;;   (kmacro "L i n k ( C-SPC C-s r e <left> <left> C-w C-s \" C-s <right> <left> , C-S-a <backspace> <backspace> ) C-a <down>"))


;; ;; (fset 'fh-flag-to-FLAG
;; ;;   (kmacro "M-x s u b w o r d - u p c a s e <return> M-x <return> SPC = SPC F l a g ( C-a C-SPC M-<right> M-<right> C-q C-S-a \" C-f \" , SPC \" C-f M-<left> M-<left> M-x s u b w o r d - d o w n <return> M-x <return> \" ) C-a <down> M-x C-g"))




;; ;; ;;; ein:notebook-save-notebook-command when in ein mode
;; ;;   (defun save-buffer (&optional arg)
;; ;;     (interactive "p")
;; ;;     (if (eq major-mode 'ein:notebook-multilang-mode)
;; ;;         (ein:notebook-save-notebook-command)
;; ;;       (let ((modp (buffer-modified-p))
;; ;;             (make-backup-files (or (and make-backup-files (not (eq arg 0)))
;; ;;                                    (memq arg '(16 64)))))
;; ;;         (and modp (memq arg '(16 64)) (setq buffer-backed-up nil))
;; ;;         (if (and modp (buffer-file-name))
;; ;;             (message "Saving file %s..." (buffer-file-name)))
;; ;;         (basic-save-buffer)
;; ;;         (and modp (memq arg '(4 64)) (setq buffer-backed-up nil)))))

;; ;; ;;; advice /defadvice that fails
;; ;; (defun save-ein()
;; ;;     (if (eq major-mode 'ein:notebook-multilang-mode)
;; ;;       (ein:notebook-save-notebook-command))))
;; ;; (advice-add 'save-buffer :before #'save-ein)


;; ;; ------------------------------------------------------------------------- ;

;; (with-system darwin
;;   (check-exec "pyright"
;;     ((message "pyright is installed at %s" (executable-find "pyright")))
;;     ((progn
;;        (message "pyright is not installed")
;;        (sdev/brew-install "pyright")
;;        ))))


;; ;; (use-package lsp-pyright
;; ;;   :ensure t
;; ;;   :hook (python-mode . (lambda ()
;; ;;                          (require 'lsp-pyright)
;; ;;                          (lsp))))  ; or lsp-deferred


(defun break-line-at-80 ()
  "Move to the first space before column 80 on the current line and insert a newline."
  (interactive)
  (let ((limit 80))
    (save-excursion
      (beginning-of-line)
      (if (<= (line-end-position) (+ (line-beginning-position) limit))
        (message "Line is shorter than 80 columns, no break needed.")
        (move-to-column limit)
        (search-backward " " (line-beginning-position) t)
        (newline)))))

(global-set-key (kbd "C-c b") 'break-line-at-80)


(defun go-to-first-line-over-80 ()
  "Move to the first line after the current line where the content exceeds 80 characters."
  (interactive)
  (let ((limit 80)
         (found nil))
    (while (and (not found) (not (eobp)))
      (forward-line 1)
      (when (> (line-end-position) (+ (line-beginning-position) limit))
        (setq found t)))
    (if found
      (move-to-column (1+ limit) t)
      (message "No line over 80 characters found after the current line."))))


(global-set-key (kbd "C-c o") 'go-to-first-line-over-80)



;; (use-package lsp-pyright
;;   :ensure t
;;   :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;; 			 (lsp-diagnostics-flycheck-enable)

;;                          (lsp))))






(defun split-long-string (s &optional min-length max-length)
  (let* ((min-length (or min-length 60))
          (max-length (or max-length 80))
          (match (string-match "\\`\\([fFrRbB]*\\)\\(['\"]\\{1,3\\}\\)\\(.*\\)\\2\\(.*\\)\\'" s))
          (prefix (if match (match-string 1 s) ""))
          (quote-char (if match (substring (match-string 2 s) 0 1) "\""))
          (content (if match (match-string 3 s) s))
          (trailing (if match (match-string 4 s) ""))
          (triple-quote (make-string 3 (string-to-char quote-char)))
          split-lines)
    (while (> (length content) max-length)
      (let ((split-pos nil)
             (pos-forward (string-match "[ \t]" content min-length)))
        (if (and pos-forward (<= pos-forward max-length))
          (setq split-pos pos-forward)
          (let ((rev-sub (substring content 0 min-length))
                 (last-space-pos nil)
                 (search-pos 0))
            (while (string-match "[ \t]" rev-sub search-pos)
              (setq last-space-pos (match-beginning 0))
              (setq search-pos (1+ last-space-pos)))
            (setq split-pos (or last-space-pos max-length))))
        (push (string-trim (substring content 0 split-pos)) split-lines)
        (setq content (string-trim-left (substring content split-pos)))))
    (push content split-lines)
    (concat prefix triple-quote "\n"
      (mapconcat #'identity (reverse split-lines) "\n")
      "\n" triple-quote trailing)))


(defun split-long-string-at-point (&optional min-length max-length)
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'line))
          (start (car bounds))
          (end (cdr bounds))
          (original (string-trim (buffer-substring-no-properties start end)))
          (processed (split-long-string original min-length max-length)))
    (unless (string= original processed)
      (delete-region start end)
      (insert processed))))







(setq eldoc-idle-delay 0.75)  ;; or even 1.0
(setq eldoc-echo-area-use-multiline-p nil)
(add-hook 'python-mode-hook (lambda () (eldoc-mode -1)))
(remove-hook 'python-mode-hook #'eldoc-mode)
(global-eldoc-mode -1)

(setq comint-buffer-maximum-size 1000)


(setq lsp-idle-delay 0.05)
(setq lsp-log-io nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-snippet nil)
(setq lsp-enable-folding nil)



;; ------------------------------------------------------------------------- ;;


(use-package flymake-ruff
  :straight (flymake-ruff
              :type git
              :host github
              :repo "erickgnavar/flymake-ruff"))



;; Prevent Flymake from running automatically on changes
(setq flymake-no-changes-timeout nil)  ;; Disable auto-triggering on edits
(setq flymake-start-on-save-buffer t)  ;; Allow Flymake on save
(setq flymake-start-on-flymake-mode nil)  ;; Prevent immediate checks when opening a file

;; Run Flymake Ruff only once per save
(defun my-flymake-run-on-save ()
  "Run Flymake Ruff only when the buffer is saved, not on edits."
  (remove-hook 'flymake-diagnostic-functions 'flymake-ruff--run-checker t)
  (add-hook 'after-save-hook
    (lambda ()
      (add-hook 'flymake-diagnostic-functions 'flymake-ruff--run-checker nil t)
      (flymake-start)
      (remove-hook 'flymake-diagnostic-functions 'flymake-ruff--run-checker t))
    nil t))

(add-hook 'python-mode-hook #'my-flymake-run-on-save)



(setq semantic-idle-scheduler-work-idle-time 5)
(setq semantic-idle-scheduler-max-buffer-size 500000)
(setq semantic-lex-python-mode nil)
(remove-hook 'semantic-idle-scheduler-functions 'semantic-idle-scheduler-refresh-tags)


;; Disable Semantic Mode for Python
(add-hook 'python-mode-hook (lambda () (semantic-mode -1)))
(global-semantic-idle-scheduler-mode -1)


(add-hook 'python-mode-hook
  (lambda ()
    (flymake-mode)
    (flymake-ruff-load)
    (flymake-start)
    )
  )



(use-package lsp-pyright
  :straight t
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))) 


(defun insert-local-global-modes ()
  "Insert a line showing the current major mode and enabled minor modes."
  (interactive)
  (let* ((enabled (seq-filter (lambda (sym) (and (boundp sym) (symbol-value sym)))
                    minor-mode-list))
          (result  (format "Major: %s | Enabled minors: %S" major-mode enabled)))
    (insert result)
    (message "%s" result)))

;; Ensure lsp, lsp-pyright and uv are installed first.
(with-eval-after-load 'lsp-pyright
  ;; Always start Pyright through the project’s uv-managed venv
  (setq lsp-pyright-langserver-command
    '("uv" "run" "--" "pyright-langserver" "--stdio"))
  ;; Tell Pyright where the venv lives so import resolution is correct
  (setq lsp-pyright-venv-path ".")      ; path (project root)
  (setq lsp-pyright-venv-directory ".venv"))



(defun quick-pyright-check ()
  (interactive)
  (let* ((pyright-exe (executable-find "pyright"))
          (source-file (buffer-file-name))
          (output-buffer "*Pyright Errors*")
          (current-window (selected-window))
          (other-window (next-window current-window nil t)))
    (unless pyright-exe
      (error "Pyright executable not found."))
    (with-temp-buffer
      (call-process pyright-exe nil (current-buffer) nil "--outputjson" source-file)
      (goto-char (point-min))
      (let* ((json-object-type 'hash-table)
              (json-array-type 'vector)
              (json (json-parse-buffer))
              (diags (append (gethash "generalDiagnostics" json) nil)))
        (with-current-buffer (get-buffer-create output-buffer)
          (erase-buffer)
          (if (null diags)
            (insert "No diagnostics found by Pyright.\n")
            (insert (format "Pyright found %d issues:\n\n" (length diags)))
            (dolist (diag diags)
              (let* ((severity (capitalize (gethash "severity" diag)))
                      (message (gethash "message" diag))
                      (range (gethash "range" diag))
                      (start (gethash "start" range))
                      (line (1+ (gethash "line" start)))
                      (col (1+ (gethash "character" start)))
                      (header (format "[%s] Line %-4d Col %-3d \n%s:%d:%d\n"
                                severity line col source-file line col))
                      (wrapped-message (with-temp-buffer
                                         (insert message)
                                         (fill-region (point-min) (point-max) nil nil 100)
                                         (indent-rigidly (point-min) (point-max) 4)
                                         (buffer-string))))
                (insert header wrapped-message "\n\n"))))
          (compilation-mode))
        (with-selected-window other-window
          (let ((display-buffer-alist
                  `(("*Pyright Errors*" . ((display-buffer-reuse-window
                                             display-buffer-same-window))))))
            (pop-to-buffer output-buffer)))))))


(use-package python
  :preface
  (defvar-local my/venv-name nil)
  (defun my/find-project-root ()
    (or (locate-dominating-file default-directory "pyrightconfig.json")
      (cl-loop for d = default-directory then (file-name-directory (directory-file-name d))
        while d thereis
        (let ((toml-file (expand-file-name "pyproject.toml" d)))
          (and (file-exists-p toml-file)
            (with-temp-buffer
              (insert-file-contents toml-file)
              (goto-char (point-min))
              (search-forward "[tool.pyright]" nil t) d))))))
  (defun my/bootstrap-python ()
    (let* ((root (my/find-project-root))
            (cfg  (and root (expand-file-name "pyrightconfig.json" root)))
            (venv-root (or (and root
                             (cl-loop for n in '(".venv" "venv")
                               thereis (let ((p (expand-file-name n root)))
                                         (when (file-directory-p p) p))))
                         (getenv "VIRTUAL_ENV")))
            (venv-bin (and venv-root (concat (file-name-as-directory (tramp-file-local-name venv-root)) "bin/")))
            (py      (and venv-bin (concat venv-bin "python"))))
      (setq-local compile-command (if cfg
                                    (format "pyright --project %s" (shell-quote-argument cfg))
                                    "pyright"))
      (when (and cfg (bound-and-true-p lsp-mode))
        (setq-local lsp-pyright-project-root root))
      (when venv-bin
        (setq-local python-shell-interpreter py
          python-shell-virtualenv-root venv-root
          my/venv-name (file-name-nondirectory (directory-file-name venv-root)))
        (make-local-variable 'exec-path)
        (add-to-list 'exec-path venv-bin)
        (make-local-variable 'process-environment)
        (setenv "PATH" (mapconcat #'identity exec-path ":"))
        (setenv "VIRTUAL_ENV" venv-root))))
  (define-minor-mode my/python-autoenv-mode
    "" nil "" nil
    (if my/python-autoenv-mode
      (progn
        (my/bootstrap-python)
        (unless (assoc 'my/python-autoenv-mode mode-line-misc-info)
          (push '(my/python-autoenv-mode
                   (:eval (when my/python-autoenv-mode
                            (format " V:%s" (or my/venv-name "")))))
            mode-line-misc-info)))
      (kill-local-variable 'python-shell-interpreter)
      (kill-local-variable 'python-shell-virtualenv-root)
      (kill-local-variable 'exec-path)
      (kill-local-variable 'process-environment)
      (setq-local my/venv-name nil)))
  :hook ((python-mode . my/python-autoenv-mode)
          (python-mode . (lambda () (my/python-autoenv-mode 1)))
          (compilation-mode . (lambda ()
				(unless (assoc 'pyright compilation-error-regexp-alist-alist)
                                  (add-to-list 'compilation-error-regexp-alist 'pyright)
                                  (add-to-list 'compilation-error-regexp-alist-alist
                                    '(pyright "^\\s-+\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\).+$" 1 2 3))))))
  :config
  (setq python-shell-completion-native-enable nil))



(defun sdev/copy-diagnostic-at-point ()
  (interactive)
  (let ((msg
          (or
            ;; Flymake
            (when (and (bound-and-true-p flymake-mode)
                    (fboundp 'flymake-diagnostics))
              (let ((diag (car (flymake-diagnostics (point)))))
		(when diag (flymake-diagnostic-text diag))))
            ;; Flycheck
            (when (and (bound-and-true-p flycheck-mode)
                    (fboundp 'flycheck-overlay-errors-at))
              (let ((err (car (flycheck-overlay-errors-at (point)))))
		(when err (flycheck-error-message err))))
            ;; LSP overlays as last resort
            (when (fboundp 'lsp--point-diagnostics)
              (let ((diag (car (lsp--point-diagnostics))))
		(when diag (lsp-diagnostic-message diag)))))))
    (if msg
      (progn (kill-new msg)
        (message "Copied diagnostic: %s" msg))
      (user-error "No diagnostic at point"))))

(define-key python-mode-map (kbd "C-c C-d") 'sdev/copy-diagnostic-at-point)




(defun my/--lines-around (line up down)
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (- line up 1)))
    (let ((start (point)))
      (goto-char (point-min))
      (forward-line (min (line-number-at-pos (point-max)) (+ line down)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))




(defun sdev/copy-diagnostic-context ()
  "Copy diagnostic message, ±2 lines of context around the line of the diagnostic (not just point).
If possible, also grabs ±5 lines around the symbol declaration via LSP."
  (interactive)
  (let* (
          ;; Try Flymake at point
          (diag
            (or
              (when (and (bound-and-true-p flymake-mode)
                      (fboundp 'flymake-diagnostics))
		(car (flymake-diagnostics (point))))
              (when (and (bound-and-true-p flycheck-mode)
                      (fboundp 'flycheck-overlay-errors-at))
		(car (flycheck-overlay-errors-at (point))))
              (when (fboundp 'lsp--point-diagnostics)
		(car (lsp--point-diagnostics)))))
          (diag-msg
            (cond
              ((and diag (fboundp 'flymake-diagnostic-p) (flymake-diagnostic-p diag))
		(flymake-diagnostic-text diag))
              ((and diag (fboundp 'flycheck-error-p) (flycheck-error-p diag))
		(flycheck-error-message diag))
              ((and diag (hash-table-p diag))
		(lsp-diagnostic-message diag))))
          ;; Determine the actual source line of the squiggly, 1-based
          (diag-line
            (cond
              ((and diag (fboundp 'flymake-diagnostic-p) (flymake-diagnostic-p diag))
		(save-excursion
		  (goto-char (flymake-diagnostic-beg diag))
		  (line-number-at-pos)))
              ((and diag (fboundp 'flycheck-error-p) (flycheck-error-p diag))
		(flycheck-error-line diag))
              ((and diag (hash-table-p diag))
		(let ((range (gethash "range" diag)))
		  (when range
                    (let ((start (gethash "start" range)))
                      (when start
			(1+ (gethash "line" start)))))))
              (t nil)))
          ;; Now always show ±2 of the real squiggly line
          (snippet (when (and diag-msg diag-line) (my/--lines-around diag-line 2 2)))
          (decl-snippet nil))
    (unless diag-msg
      (user-error "No diagnostic at point"))
    (when (and (fboundp 'lsp--capability)
            (lsp--capability "declarationProvider"))
      (let* ((params (lsp--text-document-position-params))
              (locs   (lsp-request "textDocument/declaration" params))
              (loc    (if (and locs (sequencep locs)) (car locs) locs)))
        (when (and loc (hash-table-p loc))
          (let* ((file      (lsp--uri-to-path (gethash "uri" loc)))
                  (start     (gethash "start" (gethash "range" loc)))
                  (decl-line (1+ (gethash "line" start))))
            (with-current-buffer (find-file-noselect file)
              (setq decl-snippet
                (format "─ Declaration @ %s:%d (±5)\n%s"
                  (file-name-nondirectory file)
                  decl-line
                  (my/--lines-around decl-line 5 5))))))))
    (let ((full (concat
                  "─ Diagnostic\n" diag-msg "\n\n"
                  (if (and diag-line)
                    (format "─ Context (±2, around line %d)\n" diag-line)
                    "─ Context (±2)\n")
                  (or snippet "")
                  (when decl-snippet (concat "\n\n" decl-snippet)))))
      (kill-new full)
      (message "Copied diagnostic context to clipboard"))))



(defun sdev/copy-hover-context (&optional buf-lines)
  "Copy LSP hover info at point, with ±N lines (default 2) of context."
  (interactive)
  (let* ((buf-lines (or buf-lines 2))
          (hover (lsp-request "textDocument/hover" (lsp--text-document-position-params)))
          (hover-str
            (let ((contents (and hover (gethash "contents" hover))))
              (cond
		((stringp contents) contents)
		((and (hash-table-p contents)
                   (gethash "kind" contents)
                   (gethash "value" contents)) ; MarkupContent
		  (gethash "value" contents))
		((and (sequencep contents) (= (length contents) 0)) "")
		((and (sequencep contents))
		  (mapconcat
                    (lambda (x)
                      (cond
			((stringp x) x)
			((and (hash-table-p x)
                           (gethash "value" x)) ; MarkedString as hash
			  (gethash "value" x))
			((and (hash-table-p x)
                           (gethash "language" x)
                           (gethash "value" x))
			  (format "```%s\n%s\n```"
                            (gethash "language" x)
                            (gethash "value" x)))
			(t (prin1-to-string x))))
                    contents "\n"))
		(t (prin1-to-string contents)))))
          (cur-line (line-number-at-pos))
          (context (my--lines-around cur-line buf-lines buf-lines))
          (out (concat
                 "─ Hover\n" (or hover-str "[no hover info]") "\n\n"
                 (format "─ Context (±%d, around line %d)\n" buf-lines cur-line)
                 context)))
    (when (string-empty-p (string-trim hover-str))
      (user-error "No hover info at point"))
    (kill-new out)
    (message "Copied hover context to clipboard")))


(define-key python-mode-map (kbd "C-c C-s") 'sdev/copy-diagnostic-context)




;; Ensure lsp, lsp-pyright, and flymake-ruff are installed and loaded

;; Enable both flymake-mode and lsp-mode for Python
(add-hook 'python-mode-hook
  (lambda ()
    (flymake-mode 1)
    (lsp-deferred)))

;; Manually add flymake-ruff as a backend, but trigger only on save
(defun my/flymake-ruff-on-save ()
  "Run Flymake Ruff after saving, in addition to any existing diagnostics."
  (when (derived-mode-p 'python-mode)
    ;; Prevent stacking redundant hooks
    (unless (member 'flymake-ruff--run-checker flymake-diagnostic-functions)
      (add-hook 'flymake-diagnostic-functions 'flymake-ruff--run-checker nil t))
    (flymake-start)))

(add-hook 'python-mode-hook
  (lambda ()
    (add-hook 'after-save-hook #'my/flymake-ruff-on-save nil t)))

;; Optional: ensure flymake-ruff is loaded when python-mode loads
(add-hook 'python-mode-hook #'flymake-ruff-load)

;; Optionally customize Flymake UI
(setq flymake-no-changes-timeout nil)    ; don't run on idle
(setq flymake-start-on-save-buffer t)    ; allow on save
(setq flymake-start-on-flymake-mode t)   ; start on mode activation

(add-hook 'python-mode-hook (lambda ()
                              (flymake-mode 1)
                              (lsp-deferred)))

;; ------------------------------------------------------------------------- ;

(defun sdev--next-diagnostic-pos-flymake ()
  (when (and (bound-and-true-p flymake-mode) (fboundp 'flymake-diagnostics))
    (let* ((cur (point))
            (nearest-pos nil))
      (dolist (diag (flymake-diagnostics (point-min) (point-max)))
        (let ((beg (flymake-diagnostic-beg diag)))
          (when (and (> beg cur)
                  (or (not nearest-pos) (< beg nearest-pos)))
            (setq nearest-pos beg))))
      (when nearest-pos (cons nearest-pos 'flymake)))))

(defun sdev--next-diagnostic-pos-flycheck ()
  (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-overlay-errors-in))
    (let* ((cur (point))
            (nearest-pos nil))
      (dolist (err (flycheck-overlay-errors-in (point-min) (point-max)))
        (let ((beg (flycheck-error-pos err)))
          (when (and beg (> beg cur)
                  (or (not nearest-pos) (< beg nearest-pos)))
            (setq nearest-pos beg))))
      (when nearest-pos (cons nearest-pos 'flycheck)))))

(defun sdev--next-diagnostic-pos-lsp ()
  (when (and (boundp 'lsp-diagnostics) lsp-diagnostics)
    (let* ((cur (point))
            (nearest-pos nil))
      (maphash
	(lambda (_file diags)
          (dolist (diag diags)
            (let* ((range (gethash "range" diag))
                    (start (gethash "start" range))
                    (line (1+ (gethash "line" start)))
                    (char (gethash "character" start))
                    (pos (save-excursion
                           (goto-char (point-min))
                           (forward-line (1- line))
                           (forward-char char)
                           (point))))
              (when (and (> pos cur)
                      (or (not nearest-pos) (< pos nearest-pos)))
		(setq nearest-pos pos)))))
	lsp-diagnostics)
      (when nearest-pos (cons nearest-pos 'lsp)))))

(defun sdev/goto-next-diagnostic ()
  "Go to the next diagnostic in buffer, considering Flymake, Flycheck, or LSP overlays (whichever is closest).
Echoes which backend the jump is from."
  (interactive)
  (let ((orig-pos (point)))
    (let* ((fm (sdev--next-diagnostic-pos-flymake))
            (fc (sdev--next-diagnostic-pos-flycheck))
            (ls (save-excursion (goto-char orig-pos) (sdev--next-diagnostic-pos-lsp)))
            (all (delq nil (list fm fc ls)))
            (nearest (car (sort all (lambda (a b) (< (car a) (car b)))))))
      (if nearest
        (let ((pos (car nearest))
               (sys (cdr nearest)))
          (goto-char pos)
          (push-mark)
          (recenter)
          (message "Jumped to %s diagnostic at char %d (line %d)"
            sys pos (line-number-at-pos pos)))
        (user-error "No next diagnostic found from Flymake, Flycheck, or LSP")))))


;; --- End: Unified Flymake setup ---

(setq lsp-use-plists nil) ;; hash-table is faster

;; ── 1. Stop eldoc from spamming LSP ──────────────────────────────────────────
(setq eldoc-idle-delay 1.0          ; wait before querying
  lsp-eldoc-enable-hover nil)   ; or disable for LSP buffers
(remove-hook 'python-mode-hook #'eldoc-mode) ; ← if you don’t need it

;; ── 2. Trim LSP payload & frequency ─────────────────────────────────────────
(setq lsp-idle-delay 0.8                    ; debounce change notifications
  lsp-enable-symbol-highlighting nil    ; big doc responses
  lsp-enable-folding nil
  lsp-signature-auto-activate nil
  lsp-log-io nil                        ; no heavy logging
  lsp-use-plists nil)                   ; hash-table parsing is faster

;; Diagnostics only on save, not on every keystroke
(setq lsp-diagnostic-package :none)
(add-hook 'after-save-hook #'lsp-diagnostics--enable)

;; ── 3. Make save-buffer cheap ───────────────────────────────────────────────
(setq lsp-before-save-edits nil)           ; stop auto-format
(remove-hook 'before-save-hook #'blacken-buffer) ; if you use blacken/py-isort
(setq vc-handled-backends nil)             ; skip Git status on save
(global-semantic-idle-scheduler-mode -1)

;; ── 5. Helm/Smex latency fixes ──────────────────────────────────────────────
(setq helm-input-idle-delay 0.05
  helm-idle-delay        0.7
  smex-save-file         "/tmp/.smex-items") ; small tmp file


(setq lsp-pyright-disable-language-services nil)
(setq lsp-pyright-diagnostic-mode "workspace")   ; <- incremental

(add-hook 'python-mode-hook (lambda () (font-lock-mode 1)))


;;                                Keybindings                                ;
;; ------------------------------------------------------------------------- ;


(define-key python-mode-map (kbd "C-c C-s") 'send-py-line-p)

(define-key python-mode-map (kbd "C-c >") 'sdev/goto-next-diagnostic)

(define-key python-mode-map (kbd "C-c C-a") 'send-py-line)
(define-key python-mode-map (kbd "C-c C-0") 'eval-last-sexp)

(define-key python-mode-map (kbd "C-c C-r") 'send-py-region)
(define-key python-mode-map (kbd "C-c C-_") 'toggle-camelcase-underscores)
;; (define-key python-mode-map (kbd "C--") 'send-py-line)

(define-key python-mode-map (kbd "C--") 'send-current-line-to-second-line)


(define-key compilation-mode-map (kbd "RET") 'custom-compile-go-to-error)
(define-key compilation-mode-map (kbd "g") 'custom-compile-go-to-error)


(define-key python-mode-map (kbd "C-c C-c") 'tiqsi-uv-compile)

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c p") 'py-copy-defun-to-clipboard))

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c d") 'py-kill-defun))


;; (defun send-js-line ()
;;   (interactive)
;;   (let ((py-temp (thing-at-point 'line t)) )
;;     (comint-send-string "*eshell*" py-temp)))

;; (define-key js-mode-map (kbd "C-c C-a") 'send-js-line)

;; (define-key python-mode-map (kbd "C-c C-s") 'send-py-line-p)
;; (define-key python-mode-map (kbd "C-c C-r") 'send-py-region)

;; ------------------------------------------------------------------------- ;
;;                        Python-specific functions                          ;
;; ------------------------------------------------------------------------- ;

(defun insert-df-checker (start end)
  "Insert a `df_checker` call with the selected region as the argument."
  (interactive "r")
  (let ((region-text (buffer-substring-no-properties start end)))
    (goto-char end)
    (insert "\n\n")
    (insert "#                                  DF CHECK                                 #\n")
    (insert "# ------------------------------------------------------------------------- #\n")
    (insert (format "df_checker = browse_df(\n    %s,\n    True,\n    4,\n)\n" region-text))
    (insert "pprint_df(next(df_checker))\n")
    (insert "# ------------------------------------------------------------------------- #\n")
    (insert "\n")))

(defun python-wrap-try-except (beg end)
  "Wrap the region in a Python try-except block, preserving indentation."
  (interactive "r")
  (let* ((region (buffer-substring-no-properties beg end))
          (indented-region (with-temp-buffer
                             (insert region)
                             (python-indent-region (point-min) (point-max))
                             (buffer-string)))
          (indentation (save-excursion
                         (goto-char beg)
                         (back-to-indentation)
                         (buffer-substring-no-properties (line-beginning-position) (point)))))
    (delete-region beg end)
    (insert (format "%stry:\n%s%s\n%sexcept Exception as e:\n%s"
              indentation
              indentation
              (replace-regexp-in-string "^" (concat indentation "    ") indented-region)
              indentation
              (concat indentation "    "))))
  (forward-line)
  (back-to-indentation))

(defun py-copy-defun-to-clipboard ()
  "Copy the current Python function or method definition to the kill ring.

Identifies the boundaries of the Python function (`def` or `async def`)
or method containing the point, regardless of the point's exact
location within it, and copies the entire block to the kill ring
(clipboard).

The cursor position remains unchanged after execution."
  (interactive)

  ;; --- 1. Check if in a Python buffer ---
  (unless (derived-mode-p 'python-base-mode 'python-mode 'python-ts-mode)
    (user-error "This command requires a Python buffer (python-mode or similar)."))

  ;; --- 2. Find Boundaries and Copy ---
  (save-excursion
    ;; condition-case: Bind 'err' IF an error occurs in the body (progn)
    (condition-case err
      ;; Body: Code to try executing
      (progn
        (beginning-of-defun)
        (let ((start (point)))
          (end-of-defun)
          (let ((end (point)))
            (kill-ring-save start end)
            (message "Python definition copied to kill ring (%d chars)" (- end start))
            ))) ; End progn

      ;; Handler: Executed ONLY if an 'error' happens in the body (progn)
      (error
	;; 'err' is guaranteed to be bound HERE by condition-case
	(user-error "Could not find Python function/method boundaries at point. %s" err)))
    ) ; End save-excursion
  )

(defun py-kill-defun ()
  "Kill (cut) the current Python function or method definition.
Handles errors finding boundaries more quietly."
  (interactive)

  ;; --- 1. Check if in a Python buffer ---
  (unless (derived-mode-p 'python-base-mode 'python-mode 'python-ts-mode)
    (user-error "This command requires a Python buffer (python-mode or similar)."))

  ;; --- 2. Find Boundaries and Kill (Cut) ---
  ;; condition-case structure: (condition-case VAR BODY HANDLER)
  (condition-case err ; VAR: 'err' - will hold error data IF handler is run

    ;; --- BODY: Code to attempt ---
    (progn
      (beginning-of-defun)
      (let ((start (point)))
        (end-of-defun)
        (let ((end (point)))
          (kill-region start end) ; Cut the region
          ;; Success message
          (message "Python definition killed (cut) to kill ring (%d chars)" (- end start))
          ))) ; End of BODY (progn block)

    ;; --- HANDLER: Code to run ONLY if an 'error' occurs in BODY ---
    (error
      ;; Display a simpler error message without the detailed 'err' data.
      (user-error "Could not find Python function/method boundaries at point."))

    ) ; End of condition-case
  ) ; End of defun

;; ------------------------------------------------------------------------- ;
;;                    LSP and Development Environment                        ;
;; ------------------------------------------------------------------------- ;

(defun activate-lsp-bridge-with-uv ()
  "Set up lsp-bridge with uv virtual environment for Python files."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (let ((default-directory (file-name-directory buffer-file-name)))
      (setq-local lsp-bridge-python-command
        (string-trim (shell-command-to-string "cd $PWD && uv_source && which python")))
      (setq-local lsp-bridge-python-default-server 'pyright)
      (lsp-bridge-mode 1))))

(defun install-pyright-in-uv ()
  (interactive)
  (shell-command "uv_source && pip install pyright"))

(defun insert-colored-print (text)
  "Insert a colored print statement with customizable message."
  (interactive "sEnter message: ")
  (insert (format "print(\"\\033[32m*%s\\033[0m\")" text)))

;; ------------------------------------------------------------------------- ;
;;                            Python Keybindings                             ;
;; ------------------------------------------------------------------------- ;

;; LSP keybindings for Python mode
(define-key python-mode-map (kbd "C-.") 'conditional-xref-lsp-find-definition)       ;; Direct jump to definition
(define-key python-mode-map (kbd "C->") 'conditional-xref-lsp-find-definition-side-buffer) ;; Jump to definition in side buffer
(define-key python-mode-map (kbd "C-`") 'lsp-ui-peek-find-definitions)
(define-key python-mode-map (kbd "C-,") 'xref-go-back)      ;; Jump back
(define-key python-mode-map (kbd "C-~") 'lsp-ui-peek-find-references)        ;; Find references

;; Hook for LSP
(add-hook 'python-mode-hook 'lsp)

(provide 'programming-python-lite)

;;; programming-python-lite.el ends here
