;;; programming-python-lite.el --- Tiqsi python programming support

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
  (setq python-shell-interpreter  "/tiqsi-emacs/modules/programming/remote-python.sh"
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


;; (straight-require 'popwin)

;; (setq popwin:popup-window-height 35)
;; (setq popwin:popup-window-width 15)


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

;; ------------------------------------------------------------------------- ;
;;                                Keybindings                                ;
;; ------------------------------------------------------------------------- ;



(define-key python-mode-map (kbd "C-c C-s") 'send-py-line-p)

(define-key python-mode-map (kbd "C-c C-a") 'send-py-line)
(define-key python-mode-map (kbd "C-c C-0") 'eval-last-sexp)

(define-key python-mode-map (kbd "C-c C-r") 'send-py-region)
(define-key python-mode-map (kbd "C-c C-_") 'toggle-camelcase-underscores)


(define-key compilation-mode-map (kbd "RET") 'custom-compile-go-to-error)
(define-key compilation-mode-map (kbd "g") 'custom-compile-go-to-error)


(define-key python-mode-map (kbd "C-c C-c") 'tiqsi-uv-compile)


;; (defun send-js-line ()
;;   (interactive)
;;   (let ((py-temp (thing-at-point 'line t)) )
;;     (comint-send-string "*eshell*" py-temp)))

;; (define-key js-mode-map (kbd "C-c C-a") 'send-js-line)

;; (define-key python-mode-map (kbd "C-c C-s") 'send-py-line-p)
;; (define-key python-mode-map (kbd "C-c C-r") 'send-py-region)

(provide 'programming-python-lite)

;;; programming-python-lite.el ends here
