;;; programming-python-lite.el --- Tiqsi python programming support  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'comint)
(require 'compile)
(require 'python)

(setq python-shell-prompt-detect-failure-warning nil)


(defun tiqsi-python-process ()
  (or (python-shell-get-process)
    (progn (run-python nil nil t)
      (python-shell-get-process))))


(defun send-py-line ()
  (interactive)
  (python-shell-send-string (string-trim (thing-at-point 'line t)) (tiqsi-python-process)))


(defun send-py-line-p ()
  (interactive)
  (let ((line (string-trim (thing-at-point 'line t))))
    (python-shell-send-string (format "%s; print(%s)" line line) (tiqsi-python-process))))


(defun send-py-region (begin end)
  (interactive "r")
  (python-shell-send-string (buffer-substring-no-properties begin end) (tiqsi-python-process)))


(defun extract-python-functions-to-clipboard (start end)
  (interactive "r")
  (let* ((module (file-name-base (buffer-file-name)))
          (code (buffer-substring-no-properties start end))
          (names (with-temp-buffer
                   (insert code)
                   (goto-char (point-min))
                   (let (acc)
                     (while (re-search-forward "^def \\([a-zA-Z0-9_]+\\)\\s-*(" nil t)
                       (push (match-string 1) acc))
                     (nreverse acc))))
          (import-string (format "from %s import (%s)" module (string-join names ", "))))
    (kill-new import-string)
    (message "Copied to clipboard: %s" import-string)))


(defvar tiqsi-compile--command nil)

(defun tiqsi-uv-compile (compile-string)
  (interactive (list (read-string "String to compile: " "uv run ")))
  (let ((dir (or (locate-dominating-file default-directory "pyproject.toml") default-directory)))
    (setq tiqsi-compile--command compile-string)
    (with-selected-window (next-window (selected-window) nil t)
      (let ((default-directory dir)
             (compilation-buffer-name-function (lambda (_mode) "*tiqsi-uv-compile*"))
             (display-buffer-alist
               `((,(regexp-quote "*tiqsi-uv-compile*")
                   . ((display-buffer-reuse-window display-buffer-same-window))))))
        (compile (concat "cd " (shell-quote-argument dir) " && " compile-string))))))


(defun custom-compile-go-to-error ()
  (interactive)
  (let ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (orig-window (selected-window)))
    (when (string-match "^\\([^:]+\\):\\([0-9]+\\):.*$" text)
      (let ((file (expand-file-name (match-string 1 text)))
             (line-num (string-to-number (match-string 2 text))))
        (when (file-exists-p file)
          (find-file-other-window file)
          (goto-char (point-min))
          (forward-line (1- line-num))
          (recenter))))
    (select-window orig-window)))


(defun python-find-functions-without-docstrings-ag (directory)
  (interactive "DDirectory: ")
  (unless (executable-find "ag")
    (error "ag not found in exec-path"))
  (let ((output-buffer (get-buffer-create "*Python Functions Without Docstrings*"))
         (pattern "def\\s+[a-zA-Z_][a-zA-Z0-9_]*\\s*\\([^)]*\\):\\s*\\n\\s*(?![\\s\\t]*(?:'''|\"\"\"))"))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Searching for Python functions without docstrings in " directory "...\n"))
      (setq buffer-read-only nil))
    (set-process-sentinel
      (start-file-process "ag-search" output-buffer "ag" "--vimgrep" "--python"
        pattern (expand-file-name directory))
      (lambda (p _event)
        (when (eq (process-status p) 'exit)
          (with-current-buffer (process-buffer p)
            (let ((inhibit-read-only t)
                   (status (process-exit-status p)))
              (goto-char (point-max))
              (insert (cond ((= status 0) "\nSearch completed. Issues found.\n")
                        ((= status 1) "\nSearch completed. No functions without docstrings found.\n")
                        (t (format "\nSearch failed with error code %d.\n" status))))
              (compilation-mode)
              (setq buffer-read-only t))
            (display-buffer (current-buffer))))))))


;; ------------------------------------------------------------------------- ;


(defun sdev--set-python-interpreter (interpreter args)
  (setq python-shell-interpreter interpreter
    python-shell-interpreter-args args))


(defun sdev--ensure-uv ()
  (unless (executable-find "uv")
    (error "uv not found in exec-path")))


(defun sdev-use-uv ()
  (interactive)
  (sdev--ensure-uv)
  (sdev--set-python-interpreter "uv" "run python -i"))


(defun sdev-use-uv-ipython ()
  (interactive)
  (sdev--ensure-uv)
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (sdev--set-python-interpreter "uv" "run --with ipython ipython --simple-prompt -i"))


(defun sdev-custom-venv (&optional script)
  (interactive (list (when current-prefix-arg
                       (read-file-name "Path to Python shell script: " default-directory "remote-python.sh"))))
  (let ((script (or script (concat default-directory "remote-python.sh"))))
    (unless (file-executable-p script)
      (error "Script %S not found or not executable" script))
    (sdev--set-python-interpreter script "-i")))


(defun sdev-use-remote ()
  (interactive)
  (sdev--set-python-interpreter "/tiqsi-emacs/modules/programming/remote-python.sh" "-i"))


(defun sdev-use-hetzner ()
  (interactive)
  (sdev--set-python-interpreter "ssh" "-t root@135.181.198.90 /opt/conda/bin/python -i"))


(when (executable-find "uv")
  (sdev-use-uv-ipython))


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



(defun pandas-print-all ()
  "Insert code to configure Pandas to display all rows and columns."
  (interactive)
  (insert "import pandas as pd\n\n# Display all rows and columns\npd.set_option('display.max_rows', None)\npd.set_option('display.max_columns', None)\n"))

(defun pandas-print-shortened ()
  "Insert code to reset Pandas display options to default shortened mode."
  (interactive)
  (insert "import pandas as pd\n\n# Reset display options to defaults\npd.reset_option('display.max_rows')\npd.reset_option('display.max_columns')\n"))


(defvar tiqsi-python-language-server
  '("uvx" "--from" "basedpyright" "basedpyright-langserver" "--stdio"))


(defun tiqsi-python-completion-setup ()
  (setq-local company-idle-delay 0.15
    company-minimum-prefix-length 2
    company-backends '((company-capf :with company-dabbrev-code) company-files))
  (company-mode 1)
  (when (and buffer-file-name (executable-find "uvx"))
    (eglot-ensure)))


(add-hook 'python-mode-hook #'tiqsi-python-completion-setup)
(add-hook 'inferior-python-mode-hook #'company-mode)


(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs (cons '(python-mode python-ts-mode) tiqsi-python-language-server))
  (setq eglot-sync-connect nil
    eglot-connect-timeout 120
    eglot-autoshutdown t
    eglot-send-changes-idle-time 0.3
    eglot-events-buffer-config '(:size 0 :format short)
    eglot-ignored-server-capabilities '(:inlayHintProvider :documentHighlightProvider :codeLensProvider
                                         :documentOnTypeFormattingProvider :colorProvider :foldingRangeProvider))
  (add-to-list 'completion-category-overrides '(eglot-capf (styles basic flex))))


;; ------------------------------------------------------------------------- ;
;;                                Keybindings                                ;
;; ------------------------------------------------------------------------- ;


(straight-require 'ruff-format)

(add-hook 'python-mode-hook 'ruff-format-on-save-mode)


(define-key python-mode-map (kbd "C-c C-s") 'send-py-line-p)
(define-key python-mode-map (kbd "C-c C-a") 'send-py-line)
(define-key python-mode-map (kbd "C-c C-0") 'eval-last-sexp)
(define-key python-mode-map (kbd "C-c C-r") 'send-py-region)
(define-key python-mode-map (kbd "C-c C-c") 'tiqsi-uv-compile)
(define-key python-mode-map (kbd "C-c >") 'sdev/next-issue)
(define-key python-mode-map (kbd "C-c <") 'sdev/previous-issue)

(define-key compilation-mode-map (kbd "RET") 'custom-compile-go-to-error)
(define-key compilation-mode-map (kbd "g") 'custom-compile-go-to-error)


(provide 'programming-python-lite)

;;; programming-python-lite.el ends here
