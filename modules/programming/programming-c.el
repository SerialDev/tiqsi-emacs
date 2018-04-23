;;; programming-c.el --- Tiqsi C & CPP programming support based on clang

;;; Commentary:
;; 
;; pip install compdb
;; apt-get install libclang-dev
;;ninja -t compdb `ninja -t rules | grep 'CXX_COMPILER_'`


;----{Completion}---;

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))


;----{Formatting}---;

;;  (cond ((file-exists-p buffer-file-name) t)
;;         ((string-match "[.]hin" buffer-file-name) (tiqsi-source-format))
;;         ((string-match "[.]cin" buffer-file-name) (tiqsi-source-format))
;;         ((string-match "[.]h" buffer-file-name) (tiqsi-header-format))
;;         ((string-match "[.]cpp" buffer-file-name) (tiqsi-source-format)))

;----{Navigation}---;

;;
;; Grep for the symbol under the cursor.  Works only for C / C++ / H / RC
;; files
;;
(defun grep-curdir-symbol-at-point ()
  "Grep current directory for symbol at cursor."
  (interactive)
  (grep (concat "grep -n -e " (current-word)  " *.c *.cpp *.h *.rc NUL"
)))


(defun tiqsi-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	(setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))

(defun tiqsi-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (tiqsi-find-corresponding-file)
  (other-window -1))


(when tiqsi-linux
  (setq rtags-path "/rtags/bin")
  )

(setq rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))

(setq rtags-autostart-diagnostics t)
(setq rtags-enable-standard-keybindings t)

(setq rtags-use-helm t)

(defvar rtags:goto-stack '())

(defun rtags:jump-to-definition ()
  (interactive)
  (add-to-list 'rtags:goto-stack
               (list (buffer-name) (point)))
  (rtags-find-symbol-at-point))


(defun rtags:jump-back ()
  (interactive)
  (let ((p (pop rtags:goto-stack)))
    (if p (progn
            (switch-to-buffer (nth 0 p))
            (goto-char (nth 1 p))))))


;; TODO  rtags-referece-tree fix
;; TODO  rtags-diagnostigs fix

;--{rtags flycheck}-;

;; ensure that we use only rtags checking
;; https://github.com/Andersbakken/rtags#optional-1
(defun setup-flycheck-rtags ()
  (interactive)
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-highlighting-mode nil)
  (setq-local flycheck-check-syntax-automatically nil))

(when
    (require 'rtags nil :noerror)
   ;; company completion setup
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
  ;; use rtags flycheck mode -- clang warnings shown inline
  (try-require 'flycheck-rtags)
  ;; c-mode-common-hook is also called by c++-mode
  (add-hook 'c-mode-hook #'setup-flycheck-rtags)
  (add-hook 'c++-mode-common-hook #'setup-flycheck-rtags)
)

;---{Indentation}---;

(c-add-style "microsoft"
              '("stroustrup"
                (c-offsets-alist
                 (innamespace . -)
                 (inline-open . 0)
                 (inher-cont . c-lineup-multi-inher)
                 (arglist-cont-nonempty . +)
                 (template-args-cont . +))))
(setq c-default-style "microsoft")

; C++ indentation style
(defconst tiqsi-big-fun-c-style
  '(;(c-electric-pound-behavior   . t)
    (c-tab-always-indent         . t)
    (c-comment-only-line-offset  . 0)
    (c-hanging-braces-alist      . ((class-open)
                                    (class-close)
                                    (defun-open)
                                    (defun-close)
                                    (inline-open)
                                    (inline-close)
                                    (brace-list-open)
                                    (brace-list-close)
                                    (brace-list-intro)
                                    (brace-list-entry)
                                    (block-open)
                                    (block-close)
                                    (substatement-open)
                                    (statement-case-open)
                                    (class-open)))

    (c-hanging-colons-alist      . ((inher-intro)
                                    (case-label)
                                    (label)
                                    (access-label)
                                    (access-key)
                                    (member-init-intro)))

    (c-cleanup-list              . (scope-operator
                                    list-close-comma
                                    defun-close-semi))

    (c-offsets-alist             . ((arglist-close         .  c-lineup-arglist)
                                    (label                 . -4)
                                    (access-label          . -4)
                                    (substatement-open     .  0)
                                    (statement-case-intro  .  4)
                                    (statement-block-intro .  4)
                                    (case-label            .  4)
                                    (block-open            .  0)
                                    (inline-open           .  0)
                                    (topmost-intro-cont    .  0)
                                    (knr-argdecl-intro     . -4)
                                    (brace-list-open       .  0)
                                    (brace-list-intro      .  4)))
    (c-echo-syntactic-information-p . t))
    "Casey's Big Fun C++ Style")

;; (add-hook 'c-mode-common-hook 'tiqsi-big-fun-c-hook)


(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-common-hook ()
 ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
 (c-set-offset 'substatement-open 0)
 ;; other customizations can go here

 (setq
  ;; use gdb-many-windows by default
  gdb-many-windows t
  ;; Non-nil means display source file containing the main routine at startup
  gdb-show-main t
  )

 (cwarn-mode 1)
 (setq c++-tab-always-indent t)
 (setq c-basic-offset 4)                  ;; Default is 2
 (setq c-indent-level 4)                  ;; Default is 2

 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
 (setq tab-width 4)
 (setq indent-tabs-mode t)  ; use spaces only if nil
 )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)


;----{Insertions}---;

(defun tiqsi-header-format ()
  "Format the given file as a header file."
  (interactive)
  (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  (insert "#if !defined(")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H)\n")
  (insert "/* ========================================================================\n")
  (insert "   $File: $\n")
  (insert "   $Date: $\n")
  (insert "   $Revision: $\n")
  (insert "   $Creator: Andres Mariscal $\n")
  (insert "   $Notice: (C) Copyright 2015 Andres Mariscal. All Rights Reserved. $\n")
  (insert "   ======================================================================== */\n")
  (insert "\n")
  (insert "#define ")
  (push-mark)
  (insert BaseFileName)
  (upcase-region (mark) (point))
  (pop-mark)
  (insert "_H\n")
  (insert "#endif")
  )


  (defun tiqsi-source-format ()
     "Format the given file as a source file."
     (interactive)
     (setq BaseFileName (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
     (insert "/* ========================================================================\n")
     (insert "   $File: $\n")
     (insert "   $Date: $\n")
     (insert "   $Revision: $\n")
     (insert "   $Creator: Andres Mariscal $\n")
     (insert "   $Notice: (C) Copyright 2015 Andres Mariscal. All Rights Reserved. $\n")
     (insert "   ======================================================================== */\n")
  )


;;  for cpp create class-close

(defun cpp-createclass ()
  (interactive)
  (setq classname (file-name-sans-extension (file-name-nondirectory   buffer-file-name)))
  (insert 
"/**
  * " classname".h 
  *
  * Author: SerialDev
  * Modified: " (format-time-string "%Y-%m-%d") "
  * Licence: GNU GPL
  */
#ifndef "(upcase classname)"
#define "(upcase classname)"

class " classname "
{
  public:
    "classname"();
    ~"classname"();

  private:

};
#endif
"))


;; Interesting C++ Stuff found here: http://dotemacs.de/dotfiles/DavidJolley.emacs.html

;; If point is in a class definition, return the name of the
;; class. Otherwise, return nil. Thanks to Elijah Daniel for this one.

(defun ewd-classname ()
  "If the point is in a class definition, gets the name of the class.
Return nil otherwise." 
  (interactive)
 (save-excursion
    (let ((brace (assoc 'inclass (c-guess-basic-syntax))))
      (if (null brace) '()        (goto-char (cdr brace))
	(let ((class-open (assoc 'class-open (c-guess-basic-syntax))))
	  (if class-open (goto-char (cdr class-open)))
	  (if (looking-at "^class[ \t]+\\([A-Za-z_][^ \t:{]*\\)")
	      (buffer-substring (match-beginning 1) (match-end 1))
	    (error "Error parsing class definition!")))))))

;; Insert function prototype in current header file and matching
;; function body in implementation file.
(defun ewd-insert-new-method (rettype proto)
  "Insert a function declaration into the current class header file at
point, along with matching function definition in the corresponding
implementation file, complete with class name and scope resolution
operator.  This function expects the implementation file to be named
foo.cpp and in the same directory as the current header file, foo.h."
  (interactive "sReturn type:\nsPrototype: ")
  (let ((classname (ewd-classname))
	(c-tab-always-indent t))
    (if (null classname) (message "Not in class definition!")
      (unless (string-equal rettype "") (setq rettype (concat rettype " ")))
      (insert rettype proto ";")
      (c-indent-command)
      (save-window-excursion
	(find-file (concat (file-name-sans-extension (buffer-file-name))
			   ".cpp"))
	(end-of-buffer)
	(insert "\n\n")
	(insert-function-comment)
	(end-of-buffer)
	(insert rettype classname "::" proto "\n{\n}\n")))))


;---{Compilation}---;

; Setup my compilation mode
(defun tiqsi-big-fun-compilation-hook ()
  (make-local-variable 'truncate-lines)
  (setq truncate-lines nil))

(add-hook 'compilation-mode-hook 'tiqsi-big-fun-compilation-hook)


(defun find-project-directory-recursive ()
  "Recursively search for a makefile."
  (interactive)
  (if (file-exists-p tiqsi-makescript) t
      (cd "../")
      (find-project-directory-recursive)))


(defun find-project-directory ()
  "Find the project directory."
  (interactive)
  (setq find-project-from-directory (current-buffer-path))
  (switch-to-buffer-other-window "*compilation*")
  (if compilation-directory-locked (cd last-compilation-directory)
  (cd find-project-from-directory)
  (find-project-directory-recursive)
  (setq last-compilation-directory default-directory)))


(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun current-buffer-path()
  (file-name-directory (buffer-file-name)))


(defun lock-compilation-directory ()
  "The compilation process should NOT hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked t)
  (message "Compilation directory is locked."))

(defun unlock-compilation-directory ()
  "The compilation process SHOULD hunt for a makefile"
  (interactive)
  (setq compilation-directory-locked nil)
  (message "Compilation directory is roaming."))

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (if (find-project-directory) (compile tiqsi-makescript))
  (other-window 1))

;; Define + active modification to compile that locally sets
;; shell-command-switch to "-ic".
(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))

;----{Completion}---;

;                                Get all possible dabbrev expansions                                ;
;; from https://curiousprogrammer.wordpress.com/2009/04/07/autocomplete-and-dabbrev/

(defun ac-source-dabbrev (abbrev)
  (interactive)
  (dabbrev--reset-global-variables)
  (let ((dabbrev-check-all-buffers t))
    (sort (dabbrev--find-all-expansions abbrev t) #'string<)))

(defvar ac-source-dabbrev-words
  '((candidates
     . (lambda () (all-completions ac-target
                                   (ac-source-dabbrev ac-target)))))
  "Get all the completions using dabbrev")

(setq-default ac-sources '(ac-source-dabbrev-words))

(defun ac-self-insert ()
  (interactive)
  (self-insert-command 1)
  (ac-start))

(defun ac-fix-keymap ()
  (let ((i 32))
    (while (<= i ?z)
      (define-key ac-complete-mode-map
        (make-string 1 i) 'ac-self-insert)
      (incf i))))

(ac-fix-keymap)

(define-key ac-complete-mode-map (kbd "DEL")
  (lambda ()
    (interactive)
    (backward-delete-char-untabify 1)
    (ac-start)))

;; (setq ac-auto-start nil)
(setq tab-always-indent 'complete)

;---{Keybindings}---;


;; (global-set-key (kbd "<f6>") 'ac-start)

;; (define-key ac-complete-mode-map [t] 'ac-self-insert)
(define-key ac-complete-mode-map (kbd "M-x") 'execute-extended-command)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-g") 'ac-stop)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key global-map "\em" 'make-without-asking)
(global-set-key [C-f1] 'show-file-name) ; Or any other key you want
(define-key c++-mode-map (kbd "C-?") 'irony-get-type)
(global-set-key (kbd "C-?" ) nil)
(global-set-key "\C-ci" 'ewd-insert-new-method)
(define-key c++-mode-map [f12] 'tiqsi-find-corresponding-file)
(define-key c++-mode-map [M-f12] 'tiqsi-find-corresponding-file-other-window)
; Alternate bindings for F-keyless setups (ie MacOS X terminal)
(define-key c++-mode-map "\ec" 'tiqsi-find-corresponding-file)
(define-key c++-mode-map "\eC" 'tiqsi-find-corresponding-file-other-window)
(define-key c++-mode-map "\es" 'tiqsi-save-buffer)
;; (define-key c++-mode-map [S-tab] 'indent-for-tab-command)
(define-key c++-mode-map "\t" 'indent-for-tab-command)
(define-key c++-mode-map (kbd "<backtab>")'un-indent-by-removing-4-spaces)
;; (define-key c++-mode-map (kbd "<S-tab>") 'indent-for-tab-command)
(define-key c++-mode-map "\C-y" 'indent-for-tab-command)
(define-key c++-mode-map [C-tab] 'indent-region)
(define-key c++-mode-map "	" 'indent-region)
(define-key c++-mode-map "\ej" 'imenu)
(define-key c++-mode-map "\e." 'c-fill-paragraph)
(define-key c++-mode-map "\e/" 'c-mark-function)
(define-key c++-mode-map "\e " 'set-mark-command)
(define-key c++-mode-map "\eq" 'append-as-kill)
(define-key c++-mode-map "\ea" 'yank)
(define-key c++-mode-map "\ez" 'kill-region)


(define-key c++-mode-map (kbd "C-.") 'rtags:jump-to-definition)
(define-key c++-mode-map (kbd "C-,") 'rtags:jump-back)
(define-key c++-mode-map (kbd "C-r f") 'rtags-fixit)
(define-key c++-mode-map (kbd "C-n") 'rtags-next-diag)


(provide 'programming-c)

;;; programming-c.el ends here
