;;; programming-c.el --- Tiqsi C & CPP programming support based on clang  -*- lexical-binding: t -*-

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
;;
;; pip install compdb
;; apt-get install libclang-dev
;; ninja -t compdb `ninja -t rules | grep 'CXX_COMPILER_'`


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
  (setq rtags-path "/rtags/bin/")
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

(setq tiqsi-makescript "./build.sh")



(if tiqsi-win32
    (defun compile (data)

      (if (get-buffer "*shell*")
	  (progn
	    (kill-buffer "*shell*")
	    (send-to-shell data)
	    (send-to-shell "exit")
	    (sdev/jump-window))
	(progn
	  (send-to-shell data)
	  (send-to-shell "exit")
	  (sdev/jump-window))
	)
      )
  (message "compile defined"))


(defun compile-c-lang()
  (interactive)
  (if (or (file-exists-p "meson.build") (file-exists-p "../meson.build"))
      (compile-meson)
    (if (or (file-exists-p "CMakeLists.txt") (file-exists-p "../CMakeLists.txt"))
	(compile-cmake)
      (make-without-asking))))

(defun compile-meson()
  (if (file-directory-p "build")
      (progn ( message "compiling")
	     (if (file-exists-p "meson.build") 
		 (compile "cd build && ninja -t compdb cxx cc > compile_commands.json && ninja")
	       (compile "cd .. && build && ninja -t compdb cxx cc > compile_commands.json && ninja")))
    (progn
      (message "Generating meson & compiling")
      (if (file-exists-p "meson.build") 
	  (compile "meson build && cd build && ninja -t compdb cxx cc > compile_commands.json && ninja")
	(compile "cd .. && meson build && cd build && ninja -t compdb cxx cc > compile_commands.json && ninja"))

      (add-to-rtags))))

(defun compile-cmake()
  (interactive)
  (if (file-directory-p "build")
      (progn
	(message "compiling"
		 (compile "cd build && cmake .. && make"))
	(add-to-rtags))
    (progn (message "Creating Build directory before building"
		    (compile "mkdir build && cd build && cmake .. && make")))))


;; Tiqsi find file
(defun tiqsi-parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun tiqsi-find-file-in-hierarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file (concat current-dir fname))
        (parent (tiqsi-parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (tiqsi-find-file-in-hierarchy parent fname)))))


(defun tiqsi-get-string-from-file (filePath)
  "Return filePath's file content.
;; thanks to “Pascal J Bourguignon” and “TheFlyingDutchman 〔zzbba…@aol.com〕”. 2010-09-02
"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun tiqsi-search-file-get-string(filename)
  (tiqsi-get-string-from-file
		       (tiqsi-find-file-in-hierarchy
			(file-name-directory buffer-file-name) filename ))
  )

(defun tiqsi--join (sep string-list)
  (mapconcat 'identity (split-string string-list) sep)
  )

(defun tiqsi--remove-newlines(source)
  (tiqsi--join " " source))

(defmacro tiqsi--parsec-with-index (data &rest parsers )
  `(let ((parsec-result  (parsec-with-input ,data
			,@parsers))
	 (idx (parsec-with-input ,data
		(parsec-query
		 ,@parsers
		 :end ))) )
         (cons parsec-result idx)))


(defun tiqsi--parsec-alphanumeric ()
  (parsec-or
   (parsec-or
    (parsec-eol-or-eof)
    (parsec-letter))
   (parsec-digit)))

(defun tiqsi--parsec-ascii-special-chars-no-brackets ()
  (parsec-re "['\\!%#\"$ &\*\+\-/,\.:;\|^_`~=\?]")
  )

(defun tiqsi--parsec-ascii-special-chars-no-brackets-pound ()
  (parsec-re "['\\!%\"$ &\*\+\-/,\.:;\|^_`~=\?]")
  )

(defun tiqsi--parsec-all-ascii-no-brackets ()
  (parsec-or
   (tiqsi--parsec-ascii-special-chars-no-brackets-pound)
   (tiqsi--parsec-alphanumeric)))




(defun tiqsi--parsec-whitespace ()
     (parsec-re"[ \t\r\n\v\f]"))


(defun tiqsi--parsec-brackets()
  (parsec-re "[]{[}()]")
  )

(defun tiqsi--parsec-all-ascii()
  (parsec-or
   (tiqsi--parsec-brackets)
   (tiqsi--parsec-all-ascii-no-brackets)
   )
)


(defun tiqsi--parsec-all-ascii-no-brackets-except (&optional bracket)
  (parsec-or
  (parsec-re (format "['\\!%s#\"$ &\*\+\-/,\.:;\|^_` ~=\?]" bracket))
    (tiqsi--parsec-alphanumeric)))


(defun tiqsi--parsec-between-round-brackets(&optional bracket)
  (parsec-between
   (parsec-ch ?\()
   (parsec-ch ?\))
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets-except bracket)
    )))

(defun tiqsi--parsec-between-square-brackets()
  (parsec-between
   (parsec-ch ?\[)
   (parsec-ch ?\])
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets)
    )))

(defun tiqsi--parsec-between-curly-brackets()
  (parsec-between
   (parsec-ch ?\{)
   (parsec-ch ?\})
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets)
    )))

(defun tiqsi--parsec-between-angle-brackets()
  (parsec-between
   (parsec-ch ?\<)
   (parsec-ch ?\>)
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets)
    )))

(defun tiqsi--parsec-between-single-quotes()
  (parsec-between
   (parsec-ch ?\')
   (parsec-ch ?\')
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets)
    )))

(defun tiqsi--parsec-between-double-quotes()
  (parsec-between
   (parsec-ch ?\")
   (parsec-ch ?\")
   (parsec-many-as-string
    (tiqsi--parsec-all-ascii-no-brackets)
    )))

(defun tiqsi--parsec-retrieve-remaining-by-idx (data idx)
  (condition-case nil
      (substring data idx nil)
    (error ""))
  )


(defmacro tiqsi--parsec-with-remainder (data &rest parsers )
  `(let ((parsec-result  (parsec-with-input ,data
			,@parsers))
	 (idx (parsec-with-input ,data
		(parsec-query
		 ,@parsers
		 :end ))) )
     (cons parsec-result
	   (tiqsi--parsec-retrieve-remaining-by-idx ,data
						    (if  (equal (car? idx) 'parsec-error)
							0
						      (- idx 1))
						    ))))


(defmacro tiqsi--parsec-with-index-remainder (data &rest parsers )
  `(let ((parsec-result  (parsec-with-input ,data
			,@parsers))
	 (idx (parsec-with-input ,data
		(parsec-query
		 ,@parsers
		 :end ))) )
     (cons parsec-result
	   (cons idx
		 (tiqsi--parsec-retrieve-remaining-by-idx ,data
 							  (if  (equal (car? idx) 'parsec-error)
							      0
							    (- idx 1))
							  )))))


;; END tiqsi-find-file

(defun tiqsi--get-cmakelist-content()
  (interactive)
  (tiqsi-search-file-get-string "CMakeLists.txt"))

(defun tiqsi--cmake--parsec-pound-comment ()
  (parsec-or
   (tiqsi--cmake--parsec-pound-comment-block)
   (tiqsi--cmake--parsec-pound-comment-inline)
   )
  )

(defun tiqsi--cmake--parsec-pound-comment-inline ()
  (parsec-collect
  (parsec-ch ?\#)
  (parsec-until-as-string
     (parsec-ch ?\#)
     )
  )
)

(defun tiqsi--cmake--parsec-pound-comment-block ()
  (parsec-collect
  (parsec-str "#[[")
  (parsec-until-as-string
     (parsec-str "]]")
     )
  )
)


(defun tiqsi-parse-cmake-recurse  ( data)
  (if (equal data "")
      (message "Done parsing cmake")
	(let (
	      (result (tiqsi--parsec-with-remainder data
							(parsec-or
							(parsec-collect
							 (parsec-many-s
							   (tiqsi--parsec-all-ascii-no-brackets )
							  )
							 (tiqsi--parsec-between-round-brackets "}{")
							 )
							(tiqsi--cmake--parsec-pound-comment)))))
	  (add-to-list 'tiqsi-parse-cmake-result (car result))
	  (tiqsi-parse-cmake-recurse (cdr result) ))))

(defmacro tiqsi-parse-cmake (data)

  `(let ((tiqsi-parse-cmake-result '() ))
      (tiqsi-parse-cmake-recurse ,data)
      tiqsi-parse-cmake-result
      )
)

(defun tiqsi--cmake-find-and-parse ()
      (tiqsi-parse-cmake (tiqsi--remove-newlines (tiqsi--get-cmakelist-content))))


(defun tiqsi-cmake-get-executable-name ()
  (interactive)
  (car
   (split-string
    (car
     (cdr
      (tiqsi--cmake-find-add-executable
       (tiqsi--cmake-find-and-parse)))) " ")))
       ;; (tiqsi-parse-cmake (tiqsi--remove-newlines (tiqsi--get-cmakelist-content)))    ))) " ")))


(defun tiqsi--cmake-find-add-executable (parsed-list)
  (condition-case nil
      (message (caar parsed-list))
    (error nil))
  (if (or (equal (length (car parsed-list)) 1)
	  (equal (caar parsed-list) "add_executable"))
      (car parsed-list)
    (tiqsi--cmake-find-add-executable (cdr parsed-list)))
  )




; ------------------------------------------------------------------------------------------------- ;
;                                           TODO                                                    ;
; ------------------------------------------------------------------------------------------------- ;
; fix this: meson-executable-name should be inferred from meson.build file                           ;
; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ;

(setq meson-executable-name "demo")

(defun run-meson ()
(interactive)
  (let ((command (if (file-exists-p "meson.build")
		     "cd build "
		   "cd .. && cd build ") ))

    (if tiqsi-win32
    	(async-shell-command (format "%s && %s.exe" command meson-executable-name))
      (async-shell-command (format "%s && ./%s" command meson-executable-name)))))

(defun close-side-come-back ()
  (interactive)
  (sdev/jump-window)
  (kill-current-buffer)
  (sdev/jump-window))


(defun tiqsi-cmake-run-executable()
  (interactive)
  (async-shell-command (format "cd build && ./%s" (tiqsi-cmake-get-executable-name)))
  ;; (sdev/jump-window)
  ;; (print (format "cd build && ./%s" (tiqsi-cmake-get-executable-name)))
  )



(defun run-c-lang()
  (interactive)
  (if (or (file-exists-p "meson.build") (file-exists-p "../meson.build"))
      (run-meson)
  (if (or (file-exists-p "CMakeLists.txt") (file-exists-p "../CMakeLists.txt"))
	(tiqsi-cmake-run-executable)
      (run-without-asking))))


(defun start-rtags()
  (interactive)
  (call-process-shell-command (format "%srdm" rtags-path) nil 0)
  )

(eval-after-load 'cc-mode (start-rtags))
;; (insert (format "%srdm" rtags-path) )/home/serialdev/rtags/bin/rdm/home/serialdev/rtags/bin/rdm

(defun add-to-rtags()
  (interactive)
  (call-process-shell-command
   (format "%src -J %sbuild/%s" rtags-path (file-name-directory buffer-file-name) "compile_commands.json")
			      nil 0)
  (message
   (format "%src -J  %sbuild/%s" rtags-path (file-name-directory buffer-file-name) "compile_commands.json")))


; ------------------------------------------------------------------------------------------------- ;
;                                            Create Meson                                           ;
; ------------------------------------------------------------------------------------------------- ;
; Create Meson                                                                                      ;
; project-name -> src -> main.cpp/c                                                                 ;
;                     -> main.cpp/c                                                                 ;
;              -> meson.build                                                                       ;
;              -> .gitignore                                                                        ;
;              -> README.md                                                                         ;
; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ;

(defun tiqsi-create-meson (project-name)
  (interactive "sEnter Project Name:")
  (f-mkdir project-name)
  (f-mkdir (format "%s/src" project-name))
  (write-region "int main(int argc, char *argv[])
{

    return 0;
}"  ""  (format "%s/%s/src/main.c" (f-dirname (f-this-file ))         project-name))

  (write-region (format "
project('%s', 'c',
        default_options : ['c_std=c11'],
	version : '1.0.0',
        license : 'GPL')

src = ['src/main.c']
incdir = include_directories('include')
executable('%s', src, include_directories : incdir)" project-name project-name)  ""  (format "%s/%s/meson.build" (f-dirname (f-this-file )) project-name))

  (write-region " "  ""  (format "%s/%s/README.md" (f-dirname (f-this-file ))      project-name))
  (write-region "# Prerequisites
*.d

# Compiled Object files
*.slo
*.lo
*.o
*.obj

# Precompiled Headers
*.gch
*.pch

# Compiled Dynamic libraries
*.so
*.dylib
*.dll

# Fortran module files
*.mod
*.smod

# Compiled Static libraries
*.lai
*.la
*.a
*.lib

# Executables
*.exe
*.out
*.app
/build"  ""  (format "%s/%s/.gitignore" (f-dirname (f-this-file ))     project-name)))

; ------------------------------------------------------------------------------------------------- ;
;                                            Create CMake                                           ;
; ------------------------------------------------------------------------------------------------- ;
; Create cmake                                                                                      ;
; project-name -> src -> main.cpp/c                                                                 ;
;                     -> main.cpp/c                                                                 ;
;                     -> CMakeLists.txt                                                             ;
;                     -> .gitignore                                                                 ;
;                     -> README.md                                                                  ;
; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ;

(defun tiqsi-create-cmake (project-name)
  (interactive "sEnter Project Name:")
  (f-mkdir project-name)
  (f-mkdir (format "%s/src" project-name))

  (write-region "int main(int argc, char *argv[])
{

    return 0;
}"  ""  (format "%s/%s/src/main.c" (f-dirname (f-this-file ))         project-name))

  (write-region (format "cmake_minimum_required(VERSION 2.8.12)

project(%s)

set (CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -Wall -pedantic -Wextra\")

add_definitions(-std=c99)


add_executable(%s main.c)" project-name project-name)  ""  (format "%s/%s/src/CMakeLists.txt" (f-dirname (f-this-file )) project-name))
  (write-region " "  ""  (format "%s/%s/src/README.md" (f-dirname (f-this-file ))      project-name))
  (write-region "# Prerequisites
*.d

# Compiled Object files
*.slo
*.lo
*.o
*.obj

# Precompiled Headers
*.gch
*.pch

# Compiled Dynamic libraries
*.so
*.dylib
*.dll

# Fortran module files
*.mod
*.smod

# Compiled Static libraries
*.lai
*.la
*.a
*.lib

# Executables
*.exe
*.out
*.app
/build"  ""  (format "%s/%s/src/.gitignore" (f-dirname (f-this-file ))     project-name))
  )

(defun insert-semicolon ()
  (interactive)
  (move-end-of-line 1)
  (insert ";")
  (newline-and-indent)
  )

(defun check-for-semicolon (string)
  (if (equal (car(cdr(split-string string ";"  ))) "")
      (car(split-string string ";" ))
    nil)
  )

; ------------------------------------------------------------------------------------------------- ;
;                                                TODO                                               ;
; ------------------------------------------------------------------------------------------------- ;
;                                                                                                   ;
; parse (select (this ( now) ) ) -> ("select" "(this ( now) )")                                     ;
;                                      -> ("select" ("this "(now)"))                                ;
;                                      -> ("select" ("this ("now") ))                               ;
;                                                                                                   ;
; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ ;

(setq test-parens "(select (this (now) ) ) ")

(defun perlish-fix-regexps (regexp)
  "Simple translation of a perlish REGEXP to an emacs one."
  (let ( (new-pattern regexp) )
    (setq new-pattern (replace-regexp-in-string "(" "\\\\(" new-pattern))
    (setq new-pattern (replace-regexp-in-string ")" "\\\\)" new-pattern))
    (setq new-pattern (replace-regexp-in-string "|" "\\\\|" new-pattern))
    (setq new-pattern (replace-regexp-in-string "\\\\\"" "\"" new-pattern))
    new-pattern))


(tiqsi--parsec-with-remainder test-parens
			      (parsec-many-as-string (parsec-re "(") )

)


;; ;  -------------------------------------------------------------------------------- ;
;; ; Spiral rule : http://c-faq.com/decl/spiral.anderson.html
;; ;; [X] or []
;; ;; => Array X size of... or Array undefined size of...
;; ;; (type1, type2)
;; ;; => function passing type1 and type2 returning...
;; ;; *
;; ;; => pointer(s) to...
;; ;; Operator precedence () * L

;; ;  -------------------------------------------------------------------------------- ;
;; ; Simple declaration
;; (setq test-simple "char *str[10];")
;; ; Pointer to Function declaration
;; (setq test-fn "char *(*fp)( int, float *);")
;; ; The Ultimate test
(setq test-ultimate "void (*signal(int, void (*fp)(int)))(int);")
;; ; ``signal is a function passing an int and a pointer to a function passing an int returning nothing (void) returning a pointer to a function passing an int returning nothing (void)''
;; ;  -------------------------------------------------------------------------------- ;
;; (popup-tip "test")

; TODO Parser for C declr to generate the tokens t be fed through
; TODO Parse (<content>)(<content>) into (<content> . "(<content>)") to handle C-style cast
; TODO spiral recursively
(defun spiral--pointer-to(token)
  (let ((data   (tiqsi--parsec-with-remainder token
				(parsec-str "*")
				)
		))
    (cons
     (if (equal
	  (car?(car data)) 'parsec-error)
	 ""
       "pointer to ")
     (cdr data))
    )
  )



(defun spiral--array(token)
  (let ((data
	 (tiqsi--parsec-with-remainder token
				       (tiqsi--parsec-between-square-brackets)
				       )))
    (cons
     (if (equal
	  (car?(car data)) 'parsec-error)
	 ""
       (if (= (length (car data)) 0)
	   "Array of size  ~Undefined of"
	 (format "Array of size %s of" (car data))))
     (cdr data))))


(defun spiral--func(token)
  (let ((data   (tiqsi--parsec-with-remainder token
  				(tiqsi--parsec-between-round-brackets)
  				)
  		))
    (cons
     (if (equal
	  (car?(car data)) 'parsec-error)
	 ""
       (format "function passing - %s - returning  " (car data) ))
     (cdr data))
    ))



(setq testing
      (tiqsi--parsec-with-remainder (check-for-semicolon test-ultimate)
				    (parsec-collect
				      (parsec-many-as-string
				       (parsec-or
					(parsec-letter)
					(tiqsi--parsec-whitespace))
				       )
				      (tiqsi--parsec-between-round-brackets "(")
				      )
				    )
)

;; (testing)

;; (check-for-semicolon test-ultimate)

;; (spiral--func "s( astat *  tast ) sss")

;; (spiral--pointer-to "*saga")

;; (spiral--array "[123]")

;; (popup-tip "test")

;; --------------------------------Create a frame with tooltip---------------------
;; TODO Make tip based on tip char len + height
(setq tip-frame-params
      '(
	(minibuffer . nil)
	(name . "*Tip Frame*")
	(lambda () (setq mode-line-format nil))
	(visibility . nil)
	(minibuffer-frame-alist nil)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(line-spacing . 0)
	(unsplittable . t)
	(undecorated . t)
	(mouse-wheel-frame . nil)
	(no-other-frame . t)
	(cursor-type . nil)
	(inhibit-double-buffering . t)
	(drag-internal-border . t)
	(no-special-glyphs . t)
	(no-accept-focus . t)
	(no-focus-on-map . t)
	(internal-border-width . 1)
	(right-fringe . 0)
	(left-fringe . 0)
	(top . -1)
	(desktop-dont-save . t)
	(left . -1)))


(defun frame--set-input-focus (frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame)))

(defun make-tip-frame (tip &rest args)
  (setq tip-frame (make-frame
		   (append (append tip-frame-params
				   `(
				     ;; (width . ,(+ (* (frame-char-width) (length tip)) (frame-char-width)))
				     (width . ,(+ (* (/ (frame-char-width) 2) (length tip)) (frame-char-width)))
				     ))
			   `(
			     (height . 5)
			     ;; (height . ,(frame-char-height))
			     ;; (height . ,(/ (frame-char-height) 2))
			     ))
		   )
	  )
    (generate-new-buffer "*Tip Frame Buffer*")


    (set-frame-position tip-frame
			(- (car (window-absolute-pixel-position)) (frame-char-width))
			(+ (cdr (window-absolute-pixel-position)) (frame-char-size)))

    (let ((current-frame (selected-frame) ))
      (make-frame-visible tip-frame)
      (select-frame tip-frame)
      (pop-to-buffer "*Tip Frame Buffer*")
      (with-current-buffer "*Tip Frame Buffer*"
	(fundamental-mode)
	(setq-local beacon-mode nil)
	(setq mode-line-format nil)
	;; (set-background-color "#5F55FF")
	(linum-mode -1)
	(insert  tip)
	)

      (frame--set-input-focus current-frame)
      (frame-restack current-frame tip-frame)
      )
    )

(defun close-tip-frame()
  (with-current-buffer "*Tip Frame Buffer*"
    (delete-region (point-min) (point-max)))
  (delete-frame tip-frame))

;; (make-tip-frame "whyssssssss")
;; (close-tip-frame)


;  -------------------------------------------------------------------------------- ;

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

(defun run-without-asking ()
  "run the current build."
  (interactive)
  (compile "./build-clang/demo")
  (other-window 1))


;; Define + active modification to compile that locally sets
;; shell-command-switch to "-ic".
(defadvice compile (around use-bashrc activate)
  "Load .bashrc in any calls to bash (e.g. so we can use aliases)"
  (let ((shell-command-switch "-ic"))
    ad-do-it))


(defun rtags-peek-definition ()
  "Peek at definition at point using rtags."
  (interactive)
  (let ((func (lambda ()
                (rtags-find-symbol-at-point)
                (rtags-location-stack-forward)
                )))
    (rtags-start-process-unless-running)
    (make-peek-frame func)))


(defun make-peek-frame (find-definition-function &rest args)
  "Make a new frame for peeking definition"
  (when (or (not (rtags-called-interactively-p)) (rtags-sandbox-id-matches))
    (let (summary
          doc-frame
          x y
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
          ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (abs-pixel-pos (save-excursion
                           (beginning-of-thing 'symbol)
                           (window-absolute-pixel-position))))
      (setq x (car abs-pixel-pos))
      ;; (setq y (cdr abs-pixel-pos))
      (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (setq doc-frame (make-frame '((minibuffer . nil)
                                    (name . "*RTags Peek*")
                                    (width . 80)
                                    (visibility . nil)
                                    (height . 15))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (set-frame-position doc-frame x y)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (with-selected-frame doc-frame
        (apply find-definition-function args)
        (read-only-mode)
        (when semantic-stickyfunc-mode (semantic-stickyfunc-mode -1))
        (recenter-top-bottom 0))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (make-frame-visible doc-frame))))

(define-key c-mode-base-map (kbd "C-c p") 'rtags-peek-definition)
(define-key c-mode-base-map (kbd "C-c d") 'delete-frame)


                                        ;------{Hydras}-----;

(defhydra hydra-rtags (:color pink :hint nil)
  "
^Symbols^                        ^Location Stack^   ^Fix it^
-------------------------------------------------------------------------------
_s_: Find symbol at point        _>_: Forward       _t_: Fix it at point
_r_: Find references at point    _<_: Backward      _T_: Fix it
_f_: Find File                                    _i_: Add include for symbol
_S_: Find symbol                 ^RDM^              _<right>_: Next diagnostic
_R_: Find refrences              _b_: Start process _<left>_: Prev diagnostic
_v_: Find virtuals at point
"
  ("s" rtags-find-symbol-at-point :color blue)
  ("r" rtags-find-references-at-point :color blue)
  ("f" rtags-find-file :color blue)
  ("S" rtags-find-symbol :color blue)
  ("R" rtags-find-references :color blue)
  ("v" rtags-find-virtuals-at-point :color blue)
  (">" rtags-location-stack-back :color red)
  ("<" rtags-location-stack-forward :color red)
  ("<right>" rtags-next-diag :color red)
  ("<left>" rtags-previous-diag :color red)
  ("t" rtags-fix-fixit-at-point :color blue)
  ("T" rtags-fixit :color blue)
  ("i" rtags-get-include-file-for-symbol :color blue)
  ("b" rtags-start-process-unless-running :color blue)
  ("ESC" nil "Exit"))

                                        ;---{Keybindings}---;


;; (global-set-key (kbd "<f6>") 'ac-start)

;; (define-key ac-complete-mode-map [t] 'ac-self-insert)
(define-key ac-complete-mode-map (kbd "M-x") 'execute-extended-command)
(define-key ac-complete-mode-map (kbd "C-n") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-g") 'ac-stop)
(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-complete-mode-map "\r" 'ac-complete)

(global-set-key [C-f1] 'show-file-name) ; Or any other key you want

(global-set-key (kbd "C-?" ) nil)
(define-key c++-mode-map (kbd "C-?") 'irony-get-type)
(define-key c-mode-map (kbd "C-?") 'irony-get-type)
(define-key c-mode-base-map (kbd "C-?") 'irony-get-type)

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
;; (define-key c++-mode-map "    " 'indent-region)
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


;; TODO Make dependant on what build-system is being used
;; (define-key global-map (kbd "M-m") 'make-without-asking)
;; (define-key global-map (kbd "M-n") 'run-without-asking)
(define-key global-map (kbd "M-m") 'compile-c-lang)
(define-key global-map (kbd "M-n") 'run-c-lang)
(define-key global-map (kbd "M-/") 'close-side-come-back)
(define-key c-mode-base-map (kbd "C-<down>") 'insert-semicolon)


(provide 'programming-c)

;;; programming-c.el ends here
