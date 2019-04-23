;;; utils-parsec.el --- Tiqsi utilities for the parsec parser combinators  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (parsec "24") (s "1.6.0"))

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

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                              Defmacro                                             ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; with-buffer-content                                                                            ...;
; utils-parsec--return-index-remainder                                                           ...;
; utils-parsec--return-remainder                                                                 ...;
; utils-parsec--lex-list-by-sep                                                                  ...;
; utils-parsec--between-brackets                                                                 ...;
; utils-parsec--between-round-brackets                                                           ...;
; utils-parsec--between-double-quotes                                                            ...;
; utils-parsec--between-single-quotes                                                            ...;
; utils-parsec--between-triple-quotes                                                            ...;
; utils-parsec--between-macro                                                                    ...;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                              DefSubst                                             ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; utils-parsec--bool                                                                             ...;
; utils-parsec--true                                                                             ...;
; utils-parsec--false                                                                            ...;
; utils-parsec--atom                                                                             ...;
; utils-parsec--number                                                                           ...;
; utils-parsec--list                                                                             ...;
; utils-parsec--space                                                                            ...;
; utils-parsec--spaces                                                                           ...;
; utils-parsec--eol                                                                              ...;
; utils-parsec--eof                                                                              ...;
; utils-parsec--pyfunc                                                                           ...;
; utils-parsec--snake                                                                            ...;
; utils-parsec--str                                                                              ...;
; utils-parsec--double                                                                           ...;
; utils-parsec--defun                                                                            ...;
; utils-parsec--defsubst                                                                         ...;
; utils-parsec--defmacro                                                                         ...;
; utils-parsec--defvar                                                                           ...;
; utils-parsec--defcustom                                                                        ...;
; utils-parsec--defminmode                                                                       ...;
; utils-parsec--declarefun                                                                       ...;
; utils-parsec--lisp-case                                                                        ...;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                               Defun                                               ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; car?                                                                                           ...;
; utils-parsec--insert-center                                                                    ...;
; utils-parsec--insert-sep                                                                       ...;
; utils-parsec--insert-sep-sparse                                                                ...;
; utils-parsec--insert-left                                                                      ...;
; utils-parsec--current-dir                                                                      ...;
; utils-parsec--parent-directory                                                                 ...;
; utils-parsec--find-file-in-hierarchy                                                           ...;
; utils-parsec--get-string-from-file                                                             ...;
; utils-parsec--search-file-get-string                                                           ...;
; utils-parsec--retrieve-remaining-by-idx                                                        ...;
; utils-parsec--join                                                                             ...;
; utils-parsec--remove-newlines                                                                  ...;
; utils-parsec--parse-space                                                                      ...;
; utils-parsec--lex-space                                                                        ...;
; utils-parsec--parse-spaces                                                                     ...;
; utils-parsec--lex-spaces                                                                       ...;
; utils-parsec--parse-number                                                                     ...;
; utils-parsec--lex-number                                                                       ...;
; utils-parsec--parse-eol                                                                        ...;
; utils-parsec--lex-eol                                                                          ...;
; utils-parsec--parse-eof                                                                        ...;
; utils-parsec--lex-eof                                                                          ...;
; utils-parsec--parse-snake                                                                      ...;
; utils-parsec--lex-snake                                                                        ...;
; utils-parsec--parse-lisp-case                                                                  ...;
; utils-parsec--lex-lisp-case                                                                    ...;
; utils-parsec--parse-defun                                                                      ...;
; utils-parsec--lex-defun                                                                        ...;
; utils-parsec--parse-defsubst                                                                   ...;
; utils-parsec--lex-defsubst                                                                     ...;
; utils-parsec--parse-defmacro                                                                   ...;
; utils-parsec--lex-defmacro                                                                     ...;
; utils-parsec--parse-defvar                                                                     ...;
; utils-parsec--lex-defvar                                                                       ...;
; utils-parsec--parse-defcustom                                                                  ...;
; utils-parsec--lex-defcustom                                                                    ...;
; utils-parsec--parse-defminmode                                                                 ...;
; utils-parsec--lex-defminmode                                                                   ...;
; utils-parsec--parse-declarefun                                                                 ...;
; utils-parsec--lex-declarefun                                                                   ...;
; utils-parsec--lex-pyfunc                                                                       ...;
; utils-parsec--parse-alphanumeric                                                               ...;
; utils-parsec--parse-double-quote-str                                                           ...;
; utils-parsec--lex-double-quote-str                                                             ...;
; utils-parsec--parse-single-quote-str                                                           ...;
; utils-parsec--lex-single-quote-str                                                             ...;
; utils-parsec--parse-triple-quote-str                                                           ...;
; utils-parsec--lex-triple-quote-str                                                             ...;
; utils-parsec--lex-py-str                                                                       ...;
; utils-parsec--lex-double                                                                       ...;
; utils-parsec--ascii-special-chars-no-brackets-semicolon                                        ...;
; utils-parsec--get-current-buffer-definitions-elisp                                             ...;
; utils-parsec--current-buffer-documentation-elisp                                               ...;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                               Defvar                                              ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                             Defcustom                                             ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                         Define-minor-mode                                         ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;                                          Declare-Function                                         ;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
;                                     End Parsec generated info                                     ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;



; ------------------------------------------------------------------------------------------------  ;
;                                    Parsec Utilities available                                     ;
; ------------------------------------------------------------------------------------------------  ;


; ------------------------------------------------------------------------------------------------  ;
;                                       Parsec Utilities TODO                                       ;
; ------------------------------------------------------------------------------------------------  ;
;                                                                                                   ;
; Recursively start walking the paths                                                               ;
; Check for -> is-dir?                                                                              ;
;           -> check files-in-dir                                                                   ;
;	    -> walk those directories until there is a match                                        ;
;                                                                                                   ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
;                                                                                                   ;
;  parse (select (this ( now) ) ) -> ("select" "(this ( now) )")                                    ;
;                                     -> ("select" ("this "(now)"))                                 ;
;                                     -> ("select" ("this ("now") ))                                ;
;                                                                                                   ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
;                                                                                                   ;
; Eval last sexpr macro with car and cdr added - speed up dev                                       ;
;                                                                                                   ;
; ------------------------------------------------------------------------------------------------  ;


; ------------------------------------------------------------------------------------------------  ;
;                                 Parsec Utilities example Use-cases TODO                           ;
; ------------------------------------------------------------------------------------------------  ;
;                                                                                                   ;
; Create a build-system loader for c-mode:                                                          ;
;   -> CMake Parser                                                                                 ;
;   -> Ninja Parser                                                                                 ;
;   -> Meson Parser                                                                                 ;
;                                                                                                   ;
; ------------------------------------------------------------------------------------------------  ;

(require 'parsec)

;                                            misc-defuns                                            ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;


(defun with-buffer-content (&rest fun)
  `(let ((content ,(with-current-buffer (get-buffer (current-buffer))
	    (buffer-substring-no-properties (point-min) (point-max))
	    )))
    ,@fun
    ))


(defun car? (input)
  (condition-case nil
      (car input)
    (error input)))

(defun utils-parsec--insert-center (string)
  (interactive "sString for inside centered message: ")
  (insert comment-start)
  (insert
    (s-truncate 99 (s-center 99 string ) ))
  (insert (s-trim comment-start))
  (newline))


(defun utils-parsec--insert-sep-full ()
  (interactive)
  (insert
   (format "%s ------------------------------------------------------------------------------------------------- %s" comment-start comment-start))
  (newline)
  )

(defun utils-parsec--insert-sep-med ()
  (interactive)
  (insert
   (format "%s - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - %s" comment-start comment-start))
  (newline)
  )

(defun utils-parsec--insert-sep-sparse ()
  (interactive)
  (insert
   (format "%s -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - %s" comment-start comment-start))
  (newline)
  )

(defun utils-parsec--insert-sep-end ()
  (interactive)
  (insert
   (format "%s _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ %s" comment-start comment-start))
  (newline)
  )

(defun utils-parsec--insert-box ()
  (interactive)
  (beginning-of-line 1)
  (utils-parsec--insert-sep-full)
  (sdev/line-to-msg-centered)
  (utils-parsec--insert-sep-full)
  (newline-and-indent)
  (utils-parsec--insert-sep-end))


(defun utils-parsec--insert-left (string)
  (interactive "sString for inside left message: ")
  (insert (s-pad-right 2 " " comment-start))
  (insert
    (s-truncate 98 (s-pad-right 99 " " string ) ))
  (insert (s-trim comment-start))
  (newline))



(defun utils-parsec--current-dir ()
  (file-name-directory buffer-file-name))

(defun utils-parsec--current-dir-up (amount)
  ()
  )

;; (utils-parsec--current-dir)




(defun utils-parsec--parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))


;; (utils-parsec--find-file-in-hierarchy (utils-parsec--current-dir) "utils-parsec.el")

;; (current-dir)


(defun utils-parsec--find-file-in-hierarchy (current-dir file-name)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file
	 (concat current-dir file-name))
	(parent
	 (utils-parsec--parent-directory
	  (expand-file-name current-dir ))  ))
    (if (file-exists-p file)
	file
      (when parent
	(utils-parsec--find-file-in-hierarchy parent file-name)))))

(defun utils-parsec--get-string-from-file (filePath)
  "Return filePath's file content
   thanks to “Pascal J Bourguignon” and
   “TheFlyingDutchman 〔zzbba…@aol.com〕”.
   2010-09-02
   "
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defmacro utils-parsec--search-file-get-string(file-name)
  `(utils-parsec--get-string-from-file
   (utils-parsec--find-file-in-hierarchy
    ,(utils-parsec--current-dir) ,file-name)))

;; utils-parsec input utilities


(defun utils-parsec--retrieve-remaining-by-idx (data idx)
  (condition-case nil
      (substring data idx nil)
    (error "")))


(defmacro utils-parsec--return-index-remainder (data &rest parsers )
  `(let ((parsec-result
	  (parsec-with-input ,data
	    ,@parsers))
	 (idx
	  (parsec-with-input ,data
	    (parsec-query
	     ,@parsers
	     :end ))))
     (cons parsec-result
	   (cons (if (> idx 1)
		     (- idx 1)
		   idx)
		 (utils-parsec--retrieve-remaining-by-idx
		  ,data
 		  (if  (equal (car? idx) 'parsec-error)
		      0
		    (- idx 1)))))))


(defmacro utils-parsec--return-remainder (data &rest parsers )
  `(let ((parsec-result
	  (parsec-with-input ,data
	    ,@parsers))
	 (idx
	  (parsec-with-input ,data
	    (parsec-query
	     ,@parsers
	     :end ))))
     (cons
      parsec-result
      (utils-parsec--retrieve-remaining-by-idx
       ,data
       (if  (equal (car? idx) 'parsec-error)
	   0
	 (- idx 1))))))

;; utils-parsec string manipulation utilities

(defun utils-parsec--join (sep string-list)
  (mapconcat 'identity (split-string string-list) sep))

(defun utils-parsec--remove-newlines(source)
  (utils-parsec--join " " source))


;; utils-parsec data type atomics
;; Info: defsubst is a macro to create inline byte-compiled functions
(defsubst utils-parsec--bool (value)
  (cons 'Bool value))

(defsubst utils-parsec--true ()
  (utils-parsec--bool 'True))

(defsubst utils-parsec--false ()
  (utils-parsec--bool 'False))

(defsubst utils-parsec--atom(atom)
  (cons 'Atom atom))

(defsubst utils-parsec--number (number)
  (cons 'Number number))

(defsubst utils-parsec--list (&rest values)
  (cons 'List values))

(defsubst utils-parsec--space (space)
  (cons 'Whitespace space))

(defsubst utils-parsec--spaces (spaces)
  (cons 'Whitespaces spaces))

(defsubst utils-parsec--eol (token)
  (cons 'Eol token))

(defsubst utils-parsec--eof (token)
  (cons 'Eof token))

(defsubst utils-parsec--pyfunc(token)
  (cons 'Pyfunc token))


(defsubst utils-parsec--func-notation(token)
  (cons 'FuncN token))


(defsubst utils-parsec--snake(token)
  (cons 'SnakeCase token))

(defsubst utils-parsec--str(token)
  (cons 'String token))

(defsubst utils-parsec--double(token)
  (cons 'Double token))

(defsubst utils-parsec--defun(token)
  (cons 'Defun token))

(defsubst utils-parsec--defsubst(token)
  (cons 'Defsubst token))

(defsubst utils-parsec--defmacro(token)
  (cons 'Defmacro token))

(defsubst utils-parsec--defvar(token)
  (cons 'Defvar token))

(defsubst utils-parsec--defcustom(token)
  (cons 'Defcustom token))

(defsubst utils-parsec--defminmode(token)
  (cons 'Defminmode token))

(defsubst utils-parsec--declarefun(token)
  (cons 'Declarefun token))

(defsubst utils-parsec--lisp-case(token)
  (cons 'LispCase token))

;; utils-parsec for lexing

(defun utils-parsec--parse-space()
  (parsec-re " "))

(defun utils-parsec--lex-space()
  (utils-parsec--space
    (utils-parsec--parse-space)))

(defun utils-parsec--parse-spaces()
  (parsec-many1-as-string (parsec-re " " )))

(defun utils-parsec--lex-spaces()
  (utils-parsec--spaces
   (utils-parsec--parse-spaces)))



(defun utils-parsec--parse-number()
  (parsec-many1-as-string (parsec-digit)))


(defun utils-parsec--lex-number()
  (utils-parsec--number
   (string-to-number
    (utils-parsec--parse-number))))

(defun utils-parsec--parse-eol()
  (parsec-eol))

(defun utils-parsec--lex-eol()
  (utils-parsec--eol
   (utils-parsec--parse-eol)))

(defun utils-parsec--parse-eof()
  (parsec-eof))


(defun utils-parsec--lex-eof()
  (utils-parsec--eof
   (utils-parsec--parse-eof)))



;; utils-parsec generic re-usable parser combinators


(defmacro utils-parsec--lex-list-by-sep (what sep)
  `(apply #'utils-parsec--list
	 (parsec-sepby
	  ,what
	  ,sep)))

(defmacro utils-parsec--between-brackets(&rest parsers)
  `(parsec-between
    (parsec-ch ?\()
    (parsec-ch ?\))
    ,@parsers))

(defmacro utils-parsec--between-round-brackets(&rest parsers)
  `(parsec-between
    (parsec-ch ?\()
    (parsec-ch ?\))
    ,@parsers))

(defmacro utils-parsec--between-double-quotes(&rest parsers)
  `(parsec-between
    (parsec-ch ?\")
    (parsec-ch ?\")
    ,@parsers))

(defmacro utils-parsec--between-single-quotes(&rest parsers)
  `(parsec-between
    (parsec-ch ?\')
    (parsec-ch ?\')
    ,@parsers))

(defmacro utils-parsec--between-triple-quotes(&rest parsers)
  `(parsec-between
    (parsec-re "\"\"\"")
    (parsec-re "\"\"\"")
    ,@parsers))

(defmacro utils-parsec--between-macro(name init end)
  `(defmacro ,(intern (format "utils-parsec--between-%s" name))
       (&rest parsers)
     (parsec-between
      (parsec-ch ,init)
      (parsec-ch ,end)
      ,(intern ",@parsers"))))


;; ;; TODO generate defmacros with macro
;; (utils-parsec--between-macro "round-brackets" ?\( ?\))
;; (utils-parsec--between-macro "square-brackets" ?\[ ?\])

(defun utils-parsec--parse-snake()
  (parsec-collect
   (parsec-many1-as-string
   (parsec-or
    (parsec-many1-s
     (parsec-letter))
     (parsec-ch ?\_)))))

(defun utils-parsec--lex-snake()
  (utils-parsec--snake
   (utils-parsec--parse-snake)))


;                                                Lisp                                               ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

(defun utils-parsec--parse-lisp-case()
  (parsec-collect
   (parsec-many1-as-string
    (parsec-or
     (parsec-many1-s
      (parsec-letter))
     (parsec-many1-s
      (parsec-digit))
     (parsec-many1-s (parsec-ch ?\?))
     (parsec-many1-s (parsec-ch ?\.))
     (parsec-many1-s (parsec-ch ?\|))
     (parsec-many1-s (parsec-ch ?\-))))))


(defun utils-parsec--lex-lisp-case()
  (utils-parsec--lisp-case
   (utils-parsec--parse-lisp-case)
  ))


(defun utils-parsec--parse-defun ()
 (parsec-between
   (parsec-re "(defun ")
   ;; (parsec-re " ?(")
   (parsec-or (parsec-re " ?(") (parsec-eol))
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defun ()
  (utils-parsec--defun
  (utils-parsec--parse-defun)))


(defun utils-parsec--parse-defsubst ()
 (parsec-between
   (parsec-re "(defsubst ")
   (parsec-or (parsec-re " ?(") (parsec-eol))
   ;; (parsec-re " ?(")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defsubst ()
  (utils-parsec--defsubst
  (utils-parsec--parse-defsubst)))


(defun utils-parsec--parse-defmacro ()
 (parsec-between
   (parsec-re "(defmacro ")
   (parsec-or (parsec-re " ?(") (parsec-eol))
   ;; (parsec-re " ?(")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defmacro ()
  (utils-parsec--defmacro
  (utils-parsec--parse-defmacro)))

;; TODO Handle non () cases
(defun utils-parsec--parse-defvar ()
 (parsec-between
   (parsec-re "(defvar ")
   (parsec-or (parsec-re " ") (parsec-ch ?\)))
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defvar ()
  (utils-parsec--defvar
  (utils-parsec--parse-defvar)))


;; TODO Handle non () cases
(defun utils-parsec--parse-defcustom ()
 (parsec-between
   (parsec-re "(defcustom ")
   (parsec-or (parsec-re " ") (parsec-ch ?\)))
   ;; (parsec-re " ")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defcustom ()
  (utils-parsec--defcustom
  (utils-parsec--parse-defcustom)))

;; TODO Handle non () cases
(defun utils-parsec--parse-defminmode ()
 (parsec-between
   (parsec-re "(define-minor-mode ")
   (parsec-or (parsec-re " ") (parsec-ch ?\)))
   ;; (parsec-re " ")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defminmode ()
  (utils-parsec--defminmode
  (utils-parsec--parse-defminmode)))

;; TODO Handle non () cases
(defun utils-parsec--parse-declarefun ()
 (parsec-between
   (parsec-re "(declare-function ")
   (parsec-or (parsec-re " ") (parsec-ch ?\)))
   ;; (parsec-re " ")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-declarefun ()
  (utils-parsec--declarefun
  (utils-parsec--parse-declarefun)))

; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

(defun utils-parsec--lex-pyfunc()

  (utils-parsec--pyfunc
  (car (parsec-collect
   (utils-parsec--lex-snake)
   (parsec-lookahead
    (parsec-ch ?\()))))

  )


(defun utils-parsec--lex-func-notation()

  (utils-parsec--func-notation
  (car (parsec-collect
   (utils-parsec--lex-snake)
   (parsec-lookahead
    (parsec-ch ?\()))))

  )

(defun utils-parsec--parse-alphanumeric()
  (parsec-many1-as-string
   (parsec-or
    (parsec-many1-as-string (parsec-letter))
    (parsec-many1-as-string (parsec-digit))
    (parsec-many1-as-string (parsec-re " ")))))




(defun utils-parsec--parse-double-quote-str()
  (utils-parsec--between-double-quotes
   (utils-parsec--parse-alphanumeric)))



(defun utils-parsec--lex-double-quote-str()
  (utils-parsec--str
   (utils-parsec--parse-double-quote-str)))


(defun utils-parsec--parse-single-quote-str()
  (utils-parsec--between-single-quotes
   (utils-parsec--parse-alphanumeric)))


(defun utils-parsec--lex-single-quote-str()
  (utils-parsec--str
   (utils-parsec--parse-single-quote-str)))


(defun utils-parsec--parse-triple-quote-str()
  (utils-parsec--between-triple-quotes
   (utils-parsec--parse-alphanumeric)))

(defun utils-parsec--lex-triple-quote-str()
  (utils-parsec--str
   (utils-parsec--parse-triple-quote-str)))


(defun utils-parsec--lex-py-str()
  (utils-parsec--str
   (parsec-or
    (utils-parsec--parse-triple-quote-str)
    (utils-parsec--parse-double-quote-str)
    (utils-parsec--parse-single-quote-str))))

(defun utils-parsec--lex-double()
  (utils-parsec--double
   (string-to-number
    (parsec-many1-as-string
     (parsec-or
      (parsec-many1-as-string (parsec-digit))
      (parsec-ch ?\.))
     ))))

;                                 TODO add type identifiers to these                                ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

(defun utils-parsec--parse-sep (sep)
  (parsec-collect
   (parsec-re (format "%s" sep) )
   (utils-parsec--lex-spaces))
  )


(defun utils-parsec--parse-func-application (sep)
  (parsec-collect
   (utils-parsec--lex-func-notation)
   (utils-parsec--between-round-brackets
    (utils-parsec--lex-list-by-sep
     (parsec-between (parsec-ch ?\')
		     (parsec-ch ?\')
		     (parsec-or
		      (utils-parsec--lex-lisp-case)
		      (utils-parsec--lex-snake-case)))
     (utils-parsec--parse-sep sep)))))


; ------------------------------------------------------------------------------------------------  ;
;                                             Test Cases                                            ;
; ------------------------------------------------------------------------------------------------  ;

;                                           String support                                          ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

(parsec-with-input "\"this is a string\""
  (utils-parsec--lex-py-str)
  )

(parsec-with-input "\'this one is too\'"
  (utils-parsec--lex-py-str)
  )

(parsec-with-input "\"\"\"this 1 is too\"\"\""
  (utils-parsec--lex-py-str)
  )

(parsec-with-input "\"\"\"this 1 is too\"\"\""
  (utils-parsec--lex-py-str)
  )

;; TODO support Ascii range of special chars
(parsec-with-input "\"\"\"this@ 1 is too\"\"\""
  (utils-parsec--lex-py-str)
  )

(parsec-with-input "123.24"
  (utils-parsec--lex-double))

;                                            Lisp Support                                           ;
; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;
(parsec-with-input "this-is--lisp"
  (utils-parsec--lex-lisp-case))


(parsec-with-input "(defun this-is--lisp ()"
  (utils-parsec--lex-defun))



;; TODO handle these cases
;; (utils-parsec--return-remainder "




;; (with-buffer-content
;;  (print content))

(defun utils-parsec--ascii-special-chars-no-brackets-semicolon ()
  (parsec-re "['\\!%#\"$ &\*\+\-/,\.:\|^_`~=\?]")
  )


(defun utils-parsec--get-current-buffer-parsed(content)
  (let ((
	 current-defuns
	 (parsec-with-input content
	     (parsec-collect
	      (parsec-many
	       (parsec-or
		(utils-parsec--lex-spaces)
		(utils-parsec--lex-eol)
		(parsec-between
		 (parsec-ch ?\;)
		 (utils-parsec--lex-eol)
		 (parsec-re ".*"))
		(parsec-or
		 (utils-parsec--lex-defun)
		 (utils-parsec--lex-defsubst)
		 (utils-parsec--lex-defmacro)
		 (utils-parsec--lex-defvar)
		 (utils-parsec--lex-defcustom)
		 (utils-parsec--lex-defminmode)
		 (utils-parsec--lex-declarefun))
		(parsec-re ".*")))))))
    current-defuns
  ))

(defun utils-parsec--collect-defuns(current-defuns)
  (cl-loop for item in current-defuns
	   with x
	   if (or(equal (car? item) 'Defmacro)
		 (equal (car? item) 'Defun)
		 (equal (car? item) 'Defsubst)
		 (equal (car? item) 'Defvar)
		 (equal (car? item) 'Defcustom)
		 (equal (car? item) 'Defminmode)
		 (equal (car? item) 'Declarefun)
		 )
	   collect item
	   ))

(defun utils-parsec--get-current-buffer-definitions-elisp()
  (with-current-buffer (current-buffer)
      (utils-parsec--collect-defuns  (car(utils-parsec--get-current-buffer-parsed
					  (buffer-substring-no-properties (point-min) (point-max)))))))


(defun utils-parsec--current-buffer-documentation-elisp ()
  (interactive)
  (let ((current-defuns
	 (utils-parsec--get-current-buffer-definitions-elisp)
	 ))

    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Defmacro")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
	     with x
	     if (equal(car? item) 'Defmacro)
	     do (progn
		  (utils-parsec--insert-left (caddr item))
		  )
	     )
    (utils-parsec--insert-sep-sparse)
    (newline)

    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "DefSubst")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
	     with x
	     if (equal(car? item) 'Defsubst)
	     do (progn
		  (utils-parsec--insert-left (caddr item))
		  )

	     )

    (utils-parsec--insert-sep-sparse)
    (newline)
    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Defun")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
	     with x
	     if (equal(car? item) 'Defun)
	     do (progn
		  (utils-parsec--insert-left (caddr item))
		  )
	     )
    (utils-parsec--insert-sep-sparse)


    (utils-parsec--insert-sep-sparse)
    (newline)
    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Defvar")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
    	     with x
    	     if (equal(car? item) 'Defvar)
    	     do (progn
    		  (utils-parsec--insert-left (caddr item))
    		  )
    	     )
    (utils-parsec--insert-sep-sparse)

    (utils-parsec--insert-sep-sparse)
    (newline)
    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Defcustom")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
    	     with x
    	     if (equal(car? item) 'Defcustom)
    	     do (progn
    		  (utils-parsec--insert-left (caddr item))
    		  )
    	     )
    (utils-parsec--insert-sep-sparse)


    (utils-parsec--insert-sep-sparse)
    (newline)
    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Define-minor-mode")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
    	     with x
    	     if (equal(car? item) 'Defminmode)
    	     do (progn
    		  (utils-parsec--insert-left (caddr item))
    		  )
    	     )
    (utils-parsec--insert-sep-sparse)


    (utils-parsec--insert-sep-sparse)
    (newline)
    (utils-parsec--insert-sep)
    (utils-parsec--insert-center "Declare-Function")
    (utils-parsec--insert-sep)
    (cl-loop for item in current-defuns
    	     with x
    	     if (equal(car? item) 'Declarefun)
    	     do (progn
    		  (utils-parsec--insert-left (caddr item))
    		  )
    	     )
    (utils-parsec--insert-sep-sparse)


    (utils-parsec--insert-center "End Parsec generated info")
    (utils-parsec--insert-sep-sparse)
    (newline)
    ))



;; (insert (utils-parsec--sep))

;; (caddr(car (get-current-buffer-definitions-elisp)))


;; (parsec-with-input "\\ \t \""
;;   (parsec-re (perlish-fix-regexps "(\\(?:b|t|n|f|r|\"|\\)|\\(?:(?:[0-2][0-9]{1,2}|3[0-6][0-9]|37[0-7]|[0-9]{1,2}))|\\(?:u(?:[0-9a-fA-F]{4})))"))
;;   )

;; "(\\(?:b|t|n|f|r|\"|\\)|\\(?:(?:[0-2][0-9]{1,2}|3[0-6][0-9]|37[0-7]|[0-9]{1,2}))|\\(?:u(?:[0-9a-fA-F]{4})))"

(parsec-with-input "(132)"

  (utils-parsec--between-round-brackets (utils-parsec--lex-number))
)

(parsec-with-input "this_is_a_func_name(132)"

  (parsec-collect
    (utils-parsec--lex-pyfunc)
    (utils-parsec--between-round-brackets (utils-parsec--lex-number))
    )
  )


(parsec-with-input "1 2  3 4"
  (utils-parsec--lex-list-by-sep
   (utils-parsec--lex-number)
   (utils-parsec--lex-spaces))
  )

(parsec-with-input "2, 3, 4, 5"
  )

(parsec-with-input ", ,  , "
  (parsec-collect
   (parsec-re "," )
   (utils-parsec--lex-spaces))
  )
(parsec-with-input ", ,  , "
  (utils-parsec--parse-sep ",")
  )


(format "%s" ",")


(utils-parsec--return-remainder "test This"
   (parsec-string "test"))

(parsec-with-input
 "2  134 2
 2"
  (parsec-many
   (parsec-or
    ;; (parsec-re " ")
    ;; (utils-parsec--lex-space)
    (utils-parsec--lex-spaces)
    (utils-parsec--lex-number)
    (utils-parsec--lex-eol)
    ;; (parsec-digit)
    ))
  )



;; TODO Eval last sexpr macro with car and cdr added - speed up dev

;; (utils-parsec--remove-newlines
;;  (utils-parsec--search-file-get-string "init.el"))


(provide 'utils-parsec)

;;; utils-parsec.el ends here
