;;; utils-parsec.el --- Tiqsi utilities for the parsec parser combinators  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (parsec "24"))

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


(defun car? (input)
  (condition-case nil
      (car input)
    (error input)))


(defun utils-parsec--current-dir ()
  (file-name-directory buffer-file-name))

(defun utils-parsec--parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))


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


(defun utils-parsec--search-file-get-string(file-name)
  (utils-parsec--get-string-from-file
   (utils-parsec--find-file-in-hierarchy
    (utils-parsec--current-dir) file-name)))

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
     (parsec-many1-s (parsec-ch ?\-))))))


(defun utils-parsec--lex-lisp-case()
  (utils-parsec--lisp-case
   (utils-parsec--parse-lisp-case)
  ))


(defun utils-parsec--parse-defun ()
 (parsec-between
   (parsec-re "(defun ")
   (parsec-re " ?(")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defun ()
  (utils-parsec--defun
  (utils-parsec--parse-defun)))


(defun utils-parsec--parse-defsubst ()
 (parsec-between
   (parsec-re "(defsubst ")
   (parsec-re " ?(")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defsubst ()
  (utils-parsec--defsubst
  (utils-parsec--parse-defsubst)))


(defun utils-parsec--parse-defmacro ()
 (parsec-between
   (parsec-re "(defmacro ")
   (parsec-re " ?(")
   (utils-parsec--lex-lisp-case)))

(defun utils-parsec--lex-defmacro ()
  (utils-parsec--defmacro
  (utils-parsec--parse-defmacro)))

; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - ;

(defun utils-parsec--lex-pyfunc()

  (utils-parsec--pyfunc
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
(car
(parsec-with-input "
(defsubst utils-parsec--bool (value)
  (cons 'Bool value))


;; utils-parsec generic re-usable parser combinators

(defmacro utils-parsec--lex-list-by-sep (what sep)
  `(apply #'utils-parsec--list
	 (parsec-sepby
	  ,what
	  ,sep)))

(defun utils-parsec--lex-eof()
  (utils-parsec--eof
   (utils-parsec--parse-eof)))




(defmacro utils-parsec--lex-list-by-sep (what sep)
  `(apply #'utils-parsec--list
	 (parsec-sepby
	  ,what
	  ,sep)))
"
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
     )
    (parsec-re ".*")
    )
   )
))
)

(defmacro with-buffer-content (&rest fun)
  `(let ((content ,(buffer-substring-no-properties (point-min) (point-max) )))
    ,@fun
  ))


(with-buffer-content
 (print content))

(defun utils-parsec--ascii-special-chars-no-brackets-semicolon ()
  (parsec-re "['\\!%#\"$ &\*\+\-/,\.:\|^_`~=\?]")
  )

(with-buffer-content 

;; (parsec-with-input content
(utils-parsec--return-remainder content

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
     )
    (parsec-re ".*")
     ;; (parsec-ch ?\?)
     ;; (parsec-re " ?(\"")
     (utils-parsec--ascii-special-chars-no-brackets-semicolon)
    )
   )
   )
  ))


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



(utils-parsec--remove-newlines
 (utils-parsec--search-file-get-string "init.el"))


(provide 'utils-parsec)

;;; utils-parsec.el ends here
