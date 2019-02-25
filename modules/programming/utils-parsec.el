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

;; TODO CMake Parser / Ninja Parser / Meson parser

(require 'parsec)

;; utils-parsec file utilities

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
	(tiqsi-find-file-in-hierarchy parent file-name)))))

(defun utils-parsec--get-string-from-file (filePath)
  "Return filePath's file content
   thanks to “Pascal J Bourguignon” and
   “TheFlyingDutchman 〔zzbba…@aol.com〕”.
   2010-09-02
   "
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


;; TODO Recursively start walking the paths
;; Check for -> is-dir?
;;           -> check files-in-dir
;;           -> walk those directories until there is a match


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
  (cons 'Snake token))

(defsubst utils-parsec--str(token)
  (cons 'String token))

;; utils-parsec for lexing


(defun utils-parsec--lex-space()
  (utils-parsec--space
    (parsec-re " " )))


(defun utils-parsec--lex-spaces()
  (utils-parsec--spaces
   (parsec-many1-as-string (parsec-re " " ))))


(defun utils-parsec--lex-number()
  (utils-parsec--number
   (string-to-number (parsec-many1-as-string (parsec-digit)))))


(defun utils-parsec--lex-eol()
  (utils-parsec--eol
   (parsec-eol)))


(defun utils-parsec--lex-eof()
  (utils-parsec--eof
   (parsec-eof)))


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

(defun utils-parsec--lex-snake()
  (utils-parsec--snake
  (parsec-collect
   (parsec-many1-as-string
   (parsec-or
    (parsec-many1-s
     (parsec-letter))
     (parsec-ch ?\_))
   )
  )))

(defun utils-parsec--lex-pyfunc()

  (utils-parsec--pyfunc
  (car (parsec-collect
   (utils-parsec--lex-snake)
   (parsec-lookahead
    (parsec-ch ?\()))))

  )

;; TODO
;; (defun utils-parsec--lex-str()
  
;;   )

;; TODO REMOVE TEST CASES 
(parsec-with-input "\"this is a string\""
  (utils-parsec--lex-snake)
  )

(parsec-with-input "this_is_a_name(132), funcall, params, for, Python)"

  (parsec-collect
  (utils-parsec--lex-pyfunc)
  (utils-parsec--between-brackets (utils-parsec--lex-number))
)
  )


(parsec-with-input "1 2  3 4"
  (utils-parsec--lex-list-by-sep
   (utils-parsec--lex-number)
   (utils-parsec--lex-spaces))
  )


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



(utils-parsec--return-remainder "test This"
   (parsec-string "test")

  )

;; TODO Eval last sexpr macro with car and cdr added - speed up dev 

(scheme-read "(test (this '(niw as))")
(scheme-read "(niw as)")



(utils-parsec--remove-newlines
 (utils-parsec--search-file-get-string "init.el"))


(provide 'utils-parsec)

;;; utils-parsec.el ends here
