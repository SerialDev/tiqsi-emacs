;;; programming-assembly.el --- Tiqsi Assembly mode support  -*- lexical-binding: t -*-

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

(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))

(add-to-list 'auto-mode-alist '("\\.S\\'" . gas-mode))


(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2 )
    (save-excursion
      (search-backward-regexp "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (search-forward-regexp "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq inputStr (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tempStr (replace-regexp-in-string "^0x" "" inputStr ))
      (setq tempStr (replace-regexp-in-string "^#x" "" tempStr ))
      (setq tempStr (replace-regexp-in-string "^#" "" tempStr ))
      )

    (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
    ))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun bin-string-to-int (8bit-string)
  "Convert 8BIT-STRING  string to integer."
  (let* ((list  '(128 64 32 16 8 4 2 1))
         (i   0)
         (int 0)
         )
    (while (< i 8)
      (if (not (string= "0" (substring 8bit-string i (1+ i))))
          (setq int (+ int (nth i list) )))
      (incf  i)
      )
    int
    ))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-bin-string (n &optional length)
  "Convert integer N to bit string (LENGTH, default 8)."
  (let* ((i    0)
         (len  (or length 8))
         (s    (make-string len ?0))
         )
    (while (< i len)
      (if (not (zerop (logand n (ash 1 i))))
          (aset s (- len (1+ i)) ?1))
      (setq i (1+ i))
      )
    s
    ))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-hex-string (n &optional separator pad)
  "Convert integer N to hex string. SEPARATOR between hunks is \"\".
PAD says to padd (bit hex string with leading zeroes."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
               (setq x (format "%x" (logand x 255)))
               (if (= 1 (length x)) (concat "0" x) x)))
   (list (ash n -24) (ash n -16) (ash n -8) n)
   separator))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-oct-string (n &optional separator)
  "Convert integer N into Octal. SEPARATOR between hunks is \"\"."
  (or separator
      (setq separator ""))
  (mapconcat
   (function (lambda (x)
               (setq x (format "%o" (logand x 511)))
               (if (= 1 (length x)) (concat "00" x)
                 (if (= 2 (length x)) (concat "0" x) x))))
   (list (ash n -27) (ash n -18) (ash n -9) n)
   separator))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun radix (str base)
  "Convert STR according to BASE."
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz")
        (case-fold-search t)
        (n 0)
        i)
    (mapcar '(lambda (c)
               (setq i (string-match (make-string 1 c) chars))
               (if (>= (or i 65536) base)
                   (error "%c illegal in base %d" c base))
               (setq n (+ (* n base) i)))
            (append str nil))
    n))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun bin-to-int (str)
  "Convert STR into binary."
  (radix str 2))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun oct-to-int (str)
  "Convert STR into octal."
  (radix str 8))

;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun hex-to-int (str)
  "Convert STR into hex."
  (if (string-match "\\`0x" str) (setq str (substring str 2)))
  (radix str 16))


;;; ----------------------------------------------------------------------
;;; 08 Jun 1997 Jamie Zawinski <jwz@netscape.com> comp.emacs
;;;
(defun int-to-net (float)
  "Decode packed FLOAT 32 bit IP addresses."
  (format "%d.%d.%d.%d"
          (truncate (% float 256))
          (truncate (% (/ float 256.0) 256))
          (truncate (% (/ float (* 256.0 256.0)) 256))
          (truncate (% (/ float (* 256.0 256.0 256.0)) 256))
          ))

;; for working with binary files List symbols in .so and .a files https://github.com/abo-abo/elf-mode
(use-package elf-mode
  :straight t
  :ensure t
  :mode (("\\.so\\'"  . elf-mode)
         ("\\.a\\'"   . elf-mode)))


                                        ;---{Keybindings}---;

;; 0x base converter -- find a good keybinding for 0xc-convert-point


;; Disaster CPP dissasemble on point

(define-key c-mode-base-map (kbd "C-c d") 'disaster)

(provide 'programming-assembly)

;;; programming-assembly.el ends here
