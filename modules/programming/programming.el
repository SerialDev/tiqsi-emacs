;;; programming.el --- Tiqsi programming modes manager  -*- lexical-binding: t -*-

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

;; Maybe add an APL mode soon ^^

;;  _____________________________________________________________
;; |  ~   !⌶  @⍫  #⍒  $⍋  %⌽  ^⍉  &⊖  *⍟  (⍱  )⍲  _!  +⌹          ||
;; |                                                             ||
;; | `◊   1¨  2¯  3<  4≤  5=  6≥  7>  8≠  9∨  0∧  -×  =÷  BACKSP ||
;; |                                                              ||
;; |         Q   W⍹  E⋸  R   T⍨  Y¥  U   I⍸  O⍥  P⍣  {⍞  }⍬   |⊣ ||
;; | TAB     q?  w⍵  e∈  r⍴  t∼  y↑  u↓  i⍳  o○  p⋆  [←  ]→   \⊢ ||
;; |                                                              ||
;; | (CAPS    A⍶  S   D   F   G   H   J⍤  K   L⌷  :≡  "≢          ||
;; |  LOCK)   a⍺  s⌈  d⌊  f_  g∇  h∆  j∘  k'  l⎕  ;⍎  '⍕  RETURN ||
;; |                                                              ||
;; |             Z   Xχ  C¢  V   B£  N   M   <⍪  >⍙  ?⍠   ||
;; | SHIFT       z⊂  x⊃  c∩  v∪  b⊥  n⊤  m|  ,⍝  .⍀  /⌿   SHIFT  ||
;; |_____________________________________________________________||
;;  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;;


(load-expand  "modules/programming/utils-parsec.el")

(load-expand  "modules/programming/programming-bnfc.el")
(load-expand  "modules/programming/programming-assembly.el")
(load-expand  "modules/programming/programming-c.el")
(load-expand  "modules/programming/programming-rust.el")
(load-expand  "modules/programming/programming-python.el")
;; (load-expand  "modules/programming/programming-scala.el")
(load-expand  "modules/programming/programming-clojure.el")
(load-expand  "modules/programming/programming-text.el")
(load-expand  "modules/programming/programming-lisp.el")
(load-expand  "modules/programming/programming-java.el")
(load-expand  "modules/programming/programming-go.el")
(load-expand  "modules/programming/programming-elm.el")
(load-expand  "modules/programming/programming-nim.el")
(load-expand  "modules/programming/programming-typescript.el")
(load-expand  "modules/programming/programming-cfsharp.el")
(load-expand  "modules/programming/programming-terraform.el")
(load-expand  "modules/programming/programming-zig.el")
(load-expand  "modules/programming/programming-jai.el")
(load-expand  "modules/programming/programming-llm.el")

(provide 'programming)

;;; programming.el ends here
