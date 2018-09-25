;;; modes-evil.el --- Tiqsi Evil vim emulation support  -*- lexical-binding: t -*-

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


                                        ;-{Initialize evil}-;

;; (evil-mode 1)
(require 'evil)
                                        ;-----{HYDRAS!}-----;

;;                                                                         ╭──────┐
;;  Navigation   Other  Sources     Mark             Do             Help   │ Helm │
;; ╭───────────────────────────────────────────────────────────────────────┴──────╯
;;         _p_   [_m_] mark         [_v_] view         [_H_] helm help
;;         ^↑^   [_t_] toggle all   [_d_] delete       [_s_] source help
;;         ^ ^   [_u_] unmark all   [_f_] follow: %(helm-attr 'follow)
;;         ^↓^    ^ ^               [_y_] yank selection
;;         _n_    ^ ^               [_w_] toggle windows
;; --------------------------------------------------------------------------------

(defhydra hydra-vim-move-mode (:color pink
                                      :hint nil)
  "
                                                                                           ╭──────┐
 Move              ^ ^        Jump      ^ ^        ^ ^                                     │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
     ^_k_^         _H_     [_w_/_W_]: Forward Start   [_$_]: End Line
     ^^↑^^         ^↑^     [_e_/_E_]: Forward End     [_g\__]: Last char Line [bug]
 _h_ ←   → _l_     _M_     [_b_/_B_]: Backwards Start [_gg_]: 1st line Doc
     ^^↓^^         ^↓^     [_0_]: Start Line          [_G_]: Last line Doc
     ^_j_^         _L_     [_\\^_]: 1st char-Line     [_<up>_/_<down>_/_<left>_/_<right>_: move window

"
                                        ;                                                Move                                               ;
                                        ;---{Window move}---;
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
                                        ;-------{Char}------;
  ("h" evil-backward-char :color red)
  ("l" evil-forward-char :color red)
  ("k" evil-previous-line :color red)
  ("j" evil-next-line :color red)
                                        ;------{Screen}-----;
  ("H" evil-window-top :color red)
  ("M" evil-window-middle :color red)
  ("L" evil-window-bottom :color red)
                                        ;                                                Jump                                               ;

                                        ;-------{word}------;
  ("w" evil-forward-word-begin :color red)
  ("W" evil-forward-WORD-begin :color red)
  ("e" evil-forward-word-end :color red)
  ("E" evil-forward-WORD-end :color red)
  ("b" evil-backward-word-begin :color red)
  ("B" evil-backward-WORD-begin :color red)
                                        ;-------{Line}------;
  ("0" evil-digit-argument-or-evil-beginning-of-line :color red)
  ("^" evil-first-non-blank :color red)
  ("$" evil-end-of-line :color red)
  ("g_" evil-last-non-blank :color red)
                                        ;-------{Doc}-------;
  ("gg" evil-goto-first-line :color red)
  ("G" evil-goto-line :color red)


                                        ;                                                Find                                               ;
                                        ;-------{Find}------;
  ("f" evil-find-char :color red)
  ("F" evil-find-char-backward :color red)
  ("t" evil-find-char-to :color red)
  ("T" evil-find-char-to-backward :color red)
  ("}" evil-forward-paragraph :color red)
  ("{" evil-backward-paragraph :color red)
  ("zz" evil-scroll-line-to-center :color red)
  ("C-u" evil-scroll-up :color red)
  ("C-b" evil-scroll-page-up :color red)
  ("C-f" evil-scroll-page-down :color red)
  ("C-d" evil-scroll-down :color red)
  (";" evil-repeat-find-char :color red)
  ("," evil-repeat-find-char-reverse :color red)

                                        ;                                               Insert                                              ;
                                        ;------{evil}-----;
  ;; ("i" evil-insert :color red)
  ;; ("I" evil-insert-line :color red)
  ;; ("a" evil-append :color red)
  ;; ("A" evil-append-line :color red)
  ;; ("o" evil-open-below :color red)
  ;; ("O" evil-open-above :color red)
  ;; ("ea" evil-forward-word-end :color red)
                                        ;------{Insert}-----;
  ("i" nil :color blue)
  ("I" sk/smarter-move-beginning-of-line :color blue)
  ("a" evil-forward-char :color blue)
  ("A" move-end-of-line :color blue)
  ("o" open-line-below :color blue)
  ("O" open-line-above :color blue)
  ;; ("ea" evil-forward-word-end :color blue)
  ("/" evil-search-forward :color blue)
  ("?" evil-search-backward :color blue)
  ("n" evil-search-next :color red)
  ("N" evil-search-previous :color red)

                                        ;                                                Edit                                               ;
                                        ;------{edit}-----;
  ("r" evil-replace :color red)
  ("J" evil-join :color red)
  ("s" evil-substitute :color red)
  ("S" evil-change-whole-line :color red)
  ("cc" evil-change-whole-line :color red)
  ("." evil-repeat :color red)
  ("u" undo-tree-undo :color red)
  ("C-r" undo-tree-redo :color red)

  ("<f1>" hydra-vim-move-mode/body "Hydra Mov" :color blue)
  ("<f2>" hydra-vim-find-mode/body "Hydra Mov" :color blue)
  ("<f3>" hydra-vim-insert-mode/body "Hydra Insert" :color blue)
  ("<f4>" hydra-vim-edit-mode/body "Hydra Edit" :color blue)
  ("<escape>" nil "cancel" :color blue)
  )


(defhydra hydra-vim-find-mode (:color pink
                                      :hint nil)
  "
                                                                                           ╭──────┐
 Find in Buffer                                                                            │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
_f_/_F_: next/prev <x>               _C-u_: Move back 1/2 screen
_t_/_T_: before next/prev <x>        _C-b_: Move back 1 screen
_}_: next function/block    _C-f_: Move fwd 1 screen
_{_: prev function/block    _C-d_: Move fwd 1/2 screen
_zz_: center cursor         _;_/_,_: Repeat f,t,F,T fwd/back

"
                                        ;                                                Move                                               ;
                                        ;---{Window move}---;
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
                                        ;-------{Char}------;
  ("h" evil-backward-char :color red)
  ("l" evil-forward-char :color red)
  ("k" evil-previous-line :color red)
  ("j" evil-next-line :color red)
                                        ;------{Screen}-----;
  ("H" evil-window-top :color red)
  ("M" evil-window-middle :color red)
  ("L" evil-window-bottom :color red)
                                        ;                                                Jump                                               ;

                                        ;-------{word}------;
  ("w" evil-forward-word-begin :color red)
  ("W" evil-forward-WORD-begin :color red)
  ("e" evil-forward-word-end :color red)
  ("E" evil-forward-WORD-end :color red)
  ("b" evil-backward-word-begin :color red)
  ("B" evil-backward-WORD-begin :color red)
                                        ;-------{Line}------;
  ("0" evil-digit-argument-or-evil-beginning-of-line :color red)
  ("^" evil-first-non-blank :color red)
  ("$" evil-end-of-line :color red)
  ("g_" evil-last-non-blank :color red)
                                        ;-------{Doc}-------;
  ("gg" evil-goto-first-line :color red)
  ("G" evil-goto-line :color red)

                                        ;                                                Find                                               ;
                                        ;-------{Find}------;
  ("f" evil-find-char :color red)
  ("F" evil-find-char-backward :color red)
  ("t" evil-find-char-to :color red)
  ("T" evil-find-char-to-backward :color red)
  ("}" evil-forward-paragraph :color red)
  ("{" evil-backward-paragraph :color red)
  ("zz" evil-scroll-line-to-center :color red)
  ("C-u" evil-scroll-up :color red)
  ("C-b" evil-scroll-page-up :color red)
  ("C-f" evil-scroll-page-down :color red)
  ("C-d" evil-scroll-down :color red)
  (";" evil-repeat-find-char :color red)
  ("," evil-repeat-find-char-reverse :color red)

                                        ;                                               Insert                                              ;
                                        ;------{evil}-----;
  ;; ("i" evil-insert :color red)
  ;; ("I" evil-insert-line :color red)
  ;; ("a" evil-append :color red)
  ;; ("A" evil-append-line :color red)
  ;; ("o" evil-open-below :color red)
  ;; ("O" evil-open-above :color red)
  ;; ("ea" evil-forward-word-end :color red)
                                        ;------{Insert}-----;
  ("i" nil :color blue)
  ("I" sk/smarter-move-beginning-of-line :color blue)
  ("a" evil-forward-char :color blue)
  ("A" move-end-of-line :color blue)
  ("o" open-line-below :color blue)
  ("O" open-line-above :color blue)
  ;; ("ea" evil-forward-word-end :color blue)
  ("/" evil-search-forward :color blue)
  ("?" evil-search-backward :color blue)
  ("n" evil-search-next :color red)
  ("N" evil-search-previous :color red)

                                        ;                                                Edit                                               ;
                                        ;------{edit}-----;
  ("r" evil-replace :color red)
  ("J" evil-join :color red)
  ("s" evil-substitute :color red)
  ("S" evil-change-whole-line :color red)
  ("cc" evil-change-whole-line :color red)
  ("." evil-repeat :color red)
  ("u" undo-tree-undo :color red)
  ("C-r" undo-tree-redo :color red)

  ("<f1>" hydra-vim-move-mode/body "Hydra Mov" :color blue)
  ("<f2>" hydra-vim-find-mode/body "Hydra Mov" :color blue)
  ("<f3>" hydra-vim-insert-mode/body "Hydra Insert" :color blue)
  ("<f4>" hydra-vim-edit-mode/body "Hydra Edit" :color blue)
  ("<escape>" nil "cancel" :color blue)
  )

(defhydra hydra-vim-insert-mode (:color pink
                                        :hint nil)
  "
                                                                                           ╭──────┐
 Move              ^ ^        Jump      ^ ^                                                │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
_i_: Insert before cursor       _O_: Open /n above
_I_: Insert beg line            _ea_: append at the end of word [bug]
_a_: Append after cursor        _/_: Search Forward
_A_: Append at EOL              _?_: Search Backward
_o_: Open /n below              _n_/_N_: Repeat search fwd/back

"
                                        ;                                                Move                                               ;
                                        ;---{Window move}---;
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
                                        ;-------{Char}------;
  ("h" evil-backward-char :color red)
  ("l" evil-forward-char :color red)
  ("k" evil-previous-line :color red)
  ("j" evil-next-line :color red)
                                        ;------{Screen}-----;
  ("H" evil-window-top :color red)
  ("M" evil-window-middle :color red)
  ("L" evil-window-bottom :color red)
                                        ;                                                Jump                                               ;

                                        ;-------{word}------;
  ("w" evil-forward-word-begin :color red)
  ("W" evil-forward-WORD-begin :color red)
  ("e" evil-forward-word-end :color red)
  ("E" evil-forward-WORD-end :color red)
  ("b" evil-backward-word-begin :color red)
  ("B" evil-backward-WORD-begin :color red)
                                        ;-------{Line}------;
  ("0" evil-digit-argument-or-evil-beginning-of-line :color red)
  ("^" evil-first-non-blank :color red)
  ("$" evil-end-of-line :color red)
  ("g_" evil-last-non-blank :color red)
                                        ;-------{Doc}-------;
  ("gg" evil-goto-first-line :color red)
  ("G" evil-goto-line :color red)

                                        ;                                                Find                                               ;
                                        ;-------{Find}------;
  ("f" evil-find-char :color red)
  ("F" evil-find-char-backward :color red)
  ("t" evil-find-char-to :color red)
  ("T" evil-find-char-to-backward :color red)
  ("}" evil-forward-paragraph :color red)
  ("{" evil-backward-paragraph :color red)
  ("zz" evil-scroll-line-to-center :color red)
  ("C-u" evil-scroll-up :color red)
  ("C-b" evil-scroll-page-up :color red)
  ("C-f" evil-scroll-page-down :color red)
  ("C-d" evil-scroll-down :color red)
  (";" evil-repeat-find-char :color red)
  ("," evil-repeat-find-char-reverse :color red)

                                        ;                                               Insert                                              ;
                                        ;------{evil}-----;
  ;; ("i" evil-insert :color red)
  ;; ("I" evil-insert-line :color red)
  ;; ("a" evil-append :color red)
  ;; ("A" evil-append-line :color red)
  ;; ("o" evil-open-below :color red)
  ;; ("O" evil-open-above :color red)
  ;; ("ea" evil-forward-word-end :color red)
                                        ;------{Insert}-----;
  ("i" nil :color blue)
  ("I" sk/smarter-move-beginning-of-line :color blue)
  ("a" evil-forward-char :color blue)
  ("A" move-end-of-line :color blue)
  ("o" open-line-below :color blue)
  ("O" open-line-above :color blue)
  ;; ("ea" evil-forward-word-end :color blue)
  ("/" evil-search-forward :color blue)
  ("?" evil-search-backward :color blue)
  ("n" evil-search-next :color red)
  ("N" evil-search-previous :color red)

                                        ;                                                Edit                                               ;
                                        ;------{edit}-----;
  ("r" evil-replace :color red)
  ("J" evil-join :color red)
  ("s" evil-substitute :color red)
  ("S" evil-change-whole-line :color red)
  ("cc" evil-change-whole-line :color red)
  ("." evil-repeat :color red)
  ("u" undo-tree-undo :color red)
  ("C-r" undo-tree-redo :color red)

  ("<f1>" hydra-vim-move-mode/body "Hydra Mov" :color blue)
  ("<f2>" hydra-vim-find-mode/body "Hydra Mov" :color blue)
  ("<f3>" hydra-vim-insert-mode/body "Hydra Insert" :color blue)
  ("<f4>" hydra-vim-edit-mode/body "Hydra Edit" :color blue)
  ("<escape>" nil "cancel" :color blue)
  )


(defhydra hydra-vim-edit-mode (:color pink
                                      :hint nil)
  "
                                                                                           ╭──────┐
 Move              ^ ^        Jump      ^ ^                                                │ Ukhu │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^
_r_: Replace one char            _u_: Undo
_J_: Join line below             _C-r_: Redo
_s_: Delete char & replace       ^ ^
_S_/_cc_: Delete line & replace  ^ ^
_._: Repeat last command         ^ ^

"
                                        ;                                                Move                                               ;
                                        ;---{Window move}---;
  ("<up>" windmove-up)
  ("<down>" windmove-down)
  ("<left>" windmove-left)
  ("<right>" windmove-right)
                                        ;-------{Char}------;
  ("h" evil-backward-char :color red)
  ("l" evil-forward-char :color red)
  ("k" evil-previous-line :color red)
  ("j" evil-next-line :color red)
                                        ;------{Screen}-----;
  ("H" evil-window-top :color red)
  ("M" evil-window-middle :color red)
  ("L" evil-window-bottom :color red)
                                        ;                                                Jump                                               ;

                                        ;-------{word}------;
  ("w" evil-forward-word-begin :color red)
  ("W" evil-forward-WORD-begin :color red)
  ("e" evil-forward-word-end :color red)
  ("E" evil-forward-WORD-end :color red)
  ("b" evil-backward-word-begin :color red)
  ("B" evil-backward-WORD-begin :color red)
                                        ;-------{Line}------;
  ("0" evil-digit-argument-or-evil-beginning-of-line :color red)
  ("^" evil-first-non-blank :color red)
  ("$" evil-end-of-line :color red)
  ("g_" evil-last-non-blank :color red)
                                        ;-------{Doc}-------;
  ("gg" evil-goto-first-line :color red)
  ("G" evil-goto-line :color red)


                                        ;                                                Find                                               ;
                                        ;-------{Find}------;
  ("f" evil-find-char :color red)
  ("F" evil-find-char-backward :color red)
  ("t" evil-find-char-to :color red)
  ("T" evil-find-char-to-backward :color red)
  ("}" evil-forward-paragraph :color red)
  ("{" evil-backward-paragraph :color red)
  ("zz" evil-scroll-line-to-center :color red)
  ("C-u" evil-scroll-up :color red)
  ("C-b" evil-scroll-page-up :color red)
  ("C-f" evil-scroll-page-down :color red)
  ("C-d" evil-scroll-down :color red)
  (";" evil-repeat-find-char :color red)
  ("," evil-repeat-find-char-reverse :color red)

                                        ;                                               Insert                                              ;
                                        ;------{evil}-----;
  ;; ("i" evil-insert :color red)
  ;; ("I" evil-insert-line :color red)
  ;; ("a" evil-append :color red)
  ;; ("A" evil-append-line :color red)
  ;; ("o" evil-open-below :color red)
  ;; ("O" evil-open-above :color red)
  ;; ("ea" evil-forward-word-end :color red)
                                        ;------{Insert}-----;
  ("i" nil :color blue)
  ("I" sk/smarter-move-beginning-of-line :color blue)
  ("a" evil-forward-char :color blue)
  ("A" move-end-of-line :color blue)
  ("o" open-line-below :color blue)
  ("O" open-line-above :color blue)
  ;; ("ea" evil-forward-word-end :color blue)
  ("/" evil-search-forward :color blue)
  ("?" evil-search-backward :color blue)
  ("n" evil-search-next :color red)
  ("N" evil-search-previous :color red)

                                        ;                                                Edit                                               ;
                                        ;------{edit}-----;
  ("r" evil-replace :color red)
  ("J" evil-join :color red)
  ("s" evil-substitute :color red)
  ("S" evil-change-whole-line :color red)
  ("cc" evil-change-whole-line :color red)
  ("." evil-repeat :color red)
  ("u" undo-tree-undo :color red)
  ("C-r" undo-tree-redo :color red)

  ("<f1>" hydra-vim-move-mode/body "Hydra Mov" :color blue)
  ("<f2>" hydra-vim-find-mode/body "Hydra Mov" :color blue)
  ("<f3>" hydra-vim-insert-mode/body "Hydra Insert" :color blue)
  ("<f4>" hydra-vim-edit-mode/body "Hydra Edit" :color blue)
  ("<escape>" nil "cancel" :color blue)
  )

(global-set-key (kbd "<escape>") 'hydra-vim-move-mode/body)



(provide 'modes-evil)

;;; modes-evil.el ends here
