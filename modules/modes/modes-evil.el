;;; modes-evil.el --- Tiqsi Evil vim emulation support

;;; Commentary:
;; 


;-{Initialize evil}-;

;; (evil-mode 1)

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

(defhydra hydra-vim-core-mode (:color pink
                             :hint nil)
  "
                                                                                           ╭──────┐
 Move                          Jump                                                   	   │ Evil │
╭──────────────────────────────────────────────────────────────────────────────────────────┴──────╯
^^                         	
      ^_k_^       _H_      [_w_/_W_]: Forward Start     _f_: next <x>             _i_: Insert before cursor	   
      ^^↑^^       ^↑^      [_e_/_E_]: Forward End       _t_: before next <x>      _I_: Insert beg line		   	
  _h_ ←   → _l_   _M_      [_b_/_B_]: Backwards Start   _}_: next function/block  _a_: Append after cursor	   
      ^^↓^^       ^↓^      [_0_]: Start Line            _{_: prev function/block  _A_: Append at EOL		   	
      ^_j_^       _L_      [_^_]: 1st nb_char Line                               ^ ^                       _o_: Open /n below current line  	
                           [_$_]: End Line              _zz_: center cursor       _O_: Open /n above current line  	
                           [_g__]: Last nb_char Line    _Cb_: Move back 1 screen  _ea_: append at the end of word  
                           [_gg_]: First line Doc       _Cf_: Move fwd 1 screen   ^ ^ 				   	
                           [_G_]: Last line Doc         _Cd_: Move fwd 1/2 screen _r_: replace single char	    
			                                _Cu_: Move back 1/2 screen                                 
                                                                                                                   
			                                                                                           
"
;                                                Move                                               ;
  ;-------{Char}------;
  ("h" evil-backward-char :color red)
  ("l" evil-forward-char :color red)
  ("k" evil-previous-line :color red)
  ("j" evil-next-line :color red)
  ;------{Screen}-----;
  ("H" evil-join :color red)
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



  ("<escape>" nil "cancel" :color blue)
     
)



(defhydra hydra-vim-visual-mode (:color pink
                             :hint nil)
  "
^^  ^Movement^           ^Visual^            ^Edit^               ^Deletion^
^^^^^^^^-----------------------------------------------------------------
_h_: Imenu-Semantic      _b_: Check buffers  _o_: Occurrences     ^ ^ 
_e_: Etags               _U_: unmark up      _h_: Apropos         ^ ^
_d_: delete              ^ ^                 _i_: Imenu-Semantic  ^ ^
_d_: delete              ^ ^                 _i_: Imenu-Semantic  ^ ^
_d_: delete              ^ ^                 _i_: Imenu-Semantic  ^ ^
		         		        		  ^ ^
_D_: delete up           ^ ^                 _T_: files only: %   ^ ^ 
_<escape>_: exit
"
  ("h" evil-backward-char :color red)
  ("<escape>" push-mark-command :color blue)
  
)


(provide 'modes-evil)

;;; modes-evil.el ends here
