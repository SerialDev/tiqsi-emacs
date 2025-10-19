;;; test-hydra-syntax.el --- Test hydra syntax

;; Test if hydra definition has syntax errors

;; First, let's define a minimal hydra with proper syntax
(defhydra hydra-claude-test (:color pink :hint nil)
  "
Test Hydra
  _c_: Start   _q_: Quit
"
  ("c" (lambda () (interactive) (message "Start")) "Start")
  ("q" nil "Quit" :exit t))

;; Now let's check the original hydra - there might be an issue with the body
;; The issue might be in the nested hydra call
(defhydra hydra-claude-modes (:color blue :hint nil)
  "Claude Modes"
  ("q" nil "quit"))

;; Fixed version of the main hydra
(defhydra hydra-claude-fixed (:color pink :hint nil)
  "
╭──────────────────────────────────────────────────────────╮
│                 Tiqsi Claude REPL Assistant              │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Session      │  Send Content     │  AI Features        │
│  ──────────   │  ─────────────    │  ─────────────      │
│  _c_: Start   │  _r_: Region      │  _e_: Fix Error     │
│  _k_: Kill    │  _f_: Function    │  _o_: Optimize      │
│  _l_: List    │  _b_: Buffer      │  _x_: Explain       │
│  _t_: Toggle  │  _a_: Ask         │  _T_: Tests         │
│  _C_: Clear   │  _s_: Paragraph   │                     │
│               │                   │  Settings           │
│               │                   │  _m_: Modes menu    │
│               │                   │  _M_: Cycle perms   │
│                                                          │
│  _q_: Quit    │  _h_: Help                              │
╰──────────────────────────────────────────────────────────╯
"
  ("c" (lambda () (interactive) (message "Start Claude")) "Start Claude")
  ("k" (lambda () (interactive) (message "Kill Session")) "Kill Session")
  ("l" (lambda () (interactive) (message "List Sessions")) "List Sessions")
  ("t" (lambda () (interactive) (message "Toggle Window")) "Toggle Window")
  ("C" (lambda () (interactive) (message "Clear Buffer")) "Clear Buffer")
  ("r" (lambda () (interactive) (message "Send Region")) "Send Region")
  ("f" (lambda () (interactive) (message "Send Function")) "Send Function")
  ("b" (lambda () (interactive) (message "Send Buffer")) "Send Buffer")
  ("a" (lambda () (interactive) (message "Ask Question")) "Ask Question")
  ("s" (lambda () (interactive) (message "Send Paragraph")) "Send Paragraph")
  ("e" (lambda () (interactive) (message "Fix Error")) "Fix Error")
  ("o" (lambda () (interactive) (message "Optimize Code")) "Optimize Code")
  ("x" (lambda () (interactive) (message "Explain Code")) "Explain Code")
  ("T" (lambda () (interactive) (message "Generate Tests")) "Generate Tests")
  ("m" hydra-claude-modes/body "Modes" :exit t)
  ("M" (lambda () (interactive) (message "Cycle Perms")) "Cycle Perms")
  ("h" (lambda () (interactive) 
         (message "Claude REPL: Enhanced with history, syntax highlighting, and more!"))
       "Help" :exit t)
  ("q" nil "Quit" :exit t))

(provide 'test-hydra-syntax)
;;; test-hydra-syntax.el ends here