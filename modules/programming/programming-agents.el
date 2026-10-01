(let* ((checkout (expand-file-name "../agent-rig.el" tiqsi-root))
       (recipe '(agent-rig :type git :host github
                          :repo "SerialDev/agent-rig.el"
                          :branch "codex/native-agent-manager"
                          :files ("*.el"))))
  (when (file-exists-p (expand-file-name "agent-rig.el" checkout))
    (setq recipe (append recipe (list :local-repo checkout))))
  (straight-use-package recipe))

(require 'hydra)

(dolist (command '(agent-rig agent-rig-start agent-rig-start-team agent-rig-switch
                  agent-rig-send agent-rig-send-region agent-rig-send-buffer
                  agent-rig-send-diff agent-rig-broadcast agent-rig-capture
                  agent-rig-return-to-code agent-rig-help agent-rig-save
                  agent-rig-restore agent-rig-set-conversation agent-rig-handoff
                  agent-rig-worktree-start agent-rig-adopt agent-rig-toggle-focus))
  (autoload command "agent-rig" nil t))

(defhydra hydra-agents (:color blue :hint nil)
  "
Agents: _a_ dashboard  _n_ new  _t_ team  _s_ switch
Context: _p_ prompt  _r_ region  _f_ buffer  _d_ diff  _b_ broadcast
Navigate: _o_ output  _c_ code  _h_ help  _q_ quit
Recovery: _S_ save seats  _R_ restore  _i_ conversation ID
Workspace: _w_ worktree  _A_ adopt  _H_ handoff  _F_ focus
"
  ("a" agent-rig)
  ("n" agent-rig-start)
  ("t" agent-rig-start-team)
  ("s" agent-rig-switch)
  ("p" agent-rig-send)
  ("r" agent-rig-send-region)
  ("f" agent-rig-send-buffer)
  ("d" agent-rig-send-diff)
  ("b" agent-rig-broadcast)
  ("o" agent-rig-capture)
  ("c" agent-rig-return-to-code)
  ("h" agent-rig-help)
  ("S" agent-rig-save)
  ("R" agent-rig-restore)
  ("i" agent-rig-set-conversation)
  ("w" agent-rig-worktree-start)
  ("A" agent-rig-adopt)
  ("H" agent-rig-handoff)
  ("F" agent-rig-toggle-focus)
  ("q" nil))

(with-eval-after-load 'agent-rig
  (define-key agent-rig-terminal-map (kbd "C-c A") #'hydra-agents/body)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'agent-rig-mode 'emacs)
    (evil-set-initial-state 'agent-rig-actions-mode 'emacs)
    (evil-set-initial-state 'agent-rig-prompt-mode 'insert)))

(global-set-key (kbd "C-c a") #'agent-rig)
(global-set-key (kbd "C-c A") #'hydra-agents/body)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c a" "agent dashboard" "C-c A" "agent menu"))

(provide 'programming-agents)
