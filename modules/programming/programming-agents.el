(let* ((checkout (expand-file-name "../agent-rig.el" tiqsi-root))
       (recipe '(agent-rig :type git :host github
                          :repo "SerialDev/agent-rig.el"
                          :branch "codex/native-agent-manager"
                          :files ("*.el"))))
  (when (file-exists-p (expand-file-name "agent-rig.el" checkout))
    (setq recipe (append recipe (list :local-repo checkout))))
  (straight-use-package recipe))

(dolist (command '(agent-rig agent-rig-start agent-rig-start-team agent-rig-send-region))
  (autoload command "agent-rig" nil t))

(global-set-key (kbd "C-c a") #'agent-rig)

(provide 'programming-agents)
