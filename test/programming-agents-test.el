;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'hydra)
(defvar tiqsi-root (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar tiqsi-test-agent-recipe nil)
(defun straight-use-package (recipe) (setq tiqsi-test-agent-recipe recipe))
(load (expand-file-name "modules/programming/programming-agents.el" tiqsi-root) nil t)

(ert-deftest tiqsi-agents-menu-autoloads-every-action ()
  (should (eq (key-binding (kbd "C-c a")) #'agent-rig))
  (should (eq (key-binding (kbd "C-c A")) #'hydra-agents/body))
  (should (commandp 'hydra-agents/body))
  (dolist (command '(agent-rig agent-rig-start agent-rig-start-team agent-rig-switch
                    agent-rig-send agent-rig-send-region agent-rig-send-buffer
                    agent-rig-send-diff agent-rig-broadcast agent-rig-capture
                    agent-rig-return-to-code agent-rig-help))
    (should (commandp command))
    (when (autoloadp (symbol-function command))
      (autoload-do-load (symbol-function command) command))
    (should (commandp command)))
  (should (eq (lookup-key agent-rig-terminal-map (kbd "C-c A")) #'hydra-agents/body)))

(ert-deftest tiqsi-agents-straight-recipe-resolves-package-modules ()
  (should (eq (car tiqsi-test-agent-recipe) 'agent-rig))
  (should (equal (plist-get (cdr tiqsi-test-agent-recipe) :repo) "SerialDev/agent-rig.el"))
  (dolist (feature '(agent-rig agent-rig-providers agent-rig-tmux))
    (should (locate-library (symbol-name feature)))))
