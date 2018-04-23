;;; modes-neotree.el --- Tiqsi neotree support

;;; Commentary:
;; 

;;; Code:

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(defun tiqsi-java-hook()

  (jdee-flycheck-enable-p)
  (neotree-show)
  )

(setq jdee-server-dir "/jdee-server/target/")

(add-hook 'jdee-mode-hook 'tiqsi-java-hook )

;---{Keybindings}---;

(define-key global-map (kbd "M-<f8>") 'neotree-toggle)


(provide 'modes-neotree)

;;; modes-neotree.el ends here
