;;; modes-dired.el --- Tiqsi dired configuration

;;; Commentary:
;; 


;; FROM : https://github.com/howardabrams/dot-files/blob/master/emacs.org
;This enhancement to dired hides the ugly details until you hit \211(\222 and shows the details with \221 )\222. 
;I also change the [\205 ] to a simple asterisk.

;; (use-package dired-details
;;   :ensure t
;;   :init   (setq dired-details-hidden-string "* ")
;;   :config (dired-details-install))

(setq dired-details-hidden-string "* ")


; The ability to create a dired buffer based on searching for files in a directory tree with find-name-dired is fantastic  
(use-package find-dired
   :straight t
   :ensure t
   :init (setq find-ls-option '("-print0 | xargs -0 ls -od" . "-od")))


;The peep project allows you to preview files before loading them into a dedicated buffer:
; (use-package peep-dired
;   :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
;   :bind (:map dired-mode-map
;               ("P" . peep-dired)))   
;(use-package dired-x)


; (use-package dired-x
;   :config
;   (progn
; (setq dired-omit-verbose nil)
; ;; toggle `dired-omit-mode' with C-x M-o
; (add-hook 'dired-mode-hook #'dired-omit-mode)
; (setq dired-omit-files
;       (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))))

(provide 'modes-dired)

;;; modes-dired.el ends here
