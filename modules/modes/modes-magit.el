;;; modes-magit.el --- Tiqsi Magit configuration

;;; Commentary:
;; 

(setq magit-completing-read-function 'ivy-completing-read)
(global-magit-file-mode t)


(use-package git-gutter-fringe
  :straight t
  :init
  (global-git-gutter-mode)
  (setq git-gutter-fr:side 'right-fringe))


;--{Push upstream}--;

(defun magit-push-arguments-maybe-upstream (magit-push-popup-fun &rest args)
  "Enable --set-upstream switch if there isn't a current upstream."
  (let ((magit-push-arguments
         (if (magit-get-remote) magit-push-arguments
           (cons "--set-upstream" magit-push-arguments))))
    (apply magit-push-popup-fun args)))
(advice-add 'magit-push-popup :around #'magit-push-arguments-maybe-upstream)

;;Combined with ido completion this allows pushing a new branch with P P RET:

;; NOTE: requires ido-completing-read+
(package-install 'ido-completing-read+)
(setq magit-completing-read-function #'magit-ido-completing-read)

(defun sdev/magit-stage-and-commit()
 (interactive)
 (magit-stage)
 (switch-to-buffer-other-window)
 (magit-commit-popup 'c)) 


;---{Keybindings}---;

(define-key magit-mode-map "\C-cc" 'magit-commit)
(define-key magit-mode-map "C-x M-g" 'magit-dispatch-popup)
;(define-key python-mode-map (kbd "C-c c") 'sdev/magit-stage-and-commit)

(provide 'modes-magit)

;;; modes-magit.el ends here
