;;; modes-ctags.el --- Tiqsi Ctags Rtags and tagging navigation support

;;; Commentary:
;;  Etags support for Ctags/Rtags Backend
;;  ctags -e -R --extras=+fq --exclude=db --exclude=test --exclude=.git --exclude=.ipynb --exclude=public -f TAGS



(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built successfully"))

(defun visit-project-tags ()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun sdev/find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))


;---{Auto update}---;

(ctags-global-auto-update-mode)
(setq ctags-update-prompt-create-tags nil);you need manually create TAGS in your project
 ;; or only turn it on for some special mode

(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'python-mode-common-hook  'turn-on-ctags-auto-update-mode)
(add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode)

;; Support Multiple TAG files
;; ;; (setq tags-table-list '("/path/of/TAGS1"    "/path/of/TAG2"))

;---{Keybindings}---;
(global-set-key (kbd "M-.") 'my-find-tag)

;;list all visited tags
(global-set-key "\M-*" 'helm-etags-plus-history)
;;go back directly rebinded instead of x-ref-pop-marker-stack
(global-set-key "\M-," 'helm-etags-plus-history-go-back)
;;go forward directly
(global-set-key "\M-/" 'helm-etags-plus-history-go-forward)

(global-set-key "\M-." 'helm-etags-plus-select)


(provide 'modes-ctags)

;;; modes-ctags.el ends here
