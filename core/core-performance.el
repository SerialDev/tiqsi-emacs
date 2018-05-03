;;; core-performance.el --- Tiqsi core performance utilities

;;; Commentary:
;; Buffer/Line/Usability performance
;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers

;;; Code:


(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq gc-cons-threshold 20000000)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 20000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq-default bidi-display-reordering nil)
(setq redisplay-dont-pause t)
(setq togle-truncate-lines t )

(defun toggle-truncate-lines ()
"Toggle whether to wrap lines at right window border."
(interactive)
   (if (eq truncate-lines nil)
     (set-variable 'truncate-lines 't)
     (set-variable 'truncate-lines nil)
     ) )

(defun toggle-line-spacing ()
"Toggle line spacing between no extra space to extra half line height."
(interactive)
(if (eq line-spacing nil)
    (setq-default line-spacing 0.5) ; add 0.5 height between lines
  (setq-default line-spacing nil)   ; no extra heigh between lines
  ))


(when (require-soft 'jit-lock)    ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1)) ; new with emacs21


(global-disable-mode 'async-bytecomp-package-mode)
(global-disable-mode 'auto-composition-mode)
(global-disable-mode 'auto-compression-mode)
(global-disable-mode 'auto-encryption-mode)
(global-disable-mode 'diff-auto-refine-mode)
(global-disable-mode 'eldoc-mode)
(global-disable-mode 'global-eldoc-mode)
(global-disable-mode 'global-git-commit-mode)
(global-disable-mode 'global-magit-file-mode)
(global-disable-mode 'line-number-mode)
(global-disable-mode 'shell-dirtrack-mode)

(provide 'core-performance)

;;; core-performance.el ends here
