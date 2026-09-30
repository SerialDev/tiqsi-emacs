;;; programming-xref.el --- Tiqsi go-to-definition and go-back  -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(require 'xref)
(require 'bind-key)
(straight-require 'dumb-jump)

(setq dumb-jump-prefer-searcher (if (executable-find "rg") 'rg 'grep)
  dumb-jump-selector 'completing-read)

(defun sdev--dumb-jump-backend ()
  (unless (or (bound-and-true-p tags-file-name) (bound-and-true-p tags-table-list))
    (dumb-jump-xref-activate)))

(add-hook 'xref-backend-functions #'sdev--dumb-jump-backend)

(advice-add 'dumb-jump-get-project-root :around
  (lambda (orig filepath)
    (let ((dumb-jump-default-project (file-name-directory (expand-file-name filepath))))
      (funcall orig filepath))))


(defun sdev--monroe-live-p ()
  (and (fboundp 'monroe-connection)
    (condition-case nil
      (process-live-p (monroe-connection))
      (error nil))))


(defun sdev--xref-find-definitions ()
  (let ((id (xref-backend-identifier-at-point (xref-find-backend))))
    (if id
      (xref-find-definitions id)
      (let ((this-command 'xref-find-definitions))
        (call-interactively 'xref-find-definitions)))))


(defun sdev/goto-definition ()
  (interactive)
  (cond
    ((minibufferp) (user-error "No definition lookup in the minibuffer"))
    ((and (derived-mode-p 'vterm-mode) (fboundp 'vterm-send-key))
      (vterm-send-key "." nil t))
    ((and (derived-mode-p 'clojure-mode) (fboundp 'monroe-jump) (sdev--monroe-live-p))
      (xref-push-marker-stack)
      (call-interactively 'monroe-jump))
    (t
      (condition-case err
        (sdev--xref-find-definitions)
        (error
          (if (eq (xref-find-backend) 'dumb-jump)
            (signal (car err) (cdr err))
            (let ((xref-backend-functions '(dumb-jump-xref-activate)))
              (condition-case nil
                (sdev--xref-find-definitions)
                (error (signal (car err) (cdr err)))))))))))


(defun sdev/go-back ()
  (interactive)
  (if (and (derived-mode-p 'vterm-mode) (fboundp 'vterm-send-key))
    (vterm-send-key "," nil t)
    (call-interactively 'xref-go-back)))


(bind-key* "M-." 'sdev/goto-definition)
(bind-key* "M-," 'sdev/go-back)

(with-eval-after-load 'evil
  (dolist (map (list evil-normal-state-map evil-motion-state-map))
    (define-key map (kbd "M-.") 'sdev/goto-definition)
    (define-key map (kbd "M-,") 'sdev/go-back)))


(provide 'programming-xref)

;;; programming-xref.el ends here
