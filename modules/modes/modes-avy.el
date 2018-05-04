;;; modes-avy.el --- Tiqsi avy configuration

;;; Commentary:
;;

(defun count-unique-visible-buffers (&optional frame)
  "Count how many buffers are currently being shown.  Defaults to
selected frame."
  (length (cl-delete-duplicates (mapcar #'window-buffer (window-list frame)))))

(defun sdev/jump-window (&optional frame)
  (interactive)
    (if
	(> (count-unique-visible-buffers) 4)
	(call-interactively #'ace-window)
    (call-interactively #'other-window)))



;---{keybindings}---;

(global-set-key (kbd "M-w") 'sdev/jump-window)
(global-set-key (kbd "C-c jj") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-c jw") 'ace-window)
(global-set-key (kbd "C-c js") 'ace-swap-window)


(provide 'modes-avy)

;;; modes-avy.el ends here
