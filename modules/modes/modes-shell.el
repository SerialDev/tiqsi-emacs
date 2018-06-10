;;; modes-shell.el --- Tiqsi shell buffer support

;;; Commentary:
;;

;;; Code:

(defun send-to-shell(command-string)
  (shell)
  (with-current-buffer "*shell*"
    (let ((process (get-buffer-process (current-buffer)))
          )
      (unless process
        (error "No process in %s" buffer-or-name))
      (goto-char (process-mark process))
      (insert command-string)
      (comint-send-input nil t )
        )))

(provide 'modes-shell)

;;; modes-shell.el ends here
