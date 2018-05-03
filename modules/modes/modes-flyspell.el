;;; modes-flyspell.el --- Tiqsi flyspell support

;;; Commentary:
;; 


(use-package flyspell
  :straight t
  :init
  :defer t
  :diminish flyspell-mode)

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
	  (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
	       (eq pos flyspell-old-pos-error))
	  (progn
	    (if (= flyspell-old-pos-error min)
		;; goto beginning of buffer
		(progn
		  (message "Restarting from end of buffer")
		  (goto-char (point-max)))
	      (backward-word 1))
	    (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
		  (let ((ovs (overlays-at pos))
			(r '()))
		    (while (and (not r) (consp ovs))
		      (if (flyspell-overlay-p (car ovs))
			  (setq r t)
			(setq ovs (cdr ovs))))
		    (not r)))
	(backward-word 1)
	(setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
	  (progn
	    (message "No more miss-spelled word!")
	    (setq arg 0))
	(forward-word)))))

;------{Hydras}-----;

(defhydra hydra-flyspell
  (:color red
   :hint nil)
  "Flyspell"
  ("j" flyspell-goto-next-error            "Goto next error")
  ("k" flyspell-goto-previous-error        "Goto previous error")
  ("q" flyspell-auto-correct-previous-word "Correct last word")
  ("ESC" nil                               "Cancel"))

(provide 'modes-flyspell)

;;; modes-flyspell.el ends here
