;;; modes-shell.el --- Tiqsi shell buffer support  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>

;; URL: https://github.com/serialdev/tiqsi-emacs
;; Keywords: lisp
;; Version: 0
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


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


;; --------------------------------Create a frame with tooltip---------------------
;; TODO Make tip based on tip char len + height
(setq tip-frame-params
      '(
	(minibuffer . nil)
	(name . "*Tip Frame*")
	(lambda () (setq mode-line-format nil))
	(visibility . nil)
	(minibuffer-frame-alist nil)
	(vertical-scroll-bars . nil)
	(horizontal-scroll-bars . nil)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(line-spacing . 0)
	(unsplittable . t)
	(undecorated . t)
	(mouse-wheel-frame . nil)
	(no-other-frame . t)
	(cursor-type . nil)
	(inhibit-double-buffering . t)
	(drag-internal-border . t)
	(no-special-glyphs . t)
	(no-accept-focus . t)
	(no-focus-on-map . t)
	(internal-border-width . 1)
	(right-fringe . 0)
	(left-fringe . 0)
	(top . -1)
	(desktop-dont-save . t)
	(left . -1)))


(defun frame--set-input-focus (frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  (when (memq (window-system frame) '(x w32 ns))
    (x-focus-frame frame)))

(defun make-tip-frame (tip &rest args)
  (setq tip-frame (make-frame
		   (append (append tip-frame-params
				   `(
				     ;; (width . ,(+ (* (frame-char-width) (length tip)) (frame-char-width)))
				     (width . ,(+ (* (/ (frame-char-width) 2) (length tip)) (frame-char-width)))
				     ))
			   `(
			     ;; (height . 5)
			     ;; (height . ,(frame-char-height))
			     (height . ,(*(frame-char-height) 2))
			     ;; (height . ,(/ (frame-char-height) 2))
			     ))
		   )
	  )
    (generate-new-buffer "*Tip Frame Buffer*")


    (set-frame-position tip-frame
			(- (car (window-absolute-pixel-position)) (frame-char-width))
			(+ (cdr (window-absolute-pixel-position)) (frame-char-size)))

    (let ((current-frame (selected-frame) ))
      (make-frame-visible tip-frame)
      (select-frame tip-frame)
      (pop-to-buffer "*Tip Frame Buffer*")
      (with-current-buffer "*Tip Frame Buffer*"
	(fundamental-mode)
	(setq-local beacon-mode nil)
	(setq mode-line-format nil)
	;; (set-background-color "#5F55FF")
	(linum-mode -1)
	(insert  tip)
	)

      (frame--set-input-focus current-frame)
      (frame-restack current-frame tip-frame)
      )
    )



(defun close-tip-frame()
  (with-current-buffer "*Tip Frame Buffer*"
    (delete-region (point-min) (point-max)))
  (delete-frame tip-frame))

;; (make-tip-frame "whyssssssss")
;; (close-tip-frame)


;  -------------------------------------------------------------------------------- ;


(make-tip-frame "")
(async-shell-command "ls" "*Tip Frame Buffer*")

(defun tooltip-command (shell-command-to-execute)
  (while-no-input
    (let ((command (shell-command-to-string shell-command-to-execute) ))
      (make-tip-frame command)
      (sit-for 3)
      ))
  (close-tip-frame)
  )

(defmacro create-tooltip-command(command-name command-to-execute)
  (let ((current-command (s-prepend "command-tooltip-" command-name) ))
    `(defun ,(intern current-command) ()
       (interactive)
       (tooltip-command ,command-to-execute)
       )
  )
)

(create-tooltip-command "ls" "ls")
(create-tooltip-command "running-processes" "ps aux | wc -l")
(create-tooltip-command "display-uid" "cut -d ':' -f 1,3 /etc/passwd | sort -t ':' -k2n - | tr ':' '\t'")
(create-tooltip-command "process-memory" "ps aux | awk '{if ($5 != 0 ) print $2,$5,$6,$11}' | sort -k2n")
(create-tooltip-command "top-largest" "du -sk /var/log/* | sort -r -n | head -10")
(create-tooltip-command "count-all-files" "ls |xargs -n1 wc -l | sort -r -n")

(provide 'modes-shell)

;;; modes-shell.el ends here
