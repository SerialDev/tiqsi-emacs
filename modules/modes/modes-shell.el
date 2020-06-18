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



; ------------------------------------------------------------------------- ;


(multiple-async-shell-commands "*Output*"
			       "echo 1; sleep 1"
			       "echo 2; sleep 1"
			       "echo 2; sleep 1"
			       "echo 3; sleep 1")


; ------------------------------------------------------------------------- ;


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
		   (append (append
			    tip-frame-params
			    `((width . ,(+ (* (/ (frame-char-width) 2) (length tip)) (frame-char-width)))))
			   `((height . ,(*(frame-char-height) 2))))))

    ;; (generate-new-buffer "*Tip Frame Buffer*")

    (set-frame-position tip-frame
			(- (car (window-absolute-pixel-position)) (frame-char-width))
			(+ (cdr (window-absolute-pixel-position)) (frame-char-size)))


    (let ((current-frame (selected-frame) ))
      (make-frame-visible tip-frame)
      (select-frame tip-frame)
      (pop-to-buffer "*Tip Frame*")
      (with-current-buffer "*Tip Frame*"
	(fundamental-mode)
	(setq-local beacon-mode nil)
	(setq mode-line-format nil)
	(set-background-color "#5F55FF")
	(linum-mode -1)
	(insert  tip)
	)
      (frame--set-input-focus current-frame)
      (frame-restack current-frame tip-frame)
      )
    )



(defun close-tip-frame()
  (with-current-buffer "*Tip Frame*"
    (delete-region (point-min) (point-max)))
  (delete-frame tip-frame))

;; (make-tip-frame "whyssssssss")
;; (close-tip-frame)


;  -------------------------------------------------------------------------------- ;


;; (make-tip-frame "e")
;; (async-shell-command "ls" "*Tip Frame Buffer*")

(defun tooltip-command (shell-command-to-execute)
  (while-no-input
    (let ((command (shell-command-to-string shell-command-to-execute) ))
      (make-tip-frame command)
      (sit-for 3)
      ))
  (close-tip-frame)
  )

; ------------------------------------------------------------------------- ;
;                           TODO: Make this async                           ;
; ------------------------------------------------------------------------- ;

(defmacro create-tooltip-command(command-name command-to-execute)
  (let ((current-command (s-prepend "command-tooltip-" command-name) ))
    `(defun ,(intern current-command) ()
       (interactive)
       (pos-tip-show (shell-command-to-string ,command-to-execute))
       )
  )
)



(straight-use-package
 '(extractor
   :type git
   :host github
   :ensure t
   :repo "DamienCassou/extractor-el"
))


(straight-use-package
 '(etop-mode
   :type git
   :host github
   :ensure t
   :repo "DamienCassou/etop-mode"
))


(when-executable
 "nmap"
 (progn
   ;; Which ports are listening for TCP connections from the network
   ;; #notice that some companies might not like you using nmap
   (create-tooltip-command "ports-listening-tcp" "nmap -sT -O localhost")
))

(create-tooltip-command "ls" "ls")
(create-tooltip-command "count-running-processes" "ps aux | wc -l")
(create-tooltip-command "display-uid" "cut -d ':' -f 1,3 /etc/passwd | sort -t ':' -k2n - | tr ':' '\t'")
(create-tooltip-command "process-memory" "ps aux | awk '{if ($5 != 0 ) print $2,$5,$6,$11}' | sort -k2n")
(create-tooltip-command "top-largest" "du -sk /var/log/* | sort -r -n | head -10")
(create-tooltip-command "count-all-files" "ls |xargs -n1 wc -l | sort -r -n")
(create-tooltip-command "show-memory-usage" "free -c 1 -mhs 1")
(create-tooltip-command "show-uptime" "uptime")
(create-tooltip-command "show-running-processes" "ps aux") ;; Make visual-line-mode active
(create-tooltip-command "show-process-tree" "pstree")
(create-tooltip-command "show-kernel-ring-buffer" "dmesg | tail -20")
(create-tooltip-command "show-maximum-n-processes" "cat /proc/sys/kernel/pid_max")
(create-tooltip-command "show-system-version" "cat /etc/*-release")
(create-tooltip-command "show-current-jobs" "jobs -l" )
(create-tooltip-command "show-system-info" "uname -a" )
(create-tooltip-command "show-system-platform" "uname -i" )
(create-tooltip-command "show-system-alias-list" "alias -p" )
(create-tooltip-command "show-disk-usage" "df -h" )
(create-tooltip-command "show-dir-usage" "du -h" )
(create-tooltip-command "show-dir-tree" "tree" )
(create-tooltip-command "show-cpu-info" "lscpu" )
(create-tooltip-command "show-libs-in-cache" "ldconfig -p" )
(create-tooltip-command "count-num-cores" "nproc --all" )
(create-tooltip-command "show-running-services" "service --status-all" )

; ------------------------------------------------------------------------- ;
;                                Improvements                               ;
; ------------------------------------------------------------------------- ;

;; Print or control the kernel ring buffer
;; dmesg
;; (create-tooltip-command "show-kernel-alert" "dmesg - -level=alert")
;; (create-tooltip-command "show-kernel-memory" "dmesg | grep -i memory")


;; TODO: improve the macro || alternative macro for the following use-cases 

;; Copy your default public key to remote user
;; ssh-copy-id <user_name>@<server_IP>

;; Display file status (size; access, modify and change time, etc) of a file (e.g. filename.txt)
;; stat filename.txt

;; Print information related to USB
;; $ dmesg | grep -i usb

;; Print with human readable time
;; $ dmesg  - -ctime

;; Print dmesg based on facility 
;; dmesg --facility=daemon
;; kern - kernel messages
;; user - random user-level messages
;; mail - mail system
;; daemon - system daemons
;; auth - security/authorization messages
;; syslog - messages generated internally by syslogd
;; lpr - line printer subsystem
;; news - network news subsystemt

;; Print log level
;; dmesg - -level=err
;; Supported levels
;; emerg - system is unusable
;; alert - action must be taken immediately
;; crit - critical conditions
;; err - error conditions
;; warn - warning conditions
;; notice - normal but significant condition
;; info - informational
;; debug - debug-level messages


;; Print shared library dependencies (e.g. for ‘ls’)
;; ldd /bin/ls


;; This will show whether the target is a builtin, a function, an alias or an external executable
;; type -a lshw


;; Kill all process of a program
;; kill -9 $(ps aux | grep 'program_name' | awk '{print $2}')

; ------------------------------------------------------------------------- ;
; ------------------------------------------------------------------------- ;


(provide 'modes-shell)

;;; modes-shell.el ends here
