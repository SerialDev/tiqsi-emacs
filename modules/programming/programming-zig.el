;;; programming-zig.el --- Tiqsi Zig mode support  -*- lexical-binding: t -*-

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


(use-package zig-mode
  :straight t
  :config (progn
            (add-hook 'zig-mode-hook 'zig-file-coding-system)
            ))


(defun zig-file-coding-system ()
  (with-current-buffer (current-buffer)
    (if (string-match "\\.d?zig\\'" buffer-file-name)
      (setq buffer-file-coding-system 'utf-8-unix)
      nil)
    ))

(defun zig-compile ()
  (interactive)
  (async-shell-command
    (s-prepend "zig build-exe " (buffer-name))
    "*zig-compilation*" "*Zig-error*"
    ))


(defun zig-repl ()
  (interactive)
  (async-shell-command
    (s-prepend
      (s-prepend "zig build-exe " (buffer-name)) " --watch") 
    "*zig-compilation*" "*Zig-error*"
    ))

(defun zig-test()
  (interactive)
  (async-shell-command
    (s-prepend "zig test " (buffer-name))
    "*zig-test*" "*Zig-error*"))

(defun zig-run()
  (interactive)
  (async-shell-command
    (s-prepend "zig run " (buffer-name))
    "*zig-test*" "*Zig-error*"))

(setq default-zig-scratch-path "/Users/amariscalcloudflare.com/Documents/workdir/personal/repos/")

(defun create-folder-in-path (folder-name path)
  "Create a folder with FOLDER-NAME in PATH.
If PATH doesn't exist, it will be created too."
  (interactive "sFolder name: \nsPath: ")
  (let ((full-path (concat path "/" folder-name)))
    (make-directory full-path t)
    (shell-command (format "chmod 775 %s" full-path))))


(defun create-zig-scratch ()
  "Create a new Zig scratch file and initialize a Zig project."
  (interactive)

  (create-folder-in-path "scratch_zig" default-zig-scratch-path)
  (async-shell-command (s-prepend "cd " (concat (concat default-zig-scratch-path "scratch_zig/") "&& zig init")))
  (find-file (concat default-zig-scratch-path "scratch_zig/src/main.zig"))
  (revert-buffer-quick)

  )

(defun goto-zig-scratch ()
  "Goto Zig scratch file."
  (interactive)
  (find-file (concat default-zig-scratch-path "scratch_zig/src/main.zig"))
  (revert-buffer-quick)
  )



(defun delete-zig-scratch-folder ()
  "Deletes the Zig scratch folder and its contents.
USAGE: (delete-zig-scratch-folder)"
  (interactive)
  (let ((zig-scratch-path (concat default-zig-scratch-path "scratch_zig")))
    (async-shell-command (s-prepend "rm -rf " zig-scratch-path))
    (message "Zig scratch folder deleted")))



(global-auto-revert-mode 1)
(setq load-physically t)
(setq load-prefer-newer t)



(defhydra hydra-ziggy (:color blue :hint nil)
  "
` ` _ _ _ _ _ _ _ _ _` ` ` ` | ^Ziggy^
`  |_ _ _ _ _ _ _ _ _ |` ` ` | -----------------------------------------------------------
` ` ` \\\\ \\ \\ // ///` ` ` ` ` | _c_: Compile
` ` `  \\\\_|_|_| // ` ` ` ` ` |
 Tiqsi |        |` ` ` ` ` ` | _t_: Run Tests
` ` `  | o    o | Emacs` ` ` | _e_: Run repl[HCS dependent]
` ` _ _ _ _ _ _ _ _ _  ` ` ` | _r_: Run
`  |_ _ _ _ _ _ _ _ _ |` ` ` |
` ` ` ` \\_ _ _ /` ` ` ` ` ` `|"
  ("c" zig-compile)
  ("e" zig-repl)
  ("t" zig-test)
  ("r" zig-run)
  ("ESC" nil "Exit"))



(define-key zig-mode-map (kbd "C-c C-c") 'zig-compile)
(define-key zig-mode-map (kbd "C-c C-t") 'zig-test)
(define-key zig-mode-map (kbd "C-c C-r") 'zig-run)
(define-key zig-mode-map (kbd "M-z") 'hydra-ziggy/body)



(provide 'programming-zig)

;;; programming-zig.el ends here
