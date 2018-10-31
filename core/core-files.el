;;; core-files.el --- Tiqsi core file functions  -*- lexical-binding: t -*-

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

                                        ;{set compilation dir};
(setq compilation-directory-locked nil)

                                        ;---{TODO related}--;

(setq tiqsi-todo-file "/todo.txt")

(defun load-todo ()
  (interactive)
  (find-file tiqsi-todo-file))

                                        ;---{LOG related}---;

(setq tiqsi-log-file "/log.txt")

(defun load-log ()
  (interactive)
  (find-file tiqsi-log-file)
  (if (boundp 'longlines-mode) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (if (equal longlines-mode t) ()
    (longlines-mode 1)
    (longlines-show-hard-newlines))
  (goto-char (point-max))
  (newline-and-indent)
  (insert-timeofday)
  (newline-and-indent)
  (newline-and-indent)
  (goto-char (point-max)))

;; Automatically creating missing parent directories when visiting a new file.

(defun tiqsi--create-non-existent-directory ()
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p parent-directory))
                   (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
          (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'tiqsi--create-non-existent-directory)

                                        ;---{MAP to MODE}---;

                                        ; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ) auto-mode-alist))



                                        ;---{Buffer File}---;

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;; Never understood why Emacs doesn't have this function.
;;
(defun rename-current-buffer-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn   (rename-file filename new-name 1)   (rename-buffer new-name)    (set-visited-file-name new-name)    (set-buffer-modified-p nil)))))) ;;

;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn     (copy-file filename newname 1)  (delete-file filename)  (set-visited-file-name newname)     (set-buffer-modified-p nil)     t))))

                                        ;--{Scratch Files}--;

(defun start--file (path)
  "Create a file at PATH, creating any containing directories as necessary.
Visit the file after creation."
  (make-directory (file-name-directory path) t)
  (find-file path))

(defun wh/start-scratch-file (file-name)
  "Create a file in ~/scratch for the given file name."
  (interactive "sName of scratch file: ")
  (start--file (expand-file-name (format "~/scratch/%s" file-name))))

(defun tiqsi-start-tmp-file (file-name)
  "Create a file in /tmp for the given file name."
  (interactive "sName of temporary file: ")
  (start--file (expand-file-name (format "/tmp/%s" file-name))))

(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)
    ))


(defun kill-scratch-buffer ()
  "Kill the current (*scratch*) buffer, then create a new one.
 This is called from a hook, kill-buffer-query-functions, and its
 purpose is to prevent the *scratch* buffer from being killed."
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)

;; Ditto for the messags buffer
(save-excursion
  (set-buffer (get-buffer-create "*Messages*"))
  (fundamental-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-messages-buffer))


(defun kill-messages-buffer ()
  "Kill the current (*Messages*) buffer, then create a new one.
 This is called from a hook, kill-buffer-query-functions, and its
 purpose is to prevent the *Messages* buffer from being killed."
  (remove-hook 'kill-buffer-query-functions 'kill-messages-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *messages* buffer
  (set-buffer (get-buffer-create "*Messages*"))
  (fundamental-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-messages-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; (setq find-file-tree-fun "findfile(){ tree -f | grep $1; } # $1 = filename, -f is full path")

(defun find-file-tree-current(str)
  (interactive "sInsert str to search:")
  (compile (format "tree -f | grep %s" str)))

(defun find-file-tree-dir(dir str)
  (interactive "sInsert directory to start:
sInsert str to search: ")
  (compile (format "tree -f %s | grep %s" dir str)))

                                        ;---{keybindings}---;

(define-key global-map [M-t] 'load-todo)
(define-key global-map [M-T] 'load-log)


(provide 'core-files)

;;; core-files.el ends here
