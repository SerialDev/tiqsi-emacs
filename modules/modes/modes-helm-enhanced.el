;;; modes-helm-enhanced.el --- Enhanced helm configuration with visual distinctions  -*- lexical-binding: t -*-

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
;; Enhanced helm configuration with visual distinctions for files, folders, and commands

;;; Code:

(require 'helm)
(require 'helm-files)
(require 'recentf)

;; Enable recentf for recent file tracking
(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 500)
(setq recentf-auto-cleanup 'never) ; Don't cleanup on startup for performance

;; Custom faces for different file types
(defface helm-enhanced-directory
  '((t (:foreground "#51afef" :weight bold)))
  "Face for directories in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-executable
  '((t (:foreground "#98be65" :weight bold)))
  "Face for executable files in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-symlink
  '((t (:foreground "#c678dd" :slant italic)))
  "Face for symlinks in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-regular-file
  '((t (:foreground "#bbc2cf")))
  "Face for regular files in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-recent-file
  '((t (:foreground "#ff6c6b" :weight normal)))
  "Face for recent files in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-command
  '((t (:foreground "#da8548" :weight bold)))
  "Face for commands in helm."
  :group 'helm-enhanced)

(defface helm-enhanced-function
  '((t (:foreground "#ECBE7B")))
  "Face for functions in helm."
  :group 'helm-enhanced)

;; Enhanced helm-mini that shows recent files prominently
(defun helm-enhanced-mini ()
  "Enhanced helm-mini with recent files and visual distinctions."
  (interactive)
  (condition-case err
      (progn
        (require 'helm)
        (require 'helm-buffers)
        (require 'recentf)
        ;; Make sure recentf is active
        (unless recentf-mode (recentf-mode 1))
        ;; Use standard helm-mini with enhanced settings
        (let ((helm-buffer-max-length 60)
              (helm-buffers-fuzzy-matching t)
              (helm-buffer-skip-remote-checking t)
              (helm-candidate-number-limit 300)
              (helm-buffer-details-flag t))
          (helm-mini)))
    (error 
     (message "Error in helm-enhanced-mini: %s. Falling back to standard helm-mini" err)
     (helm-mini))))

;; Custom transformer to add faces to different file types
(defun helm-enhanced-highlight-files (files _source)
  "Add faces to FILES based on their type."
  (cl-loop for file in files
           collect (let ((basename (file-name-nondirectory file)))
                     (cond
                      ((file-directory-p file)
                       (propertize file 'face 'helm-enhanced-directory))
                      ((file-executable-p file)
                       (propertize file 'face 'helm-enhanced-executable))
                      ((file-symlink-p file)
                       (propertize file 'face 'helm-enhanced-symlink))
                      (t
                       (propertize file 'face 'helm-enhanced-regular-file))))))

;; Enhanced M-x with command highlighting
(defun helm-enhanced-M-x-transformer (candidates _source)
  "Transform CANDIDATES to add visual distinctions."
  (cl-loop for candidate in candidates
           collect (let ((sym (intern candidate)))
                     (cond
                      ((commandp sym)
                       (propertize candidate 'face 'helm-enhanced-command))
                      ((functionp sym)
                       (propertize candidate 'face 'helm-enhanced-function))
                      (t candidate)))))

;; Configure helm sources with transformers
(with-eval-after-load 'helm-files
  (when (boundp 'helm-type-file-transformers)
    (add-to-list 'helm-type-file-transformers 'helm-enhanced-highlight-files)))

;; Enhanced helm-smex with visual distinctions
(defun helm-enhanced-smex ()
  "Enhanced helm-smex with visual distinctions."
  (interactive)
  (condition-case err
      (progn
        (require 'smex)
        (require 'helm-smex)
        ;; Initialize smex if not already done
        (unless (and (boundp 'smex-cache) smex-cache)
          (smex-initialize))
        ;; Use enhanced transformer if available
        (if (functionp 'helm-enhanced-M-x-transformer)
            (let ((helm-M-x-transformer-list '(helm-enhanced-M-x-transformer)))
              (helm-smex))
          (helm-smex)))
    (error 
     (message "Error in helm-enhanced-smex: %s. Falling back to standard helm-smex" err)
     (helm-smex))))

;; Optimized helm settings for performance with usability
(setq helm-candidate-separator "──────────────────────────────────────")
(setq helm-autoresize-max-height 50) ; Show more candidates
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

;; Show more information
(setq helm-buffer-max-length 60) ; Longer buffer names
(setq helm-buffer-details-flag t) ; Show buffer details
(setq helm-buffers-truncate-lines nil) ; Don't truncate
(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-buffer-not-found))

;; Show more candidates
(setq helm-candidate-number-limit 300) ; Show up to 300 candidates
(setq helm-buffer-max-length 50)
(setq helm-boring-file-regexp-list
      '("\\.git$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "~$"
        "\\.so$" "\\.a$" "\\.elc$" "\\.pyc$" "\\.pyo$"))

;; Better fuzzy matching
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-locate-fuzzy-match t)
(setq helm-file-cache-fuzzy-match t)
(setq helm-projectile-fuzzy-match t)

;; Performance settings that don't hurt usability
(setq helm-input-idle-delay 0.01)
(setq helm-cycle-resume-delay 2)
(setq helm-follow-input-idle-delay 0.1)
(setq helm-exit-idle-delay 0.1)

;; Keep history
(setq helm-ff-file-name-history-use-recentf t)
(setq helm-ff-search-library-in-sexp t)

;; Smart sorting - recent items first
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)

(provide 'modes-helm-enhanced)

;;; modes-helm-enhanced.el ends here