;;; modes-beacon-tiqsi.el --- Tiqsi optimized beacon implementation  -*- lexical-binding: t -*-

;; Copyright (C) 2018-  Andres Mariscal

;; Author: Andres Mariscal <carlos.mariscal.melgar@gmail.com>
;; Based on beacon.el by Artur Malabarba <emacs@endlessparentheses.com>

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
;; Performance-optimized beacon implementation for Tiqsi Emacs

;;; Code:

(require 'seq)
(require 'faces)

;;; Customization variables

(defgroup tiqsi-beacon nil
  "Customization group for tiqsi-beacon."
  :group 'convenience
  :prefix "tiqsi-beacon-")

(defcustom tiqsi-beacon-color "#c0e9f6"
  "Color of the beacon."
  :type 'color)

(defcustom tiqsi-beacon-size 20
  "Size of the beacon in characters."
  :type 'integer)

(defcustom tiqsi-beacon-blink-duration 0.2
  "Time in seconds for beacon to blink."
  :type 'number)

(defcustom tiqsi-beacon-blink-delay 0.2
  "Time in seconds before beacon starts to blink."
  :type 'number)

(defcustom tiqsi-beacon-blink-when-point-moves-vertically 10
  "Blink beacon when point moves more than this many lines."
  :type '(choice integer (const nil)))

(defcustom tiqsi-beacon-blink-when-point-moves-horizontally nil
  "Blink beacon when point moves more than this many columns."
  :type '(choice integer (const nil)))

(defcustom tiqsi-beacon-blink-when-buffer-changes t
  "Blink beacon when changing buffers."
  :type 'boolean)

(defcustom tiqsi-beacon-blink-when-window-scrolls t
  "Blink beacon when window scrolls."
  :type 'boolean)

(defcustom tiqsi-beacon-blink-when-window-changes t
  "Blink beacon when changing windows."
  :type 'boolean)

(defcustom tiqsi-beacon-blink-when-focused nil
  "Blink beacon when Emacs gains focus."
  :type 'boolean)

(defcustom tiqsi-beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode
                                                   gnus-summary-mode gnus-group-mode)
  "Major modes where beacon should not blink."
  :type '(repeat symbol))

(defcustom tiqsi-beacon-dont-blink-predicates nil
  "List of predicates that prevent beacon from blinking."
  :type 'hook)

(defcustom tiqsi-beacon-dont-blink-commands '(next-line previous-line
                                               forward-line)
  "Commands that should not trigger beacon."
  :type '(repeat symbol))

;;; Internal variables

(defvar tiqsi-beacon--overlay nil
  "Overlay used to display the beacon.")

(defvar tiqsi-beacon--timer nil
  "Timer for beacon blink.")

(defvar tiqsi-beacon--window-scrolled nil
  "Flag for detecting window scroll.")

(defvar tiqsi-beacon--previous-place nil
  "Previous cursor position.")

(defvar tiqsi-beacon--previous-window nil
  "Previous selected window.")

;;; Core functions

(defun tiqsi-beacon--make-overlay ()
  "Create or return the beacon overlay."
  (unless (and tiqsi-beacon--overlay
               (overlay-buffer tiqsi-beacon--overlay))
    (setq tiqsi-beacon--overlay (make-overlay 1 1))
    (overlay-put tiqsi-beacon--overlay 'priority most-positive-fixnum)
    (overlay-put tiqsi-beacon--overlay 'window (selected-window)))
  tiqsi-beacon--overlay)

(defun tiqsi-beacon--colored-char (char)
  "Return CHAR with beacon color applied."
  (propertize char 'face `(:foreground ,tiqsi-beacon-color)))

(defun tiqsi-beacon--make-beacon-string ()
  "Create the beacon string."
  (let ((bg (face-attribute 'default :background)))
    (mapconcat
     (lambda (n)
       (tiqsi-beacon--colored-char
        (char-to-string
         (if (= n 0) ?█ ?░))))
     (number-sequence 0 (1- tiqsi-beacon-size))
     "")))

(defun tiqsi-beacon--shine ()
  "Display the beacon at point."
  (when (and (not (minibufferp))
             (not executing-kbd-macro)
             (not (bound-and-true-p org-capture-mode))
             (not (run-hook-with-args-until-success
                   'tiqsi-beacon-dont-blink-predicates)))
    (let ((ov (tiqsi-beacon--make-overlay)))
      (save-excursion
        (move-overlay ov
                      (progn (beginning-of-line) (point))
                      (progn (end-of-line) (point))))
      (overlay-put ov 'after-string
                   (concat "\n" (tiqsi-beacon--make-beacon-string) "\n"))
      (tiqsi-beacon--fade-out))))

(defun tiqsi-beacon--fade-out ()
  "Fade out the beacon."
  (when tiqsi-beacon--timer
    (cancel-timer tiqsi-beacon--timer))
  (setq tiqsi-beacon--timer
        (run-at-time tiqsi-beacon-blink-duration nil
                     #'tiqsi-beacon--vanish)))

(defun tiqsi-beacon--vanish ()
  "Remove the beacon."
  (when tiqsi-beacon--overlay
    (delete-overlay tiqsi-beacon--overlay))
  (setq tiqsi-beacon--timer nil))

(defun tiqsi-beacon--should-blink-p ()
  "Determine if beacon should blink."
  (and (not (memq major-mode tiqsi-beacon-dont-blink-major-modes))
       (not (memq this-command tiqsi-beacon-dont-blink-commands))
       (not (bound-and-true-p multiple-cursors-mode))))

(defun tiqsi-beacon--movement-exceeds-threshold-p ()
  "Check if cursor movement exceeds threshold."
  (and tiqsi-beacon--previous-place
       (or (and tiqsi-beacon-blink-when-point-moves-vertically
                (>= (abs (- (line-number-at-pos)
                           (car tiqsi-beacon--previous-place)))
                    tiqsi-beacon-blink-when-point-moves-vertically))
           (and tiqsi-beacon-blink-when-point-moves-horizontally
                (>= (abs (- (current-column)
                           (cdr tiqsi-beacon--previous-place)))
                    tiqsi-beacon-blink-when-point-moves-horizontally)))))

(defun tiqsi-beacon--record-vars ()
  "Record current position and window."
  (setq tiqsi-beacon--previous-place (cons (line-number-at-pos) (current-column))
        tiqsi-beacon--previous-window (selected-window)))

(defun tiqsi-beacon--window-scrolled-p ()
  "Check if window was scrolled."
  (prog1 tiqsi-beacon--window-scrolled
    (setq tiqsi-beacon--window-scrolled nil)))

;;; Hook functions

(defun tiqsi-beacon--blink-on-focus ()
  "Blink beacon when Emacs gains focus."
  (when tiqsi-beacon-blink-when-focused
    (tiqsi-beacon-blink)))

(defun tiqsi-beacon--post-command ()
  "Post-command hook for beacon."
  (when (and (tiqsi-beacon--should-blink-p)
             (or (and tiqsi-beacon-blink-when-window-scrolls
                      (tiqsi-beacon--window-scrolled-p))
                 (and tiqsi-beacon-blink-when-window-changes
                      (not (eq tiqsi-beacon--previous-window
                               (selected-window))))
                 (and tiqsi-beacon-blink-when-buffer-changes
                      (not (eq (current-buffer)
                               (window-buffer tiqsi-beacon--previous-window))))
                 (tiqsi-beacon--movement-exceeds-threshold-p)))
    (tiqsi-beacon-blink))
  (tiqsi-beacon--record-vars))

(defun tiqsi-beacon--window-scroll-function (_window _start)
  "Hook function for window scroll."
  (setq tiqsi-beacon--window-scrolled t))

;;; Public functions

(defun tiqsi-beacon-blink ()
  "Blink the beacon at point."
  (interactive)
  (when tiqsi-beacon--timer
    (cancel-timer tiqsi-beacon--timer))
  (run-with-idle-timer tiqsi-beacon-blink-delay nil #'tiqsi-beacon--shine))

;;; Minor mode

(define-minor-mode tiqsi-beacon-mode
  "Toggle Tiqsi Beacon mode."
  :global t
  :group 'tiqsi-beacon
  (if tiqsi-beacon-mode
      (progn
        (add-hook 'window-scroll-functions #'tiqsi-beacon--window-scroll-function)
        (add-hook 'focus-in-hook #'tiqsi-beacon--blink-on-focus)
        (add-hook 'post-command-hook #'tiqsi-beacon--post-command)
        (add-hook 'before-change-functions #'tiqsi-beacon--vanish)
        (add-hook 'pre-command-hook #'tiqsi-beacon--record-vars))
    (remove-hook 'window-scroll-functions #'tiqsi-beacon--window-scroll-function)
    (remove-hook 'focus-in-hook #'tiqsi-beacon--blink-on-focus)
    (remove-hook 'post-command-hook #'tiqsi-beacon--post-command)
    (remove-hook 'before-change-functions #'tiqsi-beacon--vanish)
    (remove-hook 'pre-command-hook #'tiqsi-beacon--record-vars)
    (tiqsi-beacon--vanish)))

(provide 'modes-beacon-tiqsi)

;;; modes-beacon-tiqsi.el ends here