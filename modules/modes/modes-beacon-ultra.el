;;; modes-beacon-ultra.el --- Ultra-optimized beacon implementation  -*- lexical-binding: t -*-

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
;; Ultra-performance-optimized beacon - the fastest beacon in the west

;;; Code:

(require 'cl-lib)

;;; Customization variables - optimized defaults

(defgroup ultra-beacon nil
  "Ultra-fast beacon."
  :group 'convenience
  :prefix "ultra-beacon-")

(defcustom ultra-beacon-color "#d4af37"
  "Beacon color - golden yellow for comfortable visibility."
  :type 'color)

(defcustom ultra-beacon-size 30
  "Beacon size - larger for better visibility."
  :type 'integer)

(defcustom ultra-beacon-blink-duration 0.3
  "Blink duration - longer for visibility."
  :type 'number)

(defcustom ultra-beacon-blink-delay 0.0
  "Blink delay - instant response."
  :type 'number)

(defcustom ultra-beacon-minimum-movement 10
  "Minimum lines to move before blinking."
  :type 'integer)

(defcustom ultra-beacon-fade-steps 8
  "Number of fade steps - more for gentler fade."
  :type 'integer)

(defcustom ultra-beacon-push-mark nil
  "Push mark when blinking."
  :type 'boolean)

;;; Internal variables - minimal state

(defvar-local ultra-beacon--overlay nil
  "Single overlay per buffer.")

(defvar ultra-beacon--timer nil
  "Global timer.")

(defvar ultra-beacon--last-position nil
  "Last position as integer.")

(defvar ultra-beacon--last-window nil
  "Last window.")

(defvar ultra-beacon--fade-counter 0
  "Current fade step.")

(defvar ultra-beacon--beacon-string nil
  "Cached beacon string.")

(defvar ultra-beacon--colors nil
  "Cached fade colors.")

(defvar ultra-beacon--active-overlays nil
  "List of all active overlays across buffers.")

;;; Optimized functions

(defsubst ultra-beacon--cache-colors ()
  "Pre-calculate fade colors."
  (let ((bg (face-attribute 'default :background)))
    (setq ultra-beacon--colors
          (cl-loop for i from 0 to ultra-beacon-fade-steps
                   collect (ultra-beacon--blend-colors
                           ultra-beacon-color bg
                           (/ (float i) ultra-beacon-fade-steps))))))

(defsubst ultra-beacon--normalize-color (color)
  "Convert COLOR to hex format if it's a named color."
  (cond 
   ((string-match "^#[0-9a-fA-F]\\{6\\}$" color) color) ; Already hex
   ((string-equal color "white") "#ffffff")
   ((string-equal color "White") "#ffffff") 
   ((string-equal color "black") "#000000")
   ((string-equal color "Black") "#000000")
   (t "#ffffff"))) ; Default fallback

(defsubst ultra-beacon--blend-colors (c1 c2 alpha)
  "Blend colors C1 and C2 by ALPHA."
  ;; Normalize colors first
  (let* ((color1 (ultra-beacon--normalize-color c1))
         (color2 (ultra-beacon--normalize-color c2))
         (r1 (string-to-number (substring color1 1 3) 16))
         (g1 (string-to-number (substring color1 3 5) 16))
         (b1 (string-to-number (substring color1 5 7) 16))
         (r2 (string-to-number (substring color2 1 3) 16))
         (g2 (string-to-number (substring color2 3 5) 16))
         (b2 (string-to-number (substring color2 5 7) 16)))
    (format "#%02x%02x%02x"
            (truncate (+ (* r1 (- 1 alpha)) (* r2 alpha)))
            (truncate (+ (* g1 (- 1 alpha)) (* g2 alpha)))
            (truncate (+ (* b1 (- 1 alpha)) (* b2 alpha))))))

(defsubst ultra-beacon--make-beacon-string (color)
  "Create beacon string with COLOR and comet tail effect."
  (let* ((bg (face-attribute 'default :background))
         (half-size (/ ultra-beacon-size 2))
         ;; Create gradient tail - bright to dim
         (tail-colors (cl-loop for i from 1 to half-size
                              collect (ultra-beacon--blend-colors color bg (/ (float i) half-size))))
         ;; Build comet: bright center + fading tail
         (comet-string ""))
    ;; Bright center
    (setq comet-string (concat comet-string 
                              (propertize (make-string half-size ?\s)
                                         'face `(:background ,color))))
    ;; Fading tail
    (dolist (tail-color tail-colors)
      (setq comet-string (concat comet-string 
                                (propertize " " 'face `(:background ,tail-color)))))
    comet-string))

(defsubst ultra-beacon--get-overlay ()
  "Get or create overlay."
  (or (and ultra-beacon--overlay
           (overlay-buffer ultra-beacon--overlay)
           ultra-beacon--overlay)
      (setq ultra-beacon--overlay
            (let ((ov (make-overlay 1 1)))
              (overlay-put ov 'priority most-positive-fixnum)
              (overlay-put ov 'window (selected-window))
              ov))))

(defun ultra-beacon--shine ()
  "Display beacon - exactly like original beacon with after-string."
  (when (and (not (minibufferp))
             (not executing-kbd-macro)
             (pos-visible-in-window-p))
    ;; Clean up any leftover overlays first
    (ultra-beacon--cleanup-stale-overlays)
    (let ((ov (ultra-beacon--get-overlay))
          (start (point))
          (end (min (1+ (point)) (point-max))))
      ;; Position overlay at current point (handle end-of-buffer)
      (move-overlay ov start end)
      ;; Use 'after-string property like original beacon - purely visual
      (overlay-put ov 'after-string 
                   (ultra-beacon--make-beacon-string
                    (nth 0 ultra-beacon--colors)))
      ;; Set high priority to ensure visibility
      (overlay-put ov 'priority most-positive-fixnum)
      (overlay-put ov 'window (selected-window))
      ;; Track this overlay
      (add-to-list 'ultra-beacon--active-overlays ov)
      (setq ultra-beacon--fade-counter 0)
      (ultra-beacon--fade))))

(defun ultra-beacon--fade ()
  "Fade beacon - exactly like original beacon."
  (when ultra-beacon--timer
    (cancel-timer ultra-beacon--timer))
  (if (< ultra-beacon--fade-counter ultra-beacon-fade-steps)
      (progn
        (cl-incf ultra-beacon--fade-counter)
        (when (and ultra-beacon--overlay
                   (overlay-buffer ultra-beacon--overlay)
                   (nth ultra-beacon--fade-counter ultra-beacon--colors))
          (let* ((ov ultra-beacon--overlay)
                 (color (nth ultra-beacon--fade-counter ultra-beacon--colors)))
            ;; Update after-string with faded color
            (overlay-put ov 'after-string 
                         (ultra-beacon--make-beacon-string color))))
        (setq ultra-beacon--timer
              (run-at-time (* ultra-beacon-blink-duration
                             (/ 1.0 ultra-beacon-fade-steps))
                          nil #'ultra-beacon--fade)))
    (ultra-beacon--vanish)))

(defsubst ultra-beacon--vanish ()
  "Remove beacon - exactly like original beacon."
  (when ultra-beacon--overlay
    ;; Remove after-string property and delete overlay
    (overlay-put ultra-beacon--overlay 'after-string nil)
    (delete-overlay ultra-beacon--overlay)
    ;; Remove from active overlays list
    (setq ultra-beacon--active-overlays 
          (delq ultra-beacon--overlay ultra-beacon--active-overlays))
    (setq ultra-beacon--overlay nil))
  (when ultra-beacon--timer
    (cancel-timer ultra-beacon--timer)
    (setq ultra-beacon--timer nil)))

(defun ultra-beacon--cleanup-stale-overlays ()
  "Clean up any stale overlays that might be left behind."
  (setq ultra-beacon--active-overlays
        (cl-remove-if-not (lambda (ov)
                            (and (overlayp ov)
                                 (overlay-buffer ov)
                                 (buffer-live-p (overlay-buffer ov))))
                          ultra-beacon--active-overlays))
  ;; If we find overlays in other buffers/windows, clean them up
  (dolist (ov ultra-beacon--active-overlays)
    (when (and (overlayp ov)
               (overlay-buffer ov)
               (not (eq (overlay-buffer ov) (current-buffer))))
      (overlay-put ov 'after-string nil)
      (delete-overlay ov)
      (setq ultra-beacon--active-overlays 
            (delq ov ultra-beacon--active-overlays)))))

(defsubst ultra-beacon--should-blink-p ()
  "Check if should blink - optimized."
  (and ultra-beacon--last-position
       (let ((movement (abs (- (point) ultra-beacon--last-position))))
         (or (> movement (* ultra-beacon-minimum-movement 80)) ; Approximate line length
             (not (eq (selected-window) ultra-beacon--last-window))))))

(defsubst ultra-beacon--post-command ()
  "Post command hook - optimized."
  ;; Always clean up stale overlays when switching contexts
  (when (not (eq (selected-window) ultra-beacon--last-window))
    (ultra-beacon--cleanup-stale-overlays))
  (when (ultra-beacon--should-blink-p)
    (when ultra-beacon-push-mark
      (push-mark ultra-beacon--last-position t nil))
    (ultra-beacon-blink))
  (setq ultra-beacon--last-position (point)
        ultra-beacon--last-window (selected-window)))

;;; Public functions

(defun ultra-beacon-blink ()
  "Blink the beacon."
  (interactive)
  (ultra-beacon--shine))

;;; Minor mode

;;;###autoload
(define-minor-mode ultra-beacon-mode
  "Ultra-fast beacon mode."
  :global t
  :group 'ultra-beacon
  (if ultra-beacon-mode
      (progn
        (ultra-beacon--cache-colors)
        (add-hook 'post-command-hook #'ultra-beacon--post-command 90))
    (remove-hook 'post-command-hook #'ultra-beacon--post-command)
    (ultra-beacon--vanish)))

;;; Benchmark functions

(defun ultra-beacon-benchmark ()
  "Benchmark beacon performance."
  (interactive)
  (let ((start (current-time))
        (iterations 1000))
    (dotimes (_ iterations)
      (ultra-beacon--shine)
      (ultra-beacon--vanish))
    (message "Ultra-beacon: %d iterations in %.3f seconds"
             iterations
             (float-time (time-subtract (current-time) start)))))

(provide 'modes-beacon-ultra)

;;; modes-beacon-ultra.el ends here