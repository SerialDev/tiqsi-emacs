﻿;;; core-ui.el --- Tiqsi ui elements  -*- lexical-binding: t -*-

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

;;                                         Startup Windowing                                         ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(defun sdev/set-windows ()
  (interactive)
  (progn
    (split-window-horizontally)
    (other-window 1)
    (split-window-below)
    (other-window 1)
    (split-window-below)
    (delete-window)
    (other-window 1)
    (safe-execute(vterm))
    )
  )

(GNUEmacsGT25
  (sdev/set-windows)
  )

(GNUEmacs25
  (progn
    (split-window-horizontally)
    (other-window 1)
    (split-window-below)
    (other-window 1)
    (split-window-below)
    (delete-window)
    (other-window 1)
    (eshell )
    )
  )


;;                                            No Scrollbar                                           ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-not-console
  (scroll-bar-mode -1))

;;                                             NO Toolbar                                            ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-not-console
  (tool-bar-mode 0))

;;                                         NO shift to select                                        ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(setq shift-select-mode nil)

;;                                          Dont ask Yes/No                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(fset 'yes-or-no-p 'y-or-n-p)


;;                                          NO middle mouse                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
;;                  no screwing with my middle mouse button                  ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(global-unset-key [mouse-2])


;;                                           Smooth Scroll                                           ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;
(setq scroll-step 3)



;;                                 Pop window                                ;
;; ------------------------------------------------------------------------- ;

(straight-require 'popwin)

(popwin-mode 1)

(setq popwin:popup-window-height 35)
(setq popwin:popup-window-width 15)


;;                                          Show Keystrokes                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(setq echo-keystrokes 0.0001)

;; _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Face modifiers ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _  ;



;;                                          Brigth red TODO                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(make-face 'font-lock-check-face)
(make-face 'font-lock-done-face)
(make-face 'font-lock-fn-face)
(make-face 'font-lock-nus-face)
(make-face 'font-lock-wip-face)
(make-face 'font-lock-experimental-face)

(mapc (lambda (mode)
        (font-lock-add-keywords
          mode
          '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
             ("\\<\\(NOTE\\)" 1 'font-lock-note-face t)
             ("\\<\\(CHECK\\)" 1 'font-lock-check-face t)
             ("\\<\\(DONE\\)" 1 'font-lock-done-face t)
             ("\\<\\(WIP\\)" 1 'font-lock-wip-face t)
             ("\\<\\(fn\\(\\)\\)" 1 'font-lock-fn-face t)
             ("\\<\\(fn\\)" 1 'font-lock-fn-face t)
             ("\\<\\(Experimental!\\)" 1 'font-lock-experimental-face t)
	     ("^.*NOT USABLE.*" 0 'font-lock-nus-face t)


	     )))
  fixme-modes)

(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)
(modify-face 'font-lock-check-face "Yellow" nil nil t nil t nil nil)
(modify-face 'font-lock-done-face "Green" nil nil t nil t nil nil)
(modify-face 'font-lock-fn-face "Blue" nil nil t nil t nil nil)
(modify-face 'font-lock-nus-face "Grey21" nil nil t nil t nil nil)
(modify-face 'font-lock-experimental-face "Purple" nil nil t nil t nil nil)
(modify-face 'font-lock-wip-face "systemYellowColor" nil nil t nil t nil nil)


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Face modifiers _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;

;;                                           Line Highlight                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(straight-require 'hl-line+)
(global-hl-line-mode 0)

(defvar-local was-hl-line-mode-on nil)
(defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
(defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
(add-hook 'hl-line-mode-hook
  (lambda () (if hl-line-mode (setq was-hl-line-mode-on t))))

(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'emacs-lisp-mode-hook #'hl-line-mode)
(add-hook 'fundamental-mode-entry-hook 'hl-line-off-maybe)
(add-hook 'fundamental-mode-exit-hook 'hl-line-off-maybe)


(set-face-background 'hl-line "midnight blue")
(global-hl-line-unhighlight-all)


;;                                              Set Font                                             ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defun font-exists-p (font)
  "check if font exists"
  (if (null (x-list-fonts font)) nil t))


(when tiqsi-not-console
  (if (font-exists-p "PragmataPro")
    (setq tiqsi-font "PragmataPro")
    (if (font-exists-p "MesloLGS Nerd Font Mono")
      (setq tiqsi-font "MesloLGS Nerd Font Mono")
      (setq tiqsi-font "Courier New"))
    ))

;; Font cosmetic edits
(add-to-list 'default-frame-alist `(font . ,tiqsi-font))
(set-face-attribute 'default t :font tiqsi-font)
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-face-attribute 'region nil :background "#666" :foreground "#ffffff")


(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "Consolas")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 110)

  )

;; Consistent things
(defvar me/font-family            tiqsi-font  "The font to use.")

(when tiqsi-win32
  (defvar me/font-size-default      120       "The font size to use for default text.")
  (defvar me/font-size-header       140       "The font size to use for headers.")
  (defvar me/font-size-mode-line    120       "The font size to use for the mode line."))

;; BUG: bugs out posframe
;; (when tiqsi-linux
;;   (defvar me/font-size-default      420       "The font size to use for default text.")
;;   (defvar me/font-size-header       440       "The font size to use for headers.")
;;   (defvar me/font-size-mode-line    420       "The font size to use for the mode line."))

;; (when tiqsi-linux
;;    (set-face-attribute 'default nil :height 120))

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
  )
(add-hook 'window-setup-hook 'post-load-stuff t)

;;                                               Beacon                                              ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

;;                           TODO: Asyc the beacon                             ;
;; ------------------------------------------------------------------------- ;

(GNUEmacs25
  (defvar after-focus-change-function #'ignore
    ))

(use-package beacon
  :straight t
  :ensure t
  :config (progn
	    (beacon-mode 1)
	    (setq beacon-push-mark 35)
	    ;; (add-hook 'beacon-dont-blink-predicates #'fundamental-mode-p)
	    (setq beacon-color "#666600")
	    (setq beacon-blink-when-point-moves-vertically 10)
	    (setq beacon-blink-when-point-moves-horizontally 10)
	    (setq beacon-blink-when-focused t)
	    (setq beacon-blink-duration 0.2)
	    (setq beacon-blink-delay 0.2)
	    (setq beacon-size 20)))



;;                                             spaceline                                             ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when tiqsi-not-console
  (use-package spaceline-all-the-icons
    :straight t
    :after spaceline
    :config (spaceline-all-the-icons-theme)))

;;                                          Colour str repr                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(use-package rainbow-mode
  :straight t
  :ensure t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook #'rainbow-mode)))

;; (use-package highlight-indent-guides
;;   :straight t
;;   :ensure t
;;   :config (progn (setq highlight-indent-guides-method 'column)
;;             (add-hook 'python-mode-hook 'highlight-indent-guides-mode)))


;;                                         Highlight actions                                         ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

;; (straight-use-package
;;   '(volatile-highlights
;;      :type git
;;      :host github
;;      :repo "k-talo/volatile-highlights.el"
;;      :config (progn (volatile-highlights-mode t))
;;      ))


(use-package highlight-parentheses
  :straight t
  :ensure t
  :config (progn
            (global-highlight-parentheses-mode 1)
	    ))

;; _ _ _ _ _ _ _ _ _ _    /¯¯¯ Select from a list ¯¯¯\_ _ _ _ _ _ _ _ _ _    ;

(defmacro pick-one(info &rest body)
  "Prompt user to pick a choice from a list
usage (pick-one test (1 2))
"
  (interactive)
  `(message "%s" (ido-completing-read+ ,info ',@body))
  )


;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Select from a list _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    ;

;;                                          Highlight thing                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;


(use-package highlight-thing
  :straight t
  :ensure t
  :config
  (progn
    (setq highlight-thing-delay-seconds 0.15) ;; default at 0.5
    ))

;;                                         Enforce Line Limit                                        ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(GNUEmacsGT25
  (use-package column-enforce-mode
    :straight
    :ensure
    :config
    (progn
      (setq column-enforce-column 100)
      (setq column-enforce-comments nil)))
  )

;;                                         Marked region info                                        ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

;; (add-hook 'rectangle-mark-mode 'region-state-mode)
(add-hook 'text-mode 'region-state-mode)


;;                                               Clock                                               ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(display-time)

;;                                            Line numbers                                           ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(safe-execute (global-display-line-numbers-mode t))


;;                                            col numbers                                            ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(column-number-mode t)

;;                                           show file-size                                          ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(size-indication-mode t)

;; don't commit trailing whitespace 
(setq-default show-trailing-whitespace nil)
(setq-default default-indicate-empty-lines t)

(defun toggle-whitespace ()
  (interactive)
  (if (equal show-trailing-whitespace nil)
    (setq show-trailing-whitespace t)
    (setq show-trailing-whitespace nil))

  )


;; "Never, ever split a window.  Why would anyone EVER want you to do that??"
(setq split-window-preferred-function nil)


;;; TODO Will highlight when cursor on closing parenthesis, however leaves afterglow... fix that
(defadvice mic-paren-highlight (around cursorOnClosing activate)
  "Dirty hack to highlight sexps with closing delim below cursor"
  (if (eq (char-syntax (following-char)) ?\) )
    (let ((paren-priority 'close))
      (save-excursion
        (forward-char)
        ad-do-it))
    ad-do-it))


;;                                               Ediff                                               ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(defun tiqsi-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
  )

(setq ediff-window-setup-function 'tiqsi-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)


;;                                         Avoid Line Clutter                                        ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(use-package diminish
  :straight t
  :ensure t
  :config
  (progn
    (diminish 'highlight-thing-mode)
    (diminish 'highlight-indent-guides-mode)
    (diminish 'volatile-highlights-mode)
    (diminish 'highlight-parentheses-mode)
    (diminish 'rainbow-mode)
    )) ;; to use as :diminish in use packages


;;                                               Paren                                               ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(when (try-require 'paren)
  (GNUEmacs
    (show-paren-mode t)
    (setq show-paren-ring-bell-on-mismatch t))
  (XEmacs
    (paren-set-mode 'paren)))

;; if the matching paren is offscreen, show the matching line in the echo area
;; + many other useful things
(when window-system
  ;; advanced highlighting of matching parentheses
  (when (try-require 'mic-paren)

    ;; activating
    (paren-activate)))

;; TODO
;; highlight sexp look into implementing
;; http://superuser.com/questions/304848/highlight-the-matching-content-of-a-pair-of-braces-in-emacs

;; TODO enable for clojure / lisp mode only
;; (show-paren-mode t)
;; (setq show-paren-style 'expression)

;; (defun flash-region (start end)
;;   "Temporarily highlight region from START to END."
;;   (interactive)
;;   (let ((overlay (make-overlay start end)))
;;     (overlay-put overlay 'face 'secondary-selection)
;;     (overlay-put overlay 'priority 100)
;;     (run-with-timer 0.2 nil 'delete-overlay overlay)))


(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)


(use-package fill-column-indicator
  :straight t
  :ensure t
  :config )

(use-package visual-fill-column
  :straight t
  :ensure t
  :config )


(use-package whitespace
  :straight t
  :ensure t
  :config )

(setq whitespace-style '(face trailing tabs))

(setq whitespace-display-mappings
  '((space-mark 32 [183])            ; normal space
     (newline-mark 10 [?↷ 10])))      ; newline

(setq whitespace-line-column 79)

(eval-after-load 'whitespace
  (lambda ()
    (set-face-attribute 'whitespace-newline nil :foreground "#d3d7cf")
    (set-face-attribute 'whitespace-tab nil :background nil :underline "#d3d7cf")
    (set-face-attribute 'whitespace-trailing nil :background nil :underline "#a40000")
    ))

(defun whitespace-post-command-hook() nil) ; workaround for cursor slowdown

(add-hook 'prog-mode-hook 'whitespace-mode)

(setq whitespace-display-mappings
  '((space-mark 32 [183])            ; normal space
     (newline-mark 10 [?↷ 10])))      ; newline


(eval-after-load 'whitespace
  (lambda ()
    (set-face-attribute 'whitespace-newline nil :foreground "#d3d7cf")))

(defvar my-visual-line-state nil)

(defun my-visual-line-mode-hook ()
  (when visual-line-mode
    (setq my-visual-line-state
      `(whitespace-style ,whitespace-style
         whitespace-mode ,whitespace-mode
         auto-fill-mode ,auto-fill-function))

    (when whitespace-mode
      whitespace-mode -1)

    ;; display newline characters with whitespace-mode
    (make-local-variable 'whitespace-style)
    (setq whitespace-style '(newline newline-mark))
    (whitespace-mode)

    ;; disable auto-fill-mode
    (when auto-fill-function
      (auto-fill-mode -1))

    ;; visually wrap text at fill-column
    (visual-fill-column-mode)))

(add-hook 'visual-line-mode-hook 'my-visual-line-mode-hook)

(defun my-visual-line-mode-off ()
  (interactive)
  (visual-fill-column-mode--disable)
  (visual-line-mode -1)
  ;; revert the state before activating visual-line-mode
  (when my-visual-line-state
    (let ((ws-style (plist-get my-visual-line-state 'whitespace-style))
           (ws-mode (plist-get my-visual-line-state 'whitespace-mode))
           (af-mode (plist-get my-visual-line-state 'auto-fill-mode)))

      (when whitespace-mode
        (whitespace-mode -1))
      (when ws-style (setq whitespace-style ws-style))
      (when ws-mode (whitespace-mode 1))

      (when af-mode (auto-fill-mode 1)))))


(defun my-visual-line-mode-toggle ()
  (interactive)
  (if visual-line-mode
    (my-visual-line-mode-off)
    (visual-line-mode 1)))


(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

(add-hook 'mu4e-compose-mode-hook
  (defun my-mu4e-visual-line-reply ()
    "Activate visual-line-mode for specific services like GitHub."
    (let ((msg mu4e-compose-parent-message))
      (when (and msg (mu4e-message-contact-field-matches
		       msg :from '(".*@github.com$" ".*@upwork.com$")))
        (visual-line-mode)))))


;; (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))


;; _ _ _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Images ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;




(defun insert-image-from-url (&optional url)
  (interactive)
  (unless url (setq url (url-get-url-at-point)))
  (unless url
    (error "Couldn't find URL."))
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
      (let ((data (with-current-buffer buffer
                    (goto-char (point-min))
                    (search-forward "\n\n")
                    (buffer-substring (point) (point-max)))))
        (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

;; (insert-image-from-url "https://tpc.googlesyndication.com/simgad/1487126809566417335")  


(defun my-turn-current-window-into-frame ()
  (interactive)
  (let ((buffer (current-buffer)))
    (unless (one-window-p)
      (delete-window))
    (display-buffer-pop-up-frame buffer nil)))

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Images _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;

;; _ _ _ _ _ _ _ _ _ _ _ _ _ _  /¯¯¯ Themes ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _ _ _  ;

;;                                   Themes                                  ;
;; ------------------------------------------------------------------------- ;

;; (use-package exotica-theme
;;   :straight t
;;   :ensure t
;;   :config
;;   (progn 
;;     (load-theme 'exotica t)
;;     (setq exotica-theme-enable-italics t)
;;     ))

;; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  \_ _ Themes _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯  ;


;;                                    Tray                                   ;
;; ------------------------------------------------------------------------- ;

;; (straight-use-package
;;   '(awesome-tray
;;      :type git
;;      :host github
;;      :repo "manateelazycat/awesome-tray"
;;      ))

;; (awesome-tray-mode 0)


;; ------------------------------------------------------------------------- ;
;;             Interesting library, worth inspecting more for UI             ;
;; ------------------------------------------------------------------------- ;
;;                                   TODO:                                   ;
;; ------------------------------------------------------------------------- ;

(straight-use-package
  '(svg-lib
     :type git
     :host github
     :ensure t
     :repo "rougier/svg-lib"
     ))

;; ------------------------------------------------------------------------- ;


(define-key input-decode-map "\e[1;3D" [M-left])


(define-key function-key-map "\eOA" [up])
(define-key function-key-map "\e[A" [up])
(define-key function-key-map "\eOB" [down])
(define-key function-key-map "\e[B" [down])
(define-key function-key-map "\eOC" [right])
(define-key function-key-map "\e[C" [right])
(define-key function-key-map "\eOD" [left])
(define-key function-key-map "\e[D" [left])


;;                                            Keybindings                                            ;
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ;

(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; (global-set-key (kbd "C-c f") 'flash-region)


(provide 'core-ui)

;;; core-ui.el ends here
