;;; core-ui.el --- Tiqsi ui elements

;;; Commentary:
;; 

;{Startup Windowing};

(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

;---{No Scrollbar}--;

(scroll-bar-mode -1)

;----{NO Toolbar}---;

(tool-bar-mode 0)

;{NO shift to select};

(setq shift-select-mode nil)

;-{Dont ask Yes/No}-;

(fset 'yes-or-no-p 'y-or-n-p)


;-{NO middle mouse}-;

; no screwing with my middle mouse button
(global-unset-key [mouse-2])


;--{Smooth Scroll}--;

(setq scroll-step 3)


;-{Show Keystrokes}-;

(setq echo-keystrokes 0.0001)             

;-{Brigth red TODO}-;

(setq fixme-modes '(c++-mode c-mode emacs-lisp-mode python-mode))
(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(mapc (lambda (mode)
    (font-lock-add-keywords
     mode
     '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
	   ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
   fixme-modes)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)


;--{Line Highlight}-;

(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

;-----{Set Font}----;

(setq tiqsi-font "PragmataPro")
;; Font cosmetic edits
(add-to-list 'default-frame-alist '(font . "PragmataPro"))
(set-face-attribute 'default t :font "PragmataPro")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")


;; Consistent things
(defvar me/font-family            "PragmataPro"  "The font to use.")

(when tiqsi-win32
  (defvar me/font-size-default      120       "The font size to use for default text.")
  (defvar me/font-size-header       140       "The font size to use for headers.")
  (defvar me/font-size-mode-line    120       "The font size to use for the mode line."))


(when tiqsi-linux
  (defvar me/font-size-default      420       "The font size to use for default text.")
  (defvar me/font-size-header       440       "The font size to use for headers.")
  (defvar me/font-size-mode-line    420       "The font size to use for the mode line."))

(when tiqsi-linux
   (set-face-attribute 'default nil :height 200))

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (maximize-frame)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
)
(add-hook 'window-setup-hook 'post-load-stuff t)

;------{Beacon}-----;

(beacon-mode 1)
(setq beacon-push-mark 35)
(setq beacon-color "#666600")


;----{spaceline}----;

;; (use-package spaceline
;;   :straight t
;;   :ensure t
;;   :config
;;   (use-package spaceline-config
;;     :config
;;     (spaceline-toggle-minor-modes-off)
;;     (spaceline-toggle-buffer-encoding-off)
;;     (spaceline-toggle-buffer-encoding-abbrev-off)
;;     (setq powerline-default-separator 'arrow-fade)
;;     (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;;     (spaceline-define-segment line-column
;;       "The current line and column numbers."
;;       "l:%l c:%2c")
;;     (spaceline-define-segment time
;;       "The current time."
;;       (format-time-string "%H:%M"))
;;     (spaceline-define-segment date
;;       "The current date."
;;       (format-time-string "%h %d"))
;;     (spaceline-toggle-time-on)
;;     (spaceline-emacs-theme 'date 'time)
;; 	(spaceline-helm-mode)))

(use-package spaceline-all-the-icons 
  :straight t
  :after spaceline
  :config (spaceline-all-the-icons-theme))

;-{Colour str repr}-;

(use-package rainbow-mode
  :straight t
  :ensure t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook #'rainbow-mode))
  :diminish rainbow-mode)

(use-package highlight-indent-guides
  :straight t
  :ensure t
  :diminish highlight-indent-guides-mode
  :config (progn (setq highlight-indent-guides-method 'column)
                 (add-hook 'python-mode-hook #'highlight-indent-guides-mode)))


;{Highlight actions};

(volatile-highlights-mode t)

;-{Highlight thing}-;

(setq highlight-thing-delay-seconds 0.15) ;; default at 0.5


;{Enforce Line Limit};
(setq column-enforce-column 100)
(setq column-enforce-comments nil)


;{Marked region info};

(add-hook 'rectangle-mark-mode 'region-state-mode)
(add-hook 'text-mode 'region-state-mode)


;{Avoid Line Clutter};
(diminish 'highlight-thing-mode)
(diminish 'volatile-highlights-mode)
(diminish 'highlight-parentheses-mode)

;------{Clock}------;

(display-time)


;; "Never, ever split a window.  Why would anyone EVER want you to do that??"
(setq split-window-preferred-function nil)


(use-package diminish
  :ensure t) ;; to use as :diminish in use packages


;;; TODO Will highlight when cursor on closing parenthesis, however leaves afterglow... fix that
(defadvice mic-paren-highlight (around cursorOnClosing activate)
  "Dirty hack to highlight sexps with closing delim below cursor"
  (if (eq (char-syntax (following-char)) ?\) )
      (let ((paren-priority 'close))
        (save-excursion
          (forward-char)
          ad-do-it))
    ad-do-it))


;------{Ediff}------;

(defun tiqsi-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
)
(setq ediff-window-setup-function 'tiqsi-ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)


;------{Paren}------;

(global-highlight-parentheses-mode 1)

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
; TODO    
;; highlight sexp look into implementing http://superuser.com/questions/304848/highlight-the-matching-content-of-a-pair-of-braces-in-emacs 


(defun flash-region (start end)
 "Temporarily highlight region from START to END."
 (interactive)
 (let ((overlay (make-overlay start end)))
   (overlay-put overlay 'face 'secondary-selection)
   (overlay-put overlay 'priority 100)
   (run-with-timer 0.2 nil 'delete-overlay overlay)))

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)


;---{Keybindings}---;
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(global-set-key (kbd "C-c f") 'flash-region)


(provide 'core-ui)

;;; core-ui.el ends here
