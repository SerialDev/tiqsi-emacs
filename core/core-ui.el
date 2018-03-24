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

;-{Show Keystrokes}-;

(setq echo-keystrokes 0.0001)             

;{buffer performance};

;; - Casey
;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(setq gc-cons-threshold 20000000)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 20000000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


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

(setq casey-font "PragmataPro")
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

(use-package spaceline
  :straight t
  :ensure t
  :config
  (use-package spaceline-config
    :config
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (setq powerline-default-separator 'arrow-fade)
    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
    (spaceline-define-segment line-column
      "The current line and column numbers."
      "l:%l c:%2c")
    (spaceline-define-segment time
      "The current time."
      (format-time-string "%H:%M"))
    (spaceline-define-segment date
      "The current date."
      (format-time-string "%h %d"))
    (spaceline-toggle-time-on)
    (spaceline-emacs-theme 'date 'time)
	(spaceline-helm-mode)))


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

(use-package diminish
  :ensure t) ;; to use as :diminish in use packages

;---{Keybindings}---;
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)


(provide 'core-ui)

;;; core-ui.el ends here
