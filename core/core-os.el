;;; core-os.el --- Tiqsi OS dependant functionality

;;; Commentary:
;; 


;---{Determine OS}--;

(setq tiqsi-aquamacs (featurep 'aquamacs))
(setq tiqsi-linux (featurep 'x))
(setq tiqsi-win32 (not (or tiqsi-aquamacs tiqsi-linux)))
(setq tiqsi-console (eq (symbol-value 'window-system) nil))
(setq tiqsi-not-console (eq (eq (symbol-value 'window-system) nil)nil))

;-{Determine Emacs}-;

;; Emacs type --- are we running GNU Emacs?
(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
        (cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
        (cons 'progn body)))

(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
        (cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
        (cons 'progn body)))

;; Emacs version
(GNUEmacs
 (list emacs-version emacs-major-version emacs-minor-version
       system-type system-name system-configuration
       window-system
       (when (boundp 'aquamacs-version) aquamacs-version)))


;------{Win 32}-----;

(when tiqsi-win32 
  (setq tiqsi-makescript "build.bat")
  (setq tiqsi-font "PragmataPro")
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


(defun maximize-frame ()
    "Maximize the current frame"
     (interactive)
     (when tiqsi-aquamacs (aquamacs-toggle-full-frame))
     (when tiqsi-win32 (w32-send-sys-command 61488)))


(defun w32-restore-frame ()
  "Restore a minimized frame"
   (interactive)
   (w32-send-sys-command 61728))

(defun w32-maximize-frame ()
  "Maximize the current frame"
   (interactive)
   (w32-send-sys-command 61488))

;------{Linux}------;

(when tiqsi-linux
  (setq default-buffer-file-coding-system 'utf-8-unix)
  (setq tiqsi-makescript "./build.sh")
  (display-battery-mode 1) )


;------{Mac OS}-----;


(when tiqsi-aquamacs 
  (cua-mode 0) 
  (osx-key-mode 0)
  (tabbar-mode 0)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
  (setq tiqsi-makescript "./build.macosx"))

; Turn off the bell on Mac OS X
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;---{keybindings}---;
(define-key global-map "\ep" 'maximize-frame)
(define-key global-map "\ew" 'other-window)

(provide 'core-os)

;;; core-os.el ends here
