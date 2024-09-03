;;; modes-helm.el --- Tiqsi helm configuration  -*- lexical-binding: t -*-

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

(straight-require 'helm-rg)

(use-package helm
  :straight t
  :ensure t
  :config
  (progn
    ;; Resize
    (helm-autoresize-mode 1)
    (setq helm-autoresize-max-height 30)
    (setq helm-autoresize-min-height 10)

    ;; Performance
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
      helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
      helm-quick-update t
      helm-M-x-requires-pattern nil
      helm-ff-skip-boring-files t)

    ;; M-x features
    (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
    (setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

    (setq helm-boring-buffer-regexp-list
      '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))


    (setq helm-boring-file-regexp-list
      '("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*tramp" "\\*Minibuf" "\\*epc"))

    ;; Horizontal

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

    (setq helm-split-window-default-side 'below)

    ;; Semantic Imenu

    (setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)

    (setq helm-lisp-fuzzy-completion t)

    ;; Locate any file

                                        ;(shell-command "choco install; everything")
    (setq helm-locate-fuzzy-match t)
    (setq helm-apropos-fuzzy-match t)

    (when (executable-find "curl")
      (setq helm-net-prefer-curl t))))

;; Straight install requirements

(straight-require 'helm-smex)
(straight-require 'hydra)
(straight-require 'helm-projectile)

(with-system darwin
  (setq helm-rg-ripgrep-executable "/usr/local/bin/rg")
  (setq helm-rg-default-directory 'git-root)
  )

(straight-require 'helm-etags-plus) ;; Helm Etags support
(straight-require 'ac-helm) ;; Interactive ac with Helm
(straight-require 'helm-pydoc) ;; Helm Python documentation
(straight-require 'helm-descbinds) ;; Keybindings interactive search
(with-system win32
  (straight-require 'helm-w32-launcher)) ;; Start Menu Support
(straight-require 'helm-chrome) ;; Chrome Bookmarks support


;; USE BUFFER

;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame t
;;       helm-use-undecorated-frame-option t)

                                        ;-{Posframe render}-;


(defun helm-posframe()
  (interactive)
  (setq helm-display-function #'helm-posframe-display)

  (defvar helm-posframe-buffer nil)

  (defun helm-posframe-display (buffer &optional _resume)
    (setq helm-posframe-buffer buffer)
    (posframe-show
      buffer
      :background-color "#161616"
      :foreground-color "#DAB98F"
      :poshandler #'posframe-poshandler-frame-bottom-left-corner
      ;; :width (/ (window-width) 2)
      :width (+ (window-width) 2)
      ;; :height helm-display-buffer-height
      :height (/ helm-display-buffer-height 3)
      :respect-header-line t))

  (defun helm-posframe-cleanup ()
    (posframe-hide helm-posframe-buffer))

  (add-hook 'helm-cleanup-hook #'helm-posframe-cleanup))

(defun helm-posframe-cleanup()
  (interactive)
  (setq helm-display-function 'helm-default-display-buffer))

;; (helm-posframe)


;; Fuzzy search with helm
(use-package helm-flx
  :straight t
  :ensure t
  :config
  (progn
    (helm-flx-mode 1)
    (setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default
    ))

;; Interactive Silver Searcher with Helm

(use-package helm-ag
  :straight t
  :ensure t
  :config
  (progn
    (custom-set-variables
      '(helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
      '(helm-ag-command-option "--all-text")
      '(helm-ag-insert-at-point 'symbol)
      '(helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'")))
    (setq ag-highlight-search t)))

;; Dash Documentation Support

(use-package helm-dash
  :straight t
  :ensure t
  :config (progn
            (eval-after-load "helm-dash"
              '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))
            ;; (setq-local helm-dash-docsets-path "../../tools/dash/")
            (setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))
            ;; (setq helm-dash-common-docsets '(("NumPy") ("Redis") ("Pandas")))
            ;; (setq helm-dash-browser-func 'browse-url-generic)
            ;; (setq helm-dash-browser-func 'browse-url)
            (setq helm-dash-browser-func 'eww)))

                                        ;-{Patch Helpful.el};

;; (defun helm-describe-function (func)
;;   "FUNC is symbol or string."
;;   (cl-letf (((symbol-function 'message) #'ignore))
;;      (helpful-function (helm-symbolify func))
;;      ))


;; (defun helm-describe-function (func)
;;   "FUNC is symbol or string."
;;   (cl-letf (((symbol-function 'message) #'ignore))
;;     (condition-case err
;;      (progn
;;        (helpful-function (helm-symbolify func)))
;;       (argument-error
;;        (progn
;;       (describe-function (helm-symbolify func)))))
;;     nil
;; ))








                                        ;------{Google}-----;
                                        ;TODO helm-youtube support                                                                          ;


                                        ;--{Eshell History}-;

(add-hook 'eshell-mode-hook
  #'(lambda ()
      (define-key eshell-mode-map (kbd "C-c C-l")  'helm-eshell-history)))


                                        ;---{Buffer Funcs}--;

(defun amitp/buffer-file-names ()
  "A list of filenames for the current buffers"
  (loop for filename in (mapcar 'buffer-file-name (buffer-list))
    when filename
    collect filename))

(defun amitp/helm-for-files ()
  "Global filename match, over all files I typically open"
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil)
         (recentf-list
           (mapcar 'abbreviate-file-name
             (append (amitp/buffer-file-names)
               (helm-skip-boring-files (directory-files default-directory t))
               recentf-list
                                        ;amitp/global-file-list
               ))))
    (helm
      :sources '(helm-source-recentf)
      :buffer "*helm for files*")))


(defun amitp/helm-locate (candidate)
  "Fallback when helm-recentf doesn't find what I want"
  (interactive)
  (helm :sources '(helm-source-locate helm-source-find-files)
    :buffer "*helm locate*"
    :input helm-input
    :resume 'noresume))

(defun amitp/helm-for-files-fallback ()
  (interactive)
  (helm-quit-and-execute-action 'amitp/helm-locate))


                                        ;--{Prog Headlines}-;

(defun helm-objc-headlines ()
  (interactive)
  (helm :sources '(((name . "Objective-C Headlines")
                     (volatile)
                     (headline  "^[-+@]\\|^#pragma mark")))))

(defun helm-clojure-headlines ()
  "Display headlines for the current Clojure file."
  (interactive)
  (helm-mode t)
  (helm :sources '(((name . "Clojure Headlines")
                     (volatile)
                     (headline "^[;(]")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: helm-descbinds                      ;;
;;                                              ;;
;; GROUP: Convenience -> Helm -> Helm Descbinds ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(helm-descbinds-mode)



;; Swoop Editing

(use-package helm-swoop
  :straight t
  :ensure t
  :config
  (progn
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)

    ;; If you prefer fuzzy matching
    (setq helm-swoop-use-fuzzy-match t)

    ;; If there is no symbol at the cursor, use the last used words instead.
    (setq helm-swoop-pre-input-function
      (lambda ()
        (let (($pre-input (thing-at-point 'symbol)))
          (if (eq (length $pre-input) 0)
            helm-swoop-pattern ;; this variable keeps the last used words
            $pre-input))))))

                                        ;----{Multi term}---;

;; (package-install 'helm-mt)
;; (try-require 'helm-mt)
;; (when tiqsi-linux
;;   (helm-mt/reroute-terminal-functions t))

;; (define-key global-map (kbd "C-c ho") 'helm-occur)

                                        ;---{TODO re add}---;

;; (global-set-key "\C-chr"
;;                 (lambda () (interactive)
;;                   (imenu--menubar-select imenu--rescan-item)))


;;-----{HYDRAS!}-----;

(defhydra hydra-gtags (:color blue :hint nil)
  "
` ` _ _ _ _ _ _ _ _ _` ` ` ` | ^Symbols^                      ^History^
`  |_ _ _ _ _ _ _ _ _ |` ` ` | -----------------------------------------------------------
` ` ` \\\\ \\ \\ // ///` ` ` ` ` | _t_: Find tags                 _h_: Show tags stack
` ` `  \\\\_|_|_| // ` ` ` ` ` | _T_: Find tags other window    _n_: Next tag in history
 Tiqsi |        |` ` ` ` ` ` | _r_: Find rtag                 _p_: Previous tag in history
` ` `  | o    o | Emacs` ` ` | _s_: Select tag
` ` _ _ _ _ _ _ _ _ _  ` ` ` | _x_: Xref find
`  |_ _ _ _ _ _ _ _ _ |` ` ` | _l_: List tags in this function
` ` ` ` \\_ _ _ /` ` ` ` ` ` `| "
  ("r" helm-gtags-find-rtag)
  ("t" helm-gtags-find-tag)
  ("T" helm-gtags-find-tag-other-window)
  ("s" helm-gtags-select)
  ("x" xref-find-apropos)
  ("h" helm-gtags-show-stack)
  ("p" helm-gtags-previous-history)
  ("n" helm-gtags-next-history)
  ("l" helm-gtags-tags-in-this-function)
  ("ESC" nil "Exit"))


(defhydra hydra-helm-ag (:color blue :hint nil)
  "
                                                                  ^Search^                 ^Interactive^
` ` _ _ _ _ _ _ _ _ _` ` ` ` | ------------------------------------------------------
`  |_ _ _ _ _ _ _ _ _ |` ` ` | _A_: Search              _a_: Interactive search
` ` ` \\\\ \\ \\ // ///` ` ` ` ` | _F_: Search this file    _f_: Interactive search this file
` ` `  \\\\_|_|_| // ` ` ` ` ` |  _S_: Search project      _s_: Interactive search project
 Tiqsi |        |` ` ` ` ` ` |    _B_: Search buffers      _b_: Interactive search buffers
` ` `  | o    o | Emacs` ` ` |
` ` _ _ _ _ _ _ _ _ _  ` ` ` |
`  |_ _ _ _ _ _ _ _ _ |` ` ` |
` ` ` ` \\_ _ _ /` ` ` ` ` ` `|
"
  ("A" helm-ag)
  ("F" helm-ag-this-file)
  ("S" helm-ag-project-root)
  ("B" helm-ag-buffers)
  ("a" helm-do-ag)
  ("f" helm-do-ag-this-file)
  ("s" helm-do-ag-project-root)
  ("b" helm-do-ag-buffers)
  ("ESC" nil "Exit"))




(use-package helm-ag
  :defer t
  :straight t
  )

(defun enable-ag ()
  (interactive)
  (leader-set-key-for-mode major-mode "s" 'hydra-helm-ag/body)
  (which-key-add-major-mode-key-based-replacements major-mode "SPC s" "Ag"))

(defhydra hydra-window-stuff (:hint nil)
  "
                                                                            Split: _v_ert  _s_:horz
` ` _ _ _ _ _ _ _ _ _` ` ` ` | Delete: _c_lose  _o_nly
`  |_ _ _ _ _ _ _ _ _ |` ` ` | Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
` ` ` \\\\ \\ \\ // ///` ` ` ` ` | Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
` ` `  \\\\_|_|_| // ` ` ` ` ` | Winner: _u_ndo  _r_edo
 Tiqsi |        |` ` ` ` ` ` | Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
` ` `  | o    o | Emacs` ` ` | Move: _a_:up  _z_:down  _i_menu
` ` _ _ _ _ _ _ _ _ _  ` ` ` |
`  |_ _ _ _ _ _ _ _ _ |` ` ` |
` ` ` ` \\_ _ _ /` ` ` ` ` ` `|
"


  ("z" scroll-up-line)
  ("a" scroll-down-line)
  ("i" (lambda() (print "not implemented"))) ;; Used to be idomenu

  ("u" winner-undo)
  ("r" winner-redo)

  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("p" previous-buffer)
  ("n" next-buffer)
  ("b" switch-buffer)
  ("f" find-file)
  ("F" projectile-find-file)

  ("s" split-window-below)
  ("v" split-window-right)

  ("c" delete-window)
  ("o" delete-other-windows)

  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)

  ("q" nil))

;; Hydra for in Helm
(defhydra helm-like-unite ()
  "vim movement"
  ("?" helm-help "help")
  ("q" nil "exit")
  ("<SPC>" helm-toggle-visible-mark "mark")
  ("d" helm-buffer-run-kill-persistent "Delete selection")
  ("a" helm-toggle-all-marks "(un)mark all")
  ;; not sure if there's a better way to this
  ("/" (lambda ()
         (interactive)
         (execute-kbd-macro [?\C-s]))
    "search")
  ("v" helm-execute-persistent-action)
  ("g" helm-beginning-of-buffer "top")
  ("h" helm-previous-source)
  ("l" helm-next-source)
  ("G" helm-end-of-buffer "bottom")
  ("j" helm-next-line "down")
  ("k" helm-previous-line "up")
  ("i" nil "cancel"))



;; (define-key helm-map "jk" 'helm-like-unite/body)

                                        ;-------------{Keybindings}------------;

;; (defhydra hydra-helm-bookmarks (:color pink
;;                              :hint nil)
;;   "
;; ^^             ^Movement^           ^Search^          ^Misc
;; ^^^^^^^^-----------------------------------------------------------------
;; _i_: Imenu-Semantic          _b_: Check buffers        _o_: Occurrences       _k_: Show Kill Ring
;; _e_: Etags          _U_: unmark up     _h_: Apropos          _SPC_: Show Mark Ring
;; _d_: delete        ^ ^                _i_: Imenu-Semantic       _O_: multi-occur
;; _d_: delete        ^ ^                _i_: Imenu-Semantic       _O_: multi-occur
;; _d_: delete        ^ ^                _i_: Imenu-Semantic       _O_: multi-occur

;; _D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
;; _j_: jedi:related-names
;; "
;;   ("m" Buffer-menu-mark)
;; )

;; TODO Mode specific hydras send to something else
(defhydra hydra-helm-menu (:color pink
                            :hint nil)
  "
^Code^                ^Movement^           ^Search^               ^Misc
^^^^^^^^---------------------------------------------------------------------------------
_i_: Imenu-Semantic    _f_: Filesystem     _o_: Occurrences       _k_: Show Kill Ring
_e_: Etags             ^ ^                 _h_: Apropos           _SPC_: Show Mark Ring
_r_: Regexp            ^ ^                 _g_: google suggest    _bc_: Chrome bookmarks
_le_: Lisp Eval        ^ ^                 _i_: Imenu-Semantic    _x_: helm-register
_sws_: Swoop           ^ ^                 _a_: Imenu-anywhere    _cp_: Colour picker
_swm_: Swoop multi     ^ ^                 _m_: man woman pages   _ca_: Calculator
_swa_: Swoop all       ^ ^                 _lf_: Locate Files     ^ ^
_swi_: Swoop from Is   ^ ^                 _ss_: Ag search        ^ ^
_pd_: Python Doc       ^ ^                 _sp_: Ag Project-root  ^ ^
_j_: jedi:related-names
"
  ;; ("w" hydra-window-stuff :color blue)
                                        ;{references using jedi};
  ("j" helm-jedi-related-names :color blue)
                                        ;----{Ocurrences}---;
  ("o" helm-occur :color blue)
                                        ;----{helm etags}---;
  ("e" helm-etags-plus-select :color blue)
                                        ;{semantic or imenu};
  ("i" helm-semantic-or-imenu :color blue)
                                        ;-----{Apropos}-----;
  ("h" helm-apropos :color blue)
                                        ;-{browse kill ring};
  ("k" helm-show-kill-ring :color blue)
                                        ;-{browse mark ring};
  ("SPC" helm-mark-ring :color blue)
                                        ;--{google suggest}-;
  ("g" helm-google-suggest :color blue)
                                        ;----{bookmarks}----;
  ("bb" helm-bookmarks :color blue)
                                        ;-{chrome bookmarks};
  ("bc" helm-chrome-bookmarks :color blue)
                                        ;--{anywhere Imenu}-;
  ("a" helm-imenu-anywhere :color blue)
                                        ;-----{Regexp }-----;
  ("r" helm-regexp :color blue)
                                        ;----{Registers}----;
  ("x" helm-register :color blue)
                                        ;---{Colour picker}--;
  ("cp" helm-colors :color blue)
                                        ;----{Calculator}---;
  ("ca" helm-calcul-expression :color blue)
                                        ;----{man-pages}----;
  ("m" helm-man-woman :color blue)
                                        ;----{Find files}---;
  ("f" helm-find-files :color blue)
                                        ;---{Locate Files}--;
  ("lf" helm-locate :color blue)
                                        ;----{Eval elisp}---;
  ("le" helm-eval-expression-with-eldoc :color blue)
                                        ;------{Swoop}------;
  ("sws" helm-swoop :color blue)
  ("swm" helm-multi-swoop :color blue)
  ("swa" helm-multi-swoop-all :color blue)
  ("swi" helm-swoop-from-isearch :color blue)
                                        ;{Python documentation};
  ("pd" helm-pydoc :color blue)
                                        ;--{Search with Ag}-;
  ("ss"   (if tiqsi-osx
            (call-interactively 'helm-ag)
            ag-highlight-search
            ) :color blue)
  
                                        ;-{Ag project root}-;
  ("sp" helm-ag-project-root :color blue)
  ("c" nil "cancel")
  ("q" quit-window "quit" :color blue))

(define-key global-map (kbd "C-c h") 'hydra-helm-menu/body)



                                        ;{TODO move this to a hydra specific...;

(defhydra hydra-buffer (:color blue :columns 3)
  "
                Buffers :
  "
  ("n" next-buffer "next" :color red)
  ("b" ivy-switch-buffer "switch")
  ("B" ibuffer "ibuffer")
  ("p" previous-buffer "prev" :color red)
  ("C-b" buffer-menu "buffer menu")
  ("N" evil-buffer-new "new")
  ("d" kill-this-buffer "delete" :color red)
  ;; don't come back to previous buffer after delete
  ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
  ("s" save-buffer "save" :color red))

(require 'cl-lib)
(require 'helm)

(defun get-python-functions (file)
  "Extract Python function definitions from FILE."
  (if (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((result '())
             (re "^def\\s-+\\(\\w+\\)\\s-*("))
        (while (re-search-forward re nil t)
          (push (cons (match-string-no-properties 1) (cons file (point))) result))
        (message "File: %s | Functions: %s" file result)
        result))
    (progn
      (message "File not readable: %s" file)
      '())))


(defun find-duplicate-python-functions (dir)
  "Recursively find duplicate Python function definitions in a directory."
  (let* ((files (directory-files-recursively dir "\\.py$"))
          (functions (cl-remove nil (mapcar 'get-python-functions files)))
          (grouped (seq-group-by 'car functions)))
    (message "FILES: %s" files)
    (message "FUNCTIONS: %s" functions)
    (message "GROUPED: %s" grouped)
    (cl-remove-if (lambda (x) (= (length x) 1)) grouped)))



(defun helm-python-duplicate-functions-source (duplicates)
  "Create a Helm source for duplicate Python functions from DUPLICATES."
  (helm-build-sync-source "Duplicate Python Functions"
    :candidates (mapcan (lambda (group)
                          (let* ((funcs (cl-remove-if-not #'cdr group))
                                  (func-name (caar funcs))
                                  (file-info (cdar funcs))
                                  (file (car file-info))
                                  (line (cdr file-info))
                                  (dir (when (stringp file) (file-name-nondirectory (directory-file-name file))))
                                  (loc (when dir (format "%s:%s" dir line))))
                            (when loc
                              (append (list (cons (format "%s (%s)" func-name loc)
                                              (cl-loop for (name . info) in (cdr funcs)
                                                for (file . line) = info
                                                for dir = (when (stringp file) (file-name-nondirectory (directory-file-name file)))
                                                when dir
                                                collect (format "%s (%s:%s)" name dir line))))
                                (cl-loop for (name . info) in (cdr funcs)
                                  for (file . line) = info
                                  for dir = (when (stringp file) (file-name-nondirectory (directory-file-name file)))
                                  when dir
                                  collect (cons (format "%s (%s:%s)" name dir line) info))))))
                  (seq-group-by #'car duplicates))))


(defun check-duplicate-python-functions (dir)
  "Check for duplicate Python function definitions in DIR and display them with Helm."
  (interactive "DSelect directory: ")
  (let ((duplicates (find-duplicate-python-functions dir)))
    (message "DUPLICATES: %s" duplicates)
    (if duplicates
      (helm :sources (helm-python-duplicate-functions-source duplicates))
      (message "No duplicate Python function definitions found."))))





(defun my-helm-highlight-matches (candidates _source)
  (mapcar (lambda (candidate)
            (let ((display (car candidate))
                   (match (cadr candidate)))
              (cons (replace-regexp-in-string
                      (concat "\\(\\s-\\|^\\)\\(" (regexp-quote match) "\\)\\(\\s-\\|$\\)")
                      "\\1**--\\2--**\\3"
                      display)
                candidate)))
    candidates))

(defun my-helm-highlight-matches (candidates _source)
  (mapcar (lambda (candidate)
            (let ((display (car candidate))
                   (match (cadr candidate)))
              (cons (replace-regexp-in-string
                      (regexp-quote match)
                      (concat "**--" match "--**")
                      display)
                candidate)))
    candidates))



(defun my-helm-source (tuples)
  `((name . "Custom Helm Buffer")
     (candidates . ,(lambda () tuples))
     (filtered-candidate-transformer my-helm-highlight-matches)
     (action . (lambda (candidate)
                 (message "Selected: %s" candidate)))))



(defun python-functions-in-dir (directory)
  (interactive "DDirectory: ")
  (let ((py-files (directory-files-recursively directory "\\.py$"))
         (result '())
         (initial-buffer (current-buffer)))
    (dolist (file py-files)
      (when (and (not (file-symlink-p file)) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*def\\s-+\\(\\w+\\)\\s-*(" nil t)
            (let* ((function-name (match-string 1))
                    (line-number (line-number-at-pos))
                    (short-file-path (mapconcat 'identity (last (split-string file "/" t) 3) "/")))
              (push (list (format "%s :: %d :: %s" short-file-path line-number function-name) function-name file) result)))
          (unless (eq (current-buffer) initial-buffer)
            (kill-buffer)))))
    (my-helm-buffer result)))


(defun my-helm-buffer (tuples)
  (helm :sources
    (helm-build-sync-source "Custom Helm Buffer"
      :candidates (lambda () tuples)
      :filtered-candidate-transformer 'my-helm-highlight-matches
      :action (lambda (candidate)
                (let* ((info (car candidate))
                        (file (nth 2 candidate))
                        (file-line (progn (string-match "\\(.*\\) :: \\([0-9]+\\) :: .*" info)
                                     (list (match-string 1 info) (match-string 2 info)))))
                  (find-file file)
                  (goto-line (string-to-number (cadr file-line)))
                  (recenter)))
      :multiline t)
    :buffer "*helm custom buffer*"))



(defun python-functions-in-dir (directory)
  (interactive "DDirectory: ")
  (let ((py-files (directory-files-recursively directory "\\.py$"))
         (result '())
         (initial-buffer (current-buffer)))
    (dolist (file py-files)
      (when (and (not (file-symlink-p file)) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*def\\s-+\\([_a-zA-Z0-9]+\\)\\s-*(" nil t)
            (let* ((function-name (match-string 1))
                    (line-number (line-number-at-pos))
                    (short-file-path (mapconcat 'identity (last (split-string file "/" t) 3) "/")))
              (push (list (format "%s :: %d :: %s" short-file-path line-number function-name) function-name file) result)))
          (unless (eq (current-buffer) initial-buffer)
            (kill-buffer)))))
    (my-helm-buffer result)))


(defun python-duplicate-functions-in-dir (directory)
  (interactive "DDirectory: ")
  (let ((py-files (directory-files directory t "\\.py$"))
         (function-names '())
         (duplicates '())
         (initial-buffer (current-buffer)))
    (dolist (file py-files)
      (when (and (not (file-symlink-p file)) (file-exists-p file))
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (while (re-search-forward "^\\s-*def\\s-+\\([_a-zA-Z0-9]+\\)\\s-*(" nil t)
            (let* ((function-name (match-string 1))
                    (line-number (line-number-at-pos))
                    (short-file-path (mapconcat 'identity (last (split-string file "/" t) 3) "/")))
              (push (list (format "%s :: %d :: %s" short-file-path line-number function-name) function-name file) function-names)))
          (unless (eq (current-buffer) initial-buffer)
            (kill-buffer)))))
    (dolist (fn function-names)
      (when (> (cl-count (cadr fn) function-names :test (lambda (a b) (equal a (cadr b)))) 1)
        (push fn duplicates)))
    (let ((sorted-duplicates (sort (delete-dups duplicates) (lambda (a b) (string< (cadr a) (cadr b))))))
      (my-helm-buffer sorted-duplicates))))

(defun ag-search-and-highlight (query)
  "Perform ag search and highlight results in another buffer"
  (interactive "sEnter search query: ")
  (let* ((results-buffer (get-buffer-create "*Ag Search Results*"))
          (ag-command "ag")
          (ag-arguments `("-l" "--nobreak" "--nocolor" "--hidden" ,query))
          (full-directory (shell-quote-argument (expand-file-name "."))))
    (with-current-buffer results-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "Ag search results for '%s' in %s:\n\n" query default-directory)))
    (let ((process (apply 'start-file-process "ag-search" results-buffer ag-command ag-arguments)))
      (set-process-sentinel process
        (lambda (p e)
          (when (eq (process-status p) 'exit)
            (with-current-buffer (process-buffer p)
              (goto-char (point-max))
              (if (= (process-exit-status p) 0)
                (progn
                  (goto-char (point-min))
                  (if (re-search-forward "^\\([^:]+\\):\\([0-9]+\\):" nil t)
                    (insert "\nSearch completed. Issues found.\n")
                    (insert "\nSearch completed. No matches found.\n")))
                (insert (format "\nSearch failed with error code %d.\n" (process-exit-status p))))
              (highlight-regexp (format "\\(%s\\)" (regexp-quote query)) 'hi-yellow)
              (setq buffer-read-only t)
              (display-buffer (process-buffer p)))))))))



(require 'async)

(defun helm-git-repos-with-remote-pred (word)
  "Find Git repositories with remote origin URL containing WORD asynchronously."
  (interactive "sEnter search word: ")
  (let ((search-command (format "find . -type d -name .git -exec sh -c 'cd \"{}\"/.. && git config --get remote.origin.url | grep -q \"%s\" && pwd' \\;" word)))
    (message "Search Command: %s" search-command)
    (async-start
      `(lambda ()
         (shell-command-to-string ,search-command))
      (lambda (result)
	(message "Command Result: %s" (or result "nil"))
	(if (or (null result) (string= result ""))
          (message "No repositories found with the specified remote URL.")
          (let ((repos (split-string result "\n" t)))
            (helm
              :sources
              `(((name . "Git Repos with Remote")
		  (candidates . ,repos)
		  (action . (lambda (candidate) (find-file candidate))))))))))))



(defvar helm-peek-buffer nil)
(defvar helm-peek-original-window nil)

(defun helm-peek ()
  "Peek at the content of the buffer on the pointed file."
  (interactive)
  (let ((candidate (helm-get-selection)))
    (message "helm-peek candidate: %s" candidate) ; Debug logging
    (if candidate
      (progn
        (setq helm-peek-original-window (selected-window))
        (let* ((components (split-string candidate ":"))
                (file (car components))
                (line (string-to-number (nth 1 components))))
          (message "helm-peek file: %s, line: %d" file line) ; Debug logging
          (setq helm-peek-buffer (find-file-noselect file))
          (let ((peek-window (display-buffer helm-peek-buffer '((display-buffer-reuse-window display-buffer-at-bottom)
                                                                 (inhibit-same-window . t)))))
            (with-selected-window peek-window
              (with-current-buffer helm-peek-buffer
                (goto-char (point-min))
                (forward-line (1- line))
                (recenter))))
          (add-hook 'helm-after-update-hook 'helm-peek-hide-buffer)))
      (message "No file selected"))))

(defun helm-peek-hide-buffer ()
  "Hide the peek buffer when helm input is idle."
  (when helm-peek-buffer
    (let ((win (get-buffer-window helm-peek-buffer)))
      (when win
        (delete-window win)))
    (setq helm-peek-buffer nil)
    (remove-hook 'helm-after-update-hook 'helm-peek-hide-buffer)))

(with-eval-after-load 'helm
  (define-key helm-map (kbd "<f5>") 'helm-peek))

(with-eval-after-load 'helm-ag
  (define-key helm-ag-map (kbd "<f5>") 'helm-peek))


                                        ;---{Keybindings}---;

(global-set-key (kbd "M-x") 'helm-smex) ;; Offloaded to selectrum now
(global-set-key (kbd "M-X") 'helm-smex-major-mode-commands) ;; Offloaded to selectrum now
                                        ;(global-set-key ( kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))


(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history) ;; For Minibuffer
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring) ;; For Shell
                                        ;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
                                        ;(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
                                        ;(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
;; (define-key global-map (kbd "C-c b") 'helm-mini) ;; Browse Open Buffers ;; Offloaded to selectrum now

(global-set-key (kbd "C-<return>") 'ac-complete-with-helm)

(define-key global-map (kbd "M-i") 'helm-swoop-back-to-last-point)



(provide 'modes-helm)

;;; modes-helm.el ends here





