;;; core.el --- core initialization

;;; Commentary:
;; 
;;
;;
;;; Thanks to :
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;; https://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ieure/scratch-el
;; https://github.com/jopecko/dotfiles
;; http://sachachua.com
;; https://www.wisdomandwonder.com/article/10395/hide-uninteresting-files-in-dired-mode - Hiding unnecesary files in dired mode
;; http://www.mygooglest.com/fni/dot-emacs.html
;; https://github.com/sjrmanning/.emacs.d/blob/master/core/sm-defuns.el
;; https://github.com/xuchunyang/region-state.el
;;  https://github.com/larstvei/dot-emacs#global-scale-mode GLOBAL SCALE MODE ;; TODO: Make sure this works well, extend to work with minimap
;; http://xenodium.com/#installing-emacs-spaceline
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; https://github.com/k-talo/volatile-highlights.el
;; https://github.com/fgeller/highlight-thing.el
;; https://github.com/nschum/highlight-symbol.el
;; https://github.com/jordonbiondo/column-enforce-mode
;; https://github.com/xuchunyang/region-state.el
;; http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org
;; https://gitlab.com/emacs-stuff/indent-tools
;; https://github.com/malb/emacs.d/blob/master/malb.org
;; stackoverflow http://emacs.stackexchange.com/questions/18771/pop-up-documentation-with-pos-tip-or-popup
;; https://sriramkswamy.github.io/dotemacs/#orgheadline222
;; http://stackoverflow.com/questions/21955162/emacs-how-to-display-a-buffer-without-switching-window-and-without-raising-fram
;; https://github.com/emacsmirror/diminish
;; https://gist.github.com/antifuchs/9238468
;; http://sriramkswamy.github.io/dotemacs/
;; http://xenodium.com/#fishing-with-emacs
;; https://github.com/rejeep/emacs/blob/master/defuns.el
;; https://www.emacswiki.org/emacs/move-text.el and sk's init file
;; https://github.com/magnars/expand-region.el Magnars Expand Region
;; https://github.com/abo-abo/avy ;; still need to get used to it.
;; http://www.wilfred.me.uk/.emacs.d/init.html
;; http://wayback.archive.org/web/20010802153839/http://www.andersl.com:80/emacs/
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org
;; https://github.com/codemac/config/blob/master/emacs.d/boot.org ;; TODO: further edit this to do it for one buffer
;; https://github.com/sabof/stripe-buffer stripe buffer to read tables better [this: https://github.com/sabof/stripe-buffer/pull/14 applied for performance]
;; https://github.com/domtronn/all-the-icons.el all the icons 
;; https://github.com/howardabrams/dot-files/blob/master/emacs.org
;; http://chrishecker.com/images/4/4b/Remove_me.emacs
;; http://mbork.pl/2016-09-26_Emacs_now_suggests_shorter_ways_of_invocating_a_command
;; https://www.emacswiki.org/emacs/DelightedModes Delight to change line names, diminish does not have this functionality
;; https://github.com/afonso360/dotconf/  ;; Great Hydras
;; Amit P
;; Chris Hecker
;; Casey Muratory


;----{About User}---;

(setq user-full-name "C Andres Mariscal"
      user-mail-address "carlos.mariscal.melgar@gmail.com")

;----{Backup Dir}---;

;; change backup so that current directory does not clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))


;{Bootstrap Straight};

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; attempt to load a feature/library, failing silently
(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))


;{Ensure Executables};
;; Add any executables that must be found

(defun ensure-executable (exec)
  (unless (executable-find exec)
    (message (concat exec " not found in exec-path"))))

(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))

(defmacro require-soft (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if 'require' fails."
  `(require ,feature ,file 'noerror))



(load-require  'core-setup)
(load-require  'core-os)
(load-require  'core-ui)
(load-require  'core-editing)
(load-require  'core-navigation)
(load-require  'core-files)
(load-require  'core-functionality)
(load-require  'core-functions)
(load-require  'core-completion)
(load-require  'core-debug)
(load-require  'core-secrets)
(load-require  'core-performance)

(provide 'core)

;;; core.el ends here
