;;; core.el --- core initialization

;;; Commentary:
;; 
;;
;;
;;; Thanks to :
;; http://www.wisdomandwonder.com/wordpress/wp-content/uploads/2014/03/C3F.html
;; https://sites.google.com/site/steveyegge2/effective-emacs
;; https://github.com/ieure/scratch-el
;; http://sachachua.com
;; http://www.mygooglest.com/fni/dot-emacs.html
;; https://github.com/sjrmanning/.emacs.d/blob/master/core/sm-defuns.el
;; https://github.com/xuchunyang/region-state.el
;; http://xenodium.com/#installing-emacs-spaceline
;; https://github.com/antonj/Highlight-Indentation-for-Emacs
;; https://github.com/k-talo/volatile-highlights.el
;; https://github.com/fgeller/highlight-thing.el
;; https://github.com/nschum/highlight-symbol.el
;; https://github.com/jordonbiondo/column-enforce-mode
;; https://github.com/xuchunyang/region-state.el
;; https://github.com/emacsmirror/diminish
;; https://github.com/domtronn/all-the-icons.el all the icons 
;; https://www.emacswiki.org/emacs/DelightedModes Delight to change line names, diminish does not have this functionality
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



(load-require  'core-setup)
(load-require  'core-os)
(load-require  'core-ui)
(load-require  'core-files)

(provide 'core)

;;; core.el ends here
