;;; init.el --- Tiqsi Emacs base

;;; Commentary: Bankrupcy highly inspired by Doom Emacs
;; 

;; These blocks are not part of GNU Emacs.
;;
;;; License: MIT

;; straight.el based repo TODO remove this

;; (setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)



;;;###autoload
(defun __FILE__ (&optional symbol)
  "Return the string name of file/buffer that is currently begin executed.

The first approach for getting this information is perhaps the
most pervasive and reliable.  But it the most low-level and not
part of a public API, so it might change in future
implementations.  This method uses the name that is recorded by
readevalloop of `lread.c' as the car of variable
`current-load-list'.

Failing that, we use `load-file-name' which should work in some
subset of the same places that the first method works.  However
`load-file-name' will be nil for code that is eval'd.  To cover
those cases, we try `buffer-file-name' which is initially
correct, for eval'd code, but will change and may be wrong if the
code sets or switches buffers after the initial execution.

As a last resort, you can pass in SYMBOL which should be some
symbol that has been previously defined if none of the above
methods work we will use the file-name value find via
`symbol-file'."
  ;; Not used right now:
  ;; Failing the above the next approach we try is to use the value of
  ;; $# - 'the name of this file as a string'. Although it doesn't
  ;; work for eval-like things, it has the advantage that this value
  ;; persists after loading or evaluating a file. So it would be
  ;; suitable if __FILE__ were called from inside a function.

  (cond

   ;; lread.c's readevalloop sets (car current-load-list)
   ;; via macro LOADHIST_ATTACH of lisp.h. At least in Emacs
   ;; 23.0.91 and this code goes back to '93.
   ((stringp (car-safe current-load-list)) (car current-load-list))

   ;; load-like things. 'relative-file-expand' tests in
   ;; test/test-load.el indicates we should put this ahead of
   ;; $#.
   (load-file-name)

   ;; Pick up "name of this file as a string" which is set on
   ;; reading and persists. In contrast, load-file-name is set only
   ;; inside eval. As such, it won't work when not in the middle of
   ;; loading.
   ;; (#$)

   ;; eval-like things
   ((buffer-file-name))

   ;; When byte compiling. FIXME: use a more thorough precondition like
   ;; byte-compile-file is somehwere in the backtrace or that
   ;; bytecomp-filename comes from that routine?
   ;; FIXME: `bytecomp-filename' doesn't exist any more (since Emacs-24.1).
   ((boundp 'bytecomp-filename) bytecomp-filename)

   (t (symbol-file symbol) ;; last resort
      )))


(defun load-relative (file-or-list &optional symbol)
  "Load an Emacs Lisp file relative to Emacs Lisp code that is in
the process of being loaded or eval'd.

FILE-OR-LIST is either a string or a list of strings containing
files that you want to loaded.  If SYMBOL is given, the location of
of the file of where that was defined (as given by `symbol-file' is used
if other methods of finding __FILE__ don't work."

  (if (listp file-or-list)
      (mapcar (lambda(relative-file)
                (load (relative-expand-file-name relative-file symbol)))
              file-or-list)
    (load (relative-expand-file-name file-or-list symbol)))
  )

(defun relative-expand-file-name(relative-file &optional opt-file)
  "Expand RELATIVE-FILE relative to the Emacs Lisp code that is in
the process of being loaded or eval'd.

WARNING: it is best to run this function before any
buffer-setting or buffer changing operations."
  (let ((file (or opt-file (__FILE__) default-directory))
        (prefix))
    (unless file
      ;; FIXME: Since default-directory should basically never be nil, this
      ;; should basically never trigger!
      (error "Can't expand __FILE__ here and no file name given"))
    (setq prefix (file-name-directory file))
    (expand-file-name (concat prefix relative-file))))

(defun load-require(file)
  (load-relative (symbol-name file))
  (require file))

(load-relative "core/core")
(require 'core)

(load-relative "modules/modes/modes")
(require 'modes)
;;; init.el ends here
