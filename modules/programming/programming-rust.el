;;; programming-rust.el --- Tiqsi Rust programming support  -*- lexical-binding: t -*-

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

(straight-require 'hydra)
(straight-require 'rust-mode)
(straight-require 'cargo)
(straight-require 'racer)
(straight-require 'parsec)

(straight-use-package
 '(evcxr
   :type git
   :host github
   :repo "serialdev/evcxr-mode"
   :config
   (add-hook 'rust-mode-hook #'evcxr-minor-mode)
   ))


(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook
 'rust-mode-hook
 '(lambda ()
    (setq tab-width 2)
    ;; (setq racer-cmd (concat (getenv "HOME") "/cargo/bin/racer")) ;; Rustup binaries PATH
    ;; (setq racer-rust-src-path (concat (getenv "HOME") (shell-command-to-string "echo `rustc --print sysroot`/lib/rustlib/src/rust/src")))
    (setq company-tooltip-align-annotations t)
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    (setq rust-format-on-save t)
    (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
    (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

(with-system gnu/linux

  (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "which cargo") ) )))
  (setq racer-rust-src-path
      (concat (string-trim
               (shell-command-to-string "rustc --print sysroot"))
              "/lib/rustlib/src/rust/src"))
  (setq racer-cmd (s-prepend (file-name-directory (shell-command-to-string "which racer")) "racer")  )

)



(with-system darwin
  (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "which cargo") ) )))
  (setq racer-rust-src-path
      (concat (string-trim
               (shell-command-to-string "rustc --print sysroot"))
              "/lib/rustlib/src/rust/src"))

  (if-command-exists "racer"
		     (setq racer-cmd
			   (s-prepend (file-name-directory
				       (shell-command-to-string "which racer")) "racer")  )
		     ))


(when tiqsi-win32
  (setq racer-cmd (s-prepend (file-name-directory (shell-command-to-string "where racer")) "racer.exe")  )
  (setq exec-path (append exec-path  '(`,(file-name-directory (shell-command-to-string "where cargo"))) ))
  (setq racer-rust-src-path
      (concat (string-trim
               (shell-command-to-string "rustc --print sysroot"))
              "/lib/rustlib/src/rust/src"))
  )


                                        ;-{Static checking}-;


(setq flymake-rust-use-cargo 1)


(defun tiqsi/cargo-doc-tree()
  "Get the mccabe complexity for this buffer."
  (interactive)
  (message
   (shell-command-to-string(message "tree -d %starget/doc -L 1 " (projectile-project-root)))))


(defun cargo-process-run-optimized()
  (interactive)
  (compile "cargo run --release"))

(defun cargo-process-build-optimized()
  (interactive)
  (compile "cargo build --release"))



                                        ;-----{Hydras }-----;

(defhydra hydra-rust (:color pink :hint nil)
  "
^Rust Cargo commands^
----------------------------------------------------------------------------------
_r_: Run          _i_: Init          _u_: Update               _+r_: Release O
_x_: Run-example  _n_: New           _c_: Repeat               _+b_: Build O
_b_: Build        _f_: Current-test  _e_: Bench
_l_: Clean        _s_: Search        _o_: Current-file-tests
_d_: Doc          _t_: Test          _m_: Fmt
_|_: Doc Tree     _k_: Check         _q_: Clippy
"
  ("e"   cargo-process-bench :color blue)
  ("b"   cargo-process-build :color blue)
  ("l"   cargo-process-clean :color blue)
  ("d"   cargo-process-doc :color blue)
  ("n"   cargo-process-new :color blue)
  ("i"   cargo-process-init :color blue)
  ("r"   cargo-process-run :color blue)
  ("x"   cargo-process-run-example :color blue)
  ("s"   cargo-process-search :color blue)
  ("t"   cargo-process-test :color blue)
  ("u"   cargo-process-update :color blue)
  ("c"   cargo-process-repeat :color blue)
  ("f"   cargo-process-current-test :color blue)
  ("o"   cargo-process-current-file-tests :color blue)
  ("m"   cargo-process-fmt :color blue)
  ("+r" cargo-process-run-optimized :color blue)
  ("+b" cargo-process-build-optimized :color blue)
  ("|"   tiqsi/cargo-doc-tree :color blue)
  ("k"   cargo-process-check color: red)
  ("q" cargo-process-clippy :color blue)
  ("ESC" nil "Exit"))



(defun current-line ()
     (string-to-number (car (cdr(s-split " " (what-line)))))
     )

(format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name)))

(defun tiqsi--rust-print-src()
    (interactive)
  (let ((match-end  (s-split "END"   (shell-command-to-string (format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name))) ))))
  (if (s-equals? (car match-end) "")
      1
    (let ((match-match (s-trim(car(cdr(s-split "MATCH" (car match-end)))))  ))
(cl-multiple-value-bind
    (name row col path type extra)
    (s-split ","  match-match )

  (with-temp-buffer
    (insert-file-contents path)
    (goto-line (string-to-number row))
    (print (buffer-substring-no-properties (point)
    (search-forward "}" )))
    )
  )
))))



(defun tiqsi--racer-insert-struct-point()
    (interactive)
  (let (  (b-name (buffer-name)) (match-end  (s-split "END"   (shell-command-to-string (format-mode-line (s-prepend "racer find-definition %l %c " (buffer-file-name))) ))))
  ;; (let ((match-end  (s-split "END"  ttt )) (b-name (buffer-name)))
  (if (s-equals? (car match-end) "")
      1
    (let ((match-match (s-trim(car(cdr(s-split "MATCH" (car match-end)))))  ))
      (cl-multiple-value-bind
	  (name row col path type extra)
	  (s-split ","  match-match )

	(with-temp-buffer
	  (insert-file-contents path)
	  (goto-line (string-to-number row))

	  (let ((mm
		 (buffer-substring-no-properties (point)
						 (search-forward "}" )) ))
	    (with-current-buffer b-name
	      (backward-kill-word 1)
		(insert name)
	    (insert "{")
	    (newline)
	    (cl-loop for k in 
		     (cl-loop for kv in 
			      (cl-remove-if  (lambda(item) (string= "" item)) (mapcar 's-trim (s-split ","  (cadr(s-split "{" (car(s-split "}" mm)))))))
			      collect (car (s-split ":" kv))
			      )
		     do (progn
			  (insert k)
			  (insert ":")
			  (insert "  ,")
			  (newline)
			  )
		     )
	    (insert "}")
	    ))
	  ))))))

(string-to-number (format-mode-line "%l"))

(string-to-number (format-mode-line "%c"))



(defun racer-ui-tooltip ()
  "Show the docstring in a tooltip.
The tooltip's face is `racer-tooltip'
See `racer-describe'."
  (interactive)
  (-some-> (symbol-at-point)
           (symbol-name)
           (racer--describe)
           (with-current-buffer (concat "\n" (buffer-string) "\n\n"))
           (message 'racer-tooltip nil nil 1000)))


; _ _ _ _ _ _ _ _ _ _ _ _    /¯¯¯ Keybindings ¯¯¯\_ _ _ _ _ _ _ _ _ _ _ _   ;


(define-key rust-mode-map (kbd "C-c C-c") 'hydra-rust/body )
(define-key rust-mode-map (kbd "C-t") 'racer-ui-tooltip )

(define-key rust-mode-map (kbd "M-p") 'tiqsi--rust-print-src )
(define-key rust-mode-map (kbd "M-i") 'tiqsi--racer-insert-struct-point )

(define-key rust-mode-map (kbd "C-c c") 'tiqsi-compile--no-message)
(define-key rust-mode-map (kbd "C-c C-r") 'tiqsi-compile--reset-string)
(define-key rust-mode-map (kbd "C-c n") 'flymake-goto-next-error)


; ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯    \_ _ Keybindings _ _/¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯ ¯   ;

(provide 'programming-rust)

;;; programming-rust.el ends here
