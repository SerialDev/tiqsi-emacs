;;; modes-dash.el --- Tiqsi dash configuration support

;;; Commentary:
;; 
;                  TODO compatibility issues with non UNIX OS                  ;

(eval-after-load "helm-dash"
  '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))
  ;; (setq-local helm-dash-docsets-path "../../tools/dash/")

(setq helm-dash-browser-func 'browse-url)
(setq helm-dash-browser-func 'eww)


;(defun go-doc ()
;  (interactive)
;  (setq-local helm-dash-docsets '("Go")))
;; (setq-local helm-dash-docsets-path "C:/Users/Carlos/Desktop/Workspace/emacs_dir/NewEmacs/tools/dash/")
;(add-hook 'go-mode-hook 'go-doc)
;(defun py-doc ()
;  (interactive)
;  (setq-local helm-dash-docsets '("Python_3"))
;  (setq-local helm-dash-docsets '("Pandas"))
;  (setq-local helm-dash-docsets '("Numpy")))
;  
;(require 'helm-dash)

;      helm-dash-min-length 3
;      ;; helm-dash-completing-read-func 'completing-read ; 'completing-read, 'ido-completing-read
;      helm-dash-browser-func 'browse-url ; 'browse-url, 'eww
;      ;; helm-dash-connections
;      helm-dash-common-docsets '(
;	                              "Python_3" "Numpy" "Pandas"
;                           
;                                 )
;      ;; helm-dash-docsets
;      )
	  
;(global-set-key (kbd "C-c d") 'helm-dash-at-point) 
;(add-hook 'python-mode-hook 'py-doc)



(provide 'modes-dash)

;;; modes-dash.el ends here
