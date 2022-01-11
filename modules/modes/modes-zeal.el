
(straight-use-package
 '(zeal-at-point
   :type git
   :host github
   :ensure t
   :repo "jinzhu/zeal-at-point"
))


(with-system windows

  (progn
;; Use multiple docsets
(add-to-list 'zeal-at-point-mode-alist '(python-mode . ("python" "django")))


;; Use multiple docsets
(add-hook 'python-mode-hook
	  (lambda () (setq zeal-at-point-docset '("python" "django"))))

))


