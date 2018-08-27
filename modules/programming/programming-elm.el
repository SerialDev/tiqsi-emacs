
;; Based on Claudia Doppioslash's elm config https://www.lambdacat.com/post-modern-emacs-setup-for-elm/

(add-hook 'flycheck-mode-hook 'flycheck-elm-setup)
(with-eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

;; npm install -g elm-oracle

(with-eval-after-load 'company
(add-to-list 'company-backends 'company-elm))
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(add-hook 'elm-mode-hook
(lambda ()
(setq company-backends '(company-elm))))

