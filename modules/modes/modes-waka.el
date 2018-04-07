;;; modes-waka.el --- Tiqsi Wakatime Support

;;; Commentary:
;; 
(when (require 'core-secrets nil 'noerror) 
  (global-wakatime-mode 0)
  (setq wakatime-cli-path (eshell-command-result "which wakatime"))
  ) 

(provide 'modes-waka)

;;; modes-waka.el ends here
