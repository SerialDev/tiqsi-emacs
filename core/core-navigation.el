;;; core-navigation.el --- Tiqsi Navigation defuns

;;; Commentary:
;; 

;----{Searching}----;

; Commands
(set-variable 'grep-command "grep -irHn ")
(when tiqsi-win32
    (set-variable 'grep-command "findstr -s -n -i -l "))

(provide 'core-navigation)

;;; core-navigation.el ends here
