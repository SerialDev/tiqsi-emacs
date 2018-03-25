;;; modes-spotify.el --- Tiqsi spotify support TODO: alternative when running win32

;;; Commentary:
;; 


;; spotify on Emacs
;(package-install 'helm-spotify)
;(require 'helm-spotify)
;(require 'spotify) ;; platform not supported dbus error*


;(try-require 'helm-spotify)


;(defun john-spotify ()
;  "wrapper for calling spotify from keyboard shortcut and removing possibility for error"
;  (interactive)
;  (setq debug-on-error t)
;  (helm-spotify)
;  (setq debug-on-error nil))
;(global-set-key (kbd "C-x M-s") 'john-spotify)


;;Small minor mode to control spotify from Emacs

;(defun spotify-play () "Play Spotify" (interactive)
;  (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Play"))

;(defun spotify-pause () "Pause Spotify" (interactive)
;  (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Pause"))

;(defun spotify-playpause () "Play/Pause Spotify" (interactive)
;  (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause"))

;(defun spotify-back () "Starts the song over in Spotify" (interactive)
;  (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous"))

;(defun spotify-next () "Next song in Spotify" (interactive)
;  (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next"))

;(defun spotify-previous () "Previous song in Spotify" (interactive)
;  (progn (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
;	 (sit-for 0.1)
;	 (shell-command "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")))

;(global-set-key (kbd "C-c <C-S-right>") 'spotify-next)
;(global-set-key (kbd "C-c <C-S-left>") 'spotify-previous)
;(global-set-key (kbd "C-c <C-S-up>") 'spotify-playpause)

(provide 'modes-spotify)

;;; modes-spotify.el ends here
