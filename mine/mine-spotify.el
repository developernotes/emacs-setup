(defvar spotify-volume nil)

(defvar spotify-volume-differential 10)

(defun spotify-start ()
  (interactive)
  (spotify "run")
  (sleep-for 2)
  (spotify-play-pause))

(defun spotify-quit ()
  (interactive)
  (spotify "quit"))

(defun spotify-next-track ()
  (interactive)
  (spotify "next track")
  (spotify-display-track-info))

(defun spotify-previous-track ()
  (interactive)
  (spotify "previous track")
  (spotify-display-track-info))

(defun spotify-play-pause ()
  (interactive)
  (spotify "playpause"))

(defun spotify-volume-down ()
  (interactive)
  (spotify-change-volume '-))

(defun spotify-volume-up ()
  (interactive)
  (spotify-change-volume '+))

(defun spotify-display-track-info ()
  (interactive)
  (message (spotify-get-artist-song-title)))

(defun spotify-change-volume (operation)
  (when (eq nil spotify-volume)
    (spotify-get-volume))
  (setq spotify-volume (funcall operation spotify-volume spotify-volume-differential))
  (spotify (format "set sound volume to %d" spotify-volume)))

(defun spotify-get-volume ()
  (setq spotify-volume
        (string-to-number (spotify "sound volume"))))

(defun spotify-get-artist-song-title ()
  (let ((album (spotify "name of current track"))
        (artist (spotify "album artist of current track")))
    (format "%s - %s" artist album)))

(defun spotify (command)
  (string-replace "\n" ""
                  (shell-command-to-string
                   (format "osascript -e 'tell application \"Spotify\" to %s'" command))))

(global-set-key (kbd "<M-f7>")  'spotify-previous-track)
(global-set-key (kbd "<M-f8>")  'spotify-play-pause)
(global-set-key (kbd "<M-f9>")  'spotify-next-track)
(global-set-key (kbd "<M-f12>") 'spotify-volume-up)
(global-set-key (kbd "<M-f11>") 'spotify-volume-down)

(provide 'mine-spotify)
