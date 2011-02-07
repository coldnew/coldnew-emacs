;;
(eval-when-compile (require 'cl))

(emms-standard)
(emms-default-players)


;; Default emms directory
(setq emms-directory "~/.emacs.d/var/emms")

;; The default directory to look for media files.
(setq emms-source-file-default-directory "~/Music")

;; Add all file in default directory to directory tree
(emms-add-directory-tree "~/Music/")

;; The file to save emms playlists
(setq emms-history-file "~/.emacs.d/var/emms/emms.history")

;; A file used to store cached file information over sessions.
(setq emms-cache-file "~/.emacs.d/var/cache/emms.cache")

;; Enable play playlist repeatly
(setq emms-repeat-playlist t)


;; Emms-browser
(setq emms-browser-info-genre-format "%i● %n"
      emms-browser-info-artist-format "%i● %n"
      emms-browser-info-album-format "%i◎ %n"
      emms-browser-info-title-format "%i♪ %n")


;; Enable show lyrics in emms
;;(emms-lyrics t)

;; If all playlists should be restored on startup add this
(emms-history-load)

;;
(setq emms-player-list '(emms-player-mplayer emms-player-mpg321))





(provide '046-emms)
;; 046-emms.el ends here.
