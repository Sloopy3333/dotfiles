;; -*- lexical-binding: t; -*-

(use-package emms
  :commands (emms)
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-default-players '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost")
  (setq emms-player-mpd-server-port "6600")
  (setq emms-player-mpd-music-directory "~/external/hdd2/songs")
  :bind (:map emms-playlist-mode-map
              ("l" . 'emms-browser-add-tracks)
              ("l" . 'emms-toggle-repeat-playlist)
              ("l" . 'emms-insert-playlist)
              ("i" . 'emms-insert-file)
              ("t" . 'emms-toggle-repeat-track)
              ("s" . 'emms-playlist-save)
              ("m" . 'emms-shuffle)))

(provide 'music.el)
