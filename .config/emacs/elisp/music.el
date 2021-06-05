;; -*- lexical-binding: t; -*-

(use-package emms
  :defer t
  :commands (emms)
  :config
  (emms-all)
  (emms-default-players)
  :bind (:map emms-playlist-mode-map
              ("l" . 'emms-toggle-repeat-playlist)
              ("p" . 'emms-insert-playlist)
              ("i" . 'emms-insert-file)
              ("t" . 'emms-toggle-repeat-track)
              ("s" . 'emms-playlist-save)
              ("m" . 'emms-shuffle)))

(provide 'music.el)
