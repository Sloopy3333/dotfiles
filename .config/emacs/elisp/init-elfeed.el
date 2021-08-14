;; -*- lexical-binding: t; -*-

;; Elfeed
(use-package elfeed
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
        '(("https://archlinux.org/feeds/news/" Arch Linux)
          ("https://karthinks.com/index.xml" Emacs Blog)
          ("https://oremacs.com/atom.xml" Emacs Blog)
          ("https://emacsair.me/feed.xml" Emacs Blog)
          ("https://ag91.github.io/rss.xml" Emacs Blog)
          ("https://ruzkuku.com//all.atom" Emacs Blog)
          ("https://ambrevar.xyz/atom.xml" Emacs Blog)
          ("https://xenodium.com/rss.xml" Emacs Blog)
          )))

(provide 'init-elfeed.el)
