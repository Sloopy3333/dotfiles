;; -*- lexical-binding: t; -*-

;; Elfeed
(use-package elfeed
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
        '("https://archlinux.org/feeds/news/"
          "https://karthinks.com/index.xml"
          "https://github.blog/feed/"
          "https://oremacs.com/atom.xml"
          "https://suckless.org/atom.xml"
          )))

(provide 'init-elfeed.el)
