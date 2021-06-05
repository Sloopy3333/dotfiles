;; -*- lexical-binding: t; -*-

;; Elfeed
(use-package elfeed
  :defer 5
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
        '("https://archlinux.org/feeds/news/"
          "https://karthinks.com/index.xml"
          "https://github.blog/feed/"
          "https://oremacs.com/atom.xml"
          "https://suckless.org/atom.xml"
          ))
  (set-popup-rule "^\\*elfeed-entry"
                  :size 0.75 :actions '(display-buffer-below-selected)
                  :select t :quit nil :ttl t)
  )

(provide 'rss.el)
