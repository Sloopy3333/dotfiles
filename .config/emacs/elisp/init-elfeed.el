;; -*- lexical-binding: t; -*-

(defun sam/elfeed-search-browse-url-readable ()
  "alternative to `elfeed-search-browse-url' activates `eww-readable' for current buffer"
  (interactive)
  (add-hook 'eww-after-render-hook #'sam/eww-readable-buffer)
  (elfeed-search-browse-url))

;; Elfeed
(use-package elfeed
  :hook (elfeed-search-mode . elfeed-update)
  :commands (:any elfeed elfeed-update)
  :config
  (evil-define-key 'normal elfeed-search-mode-map (kbd "o") 'sam/elfeed-search-browse-url-readable)
  (setq elfeed-feeds
        '(("https://archlinux.org/feeds/news/" Arch Linux)
          ("https://karthinks.com/index.xml" Emacs Blog)
          ("https://oremacs.com/atom.xml" Emacs Blog)
          ("https://emacsair.me/feed.xml" Emacs Blog)
          ("https://ag91.github.io/rss.xml" Emacs Blog)
          ("https://ruzkuku.com//all.atom" Emacs Blog)
          ("https://ambrevar.xyz/atom.xml" Emacs Blog)
          ("https://xenodium.com/rss.xml" Emacs Blog)
          ("https://planet.emacslife.com/atom.xml" Emacs Community)
          ("https://www.autosport.com/rss/f1/news/" Sports Formula_1)
          ("http://feeds2.feedburner.com/f1fanatic" Sports Formula_1)
          )))

(provide 'init-elfeed.el)
