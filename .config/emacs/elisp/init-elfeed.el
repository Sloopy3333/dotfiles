;; -*- lexical-binding: t; -*-

(defun sam/elfeed-search-browse-url-readable ()
  "alternative to `elfeed-search-browse-url' activates `eww-readable' for current buffer"
  (interactive)
  (add-hook 'eww-after-render-hook #'sam/eww-readable-buffer)
  (elfeed-search-browse-url))

(defun sam/elfeed-browse-url-eww (&optional use-generic-p)
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (add-hook 'eww-after-render-hook #'sam/eww-readable-buffer)
    (if (string-equal major-mode "elfeed-search-mode")
        (elfeed-search-browse-url)
      (elfeed-show-visit use-generic-p))))

(defun sam/elfeed-browse-url-external (&optional use-generic-p)
  (interactive "P")
  (let ((browse-url-browser-function #'browse-url-firefox))
    (if (string-equal major-mode "elfeed-search-mode")
        (elfeed-search-browse-url)
      (elfeed-show-visit use-generic-p))))

;; Elfeed
(use-package elfeed
  :hook (elfeed-search-mode . elfeed-update)
  :commands (:any elfeed elfeed-update)
  :config
  (evil-define-key 'normal elfeed-search-mode-map (kbd "o") 'sam/elfeed-browse-url-eww)
  (evil-define-key 'normal elfeed-show-mode-map (kbd "o") 'sam/elfeed-browse-url-eww)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "O") 'sam/elfeed-browse-url-external)
  (evil-define-key 'normal elfeed-show-mode-map (kbd "O") 'sam/elfeed-browse-url-external)
  (evil-define-key 'normal elfeed-search-mode-map (kbd "r") 'elfeed-search-untag-all-unread)
  (setq elfeed-feeds
        '(("https://archlinux.org/feeds/news/" Arch Linux)
          ("https://karthinks.com/index.xml" Emacs)
          ("https://oremacs.com/atom.xml" Emacs)
          ("https://ag91.github.io/rss.xml" Emacs)
          ("https://ambrevar.xyz/atom.xml" Emacs)
          ("https://sachachua.com/blog/feed/" Emacs)
          ("https://www.gamingonlinux.com/article_rss.php?mini" Gaming Linux)
          )))

(provide 'init-elfeed.el)
