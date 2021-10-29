(defun sam/eww-readable-buffer ()
  "Once-off call to `eww-readable' after EWW is done rendering."
  (unwind-protect
      (eww-readable)
    (remove-hook 'eww-after-render-hook #'my-eww-readable-nonce)))


(use-package eww
  :config
  (setq eww-search-prefix "https://google.com/search?q=")
  (setq browse-url-browser-function 'eww-browse-url)
  )

(provide 'init-eww.el)
