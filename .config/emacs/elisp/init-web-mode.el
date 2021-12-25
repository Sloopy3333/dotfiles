(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.html\\'" "\\.css\\'" "\\.php\\'")
  :config
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)
)

(provide 'init-web-mode.el)
