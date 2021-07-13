;; -*- lexical-binding: t; -*-

(use-package base16-theme
  :config
  (setq base16-distinct-fringe-background nil)
  (setq base16-highlight-mode-line nil)
  (load-theme 'base16-gruvbox-dark-medium t))

(provide 'init-base16-theme.el)
