;; -*- lexical-binding: t; -*-

;; Parens
;; highlights matching parens
(use-package paren
  :hook (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(provide 'init-paren.el)
