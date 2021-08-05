;; -*- lexical-binding: t; -*-

(use-package haskell-mode
  :commands (haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-process-type 'auto)
  )

(provide 'init-haskell-mode.el)
