;; -*- lexical-binding: t; -*-

(use-package proced
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-auto-update-interval 2)
  (setq proced-descend t)
  )
  ;;(add-hook 'proced-mode-hook '(lambda () (proced-toggle-auto-update))))
