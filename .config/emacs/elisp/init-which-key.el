;; -*- lexical-binding: t; -*-

;; Which key
(use-package which-key
  :after evil
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'init-which-key.el)
