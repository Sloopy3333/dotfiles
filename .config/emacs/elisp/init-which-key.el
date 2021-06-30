;; -*- lexical-binding: t; -*-

;; Which key
(use-package which-key
  :after evil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(provide 'init-which-key.el)
