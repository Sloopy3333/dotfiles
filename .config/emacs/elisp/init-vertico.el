;; -*- lexical-binding: t; -*-

;; vertico
(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-y" . vertico-save)
              )
  :config
  (setq vertico-count 15)
  (setq vertico-resize 'grow-only))

(provide 'init-vertico.el)
