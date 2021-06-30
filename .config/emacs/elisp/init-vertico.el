;; -*- lexical-binding: t; -*-

;; vertico
(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ))

(provide 'init-vertico.el)
