;; -*- lexical-binding: t; -*-

(use-package selectrum
  :bind (:map selectrum-minibuffer-map
              ("C-j" . selectrum-next-candidate)
              ("C-k" . selectrum-previous-candidate)
              )
  :config
  (setq selectrum-extend-current-candidate-highlight t)
  (setq selectrum-max-window-height 30)
  (setq selectrum-fix-vertical-window-height t)
)
