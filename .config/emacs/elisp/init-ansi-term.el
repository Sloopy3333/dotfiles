;; -*- lexical-binding: t; -*-

;; a bottom terminal like vs code
(use-package window
  :straight nil
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (setq frame-title-format '("%b – Emacs"))
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise nil)
  :custom
  (display-buffer-alist
   '(("\\*ansi-term\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.3)))
   ))

(provide 'init-ansi-term.el)
