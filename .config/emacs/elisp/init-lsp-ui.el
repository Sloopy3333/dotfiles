;; -*- lexical-binding: t; -*-

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; disable doc window at side, still can use mouse hover
  (setq lsp-ui-doc-show-with-cursor nil)
  ;; disable sideline
  (setq lsp-ui-sideline-enable nil)
  )

(provide 'init-lsp-ui.el)
