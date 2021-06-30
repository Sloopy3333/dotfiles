;; -*- lexical-binding: t; -*-

;; Eglot
;; minimal Lsp
(use-package eglot
  :commands (eglot)
  :config
  ;;(add-to-list 'eglot-server-programs '(c-mode . ("clangd" "--background-index")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp"))))





(provide 'init-eglot.el)
