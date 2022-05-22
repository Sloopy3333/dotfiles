;; -*- lexical-binding: t; -*-

;; Eglot
;; minimal Lsp
(use-package eglot
;;  :disabled t
  :commands (eglot)
  :config
  ;;(add-to-list 'eglot-server-programs '(c-mode . ("clangd" "--background-index")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '(web-mode . ("vscode-html-language-server" "--stdio")))
  )






(provide 'init-eglot.el)
