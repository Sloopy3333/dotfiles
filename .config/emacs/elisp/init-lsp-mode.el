;; -*- lexical-binding: t; -*-

(use-package lsp-mode
  :commands (lsp)
  :config
  (setq lsp-modeline-diagnostics-scope :workspace)
  ;; use company mode
  (setq lsp-completion-provider :company-mode)
  ;; show completion details of candidates
  (setq lsp-completion-show-detail t)
  ;; show completion type of candidates
  (setq lsp-completion-show-kind t)
  ;; show function signature with eldoc
  (setq lsp-eldoc-enable-hover t)
  ;; don't show function signature
  (setq lsp-signature-auto-activate nil)
  ;; don't show docs in minibuffer area use mouse hover for that
  (setq lsp-signature-render-documentation nil)
  ;; highlight the symbol under the cursor
  (setq lsp-enable-symbol-highlighting nil)
  ;; headerline useful mostly in java
  (setq lsp-headerline-breadcrumb-enable t)
  ;; disable code folding
  (setq lsp-enable-folding nil)
  ;;disable textDocument/documentColor integration.
  (setq lsp-enable-text-document-color t)
  ;; disable textDocument/onTypeFormatting integration.
  (setq lsp-enable-on-type-formatting nil)
  ;; how often lsp-mode will refresh the highlights, lenses, links, etc while you type.
  (setq lsp-idle-delay 0.500)
  ;; disable code lense
  (setq lsp-lens-enable nil)
  ;; show code acions in modeline
  (setq lsp-modeline-code-actions-enable t)
  ;; show diagnostics in modeline
  (setq lsp-modeline-diagnostics-enable nil)
  ;; use flymake for diagnostics
  (setq lsp-diagnostics-provider :flymake)
  )

(provide 'init-lsp-mode.el)
