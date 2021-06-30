;; -*- lexical-binding: t; -*-

;; Company
(use-package company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (prog-mode . global-company-mode)
  :config
  ;; minimum characters to wait for to triger idle completion
  (setq company-minimum-prefix-length 2)
  ;; maximum number of candidates in tooltip
  (setq company-tooltip-limit 10)
  ;; align annotations to right tooltip border
  (setq company-tooltip-align-annotations t)
  ;; don't allow non matching input
  (setq company-require-match 'never)
  ;; disable company in some modes
  (setq company-global-modes
        '(not erc-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode))
  ;; nice frontend for company
  (setq company-frontends

        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))
  ;; company global backend
  (setq company-backends '(company-capf))
  (setq company-auto-commit nil)
  (setq company-auto-commit-chars nil)
  ;; only search current buffer
  (setq company-dabbrev-other-buffers nil)
  ;; case insensitive
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-downcase t)
  :bind (:map company-active-map
              ("<return>" . company-complete-selection)
              ("<tab>" . company-select-next)
              ("<backtab>" . company-select-previous)
              ("M-/" . 'hippie-expand)
              ))

(provide 'init-company)
