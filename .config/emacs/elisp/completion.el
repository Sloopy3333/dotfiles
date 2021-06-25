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


;; Company box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  ;; display even if only ones candidate issue there
  (setq company-box-show-single-candidate nil)
  ;; disable backend base color
  (setq company-box-backends-colors nil)
  ;; maximum number of candidates
  (setq company-box-max-candidates 20)
  ;; icons rendering method
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  ;; auto insert selected candidate
  (company-tng-mode 1)
  ;; add elisp support
  (setq company-box-icons-functions
        (cons #'company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions)))
  ;; disable documentation in echo area
  (delq 'company-echo-metadata-frontend company-frontends)

  (defun company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))

;; Eglot
;; minimal Lsp
(use-package eglot
  :commands (eglot)
  :config
  ;;(add-to-list 'eglot-server-programs '(c-mode . ("clangd" "--background-index")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))


;; Electric pair
(use-package elec-pair
  :straight nil
  :hook (prog-mode . electric-pair-mode)
  )


;; Electric indent
(use-package electric
  :straight nil
  :hook (prog-mode . electric-indent-mode)
  )


(provide 'completion.el)
