;; -*- lexical-binding: t; -*-

(use-package doom-themes
  :config
  (setq doom-themes-enable-italic t)
  (setq doom-themes-enable-bold t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))


;; solarized theme
(use-package solarized-theme
  :disabled t
  :config
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background nil)
  ;; use variable face for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; low contrast modeline
  (setq solarized-high-contrast-mode-line nil)
  ;; use less bolding
  (setq solarized-use-less-bold t)
  ;; use more italics
  (setq solarized-use-more-italic t)
  ;; use colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators t)
  ;; don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  ;; avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)
  (load-theme 'solarized-dark t))

;; Doom-modeline
;; a minimal modeline from doom emacs
(use-package doom-modeline
  :defer 0
  :config (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (setq doom-modeline-icon t)))
;; this is requried to display icons in modline while running emacsclient
(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))
(add-hook 'after-make-frame-functions
          #'enable-doom-modeline-icons)
;; disable in term mode
;;(add-hook 'term-mode-hook #'hide-mode-line-mode)


;; All-the-icons
;; adds nice icons to modline
(use-package all-the-icons
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon))


;; All-the-icons-dired
;; adds icons to dired
(use-package all-the-icons-dired
  :after (:any dired dired-jump dired-noselect)
  :hook (dired-mode . all-the-icons-dired-mode))


;; Rainbow-delimiters
;; highlights matching brackets in same color
(use-package rainbow-delimiters
  :hook (window-setup . rainbow-delimiters-mode))

;; Parens
;; highlights matching parens
(use-package paren
  :hook (emacs-startup . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))


;; Hl-line
;; enable line higlighting
(use-package hl-line
  ;; Highlights the current line
  :hook (window-setup . global-hl-line-mode)
  :init (defvar global-hl-line-modes '(prog-mode
                                       text-mode conf-mode special-mode org-agenda-mode)))


;; Number line
;; enable relative line number mode
(setq-default display-line-numbers 'relative)
;; explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)
;; Disable number line in certiain mode
(dolist
    (mode '(org-mode-hook
            term-mode-hook
            shell-mode-hook
            dired-mode-hook
            eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'theme.el)
