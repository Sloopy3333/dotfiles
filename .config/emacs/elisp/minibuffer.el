;; -*- lexical-binding: t; -*-

;; alias yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; try to keep the cursor out of the read-only portions of the minibuffer.
(setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;; vertico
(use-package vertico
  :disabled t
  :init (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              )
  )
;; Icomplete
;; simple built-in minibuffer selector
(use-package icomplete
;;  :disabled t
  :straight nil
  :bind (:map icomplete-minibuffer-map
              ("C-j" . 'icomplete-forward-completions)
              ("C-k" . 'icomplete-backward-completions)
              ("<down>" . 'icomplete-forward-completions)
              ("up" . 'icomplete-backward-completions)
              ("<tab>" . 'icomplete-forward-completions)
              ("<backtab>" . 'icomplete-backward-completions)
              ("<return>" . 'icomplete-force-complete-and-exit)
              ("u" . nil)
              :map icomplete-fido-mode-map
              ("C-j" . 'icomplete-forward-completions)
              ("C-k" . 'icomplete-backward-completions)
              ("<down>" . 'icomplete-forward-completions)
              ("up" . 'icomplete-backward-completions)
              ("<backtab>" . 'icomplete-backward-completions)
              ("<return>" . 'icomplete-force-complete-and-exit)
              ("u" . nil))
  :config
  ;; time to wait tile completion starts
  (setq icomplete-compute-delay 0)
  ;; pending-completions number over which to apply icomplete-compute-delay
  (setq icomplete-delay-completions-threshold 0)
  ;; maximum number of initial chars to apply icomplete-compute-delay
  (setq icomplete-max-delay-chars 0)
  ;; show completions without any input
  (setq icomplete-show-matches-on-no-input t)
  ;; don't hide common prefix from completion candidates
  (setq icomplete-hide-common-prefix nil)
  ;; enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  ;; maximum height of te minibuffer window
  (setq icomplete-prospects-height 20)
  ;; disable cycling
  (setq icomplete-scroll t)
  (setq icomplete-with-completion-tables t)
  (icomplete-mode)
  (icomplete-vertical-mode))


;; Minibuffer
(use-package minibuffer
  :straight nil
  :config
  ;; completion styles to be used by default see `completion-style-alist'
  ;; available styles are `partial-completion' `substring' `initials' `basic' `flex'
  (setq completion-styles
        '(partial-completion substring initials basic))
  ;; override default completion style of specific modes
  (setq completion-category-overrides
        '((file (styles . (flex)))
          (buffer (styles . (flex)))
          (project-file (styles . (flex)))
          (info-menu (styles . (flex)))))
  (setq completions-format 'vertical)
  (setq completion-ignore-case t)
  (setq completion-cycle-threshold t)
  (setq completion-flex-nospace nil)
  (setq completion-show-help t)
  (setq completion-pcm-complee-word-inserts-delimiters t)
  (setq read-answer-short t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq resize-mini-windows 'grow-only)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1))


;; Consult
;; provides various improvements to minibuffer command
(use-package consult
  :after (:any icomplete vertico)
  :config
  (setq consult-buffer-sources '(consult--source-buffer)))

;;; Marginalia
(use-package marginalia
  :after (:any icomplete vertico)
  :config
  ;;(set-face-attribute 'marginalia--documentation nil
  ;;(marginalia-documentation ((t (:inherit completions-annotations :extend t)))))
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy t)))


(provide 'minibuffer.el)
