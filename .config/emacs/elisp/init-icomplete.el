;; -*- lexical-binding: t; -*-

;; Icomplete
;; simple built-in minibuffer selector
(use-package icomplete
  :straight nil
  :hook
  (icomplete-minibuffer-setup . (lambda () (setq max-mini-window-height 20)))
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
  (setq resize-mini-windows nil)
  ;; disable cycling
  (setq icomplete-scroll t)
  (setq icomplete-with-completion-tables t)
  (icomplete-mode)
  (icomplete-vertical-mode)
  (set-face-attribute 'icomplete-selected-match nil
                      :extend t
                      :weight 'bold)
  )

(provide 'init-icomplete.el)
