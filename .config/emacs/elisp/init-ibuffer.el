(use-package ibuffer
  :straight nil
  :config
  ;; dont ask for confirmations
  (setq ibuffer-expert t)
  ;; disable summary at bottom
  (setq ibuffer-display-summary nil)
  ;; dont use other window
  (setq ibuffer-use-other-window nil)
  ;; dont cycle
  (setq ibuffer-movement-cycle nil)
  ;; sorting method for ibuffer
  (setq ibuffer-default-sorting-mode 'filename/process)
  ;; display a header line containing current filters.
  (setq ibuffer-use-header-line t)
  ;;(setq x  )
  )
