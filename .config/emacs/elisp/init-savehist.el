;; -*- lexical-binding: t; -*-

;; Savehist
;; persistant history across sessions
(use-package savehist
  :straight nil
  :hook (window-setup . savehist-mode)
  :config
  ;; save minibuffer history
  (setq savehist-save-minibuffer-history t)
  ;; don't auto save only save on kill
  (setq savehist-autosave-interval nil)
  ;; addtional variables whose history should be persist across session
  (setq savehist-additional-variables
        '(kill-ring
          register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring))
  (setq-local register-alist
              (cl-remove-if-not #'savehist-printable register-alist)))

;; Saveplace
;; save previous cursor placement in the file
(use-package saveplace
  :straight nil
  :hook (window-setup . save-place-mode))

(provide 'init-savehist.el)
