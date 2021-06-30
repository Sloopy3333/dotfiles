;; -*- lexical-binding: t; -*-

;; Gcmh
;; dynamicaly alter the `gc-cons-treshold' depending on the user-activity
;; to ensure no freezes while working
(use-package gcmh
  :hook (window-setup . gcmh-mode)
  :config
  (setq
   ;; idle time gcmh has to wait before triggering gc
   gcmh-idle-delay 10))

(provide 'init-gcmh.el)
