;; -*- lexical-binding: t; -*-

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
  :hook (dired-mode . all-the-icons-dired-mode))

(provide 'init-all-the-icons.el)
