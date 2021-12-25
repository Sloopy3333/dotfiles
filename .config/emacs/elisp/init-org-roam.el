;; -*- lexical-binding: t; -*-

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory "~/external/Org/Roam")
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("f" "How to" plain
      (file "~/external/Org/Roam/Templates/HowTo.org")
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+filetags: HowTo")
      :unnarrowed t)))
  )

(provide 'init-org-roam.el)
