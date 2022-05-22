;; -*- lexical-binding: t; -*-

;; Org mode
(use-package org
  :straight nil
  :commands (org-capture org-agenda)
  :hook (org-mode . org-indent-mode)
  :config
  (visual-line-mode 1)
  (setq org-ellipsis " ➜")

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (C . t)
       (python . t))))
  (with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("un" . "src conf-unix"))
    (add-to-list 'org-structure-template-alist '("cc" . "src C"))))


(provide 'init-org-mode.el)
