;; -*- lexical-binding: t; -*-

;;; Marginalia
(use-package marginalia
  :after (:any icomplete vertico)
  :config
  ;;(set-face-attribute 'marginalia--documentation nil
  ;;(marginalia-documentation ((t (:inherit completions-annotations :extend t)))))
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy t)))

(provide 'init-marginalia.el)
