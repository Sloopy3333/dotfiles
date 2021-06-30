;; -*- lexical-binding: t; -*-

;; Consult
;; provides various improvements to minibuffer command
(use-package consult
  :after (:any icomplete vertico)
  :config
  (setq consult-buffer-sources '(consult--source-buffer)))

(provide 'init-consult.el)
