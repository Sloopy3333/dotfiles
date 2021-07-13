;; -*- lexical-binding: t; -*-

;; use-package
(setq
 ;; if enabled you can view package load time with `use-package-report'
 use-package-compute-statistics t
 use-package-verbose nil)

;; install use-package
(straight-use-package 'use-package)

(provide 'init-use-package.el)
