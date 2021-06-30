;; -*- lexical-binding: t; -*-

;; Recentf
;; package that remembers the recently opened files
(use-package recentf
  :straight nil
  :hook (emacs-startup . recentf-mode)
  :commands recentf-open-files
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-auto-cleanup 'never)

  ;; resolve simlinks and clean the recent file names
  (defun sam\recentf-file-truename (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
      file))
  ;; resolve symlinks expand $HOME remove /sudo:X@ from recent files
  (add-to-list 'recentf-filename-handlers #'sam/recentf-file-truename)
  ;; remove text properties from recentf files
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))

(provide 'init-recentf.el)
