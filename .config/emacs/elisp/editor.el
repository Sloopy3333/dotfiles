;; -*- lexical-binding: t; -*-

;; File handling
;; resolve symlinks when opening files
(setq find-file-visit-truename t)
;; editing simlinked files under VC will visit the real file
(setq vc-follow-symlinks t)
;; suppress warning messages for symlinked files
(setq find-file-suppress-same-file-warnings t)
;; reload file as they change on disk
(setq global-auto-revert-mode t)


;; create directory if not exist in `find-file'
(add-hook 'find-file-not-found-functions
          (defun sam/create-missing-directories ()
            (unless (file-remote-p buffer-file-name)
              (let ((parent-directory (file-name-directory buffer-file-name)))
                (and (not (file-directory-p parent-directory))
                     (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                                       parent-directory))
                     (progn (make-directory parent-directory 'parents)
                            t))))))
;; disable lockfiles
(setq create-lockfiles nil)

;; Backup files
;; backup files are files with ~ at the end. they are created when the file is saved for first time from a buffer.
;; if editing a new file backup of the original(first save) will be created when you save the file for second time.
;; no more than one backup will be created untill you exit the buffer so you always have the backup of the file that existed before opening the file in buffer

;; enable backup files
(setq make-backup-files t)
;; number each backup file
(setq version-control t)
;; always use copying to create backup files
(setq backup-by-copying t)
;; clean the backup fiels automaticaly
(setq delete-old-versions t)
;; number of old backup to keep
(setq kept-old-versions 5)
;; number of new backup to keep
(setq kept-new-versions 5)
;; backup directory fir tramp files
(setq tramp-backup-directory-alist backup-directory-alist)

;; Autosave files
;; emacs creates a file with # in name until you save
;; these files are automaticaly created if emacs crashes, Xserver crashes, or emacs id killed from shell
;; files can be reverted with`revert-buffer' or `recover-file'

;; enable auo-save
(setq auto-save-default t)
;; don't auto-disable auto-save after deleting big chunks. This defeats
(setq auto-save-include-big-deletions t)


;; Recentf
;; package that remembers the recently opened files
(use-package recentf
  :straight nil
  :hook (emacs-startup . recentf-mode)
  :commands recentf-open-files
  :config
  (setq recentf-max-saved-items 200)
  (setq recentf-auto-cleanup 'never)

  (defun sam\recentf-file-truename (file)
    (if (or (not (file-remote-p file))
            (equal "sudo" (file-remote-p file 'method)))
        (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
      file))

  ;; resolve symlinks expand $HOME remove /sudo:X@ from recent files
  (add-to-list 'recentf-filename-handlers #'sam/recentf-file-truename)

  ;; remove text properties from recentf files
  (add-to-list 'recentf-filename-handlers #'substring-no-properties))


;; Text formating
;; don't insert tabs fir indentation
(setq-default indent-tabs-mode nil)
;; display width for tab characters
(setq tab-width 4)

;; hitting TAB indents the current line if point is at the left margin or in the line's indentation,
;; otherwise it inserts a "real" TAB character.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; column beyond which line wraping happens
(setq-default fill-column 80)

;; wrap words at whitespaces instead at the middle of word
(setq-default word-wrap t)
;; give each line of text only one line ie no wraping
;;(setq-default truncate-lines t)
;;(setq truncate-partial-width-windows nil)

;; add a newline at end of file
(setq require-final-newline t)

;; hard wraping
(add-hook 'text-mode-hook #'auto-fill-mode)

;; remove all trailing whitespaces before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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


;; Solong mode
;; improves performance in large files by disabling some modes
(use-package so-long
  :straight nil
  :hook (window-setup . global-so-long-mode)
  :config
  (setq so-long-threshold 400)
  ;; don't disable font-lock-mode, line-number-mode and don't make buffer read only
  (delq 'font-lock-mode so-long-minor-modes)
  (delq 'display-line-numbers-mode so-long-minor-modes)
  (delq 'buffer-read-only so-long-variable-overrides 'assq)
  ;; reduce the level of syntax highlighting
  (add-to-list 'so-long-variable-overrides '(font-lock-maximum-decoration . 1))
  ;; disable save-place in large files
  (add-to-list 'so-long-variable-overrides '(save-place-alist . nil))
  ;; disable these
  (appendq so-long-minor-modes
           '(eldoc-mode
             auto-composition-mode
             undo-tree-mode
             hl-fill-column-mode)))


;; Undo-tree
(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode))


;; disable right-to-left text mode since i dont use it
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; window management
(use-package window
  :straight nil
  :config
  (setq switch-to-buffer-obey-display-actions t)
  (setq frame-title-format '("%b – Emacs"))
  (setq frame-resize-pixelwise t)
  (setq window-resize-pixelwise nil)
  :custom
  (display-buffer-alist
   '(("\\*ansi-term\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.3)))
   ))

(provide 'editor.el)
