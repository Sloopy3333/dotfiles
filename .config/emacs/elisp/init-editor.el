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
;; backup directory
(setq backup-directory-alist '(("." . "~/.local/share/emacs/var/backup")))
;; backup directory fir tramp files
(setq tramp-backup-directory-alist backup-directory-alist)

;; Autosave files
;; emacs creates a file with # in name until you save
;; these files are automaticaly c
;; files can be reverted with`revert-buffer' or `recover-file'

;; enable auo-save
(setq auto-save-default t)
;; don't auto-disable auto-save after deleting big chunks
(setq auto-save-include-big-deletions t)

;; Text formating
;; don't insert tabs for indentation
(setq-default indent-tabs-mode nil)
;; display width for tab characters
(setq-default tab-width 4)

;; hitting TAB indents the current line if point is at the left margin or in the line's indentation,
;; otherwise it inserts a "real" TAB character.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; limit beyond which line wraping occur
(setq-default fill-column 80)

;; wrap words at whitespaces instead at the middle of word
(setq-default word-wrap t)
;; if non nil give each line of text only one line ie no wraping
(setq-default truncate-lines nil)
(setq truncate-partial-width-windows nil)

;; hard wraping
(add-hook 'prog-mode-hook 'visual-line-mode)

;; add a newline at end of file
(setq require-final-newline t)

;; remove all trailing whitespaces before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; disable right-to-left text mode since i dont use it
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)


(provide 'init-editor.el)
