;; -*- lexical-binding: t; -*-

(global-hl-line-mode 1)                                            ;; enable line highlighting
(setq-default display-line-numbers 'relative)                      ;; enable relative line number mode
(setq read-process-output-max (* 1024 1024))                       ;; increase the amount of data which Emacs reads from the process.
(setq-default bidi-display-reordering 'left-to-right               ;; disable right-to-left since i dont use it
              bidi-paragraph-direction 'left-to-right)
(setq auto-hscroll-mode 'current-line)                             ;; horizontally scroll only the current line
(setq fast-but-imprecise-scrolling t)                              ;; more performant rapid scrolling over unfontified regions.
(setq ffap-machine-p-known 'reject)                                ;; don't ping things that look like domain names.
(setq inhibit-startup-screen t)                                    ;; disable startup screen
(setq ring-bell-function 'ignore)                                  ;; disable error bell
(setq initial-scratch-message "")                                  ;; disable scratch buffer on startup
(setq initial-major-mode 'fundamental-mode)                        ;; disable scratch mode on startup
(setq mode-line-format nil)                                        ;; dont show default modeline until my modeline is loaded
(setq warning-minimum-level :emergency)                            ;; set warning messages to emergency
(setq highlight-nonselected-windows nil)                           ;; don't highlight non selected windows
(setq frame-inhibit-implied-resize t)                              ;; dont resize window implicitly
(setq create-lockfiles nil)                                        ;; disable lockfiles
(setq make-backup-files nil)                                       ;; disable backup files
(setq auto-save-default nil)                                       ;; disable autosave
(setq make-backup-files nil)                                       ;; disable backup
(setq tab-width 4)                                                 ;; set tab width to 4
(setq indent-tabs-mode nil)                                        ;; use space only
(setq tab-always-indent 'complete)                                 ;; tab completion
(setq select-enable-clipboard t)                                   ;; use system secondary clipboard
(setq select-enable-primary t)
(setq cursor-in-non-selected-window nil)                           ;; hide cursor in non-focused window
(setq help-window-select t)                                        ;; focus help window when opened
(setq blink-cursor-mode nil)                                       ;; disable cursot blinking
(setq global-visual-line-mode t)                                   ;; disable line wrap
(setq column-number-mode t)                                        ;; show column numbers mode
(setq delete-selection-mode t)                                     ;; typing will overwrite the selected text
(setq global-auto-revert-mode t)                                   ;; reload file as they change on disk
(setq inhibit-x-resources t)                                       ;; dont read xresources
(setq enable-recursive-minibuffers t)                              ;; eable recursive miniuffer
(defalias 'yes-or-no-p 'y-or-n-p)                                  ;; alias yes/no to y/n
(add-hook 'before-save-hook #'delete-trailing-whitespace)          ;; remove all trailing whitespaces before savind
(setq require-final-newline t)                                     ;; add a new line to end of file
(setq use-dialog-box nil)                                          ;; dialog will appear in echo area
(setq confirm-nonexistent-file-or-buffer nil)                      ;; no confirm visiting buffer

;; scroll settings
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 999999)

;; utf-8
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Disable number line in certiain mode
(dolist
    (mode '(org-mode-hook
            term-mode-hook
            shell-mode-hook
            dired-mode-hook
            eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Print time took to load emacs in messages buffer
(defun sam/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'sam/display-startup-time)

;; Fonts
(defun sam/set-font ()
  (set-face-attribute 'default nil
		      :font "Hack Nerd Font Mono 12"
		      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
		      :font "Hack Nerd Font 12"
		      :weight 'bold)
  (set-face-attribute 'fixed-pitch nil
		      :font "Hack Nerd Font Mono 12"
		      :weight 'regular)
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
		      :slant 'italic))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
        	(with-selected-frame frame
        	  (sam/set-font))))
  (sam/set-font))

;; Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use-Package
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)

;; No-Littering
(use-package no-littering)

;; Gcmh
(use-package gcmh
  :config
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (gcmh-mode +1))

;; Ui & Themes

;; Theme
(use-package doom-themes
  :defer 0
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-gruvbox t))

;; Doom Modline
(use-package doom-modeline
  :defer 0
  :config (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)
           (setq doom-modeline-icon t)))
;; this is requried to display icons in modline while running emacsclient
(defun enable-doom-modeline-icons (_frame)
  (setq doom-modeline-icon t))
(add-hook 'after-make-frame-functions
          #'enable-doom-modeline-icons)

;; All The Icons
(use-package all-the-icons
  :after (:any doom-modeline))

;; All-The-Icons-DIRED
(use-package all-the-icons-dired
  :after (:any dired dired-jump dired-noselect)
  :hook (dired-mode . all-the-icons-dired-mode))

;; Rainbow-Delimitters
(use-package rainbow-delimiters
  :config (rainbow-delimiters-mode))


;; Minibuffer

;; Vertico
(use-package vertico
  :defer 0
  :bind (:map vertico-map
	      ("C-j" . 'vertico-next)
	      ("C-k" . 'vertico-previous)
	      ("C-u" . 'vertico-scroll-down)
	      ("C-d" . 'vertico-scroll-up)
	      )
  :config
  (vertico-mode)
  (savehist-mode)
  (setq vertico-cycle nil
	vertico-count 15))

;; Icomplete
(use-package icomplete
  :disabled t
  :bind (:map icomplete-minibuffer-map
 	      ("C-j" . 'icomplete-forward-completions)
 	      ("C-k" . 'icomplete-backward-completions))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  ;;(setq icomplete-in-buffer t)
  (setq icomplete-prospects-height 15)
  ;;(setq icomplete-with-completion-tables t)
  (fido-mode -1)
  (savehist-mode)
  (icomplete-mode)
  (icomplete-vertical-mode)
  (set-face-attribute 'icomplete-first-match nil
                      :foreground "#8ec07c"
                      :background "#504945"
                      :extend t))
;; Completion style
(use-package minibuffer
  :straight nil
  :config
  (setq completion-styles
        '(partial-completion substring initials flex))
  (setq completion-category-overrides
 	'((file (styles . (partial-completion substring)))
          (buffer (styles . ( basic substring partial-completion)))
          (project-file (styles . (partial-completion substring)))
          (info-menu (styles . (substring)))))
  (setq completions-format 'vertical)
  (setq completion-ignore-case t)
  (setq completion-cycle-threshold t)
  (setq completion-flex-nospace nil)
  (setq completion-show-help nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq read-answer-short t)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq resize-mini-windows t)
  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;;; Consult
(use-package consult
  :after (vertico))

;;; Marginalia
(use-package marginalia
  :after (vertico)
  :config
  (marginalia-mode)
  (setq marginalia-annotators '(marginalia-annotators-heavy t)))

;;; Helpful
(use-package helpful
  :commands (helpful-callable helpful-function helpful-macro helpful-variable helpful-command helpful-key))

;; Window
(use-package window
  :straight nil
  :custom
  (display-buffer-alist
   '(("\\*e?shell\\*"
      (display-buffer-in-side-window)
      (side . bottom)
      (slot . -1)
      (window-height . 0.3)))
   ))

;; Completion

;; Company
(use-package company
  :hook (emacs-startup . global-company-mode)
  :bind (:map company-active-map
              ("<tab>" . 'company-complete-selection)
              ("C-l" . 'company-complete-selection)
              ("C-j" . 'company-select-next)
              ("C-k" . 'company-select-previous)
              ("C-h" . 'company-abort))
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-idle-delay 0.5)
  (company-show-numbers t))

;; Company box
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Eglot
(use-package eglot
  :commands (eglot))

;; Lsp-Mode
(use-package lsp-mode
  :disabled t
  :commands lsp)

;; Electric-PAIR
(use-package elec-pair
  :hook (emacs-startup . electric-pair-mode))
;;:config
;;(electric-pair-mode 1))

;; Paren
(use-package paren
  :config
  (show-paren-mode 1)
  (setq
   show-paren-highlight-openparen t
   show-paren-when-point-inside-paren t
   show-paren-when-point-periphery t
   show-paren-style 'parenthesis))

;; Magit
(setq bare-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(setq bare-work-tree (concat "--work-tree=" (expand-file-name "~")))

(defun sam/magit-status-bare ()
  "set --git-dir and --work-tree in `magit-git-global-arguments' to `bare-git-dir' and `bare-work-tree' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (add-to-list 'magit-git-global-arguments bare-git-dir)
  (add-to-list 'magit-git-global-arguments bare-work-tree)
  (call-interactively 'magit-status))

(defun sam/magit-status ()
  "removes --git-dir and --work-tree in `magit-git-global-arguments' and calls `magit-status'"
  (interactive)
  (require 'magit-git)
  (setq magit-git-global-arguments (remove bare-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove bare-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))

(use-package magit
  :commands (:any sam/magit-status sam/magit-status-bare)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Dired
(setq my-dired-copy-list nil)
(defun sam/dired-copy ()
  (interactive)
  (setq my-dired-copy-list (dired-get-marked-files)))

(defun sam/dired-paste ()
  (interactive)
  (when my-dired-copy-list
    (shell-command
     (mapconcat
      #'shell-quote-argument
      `("cp" "-r" ,@my-dired-copy-list ,".")
      " "))
    (revert-buffer :ignore-auto :noconfirm)
    (setq my-dired-copy-list nil)))

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-listing-switches "-lhDA --group-directories-first")
  (setq ls-lisp-dirs-first t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-ls-F-marks-symlinks t)
  (setq dired-hide-details-mode t)
  :hook
  (dired-mode . (lambda ()
 		  (dired-hide-details-mode)
                  (evil-local-set-key 'normal (kbd "l") 'dired-find-alternate-file)
                  (evil-local-set-key 'normal (kbd "h") '(lambda () (interactive)(find-alternate-file "..")))
                  (evil-local-set-key 'normal (kbd "v") 'dired-mark)
                  (evil-local-set-key 'normal (kbd "dd") 'dired-do-delete)
                  (evil-local-set-key 'normal (kbd "u") 'dired-unmark)
                  (evil-local-set-key 'normal (kbd "U") 'dired-unmark-all-marks)
                  (evil-local-set-key 'normal (kbd "yy") 'sam/dired-copy)
                  (evil-local-set-key 'normal (kbd "p") 'sam/dired-paste)
                  (evil-local-set-key 'normal (kbd "cw") 'dired-do-rename)
                  (evil-local-set-key 'normal (kbd "mf") 'dired-create-empty-file)
                  (evil-local-set-key 'normal (kbd "md") 'dired-create-directory)
                  (evil-local-set-key 'normal (kbd "cp") 'dired-do-chmod)
                  (evil-local-set-key 'normal (kbd "cg") 'dired-do-chgrp)
                  (evil-local-set-key 'normal (kbd "co") 'dired-do-chown)
                  (evil-local-set-key 'normal (kbd "ex") 'dired-do-compress)
                  (evil-local-set-key 'normal (kbd "W") 'wdired-change-to-wdired-mode)
                  (evil-local-set-key 'normal (kbd "r") 'revert-buffer)
                  (evil-local-set-key 'normal (kbd "gd") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/Downloads"))))
                  (evil-local-set-key 'normal (kbd "gc") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/.config"))))
                  (evil-local-set-key 'normal (kbd "gh") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/"))))
                  (evil-local-set-key 'normal (kbd "gp") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/Projects"))))
                  (evil-local-set-key 'normal (kbd "gs") '(lambda () (interactive)(find-alternate-file(expand-file-name "~/scripts"))))
                  (evil-local-set-key 'normal (kbd "Q")  'evil-delete-buffer)
                  )))

;; Keybindings

;; Evil
(use-package evil
  :defer 0
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1))

;; Evil-collection
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Undo-tree
(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode))

;; Which-key
(use-package which-key
  :after evil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; General
(defun sam/switch-to-next-buffer ()
  "similar to `switch-to-next-buffer' but ignores buffer name starting with *"
  (interactive)
  (next-buffer)
  (if (string-match-p "^\*.*\*" (buffer-name))
      (next-buffer)))

(defun sam/switch-to-prev-buffer ()
  "similar to `switch-to-prev-buffer' but ignores buffer name starting with *"
  (interactive)
  (previous-buffer)
  (if (string-match-p "^\*.*\*" (buffer-name))
      (previous-buffer)))

(defun sam/toggle-side-tree()
  (interactive)
  (if (get-buffer "Side-tree")
      (kill-buffer "Side-tree")
    (let ((dir (if (eq (vc-root-dir) nil)
 		   (dired-noselect default-directory)
 		 (dired-noselect (vc-root-dir)))))
      (display-buffer-in-side-window
       dir `((side . left)
 	     (slot . 0)
 	     (window-width . 0.15)
 	     (window-parameters . ((no-other-window . nil)
 				   (no-delete-other-window . t)
 				   (mode-line-format . (" " "%b"))))))
      (with-current-buffer dir
 	(rename-buffer "Side-tree")))))


(defun sam/toggle-eshell()
  (interactive)
  (if (get-buffer "*eshell*")
      (kill-buffer "*eshell*")
    (eshell)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  ;; evil
  (general-def 'motion
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  ;;utils
  (general-define-key
   "M-t"  'sam/toggle-eshell
   "M-SPC" 'sam/toggle-side-tree)

  ;; buffers
  (general-def 'normal
    "M-<tab>" #'sam/switch-to-next-buffer
    "M-<iso-lefttab>" #'sam/switch-to-prev-buffer
    "M-b" 'consult-buffer
    "M-q" 'kill-buffer-and-window)

  ;; window
  (general-def 'normal
    "M-Q" 'delete-window
    "M-s" 'split-window-vertically
    "M-v" 'split-window-horizontally
    "M-j" 'windmove-down
    "M-k" 'windmove-up
    "M-l" 'windmove-right
    "M-h" 'windmove-left
    "M-J" 'windmove-swap-states-down
    "M-K" 'windmove-swap-states-up
    "M-L" 'windmove-swap-states-right
    "M-H" 'windmove-swap-states-left
    "M-C-j" 'enlarge-window
    "M-C-k" 'shrink-window
    "M-C-l" 'enlarge-window-horizontally
    "M-C-h" 'shrink-window-horizontally
    )

  ;; prefix
  (general-create-definer sam/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "S-SPC")
  (sam/leader-keys
    ;;"SPC" '(execute-extended-command :which-key "M-X")
    "."   '(find-file :which-key "find file")
    "RET" '(consult-bookmark :which-key "Open Bookmarks")
    "/"   '(consult-line :which-key "search lines in buffer")
    ":"   '(eval-expression :which-key "evaluate expression")

    ;;Code
    "c"   '(:ignore c :which-key "Code")
    "cc"  '(eglot :which-key "code format")
    "cF"  '(eglot-format :which-key "code format")
    "ca"  '(eglot-code-actions :which-key "code actions")
    "cd"  '(consult-flymake :which-key "consult-flymake")
    "cf"  '(consult-imenu :which-key "consult imenu")
    "cl"   '(consult-outline :which-key "consutl-outline")'
    "cr"  '(eglot-rename :which-key "rename variable")

    ;; Files
    "f"   '(:ignore f :which-key "files")
    "ff"  '(dired-jump :which-key "dired")
    "fg"  '(consult-git-grep :which-key "gitgrep")
    "fr"  '(consult-ripgrep :which-key "ripgrep")

    ;;Magit
    "g"   '(:ignore g :which-key "Magit")
    "gd"  '(sam/magit-status-bare :which-key "magit status dotfiles")
    "gs"  '(sam/magit-status :which-key "magit status")

    ;; Help
    "h"   '(:ignore h :which-key "help")
    "ha"  '(helpful-at-point :which-key "at point help")'
    "hc"  '(helpful-command :which-key "command help")'
    "he"  '(lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/init.el")) :which-key "open emacs config")
    "hf"  '(helpful-function :which-key "function help")'
    "hk"  '(helpful-key :which-key "key help")'
    "hm"  '(helpful-macro :which-key "macro help")'
    "hr"  '(lambda () (interactive) (load-file (expand-file-name "~/.config/emacs/init.el")) :which-key "reload emacs config")
    "hs"  '(helpful-symbol :which-key "symbol help")'
    "hv"  '(helpful-variable :which-key "variable help")'

    ;; Org
    "o"   '(:ignore o :which-key "org")
    "ol"   '(consult-org-heading :which-key "go to heading")'
    "ot"   '(org-babel-tangle :which-key "tangle current buffer")'

    ;;Project
    "p"   '(:ignore p :which-key "Project")
    "pb"  '(project-switch-to-buffer :which-key "switch project buffer")
    "pc"  '(project-compile :which-key "compile project")
    "pd"  '(project-dired :which-key "project dired")
    "pf"  '(project-find-file :which-key "project find file")
    "pk"  '(project-kill-buffers :which-key "project kill buffers")
    "po"  '(consult-project-imenu :which-key "project outline")
    "pp"  '(project-switch-project :which-key "switch to project")
    "ps"  '(project-shell :which-key "project shell")

    ;; Toggles
    "t"   '(:ignore t :which-key "toggles")
    "tt"  '(sam/side-tree :which-key "toggle side tree")
    "tT"  '(consult-theme :which-key "switch theme")
    "ty"  '(consult-yank :which-key "choose yanked")
    )
  )

;; Mu4e
(defun my-mu4e-action-view-with-xwidget (msg)
  "View the body of the message inside xwidget-webkit."
  (unless (fboundp 'xwidget-webkit-browse-url)
    (mu4e-error "No xwidget support available"))
  (let* ((html (mu4e-message-field msg :body-html))
 	 (txt (mu4e-message-field msg :body-txt))
 	 (tmpfile (format "%s%x.html" temporary-file-directory (random t))))
    (unless (or html txt)
      (mu4e-error "No body part for this message"))
    (with-temp-buffer
      (insert (or html (concat "<pre>" txt "</pre>")))
      (write-file tmpfile)
      (xwidget-webkit-browse-url (concat "file://" tmpfile) t))))

(use-package mu4e
  :defer 5
  :commands mu4e
  :config
  (setq
   mu4e-completing-read-function 'completing-read                        ;; use buffer minibuffer
   mu4e-split-view 'vertical                                             ;; use vertical split
   message-kill-buffer-on-exit t                                         ;; kill message after exit
   mu4e-context-policy 'pick-first                                       ;; always pick first context
   mu4e-confirm-quit nil                                                 ;; don't ask to quit
   mu4e-change-filenames-when-moving t                                   ;; this is needed if using mbsync
   mu4e-update-interval (* 10  60)                                       ;; update in seconds
   mu4e-get-mail-command "mbsync -c /home/sam/.config/isync/mbsyncrc -a" ;; update command
   mu4e-root-maildir "~/.local/share/mail"                               ;; maildir location
   mu4e-compose-format-flowed t                                          ;; whether to compose messages to be sent as format=flowed.
   message-send-mail-function 'smtpmail-send-it                          ;; use smtpmail-send-it to send mail
   mu4e-view-prefer-html t                                               ;; whether to base the body display on the html-version.
                                        ;mu4e-view-use-gnus 't                                                 ;; use gnu article mode for view
   mu4e-view-show-images 't)                                             ;; show images in email
  (add-to-list 'mu4e-view-actions
 	       '("xViewXWidget" . my-mu4e-action-view-with-xwidget) t)

  (setq mu4e-headers-fields
        '(
          (:human-date    . 12)    ;; alternatively, use :human-date
          (:flags         . 6)
          (:from-or-to    . 21)
          (:subject       . 46))))


;; Elfeed
(use-package elfeed
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
 	'("https://archlinux.org/feeds/news/"
          "https://karthinks.com/index.xml"
          "https://github.blog/feed/"
          "https://oremacs.com/atom.xml"
          "https://suckless.org/atom.xml"
          )))


;; Org mode
(use-package org
  :straight nil
  :commands (org-capture org-agenda)
  :config
  (org-indent-mode)
  (visual-line-mode 1)
  (setq org-ellipsis " ➜")

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (C . t)
       (python . t)))))
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("un" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("c" . "src C")))))
(use-package haskell-mode)
