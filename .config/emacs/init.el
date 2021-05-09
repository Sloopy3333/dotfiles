(defun sam/set-font ()
(set-face-attribute 'default nil
        	    :font "Hack Nerd Font 12"
        	    :weight 'regular)
(set-face-attribute 'variable-pitch nil
        	    :font "Hack Nerd Font 12"
        	    :weight 'regular)
(set-face-attribute 'fixed-pitch nil
        	    :font "Hack Nerd Font 12"
        	    :weight 'regular))
 
;; if in daemon mode 
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
        	(with-selected-frame frame
        	  (sam/set-font))))
    (sam/set-font))

;; Print time took to load emacs in messages buffer
(defun sam/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
 
(add-hook 'emacs-startup-hook #'sam/display-startup-time)

;; Switch to scratch buffer
(defun sam/switch-to-scratch-buffer ()
  "Switch to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))
 
;; Switch to messages buffer
(defun sam/switch-to-messages-buffer ()
  "Switch to messages buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

;;bootstrap straight
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

;; install use-package
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)

;; Need to set user-emacs-directory before this
;; Keep emacs directory clean 
(use-package no-littering)

(use-package gcmh
  :config
  (setq gcmh-idle-delay 10 ; garbage collect after 10s of idle time
        gcmh-high-cons-threshold 16777216) ; 16mb
  )

;; A collection of doom themes
(use-package doom-themes
  :defer 0
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Minimal modeline from doom
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

;; Icons to display in modline
(use-package all-the-icons
  :defer 0)

;;Minibuffer completion package
;;(use-package selectrum
;;  :defer 0
;;  :bind (:map selectrum-minibuffer-map
;;              ("C-j" . 'selectrum-next-candidate )
;;              ("C-k" . 'selectrum-previous-candidate )
;;              ("C-d" . 'selectrum-next-page )
;;              ("C-u" . 'selectrum-previous-page ))
;;  :config
;;  (selectrum-mode +1)                                                                             
;;  (setq
;;   ;;use fix height no matter how many candidates
;;   selectrun-fix-vertical-window-height t                                                          
;;   ;;max number of items can be displayed
;;   selectrum-num-candidates-displayed 20                                                          
;;   ;;case insensitive search
;;   completion-ignore-case t                                                                      
;;   ;;extend selection background to screen width
;;   selectrum-extend-current-candidate-highlight 't                                                
;;   ;;show indices
;;   selectrum-show-indices 't)
;;  :custom-face
;;  ;;current selected item face
;;  (selectrum-current-candidate ((t (:foreground "#50fa7b" :background "#44475a" :weight bold))))    
;;  ;;matched item face
;;  (selectrum-prescient-primary-highlight ((t (:foreground "#ffb86c"))))                            
;;  (selectrum-prescient-secondary-highlight ((t (:foreground "#ffb86c")))))

(use-package vertico
  :defer 0
  :bind (:map vertico-map
	  ("C-j" . vertico-next)
	  ("C-k" . vertico-previous)
	  ("C-u" . vertico-scroll-down)
	  ("C-d" . vertico-scroll-up)
	  )
  :config
  (vertico-mode)
  (setq vertico-cycle t
	vertico-count 20))

(use-package orderless
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (savehist-mode))

;;Comanion for selectrum remembers the most used options and sorts accordingly
;;(use-package selectrum-prescient
;;  :after selectrum
;;  :config
;;  ;;turn on prescient mode
;;  (selectrum-prescient-mode +1)         
;;  ;;prescient mode will persist between session
;;  (prescient-persist-mode +1))

;; Provide varios useful commands for selectrum
(use-package consult
  :after (:any selectrum icomplete-vertical vertico)
  selectrum
  :config
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (consult-narrow-key "<"))

;; Show annotations about the commands min selectrum
(use-package marginalia
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (marginalia-mode)
  ;;show info about the item
  (setq marginalia-annotators '(marginalia-annotators-heavy t)))

;; More helpful help 
(use-package helpful
  :commands (helpful-callable helpful-function helpful-macro helpful-variable helpful-command helpful-key))

;; Vim keybinding for emacs
(use-package evil
  :defer 0
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; A modal package prevents repeated key press
(use-package hydra
  :after evil)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("q" nil "finished" :exit t))

(defhydra hydra-window-size (:timeout 4)
  "adjust window size"
  ("j" evil-window-increase-height "in")
  ("k" evil-window-decrease-height "out")
  ("l" evil-window-increase-width "in")
  ("h" evil-window-decrease-width "out")
  ("q" nil "finished" :exit t))

;; Shows all keymaps
(use-package which-key
  :after evil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; A easy way to map your keybinding
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
  :after evil
  :config
  (general-create-definer sam/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (sam/leader-keys
    "SPC" '(execute-extended-command :which-key "M-X")
    "."   '(find-file :which-key "find file")
    "RET" '(consult-bookmark :which-key "Open Bookmarks")
    "/"   '(consult-line :which-key "search lines in buffer")
    ":"   '(eval-expression :which-key "evaluate expression")

    ;; Buffers
    "b"   '(:ignore b :which-key "buffers")
    "bb"  '(consult-buffer :which-key "buffer menu")
    "b]"  '(switch-to-next-buffer :which-key "switch to next buffer")
    "b["  '(switch-to-prev-buffer :which-key "switch to previous buffer")
    "bn"  '(switch-to-next-buffer :which-key "switch to next buffer")
    "bp"  '(switch-to-prev-buffer :which-key "switch to previous buffer")
    "bs"  '(sam/switch-to-scratch-buffer :which-key "switch to scratch buffer")
    "bm"  '(sam/switch-to-messages-buffer :which-key "switch to messages buffer")
    "bd"  '(evil-delete-buffer :which-key "kill present buffer")
    "bk"  '(kill-buffer :which-key "select a buffer to kill")
    "bc"  '(clone-indirect-buffer-other-window :which-key "Clone buffer to new window")
    "bl"  '(consult-buffer :which-key "search lines in buffer")
    "bo"  '(evil-buffer-new :which-key "new empty buffer")
    "bf"  '(consult-buffer-other-frame :which-key "open a buffer in other frame")
    "bw"  '(consult-buffer-other-window :which-key "open a buffer in other windowframe")

    ;;Bookmarks
    "m"   '(:ignore m :which-key "Bookmarks")
    "ml"  '(consult-bookmark :which-key "list bookmarks")
    "mm"  '(consult-mark :which-key "jump local bookmarks")
    "ms"  '(bookmark-set :which-key "add bookmark")
    "mS"  '(bookmark-save :which-key "save bookmark")
    "mr"  '(bookmark-rename :which-key "rename bookmark")
    "md"  '(bookmark-delete :which-key "delete bookmark")

    ;;Code
    "c"   '(:ignore c :which-key "Code")
    "ca"  '(eglot-code-actions :which-key "code actions")
    "cr"  '(eglot-rename :which-key "rename variable")
    "cf"  '(consult-imenu :which-key "list functions")
    "cF"  '(eglot-format :which-key "format variable")
    "cl"   '(consult-outline :which-key "go to heading")'

    ;;Magit
    "g"   '(:ignore g :which-key "Magit")
    "gs"  '(sam/magit-status :which-key "magit status")
    "gd"  '(sam/magit-status-dotfiles :which-key "magit status dotfiles")

    ;;Project
    "p"   '(:ignore m :which-key "Project")
    "pf"  '(consult-project-imenu :which-key "list functions in project")

    ;; Org
    "o"   '(:ignore o :which-key "org")
    "ol"   '(consult-org-heading :which-key "go to heading")'
    "ot"   '(org-babel-tangle :which-key "tangle current buffer")'

    ;; Files
    "f"   '(:ignore f :which-key "files")
    "ff"  '(dired-jump :which-key "dired")
    "fr"  '(consult-ripgrep :which-key "ripgrep")
    "fg"  '(consult-git-grep :which-key "gitgrep")

    ;; Help
    "h"   '(:ignore h :which-key "help")
    "he"  '(lambda () (interactive) (find-file (expand-file-name "~/dot/dotfiles.org")) :which-key "open emacs config")
    "hr"  '(lambda () (interactive) (load-file (expand-file-name "~/.config/emacs/init.el")) :which-key "reload emacs config")
    "hf"  '(helpful-function :which-key "function help")'
    "hv"  '(helpful-variable :which-key "variable help")'
    "hk"  '(helpful-key :which-key "key help")'
    "hm"  '(helpful-macro :which-key "macro help")'
    "hc"  '(helpful-command :which-key "command help")'
    "hs"  '(helpful-symbol :which-key "symbol help")'
    "ha"  '(helpful-at-point :which-key "at point help")'

    ;; Toggles
    "t"   '(:ignore t :which-key "toggles")
    "tt"  '(consult-theme :which-key "choose theme")
    "ty"  '(consult-yank :which-key "choose yanked")
    "ts"  '(hydra-text-scale/body :which-key "scale text")

    ;; Window
    "w"   '(:ignore w :which-key "window")
    "wh"  '(evil-window-split :which-key "horizontal split")
    "wv"  '(evil-window-vsplit :which-key "vertical split")
    "wd"  '(evil-window-delete :which-key "delete window")
    "wn"  '(evil-window-next :which-key "focus next window")
    "wp"  '(evil-window-prev :which-key "focus previous window")
    "wj"  '(evil-window-down :which-key "focus down window")
    "wk"  '(evil-window-up :which-key "focus up window")
    "wl"  '(evil-window-right :which-key "focus right window")
    "wh"  '(evil-window-left :which-key "left window")
    "ws"  '(hydra-window-size/body :which-key "adjust window size")
  )
)

;; Org mode
(defun sam/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
                  (set-face-attribute (car face) nil :font "Hack Nerd Font" :weight 'bold :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))
    
(use-package org
  :commands (org-capture org-agenda)
  :config
   (sam/org-font-setup)
   (org-indent-mode)
   (visual-line-mode 1)
   (setq org-ellipsis " ➜"
   org-src-preserve-indentation t
   org-edit-src-content-indentation '0
   org-startup-indented t)
   :hook
   ;; auto tangle on save
   (org-mode . (lambda () (add-hook 'after-save-hook 'org-babel-tangle :append :local))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
	    (haskell . t)
	    (C . t)
	    (lua . t)
        (python . t)))
(push '("conf-unix" . conf-unix) org-src-lang-modes))
(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
  (add-to-list 'org-structure-template-alist '("lu" . "src lua"))
  (add-to-list 'org-structure-template-alist '("un" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("c" . "src C")))

;; Add nice bullets to org headings
 (use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Lsp with suport for various languages
 (use-package eglot
   :after (:any python-mode haskell-mode sh-mode c-mode)
   )

;; A autocompletion package
(use-package company
  :defer 0
  ;;:hook
  ;;('after-init . 'global-company-mode)
  :config
  (setq global-company-mode 't)
  :bind (:map company-active-map
    ("<tab>" . 'company-complete-selection)
    ("C-j" . 'company-select-next)
    ("C-k" . 'company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;(use-package corfu
;;  :init
;;  (corfu-global-mode)
;;  :bind (:map corfu-map
;;         ("TAB" . corfu-next)
;;         ("S-TAB" . corfu-previous))
;;  :config
;;  (setq corfu-cycle t)
;;)
;;
;;;; Dabbrev works with Corfu
;;(use-package dabbrev
;;  ;; Swap M-/ and C-M-/
;;  :bind (("M-/" . dabbrev-completion)
;;         ("C-M-/" . dabbrev-expand)))

(use-package elec-pair
  :config
  (electric-pair-mode 1)
  (electric-indent-mode 1))

;; Python mode
(use-package python-mode
  :commands (python-mode)
  :config (add-hook 'python-mode-hook 'eglot-ensure))

;; Haskell mode
(use-package haskell-mode
  :commands (haskell-mode)
  :config (add-hook 'haskell-mode-hook 'eglot-ensure))

;; Sell mode
(use-package sh-script
  :commands (sh-mode))
  ;;:config (add-hook 'sh-mode-hook 'eglot-ensure))

;; CLand mode
(use-package cc-mode
  :commands (c-mode)
  :config (add-hook 'c-mode-hook 'eglot-ensure))

;; CLand mode
(use-package lua-mode
  :commands (lua-mode)
  :config (add-hook lua-mode-hook 'eglot-ensure))

;; Wrapper for handling git bare repos 
;; A git client for emacs
(use-package magit
  :commands (:any sam/magit-status sam/magit-status-dotfiles)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;;https://emacs.stackexchange.com/questions/30602/use-nonstandard-git-directory-with-magit
;; set git-dir and work-tree
;; function to start magit on git bare repo
(defun sam/magit-status-dotfiles ()
  (interactive)
  (require 'magit-git)
  (defvar dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
  (defvar dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir) 
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (call-interactively 'magit-status))
;;
;;;; remove the git-dir and work-tree variables
(defun sam/magit-status ()
  (interactive)
  (require 'magit-git)
  (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))

;; Emacs file manager
;;TODO need configure evil keys
;;(use-package dired
;;  :straight nil
;;  :commands (dired dired-jump)
;;  :custom ((dired-listing-switches "-agho --group-directories-first")))
;;
;;(use-package dired+
;;  :after dired)
;;
;;(use-package dired-single
;;  :after dired
;;  :commands (dired dired-jump))
;;
;;(use-package all-the-icons-dired
;;  :after dired
;;  :hook (dired-mode . 'all-the-icons-dired-mode))
;;
;;(use-package dired-open
;;  :after dired
;;  :commands (dired dired-jump)
;;  :config
;;  ;; Doesn't work as expected!
;;  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
;;  (setq dired-open-extensions '(("png" . "feh")
;;                                ("mkv" . "mpv"))))
;;
;;(use-package dired-hide-dotfiles
;;  :after dired
;;  :hook (dired-mode . 'dired-hide-dotfiles-mode)
;;  :config
;;  (evil-collection-define-key 'normal 'dired-mode-map
;;    "." 'dired-hide-dotfiles-mode))

;; A rss client for emacs
;; TODO need to add my urls
(use-package elfeed
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic))))

(use-package mu4e
  :defer 5
  :commands mu4e
  :config
  (setq
   ;; use buffer selectrum
   mu4e-completing-read-function 'completing-read
   ;; use full screen view
   mu4e-split-view 'single-window 
   ;; kill message after exit
   message-kill-buffer-on-exit t
   ;; always pick first context
   mu4e-context-policy 'pick-first
   ;; don't ask to quit
   mu4e-confirm-quit nil
   ;; this is needed if using mbsync
   mu4e-change-filenames-when-moving t
   ;; update in seconds
   mu4e-update-interval (* 60  60)
   ;; update command
   mu4e-get-mail-command "mbsync -c /home/sam/.config/isync/mbsyncrc -a"
   ;; maildir location
   mu4e-maildir "~/.local/share/mail"
   ;;Whether to compose messages to be sent as format=flowed.
   mu4e-compose-format-flowed t
   ;; use smtpmail-send-it to send mail
   message-send-mail-function 'smtpmail-send-it
   ;; command used to view html emails
   mu4e-html2text-command "w3m -T text/html"
   ;;Whether to base the body display on the html-version.
   mu4e-view-prefer-html t
   ;; use gnu article mode for view
   mu4e-view-use-gnus 't
   ;; show images in email
   mu4e-view-show-images 't)
  ;; define field width
  (mu4e-headers-fields
      '((:date          .  10)
	      (:flags         .   4)
	      (:from-or-to    .  30)
	      (:mailing-list  .  30)
	      (:subject       .  nil))
        mu4e-headers-from-or-to-prefix nil)

  (setq mu4e-contexts
          (list
          ;; Work account
          (make-mu4e-context
          :name "Sudo"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Sudo" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Sudo")
                  (smtpmail-smtp-server  . "mail.cock.li")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Sudo/Drafts")
                  (mu4e-sent-folder  . "/Sudo/Sent")
                  (mu4e-trash-folder  . "/Sudo/Trash")
		  ))
	  
          ;; T&P account
          (make-mu4e-context
          :name "T&P"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/T&P" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "T&P")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/T&P/[Gmail]/Drafts")
                  (mu4e-sent-folder  . "/T&P/[Gmail]/Sent Mail")
                  (mu4e-refile-folder  . "/T&P/[Gmail].All Mail")
                  (mu4e-trash-folder  . "/T&P/[Gmail]/Trash")))

          ;; Code account
          (make-mu4e-context
          :name "Code"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Code" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Code")
                  (smtpmail-smtp-server  . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Code/Drafts")
                  (mu4e-sent-folder  . "/Code/Sent")
                  (mu4e-trash-folder  . "/Code/Trash")))

          ;; Game account
          (make-mu4e-context
          :name "Game"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Game" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Game")
                  (smtpmail-smtp-server  . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Game/Drafts")
                  (mu4e-sent-folder  . "/Game/Sent")
                  (mu4e-trash-folder  . "/Game/Trash")))

          ;; Linux account
          (make-mu4e-context
          :name "Linux"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Linux" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Linux")
                  (smtpmail-smtp-server  . "mail.cock.li")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Linux/Drafts")
                  (mu4e-sent-folder  . "/Linux/Sent")
                  (mu4e-trash-folder  . "/Linux/Trash")
		  ))

          ;; Cloud account
          (make-mu4e-context
          :name "Cloud"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Cloud" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Cloud")
                  (smtpmail-smtp-server  . "mail.cock.li")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Cloud/Drafts")
                  (mu4e-sent-folder  . "/Cloud/Sent")
                  (mu4e-trash-folder  . "/Cloud/Trash")
		  ))

          ;; Shop account
          (make-mu4e-context
          :name "Shop"
          :match-func
              (lambda (msg)
              (when msg
                  (string-prefix-p "/Shop" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name    . "Shop")
                  (smtpmail-smtp-server  . "mail.cock.li")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder  . "/Shop/Drafts")
                  (mu4e-sent-folder  . "/Shop/Sent")
                  (mu4e-trash-folder  . "/Shop/Trash")
		  ))
  ))
  )

(setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)
