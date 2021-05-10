;; -*- lexical-binding: t; -*-

(global-hl-line-mode 1)                                            ;; Enable line highlighting
(setq-default display-line-numbers 'relative)                      ;; Enable relative line number mode
(setq read-process-output-max (* 1024 1024))                       ;; Increase the amount of data which Emacs reads from the process.
(setq-default bidi-display-reordering 'left-to-right               ;; Disable right-to-left since i dont use it
              bidi-paragraph-direction 'left-to-right)
(setq auto-hscroll-mode 'current-line)                             ;; Horizontally scroll only the current line
(setq fast-but-imprecise-scrolling t)                              ;; More performant rapid scrolling over unfontified regions.
(setq ffap-machine-p-known 'reject)                                ;; Don't ping things that look like domain names.
(setq inhibit-startup-screen t)                                    ;; Disable startup screen
(setq ring-bell-function 'ignore)                                  ;; Disable error bell
(setq initial-scratch-message "")                                  ;; Disable scratch buffer on startup
(setq initial-major-mode 'fundamental-mode)                        ;; Disable scratch mode on startup
(setq mode-line-format nil)                                        ;; Dont show default modeline until my modeline is loaded
(setq warning-minimum-level :emergency)                            ;; Set warning messages to emergency
(setq highlight-nonselected-windows nil)                           ;; don't highlight non selected windows
(setq frame-inhibit-implied-resize t)                              ;; Dont resize window implicitly
(setq create-lockfiles nil)                                        ;; Disable lockfiles
(setq make-backup-files nil)                                       ;; Disable backup files
(setq auto-save-default nil)                                       ;; Disable autosave
(setq make-backup-files nil)                                       ;; Disable backup
(setq tab-width 4)                                                 ;; Set tab width to 4
(setq indent-tabs-mode nil)                                        ;; Use space only
(setq tab-always-indent 'complete)                                 ;; Tab completion
(setq x-select-enable-clipboard t)                                 ;; Use system secondary clipboard
(setq x-select-enable-primary t)                                   ;; Use system primary clipboard
(setq cursor-in-non-selected-window nil)                           ;; Hide cursor in non-focused window
(setq help-window-select t)                                        ;; Focus help window when opened
(setq blink-cursor-mode nil)                                       ;; Disable cursot blinking
(setq global-visual-line-mode t)                                   ;; Disable line wrap
(setq column-number-mode t)                                        ;; Show column numbers mode
(setq delete-selection-mode t)                                     ;; Typing will overwrite the selected text
(setq global-auto-revert-mode t)                                   ;; Reload file as they change on disk
(defalias 'yes-or-no-p 'y-or-n-p)                                  ;; Alias yes/no to y/n
(dolist                                                            ;; Disable number line in certiain mode
  (mode '(org-mode-hook
          term-mode-hook
          shell-mode-hook
          treemacs-mode-hook
          eshell-mode-hook))
          (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Set fonts
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

;; a special comment highlight for elisp so i can easily scan through diffrent parts of my config eg : ;;;; HEADING
;;https://emacs.stackexchange.com/questions/28232/syntax-highlighting-for-comments-starting-with-specific-sequence-of-characters
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
(defface heading-comment '((t (:foreground "#57c7ff" :background "#282a36" :weight bold))) "blue")
(font-lock-add-keywords
 'emacs-lisp-mode '((";;;;.*" 0 'heading-comment t)))
 

;; Print time took to load emacs in messages buffer
(defun sam/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))
 
(add-hook 'emacs-startup-hook #'sam/display-startup-time)


;;;; STRAIGHT
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


;;;; USE-PACKAGE
;; install use-package
(straight-use-package 'use-package)
(setq use-package-compute-statistics t)


;;;; NO-LITTERING
;; Need to set user-emacs-directory before this
;; Keep emacs directory clean 
(use-package no-littering)


;;;; GCMH
(use-package gcmh
  :config
  (setq gcmh-idle-delay 10 
        gcmh-high-cons-threshold 16777216)
  (gcmh-mode +1))


;;;; DOOM THEMES
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


;;;; DOOM MODLINE
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


;;;; ALL THE ICONS
;; Icons to display in modline
(use-package all-the-icons
  :defer 0)


;;;; SELECTRUM
;; Minibuffer completion package
(use-package selectrum
  :disabled t
  :defer 0
  :bind (:map selectrum-minibuffer-map
              ("C-j" . 'selectrum-next-candidate )
              ("C-k" . 'selectrum-previous-candidate )
              ("C-d" . 'selectrum-next-page )
              ("C-u" . 'selectrum-previous-page ))
  :config
  (selectrum-mode +1)                                                                             
  (setq
   ;;use fix height no matter how many candidates
   selectrun-fix-vertical-window-height t                                                          
   ;;max number of items can be displayed
   selectrum-num-candidates-displayed 20                                                          
   ;;case insensitive search
   completion-ignore-case t                                                                      
   ;;extend selection background to screen width
   selectrum-extend-current-candidate-highlight 't                                                
   ;;show indices
   selectrum-show-indices 't)
  :custom-face
  ;;current selected item face
  (selectrum-current-candidate ((t (:foreground "#50fa7b" :background "#44475a" :weight bold))))    
  ;;matched item face
  (selectrum-prescient-primary-highlight ((t (:foreground "#ffb86c"))))                            
  (selectrum-prescient-secondary-highlight ((t (:foreground "#ffb86c")))))


;;;; VERTICO
;; Minibuffer completion package
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


;;;; ORDERLESS
;; Minibuffer sorting package
(use-package orderless
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;;;; SAVEHIST
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (savehist-mode))


;;;; PRESCIENT
;;Comanion for selectrum remembers the most used options and sorts accordingly
(use-package selectrum-prescient
  :disabled t
  :after selectrum
  :config
  ;;turn on prescient mode
  (selectrum-prescient-mode +1)         
  ;;prescient mode will persist between session
  (prescient-persist-mode +1))


;;;; CONSULT
;; Provide varios useful commands for selectrum
(use-package consult
  :after (:any selectrum icomplete-vertical vertico)
  :config
  ;;(setq register-preview-delay 0
  ;;      register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window))


;;;; MARGINALIA
;; Show annotations about the commands min selectrum
(use-package marginalia
  :after (:any selectrum icomplete-vertical vertico)
  :config
  (marginalia-mode)
  ;;show info about the item
  (setq marginalia-annotators '(marginalia-annotators-heavy t)))


;;;; HELPFUL
;; More helpful help 
(use-package helpful
  :commands (helpful-callable helpful-function helpful-macro helpful-variable helpful-command helpful-key))


;;;; EVIl
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


;;;; EVIL-COLLECTION
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;;;; HYDRA
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


;;;; WHICH-KEY
;; Shows all keymaps
(use-package which-key
  :after evil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;;;; GENERAL
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
    "he"  '(lambda () (interactive) (find-file (expand-file-name "~/.config/emacs/init.el")) :which-key "open emacs config")
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


;;;; Org mode
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
   org-startup-indented t))

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


;;;; ORG-BULLET
;; Add nice bullets to org headings
 (use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


;;;; EGLOT
;; Lsp with suport for various languages
(use-package eglot
  :after (:any python-mode haskell-mode sh-mode c-mode)
  )


;;;; COMPANY
;; A autocompletion package
(use-package company
  :hook
  ('emacs-startup-hook . 'global-company-mode)
  :config
  (setq global-company-mode 't)
  :bind (:map company-active-map
    ("<tab>" . 'company-complete-selection)
    ("C-j" . 'company-select-next)
    ("C-k" . 'company-select-previous))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))


;;;; CORFU
(use-package corfu
  :disabled t
  :init
  (corfu-global-mode)
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ("S-TAB" . corfu-previous))
  :config
  (setq corfu-cycle t)
  )


;;;; DABBREV
;; Dabbrev works with Corfu
(use-package dabbrev
  :disabled t
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))


;;;; ELECTRIC
(use-package elec-pair
  :config
  (electric-pair-mode 1))


;;;; PAREN
(use-package paren
  :straight nil
  :config
  ;; Show matching parenthesis
  (show-paren-mode 1)
  (setq
  show-paren-highlight-openparen t      ;; Turns on openparen highlighting when matching forward.
  show-paren-when-point-inside-paren t  ;; Show parens when point is just inside one.
  show-paren-when-point-periphery t     ;; Show parens when point is in the line's periphery.
  show-paren-style 'parenthesis))       ;; Paren mode style parenthesis, expression, mixed

;;;; Python mode
(use-package python-mode
  :commands (python-mode)
  :config (add-hook 'python-mode-hook 'eglot-ensure))


;;;; Haskell mode
(use-package haskell-mode
  :commands (haskell-mode)
  :config (add-hook 'haskell-mode-hook 'eglot-ensure))


;;;; Sell mode
(use-package sh-script
  :commands (sh-mode))


;;;; CLang mode
(use-package cc-mode
  :commands (c-mode)
  :config (add-hook 'c-mode-hook 'eglot-ensure))


;;;; Lua mode
(use-package lua-mode
  :commands (lua-mode))

;; A git client for emacs
(use-package magit
  :commands (:any sam/magit-status sam/magit-status-dotfiles)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )
(use-package forge
  :after magit)

;; Wrapper for handling git bare repos 
;;https://emacs.stackexchange.com/questions/30602/use-nonstandard-git-directory-with-magit
(defvar dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(defvar dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))
(defun sam/magit-status-dotfiles ()
  "use `magit-status` on git bare repo set `dotfiles-git-dir` and `dotfiles-work-tree`"
  (interactive)
  (require 'magit-git)
  (add-to-list 'magit-git-global-arguments dotfiles-git-dir) 
  (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
  (call-interactively 'magit-status))

;; remove the git-dir and work-tree variables
(defun sam/magit-status ()
  "magit-status wrapper to escape `sam/magit-status-dotfiles`"
  (interactive)
  (require 'magit-git)
  (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
  (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
  (call-interactively 'magit-status))


;;;; DIRED
;; Emacs file manager
;;TODO need configure evil keys
(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first")))


;;;; ELFEED
;; A rss client for emacs
;; TODO need to add my urls
(use-package elfeed
  :commands (:any elfeed elfeed-update)
  :config
  (setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  
        ("http://nedroid.com/feed/" webcomic))))


;;;; MU4E
(use-package mu4e
  :defer 5
  :commands mu4e
  :config
  (setq
   mu4e-completing-read-function 'completing-read                        ;; use buffer minibuffer
   mu4e-split-view 'single-window ;; use full screen view
   message-kill-buffer-on-exit t ;; kill message after exit
   mu4e-context-policy 'pick-first ;; always pick first context
   mu4e-confirm-quit nil ;; don't ask to quit
   mu4e-change-filenames-when-moving t ;; this is needed if using mbsync
   mu4e-update-interval (* 60  60) ;; update in seconds
   mu4e-get-mail-command "mbsync -c /home/sam/.config/isync/mbsyncrc -a" ;; update command
   mu4e-maildir "~/.local/share/mail" ;; maildir location
   mu4e-compose-format-flowed t ;;Whether to compose messages to be sent as format=flowed.
   message-send-mail-function 'smtpmail-send-it ;; use smtpmail-send-it to send mail
   mu4e-html2text-command "w3m -T text/html" ;; command used to view html emails
   mu4e-view-prefer-html t ;;Whether to base the body display on the html-version.
   mu4e-view-use-gnus 't ;; use gnu article mode for view
   mu4e-view-show-images 't) ;; show images in email
  (mu4e-headers-fields                                                      ;; define field width
      '((:date          .  10)
	      (:flags         .   4)
	      (:from-or-to    .  30)
	      (:mailing-list  .  30)
	      (:subject       .  nil)))

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
