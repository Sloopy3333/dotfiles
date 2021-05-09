;; Increase gc memory until init
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil)
;;https://github.com/CSRaghunandan/.emacs.d/blob/master/setup-files/setup-optimizations.el
;; Increase the amount of data which Emacs reads from the process.
(setq read-process-output-max (* 1024 1024))
;; Disable right-to-left since i dont use it
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
;; Horizontally scroll only the current line
;; https://www.reddit.com/r/emacs/comments/6au45k/is_it_possible_to_truncate_long_lines_the_same/
(setq auto-hscroll-mode 'current-line)
;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)
;; don't highlight non selected windows
(setq highlight-nonselected-windows nil)
;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Disable toolbar
(tool-bar-mode -1)
;; Disable scrollbar
(scroll-bar-mode -1)
;; Disable tooltip
(tooltip-mode -1)
;; Disable menubar
(menu-bar-mode -1)

(setq
 ;; Disable startup screen
 inhibit-startup-screen t
 ;; Initial scratch buffer message
 initial-scratch-message ""
 ;;Disable error bell
 ring-bell-function 'ignore
 ;; Disable scratch buffer on startup
 initial-major-mode 'fundamental-mode
 ;; Dont resize window implicitly
 frame-inhibit-implied-resize t
 ;; Dont show default modeline until my modeline is loaded
 mode-line-format nil
 ;; Set font
 ;;default-frame-alist '((font . "Hack Nerd Font Mono-12"))
 ;; Set warning messages to emergency
 warning-minimum-level :emergency)

;; Alias yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Parenthesis
;; Show matching parenthesis
(show-paren-mode 1)
;; Paren mode style parenthesis, expression, mixed
(setq show-paren-style 'parenthesis)
;; Auto close brackets and quotes
(electric-pair-mode +1)
;; List of pairs to be completed regardless of major mode

;;Line
;; Disable line wrap
(global-visual-line-mode +1)
;; Show column numbers mode
(column-number-mode)
;; Highlight line
(global-hl-line-mode 1)
;; Enable relative line number mode
(global-display-line-numbers-mode `relative)
;; Disable line numbers for some modes
(dolist
  (mode '(org-mode-hook
          term-mode-hook
          shell-mode-hook
          treemacs-mode-hook
          eshell-mode-hook))
          (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Files
(setq
 ;; Disable lockfiles
 create-lockfiles nil
 ;; Disable backup files
 make-backup-files nil
 ;; Disable autosave
 auto-save-default nil)
 ;; Reload file as they change on disk
 (global-auto-revert-mode 1)

;; Tab
(setq-default
 ;; Set tab width to 4
 tab-width 4
 ;; Use space only
 indent-tabs-mode nil
 ;; Tab completion
 tab-always-indent 'complete
 )

;;Clipboard
(setq
 ;; Use system secondary clipboard
 x-select-enable-clipboard t
 ;; Use system primary clipboard
 x-select-enable-primary t)

;; Remember cursor location across session
(save-place-mode 1)
;; Disable cursot blinking
(blink-cursor-mode 0)
;; Typing will overwrite the selected text
(delete-selection-mode 1)

 ;; Disable vc
 vc-handled-backends nil
 ;; Disable backup
 make-backup-files nil

;;some settings for straight and use-package
(setq
    ;;use develop branch
    straight-repository-branch "develop"
    ;; check package modificaton whe they are edited only
    straight-check-for-modifications '(check-on-save find-when-checking)
    ;; always compile packages natively
    straight-disable-native-compile nil
    ;; dont load packages before reading init file
    package-enable-at-startup nil
    ;;autoload-compute-prefix nil
    ;; tell straight to use use-package
    straight-use-package-by-default t
    ;;if set to t  use-package will be more verbose and write packages loading information in message buffer useful when debuging startup time
    use-package-verbose nil
    ;;if set to t use package will add -hook by default to :hook eg: :hook (c-mode) will be interpreted as c-mode-hook this
    use-package-hook-name-sufix nil)
(advice-add 'package--ensure-init-file :override 'ignore)

;;Emacs user directory
(setq user-emacs-directory "~/.local/share/emacs")
 
;; Custom.el path
(setq custom-file "~/.config/emacs/custom.el")
