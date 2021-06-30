;; -*- lexical-binding: t; -*-

;; Evil
(use-package evil
  :hook (emacs-startup . evil-mode)
  :init
  (setq evil-undo-system 'undo-tree)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil))



;; Evil collection
(use-package evil-collection
  :after evil
  :config
  ;;(evil-collection-init '(elfeed mu4e custom magit ibuffer help helpful term ansi-term)))
  (evil-collection-init))


;; Which key
(use-package which-key
  :after evil
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))



;; General
(defun sam/switch-to-next-buffer ()
  "similar to `switch-to-next-buffer' but ignores special buffers"
  (interactive)
  (next-buffer)
  (while (string-match-p "^\\*" (buffer-name))
    (next-buffer)))

(defun sam/switch-to-prev-buffer ()
  "similar to `switch-to-prev-buffer' but ignores special buffers"
  (interactive)
  (previous-buffer)
  (while (string-match-p "^\*" (buffer-name))
    (previous-buffer)))

(defun sam/switch-next-buffer-or-window ()
  "move focus to next window if exist or cycle forward through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window 1)
    (sam/switch-to-next-buffer)
    ))

(defun sam/switch-prev-buffer-or-window ()
  "move focus to next window if exist or cycle backward through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window -1)
    (sam/switch-to-prev-buffer)
    ))

(defun sam/split-window-right ()
  "split window right switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf))))


(defun sam/split-window-below ()
  "split window below switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-below)
    (windmove-down)
    (let (($buf (generate-new-buffer "untitled")))
      (switch-to-buffer $buf))))


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

(defun sam/toggle-term()
  "toggle ansi term"
  (interactive)
  (if (get-buffer "*terminal*")
      (kill-buffer "*terminal*")
    (term)))


;; not usefull anymore
(defun sam/kill-buffer-or-window ()
  (interactive)
  (if(> (count-windows) 1)
      (progn)
      (kill-buffer-and-window)
   (kill-this-buffer)))

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
   "M-t"  'ansi-term
   ;; "M-SPC" 'sam/toggle-side-tree
   )

  ;; buffers
  (general-define-key
   "M-<tab>" 'sam/switch-to-next-buffer
   "M-<iso-lefttab>" 'sam/switch-to-prev-buffer
   "M-b" 'consult-buffer
   "M-B" 'ibuffer
   "M-q" 'kill-buffer-and-window
   "M-Q" 'delete-window
   "M-C-b" 'switch-to-buffer-other-window
   "M-C-B" 'switch-to-buffer-other-frame)

  ;; window
  (general-def 'normal
    "M-s" 'sam/split-window-right
    "M-v" 'sam/split-window-below
    ;;"M-j" 'windmove-down
    ;;"M-k" 'windmove-up
    "M-j" 'sam/switch-next-buffer-or-window
    "M-k" 'sam/switch-prev-buffer-or-window
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
    :global-prefix "C-SPC")
  (sam/leader-keys
    ;;"SPC" '(execute-extended-command :which-key "M-X")
    "."   '(find-file :which-key "find file")
    "RET" '(consult-bookmark :which-key "Open Bookmarks")
    "/"   '(consult-line :which-key "search lines in buffer")
   ";"   '(eval-expression :which-key "evaluate expression")

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
    "fg"  '(consult-ripgrep :which-key "ripgrep")
    "fr"  '(consult-recent-file :which-key "ripgrep")

    ;;Magit
    "g"   '(:ignore g :which-key "Magit")
    "gd"  '(sam/magit-status-bare :which-key "magit status dotfiles")
    "gs"  '(sam/magit-status :which-key "magit status")

    ;; Help
    "h"   '(:ignore h :which-key "help")
    "ha"  '(helpful-at-point :which-key "at point help")'
    "hc"  '(helpful-command :which-key "command help")'
    "he"  '(lambda () (interactive) (find-file (expand-file-name "~/.config/emacs")) :which-key "open emacs config")
    "hf"  '(helpful-function :which-key "function help")'
    "hk"  '(helpful-key :which-key "key help")'
    "hm"  '(helpful-macro :which-key "macro help")'
    "hr"  '(lambda () (interactive) (load-file (expand-file-name "~/.config/emacs/init.el")) :which-key "reload emacs config")
    "hs"  '(helpful-symbol :which-key "symbol help")'
    "hv"  '(helpful-variable :which-key "variable help")'
    "hh"  '(consult-apropos :which-key "apropos search")

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
    "tt"  '(consult-theme :which-key "switch theme")
    "ty"  '(consult-yank :which-key "choose yanked")
    "tf"  '(text-scale-adjust :which-key "scale text")
    )
  )

(provide 'keys.el)
