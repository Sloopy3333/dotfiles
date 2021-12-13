;; -*- lexical-binding: t; -*-

;; General
(defcustom sam/skippable-buffer-regexp
  (rx bos (or "*Messages*" "*Help*" "*Backtrace*") eos)
  "Matching buffer names are ignored by `sam/next-buffer'
and `sam/previous-buffer'."
  :type 'regexp)

(defun sam/change-buffer (change-buffer)
  "Call CHANGE-BUFFER until `sam/skippable-buffer-regexp' doesn't match."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (string-match-p sam/skippable-buffer-regexp (buffer-name))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun sam/next-buffer ()
  "Variant of `next-buffer' that skips `sam/skippable-buffer-regexp'."
  (interactive)
  (sam/change-buffer 'next-buffer))

(defun sam/prev-buffer ()
  "Variant of `previous-buffer' that skips `sam/skippable-buffer-regexp'."
  (interactive)
  (sam/change-buffer 'previous-buffer))

(defun sam/switch-next-buffer-or-window ()
  "move focus to next window if exist or cycle forward through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window 1)
    (sam/next-buffer)
    ))

(defun sam/switch-prev-buffer-or-window ()
  "move focus to next window if exist or cycle backward through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window -1)
    (sam/prev-buffer)
    ))

(defun sam/split-window-right ()
  "split window right switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (switch-to-buffer "scratch")))

(defun sam/split-window-below ()
  "split window below switch to window and rename create new buffer named untitled$"
  (interactive)
  (progn
    (split-window-below)
    (windmove-down)
    (switch-to-buffer "scratch")))

;; not usefull anymore
(defun sam/kill-buffer-or-window ()
  (interactive)
  (if(> (count-windows) 1)
      (progn)
    (kill-buffer-and-window)
    (kill-this-buffer)))


(defun sam/toggle-single-window ()
  "delete other windows if more than one window exists
or restore previous layout if a single window exists"
  (interactive)
  (if (= (count-windows) 1)
      (when single-window--last-configuration
	(set-window-configuration single-window--last-configuration))
    (setq single-window--last-configuration (current-window-configuration))
    (delete-other-windows)))

(defun sam/toggle-window-split ()
  "switch between vertical and horizontal split only works for 2 windows"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(defun delete-enclosing-parentheses (&optional arg)
  "Delete the innermost enclosing parentheses around point.
With a prefix argument N, delete the Nth level of enclosing parentheses,
where 1 is the innermost level."
  (interactive "*p")
  (save-excursion
    (backward-up-list arg)
    (let ((beg (point)))
      (forward-list)
      (delete-backward-char 1)
      (goto-char beg)
      (delete-char 1))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :after evil
  :config
  ;; evil
  (general-def 'normal
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    ;;utils
    "M-t"  'eshell
    "M-b"  'eww
    "M-w"  'webjump
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-+" 'text-scale-adjust
    ;; buffers
    "M-<tab>" 'switch-to-buffer
    "M-<iso-lefttab>" 'ibuffer
    "M-q" 'kill-this-buffer
    "M-Q" 'delete-window
    "M-C-b" 'switch-to-buffer-other-window
    "M-C-B" 'switch-to-buffer-other-frame
    "M-d" (lambda () (interactive) (kill-matching-buffers "\\*.*\\*" t t))
    ;; window
    "M-\\" 'sam/split-window-right
    "M-|" 'sam/split-window-below
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
    "M-n" 'sam/toggle-single-window
    "M-N" 'sam/toggle-window-split)

  ;; prefix
  (general-create-definer sam/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "M-SPC")
  (sam/leader-keys
    ;;"SPC" '(execute-extended-command :which-key "M-X")
    "."   '(find-file :which-key "find file")
    "RET" '(consult-bookmark :which-key "Open Bookmarks")
    "/"   '(consult-line :which-key "search lines in buffer")
    ";"   '(eval-expression :which-key "evaluate expression")

    ;;Code
    "c"   '(:ignore c :which-key "Code")
    "cc"  '(lsp :which-key "code format")
    "cF"  '(lsp-format-buffer :which-key "code format")
    "ca"  '(lsp-execute-code-action :which-key "code actions")
    "cd"  '(consult-flymake :which-key "consult-flymake")
    "cf"  '(consult-imenu :which-key "consult imenu")
    "cl"  '(consult-outline :which-key "consutl-outline")'
    "cr"  '(lsp-rename :which-key "rename variable")

    ;; Files
    "f"   '(:ignore f :which-key "files")
    "ff"  '(dired-jump :which-key "dired")
    "fg"  '(consult-ripgrep :which-key "ripgrep")
    "fr"  '(consult-recent-file :which-key "recent files")

    ;; search
    "s"   '(:ignore s :which-key "search")
    "sl"  '(consult-line :which-key "search lines")
    "sf"  '(consult-imenu :which-key "imenu")
    "sg"  '(consult-grep :which-key "grep search")
    "si"  '(consult-isearch :which-key "isearch")
    "so"  '(consult-org-heading :which-key "search org heading")

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

(provide 'init-general.el)
