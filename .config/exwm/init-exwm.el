;; -*- lexical-binding: t; -*

(defun sam/exwm-set-modeline ()
  (progn
    (setq-default mode-line-format
                  '((:eval (simple-mode-line-render
                            ;; left
                            (format-mode-line
                             (list
                              (if (frame-parameter (selected-frame) 'exwm-active)
                                  '(:eval (format " [%s] " (+ 1 exwm-workspace-current-index))))
                              "%@"
                              '(:eval (propertize "%b " 'face '(:inherit font-lock-type-face :weight bold)))
                              " "
                              "("
                              "%c"
                              ":"
                              "%l"
                              ")"
                              " "
                              "["
                              "" mode-name
                              "" vc-mode
                              "]"
                              ))
                            ;; right
                            (format-mode-line
                             (list
                              '(:eval
                                (propertize
                                 (battery-format "%p (%b %t %r W)"
                                                 (battery-linux-sysfs)) 'face '(:inherit font-lock-string-face :weight bold)))
                              " | "
                              '(:eval
                                (propertize
                                 (format-time-string "%a, %b %d %I:%M %p") 'face '(:inherit font-lock-preprocessor-face :weight bold)))
                              ))))))
    (redraw-modeline)))

;; function to spawn applications
(defun sam/exwm-spawn(name command)
  (interactive)
  (start-process-shell-command name nil command))

(defun sam/exwm-spawn-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun sam/exwm-toggle-minibuffer ()
  "Toggle visibility of echo area at the bottom of the screen"
  (interactive)
  (if (equal (frame-pixel-height) (x-display-pixel-height))
      (set-frame-height nil (+ (x-display-pixel-height) (frame-char-height)) nil t)
    (set-frame-height nil (x-display-pixel-height) nil t)))

;; window and buffer management
(defun sam/exwm-split-window-right ()
  "split window right switch to window and switch to scratch buffer"
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (switch-to-buffer "scratch")))

(defun sam/exwm-split-window-below ()
  "split window below switch to window and switch to scratch buffer"
  (interactive)
  (progn
    (split-window-below)
    (windmove-down)
    (switch-to-buffer "scratch")))

(defun sam/exwm-switch-next-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window +1)
    (next-buffer)))

(defun sam/exwm-switch-prev-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window -1)
    (previous-buffer)))


(defun sam/exwm-toggle-single-window ()
  "delete other windows if more than one window exists
or restore previous layout if a single window exists"
  (interactive)
  (if (= (count-windows) 1)
      (when single-window--last-configuration
	    (set-window-configuration single-window--last-configuration))
    (setq single-window--last-configuration (current-window-configuration))
    (delete-other-windows)))

(defun sam/exwm-toggle-window-split ()
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


;; volume
(defun sam/exwm-volume-ctl (opt)
  (progn
    (start-process-shell-command "volume" nil (concat "amixer set Master " opt))
    ;; :timeout 2000))
    (message "Volume is %s [%s]"
             (string-trim (shell-command-to-string "amixer sget Master | grep -o -m 1 '[[:digit:]]*%' | tr -d '%'"))
             (string-trim (shell-command-to-string "amixer sget Master | grep -o -m 1 '\[[a-z]*\]' |  tr -d '%[]'")))))

;; backlight
(defun sam/exwm-backlight-ctl (opt)
  (start-file-process-shell-command "backlight" nil (concat "xbacklight " opt))
  (sit-for 1)
  (message "Backlight is %-1.1s" (shell-command-to-string "xbacklight -get")))

;; screenshot
(defun sam/exwm-screenshot (&optional rename select)
  "takes screenshot with maim
when rename is non-nil asks for a screenshot name
when seelct is non-nil does a screenshot of selected part"
  (interactive)
  (let ((path
         (concat "~/external/hdd2/screenshots/"
                 (if rename
                     (concat (read-string "Screenshot name: ") ".png")
                   (concat (format-time-string "%y-%m-%d,%H:%M:%S") ".png")))))
    (if select
        (start-process-shell-command "maim" nil (concat "maim -s " path))
      (start-process-shell-command "maim" nil (concat "maim " path)))
    (sit-for 1)
    (message "screenshot saved at %s" path)))

;; Prompts
;; run prompt
(let ((system-executables (seq-mapcat
                           (lambda (bin)
                             (directory-files bin nil "^[^.]"))
                           exec-path)))
  (defun sam/exwm-run-prompt (command)
    "list executables in path listed in `exec-path' and run selected"
    (interactive (list (completing-read "Run: " system-executables)))
    (start-process-shell-command command nil command)))

;; system power prompt
(let ((system-power-options
       '(("Suspend"  . "systemctl suspend")
         ("Poweroff" . "systemctl poweroff")
         ("Reboot"   . "systemctl reboot")
         ("Lock"     . "slock"))
       )        )
  (defun sam/exwm-power-prompt(command)
    "run system power commands in `system-power-options'"
    (interactive (list (completing-read "Execute: "  system-power-options)))
    (when (yes-or-no-p (format "do you realy want to %s the system" command))
      (start-process-shell-command command nil (cdr(assoc command system-power-options))))
    ))

;; package manager
(defun sam/exwm-pacman-install-prompt (command)
  "a prompt to install packages"
  (interactive (list (completing-read "Pacman Install: " (split-string(shell-command-to-string "pacman -Slq")))))
  (start-process-shell-command (concat "pacman install" command) nil (concat " xterm -e sudo pacman -S " command)))

(defun sam/exwm-pacman-uninstall-prompt (command)
  "a prompt to uninstall packages"
  (interactive (list(completing-read "Pacman Uninstall: " (split-string(shell-command-to-string "pacman -Q")))))
  (start-process-shell-command (concat "Pacman uninstall" command) nil (concat "xterm -e sudo pacman -Rns " command)))

(defun sam/exwm-paru-install-prompt (command)
  "a prompt to install packages"
  (interactive (list(completing-read "Paru Install: " (split-string(shell-command-to-string "paru -Slq")))))
  (start-process-shell-command (concat "paru install" command) nil (concat " xterm -e paru -S " command)))

;; window rules
(defun sam/exwm-window-rules ()
  "window rules for exwm"
  (interactive)
  (pcase exwm-class-name
    ("mpv" (exwm-floating-toggle-floating))))

(defun sam/exwm-rename-buffer()
  "rename exwm buffers to more meaningful name classname:title"
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name "-"
           (if (<= (length exwm-title) 60)
               exwm-title
             (concat (substring exwm-title 0 59) "...")))))

(use-package exwm
  :config
  ;; enable system tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; hide minibuffer when not in use
  (setq exwm-workspace-minibuffer-position 'bottom)

  ;; setup modeline for exwm
  (sam/exwm-set-modeline)

  (setq exwm-layout-show-all-buffers nil)
  (setq exwm-workspace-show-all-buffers nil)

  ;; hide modeline for floating windows and X11 windows
  (add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
  (add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)
  (add-hook 'exwm-manage-finish-hook #'exwm-layout-hide-mode-line)
  ;;(add-hook 'exwm-manage-finish-hook #'sam/exwm-hide-modeline-and-minibuffer)

  ;; rename buffers
  (add-hook 'exwm-update-class-hook #'sam/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'sam/exwm-rename-buffer)

  ;; window rules
  (add-hook 'exwm-manage-finish-hook 'sam/exwm-window-rules)


  ;; mouse folow window
  (setq mouse-autoselect-window t)
  (setq focus-follows-mouse t)

  ;; set defualt mode to char mode
  (setq exwm-manage-configurations '((t char-mode t)))

  ;; initial number of workspaces
  (setq exwm-workspace-number 4)

  ;; name workspace from 1 insted of 0
  (setq exwm-workspace-index-map
        (lambda (index) (number-to-string (1+ index))))

  ;; this is to make sure that XF86 keys work in exwm buffers as well
  (dolist (k '(Scroll_Lock
               XF68Back
               XF86AudioLowerVolume
               XF86AudioMute
               XF86AudioMute
               XF86AudioNext
               XF86AudioPlay
               XF86AudioPrev
               XF86AudioRaiseVolume
               XF86AudioStop
               XF86Forward
               XF86MonBrightnessDown
               XF86MonBrightnessUp
               XF86PowerOff
               XF86ScreenSaver
               print))
    (cl-pushnew k exwm-input-prefix-keys))

  ;; switch to ith worspace with s-i and move window to workspace
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create (1-, i)))))
  (exwm-input-set-key (kbd "s-w")  #'exwm-workspace-switch)
  (exwm-input-set-key (kbd "s-W")  #'exwm-workspace-move-window)

  ;; execute commnds
  (exwm-input-set-key (kbd "s-x")  #'execute-extended-command)
  (exwm-input-set-key (kbd "s-;")  #'eval-expression)

  ;; kill restart reset
  (exwm-input-set-key (kbd "s-c") #'exwm-restart)
  (exwm-input-set-key (kbd "s-C") #'kill-emacs)
  (exwm-input-set-key (kbd "s-R") #'exwm-reset)

  ;; edit exwm config
  (exwm-input-set-key (kbd "s-e") #'(lambda () (interactive) (find-file (expand-file-name "~/.config/exwm/init-exwm.el"))))

  ;; change input methods
  (exwm-input-set-key (kbd "s-i") #'exwm-input--next-key)

  ;; delete, burry buffer and window
  (exwm-input-set-key (kbd "s-q") #'kill-this-buffer)
  (exwm-input-set-key (kbd "s-Q") #'delete-window)
  (exwm-input-set-key (kbd "s-C-q") #'bury-buffer)
  (exwm-input-set-key (kbd "s-d") (lambda () (interactive) (kill-matching-buffers "\\*.*\\*" t t)))

  ;; buffer switching
  (exwm-input-set-key (kbd "s-<tab>") #'switch-to-buffer)
  (exwm-input-set-key (kbd "s-<iso-lefttab>") #'ibuffer)

  ;; window split
  (exwm-input-set-key (kbd "s-\\") #'sam/exwm-split-window-right)
  (exwm-input-set-key (kbd "s-|") #'sam/exwm-split-window-below)
  (exwm-input-set-key (kbd "s-=") #'balance-windows)
  (exwm-input-set-key (kbd "s-n") #'sam/exwm-toggle-single-window)
  (exwm-input-set-key (kbd "s-N") #'sam/exwm-toggle-window-split)

  ;; window focus switching
  (exwm-input-set-key (kbd "s-j") #'sam/exwm-switch-next-buffer-or-window)
  (exwm-input-set-key (kbd "s-k") #'sam/exwm-switch-prev-buffer-or-window)
  (exwm-input-set-key (kbd "s-h") #'windmove-left)
  (exwm-input-set-key (kbd "s-l") #'windmove-right)

  ;; window moving
  (exwm-input-set-key (kbd "s-J") #'windmove-swap-states-down)
  (exwm-input-set-key (kbd "s-K") #'windmove-swap-states-up)
  (exwm-input-set-key (kbd "s-L") #'windmove-swap-states-right)
  (exwm-input-set-key (kbd "s-H") #'windmove-swap-states-left)

  ;; window resizing
  (exwm-input-set-key (kbd "s-C-j") #'enlarge-window)
  (exwm-input-set-key (kbd "s-C-k") #'shrink-window)
  (exwm-input-set-key (kbd "s-C-l") #'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-C-h") #'shrink-window-horizontally)

  ;; toggle floating, fullscreen and modeline
  (exwm-input-set-key (kbd "s-C-t") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-C-f") #'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-C-m") #'exwm-layout-toggle-mode-line)
  (exwm-input-set-key (kbd "s-M") #'sam/exwm-toggle-minibuffer)

  ;; winner mode
  (exwm-input-set-key (kbd "s-u") #' winner-undo)
  (exwm-input-set-key (kbd "s-U")  #' winner-redo)

  ;; spawn applications
  (exwm-input-set-key (kbd "s-t") #'eshell)
  (exwm-input-set-key (kbd "s-f") #'dired-jump)
  (exwm-input-set-key (kbd "s-m") #'mu4e)
  (exwm-input-set-key (kbd "s-r") #'elfeed)
  (exwm-input-set-key (kbd "s-F") (lambda() (interactive) (sam/exwm-spawn "pcmanfm" "pcmanfm")))
  (exwm-input-set-key (kbd "s-T") (lambda() (interactive) (sam/exwm-spawn "xterm" "xterm")))
  (exwm-input-set-key (kbd "s-b") (lambda() (interactive) (sam/exwm-spawn "chromium" "chromium --disable-software-rasterizer --profile-directory='Profile 1'")))
  (exwm-input-set-key (kbd "s-B") (lambda() (interactive) (sam/exwm-spawn "chromium" "chromium --disable-software-rasterizer --profile-directory='Profile 2'")))

  ;; application launcher
  (exwm-input-set-key (kbd "s-SPC") #'sam/exwm-run-prompt)

  ;; password prompt
  (exwm-input-set-key (kbd "s-p P") #'pass)
  (exwm-input-set-key (kbd "s-p c") #'password-store-clear)
  (exwm-input-set-key (kbd "s-p e") #'password-store-edit)
  (exwm-input-set-key (kbd "s-p p") #'password-store-copy)
  (exwm-input-set-key (kbd "s-p u") #'password-store-copy-field)

  ;; system prompts
  (exwm-input-set-key (kbd "s-s p") #'sam/exwm-power-prompt)
  (exwm-input-set-key (kbd "s-s m") #'man)
  (exwm-input-set-key (kbd "s-s h") #'proced)

  ;; pacman prompts
  (exwm-input-set-key (kbd "s-P i") #'sam/exwm-pacman-install-prompt)
  (exwm-input-set-key (kbd "s-P r") #'sam/exwm-pacman-uninstall-prompt)
  (exwm-input-set-key (kbd "s-P u") (lambda() (interactive) (start-process-shell-command "pacman update" nil "xterm -e sudo pacman -Syu")))
  (exwm-input-set-key (kbd "s-P y") #'sam/exwm-paru-install-prompt)

  ;; volume control
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (lambda () (interactive) (sam/exwm-volume-ctl "5%+")))
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (lambda () (interactive) (sam/exwm-volume-ctl "5%-")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")        (lambda () (interactive) (sam/exwm-volume-ctl "toggle")))

  ;; backlight control
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   (lambda () (interactive) (sam/exwm-backlight-ctl "-inc +2")))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (sam/exwm-backlight-ctl "-dec +2")))

  ;;screenshot
  (exwm-input--set-key (kbd "<print>") (lambda () (interactive) (sam/exwm-screenshot nil nil)))
  (exwm-input--set-key (kbd "S-<print>") (lambda () (interactive) (sam/exwm-screenshot nil t)))
  (exwm-input--set-key (kbd "s-<print>") (lambda () (interactive) (sam/exwm-screenshot t nil)))
  (exwm-input--set-key (kbd "s-S-<print>") (lambda () (interactive) (sam/exwm-screenshot t t)))

  (exwm-enable))

;;(provide 'init.exwm.el)
