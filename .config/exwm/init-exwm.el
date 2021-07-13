;; -*- lexical-binding: t; -*-

;; window and buffer management
(defun sam/exwm-split-window-right ()
  "split window right switch to window and switch to scratch buffer"
  (interactive)
  (progn
    (split-window-right)
    (windmove-right)
    (switch-to-buffer "scratch")))
   ;; (let (($buf (generate-new-buffer "untitled")))
   ;;   (switch-to-buffer $buf))))

(defun sam/exwm-split-window-below ()
  "split window below switch to window and switch to scratch buffer"
  (interactive)
  (progn
    (split-window-below)
    (windmove-down)
    (switch-to-buffer "scratch")))
    ;;(let (($buf (generate-new-buffer "untitled")))
    ;;  (switch-to-buffer $buf))))

(defun sam/exwm-switch-to-next-buffer ()
  "similar to `switch-to-next-buffer' but ignores special buffers"
  (interactive)
  (next-buffer)
  (while (string-match-p "^*" (buffer-name))
    (next-buffer)))

(defun sam/exwm-switch-to-prev-buffer ()
  "similar to `switch-to-prev-buffer' but ignores special buffers"
  (interactive)
  (previous-buffer)
  (while (string-match-p "^*" (buffer-name))
    (previous-buffer)))

(defun sam/exwm-switch-next-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window 1)
    (sam/switch-to-next-buffer)))

(defun sam/exwm-switch-prev-buffer-or-window ()
  "move focus to next window if exist or cycle through the buffer list"
  (interactive)
  (if(> (count-windows) 1)
      (other-window -1)
    (sam/switch-to-prev-buffer)))

;; screenshot
(defun sam/exwm-screenshot-full ()
  (interactive)
  (let ((path (concat "~/external/hdd2/screenshots/" (format-time-string "%y-%m-%d,%H:%M:%S") ".png")))
  (start-process-shell-command nil "maim" (concat "maim " path))
  (message (concat "screenshot saved at " path))))

(defun sam/exwm-screenshot-select ()
  (interactive)
  (let ((path (concat "~/external/hdd2/screenshots/" (format-time-string "%y-%m-%d,%H:%M:%S") ".png")))
  (start-process-shell-command "maim" "maim" (concat "maim -s " path))
  (message (concat "screenshot saved at " path))))

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
       ))
  (defun sam/exwm-power-prompt(command)
    "run system power commands in `system-power-options'"
    (interactive (list (completing-read "Execute: "  system-power-options)))
      (when (yes-or-no-p (format "do you realy want to %s the system" command))
        (start-process-shell-command command nil (cdr(assoc command system-power-options))))
    ))

;; package manager
(defun sam/exwm-pacman-install-prompt (command)
  "a prompt to install packages"
  (interactive (list(completing-read "Pacman Install: " (split-string(shell-command-to-string "pacman -Slq")))))
  (start-process-shell-command (concat "pacman install" command) nil (concat " xterm -e sudo pacman -S " command)))

(defun sam/exwm-pacman-uninstall-prompt (command)
  "a prompt to uninstall packages"
  (interactive (list(completing-read Pacman "Uninstall: " (split-string(shell-command-to-string "pacman -Q")))))
  (start-process-shell-command (concat "pacman uninstall" command) nil (concat "xterm -e sudo pacman -Rns " command)))

(defun sam/exwm-paru-install-prompt (command)
  "a prompt to install packages"
  (interactive (list(completing-read "Paru Install: " (split-string(shell-command-to-string "paru -Slq")))))
  (start-process-shell-command (concat "paru install" command) nil (concat " xterm -e yay -S " command)))

;; window rules
(defun sam/exwm-window-rules ()
  "window rules for exwm"
  (interactive)
  (pcase exwm-class-name
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))
    ))

(defun sam/exwm-rename-buffer()
  "rename exwm buffers to more meaningful name classname:title"
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50)
               exwm-title
             (concat (substring exwm-title 0 49) "...")))))

(use-package exwm
  :config
  ;; enable system tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; rename buffers
  (add-hook 'exwm-update-class-hook #'sam/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'sam/exwm-rename-buffer)

  ;; window rules
  (add-hook 'exwm-manage-finish-hook 'sam/exwm-window-rules)

  ;; hide the minibuffer and echo area when they're not used, by
  ;;(setq exwm-workspace-minibuffer-position 'top)

  (setq exwm-workspace-show-all-buffers t)

  ;; initial number of workspaces
  (setq exwm-workspace-number 2)

  ;; let switch buffers in other buffers
  (setq exwm-layout-show-all-buffers t)

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

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\M-&
          ?\M-:
          ?\M-\
          ?\M-`
          ?\M-x))

  ;; any key followed by C-q will be sent to the focused window rather than exwm
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)


  ;; switch to ith worspace with s-i
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create (1- ,i)))))

  (exwm-input-set-key (kbd "s-!")  #'exwm-workspace-move-window)
  ;; kill restart reset
  (exwm-input-set-key (kbd "s-c") #'exwm-restart)
  (exwm-input-set-key (kbd "s-C") #'kill-emacs)
  (exwm-input-set-key (kbd "s-R") #'exwm-reset)

  ;; buffer switching
  (exwm-input-set-key (kbd "s-<iso-lefttab>") #'sam/exwm-switch-to-prev-buffer)
  (exwm-input-set-key (kbd "s-<tab>") #'sam/exwm-switch-to-next-buffer)
  (exwm-input-set-key (kbd "s-a") #'switch-to-buffer)
  (exwm-input-set-key (kbd "s-A") #'ibuffer)

  ;; delete buffer
  (exwm-input-set-key (kbd "s-q") #'kill-buffer-and-window)
  (exwm-input-set-key (kbd "s-C-q") #'delete-window)
  (exwm-input-set-key (kbd "s-Q") #'bury-buffer)

  ;; window split
  (exwm-input-set-key (kbd "s-<return>") #'sam/exwm-split-window-right)
  (exwm-input-set-key (kbd "s-S-<return>") #'sam/exwm-split-window-below)

  ;; window focus
  (exwm-input-set-key (kbd "s-j") #'sam/exwm-switch-next-buffer-or-window)
  (exwm-input-set-key (kbd "s-k") #'sam/exwm-switch-prev-buffer-or-window)
  (exwm-input-set-key (kbd "s-h") #'windmove-left)
  (exwm-input-set-key (kbd "s-l") #'windmove-right)

  ;; window movement
  (exwm-input-set-key (kbd "s-J") #'windmove-swap-states-down)
  (exwm-input-set-key (kbd "s-K") #'windmove-swap-states-up)
  (exwm-input-set-key (kbd "s-L") #'windmove-swap-states-right)
  (exwm-input-set-key (kbd "s-H") #'windmove-swap-states-left)

  ;; window resizing
  (exwm-input-set-key (kbd "s-C-j") #'enlarge-window)
  (exwm-input-set-key (kbd "s-C-k") #'shrink-window)
  (exwm-input-set-key (kbd "s-C-l") #'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-C-h") #'shrink-window-horizontally)

  ;; floating and fullscreen
  (exwm-input-set-key (kbd "s-C-t") #'exwm-floating-toggle-floating)
  (exwm-input-set-key (kbd "s-C-f") #'exwm-layout-toggle-fullscreen)

  ;; spawn applications
  (exwm-input-set-key (kbd "s-B") (lambda() (interactive) (start-process-shell-command "librewolf" nil "librewolf -p 'Logins'")))
  (exwm-input-set-key (kbd "s-F") (lambda() (interactive) (start-process-shell-command "pcmanfm" nil "pcmanfm")))
  (exwm-input-set-key (kbd "s-T") (lambda() (interactive) (start-process-shell-command "xterm" nil "xterm")))
  (exwm-input-set-key (kbd "s-b") (lambda() (interactive) (start-process-shell-command "librewolf" nil "librewolf -p 'Regular'")))
  (exwm-input-set-key (kbd "s-f") #'dired-jump)
  (exwm-input-set-key (kbd "s-m") #'mu4e)
  (exwm-input-set-key (kbd "s-r") #'elfeed)
  (exwm-input-set-key (kbd "s-t") #'eshell)

  ;; prompts
  ;; application launcher
  (exwm-input-set-key (kbd "s-SPC") #'sam/exwm-run-prompt)
  ;; passwords
  (exwm-input-set-key (kbd "s-p P") #'pass)
  (exwm-input-set-key (kbd "s-p c") #'password-store-clear)
  (exwm-input-set-key (kbd "s-p e") #'password-store-edit)
  (exwm-input-set-key (kbd "s-p p") #'password-store-copy)
  (exwm-input-set-key (kbd "s-p u") #'password-store-copy-field)
  ;; system
  (exwm-input-set-key (kbd "s-s p") #'sam/exwm-power-prompt)
  ;; pacman
  (exwm-input-set-key (kbd "s-P i") #'sam/exwm-pacman-install-prompt)
  (exwm-input-set-key (kbd "s-P r") #'sam/exwm-pacman-uninstall-prompt)
  (exwm-input-set-key (kbd "s-P u") (lambda() (interactive) (start-process-shell-command "pacman update" nil "xterm -e sudo pacman -Syu")))
  (exwm-input-set-key (kbd "s-P y") #'sam/exwm-paru-install-prompt)
  ;; help
  (exwm-input-set-key (kbd "s-d m") #'man)
  (exwm-input-set-key (kbd "s-d f") #'helpful-function)
  (exwm-input-set-key (kbd "s-d v") #'helpful-variable)
  (exwm-input-set-key (kbd "s-d s") #'helpful-symbol)
  (exwm-input-set-key (kbd "s-d k") #'helpful-key)
  (exwm-input-set-key (kbd "s-d a") #'consult-apropos)
  (exwm-input-set-key (kbd "s-d c") #'helpful-command)
  (exwm-input-set-key (kbd "s-d h") #'helpful-at-point)

  ;; volume control
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (lambda () (interactive) (start-process "volume" nil "pamixer" "-i" "5")))
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (lambda () (interactive) (start-process "volume" nil "pamixer" "-d" "5")))
  (exwm-input-set-key (kbd "<XF86AudioMute>")        (lambda () (interactive) (start-process "volume" nil "pamixer" "-t")))

  ;; backlight control
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>")   (lambda () (interactive) (start-process "backlight" nil "xbacklight" "-inc" "+2")))
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") (lambda () (interactive) (start-process "backlight" nil "xbacklight" "-inc" "-2")))

  ;;screenshot
  (exwm-input--set-key (kbd "s-<print>") #'sam/exwm-screenshot-full)
  (exwm-input--set-key (kbd "s-S-<print>") #'sam/exwm-screenshot-select)

  (exwm-enable))


(provide 'init.exwm.el)
