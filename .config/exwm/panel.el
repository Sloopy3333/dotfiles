(defun my-setup-header-line-format ()
  "Hide header line if required."
  (set-window-parameter (next-window) 'header-line-format
                        (unless (window-at-side-p (next-window) 'top)
                          'none)))

(add-hook 'exwm-update-class-hook #'my-setup-header-line-format)

(set-face-attribute 'header-line nil
                    :background "#000000"
                    :foreground "#cccccc"
                    :height 100)

;; See https://github.com/TatriX/good-line
(defun good-line-format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (when (and (display-graphic-p) (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (concat
     left
     " "
     (propertize  " "
                  'display `((space :align-to (- (+ right right-fringe right-margin) ,(+ reserve 0)))))
     right)))

(setq display-time-default-load-average nil)
  (setq display-time-interval 1)
  (setq display-time-format "%a %d %b %Y %I:%M %p")
  (display-time-mode 1)

(setq header-line-format
              '(:eval (good-line-format
                       ;; Left
                       (when
                           (and (window-at-side-p nil 'top)
                                (window-at-side-p nil 'left))
                         (format " [%s]" (exwm-workspace-current-index)))
                       ;; Right
                       (when (and (window-at-side-p nil 'top)
                                  (window-at-side-p nil 'right))
                         (format "CPU %s | MEM %s |%s| %s"
                                 (let ((cpu (format "%02d%%%%" my-cpu-usage)))
                                   (if (> my-cpu-usage 75)
                                       (propertize cpu 'font-lock-face '(:foreground "#ff2222"))
                                     cpu))
                                 (let* ((used (car my-mem-usage))
                                        (total (cdr my-mem-usage))
                                        (mem (format "%.2f/%.2f"  used total)))
                                   (if (> (/ used total) 0.75)
                                       (propertize mem 'font-lock-face '(:foreground "#ff2222"))
                                     mem))
                                 telega-mode-line-string
                                 display-time-string)))))

;; Install "systat" package via yor distro package manager
(defvar my-cpu-usage 0
  "Current CPU usage percent.")

(defun my-start-cpu-usage-process ()
  "Start `mstat' process updating `my-cpu-usage' variable."
  (when-let ((old-process (get-process "mpstat")))
    (kill-process old-process))
  (make-process :name "mpstat"
                :command '("mpstat" "2")
                :connection-type 'pipe
                :filter (lambda (process output)
                          (let* ((last-column (car (last (split-string output))))
                                 (idle (cl-parse-integer last-column :junk-allowed t)))
                            (when idle
                              (setq my-cpu-usage (- 100 idle)))))))


(my-start-cpu-usage-process)

(defvar my-mem-usage '(0 . 0)
  "Current MEM usage in GB (used . free).")

(defun my-start-mem-usage-process ()
  "Start `free' process updating `my-cpu-usage' variable."
  (when-let ((old-process (get-process "free")))
    (kill-process old-process))
  (make-process :name "free"
                :command '("free" "--mebi" "-s" "2")
                :connection-type 'pipe
                :filter (lambda (process output)
                          (let ((columns (split-string output)))
                            (let ((total (string-to-number (nth 7 columns)))
                                  (used (string-to-number (nth 8 columns)))
                                  (shared (string-to-number (nth 10 columns))))
                              (setq my-mem-usage (cons (/ (+ used shared) 1024.0)
                                                       (/ total 1024.0))))))))

(my-start-mem-usage-process)

(add-hook 'window-configuration-change-hook #'my-setup-header-line-format)


(defun my-setup-header-line-format ()
  "Hide header line if required."
  (interactive)
  (set-window-parameter (next-window) 'header-line-format
                        (unless (window-at-side-p (next-window) 'top)
                          'none)))
(add-hook 'window-configuration-change-hook #'my-setup-header-line-format)
