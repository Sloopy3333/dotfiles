(in-package :nyxt)
(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  "M-j" 'switch-buffer-next
  "M-k" 'switch-buffer-previous
  "M-b" 'switch-buffer
  "M-n" 'make-buffer
  "C-u" 'nyxt/web-mode:scroll-page-up
  "C-d" 'nyxt/web-mode:scroll-page-down)

(define-mode my-mode ()
  ((keymap-scheme (keymap:make-scheme
                   scheme:cua *my-keymap*
                   scheme:emacs *my-keymap*
                   scheme:vi-normal *my-keymap*))))

(define-configuration buffer
    ((default-modes (append '(my-mode vi-normal-mode ) %slot-default%))))

(define-configuration (prompt-buffer)
  ((default-modes (append '(vi-insert-mode) %slot-default%))))

;;(defvar *my-search-engines* nil)
;;(setf *my-search-engines*
;;      (list
;;       '("wi" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
;;       '("yt" "https://www.youtube.com/results?search_query=~a" "https://www.youtube.com/")
;;       '("go" "https://www.google.com/search?q=~a" "https://www.google.com/")))
;;
;;(define-configuration browser
;;  ((search-engines (append (mapcar (lambda (x)
;;				     (make-instance 'search-engine
;;						:shortcut (first x)
;;						:search-url (second x)
;;						:fallback-url (third x)))
;;				   *my-search-engines*)
;;                           %slot-default))))
(in-package #:nyxt-user)
