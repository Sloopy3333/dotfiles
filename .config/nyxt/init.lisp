(in-package :nyxt)
(defvar *my-keymap* (make-keymap "my-map"))
(define-key *my-keymap*
  ;;"b b" 'switch-buffer
  ;;"b n" 'make-buffer
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
;;       '("wiki" "https://en.wikipedia.org/w/index.php?search=~a" "https://en.wikipedia.org/")
;;       '("go" "https://www.google.com/search?q=~a" "https://www.google.com/")
;;       '("define" "https://en.wiktionary.org/w/index.php?search=~a" "https://en.wiktionary.org/")
;;       '("python3" "https://docs.python.org/3/search.html?q=~a" "https://docs.python.org/3")
;;       '("doi" "https://dx.doi.org/~a" "https://dx.doi.org/")))
;;
;;(define-configuration browser
;;  ((search-engines (append (mapcar (lambda (x)
;;				     (make-instance 'search-engine
;;						:shortcut (first x)
;;						:search-url (second x)
;;						:fallback-url (third x)))
;;				   *my-search-engines*)
;;                           %slot-default))))
(define-configuration browser
  ((search-engines (append (list (make-instance 'search-engine
						:shortcut "yt"
						:search-url "https://www.youtube.com/results?search_query=~a"
						:fallback-url "https://www.youtube.com/"))
                           %slot-default))))
