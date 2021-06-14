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

(define-configuration window
  ((message-buffer-style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "white")))))))

(define-configuration prompt-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '((body
               :background-color "black"
               :color "white")
              ("#prompt-area"
               :background-color "black")
              ("#input"
               :background-color "white")
              (".source-name"
               :color "black"
               :background-color "#556B2F")
              (".source-content"
               :background-color "black")
              (".source-content th"
               :border "1px solid #556B2F"
               :background-color "black")
              ("#selection"
               :background-color "#CD5C5C"
               :color "black")
              (.marked :background-color "#8B3A3A"
                       :font-weight "bold"
                       :color "white")
              (.selected :background-color "black"
                         :color "white")))))))

(define-configuration internal-buffer
  ((style
    (str:concat
     %slot-default%
     (cl-css:css
      '((title
         :color "#CD5C5C")
        (body
         :background-color "black"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#556B2F")
        (.button
         :color "lightgray"
         :background-color "#556B2F")))))))

(define-configuration nyxt/history-tree-mode:history-tree-mode
  ((nyxt/history-tree-mode::style
    (str:concat
     %slot-default%
     (cl-css:css
      '((body
         :background-color "black"
         :color "lightgray")
        (hr
         :color "darkgray")
        (a
         :color "#556B2F")
        ("ul li::before"
         :background-color "white")
        ("ul li::after"
         :background-color "white")
        ("ul li:only-child::before"
         :background-color "white")))))))

(define-configuration nyxt/web-mode:web-mode
  ((nyxt/web-mode:highlighted-box-style
    (cl-css:css
     '((".nyxt-hint.nyxt-highlight-hint"
        :background "#CD5C5C")))
    :documentation "The style of highlighted boxes, e.g. link hints.")))

(define-configuration status-buffer
  ((style (str:concat
           %slot-default%
           (cl-css:css
            '(("#container"
               ;; Columns: controls, arrow, url, arrow, tabs, arrow, modes
               :grid-template-columns "10px 10px 1fr 10px 2fr 10px 250px")
              ("#controls"
                :border-top "1px solid white")
              ("#url"
               :background-color "black"
               :color "white"
               :border-top "1px solid white")
              ("#modes"
               :background-color "black"
               :border-top "1px solid white")
              ("#tabs"
               :background-color "#CD5C5C"
               :color "black"
               :border-top "1px solid white")))))))
