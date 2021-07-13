;; -*- lexical-binding: t; -*-

;; Company box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  ;; display even if only ones candidate issue there
  (setq company-box-show-single-candidate nil)
  ;; disable backend base color
  (setq company-box-backends-colors nil)
  ;; maximum number of candidates
  (setq company-box-max-candidates 20)
  ;; icons rendering method
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  ;; auto insert selected candidate
  (company-tng-mode 1)
  ;; add elisp support
  (setq company-box-icons-functions
        (cons #'company-box-icons--elisp-fn
              (delq 'company-box-icons--elisp
                    company-box-icons-functions)))
  ;; disable documentation in echo area
  (delq 'company-echo-metadata-frontend company-frontends)

  (defun company-box-icons--elisp-fn (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym)  'ElispFunction)
              ((boundp sym)   'ElispVariable)
              ((featurep sym) 'ElispFeature)
              ((facep sym)    'ElispFace))))))


(provide 'init-company-box.el)
