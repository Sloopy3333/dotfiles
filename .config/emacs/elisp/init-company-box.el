;; -*- lexical-binding: t; -*-

;; improments to company mode especialy the docs ui
(defun company-box-icons--elisp-fn (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((sym (intern candidate)))
      (cond ((fboundp sym)  'ElispFunction)
            ((boundp sym)   'ElispVariable)
            ((featurep sym) 'ElispFeature)
            ((facep sym)    'ElispFace)))))

(provide 'init-company-box.el)
