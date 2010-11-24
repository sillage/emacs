(defun sgml-mode-setup ()

  ;; BINDINGS

  ;; comment
  (define-key
    html-mode-map
    [(control c) (control c)]
    'comment-region)

  )

(add-hook 'sgml-mode-hook 'sgml-mode-setup)
