(custom-set-variables
 '(python-indent 2))

(defun python-mode-setup ()

  ;; ------------- ;;
  ;; CONFIGURATION ;;
  ;; ------------- ;;

  ;; Comment boxing style
  (set 'my-rebox-style 423)

  ;; -------- ;;
  ;; BINDINGS ;;
  ;; -------- ;;

  ;; comment
  (define-key
    py-mode-map
    [(control c) (control c)]
    'comment-region)

  ;; rebox
  (define-key
    py-mode-map
    [(meta q)]
    'my-rebox-comment)

  )

(add-hook 'python-mode-hook 'python-mode-setup)

(provide 'my-python-mode)
