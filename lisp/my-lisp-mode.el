(defun my-lisp-mode-setup ()

  (require 'lisp-mode)


  ;; ------------- ;;
  ;; Configuration ;;
  ;; ------------- ;;

  ;; Rebox style
  (set 'my-rebox-style 523)

  ;; -------- ;;
  ;; Bindings ;;
  ;; -------- ;;

  ;; Rebox comment
  (define-key
    lisp-mode-shared-map
    [(meta q)]
    'my-rebox-comment)


  )

(add-hook 'lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-setup)

(provide 'my-lisp-mode)
