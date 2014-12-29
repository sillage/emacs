(require 'rebox)

(defvar my-rebox-style 523)

(make-variable-buffer-local 'my-rebox-style)

(defun my-rebox-comment (style)
  (interactive "P")
  (if style
      (let ((rebox-default-style my-rebox-style)) (rebox-comment nil))
    (rebox-comment nil)))

(provide 'my-rebox)
