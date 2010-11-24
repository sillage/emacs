(defun window-set-width (width)
  (interactive)
  (enlarge-window-horizontally (- width (window-width))))

(provide 'my-layout)
