(defun insert-fixme (&optional msg)
  (interactive "sFixme: ")
  (save-excursion
    (end-of-line)
    (when (not (looking-back "^\\s *"))
      (insert " "))
    (setq start (point))
    (insert "FIXME")
    (when (not (string-equal msg ""))
      (insert ": " msg))
    (comment-region start (point))))


(provide 'my-fixme)
