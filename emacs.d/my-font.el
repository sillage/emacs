(defconst default-font-size 99)

(defun set-font-size (&optional size)
  "Set the font size to SIZE (default: default-font-size)."
  (interactive "nSize: ")
  (unless size
    (setq size default-font-size))
  (set-face-attribute 'default nil :height size))

(defun reset-font-size ()
  (interactive)
  (set-font-size))

(defun current-font-size ()
  (face-attribute 'default :height))

(defun alter-font-size (f)
  (let ((curr (current-font-size)))
  (let ((new (funcall f curr)))
    (set-font-size new)
    (message (concat "New font size: " (int-to-string curr) " -> " (int-to-string new))))))

(defun inc-font-size ()
  (interactive)
  (alter-font-size (lambda (x) (ceiling (* x 1.1)))))

(defun dec-font-size ()
  (interactive)
  (alter-font-size (lambda (x) (ceiling (* x 0.9)))))


(global-set-key [(control +)] 'inc-font-size)
(global-set-key [(control -)] 'dec-font-size)
(global-set-key [(control =)] 'reset-font-size)

(provide 'my-font)
