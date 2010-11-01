;; ------ ;;
;; String ;;
;; ------ ;;

(defun remove-prefix-from-string (prefix string)
  (let ((rg (concat "^" prefix)))
    (replace-regexp-in-string rg "" path)))


;; ----- ;;
;; Emacs ;;
;; ----- ;;

(defun buffer-empty-p ()
  (equal (point-min) (point-max)))

(defun current-file-name ()
  (buffer-file-name (current-buffer)))

(defun cwd ()
  (replace-regexp-in-string "Directory " "" (pwd)))

(defun rbegin ()
  (min (point) (mark)))

(defun rend ()
  (max (point) (mark)))


;; ---- ;;
;; Lisp ;;
;; ---- ;;

(defun filter (condp l)
  (if l
      (let ((head (car l))
	    (tail (filter condp (cdr l))))
	(if (funcall condp head)
	    (cons head tail)
	  tail))
    ()))


;; ------- ;;
;; Provide ;;
;; ------- ;;

(provide 'my-elisp)
