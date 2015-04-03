;;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"
;;
;;; Commentary:
;; Based on Quentin Hocquet <mefyl at lrde dot epita dot fr> configuration
;; > Based on Nicolas Despres <despre_n at lrde dot epita dot fr> configuration
;; > Thanks go to Michael Cadilhac <micha at lrde dot epita dot fr> for his help
;;

;;; Code:

(defun may-load (filename)
  "Load file FILENAME only if it exists."
  (when (file-readable-p filename)
    (load-file filename)))

;; Load local distribution configuration file
(may-load "~/.emacs.site")

;; Add location of your configuration files
(add-to-list 'load-path "~/.emacs.d/lisp" 'append)

(require 'macosx)
(require 'my-autoload)
(require 'my-c-mode)
(require 'my-elisp)
(require 'my-font)
(require 'my-layout)
(require 'my-lisp-mode)
(require 'my-python-mode)

(defconst has-gnuserv
  (fboundp 'gnuserv-start)
  "Whether gnuserv is available.")

;; Version detection

(defconst xemacs (string-match "XEmacs" emacs-version)
  "Non-nil if XEmacs, nil otherwise.")

(defconst emacs22 (= 22 emacs-major-version)
  "Non-nil if Emacs 22, nil otherwise.")

(defconst emacs23 (= 23 emacs-major-version)
  "Non-nil if Emacs 23, nil otherwise.")

;; CUSTOM FUNCTIONS

;; Reload conf

(defun reload ()
  "Reload configuration file."
  (interactive)
  (may-load "~/.emacs.d/init.el"))

;; Compilation
(defvar cpu-number 1
  "Number of parallel processing units on this system.")

(setq compile-command "")

;; Edition
(defun c-switch-hh-cc ()
  (interactive)
  (let ((other
         (let ((file (buffer-file-name)))
           (if (string-match "\\.hh$" file)
               (replace-regexp-in-string "\\.hh$" ".cc" file)
             (replace-regexp-in-string "\\.cc$" ".hh" file)))))
    (find-file other)))

(defun count-words (start end)
  "Return number of words between START and END."
  (let
      ((begin (min start end))
       (end (max start end)))
    (save-excursion
      (goto-char begin)
      (re-search-forward "\\W*")        ; skip blank
      (setq i 0)
      (while (< (point) end)
        (re-search-forward "\\w+")
        (when (<= (point) end)
          (setq i (+ 1 i)))
        (re-search-forward "\\W*"))))
  i)

(defun stat-region (start end)
  "Print number of lines, words and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d words, %d characters."
           (count-lines start end) (count-words start end) (- end start)))

(defun ruby-command (cmd &optional output-buffer error-buffer)
  "Like shell-command, but using ruby."
  (interactive (list (read-from-minibuffer "Ruby command: "
                                           nil nil nil 'ruby-command-history)
                     current-prefix-arg
                     shell-command-default-error-buffer))
  (shell-command (concat "ruby -e '" cmd "'") output-buffer error-buffer))

;; C/C++

(defun insert-header-guard ()
  "Insert header guard in C/C++ header file.
Recognized extensions: .h, .hh or .hxx"
  (interactive)
  (if (string-match "\\.h\\(h\\|xx\\)?\\'" (file-name-nondirectory buffer-file-name))
      (let ((header-guard
             (concat
              (upcase (replace-regexp-in-string "[-.]" "_" (file-name-nondirectory buffer-file-name)))
              "_")))
        (save-excursion
          (goto-char (point-min))
          (insert "#ifndef " header-guard "\n")
          (insert "# define " header-guard "\n\n")
          (goto-char (point-max))
          (insert "\n#endif /* !" header-guard " */\n")))
    (message "Invalid C/C++ header file.")))

(defun insert-header-inclusion ()
  "Insert header inclusion in C/C++ source file.
Recognized extensions: .c, .cc or .cpp"
  (interactive)
  (if (string-match "\\.c\\(c\\|pp\\)?\\'"
                    (file-name-nondirectory buffer-file-name))
      (let
          ((header-inclusion (replace-regexp-in-string
                              ".c\\'" ".h"
                              (replace-regexp-in-string
                               ".c\\(c\\|pp\\)\\'" ".hh"
                               (file-name-nondirectory buffer-file-name)))))
        (insert "#include \"" header-inclusion "\"\n\n"))
    (message "Invalid C/C++ source file.")))

;; Auto insert C/C++ header guard in empty C/C++ header file.
;; Recognized extensions: .h, .hh or .hxx
(add-hook 'find-file-hook
          (lambda ()
            (when (and (memq major-mode '(c-mode c++-mode))
                       (equal (point-min) (point-max))
                       (string-match "\\.h\\(h\\|xx\\)?\\'"
                                     (file-name-nondirectory buffer-file-name)))
              (insert-header-guard)
              (goto-line 3)
              (insert "\n"))))

;; Auto insert C/C++ header inclusion in empty C/C++ source file.
;; Recognized extensions: .c, .cc or .cpp
(add-hook 'find-file-hook
          (lambda ()
            (when (and (memq major-mode '(c-mode c++-mode))
                       (equal (point-min) (point-max))
                       (string-match "\\.c\\(c\\|pp\\)?\\'"
                                     (file-name-nondirectory buffer-file-name)))
              (insert-header-inclusion))))


(defun sandbox ()
  "Opens a C++ sandbox in current window."
  (interactive)
  (cd "/tmp")
  (let ((file (make-temp-file "/tmp/" nil ".cc")))
    (find-file file)
    (insert "int main()\n{\n\n}\n")
    (line-move -2)
    (save-buffer)
    (compile (concat "g++ -W -Wall -I /usr/include/qt4/ -I /usr/include/qt4/QtCore/ -L /usr/lib/qt4 -lQtCore " file " && ./a.out"))))

(defun c-insert-debug (&optional msg)
  (interactive)
  (when (not (looking-at "\\W*$"))
    (beginning-of-line)
    (insert "\n")
    (line-move -1))
  (c-indent-line)
  (insert "std::cerr << \"\" << std::endl;")
  (backward-char 15))

(defun c-insert-block (&optional r b a)
  (interactive "P")
  (unless b (setq b ""))
  (unless a (setq a ""))
  (if r
      (progn
        (save-excursion
          (goto-char (rbegin))
          (beginning-of-line)
          (insert "\n")
          (line-move -1)
          (insert b "{")
          (c-indent-line))
        (save-excursion
          (goto-char (- (rend) 1))
          (end-of-line)
          (insert "\n}" a)
          (c-indent-line)
          (line-move -1)
          (end-of-line))
        (indent-region (rbegin) (rend)))
    (progn
      (beginning-of-line)
      (setq begin (point))
      (insert b "{\n")
      (end-of-line)
      (insert "\n}" a)
      (indent-region begin (point))
      (line-move -1)
      (end-of-line))))

(defun c-insert-braces (&optional r)
  (interactive "P")
  (c-insert-block r))

(defun c-insert-ns (name r)
  (interactive "sName: \nP")
  (c-insert-block r (concat "namespace " name "\n")))

(defun c-insert-switch (value r)
  (interactive "sValue: \nP")
  (c-insert-block r (concat "switch (" value ")\n")))

(defun c-insert-if (c r)
  (interactive "sCondition: \nP")
  (c-insert-block r (concat "if (" c ")\n")))

(defun c-insert-class (name)
  (interactive "sName: ")
  (c-insert-block () (concat "class " name "\n") ";")
  (insert "public:")
  (c-indent-line)
  (insert "\n")
  (c-indent-line))


;;; OPTIONS
(setq inhibit-startup-screen t)     ; don't show the GNU splash screen
(setq frame-title-format "%b")      ; titlebar shows buffer's name
(global-font-lock-mode 1)           ; syntax highlighting
(setq font-lock-maximum-decoration t)   ; max decoration for all modes
;; (transient-mark-mode 1)                 ; highlight selection
(size-indication-mode 1)                ; buffer's size
(line-number-mode 1)                    ; line number
(column-number-mode 1)                  ; column number
(when (display-graphic-p)
  (scroll-bar-mode -1)                  ; no scroll bar
  (menu-bar-mode 1)                     ; menu bar
  (tool-bar-mode -1)                    ; no tool bar
  (mouse-wheel-mode 1))                 ; enable mouse wheel
(setq scroll-step 1)                    ; smooth scrolling

(setq delete-auto-save-files t)    ; delete unnecessary autosave files
(setq delete-old-versions t)       ; delete oldversion file
(setq make-backup-files nil)       ; no backupfile

(if (display-graphic-p)
    (normal-erase-is-backspace-mode 1)) ; make delete work as it should

(defalias 'yes-or-no-p 'y-or-n-p)    ; 'y or n' instead of 'yes or no'
(setq-default major-mode 'text-mode) ; change default major mode to text
(setq ring-bell-function 'ignore)    ; turn the alarm totally off


;; FIXME: wanted 99.9% of the time, but can cause your death 0.1% of
;; the time =). TODO: save buffer before reverting
;;(global-auto-revert-mode t)         ; auto revert modified files

;; (pc-selection-mode)                     ; selection with shift
(auto-image-file-mode)                  ; to see picture in emacs
;; (dynamic-completion-mode)               ; dynamic completion
(show-paren-mode 1)                 ; match parenthesis
(setq-default indent-tabs-mode nil) ; nil == don't use fucking tabs to indent

;;; HOOKS

;; Delete trailing whitespaces on save
;; Warning: do not use it with Gnus, signature fail.
;;(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; (defun eightycols nil
;;   "Highlight when > 80 cols.
;; Useless with emacs23+ (whitespace-mode)"
;;   (defface line-overflow
;;     '((t (:background "red" :foreground "black")))
;;     "Face to use for `hl-line-face'.")
;;   (highlight-regexp "^.\\{80,\\}$" 'line-overflow))
;; (add-hook 'find-file-hook 'eightycols)

(custom-set-faces                       ; red comments
 '(font-lock-comment-face ((t (:foreground "dark red")))))

;; Shebangs
(defun insert-shebang (bin)
  (interactive "sBin: ")
  (save-excursion
    (goto-char (point-min))
    (insert "#! " bin "\n\n")))
(defun insert-shebang-if-empty (bin)
  (when (buffer-empty-p)
    (insert-shebang bin)))
;; sh
(add-hook 'sh-mode-hook
          (lambda () (insert-shebang-if-empty "/bin/sh")))
;; Ruby
(add-hook 'ruby-mode-hook
          (lambda () (insert-shebang-if-empty "/usr/bin/ruby")))
;; Python
(add-hook 'python-mode-hook
          (lambda () (insert-shebang-if-empty "/usr/bin/python")))
;; Perl
(add-hook 'perl-mode-hook
          (lambda () (insert-shebang-if-empty "/usr/bin/perl")))


;; Start code folding mode in C/C++ mode
(add-hook 'c-mode-common-hook (lambda () (hs-minor-mode 1)))


;; File extensions
(add-to-list 'auto-mode-alist '("\\.l\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ll\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.yy\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xcc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.xhh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . sh-mode)) ; Qt .pro files
(add-to-list 'auto-mode-alist '("configure\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("Drakefile\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . change-log-mode))


;;; ido --- interactively do things
(defconst has-ido (>= emacs-major-version 22))

(when (ido-mode 1)
  (ido-everywhere 1)
  ;; tab means tab, i.e. complete. Not "open this file", stupid.
  (setq ido-confirm-unique-completion t)
  ;; If the file doesn't exist, do not try to invent one from a
  ;; transplanar directory. I just want a new file.
  (setq ido-auto-merge-work-directories-length -1)
  ;; If buffer name doesn't exist, create one.
  (setq ido-create-new-buffer 'always)
  ;; Don't switch to GDB-mode buffers
  (add-to-list 'ido-ignore-buffers "\\`\\*locals of.*\\*\\'")
  (add-to-list 'ido-ignore-buffers "\\`\\*gud\\*\\'")
  (add-to-list 'ido-ignore-buffers "\\`\\*stack frames of.*\\*\\'")
  (add-to-list 'ido-ignore-buffers "\\`\\*breakpoints of.*\\*\\'")
  (add-to-list 'ido-ignore-buffers "locals"))


;; GNUSERV
(when has-gnuserv
  (gnuserv-start)
  ;; (global-set-key [(control x) (control c)] 'gnuserv-close-session)
  (add-hook 'gnuserv-visit-hook 'configure-frame))

;; GNUS
(setq gnus-select-method '(nntp "news.epita.fr")) ; news server
(setq user-full-name "my-name")                   ; set my name
(setq user-nickname "my-nickname")                ; set my nickname
(setq user-mail-address "email@address") ; set my email address


;;; BINDINGS

;; BINDINGS :: windows
(global-unset-key [(control s)])
(global-set-key [(control s) (v)] 'split-window-horizontally)
(global-set-key [(control s) (h)] 'split-window-vertically)
(global-set-key [(control s) (d)] 'delete-window)
(global-set-key [(control s) (o)] 'delete-other-windows)

;; BINDINGS :: ido
(when (featurep 'ido)
  (global-set-key [(control b)] 'ido-switch-buffer))

;; BINDINGS :: isearch
(global-set-key [(control f)] 'isearch-forward-regexp) ; search regexp
(global-set-key [(control r)] 'query-replace-regexp) ; replace regexp
(define-key isearch-mode-map
  [(control n)] 'isearch-repeat-forward) ; next occurence
(define-key isearch-mode-map
  [(control p)] 'isearch-repeat-backward) ; previous occurence
(define-key isearch-mode-map
  [(control z)] 'isearch-cancel)     ; quit and go back to start point
(define-key isearch-mode-map
  [(control f)] 'isearch-exit)          ; abort
(define-key isearch-mode-map
  [(control r)] 'isearch-query-replace) ; switch to replace mode
(define-key isearch-mode-map
  [S-insert] 'isearch-yank-kill)        ; paste
(define-key isearch-mode-map
  [(control e)] 'isearch-toggle-regexp) ; toggle regexp
(define-key isearch-mode-map
  [(control l)] 'isearch-yank-line)     ; yank line from buffer
(define-key isearch-mode-map
  [(control w)] 'isearch-yank-word)     ; yank word from buffer
(define-key isearch-mode-map
  [(control c)] 'isearch-yank-char)     ; yank char from buffer

;; BINDINGS :: Lisp
(define-key lisp-mode-map
  [(control c) (control f)] 'insert-fixme) ; insert fixme

;; ;; BINDINGS :: Ruby
;; (define-key ruby-mode-map
;;   [(control c) (control f)] 'insert-fixme) ; insert fixme

;; BINDINGS :: C/C++
(require 'cc-mode)

(define-key c-mode-base-map
  [(control c) (w)]
  'c-switch-hh-cc)                      ; switch between .hh and .cc
(define-key c-mode-base-map
  [(control c) (f)]
  'hs-hide-block)                       ; fold code
(define-key c-mode-base-map
  [(control c) (s)]
  'hs-show-block)                       ; unfold code
(define-key c-mode-base-map
  [(control c) (control n)]
  'c-insert-ns)                         ; insert namespace
(define-key c-mode-base-map
  [(control c) (control s)]
  'c-insert-switch)                     ; insert switch
(define-key c-mode-base-map
  [(control c) (control i)]
  'c-insert-if)                         ; insert if
(define-key c-mode-base-map
  [(control c) (control b)]
  'c-insert-braces)                     ; insert braces
(define-key c-mode-base-map
  [(control c) (control f)]
  'insert-fixme)                        ; insert fixme
(define-key c-mode-base-map
  [(control c) (control d)]
  'c-insert-debug)                      ; insert C++ debug
(define-key c-mode-base-map
  [(control c) (control l)]
  'c-insert-class)                      ; insert class

;; ;; BINDINGS :: C/C++ :: XRefactory
;; (define-key
;;   c-mode-base-map
;;   [(control c) (d)]
;;   'xref-push-and-goto-definition)                       ; goto definition
;; (define-key
;;   c-mode-base-map
;;   [(control c) (b)]
;;   'xref-pop-and-return)                                 ; go back
;; (define-key
;;   c-mode-base-map
;;   [C-return]
;;   'xref-completion)                                     ; complete

;; BINDINGS :: misc
(global-set-key [(meta =)] 'stat-region)

(if (display-graphic-p)
    (global-set-key [(control z)] 'undo)) ; undo only in graphic mode
;; (global-set-key [(control a)] 'mark-whole-buffer) ; select whole buffer
(global-set-key [(control return)] 'hippie-expand) ; auto completion
(global-set-key [C-home] 'beginning-of-buffer) ; go to the beginning of buffer
(global-set-key [C-end] 'end-of-buffer)    ; go to the end of buffer
(global-set-key [(meta g)] 'goto-line)     ; goto line #
(global-set-key [M-left] 'windmove-left)   ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up] 'windmove-up)       ; move to upper window
(global-set-key [M-down] 'windmove-down)   ; move to lower window
(global-set-key [(control c) (c)] 'recompile)
(global-set-key [(control c) (e)] 'next-error)
(global-set-key [(control tab)] 'other-window) ; Ctrl-Tab = Next buffer
(global-set-key [C-S-iso-lefttab]
 '(lambda () (interactive) (other-window -1))) ; Ctrl-Shift-Tab = Previous buffer
(global-set-key [(control delete)] 'kill-word) ; kill word forward
(global-set-key [(meta ~)] 'ruby-command)      ; run ruby command


;;; COLORS
(defun configure-frame ()
  (set-background-color "black")
  (set-foreground-color "white")
  (set-cursor-color "Orangered")
  (set-font-size))
(configure-frame)

;; Qt
;; (font-lock-add-keywords 'c++-mode
;;   '(("foreach\\|rforeach\\|forever\\|emit" . 'font-lock-keyword-face)))

;; Lisp mode
(require 'lisp-mode)
(define-key
  lisp-mode-shared-map
  [(control c) (control c)]
  'comment-region)                      ; lisp comment


;;; C / C++ mode
(require 'cc-mode)

;; `EPITA' Coding Style.
(defconst epita-style
  '((c-basic-offset . 2)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((substatement-open before after)))
    (c-offsets-alist . ((topmost-intro        . 0)
                        (substatement         . +)
                        (substatement-open    . 0)
                        (case-label           . +)
                        (access-label         . -)
                        (inclass              . ++)
                        (inline-open          . 0))))
  "EPITA Coding Style.")
(c-add-style "epita" epita-style)
;; Use `EPITA' Coding Style by default.
(setq c-default-style "epita")
;; (add-to-list 'c-default-style '(c-mode . "epita"))
;; (add-to-list 'c-default-style '(c++-mode . "epita"))
;; (add-to-list 'c-default-style '(java-mode . "epita"))
;; (add-to-list 'c-default-style '(awk-mode . "epita"))
(setq js-indent-level 2)                ; JavaScript indentation

;; ;; Indent and format C program source using GNU `indent' program.
;; (defun c-reformat-buffer()
;;   (interactive)
;;   (save-buffer)
;;   (setq sh-indent-command (concat
;;                            "indent -st -bad --blank-lines-after-procedures "
;;                            "-bli0 -i4 -l79 -ncs -npcs -nut -npsl -fca "
;;                            "-lc79 -fc1 -cli4 -bap -sob -ci4 -nlp "
;;                            buffer-file-name))
;;   (mark-whole-buffer)
;;   (universal-argument)
;;   (shell-command-on-region
;;    (point-min)
;;    (point-max)
;;    sh-indent-command
;;    (buffer-name))
;;   (save-buffer))
;; (define-key c-mode-base-map [f7] 'c-reformat-buffer)

;; Compilation
(setq compilation-window-height 14)
(setq compilation-scroll-output t)

;; make C-Q RET insert a \n, not a ^M
;; (defadvice insert-and-inherit (before ENCULAY activate)
;;   (when (eq (car args) ?)
;;     (setcar args ?\n)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(gdb-max-frames 1024)
 '(python-indent 2)                     ; indentation python
 '(require-final-newline t) ; Whether to add a newline automatically at the end of the file.
 '(speedbar-frame-parameters (quote ((minibuffer . t) (width . 20) (border-width . 0) (menu-bar-lines . 0) (tool-bar-lines . 0) (unsplittable . t) (left-fringe . 0)))))

(require 'uniquify)
;; rename buffers when having same name
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

(setq-default ispell-program-name "aspell")

;; GDB
(setq-default gdb-many-windows t)


;; Recognize test suite output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^\\(PASS\\|SKIP\\|XFAIL\\|TFAIL\\): \\(.*\\)$" 2 () () 0 2))
(add-to-list 'compilation-error-regexp-alist
             '("^\\(FAIL\\|XPASS\\): \\(.*\\)$" 2 () () 2 2))

;; (require 'flymake)
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; ;; Xrefactory configuration part ;;
;; ;; some Xrefactory defaults can be set here
;; (defvar xref-current-project nil)      ; can be also "my_project_name"
;; (defvar xref-key-binding 'global)      ; can be also 'local or 'none
;; (setq load-path (cons "/tmp/xref/emacs" load-path))
;; (setq exec-path (cons "/tmp/xref" exec-path))
;; (load "xrefactory")
;; ;; end of Xrefactory configuration part ;;
;; (message "xrefactory loaded")


;; Save and restore window layout
(defvar winconf-ring ())

(defun push-winconf ()
  (interactive)
  (window-configuration-to-register ?%)
  (push (get-register ?%) winconf-ring))

(defun pop-winconf ()
  (interactive)
  (set-register ?% (pop winconf-ring))
  (jump-to-register ?%))

(defun restore-winconf ()
  (interactive)
  (set-register ?% (car winconf-ring))
  (jump-to-register ?%))

(may-load "~/.emacs.local")


;; Tab width
(setq default-tab-width 8)


;; UTF-8
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Tiger Compiler
(autoload 'tiger-mode "tiger" "Load tiger-mode" t)
(add-to-list 'auto-mode-alist '("\\.ti[gh]$" . tiger-mode))


;; Tuareg-mode for Caml code
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi"))
  (add-to-list 'completion-ignored-extensions ext))

;; cmake-mode
;; (setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

;; Height and Width of frame
(when (display-graphic-p)
  (set-frame-height (selected-frame) 42) ; because 42.
  (set-frame-width (selected-frame) 80))

;;; auto-complete
;(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/ac-dict")
;(ac-config-default)

;;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet/snippets")

;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (append '(("\\.text\\'" . markdown-mode)
                ("\\.mdwn\\'" . markdown-mode)
                ("\\.md\\'" . markdown-mode)
                ("\\.mdt\\'" . markdown-mode)
                ("\\.mdown\\'" . markdown-mode)
                ("\\.mkdn\\'" . markdown-mode)
                ("\\.mkd\\'" . markdown-mode)
                ("\\.markdown\\'" . markdown-mode))
              auto-mode-alist))

;; VisualBasicMode
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" .
                                 visual-basic-mode)) auto-mode-alist))

;;; init.el ends here
