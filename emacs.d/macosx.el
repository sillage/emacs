;;; macosx.el --- Specific settings for Mac OS X

;;; Commentary:
;; Some settings useful in Mac OS X.

;;; Code:

;; Encoding for Terminal.app on OS X
;; Found at http://www.emacswiki.org/emacs/EmacsForMacOS#toc22
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Option key is meta key on OS X
;; Found at http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)      ; cannot use <cmd>+h to hide!!!
(setq mac-option-modifier nil)

(provide 'macosx)

;;; macosx.el ends here
