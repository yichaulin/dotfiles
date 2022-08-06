;; trun off warning ring
(setq ring-bell-function 'ignore)
;; auto reload file
(global-auto-revert-mode t)
;; trun on line-num
(global-linum-mode t)
;; turn on custom abbrev
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(("mx" "max da da")))
;; don't backup file
(setq make-backup-files nil)
;; turn off auto save
(setq auto-save-default nil)
;; open recent file mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;; use Hilight Matching Parenthesess if major mode is Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
;; re-write selected word
(delete-selection-mode t)


(provide 'init-better-defaults)

