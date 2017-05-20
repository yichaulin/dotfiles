
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; install packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  )
(require 'cl)

;; add whatever packages you want here
(defvar max/packages '(company monokai-theme hungry-delete) "Default packages")

(defun max/packages-installed-p()
  (loop for pkg in max/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (max/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg max/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))
               
  
;; turn off tool bar
(tool-bar-mode -1)
;; trun off scroll bar
(scroll-bar-mode -1)
;; trun on line-num
(global-linum-mode t)

(setq inhibit-splash-screen t)

;; press f2 to open init file
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-my-init-file)

;; auto use company(complete-any) mode
(global-company-mode t)

;; don't backup file
(setq make-backup-files nil)

;; open recent file mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; re-write selected word
(delete-selection-mode t)

;; auto full-screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; use Hilight Matching Parenthesess if major mode is Emacs-Lisp
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; hilight current line
(global-hl-line-mode t)

;; load monokai theme
(load-theme 'monokai t)

;; enable hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(package-selected-packages (quote (monokai-them company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
