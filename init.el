
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
(defvar max/packages '(company monokai-theme hungry-delete swiper counsel smartparens
			       js2-mode nodejs-repl) "Default packages")

(setq package-selected-packages max/packages)

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

;; config for hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

;; config for swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)

;; config for smartparens
(require 'smartparens-config)
(smartparens-global-mode t)

;; config for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.08)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "knobColor")))))
