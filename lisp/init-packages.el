;; install packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  )
(require 'cl)

;; add whatever packages you want here
(defvar max/packages '(company monokai-theme hungry-delete swiper counsel smartparens
			       js2-mode nodejs-repl popwin) "Default packages")

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

;; config for smartparens
(smartparens-global-mode t)

;; config for hungry-delete
(require 'hungry-delete)
(global-hungry-delete-mode)

;; config for swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; config for js files
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

;; auto use company(complete-any) mode
(global-company-mode t)

;; load monokai theme
(load-theme 'monokai t)

;; config for popwin
(require 'popwin)
(popwin-mode t)

(provide 'init-packages)
