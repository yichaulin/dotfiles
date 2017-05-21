;; turn off tool barba
(tool-bar-mode -1)
;; trun off scroll bar
(scroll-bar-mode -1)
;; trun off default initail page
(setq inhibit-splash-screen t)
;; change cursor type
(setq-default cursor-type 'bar)
;; auto full-screen
(setq initial-frame-alist (quote ((fullscreen . maximized))))
;; hilight current line
(global-hl-line-mode t)




(provide 'init-ui)
