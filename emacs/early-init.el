;; -----------------------------------------------------------------------------
;; Early init file for 8dcc's Emacs.
;; See: https://github.com/8dcc/emacs-dotfiles
;; -----------------------------------------------------------------------------

;; Increase the garbage collection threshold to improve startup times.
(setq gc-cons-threshold (* 100 1024 1024))

;; Disable package.el (for straight.el)
(setq package-enable-at-startup nil)
