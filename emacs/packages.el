
;;------------------------------------------------------------------------------
;; Evil mode

;; Install evil package (vim keybinds). Also configure split direction.
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t)
  (evil-mode))

;; Install evil-collection package for using vim keybinds in other buffer types
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;------------------------------------------------------------------------------
;; General (Space keybind prefix)

(use-package general
  :config
  (general-evil-setup t))

;;------------------------------------------------------------------------------
;; Which key (keybind completion menu)

(use-package which-key)
(which-key-mode)

;;------------------------------------------------------------------------------
;; Projectile

(use-package projectile
  :config
  (projectile-global-mode 1))

;;------------------------------------------------------------------------------
;; Dashboard

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "8dcc's Emacs"
        dashboard-startup-banner (concat user-emacs-directory "img/splash.png")
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-footer nil
        dashboard-page-separator "\n\n"
        dashboard-items '((recents . 10)
                          (projects . 5)))
  :config
  (dashboard-setup-startup-hook))

;;------------------------------------------------------------------------------
;; Nasm mode (fork)

(straight-use-package
  '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

