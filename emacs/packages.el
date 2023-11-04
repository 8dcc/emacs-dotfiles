
;; TODO:
;; Pop-ups for help, etc.
;; Shells/terms

;;------------------------------------------------------------------------------
;; Evil mode

;; Install evil package (vim keybinds). Also configure split direction.
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo)
  (evil-mode))

;; Install evil-collection package for using vim keybinds in other buffer types
(use-package evil-collection
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

;;------------------------------------------------------------------------------
;; General (Space keybind prefix)

;; Use "SPC" as prefix for normal modes, and "M-SPC" for other modes like
;; evil's insert. Main keybinds are changed from init.el
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer x8dcc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC"))

;;------------------------------------------------------------------------------
;; Which key (keybind completion menu)

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-upercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t))

;;------------------------------------------------------------------------------
;; Projectile

(use-package projectile
  :config
  (projectile-global-mode 1))

;;------------------------------------------------------------------------------
;; Magit

(use-package magit)

;;------------------------------------------------------------------------------
;; Highlight TODOs

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("DELME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

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

