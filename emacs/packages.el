
;;------------------------------------------------------------------------------
;; Evil mode

;; Install evil package (vim keybinds). Also configure split direction.
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t)
  (evil-mode))

;; Install evil-collection package for using vim keybinds in other buffer types
(use-package evil-collection
  :after evil
  :ensure t
  :config
    (evil-collection-init))

;;------------------------------------------------------------------------------
;; General (Space keybind prefix)

(use-package general
  :ensure t
  :config
    (general-evil-setup t))

;;------------------------------------------------------------------------------
;; Which key (keybind completion menu)

(use-package which-key
  :ensure t)
(which-key-mode)


