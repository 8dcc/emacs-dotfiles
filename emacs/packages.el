
;; TODO:
;; M-x completion (menu)

;;------------------------------------------------------------------------------
;; Diminish (hide minor modes from the modeline)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'visual-line-mode))

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
  :diminish evil-collection-unimpaired-mode
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
  :diminish
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
  :diminish
  :config
  (projectile-global-mode 1))

;;------------------------------------------------------------------------------
;; Magit

(use-package magit
  :config
  ;; Fullscreen status window
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

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
;; Popups

(use-package popper
  :config
  (setq popper-group-function #'popper-group-by-projectile
        popper-mode-line '(" *POP* ")
        popper-reference-buffers '(compilation-mode
                                   messages-mode
                                   help-mode
                                   occur-mode
                                   "^\\*Warnings\\*"
                                   "^\\*Compile-Log\\*"
                                   ;"^\\*Messages\\*"
                                   "^\\*Backtrace\\*"
                                   "^\\*evil-registers\\*"
                                   "^\\*Apropos\\*"
                                   ;"^\\*Completions\\*"
                                   "^Calc:"))
  (popper-mode 1))

;;------------------------------------------------------------------------------
;; Vterm

(use-package vterm
  :config
  (setq shell-file-name "/bin/bash"
        vterm-max-scrollback 1000))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project)
  (add-to-list
    'display-buffer-alist
    '((lambda (buffer-or-name _)
        (let ((buffer (get-buffer buffer-or-name)))
          (with-current-buffer buffer
            (or (equal major-mode 'vterm-mode)
                (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
      (display-buffer-reuse-window display-buffer-at-bottom)
      (direction . bottom)
      (reusable-frames . visible)
      (window-height . 0.35))))

;;------------------------------------------------------------------------------
;; Highlight TODOs

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("DELME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;;------------------------------------------------------------------------------
;; Git gutter fringe plus

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (setq-default left-fringe-width  3)
  (fringe-helper-define
    'git-gutter-fr:added nil
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX.")
  (fringe-helper-define
    'git-gutter-fr:deleted nil
    ".........."
    ".........."
    ".........."
    ".........."
    ".........."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".........."
    ".........."
    "..........")
  (fringe-helper-define
    'git-gutter-fr:modified nil
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX."
    ".XXXXXXXX.")
  (set-face-foreground 'git-gutter-fr:modified "#4FC3F7")
  (global-git-gutter-mode 1))

;;------------------------------------------------------------------------------
;; Drag stuff (M-<down> and M-<up>)

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

;;------------------------------------------------------------------------------
;; Display colors of RGB strings (uncomment when needed)

;(use-package rainbow-mode
;  :diminish
;  :hook prog-mode)

;;------------------------------------------------------------------------------
;; Nasm mode (fork)

(straight-use-package
  '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

