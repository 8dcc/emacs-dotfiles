(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'x8dcc-modus-vivendi)

(setq fancy-splash-image (concat user-emacs-directory "img/splash.png"))

(set-face-attribute 'default nil
  :font "Dina 8")

(set-fontset-font t 'unicode "Cozette 10")

(set-face-attribute 'variable-pitch nil
  :font "Fira Sans 12"
  :weight 'light)

(set-face-attribute 'fixed-pitch nil
  :font "Dina 8")

(add-to-list 'default-frame-alist '(font . "Dina 8"))

(set-face-attribute
 'org-level-1 nil
 :family "Fira Code" :height 1.6 :inherit 'outline-1)

(set-face-attribute
 'org-level-2 nil
 :family "Fira Code" :height 1.2 :inherit 'outline-2)

(set-face-attribute
 'org-level-3 nil
 :family "Fira Code" :height 1.2 :inherit 'outline-3)

(set-face-attribute 'org-done nil          :inherit 'shadow :bold t)
(set-face-attribute 'org-headline-done nil :inherit 'shadow)

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'visual-line-mode))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo
        evil-mode-line-format '(after . mode-line-frame-identification))
  (evil-mode))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer x8dcc/leader-keys
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"))

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

(use-package projectile
  :diminish
  :config
  (projectile-global-mode 1))

(use-package magit
  :config
  ;; Fullscreen status window
  (setq magit-display-buffer-function
        #'magit-display-buffer-fullframe-status-v1))

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "8dcc's Emacs"
        dashboard-startup-banner (concat user-emacs-directory "img/splash.png")
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-footer nil
        dashboard-page-separator "\n\n"
        dashboard-items '((recents . 10)
                          (projects . 5)
                          (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-reverse-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package popper
  :config
  (setq popper-group-function #'popper-group-by-projectile
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
  (let ((popper-mode-line-formatted (propertize " *POP* " 'face 'bold)))
    (setq popper-mode-line popper-mode-line-formatted))
  (popper-mode 1))

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

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
(fringe-helper-define
    'git-gutter-fr:added nil
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX")
  (fringe-helper-define
    'git-gutter-fr:deleted nil
    "..."
    "..."
    "..."
    "..."
    "..."
    "..."
    "..."
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "..."
    "..."
    "...")
  (fringe-helper-define
    'git-gutter-fr:modified nil
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX"
    "XXX")
  (global-git-gutter-mode 1))

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package rainbow-mode
  :diminish
  :hook prog-mode)

(use-package vi-tilde-fringe
  :diminish
  :config
  (setq vi-tilde-fringe-bitmap-array [0 0 0 9 21 18 0 0])
  (global-vi-tilde-fringe-mode 1))

(straight-use-package
  '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(global-set-key (kbd "C-+")            'text-scale-increase)
(global-set-key (kbd "C--")            'text-scale-decrease)
(global-set-key (kbd "C-<wheel-up>")   'text-scale-increase)
(global-set-key (kbd "C-<wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-<home>") (lambda () (interactive)
                                   (text-scale-adjust 0)))

(global-set-key [escape] 'keyboard-escape-quit)

(global-set-key [remap evil-quit] #'kill-current-buffer)

(global-set-key [remap evil-save-and-close]
                (lambda () (interactive)
                  (basic-save-buffer)
                  (kill-current-buffer)))

(x8dcc/leader-keys
  "SPC" '(projectile-find-file :wk "Find file in project")
  "."   '(find-file :wk "Find file")
  ;; Buffer
  "b"   '(:ignore t :wk "Buffer")
  "b l" '(buffer-menu :wk "Buffer list")
  "b s" '(basic-save-buffer :wk "Save buffer")
  "b r" '(revert-buffer :wk "Revert buffer")
  "b k" '(kill-current-buffer :wk "Kill current buffer")
  ;; Magit
  "g"   '(:ignore t :wk "Git")
  "g g" '(magit-status :wk "Magit status")
  ;; Help
  "h"   '(:ignore t :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h k" '(describe-key :wk "Describe key")
  "h m" '(describe-mode :wk "Describe mode")
  "h v" '(describe-variable :wk "Describe variable")
  ;; Open
  "o"   '(:ignore t :wk "Open")
  "o e" '(eshell :wk "Open eshell")
  "o t" '(vterm-toggle :wk "Toggle vterm")
  ;; Project
  "p"   '(:ignore t :wk "Project")
  "p c" '(projectile-compile-project :wk "Compile project")
  "p C" '(projectile-repeat-last-command :wk "Re-compile project")
  ;; Search
  "s"   '(:ignore t :wk "Search")
  "s o" '(occur :wk "Search occurrences")
  ;; Toggle
  "t"   '(:ignore t :wk "Toggle")
  "t c" '(display-fill-column-indicator-mode :wk "Fill column line")
  "t p" '(popper-toggle :wk "Popups")
  "t s" '(whitespace-mode :wk "Whitespace visualization")
  "t v" '(visible-mode :wk "Visible")
  "t w" '(toggle-truncate-lines :wk "Line wrapping")
  "t W" '(auto-fill-mode :wk "Auto fill mode")
  ;; Window
  "w"   '(:ignore t :wk "Window")
  "w c" '(evil-window-delete :wk "Close window")
  "w s" '(evil-window-split :wk "Horizontal split window")
  "w v" '(evil-window-vsplit :wk "Vertical split window")
  "w h" '(evil-window-left :wk "Window left")
  "w l" '(evil-window-right :wk "Window right")
  "w j" '(evil-window-down :wk "Window down")
  "w k" '(evil-window-up :wk "Window up")
  "w w" '(evil-window-next :wk "Next window")
  "w H" '(evil-window-move-far-left :wk "Move window left")
  "w L" '(evil-window-move-far-right :wk "Move window right")
  "w J" '(evil-window-move-very-bottom :wk "Move window down")
  "w K" '(evil-window-move-very-top :wk "Move window up"))

(column-number-mode 1)

(defun my-mode-line-render (left right)
  "Return a string of `window-width' length.
   With LEFT and RIGHT justified respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            ;; (("%%%ds", 5) "") -> ("%5s", "") -> "     "
            (list (format (format "%%%ds" available-width) ""))
            right)))

(setq-default mode-line-format
              '("%e  λ "
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                "  "
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                mode-line-modes
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info))

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

(add-hook 'vterm-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

(setq truncate-lines nil)
(global-visual-line-mode 1)

(setq lazy-highlight-cleanup nil
      lazy-highlight-initial-delay nil
      lazy-highlight-max-at-a-time nil
      isearch-allow-scroll t)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-fill-column-indicator-character ?\u00A6
                  fill-column 80)
            (display-fill-column-indicator-mode 1)))

(indent-tabs-mode 0)
(setq-default tabs-width 4)

(global-auto-revert-mode 1)

(save-place-mode 1)

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (electric-indent-mode -1)))

(add-hook 'dashboard-mode-hook
          (lambda ()
            (vi-tilde-fringe-mode 0)))

(setq backup-directory-alist
      (list (cons ".*" (concat user-emacs-directory "trash")))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      20
      kept-old-versions      5)

(require 'battery)
(let ((battstr (battery-format "%B" (funcall battery-status-function))))
  (if (or (string= "N/A" battstr)
          (string= "unknown" battstr))
    (display-battery-mode 0)
    (display-battery-mode 1)))

(require 'org-tempo)

(setq org-directory (expand-file-name "~/Sync/Org/"))

(setq org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t)

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (setq org-link-descriptive 1)
                           (setq-local
                             electric-pair-inhibit-predicate
                             `(lambda (c)
                                (if (char-equal c ?<)
                                  t
                                  (,electric-pair-inhibit-predicate c))))))

(setq c-default-style "k&r"
      c-basic-offset 'tab-width
      c-tab-always-indent nil)