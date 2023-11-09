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
        evil-want-C-i-jump nil
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
    :prefix "SPC")
  (general-create-definer x8dcc/org-keys
    :states '(normal visual emacs)
    :keymaps 'org-mode-map
    :prefix "SPC"))

(use-package which-key
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-upercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t)
  (which-key-mode 1))

(use-package projectile
  :diminish
  :config
  (projectile-global-mode 1))

(use-package magit
  :hook ((git-commit-setup . evil-insert-state))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

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
  :config
  (vertico-mode 1)
  (vertico-reverse-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult
  :config
  (setq completion-in-region-function
		(lambda (&rest args)
          (apply (if vertico-mode
					 #'consult-completion-in-region
                   #'completion--in-region)
				 args))))

(use-package popper
  :config
  (setq popper-group-function #'popper-group-by-projectile
        popper-reference-buffers '(compilation-mode
                                   messages-mode
                                   help-mode
                                   occur-mode
								   man-mode
                                   "^\\*Warnings\\*"
                                   "^\\*Compile-Log\\*"
                                   "^\\*Backtrace\\*"
                                   "^\\*evil-registers\\*"
                                   "^\\*Apropos\\*"
                                   ;"^\\*Messages\\*"
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

(defmacro x8dcc/fringe-helper-rect (name alignment w h)
  "Convert W and H to a bitmap array, and call `define-fringe-bitmap' with NAME
and ALIGNMENT as parameters."
  `(define-fringe-bitmap ,name
     (apply #'vector
            (make-list ,h
                       (- (ash 1 ,w) 1)))
     nil nil ,alignment))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (x8dcc/fringe-helper-rect 'git-gutter-fr:added nil 3 30)
  (x8dcc/fringe-helper-rect 'git-gutter-fr:deleted nil 3 30)
  (x8dcc/fringe-helper-rect 'git-gutter-fr:modified nil 3 30)
  (global-git-gutter-mode 1))

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

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

(use-package rainbow-mode
  :diminish
  :hook prog-mode)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode  . rainbow-delimiters-mode)
         (scheme-mode      . rainbow-delimiters-mode)
         (common-lisp-mode . rainbow-delimiters-mode)))

(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package clang-format
  :config
  (setq clang-format-style "file"))

(use-package vi-tilde-fringe
  :diminish
  :hook ((prog-mode . vi-tilde-fringe-mode)
         (org-mode  . vi-tilde-fringe-mode)
         (text-mode . vi-tilde-fringe-mode))
  :config
  (setq vi-tilde-fringe-bitmap-array [0 0 0 9 21 18 0 0]))

(use-package writeroom-mode
  :config
  (setq writeroom-mode-line nil
        writeroom-restore-window-config t
        writeroom-width 81
        writeroom-global-effects '(writeroom-set-alpha
                                   writeroom-set-menu-bar-lines
                                   writeroom-set-tool-bar-lines
                                   writeroom-set-vertical-scroll-bars
                                   writeroom-set-bottom-divider-width))
  (add-hook 'writeroom-mode-enable-hook (lambda ()
                                          (fringe-mode 0)))
  (add-hook 'writeroom-mode-disable-hook (lambda ()
                                           (fringe-mode nil))))

(straight-use-package
  '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(global-set-key (kbd "C-+")            #'text-scale-increase)
(global-set-key (kbd "C--")            #'text-scale-decrease)
(global-set-key (kbd "C-<wheel-up>")   #'text-scale-increase)
(global-set-key (kbd "C-<wheel-down>") #'text-scale-decrease)
(global-set-key (kbd "C-<home>") (lambda () (interactive)
                                   (text-scale-adjust 0)))

(global-set-key [escape] #'keyboard-escape-quit)

(global-set-key (kbd "C-S-v") #'yank)

(global-set-key [remap evil-quit] #'kill-current-buffer)

(global-set-key [remap evil-save-and-close]
                (lambda () (interactive)
                  (basic-save-buffer)
                  (kill-current-buffer)))

(defun x8dcc/org-insert-link ()
  "Inserts a space in the current position, and calls `org-insert-link'."
  (interactive)
  (insert " ")
  (funcall-interactively #'org-insert-link))

(x8dcc/leader-keys
  "SPC" '(projectile-find-file :wk "Find file in project")
  "."   '(find-file            :wk "Find file")
  ;; Buffer
  "b"   '(:ignore t           :wk "Buffer")
  "b f" '(clang-format-buffer :wk "Format (clang-format)")
  "b k" '(kill-current-buffer :wk "Kill current")
  "b l" '(consult-buffer      :wk "Go to (consult)")
  "b L" '(buffer-menu         :wk "List")
  "b s" '(basic-save-buffer   :wk "Save")
  "b r" '(revert-buffer       :wk "Revert (discard)")
  ;; File
  "f"   '(:ignore t    :wk "File")
  "f r" '(recentf-open :wk "Open recent")
  ;; Magit
  "g"   '(:ignore t    :wk "Git")
  "g g" '(magit-status :wk "Magit status")
  ;; Help
  "h"   '(:ignore t         :wk "Help")
  "h f" '(describe-function :wk "Describe function")
  "h F" '(describe-face     :wk "Describe face")
  "h k" '(describe-key      :wk "Describe key")
  "h m" '(describe-mode     :wk "Describe mode")
  "h v" '(describe-variable :wk "Describe variable")
  ;; Open
  "o"   '(:ignore t    :wk "Open")
  "o -" '(dired-jump   :wk "Dired")
  "o e" '(eshell       :wk "Eshell")
  "o t" '(vterm-toggle :wk "Toggle vterm")
  ;; Project
  "p"   '(:ignore t                      :wk "Project")
  "p c" '(projectile-compile-project     :wk "Compile")
  "p C" '(projectile-repeat-last-command :wk "Re-compile")
  "p r" '(projectile-recentf             :wk "Recent files")
  ;; Search
  "s"   '(:ignore t     :wk "Search")
  "s i" '(consult-imenu :wk "Symbol")
  "s o" '(occur         :wk "Occurrences")
  ;; Toggle
  "t"   '(:ignore t                          :wk "Toggle")
  "t c" '(display-fill-column-indicator-mode :wk "Fill column line")
  "t p" '(popper-toggle                      :wk "Popups")
  "t r" '(read-only-mode                     :wk "Read only")
  "t s" '(whitespace-mode                    :wk "Whitespace visualization")
  "t v" '(visible-mode                       :wk "Visible")
  "t w" '(toggle-truncate-lines              :wk "Line wrapping")
  "t W" '(auto-fill-mode                     :wk "Auto fill mode")
  "t z" '(writeroom-mode                     :wk "Zen mode")
  ;; Window
  "w"   '(:ignore t                    :wk "Window")
  "w c" '(evil-window-delete           :wk "Close")
  "w s" '(evil-window-split            :wk "Split horizontally")
  "w v" '(evil-window-vsplit           :wk "Split vertically")
  "w h" '(evil-window-left             :wk "Left")
  "w l" '(evil-window-right            :wk "Right")
  "w j" '(evil-window-down             :wk "Down")
  "w k" '(evil-window-up               :wk "Up")
  "w w" '(evil-window-next             :wk "Next")
  "w H" '(evil-window-move-far-left    :wk "Move current left")
  "w L" '(evil-window-move-far-right   :wk "Move current right")
  "w J" '(evil-window-move-very-bottom :wk "Move current down")
  "w K" '(evil-window-move-very-top    :wk "Move current up"))

(x8dcc/org-keys
 "m"     '(:ignore t :wk "Org")
 ;; Date
 "m d"   '(:ignore t    :wk "Date")
 "m d d" '(org-deadline :wk "Deadline")
 "m d s" '(org-schedule :wk "Schedule")
 ;; Export
 "m e"   '(:ignore t                 :wk "Export")
 "m e a" '(org-ascii-export-to-ascii :wk "ASCII (text)")
 "m e h" '(org-html-export-to-html   :wk "HTML")
 "m e l" '(org-latex-export-to-latex :wk "LaTeX")
 "m e p" '(org-latex-export-to-pdf   :wk "PDF")
 ;; Link
 "m l"   '(:ignore t             :wk "Link")
 "m l l" '(x8dcc/org-insert-link :wk "Insert")
 ;; Priority
 "m p"   '(:ignore t         :wk "Priority")
 "m p d" '(org-priority-down :wk "Decrease")
 "m p p" '(org-priority      :wk "Insert")
 "m p u" '(org-priority-up   :wk "Increase")
 ;; Todo
 "m t"   '(org-todo :wk "Toggle todo")
 ;; Tangle
 "m T"   '(org-babel-tangle :wk "Tangle current file"))

(column-number-mode 1)

(defun x8dcc/mode-line-render (left right)
  "Return a string of `window-width' length. With LEFT and RIGHT justified
respectively."
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

(add-hook 'eshell-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))

(add-to-list 'display-buffer-alist
             '("\\*eshell\\*"
               (display-buffer-in-side-window (side . bottom))))

(setq truncate-lines nil)
(global-visual-line-mode 1)

(setq lazy-highlight-cleanup nil
      lazy-highlight-initial-delay nil
      lazy-highlight-max-at-a-time nil
      isearch-allow-scroll t)

(setq-default display-fill-column-indicator-character ?\u00A6
              fill-column 80)

(add-hook 'prog-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode 1)))

(setq vc-follow-symlinks t)

(global-auto-revert-mode 1)

(save-place-mode 1)

(savehist-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(indent-tabs-mode 0)
(setq-default tab-width 4)

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)
            (electric-indent-mode -1)))

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "trash"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "trash") t)))

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(require 'battery)
(let ((battstr (battery-format "%B" (funcall battery-status-function))))
  (if (or (string= "N/A" battstr)
          (string= "unknown" battstr))
    (display-battery-mode 0)
    (display-battery-mode 1)))

(require 'erc)
(require 'erc-log)
(require 'erc-stamp)
(require 'erc-track)
;(require 'erc-spelling)

(erc-log-enable)
(erc-stamp-enable)
(erc-track-enable)
;(erc-spelling-enable)

(setq erc-nick           "x8dcc"
      erc-system-name    "x8dcc"
      erc-user-full-name "x8dcc"

      ;; Don't give away machine name
      erc-anonymous-login t
      ;; Don't reply to ctcp
      erc-disable-ctcp-replies t
      ;; Notify ctcp requests
      erc-paranoid t
      ;; Warn blank lines
      erc-warn-about-blank-lines t

      ;; Enable logging
      erc-enable-logging t
      ;; Directory for logs
      erc-log-channels-directory "~/.erc-log"
      ;; When to write logs
      erc-log-write-after-send t
      erc-log-write-after-insert t
      ;; Timestamps
      erc-stamp-mode t
      erc-hide-timestamps t

      ;; Hide joins/leaves/quits
      erc-hide-list '("JOIN" "PART" "QUIT")
      ;; Max line width
      erc-fill-column 100
      ;; Align usernames to col 20
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 15
      ;; Rename buffers to network name instead of ip:port
      erc-rename-buffers t
      ;; Prompt at the bottom of the screen
      erc-scrolltobottom-mode t
      erc-input-line-position -1
      ;; Messages to mode-line
      erc-track-showcount t
      erc-track-exclude-list '("NICK" "JOIN" "PART" "QUIT" "333" "353")

      ;; Kill buffers for channels after /part
      erc-kill-buffer-on-part t
      ;; Kill buffers for private queries after quitting the server
      erc-kill-queries-on-quit t
      ;; Kill buffers for server messages after quitting the server
      erc-kill-server-buffer-on-quit t)

(setq erc-prompt (lambda ()
                   (concat "[" (buffer-name) "]:")))

(require 'org-tempo)

(setq org-directory (expand-file-name "~/Sync/Org/")
      org-agenda-files (list (concat org-directory "agenda.org")))

(setq org-fontify-quote-and-verse-blocks t
      org-src-fontify-natively t
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t)

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (setq org-link-descriptive 1)
                           (setq-local
                             electric-pair-inhibit-predicate
                             `(lambda (c)
                                (if (char-equal c ?<)
                                  t
                                  (,electric-pair-inhibit-predicate c))))))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (scheme . t)))

(setq c-default-style "k&r"
      c-basic-offset tab-width
      c-tab-always-indent nil)
