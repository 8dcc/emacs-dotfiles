
;; Identification for GPG configuration, email, templates, etc.
(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

;;------------------------------------------------------------------------------
;; Theme

;; Remove GUI bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use separate custom file for readability
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Load ~/.emacs.d/x8dcc-theme.el
(load-theme 'x8dcc)

;; Change splash screen image (should not matter since it's overwritten by
;; dashboard)
(setq fancy-splash-image (concat user-emacs-directory "img/splash.png"))

;;------------------------------------------------------------------------------
;; Fonts

;; Default
(set-face-attribute 'default nil
  :font "Dina 8")

;; Fallback (needs to be set to unicode, can't use nil)
(set-fontset-font t 'unicode "Cozette 10")

;; Variable pitch
(set-face-attribute 'variable-pitch nil
  :font "Fira Sans 12"
  :weight 'light)

;; Fixed pitch
(set-face-attribute 'fixed-pitch nil
  :font "Dina 8")

;; Needed for emacsclient (?)
(add-to-list 'default-frame-alist '(font . "Dina 8"))

;; Org mode titles
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :family "Fira Code" :height 1.6))))
  '(org-level-2 ((t (:inherit outline-2 :family "Fira Code" :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :family "Fira Code" :height 1.2)))))

;;------------------------------------------------------------------------------
;; Package managers

;; Install straight.el (with use-package) for more convenience. See:
;; https://web.archive.org/web/20230522053703/https://jeffkreeftmeijer.com/emacs-straight-use-package/
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

;; Install use-package (through straight.el) for more convenience. Setting
;; straight-use-package-by-default tells use-package to always use straight.el
;; to install packages (instead of package.el), even without specifying
;; ":straight t".
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; Install and load packages from separate file for more readability
(load (concat user-emacs-directory "packages.el"))

;;------------------------------------------------------------------------------
;; Scrolling and motion

;; Scroll smoothly when cursor moves out of the screen (1 line at a time)
;; Don't accelerate scrolling
;; Scroll window under mouse
;; Scroll 2 lines at a time with mouse wheel, and scroll horizontally with shift
(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

;;------------------------------------------------------------------------------
;; Custom keybinds

;; Zoom in and out
(global-set-key (kbd "C-+")            'text-scale-increase)
(global-set-key (kbd "C--")            'text-scale-decrease)
(global-set-key (kbd "C-<wheel-up>")   'text-scale-increase)
(global-set-key (kbd "C-<wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-<home>") (lambda () (interactive)
                                   (text-scale-adjust 0)))

;; Quit from minibuffer with one ESC
(global-set-key [escape] 'keyboard-escape-quit)

;; :q -> SPC b k
(global-set-key [remap evil-quit] #'kill-current-buffer)

;; :wq -> SPC b s + SPC b k
(global-set-key [remap evil-save-and-close]
                (lambda () (interactive)
                  (basic-save-buffer)
                  (kill-current-buffer)))

;; SPC keybinds. See packages.el, :config of the `general' package.
;; TODO: Add a bunch of keybinds:
;;  - SPC TAB * -> Workspaces
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
  "o t" '(vterm-toggle :wk "Toggle vterm")
  ;; Search
  "s"   '(:ignore t :wk "Search")
  "s o" '(occur :wk "Search occurrences")
  ;; Toggle
  "t"   '(:ignore t :wk "Toggle")
  "t c" '(display-fill-column-indicator-mode :wk "Fill column line")
  "t p" '(popper-toggle :wk "Popups")
  "t v" '(visible-mode :wk "Toggle visible")
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

;;------------------------------------------------------------------------------
;; Misc visual settings

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
;; If display-line-numbers-width-start is `t', the width of the line numbers
;; will be calculated depending on the lines of each buffer.
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

;; Set 80 column line with specified character. Try using ?\u00A6 (¦) instead
;; of ?\u2502 (│) if there are spaces between lines.
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-fill-column-indicator-character ?\u00A6
                  fill-column 80)
            (display-fill-column-indicator-mode)))

;; Hide line numbers in vterm buffers
(add-hook 'vterm-mode-hook
          (lambda ()
            (display-line-numbers-mode 0)))


;;------------------------------------------------------------------------------
;; Misc modes

;; Wrap lines by default (using words)
(global-visual-line-mode 1)

;; Automatically show changes if the file has changed on disk
(global-auto-revert-mode t)

;; Auto-close brackets, disable emacs' weird indentation
(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode t)
            (electric-indent-mode -1)))

;;------------------------------------------------------------------------------
;; Backups

;; Change backup directory to `~/.emacs.d/trash'. We use list and cons because
;; we need to evaluate the concat and the value has to be in the form:
;; '((".*" . "PATH"))
(setq backup-directory-alist
      (list (cons ".*" (concat user-emacs-directory "trash"))))

;;------------------------------------------------------------------------------
;; Battery

;; Show battery in mode line. If the battery is "N/A" or "unknown", don't
;; display.
(require 'battery)
(let ((battstr (battery-format "%B" (funcall battery-status-function))))
  (if (or (string= "N/A" battstr)
          (string= "unknown" battstr))
    (display-battery-mode 0)
    (display-battery-mode 1)))

;;------------------------------------------------------------------------------
;; Org mode

;; Enable "<s TAB" completion
(require 'org-tempo)

;; Org agenda location
(setq org-directory (expand-file-name "~/Sync/Org/"))

;; Org visual settings
(setq org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t)

;; Enable org-indent-mode (Hide leading '*' from titles)
;; Disable electric-pair-mode pairing of '<', so we can use "<s TAB" completion
(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (setq-local
                             electric-pair-inhibit-predicate
                             `(lambda (c)
                                (if (char-equal c ?<)
                                  t
                                  (,electric-pair-inhibit-predicate c))))))

;;------------------------------------------------------------------------------
;; C mode

;; Enable explicit tabs for C code (if not on beginning of line)
(setq c-default-style "k&r"
      c-basic-offset 4
      c-tab-always-indent nil)
