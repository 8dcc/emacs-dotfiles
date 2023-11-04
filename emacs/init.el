
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
;; Package managers

;; Install straight.el (with use-package) for more convenience. See:
;; https://web.archive.org/web/20230522053703/https://jeffkreeftmeijer.com/emacs-straight-use-package/
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

;; Wrap lines by default
(global-visual-line-mode 1)

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

;;------------------------------------------------------------------------------
;; Scrolling and motion

;; Scroll smoothly when cursor moves out of the screen (1 line at a time)
;; Don't accelerate scrolling
;; Scroll window under mouse
;; Scroll 2 lines at a time with mouse wheel, and scroll horizontally with shift
(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll)))

;;------------------------------------------------------------------------------
;; Custom keybinds

;; Zoom in and out
(global-set-key (kbd "C-+")            'text-scale-increase)
(global-set-key (kbd "C--")            'text-scale-decrease)
(global-set-key (kbd "C-<wheel-up>")   'text-scale-increase)
(global-set-key (kbd "C-<wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "C-<home>") (lambda () (interactive) (text-scale-adjust 0)))

;; Other <SPC> keybinds
(nvmap :prefix "SPC"
  "SPC" '(projectile-find-file :which-key "Find file in project")
  "."   '(find-file :which-key "Find file")
  ;; Buffer
  "b l" '(buffer-menu :which-key "Buffer menu")
  ;; Toggle
  "t W" '(auto-fill-mode :which-key "Auto fill mode")
  "t c" '(display-fill-column-indicator-mode :which-key "Fill column line")
  ;; Window
  "w c" '(evil-window-delete :which-key "Close window")
  ;"w n" '(evil-window-new :which-key "New window")
  "w s" '(evil-window-split :which-key "Horizontal split window")
  "w v" '(evil-window-vsplit :which-key "Vertical split window")
  "w h" '(evil-window-left :which-key "Window left")
  "w l" '(evil-window-right :which-key "Window right")
  "w j" '(evil-window-down :which-key "Window down")
  "w k" '(evil-window-up :which-key "Window up")
  "w w" '(evil-window-next :which-key "Next window")
  "w H" '(evil-window-move-far-left :which-key "Move window left")
  "w L" '(evil-window-move-far-right :which-key "Move window right")
  "w J" '(evil-window-move-very-bottom :which-key "Move window down")
  "w K" '(evil-window-move-very-top :which-key "Move window up"))
;; TODO: Add a bunch of keybinds:
;;  - SPC tab *
