
;; Identification for GPG configuration, email, templates...
(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

;;------------------------------------------------------------------------------
;; Package managers

;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; Install use-package for more convenience
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Load packages
(load (concat user-emacs-directory "packages.el"))

;;------------------------------------------------------------------------------
;; Theme

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Load ~/.emacs.d/x8dcc-theme.el
(load-theme 'x8dcc)

;; Remove GUI bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Change splash screen image (should not matter since it's overwritten by
;; dashboard)
(setq fancy-splash-image (concat user-emacs-directory "img/splash.png"))

;;------------------------------------------------------------------------------
;; Misc visual settings

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Set 80 column line with specified character. Try using ?\u00A6 (¦) instead
;; of ?\u2502 (│) if there are spaces between lines.
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-fill-column-indicator-character ?\u00A6
                  fill-column 80)))

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

;; Fallback
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
;; Custom keybinds

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

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
