
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

;; Add theme to whitelist and load
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5efe1fe52c7b6b8fa3217865cce8102eb6e53d1698acbe6395f67862b4753b34" default))
 '(package-selected-packages '(which-key evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'x8dcc)

;; Remove GUI bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Change splash screen image
(setq fancy-splash-image (concat user-emacs-directory "img/splash.png"))

;;------------------------------------------------------------------------------
;; Misc visual settings

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; 80 column line
(setq fill-column 80)

;; Using ?\u00A6 (¦) instead of "│" if there are spaces between lines.
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (setq display-fill-column-indicator-character ?\u00A6)))

;; Wrap lines by default
(global-visual-line-mode 1)

;;------------------------------------------------------------------------------
;; Fonts

;; Default
(set-face-attribute 'default nil
  :family "Dina 11")

;; Fallback
(set-fontset-font t nil "Cozette 11")

;; Variable pitch
(set-face-attribute 'variable-pitch nil
  :font "Fira Sans 12"
  :weight 'light)

;; Fixed pitch
(set-face-attribute 'fixed-pitch nil
  :font "Dina 11")

;; Needed for emacsclient (?)
(add-to-list 'default-frame-alist '(font . "Dina 11"))

;;------------------------------------------------------------------------------
;; Custom keybinds

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;; Other <SPC> keybinds
(nvmap :prefix "SPC"
  "."   '(find-file :which-key "Find file")
  "b l" '(buffer-menu :which-key "Buffer menu")
  "t W" '(auto-fill-mode :which-key "Auto fill mode")
  "t c" '(display-fill-column-indicator-mode :which-key "Fill column line"))
