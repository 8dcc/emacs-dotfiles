;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ------------------------ GENERAL SETTINGS ------------------------
;; Identification for GPG configuration, email, templates...
(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

;; Fonts
;;  * media-fonts/dina
;;  * media-fonts/fira-mono
;;  * media-fonts/fira-code
(setq doom-font (font-spec :family "Dina" :size 10)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :weight 'light :size 14)
      doom-big-font-increment 5)
(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :family "Fira Code" :height 1.5))))
  '(org-level-2 ((t (:inherit outline-2 :family "Fira Code" :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :family "Fira Code" :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :family "Fira Code" :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :family "Fira Code" :height 1.0)))))

;; Theme
(setq doom-theme 'doom-monokai-custom)

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; 80 column line
(setq fill-column 80)
;; Using ?\u00A6 (¦) instead of "│" if there are spaces between lines.
(setq display-fill-column-indicator-character ?\u00A6)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Split to the right and bellow
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Org agenda location
(setq org-directory (expand-file-name "~/Sync/Org/"))

;; ------------------------ REMAPS ------------------------

;; :q -> SPC b k
(map! [remap evil-quit] #'kill-current-buffer)

;; :wq -> SPC Z X
(map! [remap evil-save-and-close] #'doom/save-and-kill-buffer)

;; SPC b f -> Format
(map! :desc "Format current buffer" :n "SPC b f" '+format/buffer)

;; ------------------------ MODULES ------------------------

;; Generate doxygen documentation from C code
(add-to-list 'load-path (expand-file-name "~/.config/doom/modules/user-defined/"))
(load "gendoxy.el")

;; Indent guides
;(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;(after! indent-guides
;        (setq highlight-indent-guides-method 'column)
;        (setq highlight-indent-guides-character, "¦"))

;; Nov.el (For epubs). Uses doom-variable-pitch-font
(require 'nov)

;; For streaming from libre.fm using emms
(require 'emms-librefm-stream)
(setq emms-librefm-scrobbler-username "8dcc"
      emms-librefm-scrobbler-password "PASSWORD")
