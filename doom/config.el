;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ------------------------ FUNCTIONS ------------------------

;; $DOOMDIR/custom-lisp/custom-functions.el
(add-to-list 'load-path (concat doom-user-dir "custom-lisp/"))
(require 'custom-functions)

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
  '(org-level-1 ((t (:inherit outline-1 :family "Fira Code" :height 1.6))))
  '(org-level-2 ((t (:inherit outline-2 :family "Fira Code" :height 1.2))))
  '(org-level-3 ((t (:inherit outline-3 :family "Fira Code" :height 1.2)))))

;; Theme
(setq doom-theme 'doom-monokai-custom)

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
(setq display-line-numbers-type 'relative)

;; 80 column line
(setq fill-column 80)

;; Using ?\u00A6 (¦) instead of "│" if there are spaces between lines.
;(setq display-fill-column-indicator-character ?\u00A6)
(add-hook! 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook! 'prog-mode-hook
  (setq display-fill-column-indicator-character ?\u00A6))

;; Show battery in mode line. Useful for laptops.
(display-battery-mode)

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

;; SPC t W -> Toggle Auto Fill mode (automatic line wrapping)
;; SPC t w is used to toggle soft line wrapping when displaying.
(map! :desc "Auto fill mode" :n "SPC t W" 'auto-fill-mode)

;; SPC c p -> Compile in parent directory (custom function from
;;            custom-lisp/custom-functions.el)
(map! :desc "Compile parent" :n "SPC c p" 'make-parent)

;; ------------------------ MODULES ------------------------
;; For packages, see packages.el

;; For streaming from libre.fm using emms
(require 'emms-librefm-stream)
(setq emms-librefm-scrobbler-username "8dcc"
      emms-librefm-scrobbler-password "PASSWORD")
(emms-mode-line-mode 0)         ; Only display time, not song

;; Hook for org-auto-tangle pacakage
(after! org-auto-tangle
  (add-hook 'org-mode-hook 'org-auto-tangle-mode))
