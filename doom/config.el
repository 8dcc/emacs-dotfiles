;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ------------------------ GENERAL SETTINGS ------------------------
;; Identification for GPG configuration, email, templates...
(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

;; Fonts
(setq doom-font (font-spec :family "Dina" :size 10))

;; Theme
(setq doom-theme 'doom-one)

;; Style of line numbers. If set to `nil', line numbers are disabled, `t' for
;; normal numbers and `relative' for relative line numbers.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; 80 column line
(display-fill-column-indicator-mode t)

;; ------------------------ PLUGINS ------------------------

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Indent guides
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(after! indent-guides
        (setq highlight-indent-guides-method 'column)
        (setq highlight-indent-guides-character, "¦"))

;; SPC b f -> Format
(map! :desc "Format current buffer" :n "SPC b f" '+format/buffer)
