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

;; Org visual settings
(setq org-fontify-quote-and-verse-blocks t
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t)

;; ------------------------ IRC ------------------------

;; ERC variables (IRC client)
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
      erc-fill-column 130
      ;; Align usernames to col 20
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 15
      ;; Rename buffers to network name instead of ip:port
      erc-rename-buffers t

      ;; Kill buffers for channels after /part
      erc-kill-buffer-on-part t
      ;; Kill buffers for private queries after quitting the server
      erc-kill-queries-on-quit t
      ;; Kill buffers for server messages after quitting the server
      erc-kill-server-buffer-on-quit t)

;; Set prompt to buffer name
(setq erc-prompt (lambda ()
                   (concat "[" (buffer-name) "]:")))

;; Enable spelling beacuse I can't wreti
(erc-spelling-mode 1)

;; Function for connecting to libera-chat. Prompt for password. Not sure how to
;; use concat within interactive.
(defun erc-libera (erc-pass)
  ;(interactive (concat "sPassword for " erc-nick ":"))
  (interactive "sPassword: ")
  (erc-tls :server   "irc.libera.chat"
           :port     "6697"
           :password erc-pass))

;; ------------------------ REMAPS ------------------------

;; :q -> SPC b k
(map! [remap evil-quit] #'kill-current-buffer)

;; :wq -> SPC Z X
(map! [remap evil-save-and-close] #'doom/save-and-kill-buffer)

;; SPC b f -> Format
(map! :desc "Format current buffer" :n "SPC b f" '+format/buffer)

;; SPC b l -> List buffers in current window
(map! :desc "List buffers" :n "SPC b l" 'buffer-menu)

;; SPC t W -> Toggle Auto Fill mode (automatic line wrapping)
;; SPC t w is used to toggle soft line wrapping when displaying.
(map! :desc "Auto fill mode" :n "SPC t W" 'auto-fill-mode)

;; SPC c p -> Compile in parent directory (custom function from
;;            custom-lisp/custom-functions.el)
(map! :desc "Compile parent" :n "SPC c p" 'make-parent)

;; C-+ -> Increase font size
;; C-= -> Reset font size
(map! :desc "Increase font size" :n "C-+" 'text-scale-increase)
(map! :desc "Reset font size" :n "C-=" 'doom/reset-font-size)

;; ------------------------ MODULES ------------------------
;; For packages, see packages.el

;; For streaming from libre.fm using emms
(require 'emms-librefm-stream)
(setq emms-librefm-scrobbler-username "8dcc"
      emms-librefm-scrobbler-password "PASSWORD")
(emms-mode-line-mode 0)         ; Only display time, not song

;; Hook for org-auto-tangle pacakage
(add-hook 'org-mode-hook 'org-auto-tangle-mode)

;; Enable "<s TAB" keys for src blocks
(require 'org-tempo)
