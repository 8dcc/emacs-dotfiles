(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'x8dcc-modus-vivendi)

(setq fancy-splash-image (concat user-emacs-directory "my-media/splash.png"))

(set-face-attribute 'default nil
  :font "Dina 8")

(set-fontset-font t 'unicode "Cozette 10")

(set-face-attribute 'variable-pitch nil
  :font "Fira Sans 10"
  :weight 'regular)

(set-face-attribute 'fixed-pitch nil
  :font "Dina 8")

(add-to-list 'default-frame-alist '(font . "Dina 8"))

(set-face-attribute 'org-done          nil :inherit 'shadow :bold t)
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
        evil-undo-system #'undo-redo
        evil-want-C-i-jump nil
        evil-mode-line-format '(after . mode-line-frame-identification))
  :config
  (evil-select-search-module 'evil-search-module 'isearch)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t)
  (evil-mode 1))

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (add-to-list 'evil-collection-mode-list 'help)
  (evil-collection-init))

(use-package vundo)

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer x8dcc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/org-keys
    :states '(normal insert visual emacs)
    :keymaps 'org-mode-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/latex-keys
    :states '(normal insert visual emacs)
    :keymaps 'TeX-mode-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/lisp-keys
    :states '(normal insert visual emacs)
    :keymaps '(lisp-mode-map
               emacs-lisp-mode-map
               lisp-interaction-mode-map
               lisp-data-mode-map)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/c-keys
    :states '(normal insert visual emacs)
    :keymaps '(c-mode-map
               c++-mode-map
               java-mode-map
               js-mode-map)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

(use-package which-key
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
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
  (projectile-mode 1))

(use-package magit
  :hook ((git-commit-setup . evil-insert-state))
  :config
  (setq magit-diff-refine-hunk t
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package dashboard
  :init
  (setq dashboard-banner-logo-title "8dcc's Emacs"
        dashboard-startup-banner (concat user-emacs-directory
                                        "my-media/splash.png")
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-set-footer nil
        dashboard-page-separator "\n\n"
        dashboard-items '((recents . 10)
                          (projects . 5)
                          (agenda . 10)))
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
                                   "^\\*scratch\\*"
                                   "^\\*Macroexpansion\\*"
                                   "^\\* Regexp Explain \\*"))
  (let ((popper-mode-line-formatted (propertize " *POP* " 'face 'bold)))
    (setq popper-mode-line popper-mode-line-formatted))
  (popper-mode 1))

(use-package highlight-indent-guides
  :diminish
  :hook ((c-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\u00A6
        highlight-indent-guides-suppress-auto-error t
        highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "#1A1A1A"))

(defun x8dcc/indent-guide-highlighter (level responsive display)
  (if (> level 0)
      (highlight-indent-guides--highlighter-default level responsive display)))

(setq highlight-indent-guides-highlighter-function 'x8dcc/indent-guide-highlighter)

(use-package emms
  :config
  (emms-all)
  (setq emms-player-list '(emms-player-mpv
                           emms-player-vlc)
        emms-player-mpv-parameters '("--quiet"
                                     "--really-quiet"
                                     "--no-audio-display"
                                     "--no-video"
                                     "--volume=50"))
  (emms-mode-line-mode 0))

(unless (member system-type '(ms-dos windows-nt cygwin))
  (use-package spell-fu
    :hook ((org-mode markdown-mode erc-mode mail-mode text-mode) . spell-fu-mode)
    :config
    (add-hook 'spell-fu-mode-hook
              (lambda ()
                (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en_US"))
                (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "es"))))
    (add-hook 'LaTeX-mode-hook
              (lambda ()
                (setq spell-fu-faces-exclude
                      '(font-lock-keyword-face
                        font-lock-function-name-face
                        font-lock-variable-name-face
                        font-lock-warning-face
                        font-latex-sedate-face
                        font-latex-warning-face
                        font-latex-math-face))))
    (add-hook 'markdown-mode-hook
              (lambda ()
                (setq spell-fu-faces-exclude
                      '(markdown-code-face
                        markdown-html-attr-name-face
                        markdown-html-attr-value-face
                        markdown-html-tag-name-face
                        markdown-inline-code-face
                        markdown-link-face
                        markdown-markup-face
                        markdown-plain-url-face
                        markdown-reference-face
                        markdown-url-face))))
    (add-hook 'org-mode-hook
              (lambda ()
                (setq spell-fu-faces-exclude
                      '(org-block
                        org-block-begin-line
                        org-block-end-line
                        org-cite
                        org-cite-key
                        org-code
                        org-date
                        org-footnote
                        org-formula
                        org-inline-src-block
                        org-latex-and-related
                        org-link
                        org-meta-line
                        org-property-value
                        org-ref-cite-face
                        org-special-keyword
                        org-tag
                        org-todo
                        org-todo-keyword-done
                        org-todo-keyword-habt
                        org-todo-keyword-kill
                        org-todo-keyword-outd
                        org-todo-keyword-todo
                        org-todo-keyword-wait
                        org-verbatim))))))

(use-package drag-stuff
  :diminish
  :config
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package hl-todo
  :hook ((org-mode prog-mode) . hl-todo-mode)
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
  :hook ((html-mode css-mode js-mode)  . rainbow-mode))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode scheme-mode common-lisp-mode) . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package clang-format
  :config
  (setq clang-format-style "file"))

(use-package vi-tilde-fringe
  :diminish
  :hook ((prog-mode org-mode text-mode) . vi-tilde-fringe-mode)
  :config
  (setq vi-tilde-fringe-bitmap-array [0 0 0 9 21 18 0 0]))

(use-package htmlize)

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode))
  :config
  (setq org-bullets-bullet-list '("·")))

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

(use-package evil-lion
  :config
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode))

(straight-use-package
 '(big-font :type git :host github :repo "8dcc/big-font.el"))

(setq big-font-height 120
      big-font-family-alist '((default     . "Iosevka 8dcc")
                              (fixed-pitch . "Iosevka 8dcc")))

(use-package auctex)

(straight-use-package
 '(ada-mode :type git :host github :repo "tkurtbond/old-ada-mode"))

(mapc (lambda (element)
        (add-to-list 'auto-mode-alist (cons element 'ada-mode)))
      '("\\.gpr\\'" "\\.ada\\'" "\\.ads\\'" "\\.adb\\'"))

(straight-use-package
 '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

(add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))

(use-package disaster
  :init
  (setq disaster-assembly-mode 'nasm-mode
        disaster-objdump "objdump -d -M intel -Sl --no-show-raw-insn"))

(defun x8dcc/hook-funcs (target functions)
  "Hook each function in FUNCTIONS to TARGET using `add-hook'."
  (mapcar (lambda (func)
            (add-hook target func))
          functions))

(defun x8dcc/hook-to-targets (function targets)
  "Hook FUNCTION to each target in TARGETS using `add-hook'."
  (mapcar (lambda (target)
            (add-hook target function))
          targets))

(defun x8dcc/separator-comment (&optional max-width)
  (interactive)
  (unless max-width
    (setq max-width fill-column))
  (let* ((start (string-trim comment-start))
         (end   (string-trim comment-end))
         (remaining (- max-width (+ (length start)
                                    (length end)))))
    (save-excursion
      (end-of-line)
      (insert "\n" start)
      (insert-char ?- remaining)
      (insert end))))

(defun x8dcc/get-buffer-count (regexp)
  "Return the number of buffers whose name matches REGEXP."
  (length
   (seq-remove (lambda (buffer)
                 (not (string-match-p regexp
                                      (buffer-name buffer))))
               (buffer-list))))

(defun x8dcc/huge-file ()
  "Returns `t' if the current buffer has either too many characters (>500000),
or too many lines (>10000)."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(defun x8dcc/org-insert-link ()
  "Inserts a space in the current position, and calls `org-insert-link'."
  (interactive)
  (insert " ")
  (funcall-interactively #'org-insert-link))

(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)
(keymap-global-set "C-<home>" (lambda () (interactive)
                                (text-scale-adjust 0)))

(keymap-global-set "<escape>" #'keyboard-escape-quit)

(keymap-global-set "C-S-v" #'yank)

(keymap-global-set "<backtab>"
		           (lambda () (interactive)
		             ;; If there is a region active, use `evil-shift-left',
		             ;; otherwise shift the current line.
		             (if (use-region-p)
			             (evil-shift-left (region-beginning) (region-end))
		               (evil-shift-left-line 1))))

(keymap-global-set "<remap> <evil-quit>" #'kill-current-buffer)

(keymap-global-set "<remap> <evil-save-and-close>"
                   (lambda () (interactive)
                     (basic-save-buffer)
                     (kill-current-buffer)))

(with-eval-after-load 'eshell
  (keymap-set eshell-mode-map "C-l" (lambda () (interactive)
                                      (eshell/clear-scrollback)
                                      (eshell-emit-prompt))))

(with-eval-after-load 'ediff-util
  (add-hook 'ediff-startup-hook
            (lambda ()
              (keymap-set ediff-mode-map "<remap> <evil-quit>" #'ediff-quit))))

(x8dcc/leader-keys
  "SPC" '(projectile-find-file :wk "Find file in project") ;; Same as "SPC p f"
  "."   '(find-file            :wk "Find file")            ;; Same as "SPC f f"
  ;; Buffer
  "b"   '(:ignore t           :wk "Buffer")
  "b k" '(kill-current-buffer :wk "Kill current")
  "b l" '(switch-to-buffer    :wk "Switch to")
  "b L" '(buffer-menu         :wk "List")
  "b s" '(basic-save-buffer   :wk "Save")
  "b S" '(write-file          :wk "Save as")
  "b r" '(revert-buffer       :wk "Revert (discard)")
  "b x" '(scratch-buffer      :wk "Open scratch buffer")
  ;; File
  "f"   '(:ignore t          :wk "File")
  "f c" '(compile            :wk "Compile")
  "f C" '(recompile          :wk "Re-compile")
  "f f" '(find-file          :wk "Find file")
  "f o" '(ff-find-other-file :wk "Find other file")
  "f O" '(find-file-at-point :wk "Find file at point")
  "f r" '(recentf-open       :wk "Open recent")
  ;; Git
  "g"   '(:ignore t                 :wk "Git")
  "g c" '(magit-commit              :wk "Commit menu")
  "g e" '(magit-ediff-wdim          :wk "Ediff")
  "g f" '(magit-fetch               :wk "Fetch menu")
  "g F" '(magit-pull                :wk "Pull menu")
  "g g" '(magit-status              :wk "Magit status")
  "g G" '(vc-refresh-state          :wk "Refresh VC state")
  "g p" '(magit-push                :wk "Push menu")
  "g s" '(magit-stage-buffer-file   :wk "Stage current")
  "g u" '(magit-unstage-buffer-file :wk "Unstage current")
  ;; Help
  "h"   '(:ignore t               :wk "Help")
  "h e" '(view-echo-area-messages :wk "Echo area messages")
  "h f" '(describe-function       :wk "Describe function")
  "h F" '(describe-face           :wk "Describe face")
  "h i" '(info                    :wk "Open info")
  "h I" '(shortdoc                :wk "Open shortdoc")
  "h k" '(describe-key            :wk "Describe key")
  "h m" '(describe-mode           :wk "Describe mode")
  "h v" '(describe-variable       :wk "Describe variable")
  ;; Insert
  "i"   '(:ignore t               :wk "Insert")
  "i s" '(x8dcc/separator-comment :wk "Separator comment")
  ;; Open
  "o"   '(:ignore t             :wk "Open")
  "o -" '(dired-jump            :wk "Dired")
  "o a" '(org-agenda            :wk "Org agenda")
  "o c" '(calc                  :wk "Calculator")
  "o d" '(projectile-run-gdb    :wk "Debugger")
  "o e" '(x8dcc/eshell-popup    :wk "Eshell popup")
  "o E" '(x8dcc/eshell-numbered :wk "Eshell")
  "o m" '(man                   :wk "Manpage")
  ;; Project
  "p"   '(:ignore t                      :wk "Project")
  "p c" '(projectile-compile-project     :wk "Compile")
  "p C" '(projectile-repeat-last-command :wk "Re-compile")
  "p f" '(projectile-find-file           :wk "Find file")
  "p p" '(projectile-switch-project      :wk "Recent projects")
  "p r" '(projectile-recentf             :wk "Recent files")
  ;; Search
  "s"   '(:ignore t                    :wk "Search")
  "s e" '(spell-fu-goto-next-error     :wk "Next spelling error")
  "s E" '(spell-fu-goto-previous-error :wk "Previous spelling error")
  "s g" '(rgrep                        :wk "Recursive grep")
  "s h" '(highlight-regexp             :wk "Highlight")
  "s H" '(unhighlight-regexp           :wk "Unhighlight")
  "s i" '(consult-imenu                :wk "Jump to symbol")
  "s l" '(consult-line                 :wk "Jump to line")
  "s o" '(occur                        :wk "Occurrences")
  "s r" '(query-replace                :wk "Replace interactively")
  "s R" '(query-replace-regexp         :wk "Replace regex")
  "s s" '(isearch-forward              :wk "I-search")
  "s S" '(isearch-forward-regexp       :wk "I-search regex")
  ;; Toggle
  "t"   '(:ignore t                          :wk "Toggle")
  "t b" '(big-font-mode                      :wk "Big font")
  "t c" '(display-fill-column-indicator-mode :wk "Fill column line")
  "t C" '(highlight-indent-guides-mode       :wk "Indent guides")
  "t f" '(variable-pitch-mode                :wk "Variable pitch font")
  "t l" '(display-line-numbers-mode          :wk "Line numbers")
  "t L" '(hl-line-mode                       :wk "Highlight line")
  "t p" '(popper-toggle                      :wk "Last popup")
  "t P" '(popper-toggle-type                 :wk "Popup type")
  "t r" '(read-only-mode                     :wk "Read only")
  "t s" '(spell-fu-mode                      :wk "Spell checking")
  "t S" '(whitespace-mode                    :wk "Whitespace visualization")
  "t v" '(visible-mode                       :wk "Visible")
  "t w" '(toggle-truncate-lines              :wk "Line wrapping")
  "t W" '(auto-fill-mode                     :wk "Auto fill mode")
  "t z" '(writeroom-mode                     :wk "Zen mode")
  ;; Window
  "w"   '(:ignore t                    :wk "Window")
  "w c" '(evil-window-delete           :wk "Close")
  "w C" '(kill-buffer-and-window       :wk "Kill buffer and window")
  "w h" '(evil-window-left             :wk "Left")
  "w H" '(evil-window-move-far-left    :wk "Move current left")
  "w j" '(evil-window-down             :wk "Down")
  "w J" '(evil-window-move-very-bottom :wk "Move current down")
  "w k" '(evil-window-up               :wk "Up")
  "w K" '(evil-window-move-very-top    :wk "Move current up")
  "w l" '(evil-window-right            :wk "Right")
  "w L" '(evil-window-move-far-right   :wk "Move current right")
  "w s" '(evil-window-split            :wk "Split horizontally")
  "w v" '(evil-window-vsplit           :wk "Split vertically")
  "w w" '(evil-window-next             :wk "Next")
  ;; Fold
  "z"   '(:ignore t        :wk "Fold")
  "z a" '(evil-toggle-fold :wk "Toggle")
  "z c" '(evil-close-fold  :wk "Close")
  "z m" '(evil-close-folds :wk "Close all")
  "z o" '(evil-open-fold   :wk "Open")
  "z r" '(evil-open-folds  :wk "Open all"))

(x8dcc/org-keys
  ;; Insert
  "i h"   '(x8dcc/org-insert-header :wk "Default header")
  ;; Toggle
  "t i"   '(org-toggle-inline-images :wk "Inline images")
  ;; Mode (Org)
  "m"     '(:ignore t :wk "Org")
  ;; Org -> Date
  "m d"   '(:ignore t    :wk "Date")
  "m d d" '(org-deadline :wk "Deadline")
  "m d s" '(org-schedule :wk "Schedule")
  ;; Org -> Export
  "m e"   '(:ignore t                 :wk "Export")
  "m e a" '(org-ascii-export-to-ascii :wk "ASCII (text)")
  "m e h" '(org-html-export-to-html   :wk "HTML")
  "m e l" '(org-latex-export-to-latex :wk "LaTeX")
  "m e p" '(org-latex-export-to-pdf   :wk "PDF")
  ;; Org -> Link
  "m l"   '(:ignore t             :wk "Link")
  "m l l" '(x8dcc/org-insert-link :wk "Insert")
  "m l s" '(org-store-link        :wk "Store")
  ;; Org -> Priority
  "m p"   '(:ignore t         :wk "Priority")
  "m p d" '(org-priority-down :wk "Decrease")
  "m p p" '(org-priority      :wk "Insert")
  "m p u" '(org-priority-up   :wk "Increase")
  ;; Org -> Todo
  "m t"   '(org-todo :wk "Toggle todo")
  ;; Org -> Tangle
  "m T"   '(org-babel-tangle :wk "Tangle current file"))

(x8dcc/latex-keys
  ;; Mode (LaTeX)
  "m"     '(:ignore t           :wk "LaTeX")
  "m c"   '(x8dcc/latex-compile :wk "Compile to PDF")
  "m b"   '(latex-insert-block  :wk "Open block")
  "m B"   '(latex-close-block   :wk "Close block"))

(x8dcc/lisp-keys
  ;; Evaluate
  "e"   '(:ignore t            :wk "Evaluate")
  "e e" '(eval-last-sexp       :wk "Last sexp")
  "e E" '(eval-print-last-sexp :wk "Print last sexp")
  "e b" '(eval-buffer          :wk "Current buffer")
  "e r" '(eval-region          :wk "Region"))

(x8dcc/c-keys
  ;; Buffer
  "b f" '(clang-format-buffer :wk "Format")
  ;; Insert
  "i g" '(x8dcc/c-include-guard :wk "Include guards")
  ;; Evaluate
  "e"   '(:ignore t      :wk "Evaluate")
  "e m" '(c-macro-expand :wk "Expand macros in region")
  ;; Fold
  "z i" '(hide-ifdef-mode :wk "Unused ifdefs"))

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

(defun x8dcc/mode-line-region-chars (prefix middle subfix)
  "If there are characters in the selection, return a string with the number of
characters and lines, between the PREFIX and SUBFIX. If the region takes up more
than one line, it will also display the MIDDLE argument right after the number
of characters, followed by the number of lines."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end   (region-end))
             (chars (abs (- end start)))
             (lines (abs (- (line-number-at-pos end)
                            (line-number-at-pos start)))))
        (concat prefix
                (number-to-string (+ chars 1))
                (if (> lines 0)
                    (concat middle (number-to-string (+ lines 1))))
                subfix))))

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
                (:eval (x8dcc/mode-line-region-chars "(Sel " " L" ") "))
                "  "
                mode-line-modes
                (vc-mode vc-mode)
                "  "
                mode-line-misc-info))

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative
      display-line-numbers-width-start t)

(x8dcc/hook-to-targets (lambda () (display-line-numbers-mode 0))
                       '(eshell-mode-hook
                         doc-view-mode-hook
                         image-mode-hook
                         ;; Games
                         solitaire-mode-hook
                         mpuz-mode-hook
                         bubbles-mode-hook
                         gomoku-mode-hook
                         snake-mode-hook
                         tetris-mode-hook))

(add-hook 'prog-mode-hook 'hl-line-mode)

(setq hl-line-sticky-flag nil)

(setq-default truncate-lines t)
(global-visual-line-mode 0)

(setq-default display-fill-column-indicator-character ?\u00A6
              fill-column 80)

(add-hook 'prog-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode 1)))

(setq org-agenda-start-on-weekday 1
      calendar-week-start-day 1
      org-agenda-weekend-days '(6 0)
      calendar-weekend-days '(6 0))

(setq vc-follow-symlinks t)

(global-auto-revert-mode 1)

(save-place-mode 1)

(savehist-mode 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default indent-line-function #'indent-relative-first-indent-point)

(setq tab-always-indent nil)

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "trash"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "trash") t)))

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(with-eval-after-load 'battery
  (if (not (null battery-status-function))
      (let ((power-source (battery-format "%L" (funcall battery-status-function)))
            (power-status (battery-format "%B" (funcall battery-status-function))))
        (if (or (string= "N/A" power-source)
                (string= "unknown" power-source)
                (string= "N/A" power-status)
                (string= "unknown" power-status))
            (display-battery-mode 0)
          (display-battery-mode 1)))))

(require 'battery)

(setq eshell-prompt-function (lambda ()
                               (concat
                                (abbreviate-file-name (eshell/pwd))
                                (propertize " λ" 'face '(:foreground "#8490B3"))
                                (propertize " " 'face '(:inherit default))))
      eshell-prompt-regexp "^[^#λ]* [#λ] ")

(defun x8dcc/eshell-project-or-current (&optional eshell-func)
  "Run ESHELL-FUNC in the project's root whenever possible."
  (interactive)
  (unless eshell-func (setq eshell-func #'eshell))
  (if (projectile-project-p)
      (projectile-with-default-dir (projectile-acquire-root)
        (funcall eshell-func))
    (funcall eshell-func)))

(defun x8dcc/eshell-numbered (&optional eshell-func)
  "Call `x8dcc/eshell-project-or-current' with ESHELL-FUNC. If this was not the
first *eshell* buffer, append the count to the buffer name.

Uses `x8dcc/get-buffer-count' for getting the number of eshell buffers."
  (interactive)
  (unless eshell-func (setq eshell-func #'eshell))
  (let* ((eshell-buffer-num (x8dcc/get-buffer-count "\\*eshell\\*"))
         (eshell-buffer-name (if (> eshell-buffer-num 0)
                                 (concat "*eshell* ["
                                         (number-to-string eshell-buffer-num)
                                         "]")
                               "*eshell*")))
    (x8dcc/eshell-project-or-current eshell-func)))

(defun x8dcc/eshell-popup (&optional buffer-name)
  "Create or open a popup eshell buffer.

Creates a new eshell buffer with the specified BUFFER-NAME, or
\"*eshell-popup*\" if omited. Depending on `projectile-project-p', it will call
`eshell' in the project root or in the current folder. Useful for setting
different rules in `display-buffer-alist'."
  (interactive)
  (unless buffer-name (setq buffer-name "*eshell-popup*"))
  (let ((eshell-buffer-name buffer-name))
    (x8dcc/eshell-project-or-current)))

(add-to-list 'display-buffer-alist
             '("\\*eshell-popup\\*"
               (display-buffer-in-side-window (side . bottom))))

(setq dired-listing-switches "-l --all --sort=version --group-directories-first --human-readable")

(setq dired-recursive-copies  'top
      dired-recursive-deletes 'top)

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(setq lazy-highlight-cleanup t
      lazy-highlight-initial-delay 2
      lazy-highlight-max-at-a-time nil)

(setq isearch-allow-scroll t
      search-whitespace-regexp ".{,10}")

(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-subfix-format nil)

(require 'erc)
(require 'erc-log)
(require 'erc-stamp)
(require 'erc-track)
;(require 'erc-spelling)

(erc-log-enable)
(erc-stamp-enable)
(erc-track-enable)
;(erc-spelling-enable)

(advice-add 'erc :override #'erc-tls)

(setq erc-nick           "x8dcc"
      erc-system-name    "x8dcc"
      erc-user-full-name "x8dcc"

      ;; Don't give away machine name
      erc-anonymous-login t
      ;; Don't reply to CTCP
      erc-disable-ctcp-replies t
      ;; Notify CTCP requests
      erc-paranoid t
      ;; Warn blank lines
      erc-warn-about-blank-lines t

      ;; Enable logging
      erc-enable-logging t
      ;; Directory for logs
      erc-log-channels-directory (concat user-emacs-directory "erc-log")
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
      ;; Prompt at the bottom of the screen
      erc-scrolltobottom-mode t
      erc-input-line-position -1
      ;; Messages to mode-line
      erc-track-showcount t
      erc-track-exclude-list '("NICK" "JOIN" "PART" "QUIT" "333" "353")

      ;; Don't bury ERC buffers by default
      erc-join-buffer 'buffer
      ;; Kill buffers for channels after /part
      erc-kill-buffer-on-part t
      ;; Kill buffers for private queries after quitting the server
      erc-kill-queries-on-quit t
      ;; Kill buffers for server messages after quitting the server
      erc-kill-server-buffer-on-quit t)

(setq erc-prompt (lambda ()
                   (concat "[" (buffer-name) "]:")))

(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      send-mail-function 'smtpmail-send-it)

(setq epg-pinentry-mode 'loopback)

(require 'org-tempo)

(let ((expanded-org-directory (expand-file-name "~/Sync/Org/")))
  (if (file-directory-p expanded-org-directory)
      (setq org-directory expanded-org-directory
            org-agenda-files (list (concat org-directory "agenda.org")))))

(setq org-clock-sound (concat org-directory "my-media/notification.wav"))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (scheme . t)))

(setq org-html-postamble nil
      org-export-time-stamp-file nil)

(setq org-latex-hyperref-template
      "\\hypersetup{
          pdfauthor={%a},
          pdftitle={%t},
          pdfkeywords={%k},
          pdfsubject={%d},
          pdfcreator={%c},
          pdflang={%L},
          colorlinks=true
       }\n")

(setq org-fontify-quote-and-verse-blocks t
      org-src-fontify-natively t
      org-hide-emphasis-markers t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t)

(setq org-startup-folded "nofold")

(add-hook 'org-mode-hook (lambda ()
                           (org-indent-mode 1)
                           (setq org-link-descriptive 1)
                           (setq-local
                            electric-pair-inhibit-predicate
                            (lambda (c)
                              (if (char-equal c ?<)
                                  t
                                (electric-pair-default-inhibit c))))))

(setq org-highlight-latex-and-related '(latex entities))

(defun x8dcc/make-invisible (regex &optional group-num)
  "Make all ocurrences of REGEX invisible.

Searches all ocurrences of REGEX and adds them to an invisible overlay. If
GROUP-NUM is supplied, it will only add the N-th parentheses group of the regex
to the overlay."
  (interactive "sRegex: ")
  (unless group-num (setq group-num 0))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regex nil t)
      (let ((invisible-overlay (make-overlay (match-beginning group-num) (match-end group-num))))
        (overlay-put invisible-overlay 'invisible t)))))

;; NOTE: For hiding org commas, use:
;; (x8dcc/make-invisible "^\\s*\\(,\\)\\*" 1)

(defun x8dcc/org-headline-to-id (headline)
  "Converts an org-mode HEADLINE to a CUSTOM-ID dashed string. For example:
\"My test... =heading=\" would turn into \"my-test-heading\"."
  (setq headline (replace-regexp-in-string "\\(\"+\\|'+\\)" ""
                                           (downcase headline)))
  (setq headline (replace-regexp-in-string "[^[:alnum:]]+" "-"
                                           headline))
  (setq headline (replace-regexp-in-string "\\(^-+\\|-+$\\)" ""
                                           headline))
  headline)

(defun x8dcc/org-custom-id-get (&optional pom create)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.  If POM is
nil, refer to the entry at point. If the entry does not have a CUSTOM_ID, the
function returns nil. However, when CREATE is non nil, create a CUSTOM_ID if
none is present already.

In any case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID"))
          (headline (nth 4 (org-heading-components))))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (x8dcc/org-headline-to-id headline))
        (org-entry-put pom "CUSTOM_ID" id)
        id)))))

(defun x8dcc/org-custom-id-add-all ()
  "Add CUSTOM_ID properties to all headlines in the current file which do not
already have one. See `x8dcc/org-custom-id-get'."
  (interactive)
  (org-map-entries (lambda () (x8dcc/org-custom-id-get (point) 'create))))

(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "notes.org" "Notes")
         "* %T Note\n%?")
        ("s" "Selection" entry
         (file+headline "notes.org" "Selections")
         "* %T Selection from [[%F][%f]]\n%?\n#+begin_quote\n%i\n#+end_quote")))

(defun x8dcc/org-insert-header (&optional title)
  (interactive)
  (unless title
    (setq title (capitalize (file-name-base buffer-file-name))))
  (save-excursion
    (goto-char (point-min))
    (insert "#+TITLE: " title "\n"
            "#+AUTHOR: " user-full-name "\n"
            "#+OPTIONS: toc:2\n"
            "#+STARTUP: nofold\n")))

(defun x8dcc/latex-compile ()
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file))

(setq c-default-style "k&r"
      c-basic-offset tab-width
      c-tab-always-indent nil)

(setq c-doc-comment-style 'doxygen)

(setq hide-ifdef-initially t
      hide-ifdef-lines t)

(with-eval-after-load 'find-file
  (setq-default ff-quiet-mode t)
  (dolist (path '("./include" ".."))
    (add-to-list 'cc-search-directories path)))

(defun x8dcc/c-include-guard (&optional filename)
  (interactive)
  (unless filename
    (setq filename (file-name-base buffer-file-name)))
  (let ((macro-name (upcase (concat filename "_H_" ))))
    (save-excursion
      (goto-char (point-min))
      (insert "\n"
              "#ifndef " macro-name "\n"
              "#define " macro-name " 1\n")
      (goto-char (point-max))
      (insert "\n"
              "#endif /* " macro-name " */"))))

(setq gdb-many-windows t)

(setq gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)
