(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(load-theme 'x8dcc-modus-vivendi)

(setq inhibit-startup-message t)

(setq fancy-splash-image (concat user-emacs-directory "my-media/splash.png"))

(when (member "Dina" (font-family-list))
    (set-frame-font "Dina 8" t nil))

(when (member "Cozette" (font-family-list))
  (set-fontset-font t 'unicode "Cozette"))

(when (member "Fira Sans" (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family "Fira Sans"
                      :height 100
                      :weight 'regular))

(when (member "Dina" (font-family-list))
  (set-face-attribute 'fixed-pitch nil
                      :family "Dina"))

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
        evil-jumps-cross-buffers t
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

(use-package undohist
  :init
  (undohist-initialize)
  :config
  (setq undohist-ignored-files
        '("\\.gpg\\'"
          x8dcc/is-git-commit-filename
          file-remote-p)))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer x8dcc/def-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/def-keys-org
    :states '(normal insert visual emacs)
    :keymaps 'org-mode-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/def-keys-latex
    :states '(normal insert visual emacs)
    :keymaps 'TeX-mode-map
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/def-keys-lisp
    :states '(normal insert visual emacs)
    :keymaps '(lisp-mode-map
               emacs-lisp-mode-map
               lisp-interaction-mode-map
               lisp-data-mode-map)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/def-keys-c
    :states '(normal insert visual emacs)
    :keymaps '(c-mode-map
               c++-mode-map
               java-mode-map
               js-mode-map)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (general-create-definer x8dcc/def-keys-message
    :states '(normal insert visual emacs)
    :keymaps 'message-mode-map
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

(use-package consult
  :config
  (setq consult-preview-key (list :debounce 0.5 'any))
  (setq completion-in-region-function
		(lambda (&rest args)
          (apply (if vertico-mode
					 #'consult-completion-in-region
                   #'completion--in-region)
				 args))))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'consult-xref
        xref-show-xrefs-function #'consult-xref
        xref-prompt-for-identifier nil))

(use-package popper
  :config
  (setq popper-group-function #'popper-group-by-projectile
        popper-reference-buffers '(compilation-mode
                                   messages-buffer-mode
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
                                   "^\\*xref\\*"))
  (let ((popper-mode-line-formatted (propertize " *POP* " 'face 'bold)))
    (setq popper-mode-line popper-mode-line-formatted))
  (popper-mode 1))

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
                        font-lock-type-face
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

(straight-use-package
 '(move-text :type git :host github :repo "8dcc/move-text"))

(move-text-default-bindings)
(add-hook 'text-mode-hook #'move-text-mode)
(add-hook 'prog-mode-hook #'move-text-mode)

(keymap-set move-text-mode-map "M-j" 'move-text-down)
(keymap-set move-text-mode-map "M-k" 'move-text-up)

(use-package hl-todo
  :hook ((org-mode prog-mode LaTeX-mode) . hl-todo-mode)
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
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          LaTeX-mode)
         . rainbow-delimiters-mode))

(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package clang-format
  :config
  (setq clang-format-style "file"))

(use-package htmlize)

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

(use-package evil-lion
  :config
  (setq evil-lion-left-align-key (kbd "g a"))
  (setq evil-lion-right-align-key (kbd "g A"))
  (evil-lion-mode))

(straight-use-package
 '(big-font :type git :host github :repo "8dcc/big-font.el"))

(setq big-font-height 120
      big-font-family-alist '((default     . "Iosevka Comfy Fixed")
                              (fixed-pitch . "Iosevka Comfy Fixed")))

(use-package auctex)

(straight-use-package
 '(nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

(add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))

(straight-use-package
 '(beardbolt :type git :host github :repo "8dcc/beardbolt"))

(setq beardbolt-shuffle-rainbow t
      beardbolt-compile-delay nil)

(use-package x86-lookup
  :config
  (setq x86-lookup-pdf
        (concat user-emacs-directory "my-media/intel-sdm-vol2.pdf")
        x86-lookup-browse-pdf-function
        (lambda (pdf page)
          (start-process "firefox" nil "firefox"
                         (format "file://%s#page=%d" pdf page)))))

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

(defun x8dcc/count-matching-buffers (regexp)
  "Return the number of buffers whose name matches REGEXP."
  (length
   (seq-remove (lambda (buffer)
                 (not (string-match-p regexp
                                      (buffer-name buffer))))
               (buffer-list))))

(defun x8dcc/is-huge-file ()
  "Returns `t' if the current buffer has either too many characters (>500000),
or too many lines (>10000)."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(require 'git-commit)
(defun x8dcc/is-git-commit-filename (filename)
  "Returns t if FILENAME matches `git-commit-filename-regexp'."
  (string-match-p git-commit-filename-regexp filename))

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

(defun x8dcc/increment-number-at-point (&optional increment)
  "Increment the number at point by INCREMENT."
  (interactive "*p")
  (let ((pos (point)))
    (save-match-data
      (skip-chars-backward "0-9")
      (if (looking-at "[0-9]+")
          (let ((field-width (- (match-end 0) (match-beginning 0)))
                (newval (+ (string-to-number (match-string 0) 10) increment)))
            (when (< newval 0)
              (setq newval (+ (expt 10 field-width) newval)))
            (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                   newval)))
        (user-error "No number at point")))
    (goto-char pos)))

(defun x8dcc/increment-number-at-point-hex (&optional increment)
  "Increment the number forward from point by INCREMENT."
  (interactive "*p")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if increment increment 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))

(defun x8dcc/toggle-final-newline ()
  "Toggle newline insertion when saving the current buffer. See
`require-final-newline'."
  (interactive)
  (setq-local require-final-newline (not require-final-newline))
  (if require-final-newline
      (message "Final newline enabled in the current buffer.")
    (message "Final newline disabled in the current buffer.")))

(defun x8dcc/sudo-shell-command (command)
"Run the specified shell command as root, asking for the sudo password in the
minibuffer. Only the shell command is saved in the history.

See also `shell-command'."
  (interactive
   (list (read-shell-command "Shell command: " nil nil)))
  (shell-command (concat "echo "
                         (shell-quote-argument (read-passwd "[sudo] Password: "))
                         " | sudo -S "
                         command)))

(defun x8dcc/evil-kill-buffer-and-window ()
  "Kill the current buffer with `kill-current-buffer' and delete the current
window with `evil-delete-window'."
  (interactive)
  (kill-current-buffer)
  (evil-window-delete))

(defun x8dcc/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun x8dcc/indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

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

(keymap-global-set "C-<backspace>" #'x8dcc/backward-delete-word)

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

(with-eval-after-load 'cc-mode
  (add-hook 'c-mode-hook
            (lambda ()
              (keymap-set c-mode-map "RET" #'c-context-line-break))))

(x8dcc/def-keys
  "SPC" '(projectile-find-file :wk "Find file in project") ; Same as "SPC p f"
  "."   '(find-file            :wk "Find file")            ; Same as "SPC f f"
  ";"   '(comment-dwim         :wk "Comment (DWIM)")
  ;; Buffer
  "b"   '(:ignore t                        :wk "Buffer")
  "b i" '(x8dcc/indent-buffer              :wk "Indent")
  "b k" '(kill-current-buffer              :wk "Kill current")
  "b l" '(switch-to-buffer                 :wk "Switch to")
  "b L" '(buffer-menu                      :wk "List")
  "b s" '(basic-save-buffer                :wk "Save")
  "b S" '(write-file                       :wk "Save as")
  "b r" '(revert-buffer                    :wk "Revert (discard)")
  "b R" '(revert-buffer-with-coding-system :wk "Revert with coding system")
  ;; File
  "f"   '(:ignore t          :wk "File")
  "f c" '(compile            :wk "Compile")
  "f C" '(recompile          :wk "Re-compile")
  "f f" '(find-file          :wk "Find file")
  "f F" '(find-name-dired    :wk "Find wildcard recursively")
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
  "h c" '(describe-char           :wk "Describe char")
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
  ;; Jump
  "j"   '(:ignore t             :wk "Jump")
  "j i" '(consult-imenu         :wk "Imenu")
  "j j" '(evil-jump-backward    :wk "Undo buffer jump")
  "j J" '(evil-jump-forward     :wk "Redo buffer jump")
  "j d" '(xref-find-definitions :wk "Definitions")
  "j x" '(xref-find-references  :wk "X-refs")
  ;; Mode
  "m"   '(:ignore t :wk "Mode")
  ;; Open
  "o"   '(:ignore t             :wk "Open")
  "o ." '(dired-jump            :wk "Dired")
  "o !" '(shell-command         :wk "Shell command")
  "o a" '(org-agenda            :wk "Org agenda")
  "o c" '(calc                  :wk "Calculator")
  "o C" '(quick-calc            :wk "Quick calculator")
  "o d" '(projectile-run-gdb    :wk "Debugger")
  "o e" '(x8dcc/eshell-popup    :wk "Eshell popup")
  "o E" '(x8dcc/eshell-numbered :wk "Eshell")
  "o m" '(man                   :wk "Manpage")
  "o x" '(scratch-buffer        :wk "Scratch buffer")
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
  "t i" '(toggle-case-fold-search            :wk "Case-sensitive searches")
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
  "w"   '(:ignore t                         :wk "Window")
  "w c" '(evil-window-delete                :wk "Close")
  "w C" '(x8dcc/evil-kill-buffer-and-window :wk "Kill buffer and window")
  "w h" '(evil-window-left                  :wk "Left")
  "w H" '(evil-window-move-far-left         :wk "Move current left")
  "w j" '(evil-window-down                  :wk "Down")
  "w J" '(evil-window-move-very-bottom      :wk "Move current down")
  "w k" '(evil-window-up                    :wk "Up")
  "w K" '(evil-window-move-very-top         :wk "Move current up")
  "w l" '(evil-window-right                 :wk "Right")
  "w L" '(evil-window-move-far-right        :wk "Move current right")
  "w s" '(evil-window-split                 :wk "Split horizontally")
  "w v" '(evil-window-vsplit                :wk "Split vertically")
  "w w" '(evil-window-next                  :wk "Next")
  ;; Fold
  "z"   '(:ignore t        :wk "Fold")
  "z a" '(evil-toggle-fold :wk "Toggle")
  "z c" '(evil-close-fold  :wk "Close")
  "z m" '(evil-close-folds :wk "Close all")
  "z o" '(evil-open-fold   :wk "Open")
  "z r" '(evil-open-folds  :wk "Open all"))

(x8dcc/def-keys-org
  ;; Mode (Org)
  "m i"   '(org-toggle-inline-images :wk "Toggle inline images")
  "m h"   '(x8dcc/org-insert-header  :wk "Insert default header")
  "m t"   '(org-todo                 :wk "Toggle todo")
  "m T"   '(org-babel-tangle         :wk "Tangle current file")
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
  "m p u" '(org-priority-up   :wk "Increase"))

(x8dcc/def-keys-latex
  ;; Mode (LaTeX)
  "m c"   '(x8dcc/latex-compile   :wk "Compile to PDF")
  "m b"   '(latex-insert-block    :wk "Open block")
  "m B"   '(latex-close-block     :wk "Close block")
  "m m"   '(TeX-insert-macro      :wk "Insert macro")
  "m p"   '(prettify-symbols-mode :wk "Prettify symbols")
  "m s"   '(LaTeX-section         :wk "New section")
  ;; Folding
  "m f"   '(:ignore t       :wk "Fold")
  "m f f" '(TeX-fold-dwim   :wk "DWIM")
  "m f b" '(TeX-fold-buffer :wk "Entire buffer")
  ;; Text format
  "m F"   '(:ignore t                   :wk "Text format")
  "m F b" '(x8dcc/latex-font-bold       :wk "Bold")
  "m F c" '(x8dcc/latex-font-smallcaps  :wk "Smallcaps")
  "m F e" '(x8dcc/latex-font-emphasized :wk "Emphasized")
  "m F i" '(x8dcc/latex-font-italics    :wk "Italics")
  "m F r" '(x8dcc/latex-font-roman      :wk "Roman")
  "m F s" '(x8dcc/latex-font-slanted    :wk "Slanted")
  "m F t" '(x8dcc/latex-font-typewriter :wk "Typewriter"))

(x8dcc/def-keys-lisp
  ;; Evaluate
  "e"   '(:ignore t            :wk "Evaluate")
  "e e" '(eval-last-sexp       :wk "Last sexp")
  "e E" '(eval-print-last-sexp :wk "Print last sexp")
  "e b" '(eval-buffer          :wk "Current buffer")
  "e r" '(eval-region          :wk "Region"))

(x8dcc/def-keys-c
  ;; Buffer
  "b f" '(clang-format-buffer :wk "Format")
  ;; Mode (C)
  "m a" '(c-toggle-auto-newline       :wk "Toggle auto-newline")
  "m d" '(x8dcc/beardbolt-disassemble :wk "Beardbolt disassemble")
  "m h" '(c-toggle-hungry-state       :wk "Toggle hungry-delete-key")
  "m i" '(x8dcc/c-include-guard       :wk "Insert include guards")
  "m I" '(hide-ifdef-mode             :wk "Toggle visibility of unused ifdefs")
  "m l" '(c-toggle-electric-state     :wk "Toggle electric indentation")
  "m m" '(c-macro-expand              :wk "Expand macros in region"))

(x8dcc/def-keys-message
  ;; Mode (C)
  "m e" '(mml-secure-message-sign-encrypt :wk "Sign and encrypt")
  "m p" '(mml-preview                     :wk "Preview")
  "m s" '(mml-secure-message-sign         :wk "Sign")
  "m S" '(message-send                    :wk "Send"))

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

(blink-cursor-mode 0)

(setq-default display-fill-column-indicator-character ?\u00A6
              fill-column 80)

(add-hook 'prog-mode-hook
          (lambda ()
            (display-fill-column-indicator-mode 1)))

(setq org-agenda-start-on-weekday 1
      calendar-week-start-day 1
      org-agenda-weekend-days '(6 0)
      calendar-weekend-days '(6 0))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

(save-place-mode 1)

(savehist-mode 1)

(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "trash"))))

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "trash") t)))

(setq backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq-default indent-line-function #'indent-relative-first-indent-point)

(setq tab-always-indent nil)

(transient-mark-mode 0)

(defvar x8dcc/allow-modify-on-save t
  "If non-nil, allow the calling of functions that modify the buffer contents on
the save hooks.")

(defun x8dcc/toggle-modify-on-save ()
  "Toggle modifications on buffer save hooks. See `x8dcc/allow-modify-on-save'."
  (interactive)
  (setq x8dcc/allow-modify-on-save (not x8dcc/allow-modify-on-save))
  (if x8dcc/allow-modify-on-save
      (message "Buffer modifications enabled on save.")
    (message "Buffer modifications disabled on save.")))

(add-hook 'before-save-hook
          (lambda ()
            (if x8dcc/allow-modify-on-save
                (delete-trailing-whitespace))))

(setq printer-name "MainPrinter")

(defun x8dcc/lpr-buffer-pages (start end)
  "Print the current buffer using `lpr-buffer' from page START to END. The page
numbers start at 1."
  (interactive "nStarting page: \nnEnd page: ")
  (let ((lpr-switches (list "-o" (format "page-ranges=%d-%d" (max start 1) (max end 1)))))
    (lpr-buffer)))

(global-auto-revert-mode 1)

(setq vc-follow-symlinks t)

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))

(setq eshell-prompt-function (lambda ()
                               (concat
                                (abbreviate-file-name (eshell/pwd))
                                (propertize " λ" 'face '(:foreground "#8490B3"))
                                (propertize " " 'face '(:inherit default))))
      eshell-prompt-regexp "^[^#λ]* [#λ] ")

(setq eshell-hist-ignoredups t)

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

Uses `x8dcc/count-matching-buffers' for getting the number of eshell buffers."
  (interactive)
  (unless eshell-func (setq eshell-func #'eshell))
  (let* ((eshell-buffer-num (x8dcc/count-matching-buffers "\\*eshell\\*"))
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

(setq mml-secure-openpgp-sign-with-sender t)

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

(setq org-export-with-smart-quotes t)

(setq org-latex-title-command "\\maketitle\\clearpage"
      org-latex-toc-command "\\tableofcontents\\clearpage\n")

(setq org-latex-hyperref-template
      "\\hypersetup{
          pdfauthor={%a},
          pdftitle={%t},
          pdfkeywords={%k},
          pdfsubject={%d},
          pdflang={%L},
          hidelinks
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

(setq TeX-parse-self t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

(setq TeX-fold-unfold-around-mark t)

(defun x8dcc/latex-compile ()
  (interactive)
  (TeX-command "LaTeX" 'TeX-master-file))

(defun x8dcc/tex-get-font-key (key &optional font-list)
  "Find the font key in FONT-LIST for the font whose LaTeX command contains
KEY. Returns nil if the KEY is not found, or a valid font key that can be passed
to `TeX-font'. If FONT-LIST is nil, `TeX-font-list' is used."
  (unless font-list (setq font-list TeX-font-list))
  (let ((item (car font-list)))
    (cond ((string-match-p (regexp-quote key) (cadr item))
           (car item))
          ((cdr font-list)
           (x8dcc/tex-get-font-key key (cdr font-list)))
          (t nil))))

(defun x8dcc/latex-font-bold ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "bf{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-emphasized ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "emph{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-italics ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "it{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-roman ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "rm{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-smallcaps ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "sc{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-slanted ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "sl{")))
    (if key (TeX-font nil key))))
(defun x8dcc/latex-font-typewriter ()
  (interactive)
  (let ((key (x8dcc/tex-get-font-key "tt{")))
    (if key (TeX-font nil key))))

(defun LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.

  \"\\item\" is indented `LaTeX-indent-level' spaces relative to
  the the beginning of the environment.

  Continuation lines are indented either twice
  `LaTeX-indent-level', or `LaTeX-indent-level-item-continuation'
  if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp 'LaTeX-indent-level-item-continuation)
                            LaTeX-indent-level-item-continuation)
                       (* 2 LaTeX-indent-level)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            (t
             (+ contin indent))))))

(defcustom LaTeX-indent-level-item-continuation 4
  "*Indentation of continuation lines for items in itemize-like
environments."
  :group 'LaTeX-indentation
  :type 'integer)

(eval-after-load "latex"
  '(setq LaTeX-indent-environment-list
         (nconc '(("itemize" LaTeX-indent-item)
                  ("enumerate" LaTeX-indent-item)
                  ("description" LaTeX-indent-item))
                LaTeX-indent-environment-list)))

(c-add-style "x8dcc/c-style"
             `("k&r"
               (c-basic-offset . ,tab-width)
               (c-comment-only-line-offset . 0)
               (c-doc-comment-style . doxygen)
               (c-offsets-alist
                (c . c-lineup-C-comments)
                (string . -1000)
                (defun-open . 0)
                (defun-close . 0)
                (defun-block-intro . +)
                (class-open . 0)
                (class-close . 0)
                (inline-open . 0)
                (inline-close . 0)
                (func-decl-cont . +)
                (knr-argdecl-intro . +)
                (knr-argdecl . 0)
                (topmost-intro . 0)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (member-init-intro . +)
                (member-init-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inher-cont . c-lineup-multi-inher)
                (block-open . 0)
                (block-close . 0)
                (brace-list-open . 0)
                (brace-list-close . 0)
                (brace-list-intro . +)
                (brace-list-entry . 0)
                (brace-entry-open . 0)
                (statement . 0)
                (statement-cont . +)
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-case-open . 0)
                (substatement . +)
                (substatement-open . 0)
                (substatement-label . -1000)
                (case-label . +)
                (access-label . -)
                (label . -1000)
                (do-while-closure . 0)
                (else-clause . 0)
                (catch-clause . 0)
                (comment-intro . c-lineup-comment)
                (arglist-intro . +)
                (arglist-cont c-lineup-gcc-asm-reg 0)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-close . c-lineup-close-paren)
                (stream-op . c-lineup-streamop)
                (inclass . +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (cpp-define-intro c-lineup-cpp-define +) ; NOTE: Currently not documented
                (friend . 0)
                (extern-lang-open . 0)
                (extern-lang-close . 0)
                (inextern-lang . +)
                (namespace-open . 0)
                (namespace-close . 0)
                (innamespace . +)
                (module-open . 0)
                (module-close . 0)
                (inmodule . +)
                (composition-open . 0)
                (composition-close . 0)
                (incomposition . +)
                (template-args-cont c-lineup-template-args +)
                (inlambda . 0)
                (lambda-intro-cont . +)
                (inexpr-statement . +)
                (inexpr-class . +))))

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "x8dcc/c-style")))

(setq c-tab-always-indent nil)

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

(defun x8dcc/beardbolt-disassemble ()
  (interactive)
  (beardbolt-mode 1)
  (call-interactively #'beardbolt-compile))

(setq gdb-many-windows t)

(setq gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)

(setq compilation-scroll-output 'first-error)

(setq compilation-always-kill t)
