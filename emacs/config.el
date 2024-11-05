(setq user-full-name "8dcc"
      user-mail-address "8dcc.git@gmail.com")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(load-theme 'modux-vivendi)

(setq inhibit-startup-message t)

(setq fancy-splash-image (concat user-emacs-directory "my-media/splash.png"))

(when (member "Dina" (font-family-list))
    (set-frame-font "Dina 8" t nil))

(when (member "Cozette" (font-family-list))
  (set-fontset-font t 'unicode "Cozette"))

(when (member "FreeSerif" (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family "FreeSerif"
                      :height 110
                      :weight 'regular))

(when (member "Dina" (font-family-list))
  (set-face-attribute 'fixed-pitch nil
                      :family "Dina"))

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
  (dolist (mode '(abbrev-mode
                  visual-line-mode))
    (diminish mode)))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system #'undo-redo
        evil-want-C-i-jump nil
        evil-jumps-cross-buffers t
        evil-lookup-func (lambda () (man (Man-default-man-entry)))
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

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-ignore-encrypted-files t
        undo-fu-session-ignore-temp-files t))

(defmacro x8dcc/general-create-definer (name keymaps)
  "Create a general definer named NAME for the specified KEYMAPS.

Used in normal, insert, visual and emacs states. The normal prefix is \"SPC\"
and the non-normal prefix is \"M-SPC\"."
  `(general-create-definer ,name
     :states '(normal insert visual emacs)
     :keymaps ,keymaps
     :prefix "SPC"
     :non-normal-prefix "M-SPC"))

(defun x8dcc/general-create-definers (alist)
  "Create definers for all elements of ALIST, using
`x8dcc/general-create-definer'."
  (dolist (element alist)
    ;; We need to use `eval' because macro arguments are not evaluated.
    ;; FIXME: Use a better approach than `eval' and `backquote'.
    (eval `(x8dcc/general-create-definer ,(car element)
                                         (quote ,(cdr element))))))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (x8dcc/general-create-definers
   '((x8dcc/def-keys . override)
     (x8dcc/def-keys-org . org-mode-map)
     (x8dcc/def-keys-latex . LaTeX-mode-map)
     (x8dcc/def-keys-texinfo . Texinfo-mode-map)
     (x8dcc/def-keys-c . (c-mode-map
                          c++-mode-map
                          java-mode-map
                          js-mode-map))
     (x8dcc/def-keys-diff . diff-mode-map)
     (x8dcc/def-keys-message . message-mode-map)
     (x8dcc/def-keys-rmail . rmail-mode-map)
     (x8dcc/def-keys-rmail-summary . rmail-summary-mode-map))))

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

(use-package with-editor
  :hook (eshell-mode . with-editor-export-git-editor))

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
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package popper
  :config
  (setq popper-group-function #'popper-group-by-projectile)

  (setq popper-reference-buffers nil)
  (dolist (element `(compilation-mode
                     messages-buffer-mode
                     help-mode
                     occur-mode
                     man-mode
                     "*Warnings*"
                     "*Compile-Log*"
                     "*Backtrace*"
                     "*evil-registers*"
                     "*Apropos*"
                     "*scratch*"
                     "*Macroexpansion*"
                     "*xref*"
                     ,shell-command-buffer-name-async))
    (if (stringp element)
        (setq element (concat "^" (regexp-quote element) "$")))
    (add-to-list 'popper-reference-buffers element 'append))

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
    :hook ((text-mode erc-mode) . spell-fu-mode)
    :config
    (add-hook 'spell-fu-mode-hook
              (lambda ()
                (spell-fu-dictionary-add
                 (spell-fu-get-ispell-dictionary "en_US"))
                (spell-fu-dictionary-add
                 (spell-fu-get-ispell-dictionary "es"))))
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

(use-package move-text
  :straight (move-text :type git :host github :repo "8dcc/move-text")
  :hook ((text-mode prog-mode) . move-text-mode)
  :config
  (move-text-default-bindings)
  (keymap-set move-text-mode-map "M-j" 'move-text-down)
  (keymap-set move-text-mode-map "M-k" 'move-text-up))

(use-package hl-todo
  :hook ((org-mode prog-mode LaTeX-mode) . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"))

(use-package rainbow-mode
  :diminish
  :hook ((html-mode css-mode js-mode)  . rainbow-mode))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode
          scheme-mode
          common-lisp-mode
          lisp-mode
          LaTeX-mode)
         . rainbow-delimiters-mode)
  :config
  (setq rainbow-delimiters-max-face-count 6))

(use-package highlight-numbers
  :hook ((prog-mode . highlight-numbers-mode)))

(use-package clang-format
  :config
  (setq clang-format-style "file"))

(use-package htmlize)

(use-package pdf-tools
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode 0)))
  :config
  (pdf-tools-install)
  (keymap-set pdf-view-mode-map "<remap> <evil-end-of-line>" #'ignore)
  (keymap-set pdf-view-mode-map "<remap> <evil-beginning-of-line>"
              (lambda ()
                (interactive)
                (goto-char 0))))

(defun x8dcc/set-lower-bits (n)
  "Return an integer with the N lower bits set."
  (- (ash 1 n) 1))

(defun x8dcc/fringe-helper-rect (name alignment w h)
  "Convert W and H to a bitmap array, and call `define-fringe-bitmap' with NAME
and ALIGNMENT as parameters."
  (define-fringe-bitmap name
    (apply #'vector
           (make-list h (x8dcc/set-lower-bits w)))
    nil nil alignment))

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

(use-package big-font
  :straight (big-font :type git :host github :repo "8dcc/big-font.el")
  :config
  (setq big-font-faces '((default        120 "Source Code Pro")
                         (fixed-pitch    120 "Source Code Pro")
                         (variable-pitch 150 "FreeSerif"))))

(use-package plumber
  :straight (plumber :type git :host github :repo "8dcc/plumber.el"))

(use-package auctex)

(use-package ada-mode
  :straight (ada-mode :type git :host github :repo "8dcc/ada-mode")
  :config
  (dolist (var '(ada-indent ada-use-indent ada-when-indent ada-broken-indent
                 ada-continuation-indent ada-indent-record-rel-type))
    (eval `(setq ,var 4))))

(dolist (extension '("gpr" "ada" "ads" "adb"))
  (add-to-list 'auto-mode-alist (cons (concat "\\." extension "\\'")
                                      'ada-mode)))

(defun x8dcc/ada-filename-to-proc (filename)
  (thread-last
    filename
    (replace-regexp-in-string "-" "_")
    (replace-regexp-in-string "\\..\\{,3\\}\\'" "")))

(ada-define-skeleton x8dcc/skeleton-ada-source
    "Insert a basic Ada source skeleton."
    nil
    '(setq str (skeleton-read "Procedure name: "
                              (x8dcc/ada-filename-to-proc
                               (buffer-name))))
    "with Ada.Text_IO; use Ada.Text_IO;\n\n"
    "procedure " str " is\n"
    "begin\n"
    > _ "\n"
    "end " str ";" \n)

(use-package geiser-guile)

(use-package nasm-mode
  :straight (nasm-mode :type git :host github :repo "8dcc/nasm-mode"))

(add-to-list 'auto-mode-alist '("\\.asm\\'"  . nasm-mode))

(use-package c-eldoc
  :hook ((c-mode . c-turn-on-eldoc-mode)))

(use-package x86-lookup
  :config
  (setq x86-lookup-pdf
        (concat user-emacs-directory "my-media/intel-sdm-vol2.pdf")
        x86-lookup-browse-pdf-function
        (lambda (pdf page)
          (start-process "firefox" nil "firefox"
                         (format "file://%s#page=%d" pdf page)))))

(use-package beardbolt
  :straight (beardbolt :type git :host github :repo "8dcc/beardbolt")
  :config
  (setq beardbolt-shuffle-rainbow t
        beardbolt-compile-delay nil))

(use-package webpaste
  :straight (webpaste :type git :host github :repo "8dcc/webpaste.el")
  :config
  (setq webpaste-provider-priority '("bpa.st" "dpaste.org" "gist.github.com")
        webpaste-paste-confirmation t))

(defmacro x8dcc/with-current-file (file &rest body)
  "Open FILE in the background, run BODY, and save the file."
  `(with-temp-buffer
     (insert-file-contents ,file)
     ,@body
     (write-region (point-min) (point-max) ,file)))

(defun x8dcc/hook-funcs (target functions)
  "Hook each function in FUNCTIONS to TARGET using `add-hook'."
  (dolist (func functions)
    (add-hook target func)))

(defun x8dcc/hook-to-targets (function targets)
  "Hook FUNCTION to each target in TARGETS using `add-hook'."
  (dolist (target targets)
    (add-hook target function)))

(defun x8dcc/keymaps-set (keymaps key func)
  "Define the KEY string to FUNC in every keymap in the KEYMAPS list."
  (defun eval-keymap (symbol-or-keymap)
    (if (keymapp symbol-or-keymap)
        symbol-or-keymap
      (eval symbol-or-keymap)))
  (dolist (keymap keymaps)
    (keymap-set (eval-keymap keymap) key func))
  func)

(defun x8dcc/keymap-set-alist (keymap key-alist)
  "Define the specified KEY-ALIST in a specific KEYMAP.

Each element in the KEY-ALIST list have the format (KEY . FUNC), and they
represent the first and second arguments of `keymap-set', respectively."
  (defun eval-function (symbol-or-function)
    (if (functionp symbol-or-function)
        symbol-or-function
      (function symbol-or-function)))
  (dolist (key-pair key-alist)
    (keymap-set keymap
                (car key-pair)
                (eval-function (cdr key-pair)))))

(defun x8dcc/alist-insert-before-key (alist new-element key &optional compare-fn)
  "Insert NEW-ELEMENT to ALIST before KEY is found.

If none of the elements of ALIST contains KEY, NEW-ELEMENT is appended to the
end of the ALIST.

The optional argument COMPARE-FN specifies a function with arguments (ELT LIST)
that will be used to check if the KEY matches each element of ALIST. If
COMPARE-FN is nil, the function checks if the `car' of each element of ALIST is
equal to KEY."
  (unless compare-fn
    (setq compare-fn (lambda (elt list)
                       (equal elt (car list)))))
  (cond ((null alist)
         (list new-element))
        ((funcall compare-fn key (car alist))
         (cons new-element alist))
        (t
         (cons (car alist)
               (x8dcc/alist-insert-before-key
                (cdr alist) new-element key compare-fn)))))

(defun x8dcc/replace-regexps-in-string (alist string)
  "Return a copy of STRING with all the regexps in ALIST replaced.

Each element in the ALIST is a replacement with the form (REGEXP . REP), that
will be used for replacing with the `replace-regexp-in-string' function."
  (if (null alist)
      string
    (x8dcc/replace-regexps-in-string
     (cdr alist)
     (replace-regexp-in-string (caar alist)
                               (cdar alist)
                               string))))

(defun x8dcc/count-matching-buffers (regexp)
  "Return the number of buffers whose whole name matches REGEXP.
The REGEXP is wrapped in \"^...$\"."
  (length
   (seq-filter (lambda (buffer)
                 (string-match-p (concat "^" regexp "$")
                                 (buffer-name buffer)))
               (buffer-list))))

(defun x8dcc/suffixed-buffer-name (name)
  "Append suffix to NAME if there is a buffer with that name.
The suffix is a number wrapped in square brackets.

The `x8dcc/count-matching-buffers' function is used to count the number of
buffers with that NAME and, optionally, a suffix. That is, both \"foo\" and
\"foo [4]\" are counted. If there are no buffers with that name, NAME is
returned.

Note that NAME is a normal string, not a regexp."
  (let ((count (x8dcc/count-matching-buffers
                (concat (regexp-quote name)
                        "\\(?: \\[[[:digit:]]+\\]\\)?"))))
    (if (> count 0)
        (concat name " [" (number-to-string count) "]")
      name)))

(defun x8dcc/is-huge-file ()
  "Returns `t' if the current buffer has either too many characters (>500000),
or too many lines (>10000)."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(defun x8dcc/date-tomorrow ()
  "Return a string representing the date for tomorrow."
  (format-time-string "%Y-%m-%d %a"
                      (time-add (current-time)
                                (* 24 60 60))))

(defun x8dcc/separator-comment (&optional max-width)
  "Insert a separator comment in the next line based on `comment-start' and
`comment-end'."
  (interactive "P")
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

(defun x8dcc/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point)
                 (save-excursion
                   (backward-word arg)
                   (point))))

(defun x8dcc/indent-buffer ()
  "Indent the current buffer using `indent-region'."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun x8dcc/unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single logical line.
Opposite of `fill-region'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

(defun x8dcc/evil-kill-buffer-and-window ()
  "Kill the current buffer with `kill-current-buffer' and delete the current
window with `evil-delete-window'."
  (interactive)
  (kill-current-buffer)
  (evil-window-delete))

(evil-define-operator x8dcc/evil-fill-indent (beg end)
  "Fill text to `fill-column' using `evil-fill' and indent it with
`evil-indent'."
  :move-point nil
  :type line
  (save-excursion
    (goto-char beg)
    (let ((fill-column (- fill-column (current-indentation))))
      (indent-region beg end 0)
      (evil-fill beg end)
      (evil-indent beg end))))

(defun x8dcc/tab-move-left (&optional arg)
  "Move the current tab ARG positions to the left."
  (interactive "p")
  (tab-bar-move-tab (- arg)))

(defun x8dcc/tab-move-right (&optional arg)
  "Move the current tab ARG positions to the left."
  (interactive "p")
  (tab-bar-move-tab arg))

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
      (let ((invisible-overlay (make-overlay (match-beginning group-num)
                                             (match-end group-num))))
        (overlay-put invisible-overlay 'invisible t)))))

(defun x8dcc/eshell-clear ()
  "Clear an eshell buffer, and print the prompt.
Alternative to `recenter-top-bottom'."
  (interactive)
  (eshell/clear-scrollback)
  (eshell-emit-prompt))

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

(defun x8dcc/reb-change-syntax (new-syntax)
  "Set `reb-re-syntax' to the specified value. When called interactively, switch
between `read' and `rx'."
  (interactive (list (if (equal reb-re-syntax 'read)
                         'rx
                       'read)))
  (message "Changed re-builder syntax to `%s'" new-syntax)
  (reb-change-syntax new-syntax))

(defconst x8dcc/quick-calc-replacements
  `((,(rx (or line-start space) "0x" (group not-newline)) . "16#\\1")
    (,(rx (or line-start space) "0o" (group not-newline)) . "8#\\1")
    (,(rx (or line-start space) "0b" (group not-newline)) . "2#\\1"))
  "Alist of regexp replacements that should be applied to the input when calling
`x8dcc/quick-calc'.")

(defun x8dcc/quick-calc (input)
  "Replace input according to `x8dcc/quick-calc-replacements', and call
`calc-do-quick-calc'."
  (interactive
   (list (read-string "Quick calc: " nil
                      'calc-quick-calc-history)))
  ;; TODO: Show different bases, like `quick-calc' does.
  (message
   (format "Result: %s"
           (calc-eval
            (x8dcc/replace-regexps-in-string x8dcc/quick-calc-replacements
                                             input)))))

(defun x8dcc/git-add-edit (&optional files)
  "Stage the specified files with Git using \"git add --edit\"."
  (interactive)
  (with-editor* "GIT_EDITOR"
    (vc-git-command nil 'async files "add" "--edit")))

(setq scroll-step 1
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse t
      mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(keymap-global-set "C-+"            #'text-scale-increase)
(keymap-global-set "C--"            #'text-scale-decrease)
(keymap-global-set "C-<wheel-up>"   #'text-scale-increase)
(keymap-global-set "C-<wheel-down>" #'text-scale-decrease)
(keymap-global-set "C-<home>"
                   (lambda ()
                     (interactive)
                     (text-scale-adjust 0)))

(keymap-global-set "<escape>" #'keyboard-escape-quit)

(keymap-global-set "C-S-v" #'yank)

(keymap-global-set "C-<backspace>" #'x8dcc/backward-delete-word)

(keymap-global-set "<backtab>"
                   (lambda ()
                     (interactive)
                     ;; If there is a region active, use `evil-shift-left',
                     ;; otherwise shift the current line.
                     (if (use-region-p)
                         (evil-shift-left (region-beginning) (region-end))
                       (evil-shift-left-line 1))))

(keymap-global-set "<remap> <evil-quit>" #'kill-current-buffer)

(keymap-global-set "<remap> <evil-save-and-close>"
                   (lambda ()
                     (interactive)
                     (basic-save-buffer)
                     (kill-current-buffer)))

(keymap-global-set "<remap> <evil-save-modified-and-close>"
                   (lambda ()
                     (interactive)
                     (message "Ignoring quit keybind...")))

(evil-global-set-key 'normal (kbd "x") #'delete-forward-char)
(evil-global-set-key 'normal (kbd "X") #'delete-backward-char)

(dolist (state '(normal visual motion))
  (evil-global-set-key state (kbd "g W") #'x8dcc/evil-fill-indent))

(define-key input-decode-map [?\C-i]
            (lambda (prompt)
              (if (and (= (length (this-single-command-raw-keys)) 1)
                       (eql (aref (this-single-command-raw-keys) 0) ?\C-i)
                       (bound-and-true-p evil-mode)
                       (eq evil-state 'normal))
                  (kbd "<C-i>")
                (kbd "TAB"))))

(evil-global-set-key 'normal (kbd "<C-i>") #'evil-jump-forward)

(with-eval-after-load 'latex
  (keymap-set LaTeX-mode-map "<remap> <evil-fill>" #'LaTeX-fill-region))

(with-eval-after-load 'eshell
  (x8dcc/keymap-set-alist
   eshell-mode-map
   '(("C-l" . x8dcc/eshell-clear)
     ("<home>" . eshell-bol))))

(with-eval-after-load 'ediff-util
  (add-hook 'ediff-startup-hook
            (lambda ()
              (keymap-set ediff-mode-map
                          "<remap> <evil-quit>"
                          #'ediff-quit))))

(with-eval-after-load 'cc-mode
  (keymap-set c-mode-map
              "RET"
              #'c-context-line-break))

(with-eval-after-load 'rmailsum
  (evil-define-key 'normal rmail-summary-mode-map
    (kbd "RET") #'rmail-summary-goto-msg))

(x8dcc/def-keys
  "SPC" '(projectile-find-file :wk "Find file in project") ; Same as "SPC p f"
  "."   '(find-file            :wk "Find file")            ; Same as "SPC f f"
  ;; Tab
  "TAB"           '(                     :ignore t :wk "Tab")
  "TAB TAB"       '(tab-recent           :wk "Switch to recent")
  "TAB c"         '(tab-close            :wk "Close")
  "TAB l"         '(tab-switch           :wk "Switch to")
  "TAB n"         '(tab-new              :wk "New")
  "TAB r"         '(tab-rename           :wk "Rename")
  "TAB t"         '(tab-bar-mode         :wk "Toggle bar display")
  "TAB <left>"    '(tab-previous         :wk "Switch to previous")
  "TAB <right>"   '(tab-next             :wk "Switch to next")
  "TAB S-<left>"  '(x8dcc/tab-move-left  :wk "Move left")
  "TAB S-<right>" '(x8dcc/tab-move-right :wk "Move right")
  ;; Buffer
  "b"         '(:ignore t                        :wk "Buffer")
  "b b"       '(previous-buffer                  :wk "Switch to previous")
  "b B"       '(next-buffer                      :wk "Switch to next")
  "b c"       '(revert-buffer-with-coding-system :wk "Revert with coding system")
  "b i"       '(x8dcc/indent-buffer              :wk "Indent")
  "b k"       '(kill-current-buffer              :wk "Kill current")
  "b K"       '(kill-buffer                      :wk "Kill other")
  "b l"       '(switch-to-buffer                 :wk "Switch to")
  "b L"       '(buffer-menu                      :wk "List")
  "b s"       '(basic-save-buffer                :wk "Save")
  "b S"       '(write-file                       :wk "Save as")
  "b r"       '(revert-buffer                    :wk "Revert (discard)")
  "b R"       '(rename-buffer                    :wk "Rename")
  "b <left>"  '(previous-buffer                  :wk "Switch to previous")
  "b <right>" '(next-buffer                      :wk "Switch to next")
  ;; Comment
  "c"   '(:ignore t      :wk "Comment")
  "c b" '(comment-box    :wk "Box")
  "c c" '(comment-dwim   :wk "DWIM")
  "c i" '(comment-indent :wk "Indent")
  "c l" '(comment-line   :wk "Line")
  "c r" '(comment-region :wk "Region")
  ;; Evaluate
  "e"   '(:ignore t        :wk "Evaluate")
  "e b" '(eval-buffer      :wk "Current buffer")
  "e e" '(eval-last-sexp   :wk "Last sexp")
  "e E" '(eval-expression  :wk "Expression")
  "e p" '(plumber-plumb    :wk "Plumb")
  "e P" '(plumber-plumb-as :wk "Plumb as")
  "e r" '(eval-region      :wk "Region")
  ;; File
  "f"   '(:ignore t          :wk "File")
  "f c" '(compile            :wk "Compile")
  "f C" '(recompile          :wk "Re-compile")
  "f d" '(diff               :wk "Diff")
  "f D" '(ediff              :wk "Ediff")
  "f f" '(find-file          :wk "Find file")
  "f F" '(find-name-dired    :wk "Find wildcard recursively")
  "f o" '(ff-find-other-file :wk "Find other file")
  "f O" '(find-file-at-point :wk "Find file at point")
  "f r" '(recentf-open       :wk "Open recent")
  ;; Magit
  "g"   '(:ignore t                 :wk "Magit")
  "g c" '(magit-commit              :wk "Commit menu")
  "g e" '(magit-ediff-wdim          :wk "Ediff (DWIM)")
  "g f" '(magit-fetch               :wk "Fetch menu")
  "g F" '(magit-pull                :wk "Pull menu")
  "g g" '(magit-status              :wk "Status")
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
  "h l" '(find-library            :wk "Find library")
  "h m" '(describe-mode           :wk "Describe mode")
  "h v" '(describe-variable       :wk "Describe variable")
  ;; Insert
  "i"   '(:ignore t               :wk "Insert")
  "i c" '(insert-char             :wk "Character")
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
  "o &" '(async-shell-command   :wk "Async shell command")
  "o c" '(calc                  :wk "Calculator")
  "o C" '(quick-calc            :wk "Quick calculator")
  "o d" '(projectile-run-gdb    :wk "Debugger")
  "o e" '(x8dcc/eshell-popup    :wk "Eshell popup")
  "o E" '(x8dcc/eshell-numbered :wk "Eshell")
  "o m" '(man                   :wk "Manpage")
  "o M" '(compose-mail          :wk "Compose mail")
  "o x" '(scratch-buffer        :wk "Scratch buffer")
  ;; Open -> Org
  "o o"   '(:ignore t   :wk "Org")
  "o o a" '(org-agenda  :wk "Agenda")
  "o o c" '(org-capture :wk "Capture")
  ;; Project
  "p"   '(:ignore t                      :wk "Project")
  "p c" '(projectile-compile-project     :wk "Compile")
  "p C" '(projectile-repeat-last-command :wk "Re-compile")
  "p f" '(projectile-find-file           :wk "Find file")
  "p p" '(projectile-switch-project      :wk "Recent projects")
  "p r" '(project-query-replace-regexp   :wk "Replace regex (query)")
  ;; Search
  "s"   '(:ignore t                    :wk "Search")
  "s g" '(rgrep                        :wk "Grep (recursive)")
  "s h" '(highlight-regexp             :wk "Highlight")
  "s H" '(unhighlight-regexp           :wk "Unhighlight")
  "s o" '(occur                        :wk "Occurrences")
  "s r" '(query-replace                :wk "Replace (query)")
  "s R" '(query-replace-regexp         :wk "Replace regex (query)")
  "s s" '(isearch-forward              :wk "I-search")
  "s S" '(isearch-forward-regexp       :wk "I-search regex")
  ;; Search -> Next
  "s n"   '(:ignore t                :wk "Next")
  "s n n" '(next-error               :wk "Error/match")
  "s n s" '(spell-fu-goto-next-error :wk "Spelling error")
  ;; Search -> Previous
  "s p"   '(:ignore t                    :wk "Previous")
  "s p p" '(previous-error               :wk "Error/match")
  "s p s" '(spell-fu-goto-previous-error :wk "Spelling error")
  ;; Toggle
  "t"   '(:ignore t                          :wk "Toggle")
  "t b" '(big-font-mode                      :wk "Big font")
  "t c" '(display-fill-column-indicator-mode :wk "Fill column line")
  "t f" '(variable-pitch-mode                :wk "Variable pitch font")
  "t i" '(toggle-case-fold-search            :wk "Case-sensitive searches")
  "t l" '(display-line-numbers-mode          :wk "Line numbers")
  "t L" '(hl-line-mode                       :wk "Highlight line")
  "t p" '(popper-toggle                      :wk "Last popup")
  "t P" '(popper-toggle-type                 :wk "Popup type")
  "t r" '(read-only-mode                     :wk "Read only mode")
  "t s" '(spell-fu-mode                      :wk "Spell checking")
  "t S" '(whitespace-mode                    :wk "Whitespace visualization")
  "t v" '(visible-mode                       :wk "Visible mode")
  "t w" '(toggle-truncate-lines              :wk "Line wrapping")
  "t W" '(auto-fill-mode                     :wk "Auto fill mode")
  ;; Version control
  "v"   '(:ignore t                :wk "VC")
  "v d" '(vc-diff                  :wk "Diff")
  "v g" '(vc-annotate              :wk "Annotate")
  "v I" '(vc-log-incoming          :wk "Incoming log")
  "v o" '(vc-revision-other-window :wk "Show other revision")
  "v p" '(vc-update                :wk "Pull changes")
  "v P" '(vc-push                  :wk "Push changes")
  "v v" '(vc-next-action           :wk "DWIM")
  "v V" '(vc-refresh-state         :wk "Refresh state")
  ;; Version control -> Branch
  "v b"   '(:ignore t        :wk "Branch")
  "v b b" '(vc-switch-branch :wk "Switch to")
  "v b c" '(vc-create-branch :wk "Create")
  ;; Version control -> Log
  "v l"   '(:ignore t           :wk "Log")
  "v l b" '(vc-print-branch-log :wk "Branch")
  "v l f" '(vc-print-log        :wk "Current file")
  "v l l" '(vc-print-root-log   :wk "Root")
  "v l o" '(vc-log-outgoing     :wk "Outgoing")
  ;; Version control -> Stash
  "v z"   '(:ignore t          :wk "Git stash")
  "v z a" '(vc-git-stash-apply :wk "Apply")
  "v z p" '(vc-git-stash-pop   :wk "Pop")
  "v z s" '(vc-git-stash-show  :wk "Show")
  "v z z" '(vc-git-stash       :wk "Create new")
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
  "m T"   '(org-babel-tangle :wk "Tangle current file")
  ;; Mode -> Date
  "m d"   '(:ignore t    :wk "Date")
  "m d d" '(org-deadline :wk "Deadline")
  "m d s" '(org-schedule :wk "Schedule")
  ;; Mode -> Export
  "m e"   '(:ignore t                     :wk "Export")
  "m e a" '(org-ascii-export-to-ascii     :wk "ASCII (text)")
  "m e h" '(org-html-export-to-html       :wk "HTML")
  "m e l" '(org-latex-export-to-latex     :wk "LaTeX")
  "m e p" '(org-latex-export-to-pdf       :wk "PDF")
  "m e t" '(org-texinfo-export-to-texinfo :wk "Texinfo")
  ;; Mode -> Insert
  "m i"   '(:ignore t                  :wk "Insert")
  "m i d" '(x8dcc/skeleton-org-default :wk "Default header")
  "m i b" '(x8dcc/skeleton-org-blog    :wk "Blog header")
  ;; Mode -> Link
  "m l"   '(:ignore t             :wk "Link")
  "m l l" '(x8dcc/org-insert-link :wk "Insert")
  "m l s" '(org-store-link        :wk "Store")
  ;; Mode -> Priority
  "m p"   '(:ignore t         :wk "Priority")
  "m p d" '(org-priority-down :wk "Decrease")
  "m p p" '(org-priority      :wk "Insert")
  "m p u" '(org-priority-up   :wk "Increase")
  ;; Mode -> Toggle
  "m t"   '(:ignore t                :wk "Toggle")
  "m t i" '(org-indent-mode          :wk "Indent mode")
  "m t I" '(org-toggle-inline-images :wk "Inline images")
  "m t t" '(org-todo                 :wk "Todo"))

(x8dcc/def-keys-latex
  ;; Mode (LaTeX)
  "m c"   '(x8dcc/latex-compile       :wk "Compile to PDF (LaTeX)")
  "m b"   '(latex-insert-block        :wk "Open block")
  "m B"   '(latex-close-block         :wk "Close block")
  "m j"   '(LaTeX-find-matching-begin :wk "Jump to block start")
  "m J"   '(LaTeX-find-matching-end   :wk "Jump to block end")
  "m m"   '(TeX-insert-macro          :wk "Insert macro")
  "m p"   '(prettify-symbols-mode     :wk "Prettify symbols")
  "m s"   '(LaTeX-section             :wk "New section")
  ;; Text format
  "m f"   '(:ignore t                   :wk "Text format")
  "m f b" '(x8dcc/latex-font-bold       :wk "Bold")
  "m f c" '(x8dcc/latex-font-smallcaps  :wk "Smallcaps")
  "m f e" '(x8dcc/latex-font-emphasized :wk "Emphasized")
  "m f i" '(x8dcc/latex-font-italics    :wk "Italics")
  "m f r" '(x8dcc/latex-font-roman      :wk "Roman")
  "m f s" '(x8dcc/latex-font-slanted    :wk "Slanted")
  "m f t" '(x8dcc/latex-font-typewriter :wk "Typewriter")
  ;; Folding
  "m F"   '(:ignore t                :wk "Fold")
  "m F f" '(TeX-fold-dwim            :wk "DWIM")
  "m F b" '(TeX-fold-buffer          :wk "Fold buffer")
  "m F B" '(TeX-fold-clearout-buffer :wk "Unfold buffer"))

(x8dcc/def-keys-texinfo
  ;; Mode (Texinfo)
  "m c"   '(x8dcc/tex-compile      :wk "Compile to PDF (TeX)")
  "m b"   '(Texinfo-environment    :wk "Open block")
  "m B"   '(texinfo-insert-@end    :wk "Close block")
  "m j"   '(Texinfo-find-env-start :wk "Jump to block start")
  "m J"   '(Texinfo-find-env-end   :wk "Jump to block end"))

(x8dcc/def-keys-c
  ;; Buffer
  "b f"   '(clang-format-buffer :wk "Format")
  ;; Mode (C)
  "m d"   '(x8dcc/beardbolt-disassemble :wk "Beardbolt disassemble")
  "m m"   '(c-macro-expand              :wk "Expand macros in region")
  ;; Mode -> Toggle
  "m t"   '(:ignore t               :wk "Toggle")
  "m t a" '(c-toggle-auto-newline   :wk "Auto-newline")
  "m t e" '(eldoc-mode              :wk "ElDoc mode")
  "m t h" '(c-toggle-hungry-state   :wk "Hungry-delete-key")
  "m t I" '(hide-ifdef-mode         :wk "Unused ifdefs")
  "m t l" '(c-toggle-electric-state :wk "Electric indentation")
  ;; Mode -> Insert
  "m i"   '(:ignore t                        :wk "Insert")
  "m i h" '(x8dcc/skeleton-c-header          :wk "Header skeleton")
  "m i l" '(x8dcc/skeleton-c-license-comment :wk "License comment (GPL-3.0)")
  "m i s" '(x8dcc/skeleton-c-source          :wk "Source skeleton"))

(general-auto-unbind-keys)
(x8dcc/def-keys-diff
  ;; Mode (Diff)
  "m a" '(diff-apply-hunk  :wk "Apply hunk to file")
  "m d" '(diff-hunk-kill   :wk "Delete hunk")
  "m e" '(diff-ediff-patch :wk "Go to ediff session")
  "m n" '(diff-hunk-next   :wk "Next hunk")
  "m p" '(diff-hunk-prev   :wk "Prev hunk")
  "m s" '(diff-split-hunk  :wk "Split hunk"))
(general-auto-unbind-keys 'disable)

(x8dcc/def-keys-message
  ;; Mode (Message)
  "m a" '(mml-attach-file                 :wk "Attach file")
  "m c" '(message-yank-original           :wk "Cite original")
  "m e" '(mml-secure-message-sign-encrypt :wk "Sign and encrypt")
  "m p" '(mml-preview                     :wk "Preview")
  "m s" '(mml-secure-message-sign         :wk "Sign")
  "m S" '(message-send                    :wk "Send"))

(x8dcc/def-keys-rmail
  ;; Mode (Rmail)
  "m d" '(rmail-delete-forward             :wk "Delete and move forward")
  "m D" '(rmail-epa-decrypt                :wk "Decrypt")
  "m g" '(rmail-get-new-mail               :wk "Get new mail")
  "m i" '(rmail-input                      :wk "Input file")
  "m m" '(rmail-summary                    :wk "Summary")
  "m n" '(rmail-next-undeleted-message     :wk "Next non-deleted")
  "m N" '(rmail-next-message               :wk "Next")
  "m o" '(rmail-output                     :wk "Output to file")
  "m O" '(rmail-output-as-seen             :wk "Output to file (as seen)")
  "m p" '(rmail-previous-undeleted-message :wk "Previous non-deleted")
  "m P" '(rmail-previous-message           :wk "Previous")
  "m r" '(rmail-reply                      :wk "Reply")
  "m s" '(rmail-expunge-and-save           :wk "Expunge and save")
  "m u" '(rmail-undelete-previous-message  :wk "Undelete current or previous")
  ;; Mode -> Label
  "m l"   '(:ignore t                      :wk "Label")
  "m l a" '(rmail-add-label                :wk "Add")
  "m l d" '(rmail-kill-label               :wk "Delete")
  "m l m" '(rmail-summary-by-labels        :wk "Summary of labeled")
  "m l n" '(rmail-next-labeled-message     :wk "Next labeled with")
  "m l p" '(rmail-previous-labeled-message :wk "Previous labeled with"))

(x8dcc/def-keys-rmail-summary
  ;; Mode (Rmail summary)
  "m d" '(rmail-summary-delete-forward   :wk "Delete and move forward")
  "m D" '(rmail-summary-epa-decrypt      :wk "Decrypt")
  "m g" '(rmail-summary-get-new-mail     :wk "Get new mail")
  "m i" '(rmail-summary-input            :wk "Input file")
  "m o" '(rmail-summary-output           :wk "Output to file")
  "m O" '(rmail-summary-output-as-seen   :wk "Output to file (as seen)")
  "m r" '(rmail-summary-reply            :wk "Reply")
  "m s" '(rmail-summary-expunge-and-save :wk "Expunge and save")
  "m u" '(rmail-summary-undelete         :wk "Undelete")
  "m U" '(rmail-summary-undelete-many    :wk "Undelete all")
  ;; Mode -> Label
  "m l"   '(:ignore t                              :wk "Label")
  "m l a" '(rmail-summary-add-label                :wk "Add")
  "m l d" '(rmail-summary-kill-label               :wk "Delete")
  "m l n" '(rmail-summary-next-labeled-message     :wk "Next labeled with")
  "m l p" '(rmail-summary-previous-labeled-message :wk "Previous labeled with"))

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
      (let ((power-source (battery-format
                           "%L" (funcall battery-status-function)))
            (power-status (battery-format
                           "%B" (funcall battery-status-function))))
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

(x8dcc/hook-to-targets (lambda ()
                         (hl-line-mode 1))
                       '(prog-mode-hook
                         dired-mode-hook))

(setq hl-line-sticky-flag nil)

(setq-default display-fill-column-indicator-character ?\u00A6
              fill-column 80)

(x8dcc/hook-to-targets
 (lambda ()
   (display-fill-column-indicator-mode 1))
 '(prog-mode-hook
   TeX-mode-hook
   message-mode-hook))

(setq tab-bar-show 1)

(setq-default truncate-lines t)
(global-visual-line-mode 0)

(blink-cursor-mode 0)

(x8dcc/hook-to-targets
 (lambda ()
   (setq indicate-buffer-boundaries 'left))
 '(text-mode-hook
   prog-mode-hook))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq indicate-buffer-boundaries
                  '((top    . left)
                    (bottom . nil) ; Don't show bottom
                    (up     . left)
                    (down   . left)))))

(defun x8dcc/set-display-same-window (regexp)
  "Specify that buffers matching REGEXP should be displayed on the same window.
Adds an entry to `display-buffer-alist'."
  (add-to-list 'display-buffer-alist
               `(,regexp
                 (display-buffer-same-window
                  display-buffer-reuse-window
                  display-buffer-in-side-window))))

(dolist (regexp '("\\*grep\\*"
                  "\\*vc-.+\\*"))
  (x8dcc/set-display-same-window regexp))

(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

(save-place-mode 1)

(savehist-mode 1)

(add-to-list 'savehist-additional-variables 'erc-ignore-list)

(defconst x8dcc/git-commit-filename-regexp
  (rx "/"
      (or "addp-hunk-edit.diff"
          "ADD_EDIT.patch"
          (seq (or "" "MERGE_"
                   (seq (or "COMMIT" "NOTES" "PULLREQ" "MERGEREQ" "TAG")
                        "_EDIT"))
               "MSG")
          (seq (or "BRANCH" "EDIT")
               "_DESCRIPTION"))
      string-end)
  "Regexp for matching git commit filenames. Obtained from git-commit.el,
version 3.3.0.50, modified by 8dcc.")

(add-to-list 'undo-fu-session-incompatible-files x8dcc/git-commit-filename-regexp)

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

(setq lazy-highlight-cleanup t
      lazy-highlight-initial-delay 2
      lazy-highlight-max-at-a-time nil)

(setq isearch-allow-scroll t
      search-whitespace-regexp ".{,10}")

(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-subfix-format nil)

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

(global-auto-revert-mode 1)

(setq auto-revert-check-vc-info t)

(setq vc-follow-symlinks t)

(setq browse-url-generic-program "firefox")

(add-hook 'prog-mode-hook
          (lambda ()
            (electric-pair-mode 1)))

(setq calendar-week-start-day 1
      calendar-weekend-days '(6 0))

(defconst x8dcc/audio-file-regexp
  (rx (seq (one-or-more (any alnum blank ?\\ ?/ ?~ ?. ?_ ?- ?\( ?\)))
           "."
           (or "flac" "mp3" "wav")))
  "Regexp for matching an audio file.")

(with-eval-after-load 'plumber
  (setq plumber-rules
        (x8dcc/alist-insert-before-key plumber-rules
                                       `("Audio file"
                                         ,x8dcc/audio-file-regexp
                                         emms-play-file)
                                       "File")))

(setq xref-show-definitions-function #'consult-xref
      xref-show-xrefs-function #'consult-xref)

(setq xref-prompt-for-identifier nil)

(setq printer-name "MainPrinter")

(defun x8dcc/lpr-buffer-pages (start end)
  "Print the current buffer using `lpr-buffer' from page START to END. The page
numbers start at 1."
  (interactive "nStarting page: \nnEnd page: ")
  (let ((lpr-switches (list "-o" (format "page-ranges=%d-%d"
                                         (max start 1) (max end 1)))))
    (lpr-buffer)))

(setq eshell-prompt-function (lambda ()
                               (concat
                                (abbreviate-file-name (eshell/pwd))
                                (propertize " λ" 'face '(:foreground "#8490B3"))
                                (propertize " " 'face '(:inherit default)))))

(setq eshell-prompt-regexp "^[^#λ]* [#λ] ")

(defun x8dcc/eshell-project-or-current (&optional eshell-func)
  "Run ESHELL-FUNC in the project's root whenever possible."
  (interactive)
  (unless eshell-func (setq eshell-func #'eshell))
  (if (projectile-project-p)
      (projectile-with-default-dir (projectile-acquire-root)
        (funcall eshell-func))
    (funcall eshell-func)))

(defun x8dcc/eshell-numbered (&optional buffer-name)
  "Open an eshell buffer, adding a number suffix when necessary.

That is, append a count to the buffer name if this was not the first buffer
named BUFFER-NAME. If BUFFER-NAME is nil, \"*eshell*\" is used.

It uses `x8dcc/eshell-project-or-current' for calling `eshell', and
`x8dcc/suffixed-buffer-name' for obtaining the buffer name."
  (interactive)
  (unless buffer-name (setq buffer-name "*eshell*"))
  (let* ((eshell-buffer-name (x8dcc/suffixed-buffer-name buffer-name)))
    (x8dcc/eshell-project-or-current)))

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

(setq eshell-hist-ignoredups t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local tab-width 4)))

(setq dired-listing-switches
      "-l --all --sort=version --group-directories-first --human-readable")

(setq dired-recursive-copies  'top
      dired-recursive-deletes 'top)

(setq dired-kill-when-opening-new-dired-buffer t)

(setq dired-auto-revert-buffer t)

(setq dired-compress-file-default-suffix ".gz"
      dired-compress-directory-default-suffix ".zip")

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-horizontally)

(require 'erc)
(add-to-list 'erc-modules 'log)
(add-to-list 'erc-modules 'stamp)
(add-to-list 'erc-modules 'track)
(erc-update-modules)

(defconst x8dcc/erc-sasl-servers
  '("irc.libera.chat")
  "List of servers that should be connected through SASL when using
`x8dcc/erc-launch'.")

(defun x8dcc/erc-launch (server port user)
  "Launch ERC through TLS or SASL, depending on `x8dcc/erc-sasl-servers'.

When called interactively, uses 6697 as the port and the value of `erc-nick' as
the user."
  (interactive
   (list (read-string (format-prompt "Server" erc-default-server)
                      nil 'erc-server-history-list erc-default-server)
         6697
         erc-nick))
  (if (member 'sasl erc-modules)
      (progn
        (message "Why was the SASL module enabled globally? Disabling.")
        (setq erc-modules (delete 'sasl erc-modules))))
  ;; Enable the SASL module if the specified server is in the
  ;; `x8dcc/erc-sasl-servers' list.
  (cond ((member server x8dcc/erc-sasl-servers)
         (message "Logging in with SASL to `%s'" server)
         (let ((erc-modules (cons 'sasl erc-modules)))
           (erc-tls :server server :port port :user user)))
        (t
         (message "Logging in with TLS to `%s'" server)
         ;; We don't need to specify the password, since
         ;; `x8dcc/erc-get-password' will be used.
         (erc-tls :server server :port port :user user))))

(advice-add 'erc :override #'x8dcc/erc-launch)

(setq erc-sasl-mechanism 'plain)

(defun x8dcc/erc-get-password (&rest plist)
  "Custom replacement for `erc-auth-source-search' that prompts for a password
if necessary."
  (let ((auth-source-password (apply #'erc-auth-source-search plist)))
    (or auth-source-password
        (let ((username (plist-get plist :user)))
          (read-passwd (or (and username
                                (format "Password for `%s': " username))
                           "Password: "))))))

;; Use for authenticating in TLS and SASL.
;; NOTE: Could be used for other `erc-auth-source-*' functions
(setq erc-auth-source-server-function #'x8dcc/erc-get-password
      erc-sasl-auth-source-function #'x8dcc/erc-get-password)

;; The password prompt will be managed by `x8dcc/erc-get-password', if
;; necessary; not by `erc-tls'.
(setq erc-prompt-for-password nil)

(setq erc-try-new-nick-p nil)

(setq erc-anonymous-login t
      erc-disable-ctcp-replies t
      erc-paranoid t)

(setq erc-nick           "x8dcc"
      erc-system-name    "x8dcc"
      erc-user-full-name "x8dcc")

(setq erc-enable-logging t)

;; Directory for logs
(setq erc-log-channels-directory
      (concat user-emacs-directory "erc-log"))

;; When to write logs
(setq erc-log-write-after-send t
      erc-log-write-after-insert t)

(setq erc-stamp-mode t
      erc-hide-timestamps t)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(setq erc-fill-column 80)

;; Align usernames to col 12
(setq erc-fill-function 'erc-fill-static
      erc-fill-static-center 12)

(setq erc-scrolltobottom-mode t
      erc-input-line-position -1)

(setq erc-track-showcount t
      erc-track-exclude-list '("NICK" "JOIN" "PART" "QUIT" "333" "353"))

(setq erc-warn-about-blank-lines t)

(setq erc-join-buffer 'buffer
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t)

(setq erc-server-reconnect-attempts 5
      erc-server-reconnect-timeout 3)

(setq erc-prompt
      (lambda ()
        (concat "[" (buffer-name) "]:")))

(defvar x8dcc/mail-directory
  (expand-file-name "~/Mail/"))

(setq read-mail-command #'rmail
      mail-user-agent 'message-user-agent)

(setq epg-pinentry-mode 'loopback)

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

(setq smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 465
      send-mail-function         #'smtpmail-send-it
      message-send-mail-function #'smtpmail-send-it)

(setq mml-secure-openpgp-sign-with-sender t)

(setq message-default-headers (concat "FCC: " x8dcc/mail-directory "sent"))

(setq rmail-file-name (concat x8dcc/mail-directory "inbox")
      rmail-secondary-file-directory x8dcc/mail-directory)

(setq rmail-primary-inbox-list
      (list (concat "/var/mail/" user-login-name)))

(setq rmail-default-file (concat x8dcc/mail-directory "saved")
      rmail-default-body-file (concat x8dcc/mail-directory "saved-body"))

(setq rmail-preserve-inbox nil)

(setq rmail-mime-prefer-html nil)

(with-eval-after-load 'org
  (require 'org-tempo))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local
             electric-pair-inhibit-predicate
             (lambda (c)
               (if (char-equal c ?<)
                   t
                 (electric-pair-default-inhibit c))))))

(let ((expanded-org-directory (expand-file-name "~/Sync/Org/")))
  (if (file-directory-p expanded-org-directory)
      (setq org-directory expanded-org-directory
            org-agenda-files (mapcar (lambda (filename)
                                       (concat org-directory filename))
                                     '("agenda.org" "notes.org")))))

(setq org-agenda-start-on-weekday calendar-week-start-day
      org-agenda-weekend-days calendar-weekend-days)

(setq org-clock-sound (concat user-emacs-directory "my-media/notification.wav"))

(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)
                             (scheme . t)
                             (shell . t)))

(setq org-html-postamble nil
      org-export-time-stamp-file nil)

(setq org-export-with-smart-quotes t)

(setq org-html-prefer-user-labels t
      org-latex-prefer-user-labels t)

(setq org-latex-title-command "\\maketitle\n\\clearpage"
      org-latex-toc-command "\\tableofcontents\n\\clearpage\n")

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

(setq org-link-descriptive t)

(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode 1)))

(set-face-attribute 'org-done          nil :inherit 'shadow :bold t)
(set-face-attribute 'org-headline-done nil :inherit 'shadow)

(setq org-highlight-latex-and-related '(latex entities))

(setq org-default-notes-file (concat org-directory "notes.org"))

(setq org-capture-templates
      '(("n" "Note" entry
         (file+headline "notes.org" "Notes")
         "* NOTE %?\nCaptured on: %T")
        ("N" "Note (for review)" entry
         (file+headline "notes.org" "Notes")
         "* REVIEW %?\nSCHEDULED: <%(x8dcc/date-tomorrow)>\nCaptured on: %T")
        ("s" "Selection" entry
         (file+headline "notes.org" "Selections")
         "* Selection from [[%F][%f]]\nCaptured on: %T\n%?\n#+begin_quote\n%i\n#+end_quote")))

(defun x8dcc/org-insert-link ()
  "Insert a space in the current position if there isn't one, and call
`org-insert-link'."
  (interactive)
  (if (not (looking-back "^\\|[ \t]" nil))
      (insert " "))
  (funcall-interactively #'org-insert-link))

(defun x8dcc/org-headline-to-id (headline)
  "Converts an org-mode HEADLINE to a CUSTOM-ID dashed string.
For example: \"My test... =heading=\" becomes \"my-test-heading\"."
  (thread-last
    (downcase headline)
    (replace-regexp-in-string "\\(\"+\\|'+\\)" "")
    (replace-regexp-in-string "[^[:alnum:]]+" "-")
    (replace-regexp-in-string "\\(^-+\\|-+$\\)" "")))

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

(define-skeleton x8dcc/skeleton-org-default
  "Insert a basic Org header skeleton."
  nil
  '(setq str (skeleton-read "Title: "))
  '(setq v1 (if (or (null str)
                    (string-empty-p str))
                (capitalize (file-name-base buffer-file-name))
              str))
  "#+TITLE: " v1 "\n"
  "#+AUTHOR: " user-full-name "\n"
  "#+OPTIONS: toc:2\n"
  "#+STARTUP: nofold\n\n"
  _ \n)

(define-skeleton x8dcc/skeleton-org-blog
  "Insert an Org skeleton for blog articles."
  nil
  '(setq str (skeleton-read "Title: "))
  '(setq v1 (if (or (null str)
                    (string-empty-p str))
                (capitalize (file-name-base buffer-file-name))
              str))
  "#+TITLE: " v1 "\n"
  "#+AUTHOR: " user-full-name "\n"
  "#+OPTIONS: toc:nil\n"
  "#+STARTUP: nofold\n"
  "#+HTML_HEAD: <link rel=\"icon\" type=\"image/x-icon\" href=\"../img/favicon.png\">\n"
  "#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"../css/main.css\">\n\n"
  "[[file:../index.org][Index]] | [[file:index.org][Up]]\n\n"
  "-----\n\n"
  "#+TOC: headlines 2\n\n"
  "* Introduction\n\n"
  _ \n)

(setq TeX-parse-self t)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (TeX-fold-mode 1)))

(setq TeX-fold-unfold-around-mark t)

(defun x8dcc/latex-compile ()
  "Compile the current master file using the \"LaTeX\" command."
  (interactive)
  (TeX-command "LaTeX" #'TeX-master-file))

(defun x8dcc/tex-compile ()
  "Compile the current master file using the \"TeX\" command."
  (interactive)
  (TeX-command "TeX" #'TeX-master-file))

(defun x8dcc/tex-get-font-key (command-string &optional font-list)
  "Find the font key in FONT-LIST for the font whose LaTeX command contains
COMMAND-STRING. Returns a valid font key that can be passed to `TeX-font', or
nil if COMMAND-STRING is not found. If FONT-LIST is nil, `TeX-font-list' is
used."
  ;; TODO: Check if `TeX-font-list' is bound.
  (unless font-list (setq font-list TeX-font-list))
  (let ((item (car font-list)))
    (cond ((string-match-p (regexp-quote command-string) (cadr item))
           (car item))
          ((cdr font-list)
           (x8dcc/tex-get-font-key command-string (cdr font-list)))
          (t nil))))

(defmacro x8dcc/tex-defun-font (func-name command-string &optional font-list)
  "Define a function named FUNC-NAME that searches for a COMMAND-STRING in
FONT-LIST using `x8dcc/tex-get-font-key'."
  `(defun ,func-name ()
     (interactive)
     (let ((key (x8dcc/tex-get-font-key ,command-string ,font-list)))
       (if key (TeX-font nil key)))))

(x8dcc/tex-defun-font x8dcc/latex-font-bold       "bf{")
(x8dcc/tex-defun-font x8dcc/latex-font-emphasized "emph{")
(x8dcc/tex-defun-font x8dcc/latex-font-italics    "it{")
(x8dcc/tex-defun-font x8dcc/latex-font-roman      "rm{")
(x8dcc/tex-defun-font x8dcc/latex-font-smallcaps  "sc{")
(x8dcc/tex-defun-font x8dcc/latex-font-slanted    "sl{")
(x8dcc/tex-defun-font x8dcc/latex-font-typewriter "tt{")

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

(with-eval-after-load 'latex
  (setq LaTeX-indent-environment-list
        (nconc '(("itemize" LaTeX-indent-item)
                 ("enumerate" LaTeX-indent-item)
                 ("description" LaTeX-indent-item))
               LaTeX-indent-environment-list)))

(define-skeleton x8dcc/skeleton-latex-article
  "Insert a generic LaTeX article skeleton."
  "Document title: "
  "\\documentclass{article}\n\n"
  "\\title{" str "}\n"
  "\\author{" user-full-name "}\n"
  "\\date{" (format-time-string "%Y") "}\n"
  "\n% ------------------------------------------------------------------------------\n"
  "% Packages\n"
  "% ------------------------------------------------------------------------------\n\n"
  "% Link sections and subsections\n"
  "\\usepackage{hyperref}\n"
  "\\hypersetup{linktoc=all, hidelinks}\n\n"
  "% Code highlighting.\n"
  "\\usepackage{listings}\n"
  "\\lstset{\n"
  "  % Showing spaces\n"
  "  showspaces=false,\n"
  "  showstringspaces=false,\n"
  "  showtabs=false,\n"
  "  % Indentation and breaks\n"
  "  tabsize=4,\n"
  "  breaklines=true,\n"
  "  breakatwhitespace=true,\n"
  "  columns=flexible,\n"
  "  % Show left, right, top and bottom borders\n"
  "  frame=tblr,\n"
  "  % Misc\n"
  "  aboveskip=3mm,\n"
  "  belowskip=3mm,\n"
  "  basicstyle={\\small\\ttfamily},\n"
  "}\n\n"
  "% Different monospace font for code blocks (listings)\n"
  "\\usepackage{inconsolata}\n"
  "\n% ------------------------------------------------------------------------------\n"
  "% Document start\n"
  "% ------------------------------------------------------------------------------\n\n"
  "\\begin{document}\n\n"
  "\\maketitle\n"
  "\\clearpage\n"
  "\\tableofcontents\n"
  "\\clearpage\n"
  "\n% ------------------------------------------------------------------------------\n"
  "\\section{" _ "}\n"
  "\\label{sec:TODO}\n"
  "% ------------------------------------------------------------------------------\n\n\n\n"
  "\\end{document}" \n)

(define-skeleton x8dcc/skeleton-latex-math
  "Insert a math LaTeX document skeleton (amsart)."
  "Document title: "
  "\\documentclass{amsart}\n\n"
  "\\title{" str "}\n"
  "\\author{" user-full-name "}\n"
  "\\date{}\n"
  "\n% ------------------------------------------------------------------------------\n"
  "% Packages\n"
  "% ------------------------------------------------------------------------------\n\n"
  "% Link sections and subsections\n"
  "\\usepackage{hyperref}\n"
  "\\hypersetup{linktoc=all, hidelinks}\n\n"
  "% Various math utilities\n"
  "\\usepackage{amsmath}\n\n"
  "% Graphs\n"
  "\\usepackage{tikz}\n"
  "\\usetikzlibrary{calc} % Coordinate calculations\n"
  "\\tikzset{>=stealth}   % Change default arrow style\n\n"
  "% Code highlighting.\n"
  "\\usepackage{listings}\n"
  "\\lstset{\n"
  "  % Showing spaces\n"
  "  showspaces=false,\n"
  "  showstringspaces=false,\n"
  "  showtabs=false,\n"
  "  % Indentation and breaks\n"
  "  tabsize=4,\n"
  "  breaklines=true,\n"
  "  breakatwhitespace=true,\n"
  "  columns=flexible,\n"
  "  % Show left, right, top and bottom borders\n"
  "  frame=tblr,\n"
  "  % Misc\n"
  "  aboveskip=3mm,\n"
  "  belowskip=3mm,\n"
  "  basicstyle={\\small\\ttfamily},\n"
  "}\n\n"
  "% Different monospace font for code blocks (listings)\n"
  "\\usepackage{inconsolata}\n"
  "\n% ------------------------------------------------------------------------------\n"
  "% Other settings\n"
  "% ------------------------------------------------------------------------------\n\n"
  "% Remove author and extra info from the headers\n"
  "\\pagestyle{plain}\n\n"
  "% New environment adding spacing for tikz pictures\n"
  "\\newenvironment{tikzpicturecenter}\n"
  "{\\begin{center}\\begin{tikzpicture}}\n"
  "    {\\end{tikzpicture}\\end{center}}\n"
  "\n% ------------------------------------------------------------------------------\n"
  "% Document start\n"
  "% ------------------------------------------------------------------------------\n\n"
  "\\begin{document}\n\n"
  "\\maketitle\n"
  "\\tableofcontents\n"
  "\\clearpage\n"
  "\n% ------------------------------------------------------------------------------\n"
  "\\section{" _ "}\n"
  "\\label{sec:TODO}\n"
  "% ------------------------------------------------------------------------------\n\n\n\n"
  "\\end{document}" \n)

(define-skeleton x8dcc/skeleton-python-main
  "Insert a basic Python skeleton with a main function."
  nil
  "#!/usr/bin/python3\n\n"
  "def main():\n"
  > "pass" _ "\n\n"
  "if __name__ == \"__main__\":\n"
  > "main()" \n)

(setq c-tab-always-indent nil)

(setq hide-ifdef-initially t
      hide-ifdef-lines t)

(add-hook 'c-mode-hook
          (lambda ()
            (cwarn-mode 1)))

;; Can't diminish before <cwarn.el> is loaded.
(with-eval-after-load 'cwarn
  (diminish 'cwarn-mode))

(with-eval-after-load 'find-file
  (setq-default ff-quiet-mode t)
  (dolist (path '("./include" ".."))
    (add-to-list 'cc-search-directories path)))

(c-add-style "x8dcc/c-style"
             `("k&r"
               (c-basic-offset . ,tab-width)
               (c-comment-only-line-offset . 0)
               (c-doc-comment-style . doxygen)
               (c-hanging-braces-alist
                (defun-open after)
                (substatement-open after))
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

(defconst x8dcc/c-constant-list
  '(;; <limits.h>
    "CHAR_BIT" "MB_LEN_MAX" "MB_CUR_MAX"
    "UCHAR_MAX" "UINT_MAX" "ULONG_MAX" "USHRT_MAX"
    "CHAR_MIN" "INT_MIN" "LONG_MIN" "SHRT_MIN"
    "CHAR_MAX" "INT_MAX" "LONG_MAX" "SHRT_MAX"
    "SCHAR_MIN" "SINT_MIN" "SLONG_MIN" "SSHRT_MIN"
    "SCHAR_MAX" "SINT_MAX" "SLONG_MAX" "SSHRT_MAX"
    "LLONG_MIN" "LLONG_MAX" "ULLONG_MAX"
    "INT8_MIN" "INT16_MIN" "INT32_MIN" "INT64_MIN"
    "INT8_MAX" "INT16_MAX" "INT32_MAX" "INT64_MAX"
    "UINT8_MAX" "UINT16_MAX" "UINT32_MAX" "UINT64_MAX"
    "INT_LEAST8_MIN" "INT_LEAST16_MIN" "INT_LEAST32_MIN" "INT_LEAST64_MIN"
    "INT_LEAST8_MAX" "INT_LEAST16_MAX" "INT_LEAST32_MAX" "INT_LEAST64_MAX"
    "UINT_LEAST8_MAX" "UINT_LEAST16_MAX" "UINT_LEAST32_MAX" "UINT_LEAST64_MAX"
    "INT_FAST8_MIN" "INT_FAST16_MIN" "INT_FAST32_MIN" "INT_FAST64_MIN"
    "INT_FAST8_MAX" "INT_FAST16_MAX" "INT_FAST32_MAX" "INT_FAST64_MAX"
    "UINT_FAST8_MAX" "UINT_FAST16_MAX" "UINT_FAST32_MAX" "UINT_FAST64_MAX"
    "INTPTR_MIN" "INTPTR_MAX" "UINTPTR_MAX"
    "INTMAX_MIN" "INTMAX_MAX" "UINTMAX_MAX"
    "PTRDIFF_MIN" "PTRDIFF_MAX" "SIG_ATOMIC_MIN" "SIG_ATOMIC_MAX"
    "SIZE_MAX" "WCHAR_MIN" "WCHAR_MAX" "WINT_MIN" "WINT_MAX"
    ;; <float.h>
    "FLT_RADIX" "FLT_ROUNDS"
    "FLT_DIG" "FLT_MANT_DIG" "FLT_EPSILON"
    "DBL_DIG" "DBL_MANT_DIG" "DBL_EPSILON"
    "LDBL_DIG" "LDBL_MANT_DIG" "LDBL_EPSILON"
    "FLT_MIN" "FLT_MAX" "FLT_MIN_EXP" "FLT_MAX_EXP"
    "DBL_MIN" "DBL_MAX" "DBL_MIN_EXP" "DBL_MAX_EXP"
    "LDBL_MIN" "LDBL_MAX" "LDBL_MIN_EXP" "LDBL_MAX_EXP"
    "FLT_MIN_10_EXP" "FLT_MAX_10_EXP"
    "DBL_MIN_10_EXP" "DBL_MAX_10_EXP"
    ;; <stdio.h>
    "_IOFBF" "_IOLBF" "_IONBF" "BUFSIZ" "FOPEN_MAX" "FILENAME_MAX" "L_tmpnam"
    "SEEK_CUR" "SEEK_END" "SEEK_SET" "TMP_MAX"
    "stdin" "stdout" "stderr" "EOF" "WEOF"
    ;; <stdlib.h>
    "EXIT_FAILURE" "EXIT_SUCCESS" "RAND_MAX"
    ;; <math.h>
    "HUGE_VAL" "HUGE_VALF" "HUGE_VALL"
    ;; <time.h>
    "CLOCKS_PER_SEC"
    ;; <locale.h>
    "LC_ALL" "LC_COLLATE" "LC_CTYPE" "LC_MONETARY" "LC_NUMERIC" "LC_TIME"
    ;; <assert.h>
    "NDEBUG"
    ;; POSIX signals
    "SIG_ERR" "SIG_IGN" "SIGABRT" "SIGFPE" "SIGILL" "SIGHUP" "SIGINT" "SIGSEGV"
    "SIGTERM" "SIGABRT" "SIGALRM" "SIGCHLD" "SIGCONT" "SIGFPE" "SIGHUP" "SIGILL"
    "SIGINT" "SIGKILL" "SIGPIPE" "SIGQUIT" "SIGSEGV" "SIGSTOP" "SIGTERM"
    "SIGTRAP" "SIGTSTP" "SIGTTIN" "SIGTTOU" "SIGUSR1" "SIGUSR2"
    ;; Misc
    "__func__" "__LINE__" "__FILE__" "__DATE__" "__TIME__" "__STDC__"
    "__STDC_VERSION__" "__STDC_HOSTED__" "__VA_ARGS__")
  "List of (non-regexp) strings for building `x8dcc/c-constant-regexp' with
`regexp-opt'.

Got most of these constant names from \"$VIMRUNTIME/syntax/c.vim\".

Not included: NULL, true, false")

(defconst x8dcc/c-constant-regexp
  (concat (regexp-opt x8dcc/c-constant-list 'symbols))
  "Regular expression matching all constants in `x8dcc/c-constant-list'.

Used for highlighting more constants with `font-lock-constant-face' in
`c-mode'.")

(font-lock-add-keywords 'c-mode
  (list (cons x8dcc/c-constant-regexp 'font-lock-constant-face)))

(define-skeleton x8dcc/skeleton-c-source
  "Insert a basic C source skeleton with a main function."
  nil
  "\n"
  "#include <stdint.h>\n"
  "#include <stdbool.h>\n"
  "#include <stdio.h>\n"
  "#include <string.h>\n"
  "#include <stdlib.h>\n\n"
  "int main(void) {\n"
  > _ "\n"
  > "return 0;\n"
  "}" \n)

(define-skeleton x8dcc/skeleton-c-header
  "Insert a basic C header skeleton with include guards."
  nil
  '(setq str (skeleton-read "Header name: "))
  '(setq v1 (concat (upcase (if (or (null str)
                                    (string-empty-p str))
                                (file-name-base buffer-file-name)
                              str))
                    "_H_"))
  "\n"
  "#ifndef " v1 "\n"
  "#define " v1 " 1\n\n"
  _ "\n\n"
  "#endif /* " v1 " */" \n)

(define-skeleton x8dcc/skeleton-c-license-comment
  "Insert a basic C header skeleton with include guards."
  nil
  '(setq str
         (let ((project (project-current)))
           (skeleton-read "Project name: "
                          (and project (project-name project)))))
  "/*\n"
  " * Copyright " (format-time-string "%Y") " 8dcc\n"
  " *\n"
  " * This file is part of " str ".\n"
  " *\n"
  " * This program is free software: you can redistribute it and/or modify it under\n"
  " * the terms of the GNU General Public License as published by the Free Software\n"
  " * Foundation, either version 3 of the License, or any later version.\n"
  " *\n"
  " * This program is distributed in the hope that it will be useful, but WITHOUT\n"
  " * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS\n"
  " * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more\n"
  " * details.\n"
  " *\n"
  " * You should have received a copy of the GNU General Public License along with\n"
  " * this program. If not, see <https://www.gnu.org/licenses/>.\n"
  " */\n" \n)

(defun x8dcc/beardbolt-disassemble ()
  "Enable `beardbolt-mode' and call `beardbolt-compile'."
  (interactive)
  (beardbolt-mode 1)
  (call-interactively #'beardbolt-compile))

(setq gdb-many-windows t)

(setq gdb-restore-window-configuration-after-quit 'if-gdb-many-windows)

(setq gdb-debuginfod-enable-setting nil)

(setq gdb-window-configuration-directory
      (concat user-emacs-directory "gdb-layouts/"))

(setq compilation-scroll-output 'first-error)

(setq compilation-always-kill t)
