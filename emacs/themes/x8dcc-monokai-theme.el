;;; x8dcc-monokai-theme.el --- x8dcc
;;; Version: 1.0
;;; Commentary:
;;; Custom monokai theme by x8dcc
;;; Code:

;; Small cheatsheet of the faces:
;;  Face name             | Description/usage
;; -----------------------|-----------------------------------------------------
;;  fringe                | Left border of a buffer, usually where the git diff lines are
;;  region                | Selected text (e.g. visual mode)
;;  minibuffer-prompt     | "M-x" and evil's ":" prompt text (not the user input)
;;  mode-line             | Face for the text in the modeline (bar above the minibuffer with the buffer name, modes, etc.)
;;  mode-line-highlight   | Face when hovering buttons in the modeline
;;  link                  | Face used by links, inherited by buttons
;;  widget-button-pressed | Face used by dashboard's buttons when being pressed (not hovered)
;;  highlight             | Face used when hovering buttons, for example
;;  shadow                | Colors used, for example, by the fill-column line
;;  isearch               | Face used for the current search match
;;  lazy-highlight        | Face used for the other search matches
;;  match                 | Face used for *Occur* matches (M-x occur)
;;  font-lock-*-face      | Faces for prog-mode token types
;;  show-paren-match      | Face for the open and closing parentheses that are being hovered
;;  term-color-*          | Colors used by terminals (not vterm or eshell)
;;  vterm-color-*         | Colors used by vterm
;;  outline-*             | Inherited by org-level-*
;;
;; TODO:
;;  border
;;  vertical-border
;;  menu

(deftheme x8dcc-monokai "Custom theme by 8dcc")
(let ((col-default        "#F8F8F2")
      (col-background     "#272822")
      (col-gray1          "#DDDDDD")
      (col-gray2          "#767679")
      (col-gray3          "#555556")
      (col-gray4          "#4E4E4E")
      (col-gray5          "#3A3A3A")
      (col-gray6          "#3E3F36")
      (col-gray7          "#222222")
      (col-gray8          "#171819")
      (col-gray9          "#161613")
      (col-black          "#000000")
      (col-white          "#FFFFFF")
      (col-red            "#E74C3C")
      (col-orange         "#FD971F")
      (col-yellow         "#E6DB74")
      (col-green          "#A6E22E")
      (col-cyan           "#66D9EF")
      (col-blue           "#268BD2")
      (col-purple         "#9C91E4")
      (col-magenta        "#F92660")
      (col-bright-red     "#ED786C")
      (col-bright-yellow  "#ECE396")
      (col-bright-green   "#BCE962")
      (col-bright-cyan    "#8CE2F3")
      (col-bright-blue    "#5CA8DD")
      (col-bright-magenta "#FA5C87")
      (col-term-back      "#1B2229"))
  (custom-theme-set-faces 'x8dcc-monokai
    `(default                  ((t (:foreground ,col-default :background ,col-background))))
    `(fringe                   ((t (:foreground ,col-gray3   :background ,col-background))))
    `(border                   ((t (:foreground ,col-gray6))))
    `(vertical-border          ((t (:foreground ,col-gray6))))
    `(cursor                   ((t (:background ,col-gray1))))
    `(region                   ((t (:background ,col-gray5))))
    `(secondary-selection      ((t (:background ,col-gray5))))
    `(minibuffer-prompt        ((t (:foreground ,col-gray2   :bold t))))
    `(line-number              ((t (:foreground ,col-gray3))))
    `(line-number-current-line ((t (:foreground ,col-gray2))))
    `(mode-line                ((t (:foreground ,col-default :background ,col-gray9))))
    `(mode-line-inactive       ((t (:foreground ,col-gray4   :background ,col-gray8 :bold nil))))
    `(mode-line-highlight      ((t (:foreground ,col-gray9   :background ,col-gray1))))
    `(link                     ((t (:underline t))))
    `(widget-button-pressed    ((t (:foreground ,col-background :background ,col-white :bold t))))

    `(success   ((t (:foreground ,col-green))))
    `(error     ((t (:foreground ,col-red))))
    `(warning   ((t (:foreground ,col-yellow))))
    `(highlight ((t (:foreground ,col-background :background ,col-gray1))))
    `(shadow    ((t (:foreground ,col-gray3))))

    `(isearch        ((t (:foreground ,col-background :background ,col-yellow))))
    `(lazy-highlight ((t (:foreground ,col-background :background ,col-yellow))))
    `(match          ((t (:foreground ,col-red :bold t))))

    `(font-lock-builtin-face       ((t (:foreground ,col-orange))))
    `(font-lock-comment-face       ((t (:foreground ,col-gray3))))
    `(font-lock-doc-face           ((t (:foreground ,col-gray3))))
    `(font-lock-function-name-face ((t (:foreground ,col-green))))
    `(font-lock-keyword-face       ((t (:foreground ,col-magenta))))
    `(font-lock-preprocessor-face  ((t (:foreground ,col-magenta :bold t))))
    `(font-lock-string-face        ((t (:foreground ,col-yellow))))
    `(font-lock-type-face          ((t (:foreground ,col-cyan))))
    `(font-lock-constant-face      ((t (:foreground ,col-purple))))
    `(font-lock-variable-name-face ((t (:foreground ,col-default))))
    `(font-lock-warning-face       ((t (:foreground ,col-yellow :bold t))))

    `(show-paren-match    ((t (:bold t))))
    `(show-paren-mismatch ((t (:foreground ,col-default :background ,col-red :bold t))))

    `(term-color-black          ((t (:foreground ,col-term-back))))
    `(term-color-blue           ((t (:foreground ,col-blue))))
    `(term-color-cyan           ((t (:foreground ,col-cyan))))
    `(term-color-green          ((t (:foreground ,col-green))))
    `(term-color-magenta        ((t (:foreground ,col-magenta))))
    `(term-color-red            ((t (:foreground ,col-red))))
    `(term-color-white          ((t (:foreground ,col-white))))
    `(term-color-yellow         ((t (:foreground ,col-yellow))))
    `(term-color-bright-black   ((t (:foreground ,col-gray3))))
    `(term-color-bright-blue    ((t (:foreground ,col-bright-blue))))
    `(term-color-bright-cyan    ((t (:foreground ,col-bright-cyan))))
    `(term-color-bright-green   ((t (:foreground ,col-bright-green))))
    `(term-color-bright-magenta ((t (:foreground ,col-bright-magenta))))
    `(term-color-bright-red     ((t (:foreground ,col-bright-red))))
    `(term-color-bright-white   ((t (:foreground ,col-white))))
    `(term-color-bright-yellow  ((t (:foreground ,col-bright-yellow))))
    `(vterm-color-black         ((t (:foreground ,col-term-back :background ,col-gray3))))
    `(vterm-color-blue          ((t (:foreground ,col-blue      :background ,col-bright-blue))))
    `(vterm-color-cyan          ((t (:foreground ,col-cyan      :background ,col-bright-cyan))))
    `(vterm-color-green         ((t (:foreground ,col-green     :background ,col-bright-green))))
    `(vterm-color-magenta       ((t (:foreground ,col-magenta   :background ,col-bright-magenta))))
    `(vterm-color-red           ((t (:foreground ,col-red       :background ,col-bright-red))))
    `(vterm-color-white         ((t (:foreground ,col-white     :background ,col-white))))
    `(vterm-color-yellow        ((t (:foreground ,col-yellow    :background ,col-bright-yellow))))

    `(vertico-current ((t (:background "#202020"))))

    `(outline-1 ((t (:foreground ,col-magenta :bold t))))
    `(outline-2 ((t (:foreground ,col-orange  :bold t))))
    `(outline-3 ((t (:foreground ,col-purple  :bold t))))
    `(outline-4 ((t (:foreground ,col-cyan    :bold t))))
    `(outline-5 ((t (:foreground ,col-green   :bold t))))
    `(outline-6 ((t (:foreground ,col-default :bold t))))
    `(outline-7 ((t (:foreground ,col-default :bold t))))
    `(outline-8 ((t (:foreground ,col-default :bold t))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                    (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'x8dcc-monokai)

;;; x8dcc-theme.el ends here
