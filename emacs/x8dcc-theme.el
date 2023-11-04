;;; x8dcc-theme.el --- x8dcc
;;; Version: 1.0
;;; Commentary:
;;; A theme called x8dcc
;;; Code:

(deftheme x8dcc "Custom theme by 8dcc")
(custom-theme-set-faces 'x8dcc
  '(default             ((t (:foreground "#F8F8F2" :background "#272822"))))
  '(cursor              ((t (:background "#DDDDDD"))))
  '(fringe              ((t (:background "#272822"))))
  '(vertical-border     ((t (:foreground "#3E3F36"))))
  '(border              ((t (:foreground "#3E3F36"))))
  '(line-number         ((t (:foreground "#555556"))))
  '(mode-line           ((t (:foreground "#F8F8F2" :background "#161613"))))
  '(mode-line-inactive  ((t (:foreground "#4E4E4E" :background "#171819" :bold nil))))
  '(region              ((t (:background "#3A3A3A"))))
  '(secondary-selection ((t (:background "#3A3A3A"))))

  '(font-lock-builtin-face              ((t (:foreground "#FD971F"))))
  '(font-lock-comment-face              ((t (:foreground "#555556"))))
  '(font-lock-doc-face                  ((t (:foreground "#555556"))))
  '(font-lock-function-name-face        ((t (:foreground "#A6E22E"))))
  '(font-lock-keyword-face              ((t (:foreground "#F92660"))))
  '(font-lock-preprocessor-face         ((t (:foreground "#F92660" :bold t))))
  '(font-lock-string-face               ((t (:foreground "#E6DB74"))))
  '(font-lock-type-face                 ((t (:foreground "#66D9EF"))))
  '(font-lock-constant-face             ((t (:foreground "#9C91E4"))))
  '(font-lock-variable-name-face        ((t (:foreground "#F8F8F2"))))

  '(minibuffer-prompt      ((t (:foreground "#767679" :bold t))))
  '(font-lock-warning-face ((t (:foreground "#E6DB74" :bold t))))
  '(show-paren-match       ((t (:bold t))))
  '(show-paren-mismatch    ((t (:foreground "#F8F8F2" :background "#E74C3C" :bold t))))
  '(link                   ((t (:underline t)))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                    (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'x8dcc)

;;; x8dcc-theme.el ends here
