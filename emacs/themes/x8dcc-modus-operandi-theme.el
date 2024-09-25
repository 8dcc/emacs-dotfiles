;;; x8dcc-modus-operandi-theme.el --- Tritanopia-optimized theme with a white background -*- lexical-binding:t -*-

;; Copyright (C) 2019-2024  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/modus-themes
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

;(eval-and-compile
;  (unless (and (fboundp 'require-theme)
;               load-file-name
;               (equal (file-name-directory load-file-name)
;                      (expand-file-name "themes/" data-directory))
;               (require-theme 'modus-themes t))
;    (require 'modus-themes))

(eval-and-compile
  (load (concat user-emacs-directory "themes/modus-themes.el"))

;;;###theme-autoload
  (deftheme x8dcc-modus-operandi
    "Tritanopia-optimized theme with a white background.
This variant is optimized for users with blue-yellow color
deficiency (tritanopia).  It conforms with the highest
legibility standard for color contrast between background and
foreground in any given piece of text, which corresponds to a
minimum contrast in relative luminance of 7:1 (WCAG AAA
standard)."
    :background-mode 'light
    :kind 'color-scheme
    :family 'modus)

  (defconst x8dcc-modus-operandi-palette
    '(
;;; Basic values

      (bg-main          "#fbf7f0")
      (bg-dim           "#efe9dd")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c9b9b0")
      (bg-inactive      "#dfd5cf")
      (border           "#9f9690")

;;; Common accent foregrounds

      (red             "#a60000")
      (red-warmer      "#b21100")
      (red-cooler      "#a0132f")
      (red-faint       "#702000")
      (red-intense     "#d00000")
      (green           "#006800")
      (green-warmer    "#316500")
      (green-cooler    "#00663f")
      (green-faint     "#2a5045")
      (green-intense   "#008900")
      (yellow          "#695500")
      (yellow-warmer   "#973300")
      (yellow-cooler   "#77492f")
      (yellow-faint    "#624416")
      (yellow-intense  "#808000")
      (blue            "#0031a9")
      (blue-warmer     "#3548cf")
      (blue-cooler     "#0000b0")
      (blue-faint      "#003497")
      (blue-intense    "#0000ff")
      (magenta         "#721045")
      (magenta-warmer  "#8f0075")
      (magenta-cooler  "#531ab6")
      (magenta-faint   "#7c318f")
      (magenta-intense "#cd22bd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#004f5f")
      (cyan-intense    "#008899")

;;; Uncommon accent foregrounds

      (rust       "#8a290f")
      (gold       "#80601f")
      (olive      "#56692d")
      (slate      "#2f3f83")
      (indigo     "#4a3a8a")
      (maroon     "#731c52")
      (pink       "#7b435c")

;;; Common accent backgrounds

      (bg-red-intense     "#ff8f88")
      (bg-green-intense   "#8adf80")
      (bg-yellow-intense  "#f3d000")
      (bg-blue-intense    "#bfc9ff")
      (bg-magenta-intense "#dfa0f0")
      (bg-cyan-intense    "#a4d5f9")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#b3fabf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

      (bg-red-nuanced     "#ffe8e8")
      (bg-green-nuanced   "#e0f6e0")
      (bg-yellow-nuanced  "#f8f0d0")
      (bg-blue-nuanced    "#ecedff")
      (bg-magenta-nuanced "#f8e6f5")
      (bg-cyan-nuanced    "#e0f2fa")

;;; Uncommon accent backgrounds

      (bg-ochre    "#f0e0cc")
      (bg-lavender "#dfdbfa")
      (bg-sage     "#c0e7d4")

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#68c0a0")
      (bg-graph-green-1   "#a5dfd0")
      (bg-graph-yellow-0  "#d99f9f")
      (bg-graph-yellow-1  "#ffb58f")
      (bg-graph-blue-0    "#80a0df")
      (bg-graph-blue-1    "#a8cfff")
      (bg-graph-magenta-0 "#efafcf")
      (bg-graph-magenta-1 "#ffdaef")
      (bg-graph-cyan-0    "#7fd3ed")
      (bg-graph-cyan-1    "#afefff")

;;; Special purpose

      (bg-completion       "#f0c1cf")
      (bg-hover            "#b2e4dc")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#f1d5d0")
      (bg-region           "#c2bcb5")
      (fg-region           "#000000")

      (bg-char-0 "#7feaff")
      (bg-char-1 "#ffaaff")
      (bg-char-2 "#dff000")

      (bg-mode-line-active        "#cab9b2")
      (fg-mode-line-active        "#000000")
      (border-mode-line-active    "#545454")
      (bg-mode-line-inactive      "#dfd9cf")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a59a94")

      (modeline-err     "#7f0000")
      (modeline-warning "#5f0070")
      (modeline-info    "#002580")

      (bg-tab-bar      "#e0d4ce")
      (bg-tab-current  "#fbf7f0")
      (bg-tab-other    "#c8b8b2")

;;; Diffs

      (bg-added           "#C4D7B2")
      (bg-added-faint     "#D7E8C9")
      (bg-added-refine    "#9EC587")
      (bg-added-fringe    "#5BD75B")
      (fg-added           "#00381f")
      (fg-added-intense   "#002910")

      (bg-changed         "#b5e7ff")
      (bg-changed-faint   "#c6f6ff")
      (bg-changed-refine  "#9adcef")
      (bg-changed-fringe  "#1782cc")
      (fg-changed         "#005079")
      (fg-changed-intense "#0043aa")

      (bg-removed         "#ffd8d5")
      (bg-removed-faint   "#ffe9e9")
      (bg-removed-refine  "#f3b5af")
      (bg-removed-fringe  "#d84a4f")
      (fg-removed         "#8f1313")
      (fg-removed-intense "#aa2222")

      (bg-diff-context    "#f3f3f3")

;;; Paren match

      (bg-paren-match        undefined)
      (fg-paren-match        undefined)
      (bg-paren-expression   "#efd3f5")
      (bold-paren-match      t)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor fg-main)

      (keybind red)
      (name red-cooler)
      (identifier red-faint)

      (err red-warmer)
      (warning yellow-intense)
      (info green)

      (underline-err red-intense)
      (underline-warning magenta-intense)
      (underline-note green-intense)

      (bg-prominent-err bg-red-intense)
      (fg-prominent-err fg-main)
      (bg-prominent-warning bg-magenta-intense)
      (fg-prominent-warning fg-main)
      (bg-prominent-note bg-cyan-intense)
      (fg-prominent-note fg-main)

      (bg-active-argument bg-red-nuanced)
      (fg-active-argument red-warmer)
      (bg-active-value bg-cyan-nuanced)
      (fg-active-value cyan)

;;;; Code mappings

      (builtin magenta)
      (comment fg-dim)
      (constant green-cooler)
      (docstring comment)
      (docmarkup magenta-faint)
      (fnname cyan-warmer)
      (keyword red-cooler)
      (preprocessor red-warmer)
      (string cyan)
      (type blue-warmer)
      (variable cyan-cooler)
      (rx-construct red)
      (rx-backslash magenta)

;;;; Accent mappings

      (accent-0 cyan)
      (accent-1 red-warmer)
      (accent-2 cyan-cooler)
      (accent-3 magenta)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 cyan)
      (fg-completion-match-1 green-warmer)
      (fg-completion-match-2 magenta)
      (fg-completion-match-3 cyan-cooler)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common cyan-cooler)
      (date-deadline red)
      (date-event fg-alt)
      (date-holiday red)
      (date-holiday-other cyan)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled magenta)
      (date-weekday cyan)
      (date-weekend red-faint)

;;;; Line number mappings

      (fg-line-number-inactive fg-dim)
      (fg-line-number-active fg-main)
      (bg-line-number-inactive bg-dim)
      (bg-line-number-active bg-active)

;;;; Link mappings

      (fg-link cyan)
      (bg-link unspecified)
      (underline-link cyan)

      (fg-link-symbolic cyan-cooler)
      (bg-link-symbolic unspecified)
      (underline-link-symbolic cyan-cooler)

      (fg-link-visited magenta)
      (bg-link-visited unspecified)
      (underline-link-visited magenta)

;;;; Mail mappings

      (mail-cite-0 cyan-faint)
      (mail-cite-1 red-faint)
      (mail-cite-2 magenta-warmer)
      (mail-cite-3 cyan-warmer)
      (mail-part cyan-cooler)
      (mail-recipient cyan)
      (mail-subject red-cooler)
      (mail-other cyan)

;;;; Mark mappings

      (bg-mark-delete bg-red-subtle)
      (fg-mark-delete red)
      (bg-mark-select bg-cyan-subtle)
      (fg-mark-select cyan)
      (bg-mark-other bg-magenta-subtle)
      (fg-mark-other magenta)

;;;; Prompt mappings

      (fg-prompt cyan-cooler)
      (bg-prompt unspecified)

;;;; Prose mappings

      (prose-block fg-dim)
      (prose-code cyan)
      (prose-done fg-dim)
      (prose-macro red-warmer)
      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)
      (prose-table fg-alt)
      (prose-tag fg-alt)
      (prose-todo yellow-intense)
      (prose-verbatim magenta-warmer)

;;;; Rainbow mappings

      (rainbow-0 red)
      (rainbow-1 yellow-warmer)
      (rainbow-2 yellow)
      (rainbow-3 green)
      (rainbow-4 cyan)
      (rainbow-5 blue-intense)
      (rainbow-6 magenta-intense)
      (rainbow-7 magenta)
      (rainbow-8 "#a31746")

;;;; Search mappings

      (bg-search-current bg-red-intense)
      (bg-search-lazy bg-cyan-intense)
      (bg-search-replace bg-magenta-intense)

      (bg-search-rx-group-0 bg-blue-intense)
      (bg-search-rx-group-1 bg-magenta-intense)
      (bg-search-rx-group-2 bg-cyan-subtle)
      (bg-search-rx-group-3 bg-red-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-red-intense)

;;;; Terminal mappings

      (bg-term-black           "#000000")
      (fg-term-black           "#000000")
      (bg-term-black-bright    "#595959")
      (fg-term-black-bright    "#595959")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-warmer)
      (fg-term-yellow-bright   yellow-warmer)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-warmer)
      (fg-term-blue-bright     blue-warmer)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "#a6a6a6")
      (fg-term-white           "#a6a6a6")
      (bg-term-white-bright    "#ffffff")
      (fg-term-white-bright    "#ffffff")

;;;; Other custom colors (x8dcc)

      (org-src-block-bg "#F9F9F9")
      (fill-column-indicator-fg "#BFBFBF")

;;;; Heading mappings

      (fg-heading-0 cyan-cooler)
      (fg-heading-1 fg-main)
      (fg-heading-2 red-faint)
      (fg-heading-3 cyan-faint)
      (fg-heading-4 magenta)
      (fg-heading-5 green-faint)
      (fg-heading-6 magenta-faint)
      (fg-heading-7 cyan-warmer)
      (fg-heading-8 fg-dim)

      (bg-heading-0 unspecified)
      (bg-heading-1 unspecified)
      (bg-heading-2 unspecified)
      (bg-heading-3 unspecified)
      (bg-heading-4 unspecified)
      (bg-heading-5 unspecified)
      (bg-heading-6 unspecified)
      (bg-heading-7 unspecified)
      (bg-heading-8 unspecified)

      (overline-heading-0 unspecified)
      (overline-heading-1 unspecified)
      (overline-heading-2 unspecified)
      (overline-heading-3 unspecified)
      (overline-heading-4 unspecified)
      (overline-heading-5 unspecified)
      (overline-heading-6 unspecified)
      (overline-heading-7 unspecified)
      (overline-heading-8 unspecified))
    "The entire palette of the `x8dcc-modus-operandi' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom x8dcc-modus-operandi-palette-overrides nil
    "Overrides for `x8dcc-modus-operandi-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme x8dcc-modus-operandi
                      x8dcc-modus-operandi-palette
                      x8dcc-modus-operandi-palette-overrides)

  (provide-theme 'x8dcc-modus-operandi))

;;; x8dcc-modus-operandi-theme.el ends here
