;;; panic-theme.el --- A theme inspired by panic's Coda 2.5

;; Copyright (C) 2017

;; Author: Alexander Rojas
;; URL: http://github.com/sambatyon/panic-emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of the popular Textmate theme Panic for Emacs 24, built on top
;; of the new built-in theme support in Emacs 24.
;;
;;; Credits:
;;
;; Cabel Sasser created the original theme.
;; - https://panic.com/coda/
;;
;; Kelvin Smith created monokai-theme.el
;;  on which this file is based.
;; - http://github.com/oneKelvinSmith/monokai-emacs
;;
;; Paletton for complementary colours.
;; - http://paletton.com/
;;
;; Xterm 256 Color Chart
;; - https://upload.wikimedia.org/wikipedia/en/1/15/Xterm_256color_chart.svg
;;
;;; Code:

(unless (>= emacs-major-version 24)
  (error "The panic theme requires Emacs 24 or later!"))

(deftheme panic "The Panic colour theme")

(defgroup panic nil
  "Panic theme options.
The theme has to be reloaded after changing anything in this group."
  :group 'faces)

(defcustom panic-distinct-fringe-background nil
  "Make the fringe background different from the normal background color.
Also affects 'linum-mode' background."
  :type 'boolean
  :group 'panic)

(defcustom panic-use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'panic)

(defcustom panic-height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'panic)

(defcustom panic-height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'panic)

(defcustom panic-height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'panic)

(defcustom panic-height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'panic)

(defcustom panic-height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'panic)

;; Primary colors
(defcustom panic-yellow "#fee14f"
  "Primary colors - yellow"
  :type 'string
  :group 'panic)

(defcustom panic-orange "#fcc146"
  "Primary colors - orange"
  :type 'string
  :group 'panic)

(defcustom panic-red "#f82939"
  "Primary colors - red"
  :type 'string
  :group 'panic)

(defcustom panic-magenta "#F9454E"
  "Primary colors - magenta"
  :type 'string
  :group 'panic)

(defcustom panic-blue "#3c5874"
  "Primary colors - blue"
  :type 'string
  :group 'panic)

(defcustom panic-green "#3beb29"
  "Primary colors - green"
  :type 'string
  :group 'panic)

(defcustom panic-cyan "#63cce3"
  "Primary colors - cyan"
  :type 'string
  :group 'panic)

(defcustom panic-violet "#a982ff"
  "Primary colors - violet"
  :type 'string
  :group 'panic)

(defcustom panic-gray "#4f5b66"
  "Primary colors - gray"
  :type 'string
  :group 'panic)

(defcustom panic-foreground "#F8F8F2"
  "Adaptive colors - foreground"
  :type 'string
  :group 'panic)

(defcustom panic-background "#111c2a"
  "Adaptive colors - background"
  :type 'string
  :group 'panic)

(defcustom panic-comments "#838c95"
  "Adaptive colors - comments"
  :type 'string
  :group 'panic)

(defcustom panic-emphasis "#eff1f5"
  "Adaptive colors - emphasis"
  :type 'string
  :group 'panic)

(defcustom panic-line-number "#f6f7ee"
  "Adaptive colors - line number"
  :type 'string
  :group 'panic)

(defcustom panic-highlight "#115FA5"
  "Adaptive colors - highlight"
  :type 'string
  :group 'panic)

(defcustom panic-highlight-alt "#f6f7ee"
  "Adaptive colors - highlight"
  :type 'string
  :group 'panic)

(defcustom panic-highlight-line "#152638"
  "Adaptive colors - line highlight"
  :type 'string
  :group 'panic)

(let* (;; Variable pitch
       (panic-pitch (if panic-use-variable-pitch
                          'variable-pitch
                        'default))

       ;; Definitions for guis that support 256 colors
       (panic-class '((class color) (min-colors 257)))

       ;; Darker and lighter accented colors
       (panic-yellow-l                 "#FFE872")
       (panic-yellow-d                 "#ECCB26")
       (panic-orange-l                 "#FFCF69")
       (panic-orange-d                 "#EEA919")
       (panic-red-l                    "#FA505C")
       (panic-red-d                    "#D90414")
       (panic-magenta-l                "#FC686F")
       (panic-magenta-d                "#EB1923")
       (panic-blue-l                   "#5B748C")
       (panic-blue-d                   "#274460")
       (panic-green-l                  "#5FEF50")
       (panic-green-d                  "#16E600")
       (panic-cyan-l                   "#8EDDF0")
       (panic-cyan-d                   "#3FB6D2")
       (panic-violet-l                 "#C7ADFF")
       (panic-violet-d                 "#8F5BFF")
       (panic-gray-l                   "#69747E")
       (panic-gray-d                   "#38434D")
       ;; Adaptive higher/lower contrast accented colors
       (panic-background-l             "#1D2836")
       (panic-background-d             "#08111D")
       (panic-foreground-l             "#FFFFFE")
       (panic-foreground-d             "#CFCFC2")
       (panic-comments-l               "#ABB0B4")
       (panic-comments-d               "#64727E")
       (panic-emphasis-l               "#FEFEFF")
       (panic-emphasis-d               "#CED5E2")
       (panic-line-number-l            "#FCFCFA")
       (panic-line-number-d            "#CED0BE")
       (panic-highlight-l              "#3373AC")
       (panic-highlight-d              "#094983")
       (panic-highlight-alt-l          "#FCFCFA")
       (panic-highlight-alt-d          "#CED0BE")
       (panic-highlight-line-l         "#233446")
       (panic-highlight-line-d         "#0A1827")
       ;; High contrast colors
       (panic-yellow-hc                "#FFEF9D")
       (panic-yellow-lc                "#BC9F0E")
       (panic-orange-hc                "#FFDD94")
       (panic-orange-lc                "#B77F0A")
       (panic-red-hc                   "#FB7983")
       (panic-red-lc                   "#A9000C")
       (panic-magenta-hc               "#FD9398")
       (panic-magenta-lc               "#B60912")
       (panic-blue-hc                  "#8597A9")
       (panic-blue-lc                  "#132E48")
       (panic-green-hc                 "#8AF47E")
       (panic-green-lc                 "#13C700")
       (panic-cyan-hc                  "#C5F0FA")
       (panic-cyan-lc                  "#1F9DBB")
       (panic-violet-hc                "#EFE8FF")
       (panic-violet-lc                "#EFE8FF")
       (panic-gray-hc                  "#8C949B")
       (panic-gray-lc                  "#202D38")
       (panic-background-hc            "#343F4E")
       (panic-background-lc            "#020810")
       (panic-foreground-hc            "#FFFFFF")
       (panic-foreground-lc            "#ACAC96")
       (panic-comments-hc              "#E0E1E2")
       (panic-comments-lc              "#495A6A")
       (panic-emphasis-hc              "#FFFFFF")
       (panic-emphasis-lc              "#9FABC4")
       (panic-line-number-hc           "#FFFFFF")
       (panic-line-number-lc           "#ACAF91")
       (panic-highlight-hc             "#558DBF")
       (panic-highlight-lc             "#063967")
       (panic-highlight-alt-hc         "#FFFFFF")
       (panic-highlight-alt-lc         "#ACAF91")
       (panic-highlight-line-hc        "#3A4E62")
       (panic-highlight-line-lc        "#020C16")

       ;; Distinct fringe
       (panic-fringe-bg (if panic-distinct-fringe-background
                              panic-gray
                            panic-background))

       ;; Definitions for terminals that do not support 256 colors
       (panic-256-class '((class color) (min-colors 89)))
       ;; Primary colors
       (panic-256-yellow               "#ffd75f")
       (panic-256-orange               "#ffaf5f")
       (panic-256-red                  "#ff005f")
       (panic-256-magenta              "#ff5f5f")
       (panic-256-blue                 "#5f5f87")
       (panic-256-green                "#5fff00")
       (panic-256-cyan                 "#5fd7d7")
       (panic-256-violet               "#af87ff")
       (panic-256-gray                 "#5f5f5f")
       ;; Darker and lighter accented colors
       (panic-256-yellow-l             "#ffd75f")
       (panic-256-yellow-d             "#ffd700")
       (panic-256-orange-l             "#ffd75f")
       (panic-256-orange-d             "#ffaf00")
       (panic-256-red-l                "#ff5f5f")
       (panic-256-red-d                "#d70000")
       (panic-256-magenta-l            "#ff5f5f")
       (panic-256-magenta-d            "#ff0000")
       (panic-256-blue-l               "#5f8787")
       (panic-256-blue-d               "#005f5f")
       (panic-256-green-l              "#5fff5f")
       (panic-256-green-d              "#00d700")
       (panic-256-cyan-l               "#87d7ff")
       (panic-256-cyan-d               "#5fafd7")
       (panic-256-violet-l             "#d7afff")
       (panic-256-violet-d             "#875fff")
       (panic-256-gray-l               "#5f8787")
       (panic-256-gray-d               "#5f5f5f")
       ;; Adaptive colors
       (panic-256-background           "#000000")
       (panic-256-foreground           "#ffffff")
       (panic-256-comments             "#878787")
       (panic-256-emphasis             "#ffffff")
       (panic-256-line-number          "#ffffff")
       (panic-256-highlight            "#005faf")
       (panic-256-highlight-alt        "#ffffff")
       (panic-256-highlight-line       "#00005f")
       ;; Adaptive higher/lower contrast accented colors
       (panic-256-background-l         "#00005f")
       (panic-256-background-d         "#000000")
       (panic-256-foreground-l         "#ffffff")
       (panic-256-foreground-d         "#d7d7af")
       (panic-256-comments-l           "#afafaf")
       (panic-256-comments-d           "#5f5f87")
       (panic-256-emphasis-l           "#ffffff")
       (panic-256-emphasis-d           "#d7d7d7")
       (panic-256-line-number-l        "#ffffff")
       (panic-256-line-number-d        "#d7d7af")
       (panic-256-highlight-l          "#5f87af")
       (panic-256-highlight-d          "#005f87")
       (panic-256-highlight-alt-l      "#ffffff")
       (panic-256-highlight-alt-d      "#d7d7af")
       (panic-256-highlight-line-l     "#005f5f")
       (panic-256-highlight-line-d     "#000000")

       (panic-256-background-hc        "#5f5f5f")
       (panic-256-background-lc        "#000000")
       (panic-256-foreground-hc        "#ffffff")
       (panic-256-foreground-lc        "#afaf87")
       (panic-256-comments-hc          "#d7d7d7")
       (panic-256-comments-lc          "#5f5f5f")
       (panic-256-emphasis-hc          "#ffffff")
       (panic-256-emphasis-lc          "#afafd7")
       (panic-256-line-number-hc       "#ffffff")
       (panic-256-line-number-lc       "#afaf87")
       (panic-256-highlight-hc         "#5f87af")
       (panic-256-highlight-lc         "#005f5f")
       (panic-256-highlight-alt-hc     "#ffffff")
       (panic-256-highlight-alt-lc     "#afaf87")
       (panic-256-highlight-line-hc    "#5f5f5f")
       (panic-256-highlight-line-lc    "#000000")

       ;; High contrast colors
       (panic-256-yellow-hc      panic-256-yellow-d)
       (panic-256-yellow-lc      panic-256-yellow-l)
       (panic-256-orange-hc      panic-256-orange-d)
       (panic-256-orange-lc      panic-256-orange-l)
       (panic-256-red-hc         panic-256-red-d)
       (panic-256-red-lc         panic-256-red-l)
       (panic-256-magenta-hc     panic-256-magenta-d)
       (panic-256-magenta-lc     panic-256-magenta-l)
       (panic-256-violet-hc      panic-256-violet-d)
       (panic-256-violet-lc      panic-256-violet-l)
       (panic-256-blue-hc        panic-256-blue-d)
       (panic-256-blue-lc        panic-256-blue-l)
       (panic-256-cyan-hc        panic-256-cyan-d)
       (panic-256-cyan-lc        panic-256-cyan-l)
       (panic-256-green-hc       panic-256-green-d)
       (panic-256-green-lc       panic-256-green-l)

       ;; Distinct fringe
       (panic-256-fringe-bg (if panic-distinct-fringe-background
                                  panic-256-gray
                                panic-256-background)))

  ;; Define faces
  (custom-theme-set-faces
   'panic

   ;; font lock for syntax highlighting
   `(font-lock-builtin-face
     ((,panic-class (:foreground ,panic-red
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight normal))))

   `(font-lock-comment-delimiter-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(font-lock-comment-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(font-lock-constant-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(font-lock-doc-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(font-lock-function-name-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(font-lock-keyword-face
     ((,panic-class (:foreground ,panic-red
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight normal))))

   `(font-lock-negation-char-face
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(font-lock-preprocessor-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(font-lock-regexp-grouping-construct
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal))))

   `(font-lock-regexp-grouping-backslash
     ((,panic-class (:foreground ,panic-violet
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :weight normal))))

   `(font-lock-string-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(font-lock-type-face
     ((,panic-class (:foreground ,panic-blue
                                   :italic nil))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :italic nil))))

   `(font-lock-variable-name-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(font-lock-warning-face
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold
                                   :italic t
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold
                                        :italic t
                                        :underline t))))

   `(c-annotation-face
     ((,panic-class (:inherit font-lock-constant-face))
      (,panic-256-class  (:inherit font-lock-constant-face))))

   ;; general colouring
   '(button ((t (:underline t))))

   `(default
      ((,panic-class (:foreground ,panic-foreground
                                    :background ,panic-background))
       (,panic-256-class  (:foreground ,panic-256-foreground
                                         :background ,panic-256-background))))

   `(highlight
     ((,panic-class (:background ,panic-highlight))
      (,panic-256-class  (:background ,panic-256-highlight))))

   `(lazy-highlight
     ((,panic-class (:inherit highlight
                                :background ,panic-highlight-alt))
      (,panic-256-class  (:inherit highlight
                                     :background ,panic-256-comments))))

   `(region
     ((,panic-class (:inherit highlight
                                :background ,panic-highlight))
      (,panic-256-class  (:inherit highlight
                                     :background ,panic-256-highlight))))

   `(secondary-selection
     ((,panic-class (:inherit region
                                :background ,panic-highlight-alt))
      (,panic-256-class  (:inherit region
                                     :background ,panic-256-highlight-alt))))

   `(shadow
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(match
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background
                                        :weight bold))))

   `(cursor
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-foreground
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-foreground
                                        :inverse-video t))))

   `(mouse
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-foreground
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-foreground
                                        :inverse-video t))))

   `(escape-glyph
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(escape-glyph-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(fringe
     ((,panic-class (:foreground ,panic-foreground
                                   :background ,panic-fringe-bg))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :background ,panic-256-fringe-bg))))

   `(link
     ((,panic-class (:foreground ,panic-blue
                                   :underline t
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline t
                                        :weight bold))))

   `(link-visited
     ((,panic-class (:foreground ,panic-violet
                                   :underline t
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :underline t
                                        :weight normal))))

   `(success
     ((,panic-class (:foreground ,panic-green ))
      (,panic-256-class  (:foreground ,panic-256-green ))))

   `(warning
     ((,panic-class (:foreground ,panic-yellow ))
      (,panic-256-class  (:foreground ,panic-256-yellow ))))

   `(error
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(eval-sexp-fu-flash
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-green))))

   `(eval-sexp-fu-flash-error
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-red))))

   `(trailing-whitespace
     ((,panic-class (:background ,panic-red))
      (,panic-256-class  (:background ,panic-256-red))))

   `(vertical-border
     ((,panic-class (:foreground ,panic-gray))
      (,panic-256-class  (:foreground ,panic-256-gray))))

   `(menu
     ((,panic-class (:foreground ,panic-foreground
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :background ,panic-256-background))))

   `(minibuffer-prompt
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   ;; mode-line and powerline
   `(mode-line-buffer-id
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(mode-line
     ((,panic-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,panic-emphasis
                                      :background ,panic-highlight
                                      :box (:line-width 1
                                                        :color ,panic-gray
                                                        :style unspecified)))
      (,panic-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,panic-256-foreground
                                           :background ,panic-256-background
                                           :box (:line-width 1
                                                             :color ,panic-256-highlight
                                                             :style unspecified)))))

   `(powerline-active1
     ((,panic-class (:background ,panic-gray-d))
      (,panic-256-class  (:background ,panic-256-gray-d))))

   `(powerline-active2
     ((,panic-class (:background ,panic-background))
      (,panic-256-class  (:background ,panic-256-background))))


   `(mode-line-inactive
     ((,panic-class (:inverse-video unspecified
                                      :underline unspecified
                                      :foreground ,panic-comments
                                      :background ,panic-background
                                      :box (:line-width 1
                                                        :color ,panic-gray
                                                        :style unspecified)))
      (,panic-256-class  (:inverse-video unspecified
                                           :underline unspecified
                                           :foreground ,panic-256-comments
                                           :background ,panic-256-background
                                           :box (:line-width 1
                                                             :color ,panic-256-gray
                                                             :style unspecified)))))

   `(powerline-inactive1
     ((,panic-class (:background ,panic-gray-d))
      (,panic-256-class  (:background ,panic-256-gray-d))))

   `(powerline-inactive2
     ((,panic-class (:background ,panic-background))
      (,panic-256-class  (:background ,panic-256-background))))

   ;; header-line
   `(header-line
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-highlight
                                   :box (:color ,panic-gray
                                                :line-width 1
                                                :style unspecified)))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-highlight
                                        :box (:color ,panic-256-gray
                                                     :line-width 1
                                                     :style unspecified)))))

   ;; cua
   `(cua-global-mark
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-background))))

   `(cua-rectangle
     ((,panic-class (:inherit region))
      (,panic-256-class  (:inherit region))))

   `(cua-rectangle-noselect
     ((,panic-class (:inherit secondary-selection))
      (,panic-256-class  (:inherit secondary-selection))))

   ;; diary
   `(diary
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   ;; dired
   `(dired-directory
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(dired-flagged
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(dired-header
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-background
                                        :inherit bold))))

   `(dired-ignored
     ((,panic-class (:inherit shadow))
      (,panic-256-class  (:inherit shadow))))

   `(dired-mark
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(dired-marked
     ((,panic-class (:foreground ,panic-violet
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :inherit bold))))

   `(dired-perm-write
     ((,panic-class (:foreground ,panic-foreground
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :underline t))))

   `(dired-symlink
     ((,panic-class (:foreground ,panic-cyan
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :slant italic))))

   `(dired-warning
     ((,panic-class (:foreground ,panic-orange
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :underline t))))

   ;; dropdown
   `(dropdown-list-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-blue))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-blue))))

   `(dropdown-list-selection-face
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background))))

   ;; ecb
   `(ecb-default-highlight-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(ecb-history-bucket-node-dir-soure-path-face
     ((,panic-class (:inherit ecb-history-bucket-node-face
                                :foreground ,panic-yellow))
      (,panic-256-class  (:inherit ecb-history-bucket-node-face
                                     :foreground ,panic-256-yellow))))

   `(ecb-source-in-directories-buffer-face
     ((,panic-class (:inherit ecb-directories-general-face
                                :foreground ,panic-foreground))
      (,panic-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,panic-256-foreground))))

   `(ecb-history-dead-buffer-face
     ((,panic-class (:inherit ecb-history-general-face
                                :foreground ,panic-comments))
      (,panic-256-class  (:inherit ecb-history-general-face
                                     :foreground ,panic-256-comments))))

   `(ecb-directory-not-accessible-face
     ((,panic-class (:inherit ecb-directories-general-face
                                :foreground ,panic-comments))
      (,panic-256-class  (:inherit ecb-directories-general-face
                                     :foreground ,panic-256-comments))))

   `(ecb-bucket-node-face
     ((,panic-class (:inherit ecb-default-general-face
                                :weight normal
                                :foreground ,panic-blue))
      (,panic-256-class  (:inherit ecb-default-general-face
                                     :weight normal
                                     :foreground ,panic-256-blue))))

   `(ecb-tag-header-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(ecb-analyse-bucket-element-face
     ((,panic-class (:inherit ecb-analyse-general-face
                                :foreground ,panic-green))
      (,panic-256-class  (:inherit ecb-analyse-general-face
                                     :foreground ,panic-256-green))))

   `(ecb-directories-general-face
     ((,panic-class (:inherit ecb-default-general-face
                                :height 1.0))
      (,panic-256-class  (:inherit ecb-default-general-face
                                     :height 1.0))))

   `(ecb-method-non-semantic-face
     ((,panic-class (:inherit ecb-methods-general-face
                                :foreground ,panic-cyan))
      (,panic-256-class  (:inherit ecb-methods-general-face
                                     :foreground ,panic-256-cyan))))

   `(ecb-mode-line-prefix-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(ecb-tree-guide-line-face
     ((,panic-class (:inherit ecb-default-general-face
                                :foreground ,panic-gray
                                :height 1.0))
      (,panic-256-class  (:inherit ecb-default-general-face
                                     :foreground ,panic-256-gray
                                     :height 1.0))))

   ;; ee
   `(ee-bookmarked
     ((,panic-class (:foreground ,panic-emphasis))
      (,panic-256-class  (:foreground ,panic-256-emphasis))))

   `(ee-category
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(ee-link
     ((,panic-class (:inherit link))
      (,panic-256-class  (:inherit link))))

   `(ee-link-visited
     ((,panic-class (:inherit link-visited))
      (,panic-256-class  (:inherit link-visited))))

   `(ee-marked
     ((,panic-class (:foreground ,panic-magenta
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :weight bold))))

   `(ee-omitted
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(ee-shadow
     ((,panic-class (:inherit shadow))
      (,panic-256-class  (:inherit shadow))))

   ;; grep
   `(grep-context-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(grep-error-face
     ((,panic-class (:foreground ,panic-red
                                   :weight bold
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold
                                        :underline t))))

   `(grep-hit-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(grep-match-face
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   ;; isearch
   `(isearch
     ((,panic-class (:inherit region
                                :background ,panic-green))
      (,panic-256-class  (:inherit region
                                     :background ,panic-256-green))))

   `(isearch-fail
     ((,panic-class (:inherit isearch
                                :foreground ,panic-red
                                :background ,panic-background
                                :bold t))
      (,panic-256-class  (:inherit isearch
                                     :foreground ,panic-256-red
                                     :background ,panic-256-background
                                     :bold t))))


   ;; ace-jump-mode
   `(ace-jump-face-background
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-background
                                   :inverse-video nil))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-background
                                        :inverse-video nil))))

   `(ace-jump-face-foreground
     ((,panic-class (:foreground ,panic-yellow
                                   :background ,panic-background
                                   :inverse-video nil
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background ,panic-256-background
                                        :inverse-video nil
                                        :weight bold))))

   ;; auctex
   `(font-latex-bold-face
     ((,panic-class (:inherit bold
                                :foreground ,panic-emphasis))
      (,panic-256-class  (:inherit bold
                                     :foreground ,panic-256-emphasis))))

   `(font-latex-doctex-documentation-face
     ((,panic-class (:background unspecified))
      (,panic-256-class  (:background unspecified))))

   `(font-latex-doctex-preprocessor-face
     ((,panic-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))
      (,panic-class
       (:inherit (font-latex-doctex-documentation-face
                  font-lock-builtin-face
                  font-lock-preprocessor-face)))))

   `(font-latex-italic-face
     ((,panic-class (:inherit italic :foreground ,panic-emphasis))
      (,panic-256-class  (:inherit italic :foreground ,panic-256-emphasis))))

   `(font-latex-math-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(font-latex-sectioning-0-face
     ((,panic-class (:inherit font-latex-sectioning-1-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit font-latex-sectioning-1-face
                                     :height ,panic-height-plus-1))))

   `(font-latex-sectioning-1-face
     ((,panic-class (:inherit font-latex-sectioning-2-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit font-latex-sectioning-2-face
                                     :height ,panic-height-plus-1))))

   `(font-latex-sectioning-2-face
     ((,panic-class (:inherit font-latex-sectioning-3-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit font-latex-sectioning-3-face
                                     :height ,panic-height-plus-1))))

   `(font-latex-sectioning-3-face
     ((,panic-class (:inherit font-latex-sectioning-4-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit font-latex-sectioning-4-face
                                     :height ,panic-height-plus-1))))

   `(font-latex-sectioning-4-face
     ((,panic-class (:inherit font-latex-sectioning-5-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit font-latex-sectioning-5-face
                                     :height ,panic-height-plus-1))))

   `(font-latex-sectioning-5-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-yellow
                                :weight bold))
      (,panic-256-class  (:inherit ,panic-pitch :
                                     foreground ,panic-256-yellow
                                     :weight bold))))

   `(font-latex-sedate-face
     ((,panic-class (:foreground ,panic-emphasis))
      (,panic-256-class  (:foreground ,panic-256-emphasis))))

   `(font-latex-slide-title-face
     ((,panic-class (:inherit (,panic-pitch font-lock-type-face)
                                :weight bold
                                :height ,panic-height-plus-3))
      (,panic-256-class  (:inherit (,panic-pitch font-lock-type-face)
                                     :weight bold
                                     :height ,panic-height-plus-3))))

   `(font-latex-string-face
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(font-latex-subscript-face
     ((,panic-class (:height ,panic-height-minus-1))
      (,panic-256-class  (:height ,panic-height-minus-1))))

   `(font-latex-superscript-face
     ((,panic-class (:height ,panic-height-minus-1))
      (,panic-256-class  (:height ,panic-height-minus-1))))

   `(font-latex-verbatim-face
     ((,panic-class (:inherit fixed-pitch
                                :foreground ,panic-foreground
                                :slant italic))
      (,panic-256-class  (:inherit fixed-pitch
                                     :foreground ,panic-256-foreground
                                     :slant italic))))

   `(font-latex-warning-face
     ((,panic-class (:inherit bold
                                :foreground ,panic-orange))
      (,panic-256-class  (:inherit bold
                                     :foreground ,panic-256-orange))))

   ;; auto-complete
   `(ac-candidate-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-blue))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-blue))))

   `(ac-selection-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(ac-candidate-mouse-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(ac-completion-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :underline t))))

   `(ac-gtags-candidate-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-blue))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-blue))))

   `(ac-gtags-selection-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(ac-yasnippet-candidate-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-yellow))))

   `(ac-yasnippet-selection-face
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-background))))

   ;; auto highlight symbol
   `(ahs-definition-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-blue))))

   `(ahs-edit-mode-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-highlight))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-highlight))))

   `(ahs-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :background unspecified))))

   `(ahs-plugin-bod-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-violet ))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-cyan ))))

   `(ahs-plugin-defalt-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-orange))))

   `(ahs-plugin-whole-buffer-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-green))))

   `(ahs-warning-face
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold))))

   ;; android mode
   `(android-mode-debug-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(android-mode-error-face
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold))))

   `(android-mode-info-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(android-mode-verbose-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(android-mode-warning-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   ;; anzu-mode
   `(anzu-mode-line
     ((,panic-class (:foreground ,panic-violet
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :weight bold))))

   ;; bm
   `(bm-face
     ((,panic-class (:background ,panic-yellow-lc
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow-lc
                                        :foreground ,panic-256-background))))

   `(bm-fringe-face
     ((,panic-class (:background ,panic-yellow-lc
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow-lc
                                        :foreground ,panic-256-background))))

   `(bm-fringe-persistent-face
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-background))))

   `(bm-persistent-face
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-background))))

   ;; calfw
   `(cfw:face-day-title
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(cfw:face-annotation
     ((,panic-class (:inherit cfw:face-day-title
                                :foreground ,panic-yellow))
      (,panic-256-class  (:inherit cfw:face-day-title
                                     :foreground ,panic-256-yellow))))

   `(cfw:face-default-content
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(cfw:face-default-day
     ((,panic-class (:inherit cfw:face-day-title
                                :weight bold))
      (,panic-256-class  (:inherit cfw:face-day-title
                                     :weight bold))))

   `(cfw:face-disable
     ((,panic-class (:inherit cfw:face-day-title
                                :foreground ,panic-comments))
      (,panic-256-class  (:inherit cfw:face-day-title
                                     :foreground ,panic-256-comments))))

   `(cfw:face-grid
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(cfw:face-header
     ((,panic-class (:foreground ,panic-blue-hc
                                   :background ,panic-blue-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue-hc
                                        :background ,panic-256-blue-lc
                                        :weight bold))))

   `(cfw:face-holiday
     ((,panic-class (:background nil
                                   :foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:background nil
                                        :foreground ,panic-256-red
                                        :weight bold))))

   `(cfw:face-periods
     ((,panic-class (:foreground ,panic-magenta))
      (,panic-256-class  (:foreground ,panic-256-magenta))))

   `(cfw:face-select
     ((,panic-class (:background ,panic-magenta-lc
                                   :foreground ,panic-magenta-hc))
      (,panic-256-class  (:background ,panic-256-magenta-lc
                                        :foreground ,panic-256-magenta-hc))))

   `(cfw:face-saturday
     ((,panic-class (:foreground ,panic-cyan-hc
                                   :background ,panic-cyan-lc))
      (,panic-256-class  (:foreground ,panic-256-cyan-hc
                                        :background ,panic-256-cyan-lc))))

   `(cfw:face-sunday
     ((,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold))))

   `(cfw:face-title
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-yellow
                                :weight bold
                                :height ,panic-height-plus-4))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-yellow
                                     :weight bold
                                     :height ,panic-height-plus-4))))

   `(cfw:face-today
     ((,panic-class (:weight bold
                               :background ,panic-highlight-line
                               :foreground nil))
      (,panic-256-class  (:weight bold
                                    :background ,panic-256-highlight-line
                                    :foreground nil))))

   `(cfw:face-today-title
     ((,panic-class (:background ,panic-yellow-lc
                                   :foreground ,panic-yellow-hc
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-yellow-lc
                                        :foreground ,panic-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   `(cfw:face-toolbar-button-off
     ((,panic-class (:background ,panic-yellow-lc
                                   :foreground ,panic-yellow-hc
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-yellow-lc
                                        :foreground ,panic-256-yellow-hc
                                        :weight bold))))

   `(cfw:face-toolbar-button-on
     ((,panic-class (:background ,panic-yellow-hc
                                   :foreground ,panic-yellow-lc
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-yellow-hc
                                        :foreground ,panic-256-yellow-lc
                                        :weight bold))))

   ;; cider
   `(cider-enlightened
     ((,panic-class (:foreground ,panic-yellow
                                   :background nil
                                   :box (:color ,panic-yellow :line-width -1 :style nil)))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background nil
                                        :box (:color ,panic-256-yellow :line-width -1 :style nil))) ))

   `(cider-enlightened-local
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(cider-instrumented-face
     ((,panic-class (:foreground ,panic-violet
                                   :background nil
                                   :box (:color ,panic-violet :line-width -1 :style nil)))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :background nil
                                        :box (:color ,panic-256-violet :line-width -1 :style nil)))))

   `(cider-result-overlay-face
     ((,panic-class (:foreground ,panic-blue
                                   :background nil
                                   :box (:color ,panic-blue :line-width -1 :style nil)))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background nil
                                        :box (:color ,panic-256-blue :line-width -1 :style nil)))))

   `(cider-test-error-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-orange))))

   `(cider-test-failure-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-red))))

   `(cider-test-success-face
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-green))))

   `(cider-traced-face
     ((,panic-class :box (:color ,panic-blue :line-width -1 :style nil))
      (,panic-256-class  :box (:color ,panic-256-blue :line-width -1 :style nil))))

   ;; clojure-test
   `(clojure-test-failure-face
     ((,panic-class (:foreground ,panic-red
                                   :weight bold
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-error-face
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold
                                        :underline t))))

   `(clojure-test-success-face
     ((,panic-class (:foreground ,panic-green
                                   :weight bold
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold
                                        :underline t))))

   ;; company-mode
   `(company-tooltip
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis))))

   `(company-tooltip-selection
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(company-tooltip-mouse
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(company-tooltip-common
     ((,panic-class (:foreground ,panic-blue
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline t))))

   `(company-tooltip-common-selection
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-blue
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-blue
                                        :underline t))))

   `(company-preview
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis))))

   `(company-preview-common
     ((,panic-class (:foreground ,panic-blue
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline t))))

   `(company-scrollbar-bg
     ((,panic-class (:background ,panic-gray))
      (,panic-256-class  (:background ,panic-256-gray))))

   `(company-scrollbar-fg
     ((,panic-class (:background ,panic-comments))
      (,panic-256-class  (:background ,panic-256-comments))))

   `(company-tooltip-annotation
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-green))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-green))))

   `(company-template-field
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-blue))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-blue))))

   ;; compilation
   `(compilation-column-face
     ((,panic-class (:foreground ,panic-cyan
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :underline nil))))

   `(compilation-column-number
     ((,panic-class (:inherit font-lock-doc-face
                                :foreground ,panic-cyan
                                :underline nil))
      (,panic-256-class  (:inherit font-lock-doc-face
                                     :foreground ,panic-256-cyan
                                     :underline nil))))

   `(compilation-enter-directory-face
     ((,panic-class (:foreground ,panic-green
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :underline nil))))

   `(compilation-error
     ((,panic-class (:inherit error
                                :underline nil))
      (,panic-256-class  (:inherit error
                                     :underline nil))))

   `(compilation-error-face
     ((,panic-class (:foreground ,panic-red
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :underline nil))))

   `(compilation-face
     ((,panic-class (:foreground ,panic-foreground
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :underline nil))))

   `(compilation-info
     ((,panic-class (:foreground ,panic-comments
                                   :underline nil
                                   :bold nil))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :underline nil
                                        :bold nil))))

   `(compilation-info-face
     ((,panic-class (:foreground ,panic-blue
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline nil))))

   `(compilation-leave-directory-face
     ((,panic-class (:foreground ,panic-green
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :underline nil))))

   `(compilation-line-face
     ((,panic-class (:foreground ,panic-green
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :underline nil))))

   `(compilation-line-number
     ((,panic-class (:foreground ,panic-green
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :underline nil))))

   `(compilation-warning
     ((,panic-class (:inherit warning
                                :underline nil))
      (,panic-256-class  (:inherit warning
                                     :underline nil))))

   `(compilation-warning-face
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(compilation-mode-line-exit
     ((,panic-class (:inherit compilation-info
                                :foreground ,panic-green
                                :weight bold))
      (,panic-256-class  (:inherit compilation-info
                                     :foreground ,panic-256-green
                                     :weight bold))))

   `(compilation-mode-line-fail
     ((,panic-class (:inherit compilation-error
                                :foreground ,panic-red
                                :weight bold))
      (,panic-256-class  (:inherit compilation-error
                                     :foreground ,panic-256-red
                                     :weight bold))))

   `(compilation-mode-line-run
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold))))

   ;; CSCOPE
   `(cscope-file-face
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(cscope-function-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(cscope-line-number-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(cscope-line-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(cscope-mouse-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-foreground))))

   ;; ctable
   `(ctbl:face-cell-select
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :underline ,panic-emphasis
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :underline ,panic-256-emphasis
                                        :weight bold))))

   `(ctbl:face-continue-bar
     ((,panic-class (:background ,panic-gray
                                   :foreground ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-gray
                                        :foreground ,panic-256-yellow))))

   `(ctbl:face-row-select
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground
                                   :underline t))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground
                                        :underline t))))

   ;; coffee
   `(coffee-mode-class-name
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(coffee-mode-function-param
     ((,panic-class (:foreground ,panic-violet
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :slant italic))))

   ;; custom
   `(custom-face-tag
     ((,panic-class (:inherit ,panic-pitch
                                :height ,panic-height-plus-3
                                :foreground ,panic-violet
                                :weight bold))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :height ,panic-height-plus-3
                                     :foreground ,panic-256-violet
                                     :weight bold))))

   `(custom-variable-tag
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-cyan
                                :height ,panic-height-plus-3))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-cyan
                                     :height ,panic-height-plus-3))))

   `(custom-comment-tag
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(custom-group-tag
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-blue
                                :height ,panic-height-plus-3))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-blue
                                     :height ,panic-height-plus-3))))

   `(custom-group-tag-1
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-red
                                :height ,panic-height-plus-3))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-red
                                     :height ,panic-height-plus-3))))

   `(custom-state
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   ;; diff
   `(diff-added
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background))))

   `(diff-changed
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-background))))

   `(diff-removed
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background))))

   `(diff-header
     ((,panic-class (:background ,panic-background))
      (,panic-256-class  (:background ,panic-256-background))))

   `(diff-file-header
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground
                                        :weight bold))))

   `(diff-refine-added
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-green))))

   `(diff-refine-change
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-blue))))

   `(diff-refine-removed
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-red))))

   ;; diff-hl
   `(diff-hl-change
     ((,panic-class (:background ,panic-blue-lc
                                   :foreground ,panic-blue-hc))
      (,panic-256-class  (:background ,panic-256-blue-lc
                                        :foreground ,panic-256-blue-hc))))

   `(diff-hl-delete
     ((,panic-class (:background ,panic-red-lc
                                   :foreground ,panic-red-hc))
      (,panic-256-class  (:background ,panic-256-red-lc
                                        :foreground ,panic-256-red-hc))))

   `(diff-hl-insert
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-green-hc))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-green-hc))))

   `(diff-hl-unknown
     ((,panic-class (:background ,panic-violet-lc
                                   :foreground ,panic-violet-hc))
      (,panic-256-class  (:background ,panic-256-violet-lc
                                        :foreground ,panic-256-violet-hc))))

   ;; ediff
   `(ediff-fine-diff-A
     ((,panic-class (:background ,panic-orange-lc))
      (,panic-256-class  (:background ,panic-256-orange-lc))))

   `(ediff-fine-diff-B
     ((,panic-class (:background ,panic-green-lc))
      (,panic-256-class  (:background ,panic-256-green-lc))))

   `(ediff-fine-diff-C
     ((,panic-class (:background ,panic-yellow-lc))
      (,panic-256-class  (:background ,panic-256-yellow-lc))))

   `(ediff-current-diff-C
     ((,panic-class (:background ,panic-blue-lc))
      (,panic-256-class  (:background ,panic-256-blue-lc))))

   `(ediff-even-diff-A
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-foreground-lc ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-foreground-lc ))))

   `(ediff-odd-diff-A
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-foreground-hc ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-foreground-hc ))))

   `(ediff-even-diff-B
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-foreground-hc ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-foreground-hc ))))

   `(ediff-odd-diff-B
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-foreground-lc ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-foreground-lc ))))

   `(ediff-even-diff-C
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-foreground ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-foreground ))))

   `(ediff-odd-diff-C
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-background ))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-background ))))

   ;; edts
   `(edts-face-error-line
     ((,(append '((supports :underline (:style line))) panic-class)
       (:underline (:style line :color ,panic-red)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) panic-256-class )
       (:underline (:style line :color ,panic-256-red)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-warning-line
     ((,(append '((supports :underline (:style line))) panic-class)
       (:underline (:style line :color ,panic-yellow)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-yellow-hc
                                   :background ,panic-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style line))) panic-256-class )
       (:underline (:style line :color ,panic-256-yellow)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-yellow-hc
                                        :background ,panic-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(edts-face-error-fringe-bitmap
     ((,panic-class (:foreground ,panic-red
                                   :background unspecified
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-warning-fringe-bitmap
     ((,panic-class (:foreground ,panic-yellow
                                   :background unspecified
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background unspecified
                                        :weight bold))))

   `(edts-face-error-mode-line
     ((,panic-class (:background ,panic-red
                                   :foreground unspecified))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground unspecified))))

   `(edts-face-warning-mode-line
     ((,panic-class (:background ,panic-yellow
                                   :foreground unspecified))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground unspecified))))


   ;; elfeed
   `(elfeed-search-date-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(elfeed-search-feed-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(elfeed-search-tag-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(elfeed-search-title-face
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   ;; ein
   `(ein:cell-input-area
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))
   `(ein:cell-input-prompt
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))
   `(ein:cell-output-prompt
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))
   `(ein:notification-tab-normal
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))
   `(ein:notification-tab-selected
     ((,panic-class (:foreground ,panic-orange :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-orange :inherit bold))))

   ;; enhanced ruby mode
   `(enh-ruby-string-delimiter-face
     ((,panic-class (:inherit font-lock-string-face))
      (,panic-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-heredoc-delimiter-face
     ((,panic-class (:inherit font-lock-string-face))
      (,panic-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-regexp-delimiter-face
     ((,panic-class (:inherit font-lock-string-face))
      (,panic-256-class  (:inherit font-lock-string-face))))

   `(enh-ruby-op-face
     ((,panic-class (:inherit font-lock-keyword-face))
      (,panic-256-class  (:inherit font-lock-keyword-face))))

   ;; erm-syn
   `(erm-syn-errline
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-red)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-red)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(erm-syn-warnline
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-orange)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-orange-hc
                                   :background ,panic-orange-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-orange)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-orange-hc
                                        :background ,panic-256-orange-lc
                                        :weight bold
                                        :underline t))))

   ;; epc
   `(epc:face-title
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-background
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-background
                                        :weight normal
                                        :underline nil))))

   ;; erc
   `(erc-action-face
     ((,panic-class (:inherit erc-default-face))
      (,panic-256-class  (:inherit erc-default-face))))

   `(erc-bold-face
     ((,panic-class (:weight bold))
      (,panic-256-class  (:weight bold))))

   `(erc-current-nick-face
     ((,panic-class (:foreground ,panic-blue :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight bold))))

   `(erc-dangerous-host-face
     ((,panic-class (:inherit font-lock-warning-face))
      (,panic-256-class  (:inherit font-lock-warning-face))))

   `(erc-default-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(erc-highlight-face
     ((,panic-class (:inherit erc-default-face
                                :background ,panic-highlight))
      (,panic-256-class  (:inherit erc-default-face
                                     :background ,panic-256-highlight))))

   `(erc-direct-msg-face
     ((,panic-class (:inherit erc-default-face))
      (,panic-256-class  (:inherit erc-default-face))))

   `(erc-error-face
     ((,panic-class (:inherit font-lock-warning-face))
      (,panic-256-class  (:inherit font-lock-warning-face))))

   `(erc-fool-face
     ((,panic-class (:inherit erc-default-face))
      (,panic-256-class  (:inherit erc-default-face))))

   `(erc-input-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(erc-keyword-face
     ((,panic-class (:foreground ,panic-blue
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight bold))))

   `(erc-nick-default-face
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(erc-my-nick-face
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold))))

   `(erc-nick-msg-face
     ((,panic-class (:inherit erc-default-face))
      (,panic-256-class  (:inherit erc-default-face))))

   `(erc-notice-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(erc-pal-face
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold))))

   `(erc-prompt-face
     ((,panic-class (:foreground ,panic-orange
                                   :background ,panic-background
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :background ,panic-256-background
                                        :weight bold))))

   `(erc-timestamp-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(erc-underline-face
     ((t (:underline t))))

   ;; eshell
   `(eshell-prompt
     ((,panic-class (:foreground ,panic-blue
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :inherit bold))))

   `(eshell-ls-archive
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :inherit bold))))

   `(eshell-ls-backup
     ((,panic-class (:inherit font-lock-comment-face))
      (,panic-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,panic-class (:inherit font-lock-comment-face))
      (,panic-256-class  (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,panic-class (:foreground ,panic-blue
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :inherit bold))))

   `(eshell-ls-executable
     ((,panic-class (:foreground ,panic-green
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :inherit bold))))

   `(eshell-ls-unreadable
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(eshell-ls-missing
     ((,panic-class (:inherit font-lock-warning-face))
      (,panic-256-class  (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,panic-class (:inherit font-lock-doc-face))
      (,panic-256-class  (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,panic-class (:foreground ,panic-yellow
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :inherit bold))))

   `(eshell-ls-symlink
     ((,panic-class (:foreground ,panic-cyan
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :inherit bold))))

   ;; evil-ex-substitute
   `(evil-ex-substitute-matches
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-red-l
                                   :inherit italic))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-red-l
                                        :inherit italic))))
   `(evil-ex-substitute-replacement
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-green-l
                                   :inherit italic))
      (,panic-256-class  (:background ,panic-256-highlight-line :foreground ,panic-256-green-l :inherit italic))))

   ;; evil-search-highlight-persist
   `(evil-search-highlight-persist-highlight-face
     ((,panic-class (:inherit region))
      (,panic-256-class  (:inherit region))))

   ;; fic
   `(fic-author-face
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-orange
                                   :underline t
                                   :slant italic))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-orange
                                        :underline t
                                        :slant italic))))

   `(fic-face
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-orange
                                   :weight normal
                                   :slant italic))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-orange
                                        :weight normal
                                        :slant italic))))

   `(font-lock-fic-face
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-orange
                                   :weight normal
                                   :slant italic))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-orange
                                        :weight normal
                                        :slant italic))))

   ;; flx
   `(flx-highlight-face
     ((,panic-class (:foreground ,panic-blue
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; flymake
   `(flymake-errline
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-red)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flymake-infoline
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-class (:foreground ,panic-green-hc
                                   :background ,panic-green-lc))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-green)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-256-class  (:foreground ,panic-256-green-hc
                                        :background ,panic-256-green-lc))))

   `(flymake-warnline
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-class (:foreground ,panic-yellow-hc
                                   :background ,panic-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-yellow)
                   :inherit unspecified
                   :foreground unspecified
                   :background unspecified))
      (,panic-256-class  (:foreground ,panic-256-yellow-hc
                                        :background ,panic-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   ;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-red)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-red)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold
                                        :underline t))))

   `(flycheck-warning
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-yellow)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-yellow-hc
                                   :background ,panic-yellow-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-yellow)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-yellow-hc
                                        :background ,panic-256-yellow-lc
                                        :weight bold
                                        :underline t))))

   `(flycheck-info
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-blue)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-blue-hc
                                   :background ,panic-blue-lc
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-blue)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-blue-hc
                                        :background ,panic-256-blue-lc
                                        :weight bold
                                        :underline t))))

   `(flycheck-fringe-error
     ((,panic-class (:foreground ,panic-red-hc
                                   :background ,panic-red-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red-hc
                                        :background ,panic-256-red-lc
                                        :weight bold))))

   `(flycheck-fringe-warning
     ((,panic-class (:foreground ,panic-yellow-hc
                                   :background ,panic-yellow-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow-hc
                                        :background ,panic-256-yellow-lc
                                        :weight bold))))

   `(flycheck-fringe-info
     ((,panic-class (:foreground ,panic-blue-hc
                                   :background ,panic-blue-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue-hc
                                        :background ,panic-256-blue-lc
                                        :weight bold))))

   ;; flyspell
   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-yellow)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-yellow
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-yellow)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold
                                        :underline t))))

   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) panic-class)
       (:underline (:style wave :color ,panic-red)
                   :inherit unspecified))
      (,panic-class (:foreground ,panic-red
                                   :weight bold
                                   :underline t))
      (,(append '((supports :underline (:style wave))) panic-256-class )
       (:underline (:style wave :color ,panic-256-red)
                   :inherit unspecified))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold
                                        :underline t))))


   ;; git-gutter
   `(git-gutter:added
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter:deleted
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter:modified
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter:unchanged
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   ;; git-gutter-fr
   `(git-gutter-fr:added
     ((,panic-class (:foreground ,panic-green
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :inherit bold))))

   `(git-gutter-fr:deleted
     ((,panic-class (:foreground ,panic-red
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :inherit bold))))

   `(git-gutter-fr:modified
     ((,panic-class (:foreground ,panic-blue
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :inherit bold))))

   ;; git-gutter+ and git-gutter+-fr
   `(git-gutter+-added
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter+-deleted
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter+-modified
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter+-unchanged
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-background
                                   :inherit bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-background
                                        :inherit bold))))

   `(git-gutter-fr+-added
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(git-gutter-fr+-deleted
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold))))

   `(git-gutter-fr+-modified
     ((,panic-class (:foreground ,panic-blue
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight bold))))

   ;; git-timemachine
   `(git-timemachine-minibuffer-detail-face
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-highlight-line
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-blue
                                        :background ,panic-256-highlight-line
                                        :inherit bold))))

   ;; guide-key
   `(guide-key/highlight-command-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(guide-key/key-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(guide-key/prefix-command-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   ;; gnus
   `(gnus-group-mail-1
     ((,panic-class (:weight bold
                               :inherit gnus-group-mail-1-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-mail-1-empty))))

   `(gnus-group-mail-1-empty
     ((,panic-class (:inherit gnus-group-news-1-empty))
      (,panic-256-class  (:inherit gnus-group-news-1-empty))))

   `(gnus-group-mail-2
     ((,panic-class (:weight bold
                               :inherit gnus-group-mail-2-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-mail-2-empty))))

   `(gnus-group-mail-2-empty
     ((,panic-class (:inherit gnus-group-news-2-empty))
      (,panic-256-class  (:inherit gnus-group-news-2-empty))))

   `(gnus-group-mail-3
     ((,panic-class (:weight bold
                               :inherit gnus-group-mail-3-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-mail-3-empty))))

   `(gnus-group-mail-3-empty
     ((,panic-class (:inherit gnus-group-news-3-empty))
      (,panic-256-class  (:inherit gnus-group-news-3-empty))))

   `(gnus-group-mail-low
     ((,panic-class (:weight bold
                               :inherit gnus-group-mail-low-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-mail-low-empty))))

   `(gnus-group-mail-low-empty
     ((,panic-class (:inherit gnus-group-news-low-empty))
      (,panic-256-class  (:inherit gnus-group-news-low-empty))))

   `(gnus-group-news-1
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-1-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-1-empty))))

   `(gnus-group-news-2
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-2-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-2-empty))))

   `(gnus-group-news-3
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-3-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-3-empty))))

   `(gnus-group-news-4
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-4-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-4-empty))))

   `(gnus-group-news-5
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-5-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-5-empty))))

   `(gnus-group-news-6
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-6-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-6-empty))))

   `(gnus-group-news-low
     ((,panic-class (:weight bold
                               :inherit gnus-group-news-low-empty))
      (,panic-256-class  (:weight bold
                                    :inherit gnus-group-news-low-empty))))

   `(gnus-header-content
     ((,panic-class (:inherit message-header-other))
      (,panic-256-class  (:inherit message-header-other))))

   `(gnus-header-from
     ((,panic-class (:inherit message-header-other))
      (,panic-256-class  (:inherit message-header-other))))

   `(gnus-header-name
     ((,panic-class (:inherit message-header-name))
      (,panic-256-class  (:inherit message-header-name))))

   `(gnus-header-newsgroups
     ((,panic-class (:inherit message-header-other))
      (,panic-256-class  (:inherit message-header-other))))

   `(gnus-header-subject
     ((,panic-class (:inherit message-header-subject))
      (,panic-256-class  (:inherit message-header-subject))))

   `(gnus-summary-cancelled
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(gnus-summary-high-ancient
     ((,panic-class (:foreground ,panic-blue
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight bold))))

   `(gnus-summary-high-read
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(gnus-summary-high-ticked
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold))))

   `(gnus-summary-high-unread
     ((,panic-class (:foreground ,panic-foreground
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight bold))))

   `(gnus-summary-low-ancient
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-summary-low-read
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-summary-low-ticked
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(gnus-summary-low-unread
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(gnus-summary-normal-ancient
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-summary-normal-read
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-summary-normal-ticked
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(gnus-summary-normal-unread
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(gnus-summary-selected
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(gnus-cite-1
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-cite-2
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-cite-3
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-cite-4
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-cite-5
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-cite-6
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-cite-7
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(gnus-cite-8
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(gnus-cite-9
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(gnus-cite-10
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(gnus-cite-11
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(gnus-group-news-1-empty
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(gnus-group-news-2-empty
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-group-news-3-empty
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(gnus-group-news-4-empty
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-group-news-5-empty
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(gnus-group-news-6-empty
     ((,panic-class (:foreground ,panic-blue-lc))
      (,panic-256-class  (:foreground ,panic-256-blue-lc))))

   `(gnus-group-news-low-empty
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(gnus-signature
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(gnus-x-face
     ((,panic-class (:background ,panic-foreground
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-foreground
                                        :foreground ,panic-256-background))))


   ;; helm
   `(helm-apt-deinstalled
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(helm-apt-installed
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-bookmark-directory
     ((,panic-class (:inherit helm-ff-directory))
      (,panic-256-class  (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(helm-bookmark-gnus
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(helm-bookmark-info
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-bookmark-man
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(helm-bookmark-w3m
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(helm-bookmarks-su
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(helm-buffer-file
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(helm-buffer-directory
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(helm-buffer-process
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(helm-buffer-saved-out
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background
                                        :inverse-video t))))

   `(helm-buffer-size
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(helm-candidate-number
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :bold t))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :bold t))))

   `(helm-ff-directory
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(helm-ff-executable
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-ff-file
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground))))

   `(helm-ff-invalid-symlink
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-orange
                                   :slant italic))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-orange
                                        :slant italic))))

   `(helm-ff-prefix
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background))))

   `(helm-ff-symlink
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(helm-grep-file
     ((,panic-class (:foreground ,panic-cyan
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :underline t))))

   `(helm-grep-finish
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-grep-lineno
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(helm-grep-match
     ((,panic-class (:inherit helm-match)))
     ((,panic-256-class  (:inherit helm-match))))

   `(helm-grep-running
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(helm-header
     ((,panic-class (:inherit header-line))
      (,panic-256-class  (:inherit terminal-header-line))))

   `(helm-lisp-completion-info
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(helm-lisp-show-completion
     ((,panic-class (:foreground ,panic-yellow
                                   :background ,panic-highlight-line
                                   :bold t))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background ,panic-256-highlight-line
                                        :bold t))))

   `(helm-M-x-key
     ((,panic-class (:foreground ,panic-orange
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :underline t))))

   `(helm-moccur-buffer
     ((,panic-class (:foreground ,panic-cyan
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :underline t))))

   `(helm-match
     ((,panic-class (:foreground ,panic-green :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-green :inherit bold))))

   `(helm-match-item
     ((,panic-class (:inherit helm-match))
      (,panic-256-class  (:inherit helm-match))))

   `(helm-selection
     ((,panic-class (:background ,panic-highlight
                                   :inherit bold
                                   :underline nil))
      (,panic-256-class  (:background ,panic-256-highlight
                                        :inherit bold
                                        :underline nil))))

   `(helm-selection-line
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :underline nil))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :underline nil))))

   `(helm-separator
     ((,panic-class (:foreground ,panic-gray))
      (,panic-256-class  (:foreground ,panic-256-gray))))

   `(helm-source-header
     ((,panic-class (:background ,panic-violet-l
                                   :foreground ,panic-background
                                   :underline nil))
      (,panic-256-class  (:background ,panic-256-violet-l
                                        :foreground ,panic-256-background
                                        :underline nil))))

   `(helm-swoop-target-line-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(helm-swoop-target-line-block-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(helm-swoop-target-word-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-time-zone-current
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(helm-time-zone-home
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(helm-visible-mark
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-magenta :bold t))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-magenta :bold t))))

   ;; helm-ls-git
   `(helm-ls-git-modified-not-staged-face
     ((,panic-class :foreground ,panic-blue)
      (,panic-256-class  :foreground ,panic-256-blue)))

   `(helm-ls-git-modified-and-staged-face
     ((,panic-class :foreground ,panic-blue-l)
      (,panic-256-class  :foreground ,panic-256-blue-l)))

   `(helm-ls-git-renamed-modified-face
     ((,panic-class :foreground ,panic-blue-l)
      (,panic-256-class  :foreground ,panic-256-blue-l)))

   `(helm-ls-git-untracked-face
     ((,panic-class :foreground ,panic-orange)
      (,panic-256-class  :foreground ,panic-256-orange)))

   `(helm-ls-git-added-copied-face
     ((,panic-class :foreground ,panic-green)
      (,panic-256-class  :foreground ,panic-256-green)))

   `(helm-ls-git-added-modified-face
     ((,panic-class :foreground ,panic-green-l)
      (,panic-256-class  :foreground ,panic-256-green-l)))

   `(helm-ls-git-deleted-not-staged-face
     ((,panic-class :foreground ,panic-red)
      (,panic-256-class  :foreground ,panic-256-red)))

   `(helm-ls-git-deleted-and-staged-face
     ((,panic-class :foreground ,panic-red-l)
      (,panic-256-class  :foreground ,panic-256-red-l)))

   `(helm-ls-git-conflict-face
     ((,panic-class :foreground ,panic-yellow)
      (,panic-256-class  :foreground ,panic-256-yellow)))

   ;; hi-lock-mode
   `(hi-yellow
     ((,panic-class (:foreground ,panic-yellow-lc
                                   :background ,panic-yellow-hc))
      (,panic-256-class  (:foreground ,panic-256-yellow-lc
                                        :background ,panic-256-yellow-hc))))

   `(hi-pink
     ((,panic-class (:foreground ,panic-magenta-lc
                                   :background ,panic-magenta-hc))
      (,panic-256-class  (:foreground ,panic-256-magenta-lc
                                        :background ,panic-256-magenta-hc))))

   `(hi-green
     ((,panic-class (:foreground ,panic-green-lc
                                   :background ,panic-green-hc))
      (,panic-256-class  (:foreground ,panic-256-green-lc
                                        :background ,panic-256-green-hc))))

   `(hi-blue
     ((,panic-class (:foreground ,panic-blue-lc
                                   :background ,panic-blue-hc))
      (,panic-256-class  (:foreground ,panic-256-blue-lc
                                        :background ,panic-256-blue-hc))))

   `(hi-black-b
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background
                                        :weight bold))))

   `(hi-blue-b
     ((,panic-class (:foreground ,panic-blue-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue-lc
                                        :weight bold))))

   `(hi-green-b
     ((,panic-class (:foreground ,panic-green-lc
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green-lc
                                        :weight bold))))

   `(hi-red-b
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))))

   `(hi-black-hb
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background
                                        :weight bold))))

   ;; highlight-changes
   `(highlight-changes
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(highlight-changes-delete
     ((,panic-class (:foreground ,panic-red
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :underline t))))

   ;; highlight-indentation
   `(highlight-indentation-face
     ((,panic-class (:background ,panic-gray))
      (,panic-256-class  (:background ,panic-256-gray))))

   `(highlight-indentation-current-column-face
     ((,panic-class (:background ,panic-gray))
      (,panic-256-class  (:background ,panic-256-gray))))

   ;; hl-line-mode
   `(hl-line
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(hl-line-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   ;; ido-mode
   `(ido-first-match
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal))))

   `(ido-only-match
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-yellow
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-yellow
                                        :weight normal))))

   `(ido-subdir
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(ido-incomplete-regexp
     ((,panic-class (:foreground ,panic-red
                                   :weight bold ))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold ))))

   `(ido-indicator
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-background
                                   :width condensed))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-background
                                        :width condensed))))

   `(ido-virtual
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   ;; info
   `(info-header-xref
     ((,panic-class (:foreground ,panic-green
                                   :inherit bold
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :inherit bold
                                        :underline t))))

   `(info-menu
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(info-node
     ((,panic-class (:foreground ,panic-violet
                                   :inherit bold))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :inherit bold))))

   `(info-quoted-name
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(info-reference-item
     ((,panic-class (:background nil
                                   :underline t
                                   :inherit bold))
      (,panic-256-class  (:background nil
                                        :underline t
                                        :inherit bold))))

   `(info-string
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(info-title-1
     ((,panic-class (:height ,panic-height-plus-4))
      (,panic-256-class  (:height ,panic-height-plus-4))))

   `(info-title-2
     ((,panic-class (:height ,panic-height-plus-3))
      (,panic-256-class  (:height ,panic-height-plus-3))))

   `(info-title-3
     ((,panic-class (:height ,panic-height-plus-2))
      (,panic-256-class  (:height ,panic-height-plus-2))))

   `(info-title-4
     ((,panic-class (:height ,panic-height-plus-1))
      (,panic-256-class  (:height ,panic-height-plus-1))))

   ;; ivy
   `(ivy-current-match
     ((,panic-class (:background ,panic-gray :inherit bold))
      (,panic-256-class  (:background ,panic-gray-l :inherit bold))))

   `(ivy-minibuffer-match-face-1
     ((,panic-class (:inherit bold))
      (,panic-256-class  (:inherit bold))))

   `(ivy-minibuffer-match-face-2
     ((,panic-class (:foreground ,panic-violet
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :underline t))))

   `(ivy-minibuffer-match-face-3
     ((,panic-class (:foreground ,panic-green
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :underline t))))

   `(ivy-minibuffer-match-face-4
     ((,panic-class (:foreground ,panic-yellow
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :underline t))))

   `(ivy-remote
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(swiper-line-face
     ((,panic-class (:background ,panic-highlight-line))))

   `(swiper-match-face-1
     ((,panic-class (:background ,panic-gray-d))))

   `(swiper-match-face-2
     ((,panic-class (:background ,panic-green))))

   `(swiper-match-face-3
     ((,panic-class (:background ,panic-orange))))

   `(swiper-match-face-4
     ((,panic-class (:background ,panic-magenta))))

   ;; jabber
   `(jabber-activity-face
     ((,panic-class (:weight bold
                               :foreground ,panic-red))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-red))))

   `(jabber-activity-personal-face
     ((,panic-class (:weight bold
                               :foreground ,panic-blue))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-blue))))

   `(jabber-chat-error
     ((,panic-class (:weight bold
                               :foreground ,panic-red))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-red))))

   `(jabber-chat-prompt-foreign
     ((,panic-class (:weight bold
                               :foreground ,panic-red))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-red))))

   `(jabber-chat-prompt-local
     ((,panic-class (:weight bold
                               :foreground ,panic-blue))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-blue))))

   `(jabber-chat-prompt-system
     ((,panic-class (:weight bold
                               :foreground ,panic-green))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-green))))

   `(jabber-chat-text-foreign
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(jabber-chat-text-local
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(jabber-chat-rare-time-face
     ((,panic-class (:underline t
                                  :foreground ,panic-green))
      (,panic-256-class  (:underline t
                                       :foreground ,panic-256-green))))

   `(jabber-roster-user-away
     ((,panic-class (:slant italic
                              :foreground ,panic-green))
      (,panic-256-class  (:slant italic
                                   :foreground ,panic-256-green))))

   `(jabber-roster-user-chatty
     ((,panic-class (:weight bold
                               :foreground ,panic-orange))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-orange))))

   `(jabber-roster-user-dnd
     ((,panic-class (:slant italic
                              :foreground ,panic-red))
      (,panic-256-class  (:slant italic
                                   :foreground ,panic-256-red))))

   `(jabber-roster-user-error
     ((,panic-class (:weight light
                               :slant italic
                               :foreground ,panic-red))
      (,panic-256-class  (:weight light
                                    :slant italic
                                    :foreground ,panic-256-red))))

   `(jabber-roster-user-offline
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(jabber-roster-user-online
     ((,panic-class (:weight bold
                               :foreground ,panic-blue))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-blue))))

   `(jabber-roster-user-xa
     ((,panic-class (:slant italic
                              :foreground ,panic-magenta))
      (,panic-256-class  (:slant italic
                                   :foreground ,panic-256-magenta))))

   ;; js2-mode colors
   `(js2-error
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(js2-external-variable
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(js2-function-call
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(js2-function-param
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(js2-instance-member
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(js2-jsdoc-html-tag-delimiter
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(js2-jsdoc-html-tag-name
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(js2-jsdoc-tag
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(js2-jsdoc-type
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(js2-jsdoc-value
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(js2-magic-paren
     ((,panic-class (:underline t))
      (,panic-256-class  (:underline t))))

   `(js2-object-property
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(js2-private-function-call
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(js2-private-member
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(js2-warning
     ((,panic-class (:underline ,panic-orange))
      (,panic-256-class  (:underline ,panic-256-orange))))

   ;; jedi
   `(jedi:highlight-function-argument
     ((,panic-class (:inherit bold))
      (,panic-256-class  (:inherit bold))))

   ;; linum-mode
   `(linum
     ((,panic-class (:foreground ,panic-line-number
                                   :background ,panic-fringe-bg))
      (,panic-256-class  (:foreground ,panic-256-line-number
                                        :background ,panic-256-fringe-bg))))

   ;; linum-relative-current-face
   `(linum-relative-current-face
     ((,panic-class (:foreground ,panic-line-number
                                   :background ,panic-highlight-line))
      (,panic-256-class  (:foreground ,panic-256-line-number
                                        :background ,panic-256-highlight-line))))

   ;; lusty-explorer
   `(lusty-directory-face
     ((,panic-class (:inherit dipanic-red-directory))
      (,panic-256-class  (:inherit dipanic-red-directory))))

   `(lusty-file-face
     ((,panic-class nil)
      (,panic-256-class  nil)))

   `(lusty-match-face
     ((,panic-class (:inherit ido-first-match))
      (,panic-256-class  (:inherit ido-first-match))))

   `(lusty-slash-face
     ((,panic-class (:foreground ,panic-cyan
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :weight bold))))

   ;; magit
   ;;
   ;; TODO: Add supports for all magit faces
   ;; https://github.com/magit/magit/search?utf8=%E2%9C%93&q=face
   ;;
   `(magit-diff-added
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background))))

   `(magit-diff-added-highlight
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-highlight-line))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-highlight-line))))

   `(magit-diff-removed
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background))))

   `(magit-diff-removed-highlight
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-highlight-line))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-highlight-line))))

   `(magit-section-title
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(magit-branch
     ((,panic-class (:foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight bold))))

   `(magit-item-highlight
     ((,panic-class (:background ,panic-highlight-line
                                   :weight unspecified))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :weight unspecified))))

   `(magit-log-author
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(magit-log-graph
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(magit-log-head-label-bisect-bad
     ((,panic-class (:background ,panic-red-hc
                                   :foreground ,panic-red-lc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-red-hc
                                        :foreground ,panic-256-red-lc
                                        :box 1))))

   `(magit-log-head-label-bisect-good
     ((,panic-class (:background ,panic-green-hc
                                   :foreground ,panic-green-lc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-green-hc
                                        :foreground ,panic-256-green-lc
                                        :box 1))))

   `(magit-log-head-label-default
     ((,panic-class (:background ,panic-highlight-line
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :box 1))))

   `(magit-log-head-label-local
     ((,panic-class (:background ,panic-blue-lc
                                   :foreground ,panic-blue-hc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-blue-lc
                                        :foreground ,panic-256-blue-hc
                                        :box 1))))

   `(magit-log-head-label-patches
     ((,panic-class (:background ,panic-red-lc
                                   :foreground ,panic-red-hc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-red-lc
                                        :foreground ,panic-256-red-hc
                                        :box 1))))

   `(magit-log-head-label-remote
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-green-hc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-green-hc
                                        :box 1))))

   `(magit-log-head-label-tags
     ((,panic-class (:background ,panic-yellow-lc
                                   :foreground ,panic-yellow-hc
                                   :box 1))
      (,panic-256-class  (:background ,panic-256-yellow-lc
                                        :foreground ,panic-256-yellow-hc
                                        :box 1))))

   `(magit-log-sha1
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   ;; man
   `(Man-overstrike
     ((,panic-class (:foreground ,panic-blue
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight bold))))

   `(Man-reverse
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(Man-underline
     ((,panic-class (:foreground ,panic-green :underline t))
      (,panic-256-class  (:foreground ,panic-256-green :underline t))))

   ;; monky
   `(monky-section-title
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(monky-diff-add
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(monky-diff-del
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; markdown-mode
   `(markdown-header-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(markdown-header-face-1
     ((,panic-class (:inherit markdown-header-face
                                :height ,panic-height-plus-4))
      (,panic-256-class  (:inherit markdown-header-face
                                     :height ,panic-height-plus-4))))

   `(markdown-header-face-2
     ((,panic-class (:inherit markdown-header-face
                                :height ,panic-height-plus-3))
      (,panic-256-class  (:inherit markdown-header-face
                                     :height ,panic-height-plus-3))))

   `(markdown-header-face-3
     ((,panic-class (:inherit markdown-header-face
                                :height ,panic-height-plus-2))
      (,panic-256-class  (:inherit markdown-header-face
                                     :height ,panic-height-plus-2))))

   `(markdown-header-face-4
     ((,panic-class (:inherit markdown-header-face
                                :height ,panic-height-plus-1))
      (,panic-256-class  (:inherit markdown-header-face
                                     :height ,panic-height-plus-1))))

   `(markdown-header-face-5
     ((,panic-class (:inherit markdown-header-face))
      (,panic-256-class  (:inherit markdown-header-face))))

   `(markdown-header-face-6
     ((,panic-class (:inherit markdown-header-face))
      (,panic-256-class  (:inherit markdown-header-face))))

   ;; message-mode
   `(message-cited-text
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(message-header-name
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(message-header-other
     ((,panic-class (:foreground ,panic-foreground
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight normal))))

   `(message-header-to
     ((,panic-class (:foreground ,panic-foreground
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight normal))))

   `(message-header-cc
     ((,panic-class (:foreground ,panic-foreground
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight normal))))

   `(message-header-newsgroups
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(message-header-subject
     ((,panic-class (:foreground ,panic-cyan
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :weight normal))))

   `(message-header-xheader
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(message-mml
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(message-separator
     ((,panic-class (:foreground ,panic-comments
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :slant italic))))

   ;; mew
   `(mew-face-header-subject
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(mew-face-header-from
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(mew-face-header-date
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-header-to
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-header-key
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-header-private
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-header-important
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(mew-face-header-marginal
     ((,panic-class (:foreground ,panic-foreground
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight bold))))

   `(mew-face-header-warning
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-header-xmew
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-header-xmew-bad
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-body-url
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(mew-face-body-comment
     ((,panic-class (:foreground ,panic-foreground
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :slant italic))))

   `(mew-face-body-cite1
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-body-cite2
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(mew-face-body-cite3
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(mew-face-body-cite4
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(mew-face-body-cite5
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-mark-review
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(mew-face-mark-escape
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-mark-delete
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-mark-unlink
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(mew-face-mark-refile
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-mark-unread
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(mew-face-eof-message
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(mew-face-eof-part
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   ;; mingus
   `(mingus-directory-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(mingus-pausing-face
     ((,panic-class (:foreground ,panic-magenta))
      (,panic-256-class  (:foreground ,panic-256-magenta))))

   `(mingus-playing-face
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(mingus-playlist-face
     ((,panic-class (:foreground ,panic-cyan ))
      (,panic-256-class  (:foreground ,panic-256-cyan ))))

   `(mingus-song-file-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(mingus-stopped-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; mmm
   `(mmm-init-submode-face
     ((,panic-class (:background ,panic-violet-d))
      (,panic-256-class  (:background ,panic-256-violet-d))))

   `(mmm-cleanup-submode-face
     ((,panic-class (:background ,panic-orange-d))
      (,panic-256-class  (:background ,panic-256-orange-d))))

   `(mmm-declaration-submode-face
     ((,panic-class (:background ,panic-cyan-d))
      (,panic-256-class  (:background ,panic-256-cyan-d))))

   `(mmm-comment-submode-face
     ((,panic-class (:background ,panic-blue-d))
      (,panic-256-class  (:background ,panic-256-blue-d))))

   `(mmm-output-submode-face
     ((,panic-class (:background ,panic-red-d))
      (,panic-256-class  (:background ,panic-256-red-d))))

   `(mmm-special-submode-face
     ((,panic-class (:background ,panic-green-d))
      (,panic-256-class  (:background ,panic-256-green-d))))

   `(mmm-code-submode-face
     ((,panic-class (:background ,panic-gray))
      (,panic-256-class  (:background ,panic-256-gray))))

   `(mmm-default-submode-face
     ((,panic-class (:background ,panic-gray-d))
      (,panic-256-class  (:background ,panic-256-gray-d))))

   ;; moccur
   `(moccur-current-line-face
     ((,panic-class (:underline t))
      (,panic-256-class  (:underline t))))

   `(moccur-edit-done-face
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-background
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-background
                                        :slant italic))))

   `(moccur-edit-face
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-background))))

   `(moccur-edit-file-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(moccur-edit-reject-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(moccur-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :weight bold))))

   `(search-buffers-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :weight bold))))

   `(search-buffers-header-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-yellow
                                        :weight bold))))

   ;; mu4e
   `(mu4e-cited-1-face
     ((,panic-class (:foreground ,panic-green
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-2-face
     ((,panic-class (:foreground ,panic-blue
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-3-face
     ((,panic-class (:foreground ,panic-orange
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-4-face
     ((,panic-class (:foreground ,panic-yellow
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-5-face
     ((,panic-class (:foreground ,panic-cyan
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-6-face
     ((,panic-class (:foreground ,panic-green
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :slant italic
                                        :weight normal))))

   `(mu4e-cited-7-face
     ((,panic-class (:foreground ,panic-blue
                                   :slant italic
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :slant italic
                                        :weight normal))))

   `(mu4e-flagged-face
     ((,panic-class (:foreground ,panic-magenta
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :weight bold))))

   `(mu4e-view-url-number-face
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal))))

   `(mu4e-warning-face
     ((,panic-class (:foreground ,panic-red
                                   :slant normal
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :slant normal
                                        :weight bold))))

   `(mu4e-header-highlight-face
     ((,panic-class (:inherit unspecified
                                :foreground unspecified
                                :background ,panic-highlight-line
                                :underline ,panic-emphasis
                                :weight normal))
      (,panic-256-class  (:inherit unspecified
                                     :foreground unspecified
                                     :background ,panic-256-highlight-line
                                     :underline ,panic-256-emphasis
                                     :weight normal))))


   `(mu4e-draft-face
     ((,panic-class (:inherit font-lock-string-face))
      (,panic-256-class  (:inherit font-lock-string-face))))

   `(mu4e-footer-face
     ((,panic-class (:inherit font-lock-comment-face))
      (,panic-256-class  (:inherit font-lock-comment-face))))

   `(mu4e-forwarded-face
     ((,panic-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,panic-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-header-face
     ((,panic-class (:inherit default))
      (,panic-256-class  (:inherit default))))

   `(mu4e-header-marks-face
     ((,panic-class (:inherit font-lock-preprocessor-face))
      (,panic-256-class  (:inherit font-lock-preprocessor-face))))

   `(mu4e-header-title-face
     ((,panic-class (:inherit font-lock-type-face))
      (,panic-256-class  (:inherit font-lock-type-face))))

   `(mu4e-highlight-face
     ((,panic-class (:inherit font-lock-pseudo-keyword-face
                                :weight bold))
      (,panic-256-class  (:inherit font-lock-pseudo-keyword-face
                                     :weight bold))))

   `(mu4e-moved-face
     ((,panic-class (:inherit font-lock-comment-face
                                :slant italic))
      (,panic-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-ok-face
     ((,panic-class (:inherit font-lock-comment-face
                                :slant normal
                                :weight bold))
      (,panic-256-class  (:inherit font-lock-comment-face
                                     :slant normal
                                     :weight bold))))

   `(mu4e-replied-face
     ((,panic-class (:inherit font-lock-builtin-face
                                :weight normal))
      (,panic-256-class  (:inherit font-lock-builtin-face
                                     :weight normal))))

   `(mu4e-system-face
     ((,panic-class (:inherit font-lock-comment-face
                                :slant italic))
      (,panic-256-class  (:inherit font-lock-comment-face
                                     :slant italic))))

   `(mu4e-title-face
     ((,panic-class (:inherit font-lock-type-face
                                :weight bold))
      (,panic-256-class  (:inherit font-lock-type-face
                                     :weight bold))))

   `(mu4e-trashed-face
     ((,panic-class (:inherit font-lock-comment-face
                                :strike-through t))
      (,panic-256-class  (:inherit font-lock-comment-face
                                     :strike-through t))))

   `(mu4e-unread-face
     ((,panic-class (:inherit font-lock-keyword-face
                                :weight bold))
      (,panic-256-class  (:inherit font-lock-keyword-face
                                     :weight bold))))

   `(mu4e-view-attach-number-face
     ((,panic-class (:inherit font-lock-variable-name-face
                                :weight bold))
      (,panic-256-class  (:inherit font-lock-variable-name-face
                                     :weight bold))))

   `(mu4e-view-contact-face
     ((,panic-class (:foreground ,panic-foreground
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :weight normal))))

   `(mu4e-view-header-key-face
     ((,panic-class (:inherit message-header-name
                                :weight normal))
      (,panic-256-class  (:inherit message-header-name
                                     :weight normal))))

   `(mu4e-view-header-value-face
     ((,panic-class (:foreground ,panic-cyan
                                   :weight normal
                                   :slant normal))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :weight normal
                                        :slant normal))))

   `(mu4e-view-link-face
     ((,panic-class (:inherit link))
      (,panic-256-class  (:inherit link))))

   `(mu4e-view-special-header-value-face
     ((,panic-class (:foreground ,panic-blue
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight normal
                                        :underline nil))))

   ;; mumamo
   `(mumamo-background-chunk-submode1
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   ;; nav
   `(nav-face-heading
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(nav-face-button-num
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(nav-face-dir
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(nav-face-hdir
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(nav-face-file
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(nav-face-hfile
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; nav-flash
   `(nav-flash-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   ;; neo-tree
   `(neo-banner-face
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-background
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-background
                                        :weight bold))))


   `(neo-header-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background))))

   `(neo-root-dir-face
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background))))

   `(neo-dir-link-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-background))))

   `(neo-file-link-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(neo-button-face
     ((,panic-class (:underline nil))
      (,panic-256-class  (:underline nil))))

   `(neo-expand-btn-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(neo-vc-default-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(neo-vc-user-face
     ((,panic-class (:foreground ,panic-red
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :slant italic))))

   `(neo-vc-up-to-date-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(neo-vc-edited-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(neo-vc-needs-update-face
     ((,panic-class (:underline t))
      (,panic-256-class  (:underline t))))

   `(neo-vc-needs-merge-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(neo-vc-unlocked-changes-face
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-comments))))

   `(neo-vc-added-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(neo-vc-removed-face
     ((,panic-class (:strike-through t))
      (,panic-256-class  (:strike-through t))))

   `(neo-vc-conflict-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(neo-vc-missing-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(neo-vc-ignored-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   ;; adoc-mode / markup
   `(markup-meta-face
     ((,panic-class (:foreground ,panic-gray-l))
      (,panic-256-class  (:foreground ,panic-256-gray-l))))

   `(markup-table-face
     ((,panic-class (:foreground ,panic-blue-hc
                                   :background ,panic-blue-lc))
      (,panic-256-class  (:foreground ,panic-256-blue-hc
                                        :background ,panic-256-blue-lc))))

   `(markup-verbatim-face
     ((,panic-class (:background ,panic-orange-lc))
      (,panic-256-class  (:background ,panic-256-orange-lc))))

   `(markup-list-face
     ((,panic-class (:foreground ,panic-violet-hc
                                   :background ,panic-violet-lc))
      (,panic-256-class  (:foreground ,panic-256-violet-hc
                                        :background ,panic-256-violet-lc))))

   `(markup-replacement-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(markup-complex-replacement-face
     ((,panic-class (:foreground ,panic-violet-hc
                                   :background ,panic-violet-lc))
      (,panic-256-class  (:foreground ,panic-256-violet-hc
                                        :background ,panic-256-violet-lc))))

   `(markup-gen-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(markup-secondary-text-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; org-mode
   `(org-agenda-structure
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-highlight-line
                                   :weight bold
                                   :slant normal
                                   :inverse-video nil
                                   :height ,panic-height-plus-1
                                   :underline nil
                                   :box (:line-width 2 :color ,panic-background)))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-highlight-line
                                        :weight bold
                                        :slant normal
                                        :inverse-video nil
                                        :height ,panic-height-plus-1
                                        :underline nil
                                        :box (:line-width 2 :color ,panic-256-background)))))

   `(org-agenda-calendar-event
     ((,panic-class (:foreground ,panic-emphasis))
      (,panic-256-class  (:foreground ,panic-256-emphasis))))

   `(org-agenda-calendar-sexp
     ((,panic-class (:foreground ,panic-foreground
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :slant italic))))

   `(org-agenda-date
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video nil
                                   :overline nil
                                   :slant normal
                                   :height 1.0
                                   :box (:line-width 2 :color ,panic-background)))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video nil
                                        :overline nil
                                        :slant normal
                                        :height 1.0
                                        :box (:line-width 2 :color ,panic-256-background)))) t)

   `(org-agenda-date-weekend
     ((,panic-class (:inherit org-agenda-date
                                :inverse-video nil
                                :background unspecified
                                :foreground ,panic-comments
                                :weight unspecified
                                :underline t
                                :overline nil
                                :box unspecified))
      (,panic-256-class  (:inherit org-agenda-date
                                     :inverse-video nil
                                     :background unspecified
                                     :foreground ,panic-256-comments
                                     :weight unspecified
                                     :underline t
                                     :overline nil
                                     :box unspecified))) t)

   `(org-agenda-date-today
     ((,panic-class (:inherit org-agenda-date
                                :inverse-video t
                                :weight bold
                                :underline unspecified
                                :overline nil
                                :box unspecified
                                :foreground ,panic-blue
                                :background ,panic-background))
      (,panic-256-class  (:inherit org-agenda-date
                                     :inverse-video t
                                     :weight bold
                                     :underline unspecified
                                     :overline nil
                                     :box unspecified
                                     :foreground ,panic-256-blue
                                     :background ,panic-256-background))) t)

   `(org-agenda-done
     ((,panic-class (:foreground ,panic-comments
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :slant italic))) t)

   `(org-archived
     ((,panic-class (:foreground ,panic-comments
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :weight normal))))

   `(org-block
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-highlight-alt))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-highlight-alt))))

   `(org-block-background
     ((,panic-class (:background ,panic-highlight-alt))
      (,panic-256-class  (:background ,panic-256-highlight-alt))))

   `(org-block-begin-line
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-gray-d
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-gray-d
                                        :slant italic))))

   `(org-block-end-line
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-gray-d
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-gray-d
                                        :slant italic))))

   `(org-checkbox
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground
                                   :box (:line-width 1 :style released-button)))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground
                                        :box (:line-width 1 :style released-button)))))

   `(org-code
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(org-date
     ((,panic-class (:foreground ,panic-blue
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline t))))

   `(org-done
     ((,panic-class (:weight bold
                               :foreground ,panic-green))
      (,panic-256-class  (:weight bold
                                    :foreground ,panic-256-green))))

   `(org-ellipsis
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(org-formula
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(org-headline-done
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(org-hide
     ((,panic-class (:foreground ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-background))))

   `(org-level-1
     ((,panic-class (:inherit ,panic-pitch
                                :height ,panic-height-plus-4
                                :foreground ,panic-orange))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :height ,panic-height-plus-4
                                     :foreground ,panic-256-orange))))

   `(org-level-2
     ((,panic-class (:inherit ,panic-pitch
                                :height ,panic-height-plus-3
                                :foreground ,panic-green))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :height ,panic-height-plus-3
                                     :foreground ,panic-256-green))))

   `(org-level-3
     ((,panic-class (:inherit ,panic-pitch
                                :height ,panic-height-plus-2
                                :foreground ,panic-blue))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :height ,panic-height-plus-2
                                     :foreground ,panic-256-blue))))

   `(org-level-4
     ((,panic-class (:inherit ,panic-pitch
                                :height ,panic-height-plus-1
                                :foreground ,panic-yellow))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :height ,panic-height-plus-1
                                     :foreground ,panic-256-yellow))))

   `(org-level-5
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-cyan))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-cyan))))

   `(org-level-6
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-green))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-green))))

   `(org-level-7
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-red))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-red))))

   `(org-level-8
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-blue))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-blue))))

   `(org-link
     ((,panic-class (:foreground ,panic-blue
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :underline t))))

   `(org-sexp-date
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(org-scheduled
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(org-scheduled-previously
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(org-scheduled-today
     ((,panic-class (:foreground ,panic-blue
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :weight normal))))

   `(org-special-keyword
     ((,panic-class (:foreground ,panic-comments
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :weight bold))))

   `(org-table
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(org-tag
     ((,panic-class (:weight bold))
      (,panic-256-class  (:weight bold))))

   `(org-time-grid
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(org-todo
     ((,panic-class (:foreground ,panic-red
                                   :weight bold)))
     ((,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold))))

   `(org-upcoming-deadline
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal
                                        :underline nil))))

   `(org-warning
     ((,panic-class (:foreground ,panic-orange
                                   :weight normal
                                   :underline nil))
      (,panic-256-class  (:foreground ,panic-256-orange
                                        :weight normal
                                        :underline nil))))

   ;; org-habit (clear=blue, ready=green, alert=yellow, overdue=red. future=lower contrast)
   `(org-habit-clear-face
     ((,panic-class (:background ,panic-blue-lc
                                   :foreground ,panic-blue-hc))
      (,panic-256-class  (:background ,panic-256-blue-lc
                                        :foreground ,panic-256-blue-hc))))

   `(org-habit-clear-future-face
     ((,panic-class (:background ,panic-blue-lc))
      (,panic-256-class  (:background ,panic-256-blue-lc))))

   `(org-habit-ready-face
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-green))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-green))))

   `(org-habit-ready-future-face
     ((,panic-class (:background ,panic-green-lc))
      (,panic-256-class  (:background ,panic-256-green-lc))))

   `(org-habit-alert-face
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-yellow-lc))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-yellow-lc))))

   `(org-habit-alert-future-face
     ((,panic-class (:background ,panic-yellow-lc))
      (,panic-256-class  (:background ,panic-256-yellow-lc))))

   `(org-habit-overdue-face
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-red-lc))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-red-lc))))

   `(org-habit-overdue-future-face
     ((,panic-class (:background ,panic-red-lc))
      (,panic-256-class  (:background ,panic-256-red-lc))))

   ;; latest additions
   `(org-agenda-dimmed-todo-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(org-agenda-restriction-lock
     ((,panic-class (:background ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-yellow))))

   `(org-clock-overlay
     ((,panic-class (:background ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-yellow))))

   `(org-column
     ((,panic-class (:background ,panic-highlight-line
                                   :strike-through nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :inherit default))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :strike-through nil
                                        :underline nil
                                        :slant normal
                                        :weight normal
                                        :inherit default))))

   `(org-column-title
     ((,panic-class (:background ,panic-highlight-line
                                   :underline t
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :underline t
                                        :weight bold))))

   `(org-date-selected
     ((,panic-class (:foreground ,panic-red
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :inverse-video t))))

   `(org-document-info
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(org-document-title
     ((,panic-class (:foreground ,panic-emphasis
                                   :weight bold
                                   :height ,panic-height-plus-4))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :weight bold
                                        :height ,panic-height-plus-4))))

   `(org-drawer
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(org-footnote
     ((,panic-class (:foreground ,panic-magenta
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :underline t))))

   `(org-latex-and-export-specials
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(org-mode-line-clock-overrun
     ((,panic-class (:inherit mode-line))
      (,panic-256-class  (:inherit mode-line))))

   ;; outline
   `(outline-1
     ((,panic-class (:inherit org-level-1))
      (,panic-256-class  (:inherit org-level-1))))

   `(outline-2
     ((,panic-class (:inherit org-level-2))
      (,panic-256-class  (:inherit org-level-2))))

   `(outline-3
     ((,panic-class (:inherit org-level-3))
      (,panic-256-class  (:inherit org-level-3))))

   `(outline-4
     ((,panic-class (:inherit org-level-4))
      (,panic-256-class  (:inherit org-level-4))))

   `(outline-5
     ((,panic-class (:inherit org-level-5))
      (,panic-256-class  (:inherit org-level-5))))

   `(outline-6
     ((,panic-class (:inherit org-level-6))
      (,panic-256-class  (:inherit org-level-6))))

   `(outline-7
     ((,panic-class (:inherit org-level-7))
      (,panic-256-class  (:inherit org-level-7))))

   `(outline-8
     ((,panic-class (:inherit org-level-8))
      (,panic-256-class  (:inherit org-level-8))))

   ;; parenface
   `(paren-face
     ((,panic-256-class  (:foreground ,panic-comments))))

   ;; perspective
   `(persp-selected-face
     ((,panic-class (:foreground ,panic-blue
                                   :weight bold))))

   ;; pretty-mode
   `(pretty-mode-symbol-face
     ((,panic-class (:foreground ,panic-yellow
                                   :weight normal))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight normal))))

   ;; popup
   `(popup-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   `(popup-isearch-match
     ((,panic-class (:background ,panic-green))
      (,panic-256-class  (:background ,panic-256-green))))

   `(popup-menu-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   `(popup-menu-mouse-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-foreground))))

   `(popup-menu-selection-face
     ((,panic-class (:background ,panic-magenta
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-magenta
                                        :foreground ,panic-256-background))))

   `(popup-scroll-bar-background-face
     ((,panic-class (:background ,panic-comments))
      (,panic-256-class  (:background ,panic-256-comments))))

   `(popup-scroll-bar-foreground-face
     ((,panic-class (:background ,panic-emphasis))
      (,panic-256-class  (:background ,panic-256-emphasis))))

   `(popup-tip-face
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(rainbow-delimiters-depth-2-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(rainbow-delimiters-depth-3-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(rainbow-delimiters-depth-4-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(rainbow-delimiters-depth-5-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(rainbow-delimiters-depth-6-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(rainbow-delimiters-depth-7-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(rainbow-delimiters-depth-8-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(rainbow-delimiters-depth-9-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(rainbow-delimiters-depth-10-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(rainbow-delimiters-depth-11-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(rainbow-delimiters-depth-12-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(rainbow-delimiters-unmatched-face
     ((,panic-class (:foreground ,panic-foreground
                                   :background ,panic-background
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :background ,panic-256-background
                                        :inverse-video t))))

   ;; rhtm-mode
   `(erb-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background))))

   `(erb-delim-face
     ((,panic-class (:foreground ,panic-cyan
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :background ,panic-256-background))))

   `(erb-exec-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background))))

   `(erb-exec-delim-face
     ((,panic-class (:foreground ,panic-cyan
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :background ,panic-256-background))))

   `(erb-out-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background))))

   `(erb-out-delim-face
     ((,panic-class (:foreground ,panic-cyan
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :background ,panic-256-background))))

   `(erb-comment-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background))))

   `(erb-comment-delim-face
     ((,panic-class (:foreground ,panic-cyan
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :background ,panic-256-background))))

   ;; rst-mode
   `(rst-level-1-face
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-background))))

   `(rst-level-2-face
     ((,panic-class (:background ,panic-cyan
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-cyan
                                        :foreground ,panic-256-background))))

   `(rst-level-3-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background))))

   `(rst-level-4-face
     ((,panic-class (:background ,panic-violet
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-violet
                                        :foreground ,panic-256-background))))

   `(rst-level-5-face
     ((,panic-class (:background ,panic-magenta
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-magenta
                                        :foreground ,panic-256-background))))

   `(rst-level-6-face
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-background))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-background))))

   ;; rpm-mode
   `(rpm-spec-dir-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(rpm-spec-doc-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(rpm-spec-ghost-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(rpm-spec-macro-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(rpm-spec-obsolete-tag-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(rpm-spec-package-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(rpm-spec-section-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(rpm-spec-tag-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(rpm-spec-var-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; sh-mode
   `(sh-quoted-exec
     ((,panic-class (:foreground ,panic-violet
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-violet
                                        :weight bold))))

   `(sh-escaped-newline
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   `(sh-heredoc
     ((,panic-class (:foreground ,panic-yellow
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :weight bold))))

   ;; smartparens
   `(sp-pair-overlay-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(sp-wrap-overlay-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(sp-wrap-tag-overlay-face
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(sp-show-pair-enclosing
     ((,panic-class (:inherit highlight))
      (,panic-256-class  (:inherit highlight))))

   `(sp-show-pair-match-face
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(sp-show-pair-mismatch-face
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; show-paren
   `(show-paren-match
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(show-paren-mismatch
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; mic-paren
   `(paren-face-match
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-mismatch
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   `(paren-face-no-match
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-background
                                   :weight normal
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-background
                                        :weight normal
                                        :inverse-video t))))

   ;; SLIME
   `(slime-repl-inputed-output-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; speedbar
   `(speedbar-button-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-comments))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-comments))))

   `(speedbar-directory-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-blue))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-blue))))

   `(speedbar-file-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-foreground))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-foreground))))

   `(speedbar-highlight-face
     ((,panic-class (:inherit ,panic-pitch
                                :background ,panic-highlight-line))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :background ,panic-256-highlight-line))))

   `(speedbar-selected-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-yellow
                                :underline t))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-yellow
                                     :underline t))))

   `(speedbar-separator-face
     ((,panic-class (:inherit ,panic-pitch
                                :background ,panic-blue
                                :foreground ,panic-background
                                :overline ,panic-cyan-lc))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :background ,panic-256-blue
                                     :foreground ,panic-256-background
                                     :overline ,panic-256-cyan-lc))))

   `(speedbar-tag-face
     ((,panic-class (:inherit ,panic-pitch
                                :foreground ,panic-green))
      (,panic-256-class  (:inherit ,panic-pitch
                                     :foreground ,panic-256-green))))

   ;; sunrise commander headings
   `(sr-active-path-face
     ((,panic-class (:background ,panic-blue
                                   :foreground ,panic-background
                                   :height ,panic-height-plus-1
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-blue
                                        :foreground ,panic-256-background
                                        :height ,panic-height-plus-1
                                        :weight bold))))

   `(sr-editing-path-face
     ((,panic-class (:background ,panic-yellow
                                   :foreground ,panic-background
                                   :weight bold
                                   :height ,panic-height-plus-1))
      (,panic-256-class  (:background ,panic-256-yellow
                                        :foreground ,panic-256-background
                                        :weight bold
                                        :height ,panic-height-plus-1))))

   `(sr-highlight-path-face
     ((,panic-class (:background ,panic-green
                                   :foreground ,panic-background
                                   :weight bold
                                   :height ,panic-height-plus-1))
      (,panic-256-class  (:background ,panic-256-green
                                        :foreground ,panic-256-background
                                        :weight bold
                                        :height ,panic-height-plus-1))))

   `(sr-passive-path-face
     ((,panic-class (:background ,panic-comments
                                   :foreground ,panic-background
                                   :weight bold
                                   :height ,panic-height-plus-1))
      (,panic-256-class  (:background ,panic-256-comments
                                        :foreground ,panic-256-background
                                        :weight bold
                                        :height ,panic-height-plus-1))))

   ;; sunrise commander marked
   `(sr-marked-dir-face
     ((,panic-class (:inherit dipanic-red-marked))
      (,panic-256-class  (:inherit dipanic-red-marked))))

   `(sr-marked-file-face
     ((,panic-class (:inherit dipanic-red-marked))
      (,panic-256-class  (:inherit dipanic-red-marked))))

   `(sr-alt-marked-dir-face
     ((,panic-class (:background ,panic-magenta
                                   :foreground ,panic-background
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-magenta
                                        :foreground ,panic-256-background
                                        :weight bold))))

   `(sr-alt-marked-file-face
     ((,panic-class (:background ,panic-magenta
                                   :foreground ,panic-background
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-magenta
                                        :foreground ,panic-256-background
                                        :weight bold))))

   ;; sunrise commander fstat
   `(sr-directory-face
     ((,panic-class (:inherit dipanic-red-directory
                                :weight normal))
      (,panic-256-class  (:inherit dipanic-red-directory
                                     :weight normal))))

   `(sr-symlink-directory-face
     ((,panic-class (:inherit dipanic-red-directory
                                :slant italic
                                :weight normal))
      (,panic-256-class  (:inherit dipanic-red-directory
                                     :slant italic
                                     :weight normal))))

   `(sr-symlink-face
     ((,panic-class (:inherit dipanic-red-symlink
                                :slant italic
                                :weight normal))
      (,panic-256-class  (:inherit dipanic-red-symlink
                                     :slant italic
                                     :weight normal))))

   `(sr-broken-link-face
     ((,panic-class (:inherit dipanic-red-warning
                                :slant italic
                                :weight normal))
      (,panic-256-class  (:inherit dipanic-red-warning
                                     :slant italic
                                     :weight normal))))

   ;; sunrise commander file types
   `(sr-compressed-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(sr-encrypted-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(sr-log-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(sr-packaged-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(sr-html-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(sr-xml-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   ;; sunrise commander misc
   `(sr-clex-hotchar-face
     ((,panic-class (:background ,panic-red
                                   :foreground ,panic-background
                                   :weight bold))
      (,panic-256-class  (:background ,panic-256-red
                                        :foreground ,panic-256-background
                                        :weight bold))))

   ;; syslog-mode
   `(syslog-ip-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-yellow))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-yellow))))

   `(syslog-hour-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-green))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-green))))

   `(syslog-error-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-red
                                        :weight bold))))

   `(syslog-warn-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-orange
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-orange
                                        :weight bold))))

   `(syslog-info-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-blue
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-blue
                                        :weight bold))))

   `(syslog-debug-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-cyan
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-cyan
                                        :weight bold))))

   `(syslog-su-face
     ((,panic-class (:background unspecified
                                   :foreground ,panic-magenta))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-magenta))))

   ;; table
   `(table-cell
     ((,panic-class (:foreground ,panic-foreground
                                   :background ,panic-highlight-line))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :background ,panic-256-highlight-line))))

   ;; term
   `(term-color-black
     ((,panic-class (:foreground ,panic-background
                                   :background ,panic-highlight-line))
      (,panic-256-class  (:foreground ,panic-256-background
                                        :background ,panic-256-highlight-line))))

   `(term-color-red
     ((,panic-class (:foreground ,panic-red
                                   :background ,panic-red-d))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :background ,panic-256-red-d))))

   `(term-color-green
     ((,panic-class (:foreground ,panic-green
                                   :background ,panic-green-d))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :background ,panic-256-green-d))))

   `(term-color-yellow
     ((,panic-class (:foreground ,panic-yellow
                                   :background ,panic-yellow-d))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background ,panic-256-yellow-d))))

   `(term-color-blue
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-blue-d))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-blue-d))))

   `(term-color-magenta
     ((,panic-class (:foreground ,panic-magenta
                                   :background ,panic-magenta-d))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :background ,panic-256-magenta-d))))

   `(term-color-cyan
     ((,panic-class (:foreground ,panic-cyan
                                   :background ,panic-cyan-d))
      (,panic-256-class  (:foreground ,panic-256-cyan
                                        :background ,panic-256-cyan-d))))

   `(term-color-white
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-foreground))))

   `(term-default-fg-color
     ((,panic-class (:inherit term-color-white))
      (,panic-256-class  (:inherit term-color-white))))

   `(term-default-bg-color
     ((,panic-class (:inherit term-color-black))
      (,panic-256-class  (:inherit term-color-black))))

   ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
   ;; zencoding uses this)
   `(tooltip
     ((,panic-class (:background ,panic-green-l
                                   :foreground ,panic-background
                                   :inherit ,panic-pitch))))

   ;; tuareg
   `(tuareg-font-lock-governing-face
     ((,panic-class (:foreground ,panic-magenta
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-magenta
                                        :weight bold))))

   `(tuareg-font-lock-multistage-face
     ((,panic-class (:foreground ,panic-blue
                                   :background ,panic-highlight-line
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :background ,panic-256-highlight-line
                                        :weight bold))))

   `(tuareg-font-lock-operator-face
     ((,panic-class (:foreground ,panic-emphasis))
      (,panic-256-class  (:foreground ,panic-256-emphasis))))

   `(tuareg-font-lock-error-face
     ((,panic-class (:foreground ,panic-yellow
                                   :background ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :background ,panic-256-red
                                        :weight bold))))

   `(tuareg-font-lock-interactive-output-face
     ((,panic-class (:foreground ,panic-cyan))
      (,panic-256-class  (:foreground ,panic-256-cyan))))

   `(tuareg-font-lock-interactive-error-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-background))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-background))))

   `(undo-tree-visualizer-unmodified-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(undo-tree-visualizer-current-face
     ((,panic-class (:foreground ,panic-blue
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-blue
                                        :inverse-video t))))

   `(undo-tree-visualizer-active-branch-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :background ,panic-background
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :background ,panic-256-background
                                        :weight bold))))

   `(undo-tree-visualizer-register-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   ;; volatile highlights
   `(vhl/default-face
     ((,panic-class (:background ,panic-green-lc
                                   :foreground ,panic-green-hc))
      (,panic-256-class  (:background ,panic-256-green-lc
                                        :foreground ,panic-256-green-hc))))

   ;; w3m
   `(w3m-anchor
     ((,panic-class (:inherit link))
      (,panic-256-class  (:inherit link))))

   `(w3m-arrived-anchor
     ((,panic-class (:inherit link-visited))
      (,panic-256-class  (:inherit link-visited))))

   `(w3m-form
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground))))

   `(w3m-header-line-location-title
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-yellow))))

   `(w3m-header-line-location-content

     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   `(w3m-bold
     ((,panic-class (:foreground ,panic-emphasis
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :weight bold))))

   `(w3m-image-anchor
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-cyan
                                   :inherit link))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-cyan
                                        :inherit link))))

   `(w3m-image
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-cyan))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-cyan))))

   `(w3m-lnum-minibuffer-prompt
     ((,panic-class (:foreground ,panic-emphasis))
      (,panic-256-class  (:foreground ,panic-256-emphasis))))

   `(w3m-lnum-match
     ((,panic-class (:background ,panic-highlight-line))
      (,panic-256-class  (:background ,panic-256-highlight-line))))

   `(w3m-lnum
     ((,panic-class (:underline nil
                                  :bold nil
                                  :foreground ,panic-red))
      (,panic-256-class  (:underline nil
                                       :bold nil
                                       :foreground ,panic-256-red))))

   `(w3m-session-select
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(w3m-session-selected
     ((,panic-class (:foreground ,panic-emphasis
                                   :bold t
                                   :underline t))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :bold t
                                        :underline t))))

   `(w3m-tab-background
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground))))

   `(w3m-tab-selected-background
     ((,panic-class (:background ,panic-background
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-background
                                        :foreground ,panic-256-foreground))))

   `(w3m-tab-mouse
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-yellow))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-yellow))))

   `(w3m-tab-selected
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-emphasis
                                   :bold t))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-emphasis
                                        :bold t))))

   `(w3m-tab-unselected
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-foreground))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-foreground))))

   `(w3m-tab-selected-retrieving
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-red))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-red))))

   `(w3m-tab-unselected-retrieving
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-orange))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-orange))))

   `(w3m-tab-unselected-unseen
     ((,panic-class (:background ,panic-highlight-line
                                   :foreground ,panic-violet))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :foreground ,panic-256-violet))))

   ;; web-mode
   `(web-mode-builtin-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(web-mode-comment-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(web-mode-constant-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(web-mode-current-element-highlight-face
     ((,panic-class (:underline unspecified
                                  :weight unspecified
                                  :background ,panic-highlight-line))
      (,panic-256-class  (:underline unspecified
                                       :weight unspecified
                                       :background ,panic-256-highlight-line))))

   `(web-mode-doctype-face
     ((,panic-class (:foreground ,panic-comments
                                   :slant italic
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :slant italic
                                        :weight bold))))

   `(web-mode-folded-face
     ((,panic-class (:underline t))
      (,panic-256-class  (:underline t))))

   `(web-mode-function-name-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(web-mode-html-attr-name-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(web-mode-html-attr-custom-face
     ((,panic-class (:inherit web-mode-html-attr-name-face))
      (,panic-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-engine-face
     ((,panic-class (:inherit web-mode-block-delimiter-face))
      (,panic-256-class  (:inherit web-mode-block-delimiter-face))))

   `(web-mode-html-attr-equal-face
     ((,panic-class (:inherit web-mode-html-attr-name-face))
      (,panic-256-class  (:inherit web-mode-html-attr-name-face))))

   `(web-mode-html-attr-value-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(web-mode-html-tag-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(web-mode-keyword-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(web-mode-preprocessor-face
     ((,panic-class (:foreground ,panic-yellow
                                   :slant normal
                                   :weight unspecified))
      (,panic-256-class  (:foreground ,panic-256-yellow
                                        :slant normal
                                        :weight unspecified))))

   `(web-mode-string-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(web-mode-type-face
     ((,panic-class (:inherit font-lock-type-face))
      (,panic-256-class  (:inherit font-lock-type-face))))

   `(web-mode-variable-name-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(web-mode-warning-face
     ((,panic-class (:inherit font-lock-warning-face))
      (,panic-256-class  (:inherit font-lock-warning-face))))

   `(web-mode-block-face
     ((,panic-class (:background unspecified))
      (,panic-256-class  (:background unspecified))))

   `(web-mode-block-delimiter-face
     ((,panic-class (:inherit font-lock-preprocessor-face))
      (,panic-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-comment-face
     ((,panic-class (:inherit web-mode-comment-face))
      (,panic-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-block-control-face
     ((,panic-class (:inherit font-lock-preprocessor-face))
      (,panic-256-class  (:inherit font-lock-preprocessor-face))))

   `(web-mode-block-string-face
     ((,panic-class (:inherit web-mode-string-face))
      (,panic-256-class  (:inherit web-mode-string-face))))

   `(web-mode-comment-keyword-face
     ((,panic-class (:box 1 :weight bold))
      (,panic-256-class  (:box 1 :weight bold))))

   `(web-mode-css-at-rule-face
     ((,panic-class (:inherit font-lock-constant-face))
      (,panic-256-class  (:inherit font-lock-constant-face))))

   `(web-mode-css-pseudo-class-face
     ((,panic-class (:inherit font-lock-builtin-face))
      (,panic-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-color-face
     ((,panic-class (:inherit font-lock-builtin-face))
      (,panic-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-filter-face
     ((,panic-class (:inherit font-lock-function-name-face))
      (,panic-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-function-face
     ((,panic-class (:inherit font-lock-builtin-face))
      (,panic-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-function-call-face
     ((,panic-class (:inherit font-lock-function-name-face))
      (,panic-256-class  (:inherit font-lock-function-name-face))))

   `(web-mode-css-priority-face
     ((,panic-class (:inherit font-lock-builtin-face))
      (,panic-256-class  (:inherit font-lock-builtin-face))))

   `(web-mode-css-property-name-face
     ((,panic-class (:inherit font-lock-variable-name-face))
      (,panic-256-class  (:inherit font-lock-variable-name-face))))

   `(web-mode-css-selector-face
     ((,panic-class (:inherit font-lock-keyword-face))
      (,panic-256-class  (:inherit font-lock-keyword-face))))

   `(web-mode-css-string-face
     ((,panic-class (:inherit web-mode-string-face))
      (,panic-256-class  (:inherit web-mode-string-face))))

   `(web-mode-javascript-string-face
     ((,panic-class (:inherit web-mode-string-face))
      (,panic-256-class  (:inherit web-mode-string-face))))

   `(web-mode-json-comment-face
     ((,panic-class (:inherit web-mode-comment-face))
      (,panic-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-json-context-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(web-mode-json-key-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(web-mode-json-string-face
     ((,panic-class (:inherit web-mode-string-face))
      (,panic-256-class  (:inherit web-mode-string-face))))

   `(web-mode-param-name-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(web-mode-part-comment-face
     ((,panic-class (:inherit web-mode-comment-face))
      (,panic-256-class  (:inherit web-mode-comment-face))))

   `(web-mode-part-face
     ((,panic-class (:inherit web-mode-block-face))
      (,panic-256-class  (:inherit web-mode-block-face))))

   `(web-mode-part-string-face
     ((,panic-class (:inherit web-mode-string-face))
      (,panic-256-class  (:inherit web-mode-string-face))))

   `(web-mode-symbol-face
     ((,panic-class (:foreground ,panic-violet))
      (,panic-256-class  (:foreground ,panic-256-violet))))

   `(web-mode-whitespace-face
     ((,panic-class (:background ,panic-red))
      (,panic-256-class  (:background ,panic-256-red))))

   ;; whitespace-mode
   `(whitespace-space
     ((,panic-class (:background unspecified
                                   :foreground ,panic-comments
                                   :inverse-video unspecified
                                   :slant italic))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-comments
                                        :inverse-video unspecified
                                        :slant italic))))

   `(whitespace-hspace
     ((,panic-class (:background unspecified
                                   :foreground ,panic-emphasis
                                   :inverse-video unspecified))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-emphasis
                                        :inverse-video unspecified))))

   `(whitespace-tab
     ((,panic-class (:background unspecified
                                   :foreground ,panic-red
                                   :inverse-video unspecified
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-red
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-newline
     ((,panic-class(:background unspecified
                                  :foreground ,panic-comments
                                  :inverse-video unspecified))
      (,panic-256-class (:background unspecified
                                       :foreground ,panic-256-comments
                                       :inverse-video unspecified))))

   `(whitespace-trailing
     ((,panic-class (:background unspecified
                                   :foreground ,panic-orange-lc
                                   :inverse-video t))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-orange-lc
                                        :inverse-video t))))

   `(whitespace-line
     ((,panic-class (:background unspecified
                                   :foreground ,panic-magenta
                                   :inverse-video unspecified))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-magenta
                                        :inverse-video unspecified))))

   `(whitespace-space-before-tab
     ((,panic-class (:background ,panic-red-lc
                                   :foreground unspecified
                                   :inverse-video unspecified))
      (,panic-256-class  (:background ,panic-256-red-lc
                                        :foreground unspecified
                                        :inverse-video unspecified))))

   `(whitespace-indentation
     ((,panic-class (:background unspecified
                                   :foreground ,panic-yellow
                                   :inverse-video unspecified
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-yellow
                                        :inverse-video unspecified
                                        :weight bold))))

   `(whitespace-empty
     ((,panic-class (:background unspecified
                                   :foreground ,panic-red-lc
                                   :inverse-video t))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-red-lc
                                        :inverse-video t))))

   `(whitespace-space-after-tab
     ((,panic-class (:background unspecified
                                   :foreground ,panic-orange
                                   :inverse-video t
                                   :weight bold))
      (,panic-256-class  (:background unspecified
                                        :foreground ,panic-256-orange
                                        :inverse-video t
                                        :weight bold))))

   ;; wanderlust
   `(wl-highlight-folder-few-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(wl-highlight-folder-many-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(wl-highlight-folder-path-face
     ((,panic-class (:foreground ,panic-orange))
      (,panic-256-class  (:foreground ,panic-256-orange))))

   `(wl-highlight-folder-unread-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-folder-zero-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(wl-highlight-folder-unknown-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-message-citation-header
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(wl-highlight-message-cited-text-1
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(wl-highlight-message-cited-text-2
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-cited-text-3
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-message-cited-text-4
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-message-header-contents-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-headers-face
     ((,panic-class (:foreground ,panic-red))
      (,panic-256-class  (:foreground ,panic-256-red))))

   `(wl-highlight-message-important-header-contents
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-header-contents
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-important-header-contents2
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-signature
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   `(wl-highlight-message-unimportant-header-contents
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(wl-highlight-summary-answepanic-red-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-summary-disposed-face
     ((,panic-class (:foreground ,panic-foreground
                                   :slant italic))
      (,panic-256-class  (:foreground ,panic-256-foreground
                                        :slant italic))))

   `(wl-highlight-summary-new-face
     ((,panic-class (:foreground ,panic-blue))
      (,panic-256-class  (:foreground ,panic-256-blue))))

   `(wl-highlight-summary-normal-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(wl-highlight-summary-thread-top-face
     ((,panic-class (:foreground ,panic-yellow))
      (,panic-256-class  (:foreground ,panic-256-yellow))))

   `(wl-highlight-thread-indent-face
     ((,panic-class (:foreground ,panic-magenta))
      (,panic-256-class  (:foreground ,panic-256-magenta))))

   `(wl-highlight-summary-refiled-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(wl-highlight-summary-displaying-face
     ((,panic-class (:underline t
                                  :weight bold))
      (,panic-256-class  (:underline t
                                       :weight bold))))

   ;; weechat
   `(weechat-error-face
     ((,panic-class (:inherit error))
      (,panic-256-class  (:inherit error))))

   `(weechat-highlight-face
     ((,panic-class (:foreground ,panic-emphasis
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-emphasis
                                        :weight bold))))

   `(weechat-nick-self-face
     ((,panic-class (:foreground ,panic-green
                                   :weight unspecified
                                   :inverse-video t))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight unspecified
                                        :inverse-video t))))

   `(weechat-prompt-face
     ((,panic-class (:inherit minibuffer-prompt))
      (,panic-256-class  (:inherit minibuffer-prompt))))

   `(weechat-time-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   ;; which-func-mode
   `(which-func
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   ;; which-key
   `(which-key-key-face
     ((,panic-class (:foreground ,panic-green
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-green
                                        :weight bold))))

   `(which-key-separator-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(which-key-note-face
     ((,panic-class (:foreground ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments))))

   `(which-key-command-description-face
     ((,panic-class (:foreground ,panic-foreground))
      (,panic-256-class  (:foreground ,panic-256-foreground))))

   `(which-key-local-map-description-face
     ((,panic-class (:foreground ,panic-yellow-hc))
      (,panic-256-class  (:foreground ,panic-256-yellow-hc))))

   `(which-key-group-description-face
     ((,panic-class (:foreground ,panic-red
                                   :weight bold))
      (,panic-256-class  (:foreground ,panic-256-red
                                        :weight bold))))
   ;; window-number-mode
   `(window-number-face
     ((,panic-class (:foreground ,panic-green))
      (,panic-256-class  (:foreground ,panic-256-green))))

   ;; yascroll
   `(yascroll:thumb-text-area
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-comments))))

   `(yascroll:thumb-fringe
     ((,panic-class (:foreground ,panic-comments
                                   :background ,panic-comments))
      (,panic-256-class  (:foreground ,panic-256-comments
                                        :background ,panic-256-comments))))

   ;; zencoding
   `(zencoding-preview-input
     ((,panic-class (:background ,panic-highlight-line
                                   :box ,panic-emphasis))
      (,panic-256-class  (:background ,panic-256-highlight-line
                                        :box ,panic-256-emphasis)))))

  (custom-theme-set-variables
   'panic
   `(ansi-color-names-vector [,panic-background ,panic-red ,panic-green ,panic-yellow
                                                  ,panic-blue ,panic-magenta ,panic-cyan ,panic-foreground])

   ;; compilation
   `(compilation-message-face 'default)

   ;; fill-column-indicator
   `(fci-rule-color ,panic-highlight-line)

   ;; magit
   `(magit-diff-use-overlays nil)

   ;; highlight-changes
   `(highlight-changes-colors '(,panic-magenta ,panic-violet))

   ;; highlight-tail
   `(highlight-tail-colors
     '((,panic-highlight-line . 0)
       (,panic-green-lc . 20)
       (,panic-cyan-lc . 30)
       (,panic-blue-lc . 50)
       (,panic-yellow-lc . 60)
       (,panic-orange-lc . 70)
       (,panic-magenta-lc . 85)
       (,panic-highlight-line . 100)))

   ;; pos-tip
   `(pos-tip-foreground-color ,panic-background)
   `(pos-tip-background-color ,panic-green)

   ;; vc
   `(vc-annotate-color-map
     '((20 . ,panic-red)
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . ,panic-yellow)
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . ,panic-green)
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . ,panic-cyan)
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . ,panic-blue)))
   `(vc-annotate-very-old-color nil)
   `(vc-annotate-background nil)

   ;; weechat
   `(weechat-color-list
     (unspecified ,panic-background ,panic-highlight-line
                  ,panic-red-d ,panic-red
                  ,panic-green-d ,panic-green
                  ,panic-yellow-d ,panic-yellow
                  ,panic-blue-d ,panic-blue
                  ,panic-magenta-d ,panic-magenta
                  ,panic-cyan-d ,panic-cyan
                  ,panic-foreground ,panic-emphasis))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'panic)

;; Local Variables:
;; no-byte-compile: t
;; fill-column: 95
;; End:

;;; panic-theme.el ends here
