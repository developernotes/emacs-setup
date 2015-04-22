;;; standard-stone-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2010 Nick Parker.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme standard-stone
  "Standard stone light color theme")

(custom-theme-set-faces
 'standard-stone
 '(default ((t (:background "#fdf6e3" :foreground "#000000"))))
 '(cursor ((t (:background "#000000" :foreground "#ffffff"))))
 '(region ((t (:background "#eedc82"))))
`(mode-line ((t (:box (:line-width 1 :color "#595959")
                         :background "#f3efdb" :foreground "#595959"))))
 `(mode-line-inactive ((t (:box (:line-width 1 :color "#505050")
                                  :background "#f3efdb" :foreground "#c6c6c6")))) 
 '(fringe ((t (:background "#fdf6e3"))))
 '(minibuffer-prompt ((t (:foreground "#0000cd"))))
 '(font-lock-builtin-face ((t (:foreground "#ba55d3"))))
 '(font-lock-comment-face ((t (:foreground "#b22222"))))
 '(font-lock-constant-face ((t (:foreground "#008b8b"))))
 '(font-lock-function-name-face ((t (:foreground "#0000ff"))))
 '(font-lock-keyword-face ((t (:foreground "#a020f0"))))
 '(font-lock-string-face ((t (:foreground "#8b2252"))))
 '(font-lock-type-face ((t (:foreground "#228b22"))))
 '(font-lock-variable-name-face ((t (:foreground "#a0522d"))))
 '(font-lock-warning-face ((t (:foreground "#ff0000"))))
 '(isearch ((t (:background "#cd00cd" :foreground "#b0e2ff"))))
 '(lazy-highlight ((t (:background "#afeeee"))))
 '(link ((t (:foreground "#0000ff" :underline t))))
 '(link-visited ((t (:foreground "#8b008b" :underline t))))
 '(button ((t (:underline t))))
 '(header-line ((t (:background "#e5e5e5" :foreground "#333333"))))
 '(eshell-prompt ((t (:foreground "#1f10a8" :bold t)))))

(provide-theme 'standard-stone)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; standard-stone-theme.el  ends here
