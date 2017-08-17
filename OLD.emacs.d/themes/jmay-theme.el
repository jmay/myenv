;;; jmay-theme.el --- my color scheme, cobbled together from elsewhere
;;
;;; Commentary:
;;
;; Don't remember where I found all this stuff.
;;
;;; Code:
(deftheme jmay
  "Jason's personal theme")

(custom-theme-set-faces
 'jmay
 '(button ((t (:weight bold :underline (:color foreground-color :style line) :box (:line-width 2 :color "grey" :style released-button) :foreground "black" :background "grey" :inherit (link)))))
 '(cursor ((t (:inverse-video t :foreground "#002b36" :background "orchid"))))

 ;; THIS LINE CAUSES EMACS TO CRASH WHEN OPENING A FILE WITH NON-ASCII CHARACTER
 ;; '(default ((t (:family "Source_Code_Pro" :foundry "apple" :width normal :height 140 :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :foreground "WhiteSmoke" :background "black" :stipple nil :inherit nil))))

 '(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
 '(fixed-pitch ((t (:family "Monospace"))))

 '(font-lock-builtin-face ((t (:weight bold :foreground "LightSteelBlue"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "Salmon" :inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:slant oblique :foreground "chocolate1"))))
 '(font-lock-constant-face ((t (:weight bold :foreground "LightSlateBlue"))))
 '(font-lock-doc-face ((t (:slant oblique :foreground "LightCoral" :inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:height 1.1 :weight bold :foreground "mediumspringgreen"))))
 '(font-lock-keyword-face ((t (:weight bold :foreground "cyan1"))))
 '(font-lock-negation-char-face ((t (:weight bold :foreground "#b58900"))))
 '(font-lock-preprocessor-face ((t (:slant italic :foreground "CornFlowerBlue" :inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold :foreground "#859900" :inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold :foreground "#b58900" :inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "RosyBrown1"))))
 '(font-lock-type-face ((t (:foreground "SteelBlue1"))))
 '(font-lock-variable-name-face ((t (:foreground "Aquamarine"))))
 '(font-lock-warning-face ((t (:weight bold :underline (:color foreground-color :style line) :foreground "Pink" :inherit (error)))))
 '(fringe ((t (:foreground "Wheat" :background "grey30"))))
 '(header-line ((t (:height 0.9 :box (:line-width -1 :color "grey20" :style released-button) :foreground "grey90" :background "grey20" :inherit (mode-line)))))
 '(highlight ((t (:foreground "Old Lace" :background "gray10"))))
 '(isearch ((t (:weight normal :foreground "brown4" :background "palevioletred2"))))
 '(isearch-fail ((t (:weight bold :foreground "#dc322f" :background "red4"))))
 '(lazy-highlight ((t (:weight normal :foreground "#002b36" :background "paleturquoise4"))))
 '(link ((t (:weight bold :underline (:color foreground-color :style line) :foreground "cyan1"))))
 '(link-visited ((t (:weight normal :underline (:color foreground-color :style line) :foreground "violet" :inherit (link)))))
 '(match ((t (:weight bold :foreground "#93a1a1" :background "RoyalBlue3"))))
 '(minibuffer-prompt ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "medium blue"))))
 '(mode-line ((t (:height 0.9 :box (:line-width 1 :color nil :style nil) :foreground "Blue" :background "grey75"))))
 ;; ;; this background applies to the row#, filename & major-mode fields in the mode line
 '(mode-line-buffer-id ((t (:height 0.9 :weight bold :foreground "red" :background "#7d7d7d"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
 '(mode-line-inactive ((t (:height 0.9 :weight light :box (:line-width 1 :color nil :style nil) :foreground "grey80" :background "grey30" :inherit (mode-line)))))
 '(next-error ((t (:background "blue3" :inherit (region)))))
 '(org-level-1 ((t (:foreground "#dddddd" :background "#444444" :height 1.2 :weight bold))))
 '(query-replace ((t (:foreground "brown4" :background "palevioletred2" :inherit (isearch)))))
 '(region ((t (:foreground "#000052" :background "#9999eb"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
 '(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
 '(trailing-whitespace ((((class color) (background light)) (:background "red1")) (((class color) (background dark)) (:background "red1")) (t (:inverse-video t))))
 '(variable-pitch ((t (:family "Sans Serif"))))
)

(provide-theme 'jmay)

;; (set-face-attribute 'label nil :attribute "value")
;;
;; Faces:
;;  *region* is the selected text

;;; jmay-theme.el ends here
