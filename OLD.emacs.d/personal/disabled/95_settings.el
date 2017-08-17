;; settings.el

;; this is already in ruby.el
;; perhaps all this should be moved to programming.el
;; (highlight-indentation-mode)

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-background 'highlight-indentation-face "#202020")
            (set-face-background 'highlight-indentation-current-column-face "#999999")
            ))
