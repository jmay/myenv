(setq mac-command-modifier 'super)
;; (setq mac-option-modifier 'hyper)
(setq mac-option-modifier 'meta)
(setq mac-right-command-modifier 'alt)
(setq mac-right-control-modifier 'hyper)
(setq mac-right-option-modifier nil) ; so still works for Apple extended character entry
;; (setq ns-function-modifier 'hyper)  ; make Fn key do Hyper?

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c 8") 'mc/mark-all-dwim)



(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; navigating between buffers
;; windmove moves the cursor; buf-move swaps entire buffers
(global-set-key (kbd "<A-left>")   'windmove-left)
(global-set-key (kbd "<A-right>")  'windmove-right)
(global-set-key (kbd "<A-up>")  'windmove-up)
(global-set-key (kbd "<A-down>")  'windmove-down)

(global-set-key (kbd "<A-H-left>")   'buf-move-left)
(global-set-key (kbd "<A-H-right>")  'buf-move-right)


;; swap isearch-forward C-s and isearch-forward-regexp C-M-s
;; ditto backward C-r, C-M-R
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "M-%") 'query-replace-regexp)
;; (global-set-key (kbd "C-M-%") 'query-replace)

(global-set-key (kbd "C-x g") #'magit-status)


;; try out some alternative bindings for spacemacs-leader
(define-key key-translation-map (kbd "A-m") (kbd "s-m"))
(define-key key-translation-map (kbd "<f5>") (kbd "s-m"))
(define-key key-translation-map (kbd "<f12>") (kbd "s-m"))


(define-key org-mode-map (kbd "RET")
  'scimax/org-return)
