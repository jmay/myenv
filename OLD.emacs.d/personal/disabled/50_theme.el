;;; theme.el --- my personal theme setup
;;
;;; Commentary:
;;
;; emacs can crash in load-theme when running in terminal mode (window-system is nil)
;;
;;; Code:
(cond ((eq (window-system) 'ns)
       (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
       (load-theme 'jmay t)

       ;; (load-theme 'manoj-dark t)
       ;; (load-theme 'zenburn t)                 ; low-contrast, background is grey
       ;; (load-theme 'Amelie t)
       ;; (load-theme 'spolsky t)
       ;; (load-theme 'firebelly t)
       ;; (load-theme 'leuven) ; bright
       ;; (load-theme 'wombat) ; dark, with bright current line and fringe

       (set-face-attribute 'default nil :family "Source Code Pro")
       (set-face-attribute 'default nil :weight 'light)
       (set-face-attribute 'default nil :height 140)
       ))

;;; 50_theme.el ends here
