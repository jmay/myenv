;; (require 'org-publish)
;; (setq org-publish-project-alist
;;       '(("html"
;;          :base-directory "~/.deft"
;;          :base-extension "org"
;;          :publishing-directory "~/Documents/exports"
;;          :publishing-function org-publish-org-to-html)
;;         ("pdf"
;;          :base-directory "~/.deft/"
;;          :base-extension "org"
;;          :publishing-directory "~/Documents/exports"
;;          :publishing-function org-publish-org-to-pdf)
;;         ("all" :components ("html" "pdf"))))




;; https://github.com/milkypostman/powerline
;; activate powerline (mode line theme)

;;(powerline-default-theme)

;; (defface powerline-jmay1
;;   '((t (:inherit mode-line :foreground "#2FDE3A" :background "#222222")))
;;   "Jason's powerline face 1."
;;   :group 'powerline)

;; (defface powerline-jmay2
;;   '((t (:inherit mode-line :background "#333333" :foreground "#2FDE3A")))
;;   "Jason's powerline face 2."
;;   :group 'powerline)

;; (defface powerline-inactive1
;;   '((t (:background "grey11" :inherit mode-line-inactive)))
;;   "Powerline face 1."
;;   :group 'powerline)

;; (defface powerline-inactive2
;;   '((t (:background "grey20" :inherit mode-line-inactive)))
;;   "Powerline face 2."
;;   :group 'powerline)


;; (defun powerline-jmay-theme ()
;;   "Setup the default mode-line."
;;   (interactive)
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'powerline-jmay1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-jmay2 'powerline-inactive2))
;;                           (separator-left (intern (format "powerline-%s-%s"
;;                                                           powerline-default-separator
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            powerline-default-separator
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list (powerline-raw "%*" face1 'l)
;;                                      (powerline-buffer-size face1 'l)
;;                                      (powerline-raw mode-line-mule-info face1 'l)
;;                                      (powerline-buffer-id face1 'l)
;;                                      (when (and (boundp 'which-func-mode) which-func-mode)
;;                                        (powerline-raw which-func-format face1 'l))
;;                                      (powerline-raw " ")
;;                                      (funcall separator-left mode-line face1)
;;                                      (when (boundp 'erc-modified-channels-object)
;;                                        (powerline-raw erc-modified-channels-object face1 'l))
;;                                      (powerline-major-mode face1 'l)
;;                                      (powerline-process face1)
;;                                      (powerline-minor-modes face1 'l)
;;                                      (powerline-narrow face1 'l)
;;                                      (powerline-raw " " face1)
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-vc face2 'r)))
;;                           (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                      (funcall separator-right face2 face1)
;;                                      (powerline-raw "%4l" face1 'l)
;;                                      (powerline-raw ":" face1 'l)
;;                                      (powerline-raw "%3c" face1 'r)
;;                                      (funcall separator-right face1 mode-line)
;;                                      (powerline-raw " ")
;;                                      (powerline-raw "%6p" nil 'r)
;;                                      (powerline-hud face2 face1))))
;;                      (concat (powerline-render lhs)
;;                              (powerline-fill face2 (powerline-width rhs))
;;                              (powerline-render rhs)))))))
