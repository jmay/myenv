;;; config.el --- my configuration File for Spacemacs
;;
;; Copyright (c) 2016 Jason W. May
;;
;; Author: Jason W. May <jmay@pobox.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(setq user-full-name "Jason W. May")
(setq user-mail-address "jmay@pobox.com")

(setq sentence-end-double-space nil)

(eval-after-load 'org
  '(progn
     ;; when opening org files, don't hide anything and use pretty indentation
     (setq org-startup-folded 'showeverything)
     (setq org-startup-indented t)

     ;; fontify code in code blocks
     (setq org-src-fontify-natively t)

     ;; include everything up to level 4 in the options offered for C-c i
     (setq org-imenu-depth 4)

     ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html
     (setq org-ellipsis "⤵")

     (add-hook 'org-mode-hook 'turn-on-auto-fill)

     ;; no prompting on C-c C-c execution in source blocks
     (setq org-confirm-babel-evaluate nil)

     (setq org-capture-templates
           '(
             ("t"
              "TODO"
              entry
              (file+headline "~/Dropbox/Documents/Notes/gtd.org" "Tasks")
              "* TODO %?\n  %i\n  %a")
             ("o"
              "Otherbase TODO"
              entry
              (file+headline "~/Dropbox/Documents/Notes/otherbase-todo.org" "Tasks")
              "* TODO %?\n  %i\n")
             ("v"
              "Veriphyr TODO"
              entry
              (file+headline "~/Dropbox/Documents/Notes/veriphyr-todo.org" "Tasks")
              "* TODO %?\n  %i\n")
             ("e"
              "Emacs TODO"
              entry
              (file+headline "~/Dropbox/Documents/Notes/emacs-todo.org" "Emacs TODO")
              "* TODO %?\n  %i\n")
             ("j"
              "Journal"
              entry
              (file+datetree "~/Dropbox/Documents/Notes/journal.org")
              "* %?\n%i\n")
             ("s"
              "Schoolistry Journal"
              entry
              (file+datetree "~/Dropbox/Documents/Notes/projects/schoolistry-surveys-journal.org")
              "* %? :schoolistry:\n%i\n")
             )
           )

     ;; for MobileOrg
     (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
     (setq org-directory "~/Dropbox/Apps/MobileOrg")

     ;;(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●"))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; install sql (includes postgresql) support for org-babel
;; http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-sql.html
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (sh . t)
   (python . t)))
;; ;; add additional languages with '((language . t)))


;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (defun org-begin-template ()
       "Make a template at point."
       (interactive)
       (if (org-at-table-p)
           (call-interactively 'org-table-rotate-recalc-marks)
         (let* ((choices '(("s" . "SRC")
                           ("e" . "EXAMPLE")
                           ("q" . "QUOTE")
                           ("v" . "VERSE")
                           ("c" . "CENTER")
                           ("l" . "LaTeX")
                           ("h" . "HTML")
                           ("a" . "ASCII")))
                (key
                 (key-description
                  (vector
                   (read-key
                    (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                            (mapconcat (lambda (choice)
                                         (concat (propertize (car choice) 'face 'font-lock-type-face)
                                                 ": "
                                                 (cdr choice)))
                                       choices
                                       ", ")))))))
           (let ((result (assoc key choices)))
             (when result
               (let ((choice (cdr result)))
                 (cond
                  ((region-active-p)
                   (let ((start (region-beginning))
                         (end (region-end)))
                     (goto-char end)
                     (insert "#+END_" choice "\n")
                     (goto-char start)
                     (insert "#+BEGIN_" choice "\n")))
                  (t
                   (insert "#+BEGIN_" choice "\n")
                   (save-excursion (insert "#+END_" choice))))))))))

     (add-to-list 'org-structure-template-alist '("n" "#+NAME: ?"))
     ))

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; (set-face-background 'highlight-indentation-face "#222")
;; (set-face-background 'highlight-indentation-current-column-face "#444")
;; (defun my-highlight-indentation ()
;;   (progn
;;     (highlight-indentation-mode)
;;     (highlight-indentation-current-column-mode)
;;     ))
;; (add-hook 'prog-mode-hook 'my-highlight-indentation)


(setq-default
 ;; js2-mode
 js2-basic-offset 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

;;(use-package ruby-refactor
;;  :ensure t
;;  :config
;;  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
;;  )
;; (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)

(setq rspec-autosave-buffer t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; config.el ends here

;; REFERENCES
;; org beautification
;; https://zhangda.wordpress.com/2016/02/15/configurations-for-beautifying-emacs-org-mode/
