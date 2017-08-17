;;; funcs.el --- my configuration File for Spacemacs
;;
;; Copyright (c) 2016 Jason W. May
;;
;; Author: Jason W. May <jmay@pobox.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (thing-at-point 'word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word))
      (setq aft (thing-at-point 'word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)


;; (defun linum-update-window-scale-fix (win)
;; "fix linum for scaled text"
;; (set-window-margins win
;;                     (ceiling (* (if (boundp 'text-scale-mode-step)
;;                                     (expt text-scale-mode-step
;;                                           text-scale-mode-amount) 1)
;;                                 (if (car (window-margins))
;;                                     (car (window-margins)) 1)
;;                                 ))))
;; (advice-add #'linum-update-window :after #'linum-update-window-scale-fix)

;; Preset width nlinum
;; (add-hook 'nlinum-mode-hook
;;           (lambda ()
;;             (unless (boundp 'nlinum--width)
;;               (setq nlinum--width
;;                     (length (number-to-string
;;                              (count-lines (point-min) (point-max))))))))


(setq paradox-github-token 'a5cb30cb243c58738d60a57c56378bdadf821674)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun babel-confirm (flag)
  "Report the setting of org-confirm-babel-evaluate.
      If invoked with C-u, toggle the setting"
  (interactive "P")
  (if (equal flag '(4))
      (setq org-confirm-babel-evaluate (not org-confirm-babel-evaluate)))
  (message "Babel evaluation confirmation is %s"
           (if org-confirm-babel-evaluate "on" "off")))

(defun my-coding-settings ()
  "Show fill column indications and line numbers"
;;  (fci-mode)
  ;; (setq linum-format "%4d ")
  ;; (linum-mode)
  ;; (setq whitespace-line-column 70)
  (setq comment-empty-lines t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq js-indent-level 2)
  )

(add-hook 'prog-mode-hook 'my-coding-settings)
;; scss-mode (for CSS, SASS) is not derived from prog-mode
(add-hook 'scss-mode-hook 'my-coding-settings)
(add-hook 'web-mode-hook 'my-coding-settings)

;; (setq flycheck-highlighting-mode 'lines)

;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
(require 'org-inlinetask)
(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond
     ;; Open links like usual
     ((eq 'link (car (org-element-context)))
      (org-open-at-point-global))
     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))
     ;; add checkboxes
     ((org-at-item-checkbox-p)
      (org-insert-todo-heading nil))
     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((and (org-in-item-p) (not (bolp)))
      (if (org-element-property :contents-begin (org-element-context))
          (org-insert-heading)
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     (t
      (org-return)))))
