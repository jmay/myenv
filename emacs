;; Jason May's emacs initialization file
;;
;; my favorite modes
;;
;; markdown
;; http://jblevins.org/projects/markdown-mode/
;; I use .md for usual markdown extension
;; TODO: might want to recognize .mmd, .text, .txt
;;
(autoload 'markdown-mode "markdown-mode.el"
          "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode)
      auto-mode-alist))
(setq auto-mode-alist
      (cons '("emacs$" . emacs-lisp-mode)
      auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TextMate emulation
;; http://www.emacswiki.org/emacs/TextMate
;; fuzzy find-file completion ("Go To File")
(require 'ido)
;;(load-file "~/env/emacs-lisp/find-file-in-project.el")

(setq load-path (cons "~/env/emacs-lisp" load-path))

;;(load-file "~/env/emacs-lisp/project-root.el")

(add-to-list 'load-path "~/env/emacs-lisp/textmate.el")
(require 'textmate)
(textmate-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git

;;(require 'magit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes
;; this pulls in the color-theme library
;;(load-file "~/env/emacs-lisp/twilight-emacs/color-theme-twilight.el")

;; Solarized theme installation for emacs 23 - emacs 24 has more builtin stuff
(add-to-list 'load-path "~/env/solarized/emacs-colors-solarized")
(require 'color-theme)
(color-theme-initialize)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode (aka syntax highlighting)
(global-font-lock-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MacOSX-specific stuff
;;
;; turn off Option key as Meta
;;(setq mac-option-modifier nil)
;; turn on Command key as Meta
;; I'm used to ESC as Meta, so maybe comment-
;;(setq mac-command-modifier 'meta)
;; keeps cmd-V, cmd-C, cmd-X working for clipboard operations
;; but not cmd-O for open, cmd-S for save
;;(setq x-select-enable-clipboard 't)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(global-set-key "" 'delete-backward-char)
;;(global-set-key "" 'backward-kill-word)

;; Enable wheelmouse support by default
;;(require 'mwheel)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
;;(global-set-key [delete] 'delete-char)
;;(global-set-key [kp-delete] 'delete-char)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Perl

;; this sets the default mode for files ending in .pl
(setq auto-mode-alist (cons '("\\.pl$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
;; this sets the default mode for files with header #!...stuff.../perl
(setq interpreter-mode-alist (cons '("perl" . cperl-mode)
				   interpreter-mode-alist))

(custom-set-variables
 '(paren-mode (quote blink-paren) nil (paren))
 '(cperl-indent-level 2)
 '(cperl-continued-statement-offset 4)
 '(cperl-tab-always-indent t)
 '(indent-tabs-mode nil)
 '(user-mail-address "jmay@pobox.com" t)
 '(query-user-mail-address nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-abbrev-file "~/.emacs-abbrevs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My favorite shortcuts

(global-set-key "g" 'goto-line)

(global-set-key "%" 'query-replace-regexp)

(global-set-key "\C-c\C-h" 'help-for-help)

(global-set-key "\C-c\C-d" 'cperl-perldoc)

(global-set-key "\C-c\C-m" 'manual-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I want autofill for my text buffers
;; 051230: turning this off.  experimenting with breaking out of the tyranny of 80 cols...
;;(setq-default fill-column 76)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby on Rails

  (defun try-complete-abbrev (old)
   (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
       '(try-complete-abbrev
         try-complete-file-name
         try-expand-dabbrev))


(set-default 'cursor-type 'box)
;;(set 'cursor-type 'box)



;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; themes

(require 'color-theme)
(color-theme-initialize)
(load-file "~/env/emacs-lisp/twilight-emacs/color-theme-twilight.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; these entries came from hostingzoom.com

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(global-font-lock-mode t)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MacOSX-specific stuff
;;
;; turn off Option key as Meta
;;(setq mac-option-modifier nil)
;; turn on Command key as Meta
;; I'm used to ESC as Meta, so maybe comment-
;;(setq mac-command-modifier 'meta)
;; keeps cmd-V, cmd-C, cmd-X working for clipboard operations
;; but not cmd-O for open, cmd-S for save
;;(setq x-select-enable-clipboard 't)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "" 'delete-backward-char)
(global-set-key "" 'backward-kill-word)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(setq load-path (cons "~/env/emacs-lisp" load-path))
;;(cond (running-xemacs
;;       (setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
;;
;;       (setq load-path (nconc load-path '("/usr/share/emacs/site-lisp" "/usr/share/emacs/20.7/site-lisp"))))
;;	   )


;; Enable wheelmouse support by default
(require 'mwheel)


;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
;;(global-set-key [delete] 'delete-char)
;;(global-set-key [kp-delete] 'delete-char)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
	(global-font-lock-mode t)
))

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control
;;
;; Under GNU Emacs, don't need to load vc explicitly, but need to do
;; it for XEmacs.
;;

(load "vc")
(autoload 'cvs-update "pcl-cvs" nil t)

(global-set-key "\C-c\C-u" 'cvs-examine)
(global-set-key "\C-x\C-q" 'vc-toggle-read-only)

;; for Bitkeeper, used for MySQL source
;;(setq vc-path "/opt/bitkeeper")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this sets the default mode for files ending in .pl
(setq auto-mode-alist (cons '("\\.pl$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pm$" . cperl-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.t$" . cperl-mode) auto-mode-alist))
;; this sets the default mode for files with header #!...stuff.../perl
(setq interpreter-mode-alist (cons '("perl" . cperl-mode)
				   interpreter-mode-alist))

(custom-set-variables
 '(paren-mode (quote blink-paren) nil (paren))
 '(cperl-indent-level 4)
 '(cperl-continued-statement-offset 4)
 '(cperl-tab-always-indent t)
 '(indent-tabs-mode nil)
 '(user-mail-address "jmay@pobox.com" t)
 '(query-user-mail-address nil)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C# stuff

(setq auto-mode-alist (cons '("\\.cs$" . c++-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gnus-nntp-server "nntp.perl.org")

;;(load "diff")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(read-abbrev-file "~/.emacs-abbrevs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remembrance Agent

;;(setq remem-scopes-list '(("mail" 6 5 500)))
;;(load "remem.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My favorite shortcuts

(global-set-key "g" 'goto-line)

(global-set-key "%" 'query-replace-regexp)

(global-set-key "\C-c\C-h" 'help-for-help)

(global-set-key "\C-c\C-d" 'cperl-perldoc)

(global-set-key "\C-cu" 'cvs-update)

(global-set-key "\C-c\C-m" 'manual-entry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(setq Info-directory-list '("/opt/xemacs/lib/xemacs-21.1.14/info/" "/opt/xemacs/lib/xemacs/xemacs-packages/info/" "/usr/local/info" "/usr/share/info/" "/usr/info"))

;;(setq Info-directory-list (cons "/usr/local/info/" Info-directory-list))
;;(setq Info-directory-list (cons "/usr/local/info/" Info-directory-list))
;; (setq Info-directory-list (cons "/usr/share/info/" Info-directory-list))


;; I want autofill for my text buffers
;; 051230: turning this off.  experimenting with breaking out of the tyranny of 80 cols...
(setq-default fill-column 76)
;;(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make this available to gnuclient

;;(gnuserv-start)

(global-set-key "\C-c\C-y" 'yank-clipboard-selection)

;; haven't used svn in years, this can be retired
;;(load-file "~/env/psvn.el")
;;(global-set-key "\C-c\C-v" 'svn-status)

;; there is also vc-svn.el - compare to psvn.el?
;; (add-to-list 'vc-handled-backends 'SVN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perlnow - templating & codegen automation
;; http://obsidianrook.com/perlnow/
;;
;;(load "template/lisp/template.el")
;;(template-initialize)
;;(load "perlnow.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby on Rails

  (defun try-complete-abbrev (old)
   (if (expand-abbrev) t nil))

  (setq hippie-expand-try-functions-list
       '(try-complete-abbrev
         try-complete-file-name
         try-expand-dabbrev))

;;  (require 'rails)


;;(set-default 'cursor-type 'box)
;;(set 'cursor-type 'box)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file access (TRAMP) - active by default

(setq tramp-default-method "ssh")
