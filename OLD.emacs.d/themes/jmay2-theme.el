(deftheme jmay2
  "Created 2013-08-30.")

(custom-theme-set-variables
 'jmay2
 '(help-at-pt-timer-delay 0.3)
 '(org-agenda-files (quote ("~/.deft/emacs-todo.org" "~/.deft/otherbase-todo.org")))
 '(org-startup-folded (quote showeverything))
 ;; '(help-at-pt-display-when-idle (quote (flymake-overlay)))
 '(semantic-mode t)
 '(custom-safe-themes (quote ("fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f61972772958e166cda8aaf0eba700aad4faa0b4101cee319e894e7a747645c9" default))))

(custom-theme-set-faces
 'jmay2
 '(linum ((t (:inherit (shadow default) :background "#3F3F3F" :foreground "#9FC59F" :height 0.8)))))

(provide-theme 'jmay2)
