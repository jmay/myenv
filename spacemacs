;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; (toggle-debug-on-quit)

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------

     ;; 170817 switched from helm to ivy due to problems with helm-projectile;
     ;; this also seemed to fix issues with magit
     ivy

     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-tab-key-behavior 'complete
                      )
                                        ; using company?
     better-defaults
     emacs-lisp
     git
     github
     markdown

     (org :variables
          org-enable-reveal-js-support t
          org-enable-github-support t
          )
     spacemacs-org

     typography
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            )
     ;; spell-checking
     ;; syntax-checking
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      )
     version-control
     osx

     csv
     yaml
     (ruby :variables
           ruby-enable-ruby-on-rails-support t
           ruby-test-runner 'rspec
           ruby-version-manager 'rvm
           )
     ruby-on-rails
     html
     javascript
     shell-scripts
     sql
     python

     dash
     command-log
     ;; slack
     ;; unimpaired
     ;; erc
     chinese
     ;; ranger
     ;; nlinum ;; superseded by dotspacemacs-line-numbers? seems to be broken, bad signature
     search-engine
     floobits
     ;; spacemacs-editing
     restclient

     ;; replace C-x C-b list-buffers with ibuffer
     ;; by default group buffers by projectile project
     (ibuffer :variables ibuffer-group-buffers-by 'projects)

     ;; fci-mode ;; maybe fill-column-indicator (comes with git) is enough?
     ;; org-bullets ;; now included by default with spacemacs org layer?
     ;; org-download ;; now included by default with spacemacs org layer?

     ;; http://spacemacs.org/doc/LAYERS.html
     ;; SPC h SPC for list of available layers
     ;;jmay
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      key-chord
                                      transpose-frame
                                      paredit
                                      js2-mode

                                      ;; org-plus-contrib
                                      ;; ox-reveal
                                      ;; org-protocol-capture-html
                                      org-bookmark-heading
                                      ;; ob-applescript ;; to use Applescript in org-babel
                                      ;; ob-http
                                      ;; ob-shell
                                      ;; ob-async
                                      ;; ox-twbs ;; twitter bootstrap
                                      ;; ox-gfm ;; github-flavored markdown
                                      ;; org-jira
                                      org-mac-link ;; included with org-mode
                                      ;; ox-extra

                                      minimap
                                      ;; calfw
                                      ;; calfw-ical
                                      visual-fill-column
                                      highlight-indent-guides
                                      magithub
                                      shackle
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    ;; smartparens
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

;; UNDER CONSIDERATION
;; spacemacs-ivy
;; https://www.reddit.com/r/emacs/comments/407q2c/ivy_is_now_available_in_spacemacs/
;; https://github.com/syl20bnr/spacemacs/issues/4540
;; http://oremacs.com/2016/04/26/ivy-0.8.0/


(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default nil)
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style '(hybrid :variables
                                       hybrid-mode-enable-evilified-state t
                                       hybrid-mode-enable-hjkl-bindings t
                                       hybrid-mode-default-state 'normal)
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "s-m" ;; "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "," ;; "s-RET???"
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'all
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (setq exec-path-from-shell-check-startup-files nil)

  ;; org-mode
  (setq org-agenda-files (list
                          "~/Dropbox/Documents/Notes/00 TODO & REMINDERS.org"
                          "~/dev/other/survey/TODO.org"
                          ))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."

  ;; (spacemacs/toggle-smartparens-globally-off)

  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define-global "--" "_")

  ;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html
  ;; C-RET puts cursor at *start* of match
  (defun isearch-exit-other-end ()
    "Exit isearch, at the opposite end of the string."
    (interactive)
    (isearch-exit)
    (goto-char isearch-other-end))
  (define-key isearch-mode-map [(control return)]
    #'isearch-exit-other-end)

  ;; https://github.com/sigma/magit-gh-pulls
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

  (setq confirm-kill-emacs 'yes-or-no-p)

  ;; org-mode
  (setq org-agenda-files (list
                          "~/Dropbox/Documents/Notes/00 TODO & REMINDERS.org"
                          "~/dev/other/survey/TODO.org"
                          ))
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WIP" "DONE" "HOLD" "WONT-DO")))

  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)

  (setq org-special-ctrl-a/e t)

  ;; because I want different recentf history on different machines
  ;; (setq recentf-save-file "~/.emacs.local/recentf")

  ;; turn off line numbers *only* in org-mode
  ;; NO - this shuts it off everywhere whenever I open an org file
  ;; (defun nolinum ()
  ;;   (global-linum-mode 0)
  ;;   )
  ;; (add-hook 'org-mode-hook 'nolinum)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  ;; (use-package wttrin
  ;;   :ensure t
  ;;   :commands (wttrin)
  ;;   :init
  ;;   (setq wttrin-default-cities '("Los Altos"
  ;;                                 "San Francisco"
  ;;                                 "Brisbane")))

  ;; I don't like auto-complete in org-mode, it gives me too much junk.
  (spacemacs|disable-company org-mode)

  ;; I want compilation (rspec or other) to prefer to split horizontally (side-by-side).
  ;; Haven't found values for these that work.
  ;; (setq split-height-threshold nil)
  ;; (setq split-width-threshold 0)

  ;; avy-goto-char-timer gives you a short time to enter multiple chars of a
  ;; string, and then switches to regular avy-jump for the matches.
  (spacemacs/set-leader-keys "jc" 'avy-goto-char-timer)

  ;; This makes mouse-selection immediately copy, without needing an extra command.
  ;; (setq mouse-drag-copy-region t)

  ;; http://www.mostlymaths.net/2016/09/more-emacs-configuration-tweaks.html
  ;; multiple-cursors using (meta+) mouse clicks
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/toggle-cursor-on-click)

  ;; http://www.johndcook.com/blog/2015/02/01/rare-bigrams/
  ;; (key-chord-define-global "kk" 'function)
  (key-chord-define-global "xz" 'rspec-rerun)

  ;; until I can figure out what purpose-mode is for
  ;; https://github.com/syl20bnr/spacemacs/issues/7453
  (pupo-mode -1)
  (purpose-mode -1)

  ;; (desktop-save-mode 1)

  ;; 'fill or 'column or 'character
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

  (setq-default
   ;; js2-mode
   js2-basic-offset 2
   ;; web-mode
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2)

  ;; org-bullets
  (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (sh . t)
     (python . t)))

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

  ;; (ox-extras-activate '(ignore-headlines))

  (setq shackle-rules '(
                        (compilation-mode :noselect t :align 'right :size 0.4)
                        ("\*rspec-compilation\*" :noselect t :align 'right :size 0.4)
                        )
        shackle-default-rule '(:select t)
        )


  ;; (use-package magithub
  ;;   :after magit
  ;;   :config (magithub-feature-autoinject t)
  ;;   )

  (put 'suspend-frame 'disabled t)

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

  ;; mouse keys
  (global-set-key (kbd "<mouse-3>") 'yank)
  ;;(global-set-key (kbd "<mouse-4>") 'xxx)

  ;; try out some alternative bindings for spacemacs-leader
  (define-key key-translation-map (kbd "A-m") (kbd "s-m"))
  (define-key key-translation-map (kbd "<f5>") (kbd "s-m"))
  (define-key key-translation-map (kbd "<f12>") (kbd "s-m"))


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

  (define-key org-mode-map (kbd "RET")
    'scimax/org-return)

  ;; Replace versions from org-mac-link.el (included with org-mode).
  ;; These version use osascript shell command rather than `do-applescript', which hangs.
  (defun org-as-mac-chrome-get-frontmost-url ()
    (let ((result
           (shell-command-to-string
            "osascript -e 'tell application \"Google Chrome\" to return (the URL of active tab of front window) & \"::split::\" & (the name of window 1)'")))
      (replace-regexp-in-string
       "^\"\\|\"$" "" (car (split-string result "[\r\n]+" t)))))
  (defun org-as-mac-safari-get-frontmost-url ()
    (let ((result
           (shell-command-to-string
            "osascript -e 'tell application \"Safari\" to return (the URL of document 1) & \"::split::\" & (the name of document 1)'")))
      (replace-regexp-in-string
       "^\"\\|\"$" "" (car (split-string result "[\r\n]+" t)))))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#3E3D31" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#3E3D31" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/Documents/Notes/projects/surveyapp.org" "~/Dropbox/Documents/Notes/projects/schoolistry.org" "~/Dropbox/Documents/Notes/00 TODO & REMINDERS.org" "~/dev/other/survey/TODO.org")))
 '(org-agenda-span (quote fortnight))
 '(org-babel-python-command "python3")
 '(package-selected-packages
   (quote
    (highlight-indent-guides minitest insert-shebang hide-comnt helm-purpose window-purpose imenu-list visual-fill-column pug-mode osx-dictionary dumb-jump undo-tree ob-applescript minimap org-bookmark-heading floobits engine-mode calfw google-maps org-mac-link swiper ivy srefactor nlinum helm-gtags ggtags emoji-cheat-sheet-plus company-emoji org-protocol-capture-html slack emojify circe oauth2 websocket org-projectile mwim github-search flyspell-correct-helm flyspell-correct evil-unimpaired chinese-pyim-basedict marshal ht wttrin ox-reveal paredit csv-mode org-jira yaml-mode ox-twbs org-download evil-visual-mark-mode pinyinlib transpose-frame key-chord dash-functional tern skewer-mode simple-httpd json-snatcher json-reformat web-completion-data powerline rake inflections spinner alert log4e gntp parent-mode request fringe-helper logito pcache pkg-info epl flx magit-popup git-commit iedit anzu highlight pos-tip popup async s eyebrowse column-enforce-mode pangu-spacing find-by-pinyin-dired chinese-pyim ace-pinyin ace-jump-mode erc-gitter ranger erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks f uuidgen command-log-mode helm-dash dash-at-point livid-mode link-hint evil-ediff eshell-z company-shell auto-complete avy packed inf-ruby smartparens with-editor gh projectile helm helm-core yasnippet multiple-cursors hydra dash ag ruby-refactor helm-flyspell auto-dictionary zenburn-theme monokai-theme solarized-theme typo web-mode web-beautify tagedit sql-indent slim-mode scss-mode sass-mode projectile-rails magit-gh-pulls less-css-mode json-mode js2-refactor js2-mode js-doc jade-mode helm-css-scss haml-mode github-clone github-browse-file git-link gist fish-mode feature-mode emmet-mode company-web company-tern coffee-mode reveal-in-osx-finder pbcopy osx-trash launchctl rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv chruby bundler xterm-color toc-org smeargle shell-pop orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter gh-md flycheck-pos-tip flycheck evil-magit eshell-prompt-extras esh-help diff-hl company-statistics company-quickhelp company auto-yasnippet ac-ispell ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline smooth-scrolling restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(visual-fill-column-center-text t)
 '(weechat-color-list
   (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-faces-vector
     [default default default italic underline success warning error])
   '(ansi-color-names-vector
     ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
   '(compilation-message-face (quote default))
   '(cua-global-mark-cursor-color "#2aa198")
   '(cua-normal-cursor-color "#839496")
   '(cua-overwrite-cursor-color "#b58900")
   '(cua-read-only-cursor-color "#859900")
   '(git-commit-setup-hook
     (quote
      (git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-propertize-diff with-editor-usage-message)))
   '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
   '(highlight-symbol-colors
     (--map
      (solarized-color-blend it "#002b36" 0.25)
      (quote
       ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
   '(highlight-symbol-foreground-color "#93a1a1")
   '(highlight-tail-colors
     (quote
      (("#3E3D31" . 0)
       ("#67930F" . 20)
       ("#349B8D" . 30)
       ("#21889B" . 50)
       ("#968B26" . 60)
       ("#A45E0A" . 70)
       ("#A41F99" . 85)
       ("#3E3D31" . 100))))
   '(hl-bg-colors
     (quote
      ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
   '(hl-fg-colors
     (quote
      ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
   '(magit-diff-use-overlays nil)
   '(nrepl-message-colors
     (quote
      ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
   '(org-agenda-files
     (quote
      ("~/Dropbox/Documents/Notes/projects/surveyapp.org" "~/Dropbox/Documents/Notes/projects/schoolistry.org" "~/Dropbox/Documents/Notes/00 TODO & REMINDERS.org" "~/dev/other/survey/TODO.org")))
   '(org-agenda-span (quote fortnight))
   '(org-babel-python-command "python3")
   '(org-directory "~/Dropbox/Documents/Notes")
   '(org-imenu-depth 4)
   '(org-startup-folded (quote showeverything))
   '(org-startup-indented t)
   '(org-structure-template-alist
     (quote
      (("n" "#+BEGIN_NOTES
?
#+END_NOTES")
       ("n" "#+NAME: ?")
       ("s" "#+BEGIN_SRC ?

#+END_SRC")
       ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE")
       ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE")
       ("v" "#+BEGIN_VERSE
?
#+END_VERSE")
       ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM")
       ("c" "#+BEGIN_CENTER
?
#+END_CENTER")
       ("l" "#+BEGIN_EXPORT latex
?
#+END_EXPORT")
       ("L" "#+LaTeX: ")
       ("h" "#+BEGIN_EXPORT html
?
#+END_EXPORT")
       ("H" "#+HTML: ")
       ("a" "#+BEGIN_EXPORT ascii
?
#+END_EXPORT")
       ("A" "#+ASCII: ")
       ("i" "#+INDEX: ?")
       ("I" "#+INCLUDE: %file ?")
       ("t" "#+TITLE: "))))
   '(package-selected-packages
     (quote
      (restclient-helm helm-pydoc wgrep smex ivy-purpose ivy-hydra counsel-projectile counsel-dash counsel highlight-indent-guides minitest insert-shebang hide-comnt helm-purpose window-purpose imenu-list visual-fill-column pug-mode osx-dictionary dumb-jump undo-tree ob-applescript minimap org-bookmark-heading floobits engine-mode calfw google-maps org-mac-link swiper ivy srefactor nlinum helm-gtags ggtags emoji-cheat-sheet-plus company-emoji org-protocol-capture-html slack emojify circe oauth2 websocket org-projectile mwim github-search flyspell-correct-helm flyspell-correct evil-unimpaired chinese-pyim-basedict marshal ht wttrin ox-reveal paredit csv-mode org-jira yaml-mode ox-twbs org-download evil-visual-mark-mode pinyinlib transpose-frame key-chord dash-functional tern skewer-mode simple-httpd json-snatcher json-reformat web-completion-data powerline rake inflections spinner alert log4e gntp parent-mode request fringe-helper logito pcache pkg-info epl flx magit-popup git-commit iedit anzu highlight pos-tip popup async s eyebrowse column-enforce-mode pangu-spacing find-by-pinyin-dired chinese-pyim ace-pinyin ace-jump-mode erc-gitter ranger erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks f uuidgen command-log-mode helm-dash dash-at-point livid-mode link-hint evil-ediff eshell-z company-shell auto-complete avy packed inf-ruby smartparens with-editor gh projectile helm helm-core yasnippet multiple-cursors hydra dash ag ruby-refactor helm-flyspell auto-dictionary zenburn-theme monokai-theme solarized-theme typo web-mode web-beautify tagedit sql-indent slim-mode scss-mode sass-mode projectile-rails magit-gh-pulls less-css-mode json-mode js2-refactor js2-mode js-doc jade-mode helm-css-scss haml-mode github-clone github-browse-file git-link gist fish-mode feature-mode emmet-mode company-web company-tern coffee-mode reveal-in-osx-finder pbcopy osx-trash launchctl rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv chruby bundler xterm-color toc-org smeargle shell-pop orgit org-repo-todo org-present org-pomodoro org-plus-contrib org-bullets multi-term mmm-mode markdown-toc markdown-mode magit-gitflow magit htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-gutter-fringe+ git-gutter-fringe git-gutter+ git-gutter gh-md flycheck-pos-tip flycheck evil-magit eshell-prompt-extras esh-help diff-hl company-statistics company-quickhelp company auto-yasnippet ac-ispell ws-butler window-numbering volatile-highlights vi-tilde-fringe spaceline smooth-scrolling restart-emacs rainbow-delimiters popwin persp-mode pcre2el paradox page-break-lines open-junk-file neotree move-text macrostep lorem-ipsum linum-relative leuven-theme info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido fill-column-indicator fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-jumper evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-args evil-anzu eval-sexp-fu elisp-slime-nav define-word clean-aindent-mode buffer-move bracketed-paste auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line quelpa package-build use-package which-key bind-key bind-map evil spacemacs-theme)))
   '(pos-tip-background-color "#A6E22E")
   '(pos-tip-foreground-color "#272822")
   '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
   '(term-default-bg-color "#002b36")
   '(term-default-fg-color "#839496")
   '(vc-annotate-background nil)
   '(vc-annotate-color-map
     (quote
      ((20 . "#F92672")
       (40 . "#CF4F1F")
       (60 . "#C26C0F")
       (80 . "#E6DB74")
       (100 . "#AB8C00")
       (120 . "#A18F00")
       (140 . "#989200")
       (160 . "#8E9500")
       (180 . "#A6E22E")
       (200 . "#729A1E")
       (220 . "#609C3C")
       (240 . "#4E9D5B")
       (260 . "#3C9F79")
       (280 . "#A1EFE4")
       (300 . "#299BA6")
       (320 . "#2896B5")
       (340 . "#2790C3")
       (360 . "#66D9EF"))))
   '(vc-annotate-very-old-color nil)
   '(vc-follow-symlinks t)
   '(visual-fill-column-center-text t)
   '(weechat-color-list
     (unspecified "#272822" "#3E3D31" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
   '(xterm-color-names
     ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
   '(xterm-color-names-bright
     ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
  )
